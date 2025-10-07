use std::path::{Path, PathBuf};

use crate::{CssError, CssErrorCode, ShorthandPropertyRegistry};
use crate::error::ErrorReportGenerator;
use crate::imports_parser::extract_imports;
use crate::parser::{
    AnimationKeyFrame, CssAnimation, CssRuleset, CssRulesetProperty, CssStyleSheetItem,
    CssTransitionProperty, parse_css,
};
use crate::utils::ImportantLevel;
use bevy_asset::io::Reader;
use bevy_asset::{AssetLoader, AssetServer, AsyncReadExt, LoadContext, LoadDirectError};
use bevy_flair_core::{ComponentPropertyRef, PropertiesHashMap, PropertyRegistry, PropertyValue};
use bevy_flair_style::css_selector::CssSelector;
use bevy_flair_style::{
    AnimationKeyframesBuilder, StyleBuilderProperty, StyleSheet, StyleSheetBuilder,
    StyleSheetBuilderError,
};
use bevy_reflect::TypeRegistryArc;
use rustc_hash::FxHashMap;
use serde::{Deserialize, Serialize};
use thiserror::Error;
use tracing::{error, info, warn};

/// Error that could happen while loading a CSS stylesheet using [`CssStyleLoaderError`].
#[derive(Debug, Error)]
pub enum CssStyleLoaderError {
    /// Error that could happen while reading the file.
    #[error(transparent)]
    Io(#[from] std::io::Error),
    /// Error that could happen while reading one of the dependencies.
    #[error(transparent)]
    LoadDirect(#[from] LoadDirectError),
    /// Error that could happen while building the final StyleSheet.
    #[error(transparent)]
    StyleSheetBuilder(#[from] StyleSheetBuilderError),
    /// Css reported errors when error mode is set to [`CssStyleLoaderErrorMode::ReturnError`].
    #[error("{0}")]
    Report(String),
}

/// Different ways to handle css errors
#[derive(Debug, Copy, Clone, Default, Serialize, Deserialize)]
pub enum CssStyleLoaderErrorMode {
    /// Prints all errors in warn!()
    #[default]
    PrintWarn,
    /// Prints all errors in error!()
    PrintError,
    /// Ignore all errors
    Ignore,
    /// Return report as [`CssStyleLoaderError::Report`]
    ReturnError,
}

/// Error that could happen while loading a CSS stylesheet using [`CssStyleLoaderError`].
#[derive(Debug, Copy, Clone, Default, Serialize, Deserialize)]
pub struct CssStyleLoaderSetting {
    /// How to report errors
    #[serde(default)]
    pub error_mode: CssStyleLoaderErrorMode,
}

/// An [`AssetLoader`] for CSS stylesheets.
/// It capable of loading `.css` files.
pub struct CssStyleLoader {
    type_registry_arc: TypeRegistryArc,
    property_registry: PropertyRegistry,
    shorthand_property_registry: ShorthandPropertyRegistry,
}

pub fn get_sandboxed_path(
    roots: &Vec<PathBuf>,
    user_path_str: &str,
    current_thing_path: &PathBuf,
) -> Option<PathBuf> {
    if roots.is_empty() {
        return None;
    }

    let primary_root = &roots[0];
    let user_path = Path::new(user_path_str);

    // This will hold the path we build based on the logic below.
    let final_path: PathBuf;

    if user_path.is_absolute() {
        // --- NEW LOGIC FOR ABSOLUTE PATHS ---
        // First, check if the provided absolute path is ALREADY inside a sandbox.
        // This handles cases where a resolved sandboxed path from a previous op
        // (like path.resolve) is passed back to another op.
        if is_path_in_any_sandbox(user_path, &roots) {
            // The path is already valid and sandboxed. Normalize it and use it directly.
            final_path = match dunce::canonicalize(user_path) {
                Ok(p) => p,
                _ => return None,
            };
        } else {
            // If not, treat it as a "virtual" absolute path relative to the PRIMARY sandbox root.
            // This is for when JS asks for something like "/assets/style.css".
            let stripped_path = user_path.strip_prefix("/").unwrap_or(user_path);
            final_path = match dunce::canonicalize(primary_root.join(stripped_path)) {
                Ok(p) => p,
                _ => return None,
            };
        }
    } else {
        // Join the caller's directory with the relative path.
        final_path = match dunce::canonicalize(current_thing_path.join(user_path)) {
            Ok(p) => p,
            _ => return None,
        };
    };

    // --- FINAL SECURITY CHECKS (APPLIES TO ALL PATHS) ---
    if !is_path_in_any_sandbox(&final_path, &roots) {
        return None;
    }

    // Check for symlink escapes after canonicalization.
    match dunce::canonicalize(&final_path) {
        Ok(p) if !is_path_in_any_sandbox(&p, &roots) => {
            return None;
        }
        Ok(p) => Some(p),
        _ => None,
    }
}

fn is_path_in_any_sandbox(path_to_check: &Path, sandbox_roots: &[PathBuf]) -> bool {
    for root in sandbox_roots {
        if path_to_check.starts_with(root) {
            return true;
        }
    }
    false
}

impl CssStyleLoader {
    /// Extensions that this loader can load. basically `.css`
    pub const EXTENSIONS: &'static [&'static str] = &["css"];

    /// Creates a new [`CssStyleLoader`] with the given [`TypeRegistryArc`],  [`PropertyRegistry`] and [`ShorthandPropertyRegistry`].
    pub fn new(
        type_registry_arc: TypeRegistryArc,
        property_registry: PropertyRegistry,
        shorthand_property_registry: ShorthandPropertyRegistry,
    ) -> Self {
        Self {
            type_registry_arc,
            property_registry,
            shorthand_property_registry,
        }
    }

    pub fn load(
        &self,
        path: &PathBuf,
        mut contents: String,
        asset_server: &AssetServer,
        settings: CssStyleLoaderSetting,
        roots: &Vec<PathBuf>,
    ) -> Result<StyleSheet, CssStyleLoaderError> {
        // First we try to extract all imports
        let mut import_paths = Vec::new();

        extract_imports(&contents, |item| {
            import_paths.push(String::from(item.as_ref()));
        });

        let mut imports = FxHashMap::default();

        for import_path in import_paths {
            // get path relative to the current css file
            // let import_pathbuf = path
            //     .parent()
            //     .map(|p| p.join(&import_path))
            //     .unwrap_or_else(|| PathBuf::from(&import_path));
            let import_pathbuf = if let Some(parent) = path.parent() {
                if let Some(sandboxed_path) =
                    get_sandboxed_path(roots, import_path.as_str(), &parent.to_path_buf())
                {
                    sandboxed_path
                } else {
                    println!("Import path '{}' could not be resolved within sandbox roots", import_path);
                    return Err(CssStyleLoaderError::Report(format!(
                        "Import path '{}' could not be resolved within sandbox roots",
                        import_path
                    )));
                }
            } else {
                println!("Current CSS file has no parent directory, cannot resolve import path '{}'", import_path);
                return Err(CssStyleLoaderError::Report(format!(
                    "Import path '{}' could not be resolved because the current CSS file has no parent directory",
                    import_path
                )));
            };
            let content = std::fs::read_to_string(&import_pathbuf).map_err(|e| {
                println!("Failed to read imported CSS file '{}': {}", import_pathbuf.display(), e);
                CssStyleLoaderError::Report(format!(
                    "Failed to read imported CSS file '{}': {}",
                    import_pathbuf.display(),
                    e
                ))
            })?;
            imports.insert(import_path, self.load(&import_pathbuf, content, asset_server, settings, &roots)?);
        }

        let path_display = path.display().to_string();
        let type_registry = self.type_registry_arc.read();

        let mut builder = StyleSheetBuilder::new();
        let mut report_generator = ErrorReportGenerator::new(&path_display, &contents);

        fn report_important_level(
            report_generator: &mut ErrorReportGenerator,
            level: ImportantLevel,
        ) {
            if let ImportantLevel::Important(location) = level {
                report_generator.add_advice(
                    location,
                    "!important is not supported",
                    "!important token is being ignored, so you can remove it",
                );
            }
        }

        fn report_property_errors_recursively(
            properties: Vec<CssRulesetProperty>,
            report_generator: &mut ErrorReportGenerator,
        ) {
            for property in properties {
                match property {
                    CssRulesetProperty::NestedRuleset(nested) => {
                        if let Err(selectors_error) = nested.selectors {
                            report_generator.add_error(selectors_error);
                        }
                        report_property_errors_recursively(nested.properties, report_generator);
                    }
                    CssRulesetProperty::Error(error) => {
                        report_generator.add_error(error);
                    }
                    _ => {}
                }
            }
        }

        fn process_ruleset_recursively(
            ruleset: CssRuleset,
            parent_selectors: Option<&Vec<CssSelector>>,
            builder: &mut StyleSheetBuilder,
            report_generator: &mut ErrorReportGenerator,
        ) {
            match ruleset.selectors {
                Ok(selectors) => {
                    debug_assert!(!selectors.is_empty());
                    let mut ruleset_builder = builder.new_ruleset();

                    let selectors = match parent_selectors {
                        None => selectors,
                        Some(parent_selectors) => selectors
                            .into_iter()
                            .map(|s| s.replace_parent_selector(parent_selectors))
                            .collect(),
                    };

                    for selector in selectors.iter().cloned() {
                        ruleset_builder.add_css_selector(selector);
                    }

                    for property in ruleset.properties {
                        match property {
                            CssRulesetProperty::SingleProperty(id, value, important_level) => {
                                ruleset_builder
                                    .add_properties([(ComponentPropertyRef::Id(id), value)]);
                                report_important_level(report_generator, important_level);
                            }
                            CssRulesetProperty::MultipleProperties(properties, important_level) => {
                                ruleset_builder.add_properties(
                                    properties
                                        .into_iter()
                                        .map(|(id, value)| (ComponentPropertyRef::Id(id), value)),
                                );
                                report_important_level(report_generator, important_level);
                            }
                            CssRulesetProperty::DynamicProperty(
                                css_name,
                                parser,
                                tokens,
                                important_level,
                            ) => {
                                ruleset_builder.add_properties([StyleBuilderProperty::Dynamic {
                                    css_name,
                                    parser,
                                    tokens,
                                }]);
                                report_important_level(report_generator, important_level);
                            }
                            CssRulesetProperty::Transitions(property_transitions) => {
                                for transition_fallible in property_transitions {
                                    match transition_fallible {
                                        Ok(CssTransitionProperty {
                                            properties,
                                            options,
                                        }) => {
                                            for property in properties {
                                                ruleset_builder.add_property_transition(
                                                    property,
                                                    options.clone(),
                                                );
                                            }
                                        }
                                        Err(error) => {
                                            report_generator.add_error(error);
                                        }
                                    }
                                }
                            }
                            CssRulesetProperty::Animations(animations) => {
                                for animation_fallible in animations {
                                    match animation_fallible {
                                        Ok(CssAnimation { name, options }) => {
                                            ruleset_builder.add_animation(name, options);
                                        }
                                        Err(error) => {
                                            report_generator.add_error(error);
                                        }
                                    }
                                }
                            }
                            CssRulesetProperty::Var(var_name, tokens) => {
                                ruleset_builder.add_var(var_name, tokens);
                            }
                            CssRulesetProperty::NestedRuleset(nested_ruleset) => {
                                process_ruleset_recursively(
                                    nested_ruleset,
                                    Some(&selectors),
                                    builder,
                                    report_generator,
                                );
                                ruleset_builder = builder.new_ruleset();
                                for selector in selectors.iter().cloned() {
                                    ruleset_builder.add_css_selector(selector);
                                }
                            }
                            CssRulesetProperty::Error(error) => {
                                report_generator.add_error(error);
                            }
                        }
                    }
                }
                Err(selectors_error) => {
                    report_generator.add_error(selectors_error);
                    // We just try to find the error properties to report them too
                    report_property_errors_recursively(ruleset.properties, report_generator);
                }
            }
        }

        fn processor(
            item: CssStyleSheetItem,
            builder: &mut StyleSheetBuilder,
            report_generator: &mut ErrorReportGenerator,
            roots: &Vec<PathBuf>,
            path: &PathBuf,
        ) {
            match item {
                CssStyleSheetItem::EmbedStylesheet(style_sheet, layer) => {
                    builder.embed_style_sheet(style_sheet, layer);
                }
                CssStyleSheetItem::Inner(items) => {
                    for item in items {
                        processor(item, builder, report_generator, roots, path);
                    }
                }
                CssStyleSheetItem::LayersDefinition(layers) => {
                    builder.define_layers(&layers);
                }
                CssStyleSheetItem::RuleSet(ruleset) => {
                    process_ruleset_recursively(ruleset, None, builder, report_generator);
                }
                CssStyleSheetItem::FontFace(font_face) => {
                    for error in font_face.errors {
                        report_generator.add_error(error);
                    }
                    //builder.register_font_face(font_face.family_name, get_sandboxed_path(&roots, font_face.source.as_str(), path.parent().unwrap().as).unwrap().to_str().unwrap().to_string());

                    // instead we will only register if we find it
                    if let Some(parent) = path.parent() {
                        if let Some(sandboxed_path) =
                            get_sandboxed_path(roots, font_face.source.as_str(), &parent.to_path_buf())
                        {
                            builder.register_font_face(
                                font_face.family_name,
                                sandboxed_path.to_str().unwrap().to_string(),
                            );
                        } else {
                            report_generator.add_error(CssError::new_unlocated(CssErrorCode::new_custom(94, "aaa"), "Couldnt find  font!!"));
                        }
                    } else {
                        report_generator.add_error(CssError::new_unlocated(CssErrorCode::new_custom(94, "aaa"), "Couldnt find  font!!"));
                    }
                }
                CssStyleSheetItem::AnimationKeyFrames(keyframes) => {
                    let name = keyframes.name;

                    let mut keyframes_per_property =
                        PropertiesHashMap::<AnimationKeyframesBuilder>::default();

                    for k in keyframes.keyframes {
                        match k {
                            AnimationKeyFrame::Valid { times, properties } => {
                                for property in properties {
                                    match property {
                                        CssRulesetProperty::SingleProperty(
                                            property_id,
                                            PropertyValue::Value(sample),
                                            _,
                                        ) => {
                                            for time in &times {
                                                keyframes_per_property
                                                    .entry(property_id)
                                                    .or_default()
                                                    .add_keyframe_reflect_value(
                                                        *time,
                                                        sample.clone(),
                                                    );
                                            }
                                        }
                                        CssRulesetProperty::Error(error) => {
                                            report_generator.add_error(error);
                                        }
                                        _ => {}
                                    }
                                }
                            }
                            AnimationKeyFrame::Error(error) => {
                                report_generator.add_error(error);
                            }
                        }
                    }

                    builder.add_animation_keyframes(
                        name,
                        keyframes_per_property
                            .into_iter()
                            .filter_map(|(p, b)| Some((p, b.build().ok()?))),
                    );
                }
                CssStyleSheetItem::Error(error) => {
                    report_generator.add_error(error);
                }
            }
        }

        parse_css(
            &type_registry,
            &self.property_registry,
            &self.shorthand_property_registry,
            &imports,
            &contents,
            |item| {
                processor(item, &mut builder, &mut report_generator, roots, path);
            },
        );

        if report_generator.contains_errors() {
            match settings.error_mode {
                CssStyleLoaderErrorMode::PrintWarn => {
                    warn!("\n{}", report_generator.into_message());
                }
                CssStyleLoaderErrorMode::PrintError => {
                    error!("\n{}", report_generator.into_message());
                }
                CssStyleLoaderErrorMode::ReturnError => {
                    return Err(CssStyleLoaderError::Report(report_generator.into_message()));
                }
                CssStyleLoaderErrorMode::Ignore => {}
            }
        } else if !report_generator.is_empty() {
            info!("\n{}", report_generator.into_message());
        }

        builder.remove_all_empty_rulesets();
        Ok(builder.build_with_asset_server(&self.property_registry, asset_server)?)
    }
}

impl AssetLoader for CssStyleLoader {
    type Asset = StyleSheet;
    type Settings = CssStyleLoaderSetting;
    type Error = CssStyleLoaderError;

    async fn load(
        &self,
        reader: &mut dyn Reader,
        settings: &Self::Settings,
        load_context: &mut LoadContext<'_>,
    ) -> Result<Self::Asset, Self::Error> {
        let mut contents = String::new();
        reader.read_to_string(&mut contents).await?;

        // First we try to extract all imports
        let mut import_paths = Vec::new();

        extract_imports(&contents, |item| {
            import_paths.push(String::from(item.as_ref()));
        });

        let mut imports = FxHashMap::default();

        for import_path in import_paths {
            let loaded_asset = load_context
                .loader()
                .immediate()
                .load::<StyleSheet>(&import_path)
                .await?;
            imports.insert(import_path, loaded_asset.take());
        }

        let path_display = load_context.path().display().to_string();
        let type_registry = self.type_registry_arc.read();

        let mut builder = StyleSheetBuilder::new();
        let mut report_generator = ErrorReportGenerator::new(&path_display, &contents);

        fn report_important_level(
            report_generator: &mut ErrorReportGenerator,
            level: ImportantLevel,
        ) {
            if let ImportantLevel::Important(location) = level {
                report_generator.add_advice(
                    location,
                    "!important is not supported",
                    "!important token is being ignored, so you can remove it",
                );
            }
        }

        fn report_property_errors_recursively(
            properties: Vec<CssRulesetProperty>,
            report_generator: &mut ErrorReportGenerator,
        ) {
            for property in properties {
                match property {
                    CssRulesetProperty::NestedRuleset(nested) => {
                        if let Err(selectors_error) = nested.selectors {
                            report_generator.add_error(selectors_error);
                        }
                        report_property_errors_recursively(nested.properties, report_generator);
                    }
                    CssRulesetProperty::Error(error) => {
                        report_generator.add_error(error);
                    }
                    _ => {}
                }
            }
        }

        fn process_ruleset_recursively(
            ruleset: CssRuleset,
            parent_selectors: Option<&Vec<CssSelector>>,
            builder: &mut StyleSheetBuilder,
            report_generator: &mut ErrorReportGenerator,
        ) {
            match ruleset.selectors {
                Ok(selectors) => {
                    debug_assert!(!selectors.is_empty());
                    let mut ruleset_builder = builder.new_ruleset();

                    let selectors = match parent_selectors {
                        None => selectors,
                        Some(parent_selectors) => selectors
                            .into_iter()
                            .map(|s| s.replace_parent_selector(parent_selectors))
                            .collect(),
                    };

                    for selector in selectors.iter().cloned() {
                        ruleset_builder.add_css_selector(selector);
                    }

                    for property in ruleset.properties {
                        match property {
                            CssRulesetProperty::SingleProperty(id, value, important_level) => {
                                ruleset_builder
                                    .add_properties([(ComponentPropertyRef::Id(id), value)]);
                                report_important_level(report_generator, important_level);
                            }
                            CssRulesetProperty::MultipleProperties(properties, important_level) => {
                                ruleset_builder.add_properties(
                                    properties
                                        .into_iter()
                                        .map(|(id, value)| (ComponentPropertyRef::Id(id), value)),
                                );
                                report_important_level(report_generator, important_level);
                            }
                            CssRulesetProperty::DynamicProperty(
                                css_name,
                                parser,
                                tokens,
                                important_level,
                            ) => {
                                ruleset_builder.add_properties([StyleBuilderProperty::Dynamic {
                                    css_name,
                                    parser,
                                    tokens,
                                }]);
                                report_important_level(report_generator, important_level);
                            }
                            CssRulesetProperty::Transitions(property_transitions) => {
                                for transition_fallible in property_transitions {
                                    match transition_fallible {
                                        Ok(CssTransitionProperty {
                                            properties,
                                            options,
                                        }) => {
                                            for property in properties {
                                                ruleset_builder.add_property_transition(
                                                    property,
                                                    options.clone(),
                                                );
                                            }
                                        }
                                        Err(error) => {
                                            report_generator.add_error(error);
                                        }
                                    }
                                }
                            }
                            CssRulesetProperty::Animations(animations) => {
                                for animation_fallible in animations {
                                    match animation_fallible {
                                        Ok(CssAnimation { name, options }) => {
                                            ruleset_builder.add_animation(name, options);
                                        }
                                        Err(error) => {
                                            report_generator.add_error(error);
                                        }
                                    }
                                }
                            }
                            CssRulesetProperty::Var(var_name, tokens) => {
                                ruleset_builder.add_var(var_name, tokens);
                            }
                            CssRulesetProperty::NestedRuleset(nested_ruleset) => {
                                process_ruleset_recursively(
                                    nested_ruleset,
                                    Some(&selectors),
                                    builder,
                                    report_generator,
                                );
                                ruleset_builder = builder.new_ruleset();
                                for selector in selectors.iter().cloned() {
                                    ruleset_builder.add_css_selector(selector);
                                }
                            }
                            CssRulesetProperty::Error(error) => {
                                report_generator.add_error(error);
                            }
                        }
                    }
                }
                Err(selectors_error) => {
                    report_generator.add_error(selectors_error);
                    // We just try to find the error properties to report them too
                    report_property_errors_recursively(ruleset.properties, report_generator);
                }
            }
        }

        fn processor(
            item: CssStyleSheetItem,
            builder: &mut StyleSheetBuilder,
            report_generator: &mut ErrorReportGenerator,
        ) {
            match item {
                CssStyleSheetItem::EmbedStylesheet(style_sheet, layer) => {
                    builder.embed_style_sheet(style_sheet, layer);
                }
                CssStyleSheetItem::Inner(items) => {
                    for item in items {
                        processor(item, builder, report_generator);
                    }
                }
                CssStyleSheetItem::LayersDefinition(layers) => {
                    builder.define_layers(&layers);
                }
                CssStyleSheetItem::RuleSet(ruleset) => {
                    process_ruleset_recursively(ruleset, None, builder, report_generator);
                }
                CssStyleSheetItem::FontFace(font_face) => {
                    for error in font_face.errors {
                        report_generator.add_error(error);
                    }
                    builder.register_font_face(font_face.family_name, font_face.source);
                }
                CssStyleSheetItem::AnimationKeyFrames(keyframes) => {
                    let name = keyframes.name;

                    let mut keyframes_per_property =
                        PropertiesHashMap::<AnimationKeyframesBuilder>::default();

                    for k in keyframes.keyframes {
                        match k {
                            AnimationKeyFrame::Valid { times, properties } => {
                                for property in properties {
                                    match property {
                                        CssRulesetProperty::SingleProperty(
                                            property_id,
                                            PropertyValue::Value(sample),
                                            _,
                                        ) => {
                                            for time in &times {
                                                keyframes_per_property
                                                    .entry(property_id)
                                                    .or_default()
                                                    .add_keyframe_reflect_value(
                                                        *time,
                                                        sample.clone(),
                                                    );
                                            }
                                        }
                                        CssRulesetProperty::Error(error) => {
                                            report_generator.add_error(error);
                                        }
                                        _ => {}
                                    }
                                }
                            }
                            AnimationKeyFrame::Error(error) => {
                                report_generator.add_error(error);
                            }
                        }
                    }

                    builder.add_animation_keyframes(
                        name,
                        keyframes_per_property
                            .into_iter()
                            .filter_map(|(p, b)| Some((p, b.build().ok()?))),
                    );
                }
                CssStyleSheetItem::Error(error) => {
                    report_generator.add_error(error);
                }
            }
        }

        parse_css(
            &type_registry,
            &self.property_registry,
            &self.shorthand_property_registry,
            &imports,
            &contents,
            |item| {
                processor(item, &mut builder, &mut report_generator);
            },
        );

        if report_generator.contains_errors() {
            match settings.error_mode {
                CssStyleLoaderErrorMode::PrintWarn => {
                    warn!("\n{}", report_generator.into_message());
                }
                CssStyleLoaderErrorMode::PrintError => {
                    error!("\n{}", report_generator.into_message());
                }
                CssStyleLoaderErrorMode::ReturnError => {
                    return Err(CssStyleLoaderError::Report(report_generator.into_message()));
                }
                CssStyleLoaderErrorMode::Ignore => {}
            }
        } else if !report_generator.is_empty() {
            info!("\n{}", report_generator.into_message());
        }

        builder.remove_all_empty_rulesets();
        Ok(builder.build_with_load_context(&self.property_registry, load_context)?)
    }

    fn extensions(&self) -> &[&str] {
        Self::EXTENSIONS
    }
}
