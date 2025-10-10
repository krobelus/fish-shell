fn main() {
    // For debug builds, embedded files are not actually embedded but read from the file system, so
    // we only need to rebuild if rust-embed actually embeds the FTL files into the binary.
    #[cfg(any(not(debug_assertions), windows))]
    {
        use fish_build_helper::{rebuild_if_path_changed, workspace_root};
        rebuild_if_path_changed(workspace_root().join("localization").join("fluent"));
    }
}
