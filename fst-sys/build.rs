use std::env;
use std::path::PathBuf;

fn main() -> Result<(), std::env::VarError> {
    println!("cargo:rerun-if-changed=fst/fstapi.h");

    let bindings = bindgen::Builder::default()
        .header("fst/fstapi.h")
        .parse_callbacks(Box::new(bindgen::CargoCallbacks))
        .allowlist_recursively(false)
        .default_enum_style(bindgen::EnumVariation::Rust {
            non_exhaustive: true,
        })
        .derive_default(true)
        .derive_debug(true)
        .rust_target(bindgen::RustTarget::Stable_1_47)
        .allowlist_type("fst.*")
        .allowlist_function("fstWriter.*")
        .allowlist_function("fstUtility.*")
        .generate()
        .expect("Unable to generate bindings");

    let out_path = PathBuf::from(env::var("OUT_DIR").unwrap());
    bindings
        .write_to_file(out_path.join("bindings.rs"))
        .expect("Couldn't write bindings!");

    let mut build = cc::Build::new();
    build
        .warnings(false)
        .define("FST_CONFIG_INCLUDE", "\"fstapi.h\"")
        .define("FST_REMOVE_DUPLICATE_VC", "1")
        // .define("FST_DEBUG", "1")
        .file("fst/fstapi.c")
        .file("fst/fastlz.c")
        .file("fst/lz4.c")
        .compile("fstlib");
    println!("cargo:rustc-link-lib=static=z");

    Ok(())
}
