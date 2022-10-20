use std::env;
use std::path::PathBuf;

fn main() -> Result<(), std::env::VarError> {
    let files = ["vhpi/vhpi_user.h"];
    for file in &files {
        println!("cargo:rerun-if-changed={file}");
    }

    let bindings = bindgen::Builder::default()
        .header("vhpi/vhpi_user.h")
        .derive_default(true)
        .derive_debug(true)
        .parse_callbacks(Box::new(bindgen::CargoCallbacks))
        .allowlist_recursively(false)
        .allowlist_file("vhpi/vhpi_user.h")
        .default_macro_constant_type(bindgen::MacroTypeVariation::Signed)
        .default_enum_style(bindgen::EnumVariation::Rust {
            non_exhaustive: true,
        })
        .generate()
        .expect("Unable to generate bindings");

    let out_path = PathBuf::from(env::var("OUT_DIR").unwrap());
    bindings
        .write_to_file(out_path.join("bindings.rs"))
        .expect("Couldn't write bindings!");

    Ok(())
}
