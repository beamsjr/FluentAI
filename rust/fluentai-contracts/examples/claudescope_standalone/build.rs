use std::env;
use std::fs::{self, File};
use std::io::Write;
use std::path::Path;
use walkdir::WalkDir;

fn main() {
    println!("cargo:rerun-if-changed=../../examples/claudescope");
    
    // Generate embedded FluentAi source files
    let out_dir = env::var("OUT_DIR").unwrap();
    let dest_path = Path::new(&out_dir).join("embedded_sources.rs");
    let mut f = File::create(&dest_path).unwrap();

    writeln!(f, "// Auto-generated file containing embedded FluentAi sources").unwrap();
    writeln!(f, "use std::collections::HashMap;").unwrap();
    writeln!(f, "use once_cell::sync::Lazy;").unwrap();
    writeln!(f).unwrap();
    
    writeln!(f, "pub static CLAUDESCOPE_SOURCES: Lazy<HashMap<&'static str, &'static str>> = Lazy::new(|| {{").unwrap();
    writeln!(f, "    let mut sources = HashMap::new();").unwrap();

    // Find all .cl files in the claudescope directory
    let claudescope_path = Path::new("../../examples/claudescope");
    for entry in WalkDir::new(claudescope_path)
        .follow_links(true)
        .into_iter()
        .filter_map(|e| e.ok())
    {
        let path = entry.path();
        if path.extension().and_then(|s| s.to_str()) == Some("cl") {
            let relative_path = path.strip_prefix(claudescope_path).unwrap();
            let relative_str = relative_path.to_str().unwrap().replace('\\', "/");
            
            let content = fs::read_to_string(path).unwrap();
            writeln!(f, "    sources.insert({:?}, r###\"{}\"###);", 
                     relative_str, 
                     content.replace("\"###", "\"###{")
            ).unwrap();
        }
    }

    writeln!(f, "    sources").unwrap();
    writeln!(f, "}});").unwrap();
}