use std::process::{Command, Output};

fn run_git(args: &[&str]) -> Output {
    Command::new("git")
        .args(args)
        .output()
        .expect("failed to execture git command")
}

fn git_describe() -> Output {
    run_git(&["describe", "--tags"])
}

fn git_rev_parse() -> Output {
    run_git(&["rev-parse", "HEAD"])
}

fn version() -> Vec<u8> {
    let describe = git_describe();
    match describe.status.success() {
        true => describe.stdout,
        false => {
            let rev_parse = git_rev_parse();
            match rev_parse.status.success() {
                true => ["0.0.0-".as_bytes(), &rev_parse.stdout[0..7]].concat(),
                false => panic!("could not retrieve version from git"),
            }
        }
    }
}

fn main() {
    let version = String::from_utf8(version()).expect("could not decode git version");
    println!("cargo:rustc-env=CONFILANG_VERSION={}", version);
}
