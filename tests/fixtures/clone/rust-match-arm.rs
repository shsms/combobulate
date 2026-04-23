// -*- combobulate-test-point-overlays: ((1 outline 173)); eval: (combobulate-test-fixture-mode t); -*-
fn describe(x: Option<i32>) -> &'static str {
    match x {
        Some(0) => "zero",
        Some(_) => "nonzero",
        None => "nothing",
    }
}
