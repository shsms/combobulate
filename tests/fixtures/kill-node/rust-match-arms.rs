// -*- combobulate-test-point-overlays: ((1 outline 189) (2 outline 216)); eval: (combobulate-test-fixture-mode t); -*-
fn describe(x: Option<i32>) -> &'static str {
    match x {
        Some(0) => "zero",
        None => "nothing",
        _ => "other",
    }
}
