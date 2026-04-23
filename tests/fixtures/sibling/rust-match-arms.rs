// -*- combobulate-test-point-overlays: ((1 outline 205) (2 outline 232) (3 outline 272)); eval: (combobulate-test-fixture-mode t); -*-
fn describe(x: Option<i32>) -> &'static str {
    match x {
        Some(0) => "zero",
        Some(n) if n > 0 => "positive",
        None => "nothing",
        _ => "other",
    }
}
