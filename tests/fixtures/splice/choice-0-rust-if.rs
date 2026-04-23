// -*- combobulate-test-point-overlays: ((1 outline 149)); eval: (combobulate-test-fixture-mode t); -*-
fn gate(cond: bool) {
    if cond {
        let a = 1;
        println!("{}", a);
    }
}
