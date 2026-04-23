// -*- combobulate-test-point-overlays: ((1 outline 187) (2 outline 214) (3 outline 232) (4 outline 246) (5 outline 265)); eval: (combobulate-test-fixture-mode t); -*-
struct User {
    #[serde(rename = "n")]
    name: String,
    age: u32,
    #[serde(skip)]
    internal: bool,
}
