// -*- combobulate-test-point-overlays: ((1 outline 183) (2 outline 247) (3 outline 302)); eval: (combobulate-test-fixture-mode t); -*-
struct Item { name: String }

impl Item {
    fn new(name: String) -> Self {
        Item { name }
    }

    fn name(&self) -> &str {
        &self.name
    }

    fn rename(&mut self, n: String) {
        self.name = n;
    }
}
