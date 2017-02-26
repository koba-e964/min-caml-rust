use syntax::Type;

pub struct IdGen {
    x: usize,
}

impl IdGen {
    pub fn new() -> Self {
        IdGen { x: 0 }
    }
    pub fn gen(&mut self) -> usize {
        let a = self.x;
        self.x += 1;
        a
    }
    pub fn gen_type(&mut self) -> Type {
        let a = self.gen();
        Type::Var(a)
    }
    pub fn gen_tmp(&mut self, t: &Type) -> String {
        let a = self.gen();
        format!("v{}", a)
    }
}
