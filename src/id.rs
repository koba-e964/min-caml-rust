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
    fn id_of_typ(t: &Type) -> String {
        match *t {
            Type::Unit => "u",
            Type::Bool => "b",
            Type::Int => "i",
            Type::Float => "d",
            Type::Fun(_, _) => "f",
            Type::Tuple(_) => "t",
            Type::Array(_) => "a",
            Type::Var(_) => panic!("Var is not appropriate"),
        }.to_string()
    }
    pub fn gen_tmp(&mut self, t: &Type) -> String {
        let a = self.gen();
        format!("{}{}", Self::id_of_typ(t), a)
    }
}
