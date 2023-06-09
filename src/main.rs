use std::collections::{HashMap, HashSet};

use saida::Expr;

fn main() {
    // (\x. \y. x)(y) => \y'. y
    let e = Expr::App(
        Box::new(Expr::Lam(
            "x".to_string(),
            Box::new(Expr::Lam(
                "y".to_string(),
                Box::new(Expr::Var("y".to_string())),
            )),
        )),
        Box::new(Expr::Var("y".to_string())),
    );

    let d = HashMap::new();
    let v = e.eval(&d);
    let mut xs = HashSet::new();
    xs.insert("y");
    println!("{:?}", v.quote(&xs));
}
