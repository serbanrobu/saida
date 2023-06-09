use std::collections::{HashMap, HashSet};

pub type Identifier = String;

pub type Env = HashMap<Identifier, Value>;

pub type Context = HashMap<Identifier, Type>;

pub type Type = Value;

pub type Level = u8;

pub type Error = &'static str;

#[derive(Clone, Debug)]
pub enum Expr {
    App(Box<Expr>, Box<Expr>),
    Fun(Box<Expr>, Box<Expr>),
    Lam(Identifier, Box<Expr>),
    Sub(Identifier, Box<Expr>, Box<Expr>),
    U(Level),
    Var(Identifier),
}

impl PartialEq for Expr {
    fn eq(&self, other: &Self) -> bool {
        self.alpha_eq(other, 0, &HashMap::new(), &HashMap::new())
    }
}

impl Expr {
    pub fn alpha_eq(
        &self,
        other: &Self,
        i: usize,
        xs: &HashMap<&str, usize>,
        ys: &HashMap<&str, usize>,
    ) -> bool {
        match (self, other) {
            (Self::App(e_1, e_2), Self::App(e_3, e_4)) => {
                e_1.alpha_eq(e_3, i, xs, ys) && e_2.alpha_eq(e_4, i, xs, ys)
            }
            (Self::Lam(x, e_1), Self::Lam(y, e_2)) => e_1.alpha_eq(
                e_2,
                i + 1,
                &{
                    let mut xs_ = xs.to_owned();
                    xs_.insert(x, i);
                    xs_
                },
                &{
                    let mut ys_ = ys.to_owned();
                    ys_.insert(y, i);
                    ys_
                },
            ),
            (Self::Var(x), Self::Var(y)) => match (xs.get(x.as_str()), ys.get(y.as_str())) {
                (None, None) => x == y,
                (Some(j), Some(k)) => j == k,
                _ => false,
            },
            _ => panic!(),
        }
    }

    pub fn check(&self, t: &Type, cx: &Context) -> Result<(), Error> {
        match (self, t) {
            (Self::Fun(e_1, e_2), Type::U(_)) => {
                e_1.check(t, cx)?;
                e_2.check(t, cx)
            }
            (Self::Lam(x, e), Type::Fun(t_1, t_2)) => {
                let mut cx_ = cx.to_owned();
                cx_.insert(x.to_owned(), t_1.as_ref().to_owned());
                e.check(t_2, &cx_)
            }
            (Self::Sub(x, e_1, e_2), _) => {
                let t_1 = e_1.infer(cx)?;
                let mut cx_ = cx.to_owned();
                cx_.insert(x.to_owned(), t_1);
                e_2.check(t, &cx_)
            }
            (Self::U(i), Type::U(j)) if i < j => Ok(()),
            _ => {
                let t_ = self.infer(cx)?;
                let xs = cx.keys().map(String::as_str).collect::<HashSet<&str>>();

                if t_.quote(&xs) != t.quote(&xs) {
                    return Err("type mismatch");
                };

                Ok(())
            }
        }
    }

    pub fn eval(&self, d: &Env) -> Value {
        match self {
            Self::App(e_1, e_2) => match e_1.eval(d) {
                Value::Lam(x, e, mut d_) => {
                    d_.insert(x, e_2.eval(d));
                    e.eval(&d_)
                }
                Value::Neutral(n) => {
                    Value::Neutral(Neutral::App(Box::new(n), Box::new(e_2.eval(d))))
                }
                _ => panic!(),
            },
            Self::Fun(e_1, e_2) => Value::Fun(Box::new(e_1.eval(d)), Box::new(e_2.eval(d))),
            Self::Lam(x, e) => Value::Lam(x.to_owned(), e.to_owned(), d.to_owned()),
            Self::Sub(x, e_1, e_2) => {
                let v = e_1.eval(d);
                let mut d_1 = d.to_owned();
                d_1.insert(x.to_owned(), v);
                e_2.eval(&d_1)
            }
            &Self::U(i) => Value::U(i),
            Self::Var(x) => d
                .get(x)
                .cloned()
                .unwrap_or_else(|| Value::Neutral(Neutral::Var(x.to_owned()))),
        }
    }

    pub fn infer(&self, cx: &Context) -> Result<Type, Error> {
        match self {
            Self::App(e_1, e_2) => {
                let v = e_1.infer(cx)?;

                let Value::Fun(v_1, v_2) = v else {
                    return Err("not a function");
                };

                e_2.check(&v_1, cx)?;
                Ok(*v_2)
            }
            Self::Sub(x, e_1, e_2) => {
                let t_1 = e_1.infer(cx)?;
                let mut cx_ = cx.to_owned();
                cx_.insert(x.to_owned(), t_1);
                e_2.infer(&cx_)
            }
            Self::Var(x) => cx.get(x).cloned().ok_or("unknown identifier"),
            _ => Err("could not infer type"),
        }
    }
}

#[derive(Clone)]
pub enum Neutral {
    App(Box<Neutral>, Box<Value>),
    Var(Identifier),
}

impl Neutral {
    fn quote(&self, xs: &HashSet<&str>) -> Expr {
        match self {
            Self::App(n, v) => Expr::App(Box::new(n.quote(xs)), Box::new(v.quote(xs))),
            Self::Var(x) => Expr::Var(x.to_owned()),
        }
    }
}

#[derive(Clone)]
pub enum Value {
    Fun(Box<Value>, Box<Value>),
    Lam(Identifier, Box<Expr>, Env),
    Neutral(Neutral),
    U(Level),
}

pub fn freshen(mut x: Identifier, xs: &HashSet<&str>) -> Identifier {
    if xs.contains(x.as_str()) {
        x.push('\'');
        freshen(x, xs)
    } else {
        x
    }
}

impl Value {
    pub fn quote(&self, xs: &HashSet<&str>) -> Expr {
        match self {
            Self::Fun(v_1, v_2) => Expr::Fun(Box::new(v_1.quote(xs)), Box::new(v_2.quote(xs))),
            Self::Lam(x, e, d) => {
                let x_ = freshen(x.to_owned(), xs);
                let mut d_ = d.to_owned();
                d_.insert(x_.clone(), Value::Neutral(Neutral::Var(x_.clone())));
                let mut xs_ = xs.to_owned();
                xs_.insert(&x_);
                let e_ = e.eval(&d_).quote(&xs_);
                Expr::Lam(x_, Box::new(e_))
            }
            Self::Neutral(n) => n.quote(xs),
            &Self::U(i) => Expr::U(i),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn quotation_works() {
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

        assert_eq!(
            v.quote(&xs),
            Expr::Lam("y'".to_string(), Box::new(Expr::Var("y".to_string())))
        );
    }
}
