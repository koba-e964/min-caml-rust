use nom::{alpha, digit};
use syntax::*;
use ordered_float::OrderedFloat;

named!(simple_exp_no_index<Syntax>, alt_complete!(
    ws!(do_parse!(tag!("(") >> res: exp >> tag!(")") >> (res))) |
    ws!(do_parse!(tag!("(") >> tag!(")") >> (Syntax::Unit))) |
    ws!(do_parse!(b: bool_lit >> (Syntax::Bool(b)))) |
    ws!(do_parse!(f: float_lit >> (Syntax::Float(OrderedFloat::from(f))))) |
    ws!(do_parse!(i: int_lit >> (Syntax::Int(i)))) |
    ws!(do_parse!(id: ident >> (Syntax::Var(id))))
));

named!(simple_exp<Syntax>,
       ws!(do_parse!(
           init: simple_exp_no_index >>
               res: fold_many0!(
                   ws!(do_parse!(char!('.') >> char!('(') >> res: exp >> char!(')') >> (res))),
                   init,
                   |acc, index| {
                       Syntax::Get(Box::new(acc), Box::new(index))
                   }
               ) >>
               (res)
       ))
);

named!(pub exp<Syntax>, ws!(exp_let));

named!(exp_let<Syntax>, alt_complete!(
    ws!(do_parse!(
        tag!("let") >>
            id: ident >>
            tag!("=") >>
            e1: exp >>
            tag!("in") >>
            e2: exp_let >>
            (Syntax::Let((id, Type::Unit /* TODO should be uniquely assigned to a var */), Box::new(e1), Box::new(e2)))
    )) |
    ws!(exp_if)
)); // 1/3 done (missing: letrec, lettuple)

named!(exp_if<Syntax>, alt_complete!(
    ws!(do_parse!(
        tag!("if") >>
            e1: exp >>
            tag!("then") >>
            e2: exp >>
            tag!("else") >>
            e3: exp_if >>
            (Syntax::If(Box::new(e1), Box::new(e2), Box::new(e3)))
    )) |
    ws!(exp_assign)
));

macro_rules! stub_rule {
    ($rulename:ident, $redirect_to: ident) => {
        named!($rulename<Syntax>, alt_complete!(
            ws!($redirect_to)
        ));
    }
}

stub_rule!(exp_assign, exp_comma);
stub_rule!(exp_comma, exp_comparative);
stub_rule!(exp_comparative, exp_additive);
stub_rule!(exp_additive, exp_multiplicative);
named!(exp_multiplicative<Syntax>,
       ws!(do_parse!(
           init: exp_unary_minus >>
               res: fold_many0!(
                   ws!(pair!(mult_op, exp_unary_minus)),
                   init,
                   |acc, (op, arg)| {
                       match op {
                           Err(op) =>
                               Syntax::FloatBin(op,
                                                Box::new(acc), Box::new(arg)),
                           Ok(op) =>
                               Syntax::IntBin(op,
                                              Box::new(acc), Box::new(arg)),
                       }
                   }
               ) >>
               (res)
       ))
);
named!(mult_op<Result<IntBin, FloatBin>>, alt_complete!(
    do_parse!(tag!("*.") >> (Err(FloatBin::FMul))) |
    do_parse!(tag!("/.") >> (Err(FloatBin::FDiv)))
));

stub_rule!(exp_unary_minus, exp_app);


named!(exp_app<Syntax>, alt_complete!(
    ws!(do_parse!(tag!("!") >> res: exp_app >> (Syntax::Not(Box::new(res))))) |
    ws!(do_parse!(
        init: simple_exp >>
            res: many1!(ws!(simple_exp)) >>
            (Syntax::App(Box::new(init), res.into_boxed_slice()))
    )) |
    ws!(simple_exp)
));

named!(bool_lit<bool>, alt_complete!(
    ws!(do_parse!(tag!("true") >> (true))) |
    ws!(do_parse!(tag!("false") >> (false)))
));

named!(int_lit<i64>, ws!(do_parse!(
    x: digit >>
        (convert(x, 10))
)));

// TODO supports only digit+ . digit+
named!(float_lit<f64>, ws!(do_parse!(
    fstr: recognize!(do_parse!(
        x: digit >>
            s: preceded!(char!('.'), digit)
            >> (())) ) >>
        (convert_to_f64(fstr))
)));
                          

named!(ident<String>, ws!(do_parse!(
    peek!(not!(reserved)) >>
    s: alpha >>
        (String::from_utf8(s.to_vec()).unwrap())
)));

named!(reserved, alt_complete!(
    tag!("let") |
    tag!("in") |
    tag!("true") |
    tag!("false") |
    tag!("if") |
    tag!("then") |
    tag!("else")
));


fn convert(x: &[u8], radix: i64) -> i64 {
    let mut cur = 0;
    for &v in x.iter() {
        cur *= radix;
        cur += v as i64 - b'0' as i64;
    }
    cur
}
fn convert_to_f64(x: &[u8]) -> f64 {
    let mut cur = 0.0;
    let mut dot = false;
    let mut base = 1.0;
    for &v in x.iter() {
        let mut tmp = 0.0;
        if v == b'.' {
            dot = true;
        } else {
            tmp = (v as i64 - b'0' as i64) as f64 * base;
        }
        if !dot {
            cur *= 10.0;
        }
        cur += tmp;
        if dot {
            base /= 10.0;
        }
    }
    cur
}

/*
simple_exp: /* (* 括弧をつけなくても関数の引数になれる式 (caml2html: parser_simple) *) */
| LPAREN exp RPAREN
    { $2 }
| LPAREN RPAREN
    { Unit }
| BOOL
    { Bool($1) }
| INT
    { Int($1) }
| FLOAT
    { Float($1) }
| IDENT
    { Var($1) }
| simple_exp DOT LPAREN exp RPAREN
    { Get($1, $4) }

exp: /* (* 一般の式 (caml2html: parser_exp) *) */
| simple_exp
    { $1 }
| NOT exp
    %prec prec_app
    { Not($2) }
| MINUS exp
    %prec prec_unary_minus
    { match $2 with
    | Float(f) -> Float(-.f) (* -1.23などは型エラーではないので別扱い *)
    | e -> Neg(e) }
| exp PLUS exp /* (* 足し算を構文解析するルール (caml2html: parser_add) *) */
    { Add($1, $3) }
| exp MINUS exp
    { Sub($1, $3) }
| exp EQUAL exp
    { Eq($1, $3) }
| exp LESS_GREATER exp
    { Not(Eq($1, $3)) }
| exp LESS exp
    { Not(LE($3, $1)) }
| exp GREATER exp
    { Not(LE($1, $3)) }
| exp LESS_EQUAL exp
    { LE($1, $3) }
| exp GREATER_EQUAL exp
    { LE($3, $1) }
| IF exp THEN exp ELSE exp
    %prec prec_if
    { If($2, $4, $6) }
| MINUS_DOT exp
    %prec prec_unary_minus
    { FNeg($2) }
| exp PLUS_DOT exp
    { FAdd($1, $3) }
| exp MINUS_DOT exp
    { FSub($1, $3) }
| exp AST_DOT exp
    { FMul($1, $3) }
| exp SLASH_DOT exp
    { FDiv($1, $3) }
| LET IDENT EQUAL exp IN exp
    %prec prec_let
    { Let(addtyp $2, $4, $6) }
| LET REC fundef IN exp
    %prec prec_let
    { LetRec($3, $5) }
| exp actual_args
    %prec prec_app
    { App($1, $2) }
| elems
    { Tuple($1) }
| LET LPAREN pat RPAREN EQUAL exp IN exp
    { LetTuple($3, $6, $8) }
| simple_exp DOT LPAREN exp RPAREN LESS_MINUS exp
    { Put($1, $4, $7) }
| exp SEMICOLON exp
    { Let((Id.gentmp Type.Unit, Type.Unit), $1, $3) }
| ARRAY_CREATE simple_exp simple_exp
    %prec prec_app
    { Array($2, $3) }
| error
    { failwith
	(Printf.sprintf "parse error near characters %d-%d"
	   (Parsing.symbol_start ())
	   (Parsing.symbol_end ())) }

fundef:
| IDENT formal_args EQUAL exp
    { { name = addtyp $1; args = $2; body = $4 } }

formal_args:
| IDENT formal_args
    { addtyp $1 :: $2 }
| IDENT
    { [addtyp $1] }

actual_args:
| actual_args simple_exp
    %prec prec_app
    { $1 @ [$2] }
| simple_exp
    %prec prec_app
    { [$1] }

elems:
| elems COMMA exp
    { $1 @ [$3] }
| exp COMMA exp
    { [$1; $3] }

pat:
| pat COMMA IDENT
    { $1 @ [addtyp $3] }
| IDENT COMMA IDENT
    { [addtyp $1; addtyp $3] }
*/


/*
Operator precedence:

%right prec_let
%right SEMICOLON
%right prec_if
%right LESS_MINUS
%left COMMA
%left EQUAL LESS_GREATER LESS GREATER LESS_EQUAL GREATER_EQUAL
%left PLUS MINUS PLUS_DOT MINUS_DOT
%left AST_DOT SLASH_DOT
%right prec_unary_minus
%left prec_app
%left DOT
*/

#[cfg(test)]
mod tests {
    use parser::exp;
    use ordered_float::OrderedFloat;
    #[test]
    fn test_simple_exp() {
        use nom::IResult;
        use syntax::Syntax::*;
        assert_eq!(exp(b" ( c)"), IResult::Done(&[0u8; 0][..],
                                                Var("c".to_string())));
        assert_eq!(exp(b"() "), IResult::Done(&[0u8; 0][..], Unit));
        assert_eq!(exp(b"100"), IResult::Done(&[0u8; 0][..], Int(100)));
        assert_eq!(exp(b"10.0"), IResult::Done(
            &[0u8; 0][..], Float(OrderedFloat::from(10.0))));
        assert_eq!(exp(b"1256.25"),
                   IResult::Done(&[0u8; 0][..],
                                 Float(OrderedFloat::from(1256.25))));
        assert_eq!(exp(b"a.(b)"),
                   IResult::Done(&[0u8; 0][..],
                                 Get(Box::new(Var("a".to_string())),
                                     Box::new(Var("b".to_string())))));
        assert_eq!(exp(b"a.(b).(c)"),
                   IResult::Done(&[0u8; 0][..],
                                 Get(
                                     Box::new(
                                         Get(Box::new(Var("a".to_string())),
                                             Box::new(Var("b".to_string())))),
                                     Box::new(Var("c".to_string())))));
    }

    #[test]
    fn test_let() {
        use syntax::Syntax::{Int, Let};
        let result = exp(b"let x = 0 in 1").unwrap();
        assert_eq!(result.0, &[0u8; 0]);
        match result.1 {
            Let((id, _), e1, e2) => {
                assert_eq!(id, "x");
                assert_eq!(*e1, Int(0));
                assert_eq!(*e2, Int(1));
            }
            _ => panic!(),
        }
    }
    
    #[test]
    fn test_if() {
        use nom::IResult;
        use syntax::Syntax::{App, Int, If, Var};
        assert_eq!(exp(b"if f 3 then 4 else 0"),
                   IResult::Done(
                       &[0u8; 0][..],
                       If(
                           Box::new(App(Box::new(Var("f".to_string())),
                                        vec![Int(3)].into_boxed_slice())),
                           Box::new(Int(4)),
                           Box::new(Int(0)))));
    }

    #[test]
    fn test_mult() {
        use nom::IResult;
        use syntax::Syntax;
        use syntax::Syntax::Var;
        use syntax::FloatBin;
        assert_eq!(exp(b"x *. y"),
                   IResult::Done(&[0u8; 0][..],
                                 Syntax::FloatBin(
                                     FloatBin::FMul,
                                     Box::new(Var("x".to_string())),
                                     Box::new(Var("y".to_string())))));
        assert_eq!(exp(b"x /. y *. z"),
                   IResult::Done(&[0u8; 0][..],
                                 Syntax::FloatBin(
                                     FloatBin::FMul,
                                     Box::new(Syntax::FloatBin(
                                         FloatBin::FDiv,
                                         Box::new(Var("x".to_string())),
                                         Box::new(Var("y".to_string())))),
                                     Box::new(Var("z".to_string())))));
    }

    #[test]
    fn test_exp_not() {
        use nom::IResult;
        use syntax::Syntax::{Bool, Not};
        assert_eq!(exp(b"!!true"),
                   IResult::Done(&[0u8; 0][..],
                                 Not(
                                     Box::new(Not(
                                         Box::new(Bool(true)))))));
    }
    
    #[test]
    fn test_app() {
        use nom::IResult;
        use syntax::Syntax::{App, Int, Var};
        assert_eq!(exp(b"func 0 1"),
                   IResult::Done(&[0u8; 0][..],
                                 App(
                                     Box::new(Var("func".to_string())),
                                     vec![Int(0), Int(1)].into_boxed_slice())));
    }
}
