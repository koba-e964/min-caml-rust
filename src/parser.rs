extern crate std;
use nom::{IResult, digit};
use nom::types::CompleteByteSlice;
use syntax::*;
use id::IdGen;
use ordered_float::OrderedFloat;


pub fn parse(x: &[u8]) -> IResult<CompleteByteSlice, Syntax> {
    exp(CompleteByteSlice(x))
}

named!(simple_exp_no_index<CompleteByteSlice, Syntax>, alt!(
    ws!(do_parse!(tag!("(") >> res: exp >> tag!(")") >> (res))) |
    ws!(do_parse!(tag!("(") >> tag!(")") >> (Syntax::Unit))) |
    ws!(do_parse!(b: bool_lit >> (Syntax::Bool(b)))) |
    ws!(do_parse!(f: float_lit >> (Syntax::Float(OrderedFloat::from(f))))) |
    ws!(do_parse!(i: int_lit >> (Syntax::Int(i)))) |
    ws!(do_parse!(id: ident >> (Syntax::Var(id))))
));

named!(simple_exp<CompleteByteSlice, Syntax>,
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
named!(array_index_list<CompleteByteSlice, (Syntax, Vec<Syntax>)>,
       ws!(do_parse!(
           init: simple_exp_no_index >>
               res: fold_many1!(
                   ws!(do_parse!(char!('.') >> char!('(') >> res: exp >> char!(')') >> (res))),
                   Vec::new(),
                   |mut acc: Vec<Syntax>, index| {
                       acc.push(index);
                       acc
                   }
               ) >>
               ((init, res))
       ))
);

named!(exp<CompleteByteSlice, Syntax>, call!(exp_let));

named!(exp_let<CompleteByteSlice, Syntax>, alt!(
    ws!(do_parse!(
        tag!("let") >>
            id: ident >>
            tag!("=") >>
            e1: exp >>
            tag!("in") >>
            e2: exp_let >>
            (Syntax::Let((id, Type::Var(0) /* stub type */), Box::new(e1), Box::new(e2)))
    )) |
    ws!(do_parse!(
        tag!("let") >>
            tag!("rec") >>
            f: fundef >>
            tag!("in") >>
            e: exp_let >>
            (Syntax::LetRec(f, Box::new(e)))
    )) |
    ws!(do_parse!(
        tag!("let") >>
            tag!("(") >>
            p: pat >>
            tag!(")") >>
            tag!("=") >>
            e1: exp >>
            tag!("in") >>
            e2: exp_let >>
            (Syntax::LetTuple(p.into_boxed_slice(), Box::new(e1), Box::new(e2)))
    )) |
    call!(exp_semicolon)
));

named!(exp_let_after_if<CompleteByteSlice, Syntax>, alt!(
    ws!(do_parse!(
        tag!("let") >>
            id: ident >>
            tag!("=") >>
            e1: exp >>
            tag!("in") >>
            e2: exp_let_after_if >>
            (Syntax::Let((id, Type::Var(0) /* stub type */), Box::new(e1), Box::new(e2)))
    )) |
    ws!(do_parse!(
        tag!("let") >>
            tag!("rec") >>
            f: fundef >>
            tag!("in") >>
            e: exp_let_after_if >>
            (Syntax::LetRec(f, Box::new(e)))
    )) |
    ws!(do_parse!(
        tag!("let") >>
            tag!("(") >>
            p: pat >>
            tag!(")") >>
            tag!("=") >>
            e1: exp >>
            tag!("in") >>
            e2: exp_let_after_if >>
            (Syntax::LetTuple(p.into_boxed_slice(), Box::new(e1), Box::new(e2)))
    )) |
    call!(exp_if)
));

named!(pat<CompleteByteSlice, Vec<(String, Type)> >, ws!(do_parse!(
    init: ident >>
        res: fold_many1!(
            ws!(preceded!(tag!(","), ident)),
            vec![(init, Type::Var(0))],
            |mut acc: Vec<(String, Type)>, x| {
                acc.push((x, Type::Var(0)));
                acc
            }
        ) >>
        (res)
)));

named!(fundef<CompleteByteSlice, Fundef>,
       ws!(do_parse!(
           funname: ident >>
               args: formal_args >>
               tag!("=") >>
               e: exp >>
               (Fundef {name: (funname, Type::Var(0) /* stub type */),
                        args: args.into_boxed_slice(),
                        body: Box::new(e)})
       ))
);

named!(formal_args<CompleteByteSlice, Vec<(String, Type)>>,
       ws!(do_parse!(
           x: many1!(ident) >>
               (x.into_iter().map(|x| (x, Type::Var(0) /* stub type */)).collect())
       ))
);

named!(exp_semicolon<CompleteByteSlice, Syntax>, alt!(
    ws!(do_parse!(
        e1: exp_if >>
            tag!(";") >>
            e2: exp >>
            (Syntax::Let(("_dummy".to_string(), Type::Unit),
                         Box::new(e1), Box::new(e2)))
    )) |
    call!(exp_if)
));

named!(exp_if<CompleteByteSlice, Syntax>, alt!(
    ws!(do_parse!(
        tag!("if") >>
            e1: exp >>
            tag!("then") >>
            e2: exp >>
            tag!("else") >>
            e3: exp_let_after_if >>
            (Syntax::If(Box::new(e1), Box::new(e2), Box::new(e3)))
    )) |
    call!(exp_assign)
));

named!(exp_assign<CompleteByteSlice, Syntax>, alt!(
    ws!(do_parse!(
        base_indices: array_index_list >>
            tag!("<-") >>
            e: exp_comma >>
            ({
                let (mut base, mut indices) = base_indices;
                let last = indices.pop().unwrap(); // indices.len() >= 1
                for idx in indices {
                    base = Syntax::Get(Box::new(base), Box::new(idx));
                }
                Syntax::Put(Box::new(base), Box::new(last), Box::new(e))
            })
    )) |
    call!(exp_comma)
));

named!(exp_comma<CompleteByteSlice, Syntax>, alt!(
    ws!(do_parse!(
        init: exp_comparative >>
            res: fold_many1!(
                ws!(preceded!(tag!(","), exp_comparative)),
                vec![init],
                |mut acc: Vec<Syntax>, x| { acc.push(x); acc }
            ) >>
            (Syntax::Tuple(res.into_boxed_slice()))
    )) |
    call!(exp_comparative)
));

macro_rules! exp_binary_operator {
    ($rule:ident, $next_rule: ident, $op: ident, $folder: expr) => {
        named!($rule<CompleteByteSlice, Syntax>,
               ws!(do_parse!(
                   init: $next_rule >>
                       res: fold_many0!(
                           ws!(pair!($op, $next_rule)),
                           init,
                           $folder
                       ) >>
                       (res)
               ))
        );
    }
}
exp_binary_operator!(
    exp_comparative, exp_additive,
    comp_op,
    |acc, ((op, negated, flipped), arg)| {
        let ast = if flipped {
            Syntax::CompBin(op, Box::new(arg), Box::new(acc))
        } else {
            Syntax::CompBin(op, Box::new(acc), Box::new(arg))
        };
        if negated {
            Syntax::Not(Box::new(ast))
        } else {
            ast
        }
    }
);
// (operator, negated, arguments flipped)
named!(comp_op<CompleteByteSlice, (CompBin, bool, bool)>, alt!(
    do_parse!(tag!("<=") >> ((CompBin::LE, false, false))) |
    do_parse!(tag!(">=") >> ((CompBin::LE, false, true))) |
    do_parse!(tag!("<>") >> ((CompBin::Eq, true, false))) |
    do_parse!(tag!("=") >> ((CompBin::Eq, false, false))) |
    do_parse!(tag!("<") >> ((CompBin::LE, true, true))) |
    do_parse!(tag!(">") >> ((CompBin::LE, true, false)))
));
exp_binary_operator!(
    exp_additive, exp_multiplicative,
    add_op,
    |acc, (op, arg)| match op {
        Err(op) => Syntax::FloatBin(op, Box::new(acc), Box::new(arg)),
        Ok(op) => Syntax::IntBin(op, Box::new(acc), Box::new(arg)),
    }
);
named!(add_op<CompleteByteSlice, Result<IntBin, FloatBin>>, alt!(
    do_parse!(tag!("+.") >> (Err(FloatBin::FAdd))) |
    do_parse!(tag!("-.") >> (Err(FloatBin::FSub))) |
    do_parse!(tag!("+") >> (Ok(IntBin::Add))) |
    do_parse!(tag!("-") >> (Ok(IntBin::Sub)))
));

exp_binary_operator!(
    exp_multiplicative, exp_unary_minus,
    mult_op,
    |acc, (op, arg)| match op {
        Err(op) => Syntax::FloatBin(op, Box::new(acc), Box::new(arg)),
        Ok(op) => Syntax::IntBin(op, Box::new(acc), Box::new(arg)),
    }
);
named!(mult_op<CompleteByteSlice, Result<IntBin, FloatBin>>, alt!(
    do_parse!(tag!("*.") >> (Err(FloatBin::FMul))) |
    do_parse!(tag!("/.") >> (Err(FloatBin::FDiv)))
));

named!(exp_unary_minus<CompleteByteSlice, Syntax>, alt!(
    ws!(do_parse!(
        tag!("-.") >>
            e: exp_unary_minus >>
            (Syntax::FNeg(Box::new(e)))
    )) |
    ws!(do_parse!(
        tag!("-") >>
            e: exp_unary_minus >>
            (match e {
                Syntax::Float(_) => Syntax::FNeg(Box::new(e)),
                _ => Syntax::Neg(Box::new(e))
            })
    )) |
    call!(exp_app)
));


named!(exp_app<CompleteByteSlice, Syntax>, alt!(
    ws!(do_parse!(tag!("!") >> res: exp_app >> (Syntax::Not(Box::new(res))))) |
    ws!(do_parse!(
        _init: alt!(tag!("Array.create") | tag!("Array.make")) >>
            res: many1!(ws!(simple_exp)) >>
            ({
                let mut res = res;
                if res.len() != 2 {
                    panic!("The number of Array.create is wrong");
                }
                let res1 = res.pop().unwrap();
                let res0 = res.pop().unwrap();
                Syntax::Array(Box::new(res0), Box::new(res1))
            })
    )) |
    ws!(do_parse!(
        init: simple_exp >>
            res: many1!(call!(simple_exp)) >>
            (Syntax::App(Box::new(init), res.into_boxed_slice()))
    )) |
    call!(simple_exp)
));

named!(bool_lit<CompleteByteSlice, bool>, alt!(
    ws!(do_parse!(tag!("true") >> (true))) |
    ws!(do_parse!(tag!("false") >> (false)))
));

named!(int_lit<CompleteByteSlice, i64>, ws!(do_parse!(
    x: digit >>
        (convert(&x, 10))
)));

// TODO supports only digit+ . digit+
named!(float_lit<CompleteByteSlice, f64>, alt!(
    do_parse!(
        fstr: recognize!(do_parse!(
            _x: digit >>
                _s: preceded!(char!('.'), digit)
                >> (())) ) >>
            (convert_to_f64(&fstr))
    ) |
    do_parse!(
        fstr: recognize!(do_parse!(
            _x: digit >>
                _s: char!('.')
                >> (())) ) >>
            (convert_to_f64(&fstr))
    )
));
                          

named!(ident<CompleteByteSlice, String>, ws!(do_parse!(
    s: verify!(take_till!(is_not_ident_u8), is_ident) >>
        (String::from_utf8((&s as &[u8]).to_vec()).unwrap())
)));

fn is_not_ident_u8(x: u8) -> bool {
    !((b'0' <= x && x <= b'9') ||
      (b'A' <= x && x <= b'Z') ||
      (b'a' <= x && x <= b'z') ||
      x == b'_')
}

fn is_ident(x: CompleteByteSlice) -> bool {
    is_ident_u8_slice(&x)
}

fn is_ident_u8_slice(x: &[u8]) -> bool {
    let keywords = vec![&b"let"[..], &b"rec"[..], &b"in"[..], &b"true"[..],
                        &b"false"[..], &b"if"[..], &b"then"[..], &b"else"[..],
                        &b"Array.create"[..], &b"Array.make"[..]];
    if x.is_empty() || keywords.contains(&x) {
        return false;
    }
    !(b'0' <= x[0] && x[0] <= b'9')
}


fn convert(x: &[u8], radix: i64) -> i64 {
    let mut cur = 0;
    for &v in x.iter() {
        cur *= radix;
        cur += i64::from(v) - i64::from(b'0');
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
            tmp = (i64::from(v) - i64::from(b'0')) as f64 * base;
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

/// Assigns unique type variables to stub type variables
pub fn uniquify(expr: Syntax, id_gen: &mut IdGen) -> Syntax {
    match expr {
        Syntax::Let((x, t), e1, e2) => {
            let t = if let Type::Var(_) = t {
                id_gen.gen_type()
            } else {
                t
            };
            let e1 = uniquify(*e1, id_gen);
            let e2 = uniquify(*e2, id_gen);
            Syntax::Let((x, t), Box::new(e1), Box::new(e2))
        },
        Syntax::LetRec(Fundef {name: (name, t), mut args, body: e1},
                       e2) => {
            let t = if let Type::Var(_) = t {
                id_gen.gen_type()
            } else {
                t
            };
            for elem in args.iter_mut() {
                let entry = std::mem::replace(&mut elem.1, Type::Unit);
                let new_ty = if let Type::Var(_) = entry {
                    id_gen.gen_type()
                } else {
                    entry
                };
                elem.1 = new_ty;
            }
            let e1 = Box::new(uniquify(*e1, id_gen));
            let e2 = Box::new(uniquify(*e2, id_gen));
            Syntax::LetRec(Fundef {name: (name, t), args,
                                   body: e1},
                           e2)
        },
        Syntax::LetTuple(mut pat, e1, e2) => {
            for elem in pat.iter_mut() {
                if let Type::Var(_) = elem.1 {
                    elem.1 = id_gen.gen_type();
                }
            }
            let e1 = Box::new(uniquify(*e1, id_gen));
            let e2 = Box::new(uniquify(*e2, id_gen));
            Syntax::LetTuple(pat, e1, e2)
        }
        Syntax::Not(e1) => {
            let e1 = Box::new(uniquify(*e1, id_gen));
            Syntax::Not(e1)
        },
        Syntax::Neg(e1) => {
            let e1 = Box::new(uniquify(*e1, id_gen));
            Syntax::Neg(e1)
        },
        Syntax::IntBin(op, e1, e2) => {
            let e1 = Box::new(uniquify(*e1, id_gen));
            let e2 = Box::new(uniquify(*e2, id_gen));
            Syntax::IntBin(op, e1, e2)
        },
        Syntax::FNeg(e1) => {
            let e1 = Box::new(uniquify(*e1, id_gen));
            Syntax::FNeg(e1)
        },
        Syntax::FloatBin(op, e1, e2) => {
            let e1 = Box::new(uniquify(*e1, id_gen));
            let e2 = Box::new(uniquify(*e2, id_gen));
            Syntax::FloatBin(op, e1, e2)
        },
        Syntax::CompBin(op, e1, e2) => {
            let e1 = Box::new(uniquify(*e1, id_gen));
            let e2 = Box::new(uniquify(*e2, id_gen));
            Syntax::CompBin(op, e1, e2)
        },
        Syntax::If(e1, e2, e3) => {
            let e1 = Box::new(uniquify(*e1, id_gen));
            let e2 = Box::new(uniquify(*e2, id_gen));
            let e3 = Box::new(uniquify(*e3, id_gen));
            Syntax::If(e1, e2, e3)
        },
        Syntax::App(e1, mut e2s) => {
            let e1 = Box::new(uniquify(*e1, id_gen));
            uniquify_slice(&mut e2s, id_gen);
            Syntax::App(e1, e2s)
        },
        Syntax::Tuple(mut es) => {
            uniquify_slice(&mut es, id_gen);
            Syntax::Tuple(es)
        },
        Syntax::Array(e1, e2) => {
            let e1 = Box::new(uniquify(*e1, id_gen));
            let e2 = Box::new(uniquify(*e2, id_gen));
            Syntax::Array(e1, e2)
        },
        Syntax::Get(e1, e2) => {
            let e1 = Box::new(uniquify(*e1, id_gen));
            let e2 = Box::new(uniquify(*e2, id_gen));
            Syntax::Get(e1, e2)
        },
        Syntax::Put(e1, e2, e3) => {
            let e1 = Box::new(uniquify(*e1, id_gen));
            let e2 = Box::new(uniquify(*e2, id_gen));
            let e3 = Box::new(uniquify(*e3, id_gen));
            Syntax::Put(e1, e2, e3)
        },
        x => x, // No Syntax inside
    }
}

fn uniquify_slice(es: &mut [Syntax], id_gen: &mut IdGen) {
    for elem in es.iter_mut() {
        let entry = std::mem::replace(elem, Syntax::Unit);
        *elem = uniquify(entry, id_gen);
    }
}


pub fn remove_comments(a: &[u8]) -> Result<Vec<u8>, String> {
    let mut ret = Vec::new();
    let mut level = 0;
    let mut pos = 0;
    let len = a.len();
    while pos < len {
        if pos < len - 1 && a[pos] == b'(' && a[pos + 1] == b'*' {
            pos += 2;
            level += 1;
            continue;
        }
        if pos < len - 1 && a[pos] == b'*' && a[pos + 1] == b')' {
            pos += 2;
            if level <= 0 {
                return Err("Corresponding \"(*\" not found".to_string());
            }
            level -= 1;
            continue;
        }
        if level == 0 {
            ret.push(a[pos]);
        }
        pos += 1;
    }
    if level == 0 {
        Ok(ret)
    } else {
        Err("Comments are not balanced".to_string())
    }
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
    use parser::parse;
    use ordered_float::OrderedFloat;
    #[test]
    fn test_simple_exp() {
        use syntax::Syntax::*;
        assert_eq!(parse(b" ( c)"), Ok(((&[][..]).into(),
                                      Var("c".to_string()))));
        assert_eq!(parse(b"() "), Ok(((&[][..]).into(), Unit)));
        assert_eq!(parse(b"100"), Ok(((&[][..]).into(), Int(100))));
        assert_eq!(parse(b"10.0"), Ok((
            (&[][..]).into(), Float(OrderedFloat::from(10.0)))));
        assert_eq!(parse(b"1256.25"),
                   Ok(((&[][..]).into(),
                       Float(OrderedFloat::from(1256.25)))));
        assert_eq!(parse(b"a.(b)"),
                   Ok(((&[][..]).into(),
                       Get(Box::new(Var("a".to_string())),
                           Box::new(Var("b".to_string()))))));
        assert_eq!(parse(b"a.(b).(c)"),
                   Ok(((&[][..]).into(),
                       Get(Box::new(
                           Get(Box::new(Var("a".to_string())),
                               Box::new(Var("b".to_string())))),
                           Box::new(Var("c".to_string()))))));
    }

    #[test]
    fn test_let() {
        use syntax::Syntax::{Int, Let};
        let result = parse(b"let x = 0 in 1").unwrap();
        assert_eq!(result.0, (&[][..]).into());
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
    fn test_letrec() {
        use syntax::Fundef;
        use syntax::Syntax::{App, Int, LetRec, Var};
        let result = parse(b"let rec f x = x in f 1").unwrap();
        assert_eq!(result.0, (&[][..]).into());
        match result.1 {
            LetRec(Fundef { name: (id, _), args: _, body: e1 }, e2) => {
                assert_eq!(id, "f");
                assert_eq!(*e1, Var("x".to_string()));
                assert_eq!(*e2, App(Box::new(Var("f".to_string())),
                                    Box::new([Int(1)])));
            }
            _ => panic!(),
        }
    }

    #[test]
    fn test_lettuple() {
        use syntax::Syntax::{Int, App, LetTuple, Var};
        let result = parse(b"let (x, y) = make_pair 1 2 in x").unwrap();
        assert_eq!(result.0, (&[][..]).into());
        match result.1 {
            LetTuple(_pat, e1, e2) => {
                assert_eq!(*e1,
                           App(Box::new(Var("make_pair".to_string())),
                               Box::new([Int(1), Int(2)])));
                assert_eq!(*e2, Var("x".to_string()));
            }
            _ => panic!(),
        }
    }

    #[test]
    fn test_semicolon() {
        use syntax::Syntax::{App, Int, Let, Unit, Var};
        use syntax::Type;
        let result = parse(b"print_int 0; (); print_int 1");
        let print_int = || Box::new(Var("print_int".to_string()));
        let dummy = || ("_dummy".to_string(), Type::Unit);
        // semicolon is right-associative
        assert_eq!(result,
                   Ok(((&[][..]).into(),
                       Let(dummy(),
                           Box::new(App(print_int(),
                                        Box::new([Int(0)]))),
                           Box::new(Let(
                               dummy(),
                               Box::new(Unit),
                               Box::new(App(
                                   print_int(),
                                   Box::new([Int(1)])))))))));
    }

    #[test]
    fn test_if() {
        use syntax::Syntax::{App, Int, If, Var};
        assert_eq!(parse(b"if f 3 then 4 else 0"),
                   Ok(((&[][..]).into(),
                       If(
                           Box::new(App(Box::new(Var("f".to_string())),
                                        Box::new([Int(3)]))),
                           Box::new(Int(4)),
                           Box::new(Int(0))))));
    }

    #[test]
    fn test_if2() {
        use syntax::Syntax::{App, Int, If, Var};
        assert_eq!(parse(b" if f 3 then 4 else 0 "),
                   Ok(((&[][..]).into(),
                       If(
                           Box::new(App(Box::new(Var("f".to_string())),
                                        Box::new([Int(3)]))),
                           Box::new(Int(4)),
                           Box::new(Int(0))))));
    }

    #[test]
    fn test_comma() {
        use syntax::Syntax::{Int, Tuple, Var};
        use syntax::{Syntax, IntBin};
        let x = || Var("x".to_string());
        let y = || Var("y".to_string());
        let z = || Var("z".to_string());
        let w = || Var("w".to_string());
        assert_eq!(parse(b"x, y, (z, w)"),
                   Ok(((&[][..]).into(),
                       Tuple(
                           Box::new([x(), y(),
                                     Tuple(
                                         Box::new([z(), w()]))
                           ])))));
        assert_eq!(parse(b"x, 1 + 3"),
                   Ok(((&[][..]).into(),
                       Tuple(
                           Box::new([x(),
                                     Syntax::IntBin(IntBin::Add,
                                                    Box::new(Int(1)),
                                                    Box::new(Int(3)))
                           ])))));
    }

    #[test]
    fn test_comp() {
        use syntax::{Syntax, IntBin, CompBin};
        use syntax::Syntax::Int;
        assert_eq!(parse(b"1 < 2 + 3"),
                   Ok(((&[][..]).into(),
                       Syntax::Not(
                           Box::new(Syntax::CompBin(
                               CompBin::LE,
                               Box::new(Syntax::IntBin(
                                   IntBin::Add,
                                   Box::new(Int(2)),
                                   Box::new(Int(3)))),
                               Box::new(Int(1))))))));

        assert_eq!(parse(b"4 <= 3"),
                   Ok(((&[][..]).into(),
                       Syntax::CompBin(
                           CompBin::LE,
                           Box::new(Int(4)),
                           Box::new(Int(3))))));
        assert_eq!(parse(b"4 >= 3"),
                   Ok(((&[][..]).into(),
                       Syntax::CompBin(
                           CompBin::LE,
                           Box::new(Int(3)),
                           Box::new(Int(4))))));
        assert_eq!(parse(b"4 > 3"),
                   Ok(((&[][..]).into(),
                       Syntax::Not(Box::new(
                           Syntax::CompBin(
                               CompBin::LE,
                               Box::new(Int(4)),
                               Box::new(Int(3))))))));
        assert_eq!(parse(b"4 = 3"),
                   Ok(((&[][..]).into(),
                       Syntax::CompBin(
                           CompBin::Eq,
                           Box::new(Int(4)),
                           Box::new(Int(3))))));
        assert_eq!(parse(b"4 <> 3"),
                   Ok(((&[][..]).into(),
                       Syntax::Not(Box::new(
                           Syntax::CompBin(
                               CompBin::Eq,
                               Box::new(Int(4)),
                               Box::new(Int(3))))))));
    }


    #[test]
    fn test_mult() {
        use syntax::Syntax;
        use syntax::Syntax::Var;
        use syntax::FloatBin;
        assert_eq!(parse(b"x *. y"),
                   Ok(((&[][..]).into(),
                       Syntax::FloatBin(
                           FloatBin::FMul,
                           Box::new(Var("x".to_string())),
                           Box::new(Var("y".to_string()))))));
        assert_eq!(parse(b"x /. y *. z"),
                   Ok(((&[][..]).into(),
                       Syntax::FloatBin(
                           FloatBin::FMul,
                           Box::new(Syntax::FloatBin(
                               FloatBin::FDiv,
                               Box::new(Var("x".to_string())),
                               Box::new(Var("y".to_string())))),
                           Box::new(Var("z".to_string()))))));
    }

    #[test]
    fn test_multadd() {
        use syntax::Syntax;
        use syntax::Syntax::Var;
        use syntax::FloatBin;
        assert_eq!(parse(b"x /. y +. z"),
                   Ok(((&[][..]).into(),
                       Syntax::FloatBin(
                           FloatBin::FAdd,
                           Box::new(Syntax::FloatBin(
                               FloatBin::FDiv,
                               Box::new(Var("x".to_string())),
                               Box::new(Var("y".to_string())))),
                           Box::new(Var("z".to_string()))))));
        assert_eq!(parse(b"a -. b /. c"),
                   Ok(((&[][..]).into(),
                       Syntax::FloatBin(
                           FloatBin::FSub,
                           Box::new(Var("a".to_string())),
                           Box::new(Syntax::FloatBin(
                               FloatBin::FDiv,
                               Box::new(Var("b".to_string())),
                               Box::new(Var("c".to_string()))))))));
    }

    #[test]
    fn test_unary_minus() {
        use syntax::Syntax::{Int, Float, Neg, FNeg, Var};
        assert_eq!(parse(b"-2"),
                   Ok(((&[][..]).into(),
                       Neg(Box::new(Int(2))))));
        assert_eq!(parse(b"--2"),
                   Ok(((&[][..]).into(),
                       Neg(Box::new(Neg(
                           Box::new(Int(2))))))));
        assert_eq!(parse(b"-.x"),
                   Ok(((&[][..]).into(),
                       FNeg(
                           Box::new(Var("x".to_string()))))));
        // - followed by float literal is ok.
        assert_eq!(parse(b"-2.0"),
                   Ok(((&[][..]).into(),
                       FNeg(Box::new(Float(2.0.into()))))));
    }

    #[test]
    fn test_exp_not() {
        use syntax::Syntax::{Bool, Not};
        assert_eq!(parse(b"!!true"),
                   Ok(((&[][..]).into(),
                       Not(Box::new(Not(
                           Box::new(Bool(true))))))));
    }
    
    #[test]
    fn test_app() {
        use syntax::Syntax::{App, Int, Var};
        assert_eq!(parse(b"func 0 1"),
                   Ok(((&[][..]).into(),
                       App(
                           Box::new(Var("func".to_string())),
                           Box::new([Int(0), Int(1)])))));
    }

    #[test]
    fn test_array_create() {
        use syntax::Syntax::{Array, Int};
        assert_eq!(parse(b"Array.create 2 3"),
                   Ok(((&[][..]).into(),
                       Array(
                           Box::new(Int(2)),
                           Box::new(Int(3))))));
    }
}
