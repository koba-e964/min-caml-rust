use crate::id::IdGen;
use crate::syntax::*;
use nom::branch::alt;
use nom::bytes::complete::{tag, take_while};
use nom::character::complete::{char, digit1, multispace0, multispace1};
use nom::combinator::{map, recognize, value, verify};
use nom::multi::{many0, many1};
use nom::sequence::{delimited, preceded, separated_pair, tuple};
use nom::IResult;
use ordered_float::OrderedFloat;

pub fn parse(x: &[u8]) -> IResult<&[u8], Syntax> {
    delimited(multispace0, exp, multispace0)(x)
}

fn simple_exp_no_index(i: &[u8]) -> IResult<&[u8], Syntax> {
    alt((
        delimited(
            multispace0,
            delimited(char('('), exp, char(')')),
            multispace0,
        ),
        value(
            Syntax::Unit,
            delimited(
                multispace0,
                delimited(char('('), multispace0, char(')')),
                multispace0,
            ),
        ),
        map(delimited(multispace0, bool_lit, multispace0), Syntax::Bool),
        map(delimited(multispace0, float_lit, multispace0), |f| {
            Syntax::Float(OrderedFloat::from(f))
        }),
        map(delimited(multispace0, int_lit, multispace0), Syntax::Int),
        map(delimited(multispace0, ident, multispace0), Syntax::Var),
    ))(i)
}

fn simple_exp(i: &[u8]) -> IResult<&[u8], Syntax> {
    let indices = many0(map(
        tuple((
            char('.'),
            multispace0,
            char('('),
            multispace0,
            exp,
            multispace0,
            char(')'),
        )),
        |(_, _, _, _, e, _, _)| e,
    ));
    let inner = map(
        separated_pair(simple_exp_no_index, multispace0, indices),
        |(mut acc, indices)| {
            for idx in indices {
                acc = Syntax::Get(Box::new(acc), Box::new(idx));
            }
            acc
        },
    );
    delimited(multispace0, inner, multispace0)(i)
}
fn array_index_list(i: &[u8]) -> IResult<&[u8], (Syntax, Vec<Syntax>)> {
    let indices = many0(map(
        tuple((
            multispace0,
            char('.'),
            multispace0,
            char('('),
            multispace0,
            exp,
            multispace0,
            char(')'),
        )),
        |(_, _, _, _, _, e, _, _)| e,
    ));
    separated_pair(simple_exp_no_index, multispace0, indices)(i)
}

#[inline(always)]
fn exp(i: &[u8]) -> IResult<&[u8], Syntax> {
    exp_let(i)
}

fn exp_let(i: &[u8]) -> IResult<&[u8], Syntax> {
    alt((
        map(
            tuple((
                tag("let"),
                multispace1,
                ident,
                multispace0,
                char('='),
                multispace0,
                exp,
                multispace0,
                tag("in"),
                multispace0,
                exp_let,
            )),
            |(_, _, id, _, _, _, e1, _, _, _, e2)| {
                Syntax::Let(
                    (id, Type::Var(0) /* stub type */),
                    Box::new(e1),
                    Box::new(e2),
                )
            },
        ),
        map(
            tuple((
                tag("let"),
                multispace1,
                tag("rec"),
                multispace1,
                fundef,
                multispace0,
                tag("in"),
                multispace0,
                exp_let,
            )),
            |(_, _, _, _, f, _, _, _, e)| Syntax::LetRec(f, Box::new(e)),
        ),
        map(
            tuple((
                tag("let"),
                multispace0,
                char('('),
                multispace0,
                pat,
                multispace0,
                char(')'),
                multispace0,
                char('='),
                multispace0,
                exp,
                multispace0,
                tag("in"),
                multispace0,
                exp_let,
            )),
            |(_, _, _, _, p, _, _, _, _, _, e1, _, _, _, e2)| {
                Syntax::LetTuple(p.into_boxed_slice(), Box::new(e1), Box::new(e2))
            },
        ),
        exp_semicolon,
    ))(i)
}

fn exp_let_after_if(i: &[u8]) -> IResult<&[u8], Syntax> {
    alt((
        map(
            tuple((
                tag("let"),
                multispace1,
                ident,
                multispace0,
                char('='),
                multispace0,
                exp,
                multispace0,
                tag("in"),
                multispace0,
                exp_let_after_if,
            )),
            |(_, _, id, _, _, _, e1, _, _, _, e2)| {
                Syntax::Let(
                    (id, Type::Var(0) /* stub type */),
                    Box::new(e1),
                    Box::new(e2),
                )
            },
        ),
        map(
            tuple((
                tag("let"),
                multispace1,
                tag("rec"),
                multispace1,
                fundef,
                multispace0,
                tag("in"),
                multispace0,
                exp_let_after_if,
            )),
            |(_, _, _, _, f, _, _, _, e)| Syntax::LetRec(f, Box::new(e)),
        ),
        map(
            tuple((
                tag("let"),
                multispace0,
                char('('),
                multispace0,
                pat,
                multispace0,
                char(')'),
                multispace0,
                char('='),
                multispace0,
                exp,
                multispace0,
                tag("in"),
                multispace0,
                exp_let_after_if,
            )),
            |(_, _, _, _, p, _, _, _, _, _, e1, _, _, _, e2)| {
                Syntax::LetTuple(p.into_boxed_slice(), Box::new(e1), Box::new(e2))
            },
        ),
        exp_if,
    ))(i)
}

fn pat(i: &[u8]) -> IResult<&[u8], Vec<(String, Type)>> {
    map(
        tuple((
            ident,
            many0(delimited(
                tuple((multispace0, char(','), multispace0)),
                ident,
                multispace0,
            )),
        )),
        |(init, mut res)| {
            res.insert(0, init);
            res.into_iter()
                .map(|x| (x, Type::Var(0) /* stub type */))
                .collect()
        },
    )(i)
}

fn fundef(i: &[u8]) -> IResult<&[u8], Fundef> {
    let (i, funname) = ident(i)?;
    let (i, args) = formal_args(i)?;
    let (i, _) = delimited(multispace0, char('='), multispace0)(i)?;
    let (i, e) = exp(i)?;
    Ok((
        i,
        Fundef {
            name: (funname, Type::Var(0) /* stub type */),
            args: args.into_boxed_slice(),
            body: Box::new(e),
        },
    ))
}

fn formal_args(i: &[u8]) -> IResult<&[u8], Vec<(String, Type)>> {
    map(many1(preceded(multispace1, ident)), |x| {
        x.into_iter()
            .map(|x| (x, Type::Var(0) /* stub type */))
            .collect()
    })(i)
}

fn exp_semicolon(i: &[u8]) -> IResult<&[u8], Syntax> {
    alt((
        map(
            tuple((exp_if, multispace0, char(';'), multispace0, exp)),
            |(e1, _, _, _, e2)| {
                Syntax::Let(
                    ("_dummy".to_string(), Type::Unit),
                    Box::new(e1),
                    Box::new(e2),
                )
            },
        ),
        exp_if,
    ))(i)
}

fn exp_if(i: &[u8]) -> IResult<&[u8], Syntax> {
    alt((
        map(
            tuple((
                tag("if"),
                multispace0,
                exp,
                multispace0,
                tag("then"),
                multispace0,
                exp,
                multispace0,
                tag("else"),
                multispace0,
                exp_let_after_if,
            )),
            |(_, _, e1, _, _, _, e2, _, _, _, e3)| {
                Syntax::If(Box::new(e1), Box::new(e2), Box::new(e3))
            },
        ),
        exp_assign,
    ))(i)
}

fn exp_assign(i: &[u8]) -> IResult<&[u8], Syntax> {
    alt((
        map(
            tuple((
                array_index_list,
                multispace0,
                tag("<-"),
                multispace0,
                exp_comma,
            )),
            |(base_indices, _, _, _, e)| {
                let (mut base, mut indices) = base_indices;
                let last = indices.pop().unwrap(); // indices.len() >= 1
                for idx in indices {
                    base = Syntax::Get(Box::new(base), Box::new(idx));
                }
                Syntax::Put(Box::new(base), Box::new(last), Box::new(e))
            },
        ),
        exp_comma,
    ))(i)
}

fn exp_comma(i: &[u8]) -> IResult<&[u8], Syntax> {
    alt((
        map(
            tuple((exp_comparative, many1(preceded(char(','), exp_comparative)))),
            |(init, res)| {
                let mut res = res;
                res.insert(0, init);
                Syntax::Tuple(res.into_boxed_slice())
            },
        ),
        exp_comparative,
    ))(i)
}

macro_rules! exp_binary_operator {
    ($rule:ident, $next_rule: ident, $op: ident, $folder: expr) => {
        fn $rule(i: &[u8]) -> IResult<&[u8], Syntax> {
            map(
                tuple((
                    $next_rule,
                    many0(preceded(
                        multispace0,
                        tuple((multispace0, $op, multispace0, $next_rule)),
                    )),
                )),
                |(init, res)| {
                    res.into_iter()
                        .fold(init, |acc, (_, op, _, arg)| $folder(acc, (op, arg)))
                },
            )(i)
        }
    };
}
exp_binary_operator!(exp_comparative, exp_additive, comp_op, |acc,
                                                              (
    (op, negated, flipped),
    arg,
)| {
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
});
// (operator, negated, arguments flipped)
fn comp_op(i: &[u8]) -> IResult<&[u8], (CompBin, bool, bool)> {
    alt((
        value((CompBin::LE, false, false), tag("<=")),
        value((CompBin::LE, false, true), tag(">=")),
        value((CompBin::Eq, true, false), tag("<>")),
        value((CompBin::Eq, false, false), char('=')),
        value((CompBin::LE, true, true), char('<')),
        value((CompBin::LE, true, false), char('>')),
    ))(i)
}
exp_binary_operator!(
    exp_additive,
    exp_multiplicative,
    add_op,
    |acc, (op, arg)| match op {
        Err(op) => Syntax::FloatBin(op, Box::new(acc), Box::new(arg)),
        Ok(op) => Syntax::IntBin(op, Box::new(acc), Box::new(arg)),
    }
);
fn add_op(i: &[u8]) -> IResult<&[u8], Result<IntBin, FloatBin>> {
    alt((
        map(tag("+."), |_| Err(FloatBin::FAdd)),
        map(tag("-."), |_| Err(FloatBin::FSub)),
        map(char('+'), |_| Ok(IntBin::Add)),
        map(char('-'), |_| Ok(IntBin::Sub)),
    ))(i)
}

exp_binary_operator!(
    exp_multiplicative,
    exp_unary_minus,
    mult_op,
    |acc, (op, arg)| match op {
        Err(op) => Syntax::FloatBin(op, Box::new(acc), Box::new(arg)),
        Ok(op) => Syntax::IntBin(op, Box::new(acc), Box::new(arg)),
    }
);
fn mult_op(i: &[u8]) -> IResult<&[u8], Result<IntBin, FloatBin>> {
    alt((
        map(tag("*."), |_| Err(FloatBin::FMul)),
        map(tag("/."), |_| Err(FloatBin::FDiv)),
    ))(i)
}

fn exp_unary_minus(i: &[u8]) -> IResult<&[u8], Syntax> {
    alt((
        map(
            preceded(tuple((tag("-."), multispace0)), exp_unary_minus),
            |e| Syntax::FNeg(Box::new(e)),
        ),
        map(
            preceded(tuple((char('-'), multispace0)), exp_unary_minus),
            |e| match e {
                Syntax::Float(_) => Syntax::FNeg(Box::new(e)),
                _ => Syntax::Neg(Box::new(e)),
            },
        ),
        exp_app,
    ))(i)
}

fn exp_app(i: &[u8]) -> IResult<&[u8], Syntax> {
    alt((
        map(tuple((char('!'), exp_app)), |(_, e)| {
            Syntax::Not(Box::new(e))
        }),
        map(
            tuple((
                alt((tag("Array.create"), tag("Array.make"))),
                many1(delimited(multispace0, simple_exp, multispace0)),
            )),
            |(_tag, mut res)| {
                if res.len() != 2 {
                    panic!("The number of arguments of Array.create is wrong");
                }
                let res1 = res.pop().unwrap();
                let res0 = res.pop().unwrap();
                Syntax::Array(Box::new(res0), Box::new(res1))
            },
        ),
        map(
            tuple((simple_exp, many1(preceded(multispace0, simple_exp)))),
            |(init, res)| Syntax::App(Box::new(init), res.into_boxed_slice()),
        ),
        simple_exp,
    ))(i)
}

fn bool_lit(i: &[u8]) -> IResult<&[u8], bool> {
    alt((value(true, tag("true")), value(false, tag("false"))))(i)
}

fn int_lit(i: &[u8]) -> IResult<&[u8], i64> {
    let (i, x) = digit1(i)?;
    Ok((i, convert(x, 10)))
}

fn float_lit(i: &[u8]) -> IResult<&[u8], f64> {
    map(
        alt((
            recognize(tuple((digit1, char('.'), digit1))),
            recognize(tuple((digit1, char('.')))),
        )),
        convert_to_f64,
    )(i)
}

fn ident(i: &[u8]) -> IResult<&[u8], String> {
    let (i, s) = verify(take_while(is_ident_u8), is_ident)(i)?;
    Ok((i, String::from_utf8(s.to_vec()).unwrap()))
}

fn is_ident_u8(x: u8) -> bool {
    x.is_ascii_alphanumeric() || x == b'_'
}

fn is_ident(x: &[u8]) -> bool {
    let keywords = [
        b"let" as &[u8],
        b"rec",
        b"in",
        b"true",
        b"false",
        b"if",
        b"then",
        b"else",
        b"Array.create",
        b"Array.make",
    ];
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
        }
        Syntax::LetRec(
            Fundef {
                name: (name, t),
                mut args,
                body: e1,
            },
            e2,
        ) => {
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
            Syntax::LetRec(
                Fundef {
                    name: (name, t),
                    args,
                    body: e1,
                },
                e2,
            )
        }
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
        }
        Syntax::Neg(e1) => {
            let e1 = Box::new(uniquify(*e1, id_gen));
            Syntax::Neg(e1)
        }
        Syntax::IntBin(op, e1, e2) => {
            let e1 = Box::new(uniquify(*e1, id_gen));
            let e2 = Box::new(uniquify(*e2, id_gen));
            Syntax::IntBin(op, e1, e2)
        }
        Syntax::FNeg(e1) => {
            let e1 = Box::new(uniquify(*e1, id_gen));
            Syntax::FNeg(e1)
        }
        Syntax::FloatBin(op, e1, e2) => {
            let e1 = Box::new(uniquify(*e1, id_gen));
            let e2 = Box::new(uniquify(*e2, id_gen));
            Syntax::FloatBin(op, e1, e2)
        }
        Syntax::CompBin(op, e1, e2) => {
            let e1 = Box::new(uniquify(*e1, id_gen));
            let e2 = Box::new(uniquify(*e2, id_gen));
            Syntax::CompBin(op, e1, e2)
        }
        Syntax::If(e1, e2, e3) => {
            let e1 = Box::new(uniquify(*e1, id_gen));
            let e2 = Box::new(uniquify(*e2, id_gen));
            let e3 = Box::new(uniquify(*e3, id_gen));
            Syntax::If(e1, e2, e3)
        }
        Syntax::App(e1, mut e2s) => {
            let e1 = Box::new(uniquify(*e1, id_gen));
            uniquify_slice(&mut e2s, id_gen);
            Syntax::App(e1, e2s)
        }
        Syntax::Tuple(mut es) => {
            uniquify_slice(&mut es, id_gen);
            Syntax::Tuple(es)
        }
        Syntax::Array(e1, e2) => {
            let e1 = Box::new(uniquify(*e1, id_gen));
            let e2 = Box::new(uniquify(*e2, id_gen));
            Syntax::Array(e1, e2)
        }
        Syntax::Get(e1, e2) => {
            let e1 = Box::new(uniquify(*e1, id_gen));
            let e2 = Box::new(uniquify(*e2, id_gen));
            Syntax::Get(e1, e2)
        }
        Syntax::Put(e1, e2, e3) => {
            let e1 = Box::new(uniquify(*e1, id_gen));
            let e2 = Box::new(uniquify(*e2, id_gen));
            let e3 = Box::new(uniquify(*e3, id_gen));
            Syntax::Put(e1, e2, e3)
        }
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
    use super::*;
    use ordered_float::OrderedFloat;
    #[test]
    fn test_simple_exp() {
        use crate::syntax::Syntax::*;
        assert_eq!(parse(b" ( c)"), Ok((&[] as &[u8], Var("c".to_string()))));
        assert_eq!(parse(b"() "), Ok((&[] as &[u8], Unit)));
        assert_eq!(parse(b"100"), Ok((&[] as &[u8], Int(100))));
        assert_eq!(
            parse(b"10.0"),
            Ok((&[] as &[u8], Float(OrderedFloat::from(10.0))))
        );
        assert_eq!(
            parse(b"1256.25"),
            Ok((&[] as &[u8], Float(OrderedFloat::from(1256.25))))
        );
        assert_eq!(
            parse(b"a.(b)"),
            Ok((
                &[] as &[u8],
                Get(
                    Box::new(Var("a".to_string())),
                    Box::new(Var("b".to_string()))
                )
            ))
        );
        assert_eq!(
            parse(b"a.(b).(c)"),
            Ok((
                &[] as &[u8],
                Get(
                    Box::new(Get(
                        Box::new(Var("a".to_string())),
                        Box::new(Var("b".to_string()))
                    )),
                    Box::new(Var("c".to_string()))
                )
            ))
        );
    }

    #[test]
    fn test_let() {
        use crate::syntax::Syntax::{Int, Let};
        let result = parse(b"let x = 0 in 1").unwrap();
        assert_eq!(result.0, []);
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
        use crate::syntax::Fundef;
        use crate::syntax::Syntax::{App, Int, LetRec, Var};
        let result = parse(b"let rec f x = x in f 1").unwrap();
        assert_eq!(result.0, []);
        match result.1 {
            LetRec(
                Fundef {
                    name: (id, _),
                    args: _,
                    body: e1,
                },
                e2,
            ) => {
                assert_eq!(id, "f");
                assert_eq!(*e1, Var("x".to_string()));
                assert_eq!(*e2, App(Box::new(Var("f".to_string())), Box::new([Int(1)])));
            }
            _ => panic!(),
        }
    }

    #[test]
    fn test_formal_args() {
        let result = formal_args(b" x y").unwrap();
        assert_eq!(result.0, []);
        assert_eq!(
            result.1,
            [
                ("x".to_string(), Type::Var(0)),
                ("y".to_string(), Type::Var(0))
            ]
        );
    }

    #[test]
    fn test_lettuple() {
        use crate::syntax::Syntax::{App, Int, LetTuple, Var};
        let result = parse(b"let (x, y) = make_pair 1 2 in x").unwrap();
        assert_eq!(result.0, []);
        match result.1 {
            LetTuple(_pat, e1, e2) => {
                assert_eq!(
                    *e1,
                    App(
                        Box::new(Var("make_pair".to_string())),
                        Box::new([Int(1), Int(2)])
                    )
                );
                assert_eq!(*e2, Var("x".to_string()));
            }
            _ => panic!(),
        }
    }
    #[test]
    fn test_pat() {
        let result = pat(b"x, y").unwrap();
        assert_eq!(result.0, []);
        assert_eq!(
            result.1,
            [
                ("x".to_string(), Type::Var(0)),
                ("y".to_string(), Type::Var(0))
            ]
        );
    }

    #[test]
    fn test_semicolon() {
        use crate::syntax::Syntax::{App, Int, Let, Unit, Var};
        use crate::syntax::Type;
        let result = parse(b"print_int 0; (); print_int 1");
        let print_int = || Box::new(Var("print_int".to_string()));
        let dummy = || ("_dummy".to_string(), Type::Unit);
        // semicolon is right-associative
        assert_eq!(
            result,
            Ok((
                &[] as &[u8],
                Let(
                    dummy(),
                    Box::new(App(print_int(), Box::new([Int(0)]))),
                    Box::new(Let(
                        dummy(),
                        Box::new(Unit),
                        Box::new(App(print_int(), Box::new([Int(1)])))
                    ))
                )
            ))
        );
    }

    #[test]
    fn test_if() {
        use crate::syntax::Syntax::{App, If, Int, Var};
        assert_eq!(
            parse(b"if f 3 then 4 else 0"),
            Ok((
                &[] as &[u8],
                If(
                    Box::new(App(Box::new(Var("f".to_string())), Box::new([Int(3)]))),
                    Box::new(Int(4)),
                    Box::new(Int(0))
                )
            ))
        );
    }

    #[test]
    fn test_if2() {
        use crate::syntax::Syntax::{App, If, Int, Var};
        assert_eq!(
            parse(b" if f 3 then 4 else 0 "),
            Ok((
                &[] as &[u8],
                If(
                    Box::new(App(Box::new(Var("f".to_string())), Box::new([Int(3)]))),
                    Box::new(Int(4)),
                    Box::new(Int(0))
                )
            ))
        );
    }

    #[test]
    fn test_comma() {
        use crate::syntax::Syntax::{Int, Tuple, Var};
        use crate::syntax::{IntBin, Syntax};
        let x = || Var("x".to_string());
        let y = || Var("y".to_string());
        let z = || Var("z".to_string());
        let w = || Var("w".to_string());
        assert_eq!(
            parse(b"x, y, (z, w)"),
            Ok((
                &[] as &[u8],
                Tuple(Box::new([x(), y(), Tuple(Box::new([z(), w()]))]))
            ))
        );
        assert_eq!(
            parse(b"x, 1 + 3"),
            Ok((
                &[] as &[u8],
                Tuple(Box::new([
                    x(),
                    Syntax::IntBin(IntBin::Add, Box::new(Int(1)), Box::new(Int(3)))
                ]))
            ))
        );
    }

    #[test]
    fn test_comp() {
        use crate::syntax::Syntax::Int;
        use crate::syntax::{CompBin, IntBin, Syntax};
        assert_eq!(
            parse(b"1 < 2 + 3"),
            Ok((
                &[] as &[u8],
                Syntax::Not(Box::new(Syntax::CompBin(
                    CompBin::LE,
                    Box::new(Syntax::IntBin(
                        IntBin::Add,
                        Box::new(Int(2)),
                        Box::new(Int(3))
                    )),
                    Box::new(Int(1))
                )))
            ))
        );

        assert_eq!(
            parse(b"4 <= 3"),
            Ok((
                &[] as &[u8],
                Syntax::CompBin(CompBin::LE, Box::new(Int(4)), Box::new(Int(3)))
            ))
        );
        assert_eq!(
            parse(b"4 >= 3"),
            Ok((
                &[] as &[u8],
                Syntax::CompBin(CompBin::LE, Box::new(Int(3)), Box::new(Int(4)))
            ))
        );
        assert_eq!(
            parse(b"4 > 3"),
            Ok((
                &[] as &[u8],
                Syntax::Not(Box::new(Syntax::CompBin(
                    CompBin::LE,
                    Box::new(Int(4)),
                    Box::new(Int(3))
                )))
            ))
        );
        assert_eq!(
            parse(b"4 = 3"),
            Ok((
                &[] as &[u8],
                Syntax::CompBin(CompBin::Eq, Box::new(Int(4)), Box::new(Int(3)))
            ))
        );
        assert_eq!(
            parse(b"4 <> 3"),
            Ok((
                &[] as &[u8],
                Syntax::Not(Box::new(Syntax::CompBin(
                    CompBin::Eq,
                    Box::new(Int(4)),
                    Box::new(Int(3))
                )))
            ))
        );
    }

    #[test]
    fn test_mult() {
        use crate::syntax::FloatBin;
        use crate::syntax::Syntax;
        use crate::syntax::Syntax::Var;
        assert_eq!(
            parse(b"x *. y"),
            Ok((
                &[] as &[u8],
                Syntax::FloatBin(
                    FloatBin::FMul,
                    Box::new(Var("x".to_string())),
                    Box::new(Var("y".to_string()))
                )
            ))
        );
        assert_eq!(
            parse(b"x /. y *. z"),
            Ok((
                &[] as &[u8],
                Syntax::FloatBin(
                    FloatBin::FMul,
                    Box::new(Syntax::FloatBin(
                        FloatBin::FDiv,
                        Box::new(Var("x".to_string())),
                        Box::new(Var("y".to_string()))
                    )),
                    Box::new(Var("z".to_string()))
                )
            ))
        );
    }

    #[test]
    fn test_multadd() {
        use crate::syntax::FloatBin;
        use crate::syntax::Syntax;
        use crate::syntax::Syntax::Var;
        assert_eq!(
            parse(b"x /. y +. z"),
            Ok((
                &[] as &[u8],
                Syntax::FloatBin(
                    FloatBin::FAdd,
                    Box::new(Syntax::FloatBin(
                        FloatBin::FDiv,
                        Box::new(Var("x".to_string())),
                        Box::new(Var("y".to_string()))
                    )),
                    Box::new(Var("z".to_string()))
                )
            ))
        );
        assert_eq!(
            parse(b"a -. b /. c"),
            Ok((
                &[] as &[u8],
                Syntax::FloatBin(
                    FloatBin::FSub,
                    Box::new(Var("a".to_string())),
                    Box::new(Syntax::FloatBin(
                        FloatBin::FDiv,
                        Box::new(Var("b".to_string())),
                        Box::new(Var("c".to_string()))
                    ))
                )
            ))
        );
    }

    #[test]
    fn test_unary_minus() {
        use crate::syntax::Syntax::{FNeg, Float, Int, Neg, Var};
        assert_eq!(parse(b"-2"), Ok((&[] as &[u8], Neg(Box::new(Int(2))))));
        assert_eq!(
            parse(b"--2"),
            Ok((&[] as &[u8], Neg(Box::new(Neg(Box::new(Int(2)))))))
        );
        assert_eq!(
            parse(b"-.x"),
            Ok((&[] as &[u8], FNeg(Box::new(Var("x".to_string())))))
        );
        // - followed by float literal is ok.
        assert_eq!(
            parse(b"-2.0"),
            Ok((&[] as &[u8], FNeg(Box::new(Float(2.0.into())))))
        );
    }

    #[test]
    fn test_exp_not() {
        use crate::syntax::Syntax::{Bool, Not};
        assert_eq!(
            parse(b"!!true"),
            Ok((&[] as &[u8], Not(Box::new(Not(Box::new(Bool(true)))))))
        );
    }

    #[test]
    fn test_app() {
        use crate::syntax::Syntax::{App, Int, Var};
        assert_eq!(
            parse(b"func 0 1"),
            Ok((
                &[] as &[u8],
                App(
                    Box::new(Var("func".to_string())),
                    Box::new([Int(0), Int(1)])
                )
            ))
        );
    }

    #[test]
    fn test_array_create() {
        use crate::syntax::Syntax::{Array, Int};
        assert_eq!(
            parse(b"Array.create 2 3"),
            Ok((&[] as &[u8], Array(Box::new(Int(2)), Box::new(Int(3)))))
        );
    }
}
