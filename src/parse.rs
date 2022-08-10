use crate::data::*;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{char, digit1};
use nom::combinator::opt;
use nom::error::{Error, ErrorKind, ParseError};
use nom::number::complete::float;
use nom::sequence::{delimited, preceded, terminated, tuple};
use nom::{Err, IResult};
#[cfg(test)]
use pretty_assertions::assert_eq;
use unicode_segmentation::UnicodeSegmentation;
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct InputState {
    disable_operation: bool,
    disable_relationship: bool,
    call_depth: usize,
}

fn debug_try(funcname: &'static str, input: &str) {
    eprintln!("{} <- {}", funcname, input);
}

impl InputState {
    pub fn new() -> Self {
        Self {
            disable_operation: false,
            disable_relationship: false,
            call_depth: 0,
        }
    }
}

pub fn expression(state: InputState, input: &str) -> IResult<&str, Expression> {
    debug_try("expression", input);
    let state = InputState {
        disable_operation: state.disable_operation,
        disable_relationship: state.disable_relationship,
        call_depth: state.call_depth + 1,
    };
    if let Ok((tail, q)) = quantification(state, input) {
        Ok((tail, Expression::Q(Box::new(q))))
    } else if let Ok((tail, r)) = relationship(state, input) {
        Ok((tail, Expression::R(Box::new(r))))
    } else if let Ok((tail, o)) = operation(state, input) {
        Ok((tail, Expression::O(Box::new(o))))
    } else if let Ok((tail, f)) = function_call(state, input) {
        Ok((tail, Expression::F(Box::new(f))))
    } else if let Ok((tail, da)) = decorated(move |i| atom(state, i))(input) {
        Ok((tail, Expression::DA(Box::new(da))))
    } else if let Ok((tail, a)) = atom(state, input) {
        Ok((tail, Expression::A(Box::new(a))))
    } else if let Ok((tail, fr)) = folded_relationship(input) {
        Ok((tail, Expression::FR(Box::new(fr))))
    } else {
        // if state.disable_operation || state.disable_relationship {
        //     eprintln!("== retrying unrestricted expression");
        //     return expression(
        //         InputState {
        //             disable_operation: false,
        //             disable_relationship: false,
        //             call_depth: state.call_depth,
        //         },
        //         input,
        //     );
        // }
        Err(Err::Error(Error {
            code: ErrorKind::Alt,
            input: "",
        }))
    }
}

pub fn quantification(state: InputState, input: &str) -> IResult<&str, Quantification> {
    debug_try("quantification", input);
    let expression = |i| expression(state, i);
    let (tail, (quantifier, decos, _, expr)) =
        tuple((quantifier, decorations, whitespace, expression))(input)?;
    Ok((
        tail,
        Quantification {
            expression: expr,
            quantifier: Decorated::new(quantifier, decos),
        },
    ))
}

#[test]
fn test_quantification() {
    assert_eq!(
        quantification(
            InputState::new(),
            r#"AA n in NN, c"D"^n = lim_(i=0)^n c"D""#
        ),
        Ok((
            "",
            Quantification {
                quantifier: Decorated::none(Quantifier::Universal),
                expression: Expression::R(Box::new(Relationship {
                    lhs: Expression::DA(Box::new(Decorated::none(
                        Atom::Name("n".to_string()).into()
                    ))),
                    relation: Decorated::none(Relation::ElementOf),
                    negated: false,
                    rhs: Expression::O(Box::new(Operation {
                        lhs: Some(Expression::DA(Box::new(Decorated::none(Atom::Name(
                            "NN".into()
                        ))))),
                        operator: Decorated::none(Operator::Binary(BinaryOperator::Comma)),
                        rhs: Some(Expression::R(Box::new(Relationship {
                            lhs: Expression::DA(Box::new(Decorated::none(Atom::Text(Text {
                                font: TextFont::Caligraphic,
                                content: "D".into(),
                            })))),
                            relation: Decorated::none(Relation::Equals),
                            negated: false,
                            rhs: Expression::O(Box::new(Operation {
                                lhs: None,
                                operator: Decorated {
                                    decoratee: Operator::Big(BigOperator::Limit),
                                    decorations: Decorations {
                                        sub: Some(Atom::Group(Group::Parenthesized(
                                            Expression::R(Box::new(Relationship {
                                                lhs: Expression::DA(Box::new(Decorated::none(
                                                    Atom::Name("i".to_string())
                                                ))),
                                                relation: Decorated::none(Relation::Equals),
                                                negated: false,
                                                rhs: Expression::DA(Box::new(Decorated::none(
                                                    Atom::Number(Number::Integer(0))
                                                )))
                                            }))
                                        ))),
                                        sup: Some(Atom::Name("n".to_string())),
                                        under: None,
                                        over: None
                                    }
                                },
                                rhs: Some(Expression::DA(Box::new(Decorated::none(Atom::Text(
                                    Text {
                                        font: TextFont::Caligraphic,
                                        content: "D".into(),
                                    }
                                )))))
                            }))
                        })))
                    }))
                }))
            }
        ))
    )
}

pub fn quantifier(input: &str) -> IResult<&str, Quantifier> {
    debug_try("quantifier", input);
    if let Ok((tail, _)) = alt::<_, _, Error<_>, _>((tag("∀"), tag("AA"), tag("forall")))(input) {
        Ok((tail, Quantifier::Universal))
    } else if let Ok((tail, _)) =
        alt::<_, _, Error<_>, _>((tag("∃"), tag("EE"), tag("exists")))(input)
    {
        if let Ok((tail, _)) = tag::<_, _, Error<_>>("!")(tail) {
            Ok((tail, Quantifier::UniquelyExistential))
        } else {
            Ok((tail, Quantifier::Existential))
        }
    } else {
        Err(Err::Error(Error {
            code: ErrorKind::Alt,
            input: "",
        }))
    }
}

pub fn folded_relationship(input: &str) -> IResult<&str, FoldedRelationship> {
    debug_try("folded_relationship", input);
    let state = InputState {
        disable_operation: false,
        disable_relationship: true,
        call_depth: 0,
    };
    let expression = |i| expression(state, i);
    let (tail, (_, neg, rel, _, rhs)) = tuple((
        opt(whitespace),
        opt(alt((tag("not"), tag("!")))),
        decorated(relation),
        opt(whitespace),
        expression,
    ))(input)?;
    Ok((
        tail,
        FoldedRelationship {
            relation: rel,
            negated: match neg {
                Some(_) => true,
                None => false,
            },
            rhs: rhs,
        },
    ))
}

pub fn relationship(state: InputState, input: &str) -> IResult<&str, Relationship> {
    if state.disable_relationship {
        return Err(Err::Error(Error {
            input: input,
            code: ErrorKind::IsNot,
        }));
    }
    debug_try("relationship", input);
    let expression_no_relationship = |i| {
        expression(
            InputState {
                disable_operation: state.disable_operation,
                disable_relationship: true,
                call_depth: state.call_depth,
            },
            i,
        )
    };
    let expression_relationship = |i| expression(state, i);
    let atom = |i| atom(state, i);
    // Special case to parse expr --(x->0)-> 4
    eprintln!("    trying --(->)-> special case");
    if let Ok((tail, (lhs, _, neg, _, pred, _, _, rhs))) = tuple((
        expression_no_relationship,
        opt(whitespace),
        opt(alt((tag("not"), tag("!")))),
        tag("--"),
        atom,
        tag("->"),
        opt(whitespace),
        expression_relationship,
    ))(input)
    {
        return Ok((
            tail,
            Relationship {
                lhs: lhs,
                negated: match neg {
                    Some(_) => true,
                    None => false,
                },
                relation: Decorated::new(
                    Relation::Tends,
                    Decorations {
                        sub: Some(pred),
                        sup: None,
                        under: None,
                        over: None,
                    },
                ),
                rhs: rhs,
            },
        ));
    }
    eprintln!("     trying regular");
    let (tail, (lhs, _, neg, rel, deco, _, rhs)) = tuple((
        expression_no_relationship,
        opt(whitespace),
        opt(alt((tag("not"), tag("!")))),
        relation,
        decorations,
        opt(whitespace),
        expression_relationship,
    ))(input)?;

    Ok((
        tail,
        Relationship {
            lhs: lhs,
            negated: match neg {
                Some(_) => true,
                None => false,
            },
            relation: Decorated::new(rel, deco),
            rhs: rhs,
        },
    ))
}

#[test]
pub fn test_relationship() {
    let state = InputState {
        disable_operation: false,
        disable_relationship: false,
        call_depth: 0,
    };
    assert_eq!(
        relationship(state, "x_n -> a => f(x_n) --(n -> oo)-> f(a)"),
        Ok((
            "",
            Relationship {
                lhs: Expression::DA(Box::new(Decorated {
                    decoratee: Atom::Name("x".into()),
                    decorations: Decorations {
                        under: None,
                        over: None,
                        sup: None,
                        sub: Some(Atom::Name("n".into())),
                    }
                })),
                negated: false,
                relation: Decorated::none(Relation::Tends),
                rhs: Expression::R(Box::new(Relationship {
                    lhs: Expression::DA(Box::new(Decorated::none(Atom::Name("a".into())))),
                    negated: false,
                    relation: Decorated::none(Relation::Implies),
                    rhs: Expression::R(Box::new(Relationship {
                        lhs: Expression::F(Box::new(FunctionCall {
                            caller: Decorated::none(Atom::Name("f".into())),
                            arguments: Expression::DA(Box::new(Decorated::sub(
                                Atom::Name("x".into()),
                                Atom::Name("n".into())
                            )))
                        })),
                        relation: Decorated::sub(
                            Relation::Tends,
                            Atom::Group(Group::Parenthesized(Expression::R(Box::new(
                                Relationship {
                                    lhs: Expression::DA(Box::new(Decorated::none(Atom::Name(
                                        "n".into()
                                    )))),
                                    relation: Decorated::none(Relation::Tends),
                                    negated: false,
                                    rhs: Expression::DA(Box::new(Decorated::none(Atom::Symbol(
                                        Symbol::Infinity
                                    )))),
                                }
                            ))))
                        ),
                        negated: false,
                        rhs: Expression::F(Box::new(FunctionCall {
                            caller: Decorated::none(Atom::Name("f".into())),
                            arguments: Expression::DA(Box::new(Decorated::none(Atom::Name(
                                "a".into()
                            ))))
                        }))
                    })),
                }))
            }
        ))
    );
    assert_eq!(
        relationship(state, "e !in E => f in F"),
        Ok((
            "",
            Relationship {
                lhs: Expression::DA(Box::new(Decorated::none(Atom::Name("e".into())))),
                relation: Decorated::none(Relation::ElementOf),
                negated: true,
                rhs: Expression::R(Box::new(Relationship {
                    lhs: Expression::DA(Box::new(Decorated::none(Atom::Name("E".into())))),
                    relation: Decorated::none(Relation::Implies),
                    negated: false,
                    rhs: Expression::R(Box::new(Relationship {
                        lhs: Expression::DA(Box::new(Decorated::none(Atom::Name("f".into())))),
                        relation: Decorated::none(Relation::ElementOf),
                        negated: false,
                        rhs: Expression::DA(Box::new(Decorated::none(Atom::Name("F".into()))))
                    }))
                }))
            }
        ))
    )
}

pub fn number(input: &str) -> IResult<&str, Number> {
    debug_try("number", input);
    if let Ok((tail, float)) = _float_only_when_comma(input) {
        Ok((tail, float))
    } else if let Ok((tail, (neg, number))) = tuple((opt(char('-')), digit1::<_, Error<_>>))(input)
    {
        Ok((
            tail,
            Number::Integer(
                number.parse::<i32>().unwrap()
                    * match neg {
                        Some(_) => -1,
                        None => 1,
                    },
            ),
        ))
    } else {
        Err(Err::Error(Error {
            code: ErrorKind::Digit,
            input: "",
        }))
    }
}

pub fn _float_only_when_comma(input: &str) -> IResult<&str, Number> {
    if !input.contains('.') {
        debug_try("_float_only_when_comma", input);
        return Err(Err::Error(Error {
            code: ErrorKind::Char,
            input: "",
        }));
    }

    let (tail, (neg, nb)) = tuple((opt(char('-')), float::<_, Error<_>>))(input)?;
    Ok((
        tail,
        Number::Float(
            nb * match neg {
                Some(_) => -1.0,
                None => 1.0,
            },
        ),
    ))
}

pub fn atom(state: InputState, input: &str) -> IResult<&str, Atom, Error<&str>> {
    debug_try("atom", input);
    if let Ok((tail, txt)) = text(input) {
        Ok((tail, Atom::Text(txt)))
    } else if let Ok((tail, number)) = number(input) {
        Ok((tail, Atom::Number(number)))
    } else if let Ok((tail, qty)) = quantity(input) {
        Ok((tail, Atom::Quantity(qty)))
    } else if let Ok((tail, bare_op)) = binary_operator(input) {
        Ok((tail, Atom::BareOperator(bare_op)))
    } else if let Ok((tail, symb)) = symbol(input) {
        Ok((tail, Atom::Symbol(symb)))
    } else if let Ok((tail, name)) = name(input) {
        Ok((tail, Atom::Name(name)))
    } else if let Ok((tail, grp)) = group(state, input) {
        Ok((tail, Atom::Group(grp)))
    } else {
        Err(Err::Error(Error {
            input: "",
            code: ErrorKind::Alt,
        }))
    }
}

pub fn name(input: &str) -> IResult<&str, String> {
    debug_try("name", input);
    let mut matched = String::from("");
    for grapheme in input.graphemes(true) {
        if grapheme.chars().any(|c| c.is_alphabetic()) {
            matched += grapheme;
        } else {
            return if matched.len() == 0 {
                Err(Err::Error(Error {
                    input: input,
                    code: ErrorKind::Alpha,
                }))
            } else {
                Ok((&input[matched.len()..], matched))
            };
        }
    }
    Ok(("", matched))
}

#[test]
pub fn test_atom() {
    let state = InputState {
        disable_operation: false,
        disable_relationship: false,
        call_depth: 0,
    };
    assert_eq!(atom(state, "alpha"), Ok(("", Atom::Name("alpha".into()))));
    assert_eq!(atom(state, "abc87"), Ok(("87", Atom::Name("abc".into()))));
    assert_eq!(
        atom(state, "65486.848pal"),
        Ok(("pal", Atom::Number(Number::Float(65486.848))))
    );
    assert_eq!(
        atom(state, r#"(int_0^1 sum e in o"R") not= _|_"#),
        Ok((
            " not= _|_",
            Atom::Group(Group::Parenthesized(Expression::R(Box::new(
                Relationship {
                    lhs: Expression::O(Box::new(Operation {
                        lhs: None,
                        operator: Decorated::new(
                            Operator::Big(BigOperator::Integral),
                            Decorations {
                                sub: Some(Atom::Number(Number::Integer(0))),
                                sup: Some(Atom::Number(Number::Integer(0))),
                                under: None,
                                over: None
                            }
                        ),
                        rhs: Some(Expression::O(Box::new(Operation {
                            lhs: None,
                            operator: Decorated::none(Operator::Big(BigOperator::Sum)),
                            rhs: Some(Expression::A(Box::new(Atom::Name("e".into())))),
                        })))
                    })),
                    negated: false,
                    relation: Decorated::none(Relation::ElementOf),
                    rhs: Expression::A(Box::new(Atom::Text(Text {
                        font: TextFont::BlackboardBold,
                        content: "R".into(),
                    })))
                }
            ))))
        ))
    );
}

pub fn text(input: &str) -> IResult<&str, Text> {
    debug_try("text", input);
    let (tail, (maybe_font, content)) = tuple((opt(text_font), quoted_string))(input)?;
    Ok((
        tail,
        Text {
            content: content,
            font: match maybe_font {
                Some(f) => f,
                None => TextFont::Normal,
            },
        },
    ))
}

#[test]
pub fn test_text() {
    assert_eq!(
        text(r#"o"R \"forall\""  is the set of real numbers."#),
        Ok((
            "  is the set of real numbers.",
            Text {
                content: "R \"forall\"".into(),
                font: TextFont::BlackboardBold,
            }
        ))
    );
    assert_eq!(
        text(r#""""""#),
        Ok((
            r#""""#,
            Text {
                content: "".into(),
                font: TextFont::Normal,
            }
        ))
    );
    assert_eq!(
        text("lim_(x->"),
        Err(Err::Error(Error {
            code: ErrorKind::Tag,
            input: "lim_(x->",
        }))
    );
}

pub fn quoted_string(input: &str) -> IResult<&str, String> {
    debug_try("quoted_string", input);
    delimited(tag("\""), _in_quotes, tag("\""))(input)
}

pub fn _in_quotes(input: &str) -> IResult<&str, String> {
    debug_try("_in_quotes", input);
    let mut content = String::new();
    let mut skip_delimiter = false;
    for (i, ch) in input.char_indices() {
        if ch == '\\' && !skip_delimiter {
            skip_delimiter = true
        } else if ch == '"' && !skip_delimiter {
            return Ok((&input[i..], content));
        } else {
            content.push(ch);
            skip_delimiter = false;
        }
    }
    Err(Err::Incomplete(nom::Needed::Unknown))
}

pub fn text_font(input: &str) -> IResult<&str, TextFont> {
    debug_try("text_font", input);
    match_enum_variants_to_literals(
        input,
        vec![
            (TextFont::Caligraphic, vec!["c"]),
            (TextFont::Fraktur, vec!["f"]),
            (TextFont::BlackboardBold, vec!["o", "bb"]),
            (TextFont::Bold, vec!["b"]),
            (TextFont::Sans, vec!["s"]),
            (TextFont::Monospace, vec!["m", "tt"]),
            (TextFont::Normal, vec!["n"]),
            (TextFont::Italic, vec!["i"]),
            (TextFont::Script, vec!["s"]),
            (TextFont::MathOperator, vec!["op"]),
        ],
    )
}

pub fn group(state: InputState, input: &str) -> IResult<&str, Group> {
    debug_try("group", input);
    let expression = move |i| expression(state, i);
    if let Ok((tail, expr)) = surrounded(expression, "{", "}", true)(input) {
        Ok((tail, Group::Braced(expr)))
    } else if let Ok((tail, expr)) = surrounded(expression, "(", ")", true)(input) {
        Ok((tail, Group::Parenthesized(expr)))
    } else if let Ok((tail, expr)) = surrounded(expression, "[", "]", true)(input) {
        Ok((tail, Group::Bracketed(expr)))
    } else if let Ok((tail, expr)) = surrounded(expression, "]", "[", false)(input) {
        Ok((tail, Group::OpenRange(expr)))
    } else if let Ok((tail, expr)) = surrounded(expression, "[", "[", false)(input) {
        Ok((tail, Group::RightOpenRange(expr)))
    } else if let Ok((tail, expr)) = surrounded(expression, "]", "]", false)(input) {
        Ok((tail, Group::LeftOpenRange(expr)))
    } else if let Ok((tail, expr)) = surrounded(expression, "|]", "[|", false)(input) {
        Ok((tail, Group::OpenIntegerRange(expr)))
    } else if let Ok((tail, expr)) = surrounded(expression, "|]", "|]", false)(input) {
        Ok((tail, Group::LeftOpenIntegerRange(expr)))
    } else if let Ok((tail, expr)) = surrounded(expression, "[|", "[|", false)(input) {
        Ok((tail, Group::RightOpenIntegerRange(expr)))
    } else if let Ok((tail, expr)) = surrounded(expression, "[|", "|]", true)(input) {
        Ok((tail, Group::WhiteBracketed(expr)))
    } else if let Ok((tail, expr)) = surrounded(expression, "<", ">", false)(input) {
        Ok((tail, Group::AngleBracketed(expr)))
    } else if let Ok((tail, expr)) = surrounded(expression, "|", "|", true)(input) {
        Ok((tail, Group::AbsoluteValue(expr)))
    } else if let Ok((tail, expr)) = surrounded(expression, "||", "||", true)(input) {
        Ok((tail, Group::Norm(expr)))
    } else {
        Err(Err::Error(Error {
            code: ErrorKind::Alt,
            input: "",
        }))
    }
}

pub fn surrounded<'a, F: 'a, O, E: ParseError<&'a str>>(
    f: F,
    opening: &'static str,
    closing: &'static str,
    allow_whitespace: bool,
) -> impl Fn(&'a str) -> IResult<&'a str, O, E>
where
    F: Fn(&'a str) -> IResult<&'a str, O, E>,
{
    move |input| delimited(tag(opening), &f, tag(closing))(input)
}

pub fn decorated<'a, F: 'a, O>(f: F) -> impl Fn(&'a str) -> IResult<&'a str, Decorated<O>>
where
    F: Fn(&'a str) -> IResult<&'a str, O>,
{
    move |input| match tuple((&f, decorations))(input) {
        Ok((tail, (thing, decorations))) => Ok((
            tail,
            Decorated {
                decoratee: thing,
                decorations: decorations,
            },
        )),
        Err(e) => Err(e),
    }
}

pub fn whitespace(input: &str) -> IResult<&str, &str> {
    debug_try("whitespace", input);
    let mut tail_starts_at = 0;
    for grapheme in input.graphemes(true) {
        if grapheme != " " {
            break;
        }
        tail_starts_at += 1;
    }
    if tail_starts_at == 0 {
        Err(Err::Error(Error {
            code: ErrorKind::NoneOf,
            input: "",
        }))
    } else {
        let (parsed, tail) = input.split_at(tail_starts_at);
        Ok((tail, parsed))
    }
}

pub fn quantity(input: &str) -> IResult<&str, Quantity> {
    debug_try("quantity", input);
    let (tail, (value, _, u)) = tuple((number, whitespace, unit))(input)?;
    Ok((
        tail,
        Quantity {
            unit: Box::new(u),
            value: value,
        },
    ))
}

#[test]
pub fn test_quantity() {
    assert_eq!(
        quantity("352.8 µS/s*m^2@"),
        Ok((
            "@",
            Quantity {
                value: Number::Float(352.8),
                unit: Box::new(Unit::Ratio(
                    Box::new(Unit::Prefixed(UnitPrefix::Micro, FundamentalUnit::Siemens)),
                    Box::new(Unit::Product(
                        Box::new(Unit::Fundamental(FundamentalUnit::Second)),
                        Box::new(Unit::Power(
                            Box::new(Unit::Fundamental(FundamentalUnit::Meter)),
                            Atom::Number(Number::Integer(2)),
                        ))
                    )),
                ))
            }
        ))
    )
}

pub fn unit(input: &str) -> IResult<&str, Unit> {
    debug_try("unit", input);
    if let Ok((tail, (a, _, b))) = tuple((_unit_no_bin_op, char('*'), unit))(input) {
        Ok((tail, Unit::Product(Box::new(a), Box::new(b))))
    } else if let Ok((tail, (a, _, b))) = tuple((_unit_no_bin_op, char('/'), unit))(input) {
        Ok((tail, Unit::Ratio(Box::new(a), Box::new(b))))
    } else {
        _unit_no_bin_op(input)
    }
}

pub fn _unit_no_bin_op(input: &str) -> IResult<&str, Unit> {
    debug_try("_unit_no_bin_op", input);
    if let Ok((tail, (u, _, pow))) = tuple((_unit_no_op, char('^'), |i| {
        atom(
            InputState {
                disable_operation: false,
                disable_relationship: false,
                call_depth: 0,
            },
            i,
        )
    }))(input)
    {
        Ok((tail, Unit::Power(Box::new(u), pow)))
    } else {
        _unit_no_op(input)
    }
}

pub fn _unit_no_op(input: &str) -> IResult<&str, Unit> {
    debug_try("_unit_no_op", input);
    if let Ok((tail, (prefix, fundamental))) = tuple((unit_prefix, fundamental_unit))(input) {
        Ok((tail, Unit::Prefixed(prefix, fundamental)))
    } else if let Ok((tail, u)) = fundamental_unit(input) {
        Ok((tail, Unit::Fundamental(u)))
    } else {
        Err(Err::Error(Error {
            code: ErrorKind::Alt,
            input: "",
        }))
    }
}

#[test]
pub fn test_unit() {
    assert_eq!(
        unit("MeV*km^-1, "),
        Ok((
            ", ",
            Unit::Product(
                Box::new(Unit::Prefixed(
                    UnitPrefix::Mega,
                    FundamentalUnit::ElectronVolt
                )),
                Box::new(Unit::Power(
                    Box::new(Unit::Prefixed(UnitPrefix::Kilo, FundamentalUnit::Meter)),
                    Atom::Number(Number::Integer(-1)),
                ))
            )
        ))
    )
}

pub fn unit_prefix(input: &str) -> IResult<&str, UnitPrefix> {
    debug_try("unit_prefix", input);
    match_enum_variants_to_literals(
        input,
        vec![
            (UnitPrefix::Nano, vec!["n"]),
            (UnitPrefix::Micro, vec!["µ", "micro", "mu"]),
            (UnitPrefix::Deca, vec!["da"]),
            (UnitPrefix::Milli, vec!["m"]),
            (UnitPrefix::Deci, vec!["d"]),
            (UnitPrefix::Centi, vec!["c"]),
            (UnitPrefix::Hecto, vec!["h"]),
            (UnitPrefix::Kilo, vec!["k"]),
            (UnitPrefix::Mega, vec!["M"]),
            (UnitPrefix::Giga, vec!["G"]),
            (UnitPrefix::Tera, vec!["T"]),
            (UnitPrefix::Peta, vec!["P"]),
        ],
    )
}

pub fn fundamental_unit(input: &str) -> IResult<&str, FundamentalUnit> {
    debug_try("fundamental_unit", input);
    match_enum_variants_to_literals(
        input,
        vec![
            (FundamentalUnit::CelsisusDegree, vec!["°C", "°c"]),
            (FundamentalUnit::FarenheitDegree, vec!["°F"]),
            (FundamentalUnit::FrenchDegree, vec!["°fH", "°f"]),
            (FundamentalUnit::Degree, vec!["°"]),
            (FundamentalUnit::Radian, vec!["rad"]),
            (FundamentalUnit::MeterOfHydrogen, vec!["mHg"]),
            (FundamentalUnit::Meter, vec!["m"]),
            (FundamentalUnit::Gram, vec!["g"]),
            (FundamentalUnit::Mole, vec!["mol"]),
            (FundamentalUnit::Second, vec!["s"]),
            (FundamentalUnit::Kelvin, vec!["K"]),
            (FundamentalUnit::Liter, vec!["L"]),
            (FundamentalUnit::Farad, vec!["F"]),
            (FundamentalUnit::Henry, vec!["H"]),
            (FundamentalUnit::ElectronVolt, vec!["eV"]),
            (FundamentalUnit::Lux, vec!["lux"]),
            (FundamentalUnit::Ampere, vec!["A"]),
            (FundamentalUnit::Volt, vec!["V"]),
            (FundamentalUnit::Ohm, vec!["Ohm", "ohm", "Ω"]),
            (FundamentalUnit::Bar, vec!["bar"]),
            (FundamentalUnit::Atmosphere, vec!["atm"]),
            (FundamentalUnit::Pascal, vec!["Pa"]),
            (FundamentalUnit::Joule, vec!["J"]),
            (FundamentalUnit::Watt, vec!["W"]),
            (FundamentalUnit::Siemens, vec!["S"]),
        ],
    )
}

pub fn operation(state: InputState, input: &str) -> IResult<&str, Operation> {
    if state.disable_operation {
        return Err(Err::Error(Error {
            input: input,
            code: ErrorKind::IsNot,
        }));
    }
    let expression_no_operation = move |i| {
        expression(
            InputState {
                disable_operation: true,
                disable_relationship: state.disable_relationship,
                call_depth: state.call_depth,
            },
            i,
        )
    };
    let expression_operation = move |i| expression(state, i);
    debug_try("operation", input);
    if let Ok((tail, (lhs, _, operator, _, rhs))) = tuple((
        expression_no_operation,
        opt(whitespace),
        decorated(binary_operator),
        opt(whitespace),
        expression_operation,
    ))(input)
    {
        Ok((
            tail,
            Operation {
                lhs: Some(lhs),
                operator: Decorated {
                    decoratee: Operator::Binary(operator.decoratee),
                    decorations: operator.decorations,
                },
                rhs: Some(rhs),
            },
        ))
    } else if let Ok((tail, (operator, decorations, rhs))) = tuple((
        preceded(whitespace, prefix_operator),
        decorations,
        terminated(expression_operation, whitespace),
    ))(input)
    {
        Ok((
            tail,
            Operation {
                lhs: None,
                operator: Decorated::new(Operator::Prefix(operator), decorations),
                rhs: Some(rhs),
            },
        ))
    } else if let Ok((tail, (lhs, operator, decorations))) = tuple((
        preceded(whitespace, expression_operation),
        postfix_operator,
        terminated(decorations, whitespace),
    ))(input)
    {
        Ok((
            tail,
            Operation {
                lhs: Some(lhs),
                operator: Decorated::new(Operator::Postfix(operator), decorations),
                rhs: None,
            },
        ))
    } else if let Ok((tail, (operator, decorations, rhs))) = tuple((
        big_operator,
        decorations,
        terminated(expression_operation, whitespace),
    ))(input)
    {
        Ok((
            tail,
            Operation {
                lhs: None,
                operator: Decorated::new(Operator::Big(operator), decorations),
                rhs: Some(rhs),
            },
        ))
    } else {
        Err(nom::Err::Error(Error {
            code: ErrorKind::Alt,
            input: "",
        }))
    }
}

pub fn decorations(input: &str) -> IResult<&str, Decorations> {
    debug_try("decorations", input);
    let atom = |i| {
        atom(
            InputState {
                disable_operation: false,
                disable_relationship: false,
                call_depth: 0,
            },
            i,
        )
    };
    let (tail, (under, over, sub, sup)) = tuple((
        opt(preceded(tag("__"), atom)),
        opt(preceded(tag("^^"), atom)),
        opt(preceded(tag("_"), atom)),
        opt(preceded(tag("^"), atom)),
    ))(input)?;

    Ok((
        tail,
        Decorations {
            sub,
            sup,
            under,
            over,
        },
    ))
}

#[test]
pub fn test_decorations() {
    assert_eq!(
        decorations(r#"__8^^m"good stuff right therre!!! amirite^4???"_(x->0)88"#),
        Ok((
            "88",
            Decorations {
                under: Some(Atom::Number(Number::Integer(8))),
                over: Some(Atom::Text(Text {
                    font: TextFont::Monospace,
                    content: String::from("good stuff right therre!!! amirite^4???")
                })),
                sub: Some(Atom::Group(Group::Parenthesized(Expression::R(Box::new(
                    Relationship {
                        lhs: Expression::DA(Box::new(Decorated {
                            decoratee: Atom::Name("x".to_string()),
                            decorations: Decorations::none(),
                        })),
                        rhs: Expression::DA(Box::new(Decorated {
                            decoratee: Atom::Number(Number::Integer(0)),
                            decorations: Decorations::none(),
                        })),
                        relation: Decorated::none(Relation::Tends),
                        negated: false,
                    }
                ))))),
                sup: None,
            }
        ))
    )
}

pub fn relation(input: &str) -> IResult<&str, Relation> {
    debug_try("relation", input);
    match_enum_variants_to_literals(
        input,
        vec![
            (Relation::Implies, vec!["=>", "⇒"]),
            (Relation::Iff, vec!["<=>", "⇔", "iff"]),
            (Relation::ImpliedBy, vec!["⇐", "=<", "impliedby"]),
            (
                Relation::Congruent,
                vec!["=-", "-=", "==", "congruent", "cong", "congru"],
            ),
            (Relation::MapsTo, vec!["|->", "↦", "mapsto"]),
            (Relation::GreaterOrEqual, vec![">=", "≥"]),
            (Relation::LessOrEqual, vec!["<=", "≤"]),
            (Relation::Equals, vec!["="]),
            (Relation::Greater, vec![">"]),
            (Relation::Less, vec!["<"]),
            (Relation::Tends, vec!["->"]),
            (Relation::Subset, vec!["subset", "included", "cc", "⊂"]),
            (
                Relation::Superset,
                vec!["supset", "superset", "includes", "))", "⊃"],
            ),
            (Relation::ElementOf, vec!["in", "∈", "€"]),
            (Relation::EquivalentTo, vec!["~"]),
        ],
    )
}

pub fn operator(input: &str) -> IResult<&str, Operator> {
    debug_try("operator", input);
    if let Ok((tail, op)) = binary_operator(input) {
        Ok((tail, Operator::Binary(op)))
    } else if let Ok((tail, op)) = prefix_operator(input) {
        Ok((tail, Operator::Prefix(op)))
    } else if let Ok((tail, op)) = postfix_operator(input) {
        Ok((tail, Operator::Postfix(op)))
    } else if let Ok((tail, op)) = big_operator(input) {
        Ok((tail, Operator::Big(op)))
    } else {
        Err(Err::Error(Error {
            code: ErrorKind::Alt,
            input: "",
        }))
    }
}

pub fn binary_operator(input: &str) -> IResult<&str, BinaryOperator> {
    debug_try("binary_operator", input);
    match_enum_variants_to_literals(
        input,
        vec![
            (BinaryOperator::Addition, vec!["+"]),
            (BinaryOperator::Comma, vec![", "]),
            (BinaryOperator::Difference, vec!["-"]),
            (BinaryOperator::Division, vec!["-:-", "÷"]),
            (BinaryOperator::Fraction, vec!["/"]),
            (BinaryOperator::Remainder, vec!["mod"]),
            (BinaryOperator::DirectAddition, vec!["(+)"]),
            (BinaryOperator::CartesianProduct, vec!["*", "xx"]),
            (BinaryOperator::DotProduct, vec!["*"]),
            (BinaryOperator::VectorProduct, vec!["^", "/\\"]),
            (BinaryOperator::Composition, vec!["°"]),
            (BinaryOperator::Union, vec!["union", "uu"]),
            (BinaryOperator::Intersection, vec!["inter", "nn"]),
            (BinaryOperator::SetDifference, vec!["\\"]),
            (BinaryOperator::SymmetricDifference, vec!["Δ", "symdiff"]),
        ],
    )
}

#[test]
pub fn test_binary_operator() {
    assert_eq!(binary_operator("+"), Ok(("", BinaryOperator::Addition)));
    assert_eq!(
        prefix_operator("hmmmmmm"),
        Err(nom::Err::Error(Error {
            code: ErrorKind::Alt,
            input: ""
        }))
    );
}

pub fn prefix_operator(input: &str) -> IResult<&str, PrefixOperator> {
    debug_try("prefix_operator", input);
    match_enum_variants_to_literals(
        input,
        vec![
            (
                PrefixOperator::Transposition,
                vec!["^t", "trans", "transpose"],
            ),
            (
                PrefixOperator::Complement,
                vec!["^c", "^C", "compl", "complement"],
            ),
            (PrefixOperator::Negation, vec!["¬", "not", "-,", "negate"]),
        ],
    )
}

#[test]
pub fn test_prefix_operator() {
    assert_eq!(
        prefix_operator("¬A^B"),
        Ok(("A^B", PrefixOperator::Negation))
    );
    assert_eq!(
        prefix_operator("hmmmmmm"),
        Err(nom::Err::Error(Error {
            code: ErrorKind::Alt,
            input: ""
        }))
    );
}

pub fn postfix_operator(input: &str) -> IResult<&str, PostfixOperator> {
    debug_try("postfix_operator", input);
    match_enum_variants_to_literals(
        input,
        vec![
            (PostfixOperator::VectorMarker, vec!["->"]),
            (
                PostfixOperator::Orthogonal,
                vec!["^_|_", "^bot", "^orthog", "^orth", "^perp", "^orthogonal"],
            ),
            (PostfixOperator::Factorial, vec!["!"]),
        ],
    )
}

#[test]
pub fn test_postfix_operator() {
    assert_eq!(postfix_operator("!"), Ok(("", PostfixOperator::Factorial)));
    assert_eq!(
        postfix_operator("haha"),
        Err(nom::Err::Error(Error {
            code: ErrorKind::Alt,
            input: ""
        }))
    );
}

pub fn big_operator(input: &str) -> IResult<&str, BigOperator> {
    debug_try("big_operator", input);
    match_enum_variants_to_literals(
        input,
        vec![
            (BigOperator::Sum, vec!["sum", "Sum", "∑"]),
            (BigOperator::Product, vec!["Product"]),
            (BigOperator::Coproduct, vec!["Coproduct"]),
            (
                BigOperator::Integral,
                vec!["int", "integral", "Integral", "∫"],
            ),
            (BigOperator::Limit, vec!["lim", "Limit", "limit"]),
        ],
    )
}

#[test]
pub fn test_big_operator() {
    assert_eq!(
        big_operator("lim_(x->0)"),
        Ok(("_(x->0)", BigOperator::Limit))
    );

    assert_eq!(
        big_operator("Sum_(n=0)^oo u_n"),
        Ok(("_(n=0)^oo u_n", BigOperator::Sum))
    );

    assert_eq!(
        big_operator("UUNion"),
        Err(nom::Err::Error(Error {
            code: nom::error::ErrorKind::Alt,
            input: ""
        }))
    )
}

pub fn symbol(input: &str) -> IResult<&str, Symbol> {
    match_enum_variants_to_literals(
        input,
        vec![
            (Symbol::Infinity, vec!["oo", "infinity", "∞"]),
            (Symbol::Aleph, vec!["aleph"]),
            (Symbol::Gimmel, vec!["gimmel"]),
            (Symbol::ProofEnd, vec!["[]"]), // notsure
            (Symbol::Contradiction, vec!["imp!", "contradiction"]),
        ],
    )
}

pub fn function_call(state: InputState, input: &str) -> IResult<&str, FunctionCall> {
    let atom = move |i| atom(state, i);
    let expression = move |i| expression(state, i);
    let (tail, (name, expr)) =
        tuple((decorated(atom), surrounded(expression, "(", ")", true)))(input)?;
    Ok((
        tail,
        FunctionCall {
            caller: name,
            arguments: expr,
        },
    ))
}

pub fn match_enum_variants_to_literals<'a, T>(
    input: &'a str,
    mapping: Vec<(T, Vec<&'a str>)>,
) -> IResult<&'a str, T> {
    for (variant, literals) in mapping {
        if let Some(tail) = try_prefixes(input, literals) {
            return Ok((tail, variant));
        }
    }
    Err(Err::Error(Error {
        code: ErrorKind::Alt,
        input: "",
    }))
}

pub fn try_prefixes<'a>(input: &'a str, prefixes: Vec<&'a str>) -> Option<&'a str> {
    for prefix in prefixes {
        if let Some(tail) = input.strip_prefix(prefix) {
            return Some(tail);
        }
    }
    None
}
