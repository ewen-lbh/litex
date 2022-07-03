use std::collections::HashMap;
use std::fmt::Binary;
#[macro_use]
extern crate maplit;
use nom::branch::{alt, Alt};
use nom::bytes::complete::{tag, tag_no_case};
use nom::character::complete::char;
use nom::combinator::opt;
use nom::error::{Error, ErrorKind, ParseError};
use nom::sequence::{delimited, preceded, terminated, tuple};
use nom::{number, Err, IResult, Parser};

#[derive(Debug, PartialEq)]
enum Expression<'a> {
    Q(Box<Quantification<'a>>),
    R(Box<Relationship<'a>>),
    A(Box<Atom<'a>>),
    O(Box<Operation<'a>>),
}

#[derive(Debug, PartialEq)]
struct Quantification<'a> {
    mode: QuantificationMode,
    decorations: Decorations<'a>,
    bound: Expression<'a>,
    quantified: Expression<'a>,
}

#[derive(Debug, PartialEq)]
struct Relationship<'a> {
    lhs: Expression<'a>,
    relation: &'a Relation,
    negated: bool,
    rhs: Expression<'a>,
}

#[derive(Debug, PartialEq)]
enum Number {
    Float(f32),
    Integer(i32),
}

#[derive(Debug, PartialEq)]
enum Atom<'a> {
    Name(String),
    Number(Number),
    Quantity(Quantity<'a>),
    BareOperator(BinaryOperator),
    Text(Text),
    Group(Group<'a>),
}

#[derive(Debug, PartialEq)]
struct Text {
    font: TextFont,
    content: String,
}
#[derive(Debug, PartialEq, Eq, Hash)]
enum TextFont {
    Caligraphic,
    Fraktur,
    BlackboardBold,
    Bold,
    Sans,
    Monospace,
    Normal,
    Italic,
    Script,
}
#[derive(Debug, PartialEq)]
enum Group<'a> {
    // { expr }
    Braced(Expression<'a>),
    // ( expr )
    Parenthesized(Expression<'a>),
    // [ expr ]
    Bracketed(Expression<'a>),
    // ]expr[
    OpenRange(Expression<'a>),
    // ]expr]
    LeftOpenRange(Expression<'a>),
    // [expr[
    RightOpenRange(Expression<'a>),
    // |]expr[|
    OpenIntegerRange(Expression<'a>),
    // |]expr|]
    LeftOpenIntegerRange(Expression<'a>),
    // [|expr[|
    RightOpenIntegerRange(Expression<'a>),
    // [| expr |]
    WhiteBracketed(Expression<'a>),
    // | expr |
    AbsoluteValue(Expression<'a>),
    // || expr ||
    Norm(Expression<'a>),
    // <expr>
    AngleBracketed(Expression<'a>),
}

#[derive(Debug, PartialEq)]
struct Quantity<'a> {
    value: Number,
    unit: Box<Unit<'a>>,
}

#[derive(Debug, PartialEq)]
enum Unit<'a> {
    Product(Box<Unit<'a>>, Box<Unit<'a>>),
    Ratio(Box<Unit<'a>>, Box<Unit<'a>>),
    Power(Box<Unit<'a>>, Atom<'a>),
    Prefixed(UnitPrefix, FundamentalUnit),
    Fundamental(FundamentalUnit),
}
#[derive(Debug, PartialEq, Eq, Hash)]
enum UnitPrefix {
    Nano,
    Micro,
    Milli,
    Deci,
    Centi,
    Deca,
    Hecto,
    Kilo,
    Mega,
    Giga,
    Tera,
    Peta,
}
#[derive(Debug, PartialEq, Eq, Hash)]
enum FundamentalUnit {
    CelsisusDegree,
    FarenheitDegree,
    FrenchDegree,
    Degree,
    Radian,
    MeterOfHydrogen,
    Meter,
    Gram,
    Mole,
    Second,
    Kelvin,
    Liter,
    Farad,
    Henry,
    ElectronVolt,
    Lux,
    Ampere,
    Volt,
    Ohm,
    Bar,
    Atmosphere,
    Pascal,
    Joule,
    Watt,
    Siemens,
}

#[derive(Debug, PartialEq)]
struct Operation<'a> {
    lhs: Option<Expression<'a>>,
    operator: Operator,
    operator_decorations: Decorations<'a>,
    rhs: Option<Expression<'a>>,
}
#[derive(Debug, PartialEq, Eq, Hash)]
enum QuantificationMode {
    Universal,
    Existential,
    UniquelyExistential,
}

#[derive(Debug, PartialEq)]
struct Decorations<'a> {
    sub: Option<Atom<'a>>,
    sup: Option<Atom<'a>>,
    under: Option<Atom<'a>>,
    over: Option<Atom<'a>>,
}
#[derive(Debug, PartialEq, Eq, Hash)]
enum Relation {
    Equals,
    Greater,
    Less,
    GreaterOrEqual,
    LessOrEqual,
    Tends,
    Subset,
    Superset,
    ElementOf,
    Implies,
    Iff,
    ImpliedBy,
    Congruent,
    EquivalentTo,
    MapsTo,
}

#[derive(Debug, PartialEq)]
enum Operator {
    Binary(BinaryOperator),
    Prefix(PrefixOperator),
    Postfix(PostfixOperator),
    Big(BigOperator),
}

#[derive(Debug, PartialEq, Eq, Hash)]
enum BinaryOperator {
    Addition,
    Difference,
    Division,
    Fraction,
    Remainder,
    DirectAddition,
    CartesianProduct,
    DotProduct,
    VectorProduct,
    Composition,
    Union,
    Intersection,
    SetDifference,
    SymmetricDifference,
}

#[derive(Debug, PartialEq, Eq, Hash)]
enum PrefixOperator {
    Transposition,
    Complement,
    Negation,
}

#[derive(Debug, PartialEq, Eq, Hash)]
enum PostfixOperator {
    VectorMarker,
    Orthogonal,
    Complement,
    Transposition,
    Negation,
    Factorial,
}

#[derive(Debug, PartialEq, Eq, Hash)]
enum BigOperator {
    Limit,
    Product,
    Sum,
    DirectSum,
    Coproduct,
    Union,
    Intersection,
    Integral,
    DoubleIntegral,
    TripleIntegral,
    QuadrupleIntegral,
    ContourIntegral,
    SurfaceIntegral,
    Minimum,
    Maximum,
    Infimum,
    Supremum,
}

// fn expression(input: &str) -> IResult<&str, Expression> {
//     let (tail, parsed) = alt(quantification, alt(relationship, alt(atom, operation)))(input)?;
//     match parsed {

//     }
// }

// fn quantification(input: &str) -> IResult<&str, Quantification> {

// }

fn group(input: &str) -> IResult<&str, Group> {
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

fn surrounded<'a, O, E, F>(
    mut f: F,
    opening: &'static str,
    closing: &'static str,
) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    F: Parser<&'a str, O, E>,
{
    move |input: &'a str| delimited(tag(opening), f, tag(closing))(input)
}

fn optional_whitespace<I: Clone, O, E: ParseError<I>, F>(
    mut f: F,
) -> impl FnMut(I) -> IResult<I, O, E>
where
    F: Parser<I, O, E>,
{
    move |input: I| delimited(opt(whitespace), f, opt(whitespace))(input)
}

fn quantity(input: &str) -> IResult<&str, Quantity> {
    let (tail, (value, _, u)) = tuple((number::complete::be_f32, whitespace, unit))(input)?;
    Ok((
        tail,
        Quantity {
            unit: Box::new(u),
            value: value,
        },
    ))
}

#[test]
fn test_quantity() {
    assert_eq!(
        quantity("352.8 µS/m^2."),
        Ok((
            ".",
            Quantity {
                value: Number::Float(352.8),
                unit: Box::new(Unit::Ratio(
                    Box::new(Unit::Prefixed(UnitPrefix::Micro, FundamentalUnit::Siemens)),
                    Box::new(Unit::Power(
                        Box::new(Unit::Fundamental(FundamentalUnit::Meter)),
                        Atom::Number(Number::Integer(2)),
                    ))
                ))
            }
        ))
    )
}

fn unit(input: &str) -> IResult<&str, Unit> {
    if let Ok((tail, (a, _, b))) = tuple((unit, char('*'), unit))(input) {
        Ok((tail, Unit::Product(Box::new(a), Box::new(b))))
    } else if let Ok((tail, (a, _, b))) = tuple((unit, char('/'), unit))(input) {
        Ok((tail, Unit::Ratio(Box::new(a), Box::new(b))))
    } else if let Ok((tail, (u, _, pow))) = tuple((unit, char('^'), atom))(input) {
        Ok((tail, Unit::Power(Box::new(u), pow)))
    } else if let Ok((tail, (prefix, fundamental))) = tuple((unit_prefix, fundamental_unit))(input)
    {
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
fn test_unit() {
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

fn unit_prefix(input: &str) -> IResult<&str, UnitPrefix> {
    match_enum_variants_to_literals(
        input,
        hashmap! {
            UnitPrefix::Nano => vec!["n"],
            UnitPrefix::Micro => vec!["µ", "micro", "mu"],
            UnitPrefix::Deca => vec!["da"],
            UnitPrefix::Milli => vec!["m"],
            UnitPrefix::Deci => vec!["d"],
            UnitPrefix::Centi => vec!["c"],
            UnitPrefix::Hecto => vec!["h"],
            UnitPrefix::Kilo => vec!["k"],
            UnitPrefix::Mega => vec!["M"],
            UnitPrefix::Giga => vec!["G"],
            UnitPrefix::Tera => vec!["T"],
            UnitPrefix::Peta => vec!["P"],
        },
    )
}

fn fundamental_unit(input: &str) -> IResult<&str, FundamentalUnit> {
    match_enum_variants_to_literals(
        input,
        hashmap! {
            FundamentalUnit::CelsisusDegree => vec!["°C", "°c"],
            FundamentalUnit::FarenheitDegree => vec!["°F"],
            FundamentalUnit::FrenchDegree => vec!["°fH", "°f"],
            FundamentalUnit::Degree => vec!["°"],
            FundamentalUnit::Radian => vec!["rad"],
            FundamentalUnit::MeterOfHydrogen => vec!["mHg"],
            FundamentalUnit::Meter => vec!["m"],
            FundamentalUnit::Gram => vec!["g"],
            FundamentalUnit::Mole => vec!["mol"],
            FundamentalUnit::Second => vec!["s"],
            FundamentalUnit::Kelvin => vec!["K"],
            FundamentalUnit::Liter => vec!["L"],
            FundamentalUnit::Farad => vec!["F"],
            FundamentalUnit::Henry => vec!["H"],
            FundamentalUnit::ElectronVolt => vec!["eV"],
            FundamentalUnit::Lux => vec!["lux"],
            FundamentalUnit::Ampere => vec!["A"],
            FundamentalUnit::Volt => vec!["V"],
            FundamentalUnit::Ohm => vec!["Ohm", "ohm", "Ω"],
            FundamentalUnit::Bar => vec!["bar"],
            FundamentalUnit::Atmosphere => vec!["atm"],
            FundamentalUnit::Pascal => vec!["Pa"],
            FundamentalUnit::Joule => vec!["J"],
            FundamentalUnit::Watt => vec!["W"],
            FundamentalUnit::Siemens => vec!["S"],
        },
    )
}

fn operation(input: &str) -> IResult<&str, Operation> {
    if let Ok((tail, (lhs, operator, decorations, rhs))) = tuple((
        terminated(expression, whitespace),
        binary_operator,
        decorations,
        preceded(whitespace, expression),
    ))(input)
    {
        Ok((
            tail,
            Operation {
                lhs: Some(lhs),
                operator: Operator::Binary(operator),
                operator_decorations: decorations,
                rhs: Some(rhs),
            },
        ))
    } else if let Ok((tail, (operator, decorations, rhs))) = tuple((
        preceded(whitespace, prefix_operator),
        decorations,
        terminated(expression, whitespace),
    ))(input)
    {
        Ok((
            tail,
            Operation {
                lhs: None,
                operator: Operator::Prefix(operator),
                operator_decorations: decorations,
                rhs: Some(rhs),
            },
        ))
    } else if let Ok((tail, (lhs, operator, decorations))) = tuple((
        preceded(whitespace, expression),
        postfix_operator,
        terminated(decorations, whitespace),
    ))(input)
    {
        Ok((
            tail,
            Operation {
                lhs: Some(lhs),
                operator: Operator::Postfix(operator),
                operator_decorations: decorations,
                rhs: None,
            },
        ))
    } else if let Ok((tail, (operator, decorations, rhs))) = tuple((
        big_operator,
        decorations,
        terminated(expression, whitespace),
    ))(input)
    {
        Ok((
            tail,
            Operation {
                lhs: None,
                operator: Operator::Big(operator),
                operator_decorations: decorations,
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

fn decorations(input: &str) -> IResult<&str, Decorations> {
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
fn test_decorations() {
    assert_eq!(
        decorations(r#"__8^^m"good stuff right therre!!! amirite^4???"_(x->0)88"#),
        Ok((
            "88",
            Decorations {
                under: Some(Atom::Number(Number::Integer(8))),
                over: Some(Atom::Text(Text {
                    font: TextFont::Monospace,
                    content: String::from("good stuff right there!!! amirite^4???")
                })),
                sub: Some(Atom::Group(Group::Parenthesized(Expression::R(Box::new(
                    Relationship {
                        lhs: Expression::A(Box::new(Atom::Name("x".to_string()))),
                        rhs: Expression::A(Box::new(Atom::Number(Number::Integer(0)))),
                        relation: &Relation::Tends,
                        negated: false
                    }
                ))))),
                sup: None,
            }
        ))
    )
}

fn relation(input: &str) -> IResult<&str, Relation> {
    match_enum_variants_to_literals(
        input,
        hashmap! {
            Relation::Equals => vec!["="],
        },
    )
}

fn operator(input: &str) -> IResult<&str, Operator> {
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

fn binary_operator(input: &str) -> IResult<&str, BinaryOperator> {
    match_enum_variants_to_literals(
        input,
        hashmap! {
            BinaryOperator::Addition => vec!["+"]
        },
    )
}

#[test]
fn test_binary_operator() {
    assert_eq!(binary_operator("+"), Ok(("", BinaryOperator::Addition)));
    assert_eq!(
        prefix_operator("hmmmmmm"),
        Err(nom::Err::Error(Error {
            code: ErrorKind::Alt,
            input: ""
        }))
    );
}

fn prefix_operator(input: &str) -> IResult<&str, PrefixOperator> {
    match_enum_variants_to_literals(
        input,
        hashmap! {
            PrefixOperator::Transposition => vec!["^t", "trans", "transpose"],
            PrefixOperator::Complement => vec!["^c", "^C", "compl", "complement"],
            PrefixOperator::Negation => vec!["¬", "not", "-,", "negate"]
        },
    )
}

#[test]
fn test_prefix_operator() {
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

fn postfix_operator(input: &str) -> IResult<&str, PostfixOperator> {
    match_enum_variants_to_literals(
        input,
        hashmap! {
            PostfixOperator::VectorMarker => vec!["->"],
            PostfixOperator::Orthogonal => vec!["^_|_", "^bot", "^orthog", "^orth", "^perp", "^orthogonal"],
            PostfixOperator::Factorial => vec!["!"],
        },
    )
}

#[test]
fn test_postfix_operator() {
    assert_eq!(postfix_operator("!"), Ok(("", PostfixOperator::Factorial)));
    assert_eq!(
        postfix_operator("haha"),
        Err(nom::Err::Error(Error {
            code: ErrorKind::Alt,
            input: ""
        }))
    );
}

fn big_operator(input: &str) -> IResult<&str, BigOperator> {
    match_enum_variants_to_literals(
        input,
        hashmap! {
            BigOperator::Sum => vec!["sum", "Sum", "∑"],
            BigOperator::Product => vec!["Product"],
            BigOperator::Coproduct => vec!["Coproduct"],
            BigOperator::Integral => vec!["int", "integral", "Integral", "∫"],
            BigOperator::Limit => vec!["lim", "Limit", "limit"],
        },
    )
}

#[test]
fn test_big_operator() {
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

fn match_enum_variants_to_literals<'a, T>(
    input: &'a str,
    mapping: HashMap<T, Vec<&'a str>>,
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

fn try_prefixes<'a>(input: &'a str, prefixes: Vec<&'a str>) -> Option<&'a str> {
    for prefix in prefixes {
        if let Some(tail) = input.strip_prefix(prefix) {
            return Some(tail);
        }
    }
    None
}

fn main() {}
