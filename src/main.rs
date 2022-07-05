use nom::number::complete::float;
use std::collections::HashMap;
use unicode_segmentation::UnicodeSegmentation;
#[macro_use]
extern crate maplit;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{alpha1, char, digit1};
use nom::combinator::opt;
use nom::error::{Error, ErrorKind, ParseError};
use nom::sequence::{delimited, preceded, terminated, tuple};
use nom::{Err, IResult};

// TODO custom derive macro to implement enum for PascalCase names as \camelCase (e.g. units)
trait EmitLatex {
    fn emit(&self) -> String;
}

trait EmitLatexHasParens {
    ///! should render (...) as ...
    fn emit_throw_parens(&self) -> String;
    ///! should render (...) as {...}
    fn emit_unwrap_parens(&self) -> String;
    ///! should render (...) as \left(...\right)
    fn emit_keep_parens(&self) -> String;
}

#[derive(Debug, PartialEq)]
enum Expression {
    Q(Box<Quantification>),
    R(Box<Relationship>),
    A(Box<Atom>),
    O(Box<Operation>),
}

impl EmitLatexHasParens for Expression {
    fn emit_keep_parens(&self) -> String {
        match self {
            Expression::A(atom) => atom.emit_keep_parens(),
            Expression::O(inner) => inner.emit(),
            Expression::Q(inner) => inner.emit(),
            Expression::R(inner) => inner.emit(),
        }
    }
    fn emit_throw_parens(&self) -> String {
        match self {
            Expression::A(atom) => atom.emit_throw_parens(),
            Expression::O(inner) => inner.emit(),
            Expression::Q(inner) => inner.emit(),
            Expression::R(inner) => inner.emit(),
        }
    }
    fn emit_unwrap_parens(&self) -> String {
        match self {
            Expression::A(atom) => atom.emit_unwrap_parens(),
            Expression::O(inner) => inner.emit(),
            Expression::Q(inner) => inner.emit(),
            Expression::R(inner) => inner.emit(),
        }
    }
}

#[derive(Debug, PartialEq)]
struct Quantification {
    mode: QuantificationMode,
    decorations: Decorations,
    expression: Expression,
}

impl EmitLatex for Quantification {
    fn emit(&self) -> String {
        format!(
            "{}{},\\ {}",
            self.mode.emit(),
            self.decorations.emit(),
            self.expression.emit_keep_parens()
        )
    }
}

impl EmitLatex for QuantificationMode {
    fn emit(&self) -> String {
        match *self {
            Self::Existential => "\\exists",
            Self::UniquelyExistential => "\\exists!",
            Self::Universal => "\\forall",
        }
        .into()
    }
}

impl EmitLatex for Decorations {
    fn emit(&self) -> String {
        let mut latex = String::new();
        if let Some(sub_expr) = &self.sub {
            latex.extend(format!("_{}", sub_expr.emit_unwrap_parens()).chars());
        }
        if let Some(sup_expr) = &self.sup {
            latex.extend(format!("^{}", sup_expr.emit_unwrap_parens()).chars());
        }
        // TODO requires storing the atom/operator/whatever being decorated, thus it shouldn't be decorations(...) but a parser combinator, decorated(...).
        // if let Some(under_expr) = self.under {
        //     latex = format!("\\undertext{{{}}}", under_expr.emit_throw_parens())
        // }
        // if let Some(over_expr) = self.over {

        // }
        latex
    }
}

#[derive(Debug, PartialEq)]
struct Relationship {
    lhs: Expression,
    relation: Relation,
    relation_decorations: Decorations,
    negated: bool,
    rhs: Expression,
}

impl EmitLatex for Relationship {
    fn emit(&self) -> String {
        format!(
            "{} {}{}{} {}",
            self.lhs.emit_keep_parens(),
            if self.negated { "\\not" } else { "" },
            self.relation.emit(),
            self.relation_decorations.emit(),
            self.rhs.emit_keep_parens()
        )
    }
}

#[derive(Debug, PartialEq)]
enum Number {
    Float(f32),
    Integer(i32),
}

#[derive(Debug, PartialEq)]
enum Atom {
    Name(String),
    Number(Number),
    Quantity(Quantity),
    BareOperator(BinaryOperator),
    Text(Text),
    Group(Group),
}

impl EmitLatexHasParens for Atom {
    fn emit_keep_parens(&self) -> String {
        match self {
            Atom::Group(Group::Parenthesized(expr)) => {
                format!("\\left({}\\right)", expr.emit_keep_parens())
            }
            _ => todo!(),
        }
        .into()
    }

    fn emit_throw_parens(&self) -> String {
        match self {
            Atom::Group(Group::Parenthesized(expr)) => expr.emit_keep_parens(),
            _ => todo!(),
        }
        .into()
    }

    fn emit_unwrap_parens(&self) -> String {
        match self {
            Atom::Group(Group::Parenthesized(expr)) => format!("{{{}}}", expr.emit_keep_parens()),
            _ => todo!(),
        }
        .into()
    }
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
    MathOperator,
}
#[derive(Debug, PartialEq)]
enum Group {
    // { expr }
    Braced(Expression),
    // ( expr )
    Parenthesized(Expression),
    // [ expr ]
    Bracketed(Expression),
    // ]expr[
    OpenRange(Expression),
    // ]expr]
    LeftOpenRange(Expression),
    // [expr[
    RightOpenRange(Expression),
    // |]expr[|
    OpenIntegerRange(Expression),
    // |]expr|]
    LeftOpenIntegerRange(Expression),
    // [|expr[|
    RightOpenIntegerRange(Expression),
    // [| expr |]
    WhiteBracketed(Expression),
    // | expr |
    AbsoluteValue(Expression),
    // || expr ||
    Norm(Expression),
    // <expr>
    AngleBracketed(Expression),
}

#[derive(Debug, PartialEq)]
struct Quantity {
    value: Number,
    unit: Box<Unit>,
}

impl EmitLatex for Quantity {
    fn emit(&self) -> String {
        format!(r#"\SI{{{}}}{{{}}}"#, self.value, self.unit.emit())
    }
}

impl std::fmt::Display for Number {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Float(x) => x.fmt(f),
            Self::Integer(x) => x.fmt(f),
        }
    }
}

impl EmitLatex for Unit {
    fn emit(&self) -> String {
        match self {
            Self::Fundamental(u) => u.emit(),
            Self::Prefixed(prefix, base) => prefix.emit() + &base.emit(),
            Self::Power(base, pow) => format!("{}^{}", base.emit(), pow.emit_unwrap_parens()),
            Self::Product(a, b) => a.emit() + &b.emit(),
            Self::Ratio(a, b) => format!("{}\\per{}", a.emit(), b.emit()),
        }
    }
}

impl EmitLatex for FundamentalUnit {
    fn emit(&self) -> String {
        match *self {
            Self::Ampere => "\\ampere",
            Self::Atmosphere => "\\atm",
            Self::Bar => "\\bar",
            Self::CelsisusDegree => "\\degreeCelsius",
            Self::Degree => "\\degree",
            Self::ElectronVolt => "\\electronvolt",
            Self::Farad => "\\farad",
            Self::FarenheitDegree => "\\degreeFarenheit",
            Self::Gram => "\\gram",
            _ => todo!(),
        }
        .into()
    }
}

impl EmitLatex for UnitPrefix {
    fn emit(&self) -> String {
        match *self {
            Self::Centi => "\\centi",
            Self::Deca => "\\deca",
            Self::Deci => "\\deci",
            Self::Giga => "\\giga",
            Self::Hecto => "\\hecto",
            _ => todo!(),
        }
        .into()
    }
}

#[derive(Debug, PartialEq)]
enum Unit {
    Product(Box<Unit>, Box<Unit>),
    Ratio(Box<Unit>, Box<Unit>),
    Power(Box<Unit>, Atom),
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
struct Operation {
    lhs: Option<Expression>,
    operator: Operator,
    operator_decorations: Decorations,
    rhs: Option<Expression>,
}

macro_rules! refunwrap {
    ($e:expr) => {
        $e.as_ref().unwrap()
    };
}

impl EmitLatex for Operation {
    fn emit(&self) -> String {
        match &self.operator {
            Operator::Binary(_) => format!(
                "{} {}{} {}",
                refunwrap!(self.lhs).emit_keep_parens(),
                self.operator.emit(),
                self.operator_decorations.emit(),
                refunwrap!(self.rhs).emit_keep_parens()
            ),
            Operator::Big(_) => format!(
                "{}{} {}",
                self.operator.emit(),
                self.operator_decorations.emit(),
                refunwrap!(self.rhs).emit_keep_parens()
            ),
            Operator::Prefix(_) => format!(
                "{}{}{}",
                self.operator.emit(),
                self.operator_decorations.emit(),
                refunwrap!(self.rhs).emit_keep_parens()
            ),
            Operator::Postfix(op) => match op {
                PostfixOperator::VectorMarker => {
                    match refunwrap!(self.lhs).emit_throw_parens().len() {
                        1 => format!(
                            "\\vec{{{}}}{}",
                            refunwrap!(self.lhs).emit_keep_parens(),
                            self.operator_decorations.emit()
                        ),
                        _ => format!(
                            "\\overrightarrow{{{}}}{}",
                            refunwrap!(self.lhs).emit_unwrap_parens(),
                            self.operator_decorations.emit()
                        ),
                    }
                }
                PostfixOperator::Negation => format!(
                    "\\bar{{{}}}{}",
                    match refunwrap!(self.lhs).emit_throw_parens().len() {
                        1 => refunwrap!(self.lhs).emit_keep_parens(),
                        _ => refunwrap!(self.lhs).emit_unwrap_parens(),
                    },
                    self.operator_decorations.emit()
                ),
                _ => format!(
                    "{}{}{}",
                    refunwrap!(self.lhs).emit_keep_parens(),
                    self.operator.emit(),
                    self.operator_decorations.emit()
                ),
            },
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
enum QuantificationMode {
    Universal,
    Existential,
    UniquelyExistential,
}

#[derive(Debug, PartialEq)]
struct Decorations {
    sub: Option<Atom>,
    sup: Option<Atom>,
    under: Option<Atom>,
    over: Option<Atom>,
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

impl EmitLatex for Relation {
    fn emit(&self) -> String {
        match self {
            Self::Congruent => "\\cong",
            Self::ElementOf => "\\in",
            Self::Equals => "=",
            Self::EquivalentTo => "~",
            Self::Greater => ">",
            Self::GreaterOrEqual => "\\geq", // TODO add \geqslanted with a config switch
            Self::Iff => "\\iff",
            Self::ImpliedBy => "\\impliedby",
            Self::Implies => "\\implies",
            Self::Less => "<",
            Self::LessOrEqual => "\\leq",
            Self::MapsTo => "\\mapsto",
            Self::Subset => "\\subset",
            Self::Superset => "\\supset",
            Self::Tends => "\\to",
        }
        .to_string()
    }
}

#[derive(Debug, PartialEq)]
enum Operator {
    Binary(BinaryOperator),
    Prefix(PrefixOperator),
    Postfix(PostfixOperator),
    Big(BigOperator),
}

impl EmitLatex for Operator {
    fn emit(&self) -> String {
        match self {
            Operator::Big(op) => op.emit(),
            Operator::Binary(op) => op.emit(),
            Operator::Postfix(op) => op.emit(),
            Operator::Prefix(op) => op.emit(),
        }
    }
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

impl EmitLatex for BinaryOperator {
    fn emit(&self) -> String {
        match self {
            Self::Addition => "+",
            Self::CartesianProduct => "\\times",
            Self::Composition => "\\circ",
            Self::Difference => "-",
            Self::DirectAddition => "\\oplus",
            Self::Division => "/",
            Self::DotProduct => "\\cdot",
            Self::Fraction => "\\div", // just for Atom::BareOperator
            Self::Intersection => "\\cap",
            Self::Remainder => "\\mod",
            Self::SetDifference => "\\setminus",
            Self::SymmetricDifference => "\\Delta",
            Self::Union => "\\cup",
            Self::VectorProduct => "\\land",
        }
        .to_string()
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
enum PrefixOperator {
    Transposition,
    Complement,
    Negation,
}

impl EmitLatex for PrefixOperator {
    fn emit(&self) -> String {
        match self {
            Self::Transposition => r#"\ {}^t"#,
            Self::Complement => r#"\ {}^c"#,
            Self::Negation => r#"\lnot"#,
        }
        .to_string()
    }
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

impl EmitLatex for PostfixOperator {
    fn emit(&self) -> String {
        match self {
            Self::VectorMarker => "\\to", // the actual vector-marking should be done as a postfix operation on a name atom, such as a-> for example. This is just in case VectorMarker is used as an Atom::BareOperator.
            Self::Complement => "^\\complement",
            Self::Factorial => "!",
            Self::Negation => "\\lnot", // same as vector marker
            Self::Orthogonal => "^\\bot",
            Self::Transposition => "^\\top",
        }
        .to_string()
    }
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

impl EmitLatex for BigOperator {
    fn emit(&self) -> String {
        "\\".to_string()
            + match self {
                Self::Limit => "lim",
                Self::Product => "prod",
                Self::ContourIntegral => "oint",
                Self::Coproduct => "coprod",
                Self::DirectSum => "bigoplus",
                Self::DoubleIntegral => "iint",
                Self::Infimum => "inf",
                Self::Integral => "int",
                Self::Intersection => "bigcap",
                Self::Maximum => "max",
                Self::Minimum => "min",
                Self::QuadrupleIntegral => "iiiint",
                Self::Sum => "sum",
                Self::Supremum => "sup",
                Self::SurfaceIntegral => "oiint",
                Self::TripleIntegral => "iiint",
                Self::Union => "bigcup",
            }
    }
}

fn expression(input: &str) -> IResult<&str, Expression> {
    if let Ok((tail, q)) = quantification(input) {
        Ok((tail, Expression::Q(Box::new(q))))
    } else if let Ok((tail, r)) = relationship(input) {
        Ok((tail, Expression::R(Box::new(r))))
    } else if let Ok((tail, o)) = operation(input) {
        Ok((tail, Expression::O(Box::new(o))))
    } else if let Ok((tail, a)) = atom(input) {
        Ok((tail, Expression::A(Box::new(a))))
    } else {
        Err(Err::Error(Error {
            code: ErrorKind::Alt,
            input: "",
        }))
    }
}

fn quantification(input: &str) -> IResult<&str, Quantification> {
    let (tail, (quantifier, decos, _, expr)) =
        tuple((quantifier, decorations, whitespace, expression))(input)?;
    Ok((
        tail,
        Quantification {
            decorations: decos,
            expression: expr,
            mode: quantifier,
        },
    ))
}

fn quantifier(input: &str) -> IResult<&str, QuantificationMode> {
    if let Ok((tail, _)) = alt::<_, _, Error<_>, _>((tag("∀"), tag("AA"), tag("forall")))(input) {
        Ok((tail, QuantificationMode::Universal))
    } else if let Ok((tail, _)) =
        alt::<_, _, Error<_>, _>((tag("∃"), tag("EE"), tag("exists")))(input)
    {
        if let Ok((tail, _)) = tag::<_, _, Error<_>>("!")(tail) {
            Ok((tail, QuantificationMode::UniquelyExistential))
        } else {
            Ok((tail, QuantificationMode::Existential))
        }
    } else {
        Err(Err::Error(Error {
            code: ErrorKind::Alt,
            input: "",
        }))
    }
}

fn relationship(input: &str) -> IResult<&str, Relationship> {
    let (tail, (lhs, _, neg, rel, deco, _, rhs)) = tuple((
        expression,
        opt(whitespace),
        opt(alt((tag("not"), tag("!")))),
        relation,
        decorations,
        opt(whitespace),
        expression,
    ))(input)?;

    Ok((
        tail,
        Relationship {
            lhs: lhs,
            negated: match neg {
                Some(_) => true,
                None => false,
            },
            relation: rel,
            relation_decorations: deco,
            rhs: rhs,
        },
    ))
}

#[test]
fn test_relationship() {
    assert_eq!(
        relationship("e !in E => f in F"),
        Ok((
            "",
            Relationship {
                lhs: Expression::R(Box::new(Relationship {
                    lhs: Expression::A(Box::new(Atom::Name("e".into()))),
                    rhs: Expression::A(Box::new(Atom::Name("E".into()))),
                    negated: true,
                    relation: Relation::ElementOf,
                    relation_decorations: Decorations {
                        sub: None,
                        sup: None,
                        under: None,
                        over: None
                    },
                })),
                rhs: Expression::R(Box::new(Relationship {
                    lhs: Expression::A(Box::new(Atom::Name("f".into()))),
                    rhs: Expression::A(Box::new(Atom::Name("F".into()))),
                    negated: false,
                    relation: Relation::ElementOf,
                    relation_decorations: Decorations {
                        sub: None,
                        sup: None,
                        under: None,
                        over: None
                    },
                })),
                negated: false,
                relation: Relation::Implies,
                relation_decorations: Decorations {
                    sub: None,
                    sup: None,
                    under: None,
                    over: None
                },
            }
        ))
    )
}

fn number(input: &str) -> IResult<&str, Number> {
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

fn _float_only_when_comma(input: &str) -> IResult<&str, Number> {
    if !input.contains('.') {
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

fn atom(input: &str) -> IResult<&str, Atom, Error<&str>> {
    if let Ok((tail, txt)) = text(input) {
        Ok((tail, Atom::Text(txt)))
    } else if let Ok((tail, name)) = alpha1::<_, Error<_>>(input) {
        Ok((tail, Atom::Name(name.into())))
    } else if let Ok((tail, number)) = number(input) {
        Ok((tail, Atom::Number(number)))
    } else if let Ok((tail, qty)) = quantity(input) {
        Ok((tail, Atom::Quantity(qty)))
    } else if let Ok((tail, bare_op)) = binary_operator(input) {
        Ok((tail, Atom::BareOperator(bare_op)))
    } else if let Ok((tail, grp)) = group(input) {
        Ok((tail, Atom::Group(grp)))
    } else {
        Err(Err::Error(Error {
            input: "",
            code: ErrorKind::Alt,
        }))
    }
}

#[test]
fn test_atom() {
    assert_eq!(atom("alpha"), Ok(("", Atom::Name("alpha".into()))));
    assert_eq!(atom("abc87"), Ok(("87", Atom::Name("abc".into()))));
    assert_eq!(
        atom("65486.848pal"),
        Ok(("pal", Atom::Number(Number::Float(65486.848))))
    );
    assert_eq!(
        atom(r#"(int_0^1 sum e in o"R") not= _|_"#),
        Ok((
            " not= _|_",
            Atom::Group(Group::Parenthesized(Expression::R(Box::new(
                Relationship {
                    lhs: Expression::O(Box::new(Operation {
                        lhs: None,
                        operator: Operator::Big(BigOperator::Integral),
                        operator_decorations: Decorations {
                            sub: Some(Atom::Number(Number::Integer(0))),
                            sup: Some(Atom::Number(Number::Integer(0))),
                            under: None,
                            over: None
                        },
                        rhs: Some(Expression::O(Box::new(Operation {
                            lhs: None,
                            operator: Operator::Big(BigOperator::Sum),
                            operator_decorations: Decorations {
                                sub: None,
                                sup: None,
                                under: None,
                                over: None
                            },
                            rhs: Some(Expression::A(Box::new(Atom::Name("e".into())))),
                        })))
                    })),
                    negated: false,
                    relation: Relation::ElementOf,
                    relation_decorations: None,
                    rhs: Expression::A(Box::new(Atom::Text(Text {
                        font: TextFont::BlackboardBold,
                        content: "R".into(),
                    })))
                }
            ))))
        ))
    );
}

fn text(input: &str) -> IResult<&str, Text> {
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
fn test_text() {
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

fn quoted_string(input: &str) -> IResult<&str, String> {
    delimited(tag("\""), _in_quotes, tag("\""))(input)
}

fn _in_quotes(input: &str) -> IResult<&str, String> {
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

fn text_font(input: &str) -> IResult<&str, TextFont> {
    match_enum_variants_to_literals(
        input,
        hashmap! {
            TextFont::Caligraphic => vec!["c"],
            TextFont::Fraktur => vec!["f"],
            TextFont::BlackboardBold => vec!["o", "bb"],
            TextFont::Bold => vec!["b"],
            TextFont::Sans => vec!["s"],
            TextFont::Monospace => vec!["m", "tt"],
            TextFont::Normal => vec!["n"],
            TextFont::Italic => vec!["i"],
            TextFont::Script => vec!["s"],
            TextFont::MathOperator => vec!["op"],
        },
    )
}

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

fn surrounded<'a, F: 'a, O, E: ParseError<&'a str>>(
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

fn whitespace(input: &str) -> IResult<&str, &str> {
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

fn quantity(input: &str) -> IResult<&str, Quantity> {
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
fn test_quantity() {
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

fn unit(input: &str) -> IResult<&str, Unit> {
    if let Ok((tail, (a, _, b))) = tuple((_unit_no_bin_op, char('*'), unit))(input) {
        Ok((tail, Unit::Product(Box::new(a), Box::new(b))))
    } else if let Ok((tail, (a, _, b))) = tuple((_unit_no_bin_op, char('/'), unit))(input) {
        Ok((tail, Unit::Ratio(Box::new(a), Box::new(b))))
    } else {
        _unit_no_bin_op(input)
    }
}

fn _unit_no_bin_op(input: &str) -> IResult<&str, Unit> {
    if let Ok((tail, (u, _, pow))) = tuple((_unit_no_op, char('^'), atom))(input) {
        Ok((tail, Unit::Power(Box::new(u), pow)))
    } else {
        _unit_no_op(input)
    }
}

fn _unit_no_op(input: &str) -> IResult<&str, Unit> {
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
                        relation: Relation::Tends,
                        negated: false,
                        relation_decorations: None,
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
