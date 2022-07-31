use nom::number::complete::float;
use unicode_segmentation::UnicodeSegmentation;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{char, digit1};
use nom::character::is_alphabetic;
use nom::combinator::opt;
use nom::error::{Error, ErrorKind, ParseError};
use nom::sequence::{delimited, preceded, terminated, tuple};
use nom::{Err, IResult};
#[cfg(test)]
use pretty_assertions::assert_eq;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
struct InputState {
    disable_operation: bool,
    disable_relationship: bool,
    call_depth: i32,
}

// TODO custom derive macro to implement enum for PascalCase names as \camelCase (e.g. units)
trait EmitLatex {
    fn emit(&self) -> String;
}

trait EmitLatexHasParens {
    /// should render (...) as ...
    fn emit_throw_parens(&self) -> String;
    /// should render (...) as {...}
    fn emit_unwrap_parens(&self) -> String;
    /// should render (...) as \left(...\right)
    fn emit_keep_parens(&self) -> String;
}

#[derive(Debug, PartialEq)]
enum Expression {
    Q(Box<Quantification>),
    R(Box<Relationship>),
    FR(Box<FoldedRelationship>),
    A(Box<Atom>),
    DA(Box<Decorated<Atom>>),
    F(Box<FunctionCall>),
    O(Box<Operation>),
}

#[derive(Debug, PartialEq)]
struct FunctionCall {
    caller: Decorated<Atom>,
    arguments: Expression,
}

impl EmitLatex for FunctionCall {
    fn emit(&self) -> String {
        format!(
            "{}({})",
            self.caller.emit_keep_parens(),
            self.arguments.emit_keep_parens()
        )
    }
}

#[derive(Debug, PartialEq)]
struct Decorated<T> {
    decoratee: T,
    decorations: Decorations,
}

impl<T> Decorated<T> {
    fn none(t: T) -> Self {
        Self {
            decoratee: t,
            decorations: Decorations::none(),
        }
    }

    fn new(t: T, decorations: Decorations) -> Self {
        Self {
            decoratee: t,
            decorations,
        }
    }

    fn sub(t: T, sub: Atom) -> Self {
        Self {
            decoratee: t,
            decorations: Decorations {
                sub: Some(sub),
                sup: None,
                under: None,
                over: None,
            },
        }
    }
}

impl EmitLatexHasParens for Expression {
    fn emit_keep_parens(&self) -> String {
        match self {
            Expression::A(inner) => inner.emit_keep_parens(),
            Expression::O(inner) => inner.emit(),
            Expression::Q(inner) => inner.emit(),
            Expression::R(inner) => inner.emit(),
            Expression::FR(inner) => inner.emit(),
            Expression::DA(inner) => inner.emit_keep_parens(),
            Expression::F(inner) => inner.emit(),
        }
    }
    fn emit_throw_parens(&self) -> String {
        match self {
            Expression::A(inner) => inner.emit_throw_parens(),
            Expression::O(inner) => inner.emit(),
            Expression::Q(inner) => inner.emit(),
            Expression::FR(inner) => inner.emit(),
            Expression::R(inner) => inner.emit(),
            Expression::DA(inner) => inner.emit_throw_parens(),
            Expression::F(inner) => inner.emit(),
        }
    }
    fn emit_unwrap_parens(&self) -> String {
        match self {
            Expression::A(inner) => inner.emit_unwrap_parens(),
            Expression::O(inner) => inner.emit(),
            Expression::Q(inner) => inner.emit(),
            Expression::FR(inner) => inner.emit(),
            Expression::R(inner) => inner.emit(),
            Expression::DA(inner) => inner.emit_unwrap_parens(),
            Expression::F(inner) => inner.emit(),
        }
    }
}

#[derive(Debug, PartialEq)]
struct Quantification {
    quantifier: Decorated<Quantifier>,
    expression: Expression,
}

impl EmitLatex for Quantification {
    fn emit(&self) -> String {
        format!(
            "{},\\ {}",
            self.quantifier.emit(),
            self.expression.emit_keep_parens()
        )
    }
}

impl EmitLatex for Quantifier {
    fn emit(&self) -> String {
        match *self {
            Self::Existential => "\\exists",
            Self::UniquelyExistential => "\\exists!",
            Self::Universal => "\\forall",
        }
        .into()
    }
}

impl<T: EmitLatexHasParens> EmitLatexHasParens for Decorated<T> {
    fn emit_keep_parens(&self) -> String {
        let mut latex = self.decoratee.emit_keep_parens();
        if let Some(sub_expr) = &self.decorations.sub {
            latex.extend(format!("_{}", sub_expr.emit_unwrap_parens()).chars());
        }
        if let Some(sup_expr) = &self.decorations.sup {
            latex.extend(format!("^{}", sup_expr.emit_unwrap_parens()).chars());
        }
        if let Some(under_expr) = &self.decorations.under {
            latex = format!(
                "\\underbrace{{{}}}_{{{}}}",
                latex,
                under_expr.emit_throw_parens()
            )
        }
        if let Some(over_expr) = &self.decorations.over {
            latex = format!(
                "\\overbrace{{{}}}^{{{}}}",
                latex,
                over_expr.emit_throw_parens()
            )
        }
        latex
    }

    fn emit_throw_parens(&self) -> String {
        let mut latex = self.decoratee.emit_throw_parens();
        if let Some(sub_expr) = &self.decorations.sub {
            latex.extend(format!("_{}", sub_expr.emit_unwrap_parens()).chars());
        }
        if let Some(sup_expr) = &self.decorations.sup {
            latex.extend(format!("^{}", sup_expr.emit_unwrap_parens()).chars());
        }
        if let Some(under_expr) = &self.decorations.under {
            latex = format!(
                "\\underbrace{{{}}}_{{{}}}",
                latex,
                under_expr.emit_throw_parens()
            )
        }
        if let Some(over_expr) = &self.decorations.over {
            latex = format!(
                "\\overbrace{{{}}}^{{{}}}",
                latex,
                over_expr.emit_throw_parens()
            )
        }
        latex
    }

    fn emit_unwrap_parens(&self) -> String {
        let mut latex = self.decoratee.emit_unwrap_parens();
        if let Some(sub_expr) = &self.decorations.sub {
            latex.extend(format!("_{}", sub_expr.emit_unwrap_parens()).chars());
        }
        if let Some(sup_expr) = &self.decorations.sup {
            latex.extend(format!("^{}", sup_expr.emit_unwrap_parens()).chars());
        }
        if let Some(under_expr) = &self.decorations.under {
            latex = format!(
                "\\underbrace{{{}}}_{{{}}}",
                latex,
                under_expr.emit_throw_parens()
            )
        }
        if let Some(over_expr) = &self.decorations.over {
            latex = format!(
                "\\overbrace{{{}}}^{{{}}}",
                latex,
                over_expr.emit_throw_parens()
            )
        }
        latex
    }
}

impl<T: EmitLatex> EmitLatex for Decorated<T> {
    fn emit(&self) -> String {
        let mut latex = self.decoratee.emit();
        if let Some(sub_expr) = &self.decorations.sub {
            latex.extend(format!("_{}", sub_expr.emit_unwrap_parens()).chars());
        }
        if let Some(sup_expr) = &self.decorations.sup {
            latex.extend(format!("^{}", sup_expr.emit_unwrap_parens()).chars());
        }
        if let Some(under_expr) = &self.decorations.under {
            latex = format!(
                "\\underbrace{{{}}}_{{{}}}",
                latex,
                under_expr.emit_throw_parens()
            )
        }
        if let Some(over_expr) = &self.decorations.over {
            latex = format!(
                "\\overbrace{{{}}}^{{{}}}",
                latex,
                over_expr.emit_throw_parens()
            )
        }
        latex
    }
}

#[derive(Debug, PartialEq)]
struct Relationship {
    lhs: Expression,
    relation: Decorated<Relation>,
    negated: bool,
    rhs: Expression,
}

impl EmitLatex for Relationship {
    fn emit(&self) -> String {
        format!(
            "{} {}{} {}",
            self.lhs.emit_keep_parens(),
            if self.negated { "\\not" } else { "" },
            self.relation.emit(),
            self.rhs.emit_keep_parens()
        )
    }
}

#[derive(Debug, PartialEq)]
struct FoldedRelationship {
    relation: Decorated<Relation>,
    negated: bool,
    rhs: Expression,
}

impl EmitLatex for FoldedRelationship {
    fn emit(&self) -> String {
        format!(
            "{}{} {}",
            if self.negated { "\\not" } else { "" },
            self.relation.emit(),
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
    Symbol(Symbol),
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

impl EmitLatex for Group {
    fn emit(&self) -> String {
        "\\left".to_string()
            + &match self {
                Group::AbsoluteValue(s) => format!("|{}|", s.emit_keep_parens()),
                Group::AngleBracketed(s) => format!(r#"langle{}\rangle"#, s.emit_keep_parens()),
                Group::Braced(s) => format!(r#"\{{{}\}}"#, s.emit_keep_parens()),
                Group::Bracketed(s) => format!("[{}]", s.emit_keep_parens()),
                Group::LeftOpenIntegerRange(s) => {
                    format!("\\rrbracket{}\\rrbracket", s.emit_keep_parens())
                }
                Group::LeftOpenRange(s) => format!("]{}]", s.emit_keep_parens()),
                Group::Norm(s) => format!("\\|{}\\|", s.emit_keep_parens()),
                Group::OpenIntegerRange(s) => {
                    format!("\\rrbracket{}\\llbracket", s.emit_keep_parens())
                }
                Group::OpenRange(s) => format!("]{}[", s.emit_keep_parens()),
                Group::Parenthesized(s) => format!("({})", s.emit_keep_parens()),
                Group::RightOpenIntegerRange(s) => {
                    format!("\\llbracket{}\\llbracket", s.emit_keep_parens())
                }
                Group::RightOpenRange(s) => format!("[{}[", s.emit_keep_parens()),
                Group::WhiteBracketed(s) => {
                    format!("\\llbracket{}\\rrbracket", s.emit_keep_parens())
                }
            }
            + "\\right"
    }
}

#[test]
fn test_emit_group() {
    assert_eq!(
        Group::AngleBracketed(Expression::A(Box::new(Atom::Symbol(Symbol::Infinity)))).emit(),
        r#"\left\langle\infty\rangle\right"#.to_string()
    );
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
    operator: Decorated<Operator>,
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
            Decorated {
                decoratee: Operator::Binary(_),
                ..
            } => format!(
                "{} {} {}",
                refunwrap!(self.lhs).emit_keep_parens(),
                self.operator.emit(),
                refunwrap!(self.rhs).emit_keep_parens()
            ),
            Decorated {
                decoratee: Operator::Big(_),
                ..
            } => format!(
                "{} {}",
                self.operator.emit(),
                refunwrap!(self.rhs).emit_keep_parens()
            ),
            Decorated {
                decoratee: Operator::Prefix(_),
                ..
            } => format!(
                "{}{}",
                self.operator.emit(),
                refunwrap!(self.rhs).emit_keep_parens()
            ),
            Decorated {
                decoratee: Operator::Postfix(op),
                decorations,
            } => match op {
                PostfixOperator::VectorMarker => {
                    match refunwrap!(self.lhs).emit_throw_parens().len() {
                        1 => format!(
                            "\\vec{{{}}}{}",
                            refunwrap!(self.lhs).emit_keep_parens(),
                            decorations.emit(),
                        ),
                        _ => format!(
                            "\\overrightarrow{{{}}}{}",
                            refunwrap!(self.lhs).emit_unwrap_parens(),
                            decorations.emit(),
                        ),
                    }
                }
                PostfixOperator::Negation => format!(
                    "\\bar{{{}}}{}",
                    match refunwrap!(self.lhs).emit_throw_parens().len() {
                        1 => refunwrap!(self.lhs).emit_keep_parens(),
                        _ => refunwrap!(self.lhs).emit_unwrap_parens(),
                    },
                    decorations.emit()
                ),
                _ => format!(
                    "{}{}{}",
                    refunwrap!(self.lhs).emit_keep_parens(),
                    self.operator.emit(),
                    decorations.emit()
                ),
            },
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
enum Quantifier {
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

impl Decorations {
    fn none() -> Self {
        Decorations {
            sub: None,
            sup: None,
            under: None,
            over: None,
        }
    }
}

impl EmitLatex for Decorations {
    fn emit(&self) -> String {
        if let (None, None) = (&self.over, &self.under) {
            let mut out = match &self.sub {
                Some(s) => format!("_{}", s.emit_unwrap_parens()),
                None => "".to_string(),
            };
            out.extend(
                (match &self.sup {
                    Some(s) => format!("^{}", s.emit_unwrap_parens()),
                    None => "".to_string(),
                })
                .chars(),
            );
            out
        } else {
            panic!("should be handled in Decorated<T>!")
        }
    }
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

#[derive(Debug, PartialEq, Eq, Hash)]
enum Symbol {
    Infinity,
    Aleph,
    Beth,
    Gimmel,
    ProofEnd,
    Contradiction,
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
    Opposition,
}

impl EmitLatex for PrefixOperator {
    fn emit(&self) -> String {
        match self {
            Self::Transposition => r#"\ {}^t"#,
            Self::Complement => r#"\ {}^c"#,
            Self::Negation => r#"\lnot"#,
            Self::Opposition => "-",
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

fn expression(state: InputState, input: &str) -> IResult<&str, Expression> {
    eprintln!(
        "trying expression on {:?}, call depth is {:?}",
        input, state.call_depth
    );
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

fn quantification(state: InputState, input: &str) -> IResult<&str, Quantification> {
    eprintln!("trying quantification on {:?}", input);
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

fn quantifier(input: &str) -> IResult<&str, Quantifier> {
    eprintln!("trying quantifier on {:?}", input);
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

fn folded_relationship(input: &str) -> IResult<&str, FoldedRelationship> {
    eprintln!("trying folded_relationship on {:?}", input);
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

fn relationship(state: InputState, input: &str) -> IResult<&str, Relationship> {
    if state.disable_relationship {
        return Err(Err::Error(Error {
            input: input,
            code: ErrorKind::IsNot,
        }));
    }
    eprintln!("trying relationship on {:?}", input);
    let expression_no_relationship = |i| expression(InputState {
        disable_operation: state.disable_operation,
        disable_relationship: true,
        call_depth: state.call_depth,
    }, i);
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
    println!("     trying regular");
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
fn test_relationship() {
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
                                    lhs: Expression::DA(Box::new(Decorated::none(Atom::Name("n".into())))),
                                    relation: Decorated::none(Relation::Tends),
                                    negated: false,
                                    rhs: Expression::DA(Box::new(Decorated::none(Atom::Symbol(Symbol::Infinity)))),
                                }
                            ))))
                        ),
                        negated: false,
                        rhs: Expression::F(Box::new(FunctionCall {
                            caller: Decorated::none(Atom::Name("f".into())),
                            arguments: Expression::DA(Box::new(Decorated::none(Atom::Name("a".into()))))
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
                lhs: Expression::R(Box::new(Relationship {
                    lhs: Expression::A(Box::new(Atom::Name("e".into()))),
                    rhs: Expression::A(Box::new(Atom::Name("E".into()))),
                    negated: true,
                    relation: Decorated::none(Relation::ElementOf),
                })),
                rhs: Expression::R(Box::new(Relationship {
                    lhs: Expression::A(Box::new(Atom::Name("f".into()))),
                    rhs: Expression::A(Box::new(Atom::Name("F".into()))),
                    negated: false,
                    relation: Decorated::none(Relation::ElementOf),
                })),
                negated: false,
                relation: Decorated::none(Relation::Implies),
            }
        ))
    )
}

fn number(input: &str) -> IResult<&str, Number> {
    eprintln!("trying number on {:?}", input);
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
        eprintln!("trying _float_only_when_comma on {:?}", input);
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

fn atom(state: InputState, input: &str) -> IResult<&str, Atom, Error<&str>> {
    eprintln!("trying atom on {:?}", input);
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

fn name(input: &str) -> IResult<&str, String> {
    eprintln!("trying name on {:?}", input);
    let mut matched = String::from("");
    for grapheme in input.graphemes(true) {
        eprintln!("               current grapheme is {:?}", grapheme);
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
fn test_atom() {
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

fn text(input: &str) -> IResult<&str, Text> {
    eprintln!("trying text on {:?}", input);
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
    eprintln!("trying quoted_string on {:?}", input);
    delimited(tag("\""), _in_quotes, tag("\""))(input)
}

fn _in_quotes(input: &str) -> IResult<&str, String> {
    eprintln!("trying _in_quotes on {:?}", input);
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
    eprintln!("trying text_font on {:?}", input);
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

fn group(state: InputState, input: &str) -> IResult<&str, Group> {
    eprintln!("trying group on {:?}", input);
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

fn decorated<'a, F: 'a, O>(f: F) -> impl Fn(&'a str) -> IResult<&'a str, Decorated<O>>
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

fn whitespace(input: &str) -> IResult<&str, &str> {
    eprintln!("trying whitespace on {:?}", input);
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
    eprintln!("trying quantity on {:?}", input);
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
    eprintln!("trying unit on {:?}", input);
    if let Ok((tail, (a, _, b))) = tuple((_unit_no_bin_op, char('*'), unit))(input) {
        Ok((tail, Unit::Product(Box::new(a), Box::new(b))))
    } else if let Ok((tail, (a, _, b))) = tuple((_unit_no_bin_op, char('/'), unit))(input) {
        Ok((tail, Unit::Ratio(Box::new(a), Box::new(b))))
    } else {
        _unit_no_bin_op(input)
    }
}

fn _unit_no_bin_op(input: &str) -> IResult<&str, Unit> {
    eprintln!("trying _unit_no_bin_op on {:?}", input);
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

fn _unit_no_op(input: &str) -> IResult<&str, Unit> {
    eprintln!("trying _unit_no_op on {:?}", input);
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
    eprintln!("trying unit_prefix on {:?}", input);
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

fn fundamental_unit(input: &str) -> IResult<&str, FundamentalUnit> {
    eprintln!("trying fundamental_unit on {:?}", input);
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

fn operation(state: InputState, input: &str) -> IResult<&str, Operation> {
    if state.disable_operation {
        return Err(Err::Error(Error {
            input: input,
            code: ErrorKind::IsNot,
        }));
    }
    let expression_no_operation = move |i| expression(InputState {
        disable_operation: true,
        disable_relationship: state.disable_relationship,
        call_depth: state.call_depth,
    }, i);
    let expression_operation = move |i| expression(state, i);
    eprintln!("trying operation on {:?}", input);
    if let Ok((tail, (lhs, operator, decorations, rhs))) = tuple((
        terminated(expression_no_operation, whitespace),
        binary_operator,
        decorations,
        preceded(whitespace, expression_operation),
    ))(input)
    {
        Ok((
            tail,
            Operation {
                lhs: Some(lhs),
                operator: Decorated::new(Operator::Binary(operator), decorations),
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

fn decorations(input: &str) -> IResult<&str, Decorations> {
    eprintln!("trying decorations on {:?}", input);
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
fn test_decorations() {
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

fn relation(input: &str) -> IResult<&str, Relation> {
    eprintln!("trying relation on {:?}", input);
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

fn operator(input: &str) -> IResult<&str, Operator> {
    eprintln!("trying operator on {:?}", input);
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
    eprintln!("trying binary_operator on {:?}", input);
    match_enum_variants_to_literals(input, vec![(BinaryOperator::Addition, vec!["+"])])
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
    eprintln!("trying prefix_operator on {:?}", input);
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
    eprintln!("trying postfix_operator on {:?}", input);
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
    eprintln!("trying big_operator on {:?}", input);
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

fn symbol(input: &str) -> IResult<&str, Symbol> {
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

fn function_call(state: InputState, input: &str) -> IResult<&str, FunctionCall> {
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

fn match_enum_variants_to_literals<'a, T>(
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

fn try_prefixes<'a>(input: &'a str, prefixes: Vec<&'a str>) -> Option<&'a str> {
    for prefix in prefixes {
        if let Some(tail) = input.strip_prefix(prefix) {
            return Some(tail);
        }
    }
    None
}

fn main() {}
