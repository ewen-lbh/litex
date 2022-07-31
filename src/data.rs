#[derive(Debug, PartialEq)]
pub enum Expression {
    Q(Box<Quantification>),
    R(Box<Relationship>),
    FR(Box<FoldedRelationship>),
    A(Box<Atom>),
    DA(Box<Decorated<Atom>>),
    F(Box<FunctionCall>),
    O(Box<Operation>),
}

#[derive(Debug, PartialEq)]
pub struct FunctionCall {
    pub caller: Decorated<Atom>,
    pub arguments: Expression,
}

#[derive(Debug, PartialEq)]
pub struct Decorated<T> {
    pub decoratee: T,
    pub decorations: Decorations,
}

impl<T> Decorated<T> {
    pub fn none(t: T) -> Self {
        Self {
            decoratee: t,
            decorations: Decorations::none(),
        }
    }

    pub fn new(t: T, decorations: Decorations) -> Self {
        Self {
            decoratee: t,
            decorations,
        }
    }

    pub fn sub(t: T, sub: Atom) -> Self {
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
#[derive(Debug, PartialEq)]
pub struct Quantification {
    pub quantifier: Decorated<Quantifier>,
    pub expression: Expression,
}
#[derive(Debug, PartialEq)]
pub struct Relationship {
    pub lhs: Expression,
    pub relation: Decorated<Relation>,
    pub negated: bool,
    pub rhs: Expression,
}
#[derive(Debug, PartialEq)]
pub struct FoldedRelationship {
    pub relation: Decorated<Relation>,
    pub negated: bool,
    pub rhs: Expression,
}
#[derive(Debug, PartialEq)]
pub enum Number {
    Float(f32),
    Integer(i32),
}

#[derive(Debug, PartialEq)]
pub enum Atom {
    Name(String),
    Number(Number),
    Quantity(Quantity),
    BareOperator(BinaryOperator),
    Symbol(Symbol),
    Text(Text),
    Group(Group),
}
#[derive(Debug, PartialEq)]
pub struct Text {
    pub font: TextFont,
    pub content: String,
}
#[derive(Debug, PartialEq, Eq, Hash)]
pub enum TextFont {
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
pub enum Group {
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
pub struct Quantity {
    pub value: Number,
    pub unit: Box<Unit>,
}

#[derive(Debug, PartialEq)]
pub enum Unit {
    Product(Box<Unit>, Box<Unit>),
    Ratio(Box<Unit>, Box<Unit>),
    Power(Box<Unit>, Atom),
    Prefixed(UnitPrefix, FundamentalUnit),
    Fundamental(FundamentalUnit),
}
#[derive(Debug, PartialEq, Eq, Hash)]
pub enum UnitPrefix {
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
pub enum FundamentalUnit {
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
pub struct Operation {
    pub lhs: Option<Expression>,
    pub operator: Decorated<Operator>,
    pub rhs: Option<Expression>,
}
#[derive(Debug, PartialEq, Eq, Hash)]
pub enum Quantifier {
    Universal,
    Existential,
    UniquelyExistential,
}

#[derive(Debug, PartialEq)]
pub struct Decorations {
    pub sub: Option<Atom>,
    pub sup: Option<Atom>,
    pub under: Option<Atom>,
    pub over: Option<Atom>,
}

impl Decorations {
    pub fn none() -> Self {
        Decorations {
            sub: None,
            sup: None,
            under: None,
            over: None,
        }
    }
}
#[derive(Debug, PartialEq, Eq, Hash)]
pub enum Relation {
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

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum Symbol {
    Infinity,
    Aleph,
    Beth,
    Gimmel,
    ProofEnd,
    Contradiction,
}

#[derive(Debug, PartialEq)]
pub enum Operator {
    Binary(BinaryOperator),
    Prefix(PrefixOperator),
    Postfix(PostfixOperator),
    Big(BigOperator),
}
#[derive(Debug, PartialEq, Eq, Hash)]
pub enum BinaryOperator {
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
pub enum PrefixOperator {
    Transposition,
    Complement,
    Negation,
    Opposition,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum PostfixOperator {
    VectorMarker,
    Orthogonal,
    Complement,
    Transposition,
    Negation,
    Factorial,
}
#[derive(Debug, PartialEq, Eq, Hash)]
pub enum BigOperator {
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
