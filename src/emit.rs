use crate::data::*;
use crate::emit::ParensHandling::*;
#[cfg(test)]
use pretty_assertions::assert_eq;
// TODO custom derive macro to implement enum for PascalCase names as \camelCase (e.g. units)
pub trait EmitLatex {
    fn emit(&self) -> String;
}

pub enum ParensHandling {
    Keep,
    Unwrap,
    Throw,
}

pub trait EmitLatexHasParens {
    fn emit(&self, parens: ParensHandling) -> String;
}

pub trait EmitLatexWithParens {
    fn emit_keep_parens(&self) -> String;
    fn emit_unwrap_parens(&self) -> String;
    fn emit_throw_parens(&self) -> String;
}

impl<T: EmitLatexHasParens> EmitLatexWithParens for T {
    fn emit_throw_parens(&self) -> String {
        self.emit(Throw)
    }
    fn emit_unwrap_parens(&self) -> String {
        self.emit(Unwrap)
    }
    fn emit_keep_parens(&self) -> String {
        self.emit(Keep)
    }
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

impl EmitLatexHasParens for Expression {
    fn emit(&self, parens: ParensHandling) -> String {
        match self {
            Expression::A(inner) => inner.emit(parens),
            Expression::O(inner) => inner.emit(),
            Expression::Q(inner) => inner.emit(),
            Expression::R(inner) => inner.emit(),
            Expression::FR(inner) => inner.emit(),
            Expression::DA(inner) => inner.emit(parens),
            Expression::F(inner) => inner.emit(),
        }
    }
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
    fn emit(&self, parens: ParensHandling) -> String {
        let mut latex = self.decoratee.emit(parens);
        if let Some(sub_expr) = &self.decorations.sub {
            latex.extend(format!("_{{{}}}", sub_expr.emit_unwrap_parens()).chars());
        }
        if let Some(sup_expr) = &self.decorations.sup {
            latex.extend(format!("^{{{}}}", sup_expr.emit_unwrap_parens()).chars());
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
            latex.extend(format!("_{{{}}}", sub_expr.emit_unwrap_parens()).chars());
        }
        if let Some(sup_expr) = &self.decorations.sup {
            latex.extend(format!("^{{{}}}", sup_expr.emit_unwrap_parens()).chars());
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

enum Parens {
    Keep,
    Throw,
    Unwrap,
}

impl EmitLatexHasParens for Atom {
    fn emit(&self, parens: ParensHandling) -> String {
        match self {
            Atom::Group(group) => group.emit(parens),
            Atom::Name(name) => name.to_string(),
            Atom::Number(number) => format!("{}", number),
            Atom::Quantity(qty) => format!("\\SI{{{}}}{{{}}}", qty.value, qty.unit.emit()),
            Atom::BareOperator(op) => op.emit(),
            Atom::Symbol(symb) => symb.emit(),
            Atom::Text(txt) => txt.emit(),
        }
        .into()
    }
}

impl EmitLatexHasParens for Group {
    fn emit(&self, parens: ParensHandling) -> String {
        match &self {
            Group::Parenthesized(expr) => match parens {
                Keep => format!("\\left({}\\right)", expr.emit_keep_parens()),
                _ => format!("{}", expr.emit_keep_parens()),
            },
            Group::Braced(expr) => format!("\\left\\{{{}\\right\\}}", expr.emit_keep_parens()),
            Group::Bracketed(expr) => format!("\\left[{}\\right]", expr.emit_keep_parens()),
            Group::OpenRange(expr) => format!("\\left]{}\\right[", expr.emit_keep_parens()),
            Group::LeftOpenRange(expr) => format!("\\left]{}\\right]", expr.emit_keep_parens()),
            Group::RightOpenRange(expr) => {
                format!("\\left[{}\\right[", expr.emit_keep_parens())
            }
            Group::WhiteBracketed(expr) => format!(
                "\\left\\llbracket{}\\right\\rrbracket",
                expr.emit_keep_parens()
            ),
            Group::OpenIntegerRange(expr) => format!(
                "\\left\\rrbracket{}\\right\\llbracket",
                expr.emit_keep_parens()
            ),
            Group::LeftOpenIntegerRange(expr) => format!(
                "\\left\\rrbracket{}\\right\\rrbracket",
                expr.emit_keep_parens()
            ),
            Group::RightOpenIntegerRange(expr) => {
                format!(
                    "\\left\\llbracket{}\\right\\llbracket",
                    expr.emit_keep_parens()
                )
            }
            Group::AbsoluteValue(expr) => format!("\\left|{}\\right|", expr.emit_keep_parens()),
            Group::Norm(expr) => format!("\\left\\|{}\\right\\|", expr.emit_keep_parens()),
            Group::AngleBracketed(expr) => {
                format!("\\left\\langle{}\\right\\rangle", expr.emit_keep_parens())
            }
        }
    }
}

#[test]
fn test_emit_group() {
    assert_eq!(
        Group::AngleBracketed(Expression::A(Box::new(Atom::Symbol(Symbol::Infinity))))
            .emit(ParensHandling::Keep),
        r#"\left\langle\infty\right\rangle"#.to_string()
    );
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
            Self::Comma => ",\\ ",
        }
        .to_string()
    }
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

impl EmitLatex for Text {
    fn emit(&self) -> String {
        (match self.font {
            TextFont::Caligraphic => "\\mathcal",
            TextFont::Fraktur => "\\mathfrak",
            TextFont::BlackboardBold => "\\mathbb",
            TextFont::Bold => "\\mathbf",
            TextFont::Sans => "\\mathrm",
            TextFont::Monospace => "\\mathtt",
            TextFont::Normal => "",
            TextFont::Italic => "\\mathit",
            TextFont::Script => "\\mathsc",
            TextFont::MathOperator => "\\operatorname",
        })
        .to_owned()
            + "{"
            + &self.content
            + "}"
    }
}

impl EmitLatex for Symbol {
    fn emit(&self) -> String {
        match self {
            Symbol::Infinity => "\\infty",
            Symbol::Aleph => "\\aleph",
            Symbol::Beth => "\\beth",
            Symbol::Gimmel => "\\gimmel",
            Symbol::ProofEnd => "\\qed", // TODO user-configurable
            Symbol::Contradiction => "\\lightning", // TODO user-configurable
        }
        .into()
    }
}
