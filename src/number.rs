use num::bigint::{BigInt, BigUint, TryFromBigIntError};

use num::rational::BigRational;

use std::cmp::Ordering;
use std::convert::TryFrom;
use std::ops::Add;

#[derive(Clone, Debug)]
pub struct Number {
    pub rep: NumberRep,
    pub precision: Precision,
}

#[derive(Clone, Debug)]
pub enum NumberRep {
    ArbitraryInteger(BigInt),
    ArbitraryRational(BigRational),
    ArbitraryFloat {
        significand: Box<Number>,
        exponent: BigInt,
        base: BigUint,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub enum Precision {
    Rational {
        numerator_range: Range<BigInt>,
        denominator_range: Range<PositiveIntegerOrInvPositiveInteger>,
    },
    Float {
        base: BigUint,
        significand_range: Range<Box<Number>>,
        exponent_range: Range<BigInt>,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub struct Range<T>(LowerBound<T>, UpperBound<T>);

#[derive(Clone, Debug, PartialEq)]
pub enum Bound<T> {
    Inclusive(T),
    Exclusive(T),
    Unbounded,
}

#[derive(Clone, Debug, PartialEq)]
pub struct LowerBound<T>(Bound<T>);

#[derive(Clone, Debug, PartialEq)]
pub struct UpperBound<T>(Bound<T>);

#[derive(Clone, Debug, PartialEq)]
pub enum PositiveIntegerOrInvPositiveInteger {
    Integer(BigUint),
    Inv(BigUint),
}

impl<T: PartialOrd> Range<T> {
    fn subset_of(&self, other: &Range<T>) -> bool {
        self.0 >= other.0 && self.1 <= other.1
    }

    fn superset_of(&self, other: &Range<T>) -> bool {
        self.0 <= other.0 && self.1 >= other.1
    }
}

impl<T: PartialOrd> PartialOrd for LowerBound<T> {
    fn partial_cmp(&self, other: &LowerBound<T>) -> Option<Ordering> {
        use Bound::*;
        match (&self.0, &other.0) {
            (Unbounded, Unbounded) => Some(Ordering::Equal),
            (Inclusive(left), Inclusive(right)) => left.partial_cmp(right),
            (Exclusive(left), Exclusive(right)) => left.partial_cmp(right),
            (Unbounded, Inclusive(_)) | (Unbounded, Exclusive(_)) => {
                Some(Ordering::Less)
            }
            (Inclusive(_), Unbounded) | (Exclusive(_), Unbounded) => {
                Some(Ordering::Greater)
            }
            (Inclusive(left), Exclusive(right)) => {
                if left == right {
                    Some(Ordering::Less)
                } else {
                    left.partial_cmp(right)
                }
            }
            (Exclusive(left), Inclusive(right)) => {
                if left == right {
                    Some(Ordering::Greater)
                } else {
                    left.partial_cmp(right)
                }
            }
        }
    }
}

impl<T: PartialOrd> PartialOrd for UpperBound<T> {
    fn partial_cmp(&self, other: &UpperBound<T>) -> Option<Ordering> {
        use Bound::*;
        match (&self.0, &other.0) {
            (Unbounded, Unbounded) => Some(Ordering::Equal),
            (Inclusive(left), Inclusive(right)) => left.partial_cmp(right),
            (Exclusive(left), Exclusive(right)) => left.partial_cmp(right),
            (Unbounded, Inclusive(_)) | (Unbounded, Exclusive(_)) => {
                Some(Ordering::Greater)
            }
            (Inclusive(_), Unbounded) | (Exclusive(_), Unbounded) => {
                Some(Ordering::Less)
            }
            (Inclusive(left), Exclusive(right)) => {
                if left == right {
                    Some(Ordering::Greater)
                } else {
                    left.partial_cmp(right)
                }
            }
            (Exclusive(left), Inclusive(right)) => {
                if left == right {
                    Some(Ordering::Less)
                } else {
                    left.partial_cmp(right)
                }
            }
        }
    }
}

impl PartialEq for Number {
    fn eq(&self, other: &Number) -> bool {
        self.rep == other.rep
    }
}

impl PartialEq for NumberRep {
    fn eq(&self, other: &NumberRep) -> bool {
        use NumberRep::*;
        match (self, other) {
            (ArbitraryInteger(left), ArbitraryInteger(right)) => left == right,
            (ArbitraryRational(left), ArbitraryRational(right)) => {
                left == right
            }
            (ArbitraryRational(rat), ArbitraryInteger(int))
            | (ArbitraryInteger(int), ArbitraryRational(rat)) => {
                *rat.denom() == BigInt::from(1) && rat.numer() == int
            }
            _ => unimplemented!(),
        }
    }
}

impl Precision {
    pub fn integer(from: BigInt, to: BigInt) -> Precision {
        Precision::Rational {
            numerator_range: Range(
                LowerBound(Bound::Inclusive(from)),
                UpperBound(Bound::Inclusive(to)),
            ),
            denominator_range: Range(
                LowerBound(Bound::Inclusive(
                    PositiveIntegerOrInvPositiveInteger::Integer(
                        BigUint::from(1_usize),
                    ),
                )),
                UpperBound(Bound::Inclusive(
                    PositiveIntegerOrInvPositiveInteger::Integer(
                        BigUint::from(1_usize),
                    ),
                )),
            ),
        }
    }

    fn add(left: &Precision, right: &Precision) -> Precision {
        use Bound::*;
        use Precision::*;
        match (left, right) {
            (
                Rational {
                    denominator_range: left_denominator_range,
                    numerator_range:
                        Range(
                            LowerBound(Inclusive(left_lower)),
                            UpperBound(Inclusive(left_upper)),
                        ),
                },
                Rational {
                    denominator_range: right_denominator_range,
                    numerator_range:
                        Range(
                            LowerBound(Inclusive(right_lower)),
                            UpperBound(Inclusive(right_upper)),
                        ),
                },
            ) if left_denominator_range == right_denominator_range => {
                Rational {
                    denominator_range: left_denominator_range.clone(),
                    numerator_range: Range(
                        LowerBound(Inclusive(BigInt::from(
                            left_lower + right_lower,
                        ))),
                        UpperBound(Inclusive(BigInt::from(
                            left_upper + right_upper,
                        ))),
                    ),
                }
            }
            _ => unimplemented!(),
        }
    }
}

impl From<isize> for Number {
    fn from(int: isize) -> Number {
        Number {
            rep: NumberRep::ArbitraryInteger(BigInt::from(int)),
            precision: Precision::integer(isize::MIN.into(), isize::MAX.into()),
        }
    }
}

#[derive(Debug)]
pub enum FromNumberError {
    PrecisionTooLow,
    OutOfBounds,
    TryFromBigIntError(TryFromBigIntError<BigInt>),
}

impl From<TryFromBigIntError<BigInt>> for FromNumberError {
    fn from(e: TryFromBigIntError<BigInt>) -> FromNumberError {
        Self::TryFromBigIntError(e)
    }
}

impl TryFrom<Number> for usize {
    type Error = FromNumberError;

    fn try_from(num: Number) -> Result<usize, FromNumberError> {
        if num.precision
            < Precision::integer(usize::MIN.into(), usize::MAX.into())
        {
            return Err(FromNumberError::PrecisionTooLow);
        }
        Ok(match num.rep {
            NumberRep::ArbitraryInteger(i) => usize::try_from(i)?,
            _ => unimplemented!(),
        })
    }
}

impl Add<Number> for Number {
    type Output = Number;

    fn add(self, other: Number) -> Number {
        use NumberRep::*;
        match (self.rep, other.rep) {
            (ArbitraryInteger(left), ArbitraryInteger(right)) => Number {
                rep: ArbitraryInteger(left + right),
                precision: Precision::add(&self.precision, &other.precision),
            },
            _ => unimplemented!(),
        }
    }
}

impl PartialOrd for Precision {
    fn partial_cmp(&self, other: &Precision) -> Option<Ordering> {
        use Precision::*;
        match (self, other) {
            (
                Rational {
                    denominator_range: left_denominator_range,
                    numerator_range: left_numerator_range,
                },
                Rational {
                    denominator_range: right_denominator_range,
                    numerator_range: right_numerator_range,
                },
            ) => {
                if left_denominator_range == right_denominator_range {
                    if left_numerator_range == right_numerator_range {
                        Some(Ordering::Equal)
                    } else if left_numerator_range
                        .subset_of(right_numerator_range)
                    {
                        Some(Ordering::Less)
                    } else if left_numerator_range
                        .superset_of(right_numerator_range)
                    {
                        Some(Ordering::Greater)
                    } else {
                        None
                    }
                } else {
                    unimplemented!()
                }
            }
            (Float { .. }, Float { .. }) => unimplemented!(),
            _ => None,
        }
    }
}
