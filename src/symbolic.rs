use std::{
    collections::BTreeMap,
    ops::{Deref, DerefMut, RangeInclusive},
};

use crate::syntax::Type;

#[derive(Debug, Copy, Clone)]
pub enum Dist {
    /// Continuous uniform distribution over [0,1].
    Uniform,
    /// Standard normal distribution.
    Normal,
}

impl Dist {
    pub fn range(&self) -> RangeInclusive<i32> {
        match self {
            Dist::Uniform => 0..=1,
            Dist::Normal => -20..=20,
        }
    }

    pub fn is_uniform(&self) -> bool {
        matches!(self, Dist::Uniform)
    }

    pub fn is_normal(&self) -> bool {
        matches!(self, Dist::Normal)
    }
}

#[derive(Debug, Copy, Clone)]
pub enum SymType {
    Universal(Type),
    Prob(Dist),
}

impl SymType {
    pub fn is_prob(&self) -> bool {
        !matches!(self, SymType::Universal(_))
    }
}

type SymVarMapT = BTreeMap<String, SymType>;
#[derive(Debug, Clone, Default)]
pub struct SymVarMap(SymVarMapT);

impl Deref for SymVarMap {
    type Target = SymVarMapT;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for SymVarMap {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl FromIterator<(String, SymType)> for SymVarMap {
    fn from_iter<T: IntoIterator<Item = (String, SymType)>>(iter: T) -> Self {
        let inner: BTreeMap<_, _> = iter.into_iter().collect();
        Self(inner)
    }
}

impl IntoIterator for SymVarMap {
    type Item = (String, SymType);

    type IntoIter = <SymVarMapT as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}
