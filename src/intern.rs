use std::cmp::Ordering;
use std::collections::HashSet;
use std::fmt;
use std::fmt::Display;
use std::hash::Hash;
use std::mem;
use std::ops::Deref;
use std::sync::Mutex;

#[derive(Debug, Hash)]
pub struct Interned<'interner, Element: Eq + Hash>(&'interner Element);

pub struct Interner<Element: Eq + Hash> {
    backing: Mutex<HashSet<Box<Element>>>,
}

impl<'interner, Element: Eq + Hash> Interner<Element> {
    pub fn intern(
        &'interner self,
        element: Element,
    ) -> Interned<'interner, Element> {
        let mut set = self.backing.lock().unwrap();
        let element_ref = set.get_or_insert(Box::new(element));
        // Safety: We never remove elements from the set, and the elements are boxed, so their address is stable.
        Interned(*unsafe {
            mem::transmute::<&Box<Element>, &&'interner Element>(element_ref)
        })
    }

    pub fn new() -> Interner<Element> {
        Interner {
            backing: Mutex::new(HashSet::new()),
        }
    }
}

impl<'interner, Element: Eq + Hash> Deref for Interned<'interner, Element> {
    type Target = Element;

    fn deref(&self) -> &Element {
        &*self.0
    }
}

impl<'interner, Element: Eq + Hash> PartialEq for Interned<'interner, Element> {
    fn eq(&self, other: &Interned<'interner, Element>) -> bool {
        self.0 as *const _ == other.0 as *const _
    }
}

impl<Element: Eq + Hash> Eq for Interned<'_, Element> {}

impl<'interner, Element: Eq + Hash> Clone for Interned<'interner, Element> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<'interner, Element: Eq + Hash> Copy for Interned<'interner, Element> {}

impl<'interner, Element: Eq + Hash> PartialOrd
    for Interned<'interner, Element>
{
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        (self.0 as *const _ as usize)
            .partial_cmp(&(other.0 as *const _ as usize))
    }
}

impl<'interner, Element: Eq + Hash> Ord for Interned<'interner, Element> {
    fn cmp(&self, other: &Self) -> Ordering {
        (self.0 as *const _ as usize).cmp(&(other.0 as *const _ as usize))
    }
}

impl<'a, T: Eq + Hash + Display> Display for Interned<'a, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}
