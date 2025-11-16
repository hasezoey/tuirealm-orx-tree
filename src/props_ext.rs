use tuirealm::{
	AttrValue,
	Attribute,
	Props,
};

/// Extra function on [`Props`] to make life easier and apply DRY.
///
/// Some functions might be upstreamed at some point.
pub trait PropsExt {
	fn should_display(&self) -> bool;
}

impl PropsExt for Props {
	fn should_display(&self) -> bool {
		return self.get_or(Attribute::Display, AttrValue::Flag(true)) == AttrValue::Flag(true);
	}
}
