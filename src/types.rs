//! Type Aliases

use orx_tree::Dyn;
use tuirealm::ratatui::text::{
	Line,
	ToLine,
};

#[expect(type_alias_bounds)]
pub type Tree<V: NodeValue> = orx_tree::DynTree<V>;
#[expect(type_alias_bounds)]
pub type NodeIdx<V: NodeValue> = orx_tree::NodeIdx<Dyn<V>>;
#[expect(type_alias_bounds)]
pub type Node<'a, V: NodeValue> = orx_tree::Node<'a, Dyn<V>>;
#[expect(type_alias_bounds)]
pub type NodeMut<'a, V: NodeValue> = orx_tree::NodeMut<'a, Dyn<V>>;

pub trait NodeValue {
	/// Get the text to display for the current node value.
	fn get_text(&self) -> Line<'_>;
}

impl<T> NodeValue for T
where
	T: ToLine,
{
	fn get_text(&self) -> Line<'_> {
		return self.to_line();
	}
}
