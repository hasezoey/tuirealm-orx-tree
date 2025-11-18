//! Type Aliases

use orx_tree::Dyn;
use tuirealm::{
	props::Style,
	ratatui::{
		buffer::Buffer,
		layout::Rect,
		text::Line,
		widgets::Widget,
	},
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
	/// Render the current value.
	///
	/// Also see [`Widget::render`].
	fn render(&self, buf: &mut Buffer, area: Rect, offset: usize, style: Style);
}

impl NodeValue for &str {
	fn render(&self, buf: &mut Buffer, area: Rect, offset: usize, style: Style) {
		let offset = self.ceil_char_boundary(offset);

		let slice = &self[offset..];

		// if the slice is empty, "Line" will not draw anything
		// but we want to still draw the highlight style, even if the value itself is out-of-bounds
		if slice.is_empty() {
			Line::raw(" ").style(style).render(area, buf);
		} else {
			Line::raw(slice).style(style).render(area, buf);
		}
	}
}

impl NodeValue for String {
	fn render(&self, buf: &mut Buffer, area: Rect, offset: usize, style: Style) {
		NodeValue::render(&self.as_str(), buf, area, offset, style);
	}
}
