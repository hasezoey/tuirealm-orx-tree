//! Module for all the render parts

use orx_tree::{
	Dfs,
	NodeRef,
	Traverser,
	traversal::OverDepthNode,
};
use tuirealm::{
	props::Style,
	ratatui::{
		buffer::Buffer,
		layout::Rect,
		widgets::{
			Block,
			Clear,
			StatefulWidget,
			Widget,
		},
	},
};

use crate::{
	component::TreeViewState,
	types::{
		Node,
		NodeValue,
		Tree,
	},
};

/// The symbol to use to display before a node that is closed but has children.
///
/// It should be `▶`.
pub const CHILD_CLOSED_INDICATOR: &str = "\u{25b6}";
/// The symbol to use to display before a node that is open and has children
///
/// It should be `▼`.
pub const CHILD_OPENED_INDICATOR: &str = "\u{25bc}";
/// The length to allocate for the Indicators ([`CHILD_CLOSED_INDICATOR`] & [`CHILD_OPENED_INDICATOR`]).
/// Any extra length not covered in the Indicators will be filled with space.
pub const CHILD_INDICATOR_LENGTH: u16 = 2;
/// Default indentation length (`* depth`)
pub const DEFAULT_INDENT: usize = 2;
/// Default text to display if the tree is empty.
pub const DEFAULT_EMPTY_TREE_TEXT: &str = "The Tree is empty";
/// Default Highlight Symbol draw width.
pub const DEFAULT_HG_WIDTH: u16 = 2;

/// The ratatui widget to draw.
///
/// Likely not what you want to use.
#[derive(Debug, Clone)]
#[must_use]
pub struct TreeWidget<'a, V: NodeValue> {
	/// The tree to render
	tree: &'a Tree<V>,

	/// The main style of the tree
	main_style:       Style,
	/// The Highlight Style for the currently highlighted element
	hg_style:         Style,
	/// The Highlight symbol for the currently highlighted element
	hg_str:           Option<&'a str>,
	/// The Highlight symbol draw width
	hg_width:         u16,
	/// The Highlight symbol draw behavior
	hg_draw_behavior: HighlightDrawBehavior,
	/// How much to indent a child compared to the parent
	indent_size:      usize,
	/// Optional block to render around the widget itself
	block:            Option<Block<'a>>,

	/// Text to display if the tree is empty (not even a root node)
	empty_tree_text: &'a str,
}

impl<'a, V> TreeWidget<'a, V>
where
	V: NodeValue,
{
	/// Create a new widget with the given tree and otherwise default values.
	pub fn new(tree: &'a Tree<V>) -> Self {
		return Self {
			tree,
			main_style: Style::default(),
			hg_style: Style::default(),
			hg_str: None,
			hg_width: DEFAULT_HG_WIDTH,
			hg_draw_behavior: HighlightDrawBehavior::default(),
			indent_size: DEFAULT_INDENT,
			block: None,
			empty_tree_text: DEFAULT_EMPTY_TREE_TEXT,
		};
	}

	/// Set a custom block to draw around the main widget.
	pub fn block(mut self, block: Block<'a>) -> Self {
		self.block = Some(block);

		return self;
	}

	/// Set the main style of the Tree.
	pub fn style(mut self, style: Style) -> Self {
		self.main_style = style;

		return self;
	}

	/// Set the Highlight style of the currently selected node.
	pub fn hg_style(mut self, style: Style) -> Self {
		self.hg_style = style;

		return self;
	}

	/// Set the Highlight Symbol to draw in addition to the line itself.
	pub fn hg_str(mut self, val: &'a str) -> Self {
		self.hg_str = Some(val);

		return self;
	}

	/// Set the Highlight Symbol draw width.
	///
	/// By default [`DEFAULT_HG_WIDTH`].
	pub fn hg_width(mut self, width: u16) -> Self {
		self.hg_width = width;

		return self;
	}

	/// Set the Highlight Symbol Draw behavior.
	///
	/// See [`HighlightDrawBehavior`] for available and default behavior.
	pub fn hg_draw_behavior(mut self, behavior: HighlightDrawBehavior) -> Self {
		self.hg_draw_behavior = behavior;

		return self;
	}

	/// Set a custom indent size.
	///
	/// Default: [`DEFAULT_INDENT`]
	pub fn indent(mut self, indent: usize) -> Self {
		self.indent_size = indent;

		return self;
	}

	/// Set a empty tree text.
	///
	/// Default: [`DEFAULT_EMPTY_TREE_TEXT`]
	pub fn empty_tree_text(mut self, val: &'a str) -> Self {
		self.empty_tree_text = val;

		return self;
	}
}

impl<V> StatefulWidget for TreeWidget<'_, V>
where
	V: NodeValue,
{
	type State = TreeViewState<V>;

	fn render(self, area: Rect, buf: &mut Buffer, state: &mut Self::State) {
		// render the block, if set
		let area = if let Some(block) = self.block {
			let inner = block.inner(area);
			block.render(area, buf);
			inner
		} else {
			area
		};

		if area.is_empty() {
			return;
		}

		state.set_last_size(area, self.tree);

		// The tree may not have a root set yet, if it is not set, we dont need to render anything
		let Some(root_node) = self.tree.get_root() else {
			self.empty_tree_text.render(area, buf);

			return;
		};
		let mut traverser = Dfs::<OverDepthNode>::new();
		let walker = root_node.walk_with(&mut traverser);

		let mut remaining_area = area;
		let mut remaining_offset = state.get_offset();

		for (depth, node) in walker {
			// the only time this can return none, is when the current node is the root node
			// which we want to treat as if it had a parent and that parent is open.

			// dont render if the parent is not opened
			if !is_parent_open(node.clone(), state) {
				continue;
			}

			// dont continue the loop if there is not more space to draw
			if remaining_area.is_empty() {
				break;
			}

			if remaining_offset.get_vertical() != 0 {
				remaining_offset.decr_vertical();
				continue;
			}

			// get the indent for this node to visually indicate it is part of something
			let mut indent = depth * self.indent_size;

			// in the static case, we want to indent everything by the highlight symbol draw width, except the currently selected one
			// which will later be decremented again.
			if self.hg_draw_behavior == HighlightDrawBehavior::Static {
				indent += usize::from(self.hg_width);
			}

			let mut line_area = remaining_area;
			// This can be done without clamping, as we at this point know that the area is not empty,
			// so it must have at least one width and one height.
			// This setting may not be necessary as we are currently rendering lines, but it is more correct to do this anyway.
			line_area.height = 1;
			let mut display_offset_horiz = remaining_offset.get_horizontal();

			let is_selected = state.is_selected(&node.idx());

			if is_selected && let Some(hg_symbol) = self.hg_str {
				Indicator::render(hg_symbol, self.hg_width, &mut display_offset_horiz, &mut line_area, buf);
				// in both "CombineIndent" and "Static" cases, we want to remove it from the indent again.
				indent = indent.saturating_sub(usize::from(self.hg_width));
			}

			let clear_area = calc_area_for_value(&mut display_offset_horiz, &mut line_area, indent);

			// render the indent
			Clear.render(clear_area, buf);

			let use_style = if is_selected { self.hg_style } else { self.main_style };

			// render the main data
			node.data()
				.render_with_indicators(buf, line_area, display_offset_horiz, use_style, node.is_leaf(), || {
					return state.is_opened(node.idx());
				});

			remaining_area.height = remaining_area.height.saturating_sub(1);
			remaining_area.y += 1;
		}
	}
}

/// Set the Highlight Symbol Draw behavior.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
#[repr(u16)]
pub enum HighlightDrawBehavior {
	/// Combine Highlight symbol width and indent, if possible.
	///
	/// Where not possible, the remainder will get indented by highlight symbol, only if it is currently selected.
	///
	/// This is the default.
	#[default]
	CombineIndent = 0,
	/// Always add Highlight symbol width to the indent to make for a more static feeling experience.
	Static        = 1,
}

impl HighlightDrawBehavior {
	/// Represent the current value as a `u16` for attribute storage.
	pub fn to_u16(self) -> u16 {
		return self as u16;
	}

	/// Get the value from a `u16`.
	///
	/// This assumes only the valid numbers are used from [`to_u16`](Self::to_u16).
	pub fn from_u16(val: u16) -> Self {
		return match val {
			0 => Self::CombineIndent,
			1 => Self::Static,
			_ => unreachable!(),
		};
	}
}

/// Get wheter the parent of the current node is open or not.
/// Root node will always count as having a open parent.
#[inline]
pub(crate) fn is_parent_open<V>(node: Node<'_, V>, state: &TreeViewState<V>) -> bool
where
	V: NodeValue,
{
	// this could be written with ".any" but i think this is cleaner than some "!"(bool invert)
	for parent in node.ancestors() {
		if !state.is_opened(parent.idx()) {
			return false;
		}
	}

	return true;
}

/// Calculate the area for `value`, removing that area from `available_area` and `display_offset_horiz`.
pub fn calc_area_for_value(display_offset_horiz: &mut usize, available_area: &mut Rect, value: usize) -> Rect {
	let draw_value = value.saturating_sub(*display_offset_horiz);

	*display_offset_horiz = display_offset_horiz.saturating_sub(value.saturating_sub(draw_value));

	// properly set "value_area" as there are
	let mut value_area = *available_area;
	value_area.width = value_area.width.min(u16::try_from(draw_value).unwrap_or(u16::MAX));

	// remove "value_area" from "available_area"
	// sadly there is no built-in "Rect" function to remove a specific section
	let offset = value_area.width;
	available_area.width = available_area.width.saturating_sub(offset);
	available_area.x += offset;

	return value_area;
}

/// Render a symbol to the screen and remove the used-up space.
#[derive(Debug, Clone, Copy)]
pub struct Indicator;

impl Indicator {
	/// Render a single symbol with `draw_length` length allocated to it. This length should be *at least* the draw size of the character(s).
	///
	/// For example for japanese characters and most emojis this should be at least `2`.
	/// This function does not do this draw length checking and trusts the input.
	///
	/// This will modify the given display offset and available area to remove the space this has been drawn on.
	///
	/// Limitation: this implementation expects only a single drawable character,
	/// if more are provided, they will only be drawn in full or not at all.
	///
	/// Example: if `123` is input as a symbol and `4` as the length, then it will only display `123 ` or `   `(offset 1), never `23 `.
	pub fn render(
		symbol: &str,
		draw_length: u16,
		display_offset_horiz: &mut usize,
		available_area: &mut Rect,
		buf: &mut Buffer,
	) {
		let draw_area = calc_area_for_value(display_offset_horiz, available_area, usize::from(draw_length));

		if draw_area.is_empty() {
			return;
		}

		// clear the area in case allocated length is higher than the symbol length
		Clear.render(draw_area, buf);

		// Only draw the symbol in full and only at the beginning.
		// This limitation is because we dont count grapheme length
		// and can arbitrarily index into the string.
		if draw_area.width >= draw_length {
			symbol.render(draw_area, buf);
		}
	}
}

/// Render the true/false symbol based on the given state in the given area.
///
/// The default is [`CHILD_OPENED_INDICATOR`] and [`CHILD_CLOSED_INDICATOR`] for open/closed drawing.
///
/// Note that the `allocate_length` should be at least the draw width of the biggest character.
///
/// Limitation: this implementation expects only a single drawable character in either state,
/// if more are provided, they will only be drawn in full or not at all.
///
/// Example: if `123` is input as a symbol and `4` as the length, then it will only display `123 ` or `   `(offset 1), never `23 `.
#[derive(Debug, Clone)]
pub struct OrIndicators<'a> {
	/// The indicator text to draw for the "true" state
	on_true:         &'a str,
	/// The indicator text to draw for the "false" state
	on_false:        &'a str,
	/// The cell length to allocate for the symbols
	allocate_length: u16,
}

impl Default for OrIndicators<'static> {
	fn default() -> Self {
		return Self {
			on_true:         CHILD_OPENED_INDICATOR,
			on_false:        CHILD_CLOSED_INDICATOR,
			allocate_length: CHILD_INDICATOR_LENGTH,
		};
	}
}

impl<'a> OrIndicators<'a> {
	/// Create a new instance with custom indicators.
	///
	/// Note: it should be ensured that `open` and `closed` are fitting within `length`.
	/// This struct does not do grapheme / drawable length checking.
	#[inline]
	pub const fn new(open: &'a str, closed: &'a str, length: u16) -> Self {
		return Self {
			on_true:         open,
			on_false:        closed,
			allocate_length: length,
		};
	}

	/// Render the symbol based on `open` in the given `available_area`, but modify it to remove the used area.
	pub fn render(&self, display_offset_horiz: &mut usize, available_area: &mut Rect, buf: &mut Buffer, state: bool) {
		let symbol = if state { self.on_true } else { self.on_false };
		Indicator::render(symbol, self.allocate_length, display_offset_horiz, available_area, buf);
	}
}
