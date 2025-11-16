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
		layout::{
			Constraint,
			Layout,
			Rect,
		},
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

/// The symbol to use to display before a node that is closed but has children
pub const CHILD_CLOSED_INDICATOR: &str = "\u{25b6}";
/// The symbol to use to display before a node that is open and has children
pub const CHILD_OPENED_INDICATOR: &str = "\u{25bc}";
/// The length to allocate for the Indicators ([`CHILD_CLOSED_INDICATOR`] & [`CHILD_OPENED_INDICATOR`]).
/// Any extra length not covered in the Indicators will be filled with space.
pub const CHILD_INDICATOR_LENGTH: u16 = 2;
pub const DEFAULT_INDENT: usize = 2;

/// The ratatui widget to draw.
///
/// Likely not what you want to use.
#[derive(Debug, Clone)]
#[must_use]
pub struct TreeWidget<'a, V: NodeValue> {
	/// The tree to render
	tree: &'a Tree<V>,

	/// The main style of the tree
	main_style:  Style,
	/// The Highlight Style for the currently highlighted element
	hg_style:    Style,
	/// The Highlight symbol for the currently highlighted element
	hg_str:      Option<&'a str>,
	/// How much to indent a child compared to the parent
	indent_size: usize,
	/// Optional block to render around the widget itself
	block:       Option<Block<'a>>,
}

impl<'a, V> TreeWidget<'a, V>
where
	V: NodeValue,
{
	pub fn new(tree: &'a Tree<V>) -> Self {
		return Self {
			tree,
			main_style: Style::default(),
			hg_style: Style::default(),
			hg_str: None,
			indent_size: DEFAULT_INDENT,
			block: None,
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

	/// Set a custom indent size.
	///
	/// Default: [`DEFAULT_INDENT`]
	pub fn indent(mut self, indent: usize) -> Self {
		self.indent_size = indent;

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

		state.set_last_size(area);

		// The tree may not have a root set yet, if it is not set, we dont need to render anything
		let Some(root_node) = self.tree.get_root() else {
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

			let is_leaf = node.num_children() == 0;

			// get the indent for this node to visually indicate it is part of something
			let indent_leaf = if is_leaf { CHILD_INDICATOR_LENGTH } else { 0 };
			let indent = (depth * self.indent_size) + usize::from(indent_leaf);
			let indent = u16::try_from(indent).unwrap_or(u16::MAX);

			let child_sym_length = if is_leaf { 0 } else { CHILD_INDICATOR_LENGTH };

			let [clear_area, child_sym, line_area] = Layout::horizontal([
				Constraint::Length(indent),
				Constraint::Length(child_sym_length),
				Constraint::Fill(1),
			])
			.areas(remaining_area);

			// render the indent
			Clear.render(clear_area, buf);

			// render the indicators
			if !is_leaf && !child_sym.is_empty() {
				let sym = if state.is_opened(&node.idx()) {
					CHILD_OPENED_INDICATOR
				} else {
					CHILD_CLOSED_INDICATOR
				};

				sym.render(child_sym, buf);
			}

			let use_style = if state.is_selected(&node.idx()) {
				self.hg_style
			} else {
				self.main_style
			};

			// render the main data
			node.data().get_text().style(use_style).render(line_area, buf);

			remaining_area.height = remaining_area.height.saturating_sub(1);
			remaining_area.y += 1;
		}
	}
}

/// Get wheter the parent of the current node is open or not.
/// Root node will always count as having a open parent.
#[inline]
pub(crate) fn is_parent_open<V>(node: Node<'_, V>, state: &TreeViewState<V>) -> bool
where
	V: NodeValue,
{
	// we only want to check the parent and up of the given node
	let mut to_check = node.parent();

	// if the input node already does not have a parent (`to_check = None`), use base case "true"
	// because that is the root node, which we always want to display
	while let Some(node) = to_check {
		if state.is_opened(&node.idx()) {
			to_check = node.parent();
		} else {
			return false;
		}
	}

	return true;
}
