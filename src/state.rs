use orx_tree::{
	Dfs,
	NodeRef,
	Traverser,
	traversal::OverNode,
};
use tuirealm::ratatui::layout::Rect;

use crate::{
	types::{
		Node,
		NodeIdx,
		NodeValue,
		Tree,
	},
	widget::is_parent_open,
};

/// The distance to preview instead of always having the last displayed element be the selected one.
///
/// Note that this distance is reduced when there is not enough area to display.
const PREVIEW_DISTANCE: usize = 2;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub(crate) struct Offset {
	// Horizontal offset
	x: usize,
	// Vertical offset
	y: usize,
}

impl Offset {
	/// Reset the offset to `0`.
	pub fn reset(&mut self) {
		self.x = 0;
		self.y = 0;
	}

	/// Set the vertical offset.
	pub fn set_vertical(&mut self, to: usize) {
		self.y = to;
	}

	// /// Set the horizontal offset.
	// pub fn set_horizontal(&mut self, to: usize) {
	// 	self.x = to;
	// }

	/// Decrement the vertical offset by `1`.
	pub fn decr_vertical(&mut self) {
		self.y = self.y.saturating_sub(1);
	}

	// /// Decrement the horizontal offset by `1`.
	// pub fn decr_horizontal(&mut self) {
	// 	self.x = self.x.saturating_sub(1);
	// }

	/// Get the current vertical value.
	pub fn get_vertical(&self) -> usize {
		self.y
	}
}

/// The main state-keeper
#[derive(Debug, Clone, PartialEq)]
pub struct TreeViewState<V: NodeValue> {
	selected: Option<NodeIdx<V>>,
	open:     Vec<NodeIdx<V>>,

	/// The last known size of the tree area.
	last_tree_size: Option<Rect>,
	/// The offset to skip drawing.
	display_offset: Offset,
}

impl<V> Default for TreeViewState<V>
where
	V: NodeValue,
{
	fn default() -> Self {
		return Self {
			selected:       None,
			open:           Vec::default(),
			last_tree_size: None,
			display_offset: Offset::default(),
		};
	}
}

impl<V> TreeViewState<V>
where
	V: NodeValue,
{
	/// Is the given node selected?
	pub fn is_selected(&self, node: &NodeIdx<V>) -> bool {
		let Some(selected) = self.selected.as_ref() else {
			return false;
		};
		return node == selected;
	}

	/// Is the given node opened?
	pub fn is_opened(&self, node: &NodeIdx<V>) -> bool {
		return self.open.contains(node);
	}

	/// Open a specific Node.
	pub fn open(&mut self, node: NodeIdx<V>) {
		self.open.push(node);
	}

	/// Open a specific Node.
	pub fn close(&mut self, node: &NodeIdx<V>) {
		self.open.retain(|v| return v != node);
	}

	/// Select a specific node or unselect the current node by setting it to `None`.
	///
	/// NOTE: this does *not* change the offset.
	pub fn select(&mut self, node: Option<NodeIdx<V>>) {
		self.selected = node;
	}

	/// Get a copy of the current offset.
	pub(crate) fn get_offset(&self) -> Offset {
		return self.display_offset;
	}

	/// Get the offset mutably.
	pub(crate) fn get_offset_mut(&mut self) -> &mut Offset {
		return &mut self.display_offset;
	}

	/// Calculate & set the display offset for downwards motion.
	///
	/// This does not change display offset if the new `node_offset` still fits within the curren display offset + height.
	fn set_vert_offset_down(&mut self, node_offset: usize) {
		let area = self.last_tree_size.unwrap_or_default();
		let height_as_usize = usize::from(area.height.saturating_sub(1));

		let old_offset = self.display_offset.get_vertical();

		if (old_offset + height_as_usize).saturating_sub(PREVIEW_DISTANCE) < node_offset {
			self.display_offset
				.set_vertical(node_offset.saturating_sub(height_as_usize.saturating_sub(PREVIEW_DISTANCE)));
		}
	}

	/// Calculate & set the display offset for upwards motion.
	///
	/// This does not change display offset if the new `node_offset` still fits within the curren display offset.
	fn set_vert_offset_up(&mut self, node_offset: usize) {
		let old_offset = self.display_offset.get_vertical();

		if old_offset > node_offset.saturating_sub(PREVIEW_DISTANCE) {
			self.display_offset
				.set_vertical(node_offset.saturating_sub(PREVIEW_DISTANCE));
		}
	}

	/// Select the next node downwards.
	///
	/// Fetches the next node, calculates the offset and applies it.
	pub fn select_next_down(&mut self, tree: &Tree<V>) {
		let next = self.get_next_node_down(tree);

		match next.as_ref().and_then(|v| return self.get_offset_of_node(tree, v)) {
			Some(offset) => {
				self.set_vert_offset_down(offset);
			},
			None => self.display_offset.reset(),
		}

		self.select(next);
	}

	/// Select the next node upwards.
	///
	/// Fetches the next node, calculates the offset and applies it.
	pub fn select_next_up(&mut self, tree: &Tree<V>) {
		let next = self.get_next_node_up(tree);

		match next.as_ref().and_then(|v| return self.get_offset_of_node(tree, v)) {
			Some(offset) => {
				self.set_vert_offset_up(offset);
			},
			None => self.display_offset.reset(),
		}

		self.select(next);
	}

	/// Select the first node.
	///
	/// Fetches the next node, resets the offset and applies it.
	pub fn select_first(&mut self, tree: &Tree<V>) {
		self.display_offset.reset();
		self.select(tree.get_root().map(|v| return v.idx()));
	}

	/// Select the last node of the last opened parent node.
	///
	/// Fetches the next node, calculates the offset and applies it.
	pub fn select_last(&mut self, tree: &Tree<V>) {
		let next = self.get_last_open_node(tree);

		match next.as_ref().and_then(|v| return self.get_offset_of_node(tree, v)) {
			Some(offset) => {
				self.set_vert_offset_down(offset);
			},
			None => self.display_offset.reset(),
		}

		self.select(next);
	}

	/// Select the last element (minus [`PREVIEW_DISTANCE`]) in current view, if that is already selected, scroll down by `area.height - PREVIEW_DISTANCE`.
	pub fn select_pg_down(&mut self, tree: &Tree<V>) {
		// TODO: do a more efficient implementation
		// first get the offset of the currently selected node
		let offset_curr_sel = self
			.selected()
			.and_then(|v| tree.get_node(v))
			.and_then(|v| self.get_offset_of_node(tree, &v.idx()))
			.unwrap_or(0);
		let old_offset = self.display_offset.get_vertical();
		let area = self.last_tree_size.unwrap_or_default();
		let height_as_usize = usize::from(area.height.saturating_sub(1));

		// get how many iterations / lines to scroll by, depending on if we are on the last visible element or somewhere in-between
		let mut iterations = if (old_offset + height_as_usize).saturating_sub(PREVIEW_DISTANCE) > offset_curr_sel {
			(old_offset + height_as_usize)
				.saturating_sub(PREVIEW_DISTANCE)
				.saturating_sub(offset_curr_sel)
		} else {
			height_as_usize.saturating_sub(PREVIEW_DISTANCE)
		};

		let Some(mut next) = self
			.selected()
			.and_then(|v| tree.get_node(v))
			.or_else(|| tree.get_root())
		else {
			// no nodes in tree
			return;
		};

		// scroll for "iterations" / lines
		while iterations != 0 {
			let old = next.idx();
			next = self.get_next_node_down_checked(next);

			if old == next.idx() {
				break;
			}

			iterations -= 1;
		}

		// normal set scroll and offset
		let next = next.idx();

		match self.get_offset_of_node(tree, &next) {
			Some(offset) => {
				self.set_vert_offset_down(offset);
			},
			None => self.display_offset.reset(),
		}

		self.select(Some(next));
	}

	/// Select the first element (minus [`PREVIEW_DISTANCE`]) in current view, if that is already selected, scroll up by `area.height - PREVIEW_DISTANCE`.
	pub fn select_pg_up(&mut self, tree: &Tree<V>) {
		// TODO: do a more efficient implementation
		// first get the offset of the currently selected node
		let offset_curr_sel = self
			.selected()
			.and_then(|v| tree.get_node(v))
			.and_then(|v| self.get_offset_of_node(tree, &v.idx()))
			.unwrap_or(0);
		let old_offset = self.display_offset.get_vertical();
		let area = self.last_tree_size.unwrap_or_default();
		let height_as_usize = usize::from(area.height.saturating_sub(1));

		// get how many iterations / lines to scroll by, depending on if we are on the first visible element or somewhere in-between
		let mut iterations = if old_offset < offset_curr_sel.saturating_sub(PREVIEW_DISTANCE) {
			offset_curr_sel
				.saturating_sub(old_offset)
				.saturating_sub(PREVIEW_DISTANCE)
		} else {
			height_as_usize.saturating_sub(PREVIEW_DISTANCE)
		};

		let Some(mut next) = self
			.selected()
			.and_then(|v| tree.get_node(v))
			.or_else(|| tree.get_root())
		else {
			// no nodes in tree
			return;
		};

		// scroll for "iterations" / lines
		while iterations != 0 {
			let old = next.idx();
			next = self.get_next_node_up_checked(next);

			if old == next.idx() {
				break;
			}

			iterations -= 1;
		}

		// normal set scroll and offset
		let next = next.idx();

		match self.get_offset_of_node(tree, &next) {
			Some(offset) => {
				self.set_vert_offset_up(offset);
			},
			None => self.display_offset.reset(),
		}

		self.select(Some(next));
	}

	/// Get the current select value.
	pub fn selected(&self) -> Option<&NodeIdx<V>> {
		return self.selected.as_ref();
	}

	/// Set the last known tree draw area (excluding the block).
	pub(crate) fn set_last_size(&mut self, area: Rect) {
		self.last_tree_size = Some(area);
	}

	/// Get the next node downwards, where:
	/// - Base-case: select root node
	/// - If current node is not open, select next sibling downwards
	///   - If there is no next sibling, try next sibling of parent (recursive, until hitting Root node, which aborts)
	/// - If current note is open and has children, select first child
	pub fn get_next_node_down(&self, tree: &Tree<V>) -> Option<NodeIdx<V>> {
		if let Some(selected) = self.selected().and_then(|v| return tree.get_node(v)) {
			return Some(self.get_next_node_down_checked(selected).idx());
		}
		return Some(tree.get_root()?.idx());
	}

	/// Dont use this function directly, use [`get_next_node_down`].
	///
	/// Get the next node downwards, otherwise returning itself, for order see [`get_next_node_down`].
	fn get_next_node_down_checked<'a>(&self, selected: Node<'a, V>) -> Node<'a, V> {
		// if the current node is open and has children, get the first child
		if self.is_opened(&selected.idx()) {
			if let Some(child) = selected.get_child(0) {
				return child;
			}
		}

		// otherwise get the next sibling; if there is no next sibling, return self
		return Self::get_next_sibling_down(&selected).unwrap_or_else(|| selected);
	}

	/// Dont use this function directly, use [`get_next_node_down`].
	///
	/// Get the next node downwards where:
	/// - If current node is open, select first child
	/// - If current node is not open:
	///   - Select the next sibling, or
	///   - Select the next sibling of the parent (until hitting the root node, which returns `None`)
	fn get_next_sibling_down<'a>(selected: &Node<'a, V>) -> Option<Node<'a, V>> {
		let sibling_idx = selected.sibling_idx();
		let parent = selected.parent()?;

		// If we are on the last sibling of the current node, try next silbing of parent
		if sibling_idx == parent.num_children().saturating_sub(1) {
			return Self::get_next_sibling_down(&parent);
		}

		// We are not at the last sibling yet, so get the next sibling
		return parent.get_child(sibling_idx + 1);
	}

	/// Get the next node upwards, where:
	/// - Base-case: select root node
	/// - If parent has another sibling before, use that
	///   - If that sibling is open, use the last child of that (recursively)
	/// - If parent has no other sibling before, use parent
	pub fn get_next_node_up(&self, tree: &Tree<V>) -> Option<NodeIdx<V>> {
		if let Some(selected) = self.selected().and_then(|v| return tree.get_node(v)) {
			return Some(self.get_next_node_up_checked(selected).idx());
		}
		return Some(tree.get_root()?.idx());
	}

	/// Dont use this function directly, use [`get_next_node_up`].
	///
	/// Get the next node upwards where:
	/// - Base-case: return itself
	/// - If parent has another sibling before, use that
	///   - If that sibling is open, use the last child of that (recursively)
	/// - If parent has no other sibling before, use parent
	fn get_next_node_up_checked<'a>(&self, selected: Node<'a, V>) -> Node<'a, V> {
		let sibling_idx = selected.sibling_idx();

		let Some(parent) = selected.parent() else {
			return selected;
		};

		if sibling_idx == 0 {
			return parent;
		}

		if let Some(sibling) = parent.get_child(sibling_idx.saturating_sub(1)) {
			return self.get_last_open_node_of(sibling);
		}

		return selected;
	}

	/// Get the last open node in `selected`, recursively.
	fn get_last_open_node_of<'a>(&self, selected: Node<'a, V>) -> Node<'a, V> {
		if self.is_opened(&selected.idx()) {
			if let Some(child) = selected.get_child(selected.num_children().saturating_sub(1)) {
				return self.get_last_open_node_of(child);
			}
		}

		return selected;
	}

	/// Get the very last (bottom) node of the tree.
	pub fn get_last_open_node(&self, tree: &Tree<V>) -> Option<NodeIdx<V>> {
		let mut node = tree.get_root()?;

		while self.is_opened(&node.idx()) && node.num_children() > 0 {
			node = self.get_last_open_node_of(node);
		}

		return Some(node.idx());
	}

	/// Get the offset of a specific node as it would be in draw order.
	fn get_offset_of_node(&self, tree: &Tree<V>, predicate: &NodeIdx<V>) -> Option<usize> {
		let root_node = tree.get_root()?;
		// TODO: generic walker
		let mut traverser = Dfs::<OverNode>::new();
		let walker = root_node.walk_with(&mut traverser);

		let mut position: usize = 0;

		for node in walker {
			if !is_parent_open(node.clone(), self) {
				continue;
			}

			position += 1;

			if node.idx() == *predicate {
				break;
			}
		}

		if position == 0 {
			return None;
		} else {
			return Some(position.saturating_sub(1));
		}
	}
}
