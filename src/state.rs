use std::num::NonZeroUsize;

use orx_tree::{
	Dfs,
	NodeRef,
	Traverser,
	traversal::OverNode,
};
use tuirealm::ratatui::layout::{
	Rect,
	Size,
};

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
pub const PREVIEW_DISTANCE_DEFAULT: u16 = 2;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub(crate) struct Offset {
	// Horizontal offset
	x: usize,
	// Vertical offset
	y: usize,
}

impl Offset {
	/// Create a new offset with specific values
	#[allow(unused)]
	pub fn new(x: usize, y: usize) -> Self {
		return Self { x, y };
	}

	/// Reset the offset to `0`.
	pub fn reset(&mut self) {
		self.x = 0;
		self.y = 0;
	}

	/// Set the vertical offset.
	pub fn set_vertical(&mut self, to: usize) {
		self.y = to;
	}

	/// Set the horizontal offset.
	pub fn set_horizontal(&mut self, to: usize) {
		self.x = to;
	}

	/// Decrement the vertical offset by `1`.
	pub fn decr_vertical(&mut self) {
		self.y = self.y.saturating_sub(1);
	}

	/// Decrement the horizontal offset by `1`.
	#[expect(dead_code)]
	pub fn decr_horizontal(&mut self) {
		self.x = self.x.saturating_sub(1);
	}

	/// Get the current vertical value.
	pub fn get_vertical(&self) -> usize {
		return self.y;
	}

	/// Get the current horizontal value.
	pub fn get_horizontal(&self) -> usize {
		return self.x;
	}
}

/// A [`NonZeroUsize`] that is `1`, compile-time checked.
const ONE_NONEZERO: NonZeroUsize = NonZeroUsize::new(1).unwrap();

/// The main state-keeper
#[derive(Debug, Clone, PartialEq)]
pub struct TreeViewState<V: NodeValue> {
	selected: Option<NodeIdx<V>>,
	open:     Vec<NodeIdx<V>>,

	/// The last known size of the tree area.
	last_tree_size: Option<Rect>,
	/// The offset to skip drawing.
	display_offset: Offset,

	/// Determines how many cells to step in a scroll.
	scroll_step_horiz: NonZeroUsize,
	/// Determines how many lines to step in a scroll.
	scroll_step_verti: NonZeroUsize,

	/// Determines the vertical amount of elements to display before/after the selection
	/// (depending on motion direction).
	preview_distance_vertical: usize,
}

impl<V> Default for TreeViewState<V>
where
	V: NodeValue,
{
	fn default() -> Self {
		return Self {
			selected: None,
			open: Vec::default(),
			last_tree_size: None,
			display_offset: Offset::default(),
			scroll_step_horiz: ONE_NONEZERO,
			scroll_step_verti: ONE_NONEZERO,
			preview_distance_vertical: usize::from(PREVIEW_DISTANCE_DEFAULT),
		};
	}
}

impl<V> TreeViewState<V>
where
	V: NodeValue,
{
	/// Set the horizontal scroll stepping.
	///
	/// Default: `1`
	pub fn set_horizontal_scroll_step(&mut self, stepping: NonZeroUsize) {
		self.scroll_step_horiz = stepping;
	}

	/// Set the vertical scroll stepping.
	///
	/// Default: `1`
	pub fn set_vertical_scroll_step(&mut self, stepping: NonZeroUsize) {
		self.scroll_step_verti = stepping;
	}

	/// Set a custom vertical preview distance.
	///
	/// This value keeps at least `distance` amount of element before / after the selection on scroll.
	/// Note that this distance is reduced when there is not enough area to display.
	///
	/// Default: [`PREVIEW_DISTANCE_DEFAULT`]
	pub fn set_preview_distance_horizontal(&mut self, distance: u16) {
		self.preview_distance_vertical = usize::from(distance);
	}

	/// Get the current horizontal scroll stepping.
	pub fn get_horizontal_scroll_step(&self) -> NonZeroUsize {
		return self.scroll_step_horiz;
	}

	/// Get the current vertical scroll stepping.
	pub fn get_vertical_scroll_step(&self) -> NonZeroUsize {
		return self.scroll_step_verti;
	}

	/// Is the given node selected?
	pub fn is_selected(&self, node: &NodeIdx<V>) -> bool {
		let Some(selected) = self.selected.as_ref() else {
			return false;
		};
		return node == selected;
	}

	/// Is the given node opened?
	pub fn is_opened(&self, node: NodeIdx<V>) -> bool {
		return self.open.contains(&node);
	}

	/// Open a specific Node.
	pub fn open(&mut self, node: NodeIdx<V>) {
		self.open.push(node);
	}

	/// Open all parents of the given node.
	///
	/// Does not open the node itself.
	pub fn open_all_parents(&mut self, tree: &Tree<V>, node: NodeIdx<V>) {
		let Some(mut node) = tree.get_node(node) else {
			return;
		};

		while let Some(parent) = node.parent() {
			self.open(parent.idx());

			node = parent;
		}
	}

	/// Open a specific Node.
	pub fn close(&mut self, node: NodeIdx<V>) {
		self.open.retain(|v| return *v != node);
	}

	/// Get all open nodes.
	pub(crate) fn get_all_open(&self) -> &[NodeIdx<V>] {
		return &self.open;
	}

	/// Force-overwrite all open nodes with a new value.
	///
	/// This is necessary after a memory reclaim which potentially invalidates [`NodeIdx`]'s
	/// and have to be re-generated.
	pub(crate) fn overwrite_open(&mut self, open: Vec<NodeIdx<V>>) {
		self.open = open;
	}

	/// Select a specific node or unselect the current node by setting it to `None`.
	///
	/// NOTE: this does *not* change the offset, use [`select_set_offset`](Self::select_set_offset) instead.
	pub fn select(&mut self, node: Option<NodeIdx<V>>) {
		self.selected = node;
	}

	/// Get a copy of the current offset.
	pub(crate) fn get_offset(&self) -> Offset {
		return self.display_offset;
	}

	/// Get the offset mutably.
	#[expect(dead_code)]
	pub(crate) fn get_offset_mut(&mut self) -> &mut Offset {
		return &mut self.display_offset;
	}

	/// Calculate & set display offset for a new node_offset.
	///
	/// This does not change the display offset if the new `node_offset` still fits within the current visible bounds (minus preview-distance).
	fn set_vert_offset(&mut self, node_offset: usize) {
		let area = self.last_tree_size.unwrap_or_default();
		// convert it for easier usage and remove 1 as offsets are index(0) based.
		let height_as_usize = usize::from(area.height.saturating_sub(1));

		let old_offset = self.display_offset.get_vertical();

		if (old_offset + height_as_usize).saturating_sub(self.preview_distance_vertical) < node_offset {
			// downwards motion
			self.display_offset.set_vertical(
				node_offset.saturating_sub(height_as_usize.saturating_sub(self.preview_distance_vertical)),
			);
		} else if old_offset > node_offset.saturating_sub(self.preview_distance_vertical) {
			// upwards motion
			self.display_offset
				.set_vertical(node_offset.saturating_sub(self.preview_distance_vertical));
		}
	}

	/// Select the next node downwards.
	///
	/// Fetches the next node, calculates the offset and applies it.
	pub fn select_next_down(&mut self, tree: &Tree<V>) {
		let next = self.get_next_node_down(tree);

		if let Some(node) = next {
			self.select_set_offset(tree, node);
		} else {
			self.display_offset.reset();
			self.select(None);
		}
	}

	/// Select the given node and move the display offset so that it is within view.
	pub fn select_set_offset(&mut self, tree: &Tree<V>, node: NodeIdx<V>) {
		match self.get_offset_of_node(tree, node) {
			Some(offset) => {
				self.set_vert_offset(offset);
			},
			None => self.display_offset.reset(),
		}

		self.select(Some(node));
	}

	/// Select the next node upwards.
	///
	/// Fetches the next node, calculates the offset and applies it.
	pub fn select_next_up(&mut self, tree: &Tree<V>) {
		let next = self.get_next_node_up(tree);

		if let Some(node) = next {
			self.select_set_offset(tree, node);
		} else {
			self.display_offset.reset();
			self.select(None);
		}
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

		match next.as_ref().and_then(|v| return self.get_offset_of_node(tree, *v)) {
			Some(offset) => {
				self.set_vert_offset(offset);
			},
			None => self.display_offset.reset(),
		}

		self.select(next);
	}

	/// Select the last element (minus [`PREVIEW_DISTANCE_DEFAULT`]) in current view, if that is already selected, scroll down by `area.height - self.preview_distance_vertical`.
	pub fn select_pg_down(&mut self, tree: &Tree<V>) {
		// TODO: do a more efficient implementation
		// first get the offset of the currently selected node
		let offset_curr_sel = self
			.selected()
			.and_then(|v| return tree.get_node(v))
			.and_then(|v| return self.get_offset_of_node(tree, v.idx()))
			.unwrap_or(0);
		let old_offset = self.display_offset.get_vertical();
		let area = self.last_tree_size.unwrap_or_default();
		let height_as_usize = usize::from(area.height.saturating_sub(1));

		// get how many iterations / lines to scroll by, depending on if we are on the last visible element or somewhere in-between
		let mut iterations =
			if (old_offset + height_as_usize).saturating_sub(self.preview_distance_vertical) > offset_curr_sel {
				(old_offset + height_as_usize)
					.saturating_sub(self.preview_distance_vertical)
					.saturating_sub(offset_curr_sel)
			} else {
				height_as_usize.saturating_sub(self.preview_distance_vertical)
			};

		let Some(mut next) = self
			.selected()
			.and_then(|v| return tree.get_node(v))
			.or_else(|| return tree.get_root())
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

		match self.get_offset_of_node(tree, next) {
			Some(offset) => {
				self.set_vert_offset(offset);
			},
			None => self.display_offset.reset(),
		}

		self.select(Some(next));
	}

	/// Select the first element (minus [`PREVIEW_DISTANCE_DEFAULT`]) in current view, if that is already selected, scroll up by `area.height - self.preview_distance_vertical`.
	pub fn select_pg_up(&mut self, tree: &Tree<V>) {
		// TODO: do a more efficient implementation
		// first get the offset of the currently selected node
		let offset_curr_sel = self
			.selected()
			.and_then(|v| return tree.get_node(v))
			.and_then(|v| return self.get_offset_of_node(tree, v.idx()))
			.unwrap_or(0);
		let old_offset = self.display_offset.get_vertical();
		let area = self.last_tree_size.unwrap_or_default();
		let height_as_usize = usize::from(area.height.saturating_sub(1));

		// get how many iterations / lines to scroll by, depending on if we are on the first visible element or somewhere in-between
		let mut iterations = if old_offset < offset_curr_sel.saturating_sub(self.preview_distance_vertical) {
			// current selection is still above old_offset (plus preview), so we dont scroll a page
			// but select the first element in the visible area (plus preview distance)
			offset_curr_sel
				.saturating_sub(old_offset)
				.saturating_sub(self.preview_distance_vertical)
		} else {
			height_as_usize.saturating_sub(self.preview_distance_vertical)
		};

		let Some(mut next) = self
			.selected()
			.and_then(|v| return tree.get_node(v))
			.or_else(|| return tree.get_root())
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

		match self.get_offset_of_node(tree, next) {
			Some(offset) => {
				self.set_vert_offset(offset);
			},
			None => self.display_offset.reset(),
		}

		self.select(Some(next));
	}

	/// Select the parent node of the currently selected node, if there is one.
	///
	/// Fetches the next node, calculates the offset and applies it.
	pub fn select_parent(&mut self, tree: &Tree<V>) {
		let Some(current) = self.selected() else {
			return;
		};

		let Some(next) = tree
			.get_node(current)
			.and_then(|v| return v.parent())
			.map(|v| return v.idx())
		else {
			return;
		};

		match self.get_offset_of_node(tree, next) {
			Some(offset) => {
				self.set_vert_offset(offset);
			},
			None => self.display_offset.reset(),
		}

		self.select(Some(next));
	}

	/// Get the current select value.
	pub fn selected(&self) -> Option<NodeIdx<V>> {
		return self.selected;
	}

	/// Change the display offset so that the currently selected node is always visible.
	///
	/// For example after a component size change.
	pub fn clamp_offset_selected(&mut self, tree: &Tree<V>) {
		let Some(current_pos) = self
			.selected()
			.and_then(|v| return tree.get_node(v))
			.and_then(|v| return self.get_offset_of_node(tree, v.idx()))
		else {
			// no node is selected, or it is not valid anymore
			return;
		};

		self.set_vert_offset(current_pos);
	}

	/// Set the last known tree draw area (excluding the block).
	///
	/// And clamp the display offset so that selected is always visible, if necessary.
	pub(crate) fn set_last_size(&mut self, area: Rect, tree: &Tree<V>) {
		let need_clamping = self
			.last_tree_size
			.is_some_and(|v| return size_height_gt(v.as_size(), area.as_size()))
			&& self.selected().is_some();

		self.last_tree_size = Some(area);

		if need_clamping {
			self.clamp_offset_selected(tree);
		}
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

	/// Dont use this function directly, use [`get_next_node_down`](Self::get_next_node_down).
	///
	/// Get the next node downwards, otherwise returning itself, for order see [`get_next_node_down`](Self::get_next_node_down).
	fn get_next_node_down_checked<'a>(&self, selected: Node<'a, V>) -> Node<'a, V> {
		// if the current node is open and has children, get the first child
		if self.is_opened(selected.idx())
			&& let Some(child) = selected.get_child(0)
		{
			return child;
		}

		// otherwise get the next sibling; if there is no next sibling, return self
		return Self::get_next_sibling_down(&selected).unwrap_or_else(|| return selected);
	}

	/// Dont use this function directly, use [`get_next_node_down`](Self::get_next_node_down).
	///
	/// Get the next node downwards where:
	/// - If current node is open, select first child
	/// - If current node is not open:
	///   - Select the next sibling, or
	///   - Select the next sibling of the parent (until hitting the root node, which returns `None`)
	fn get_next_sibling_down<'a>(selected: &Node<'a, V>) -> Option<Node<'a, V>> {
		let mut node_sibling_idx = selected.sibling_idx();
		let mut parent = selected.parent()?;

		while node_sibling_idx == parent.num_children().saturating_sub(1) {
			node_sibling_idx = parent.sibling_idx();
			parent = parent.parent()?;
		}

		// We are not at the last sibling yet, so get the next sibling
		return parent.get_child(node_sibling_idx + 1);
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

	/// Dont use this function directly, use [`get_next_node_up`](Self::get_next_node_up).
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
		let mut node = selected;

		while self.is_opened(node.idx())
			&& let Some(child) = node.get_child(node.num_children().saturating_sub(1))
		{
			node = child;
		}

		return node;
	}

	/// Get the very last (bottom) node of the tree.
	pub fn get_last_open_node(&self, tree: &Tree<V>) -> Option<NodeIdx<V>> {
		let mut node = tree.get_root()?;

		while self.is_opened(node.idx()) && node.num_children() > 0 {
			node = self.get_last_open_node_of(node);
		}

		return Some(node.idx());
	}

	/// Get the offset of a specific node as it would be in draw order.
	fn get_offset_of_node(&self, tree: &Tree<V>, predicate: NodeIdx<V>) -> Option<usize> {
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

			if node.idx() == predicate {
				break;
			}
		}

		if position == 0 {
			return None;
		} else {
			return Some(position.saturating_sub(1));
		}
	}

	/// Scroll Right by the set stepping.
	pub fn scroll_right(&mut self) {
		self.display_offset
			.set_horizontal(self.display_offset.get_horizontal() + self.get_horizontal_scroll_step().get());
	}

	/// Scroll Left by the set stepping.
	pub fn scroll_left(&mut self) {
		self.display_offset.set_horizontal(
			self.display_offset
				.get_horizontal()
				.saturating_sub(self.get_horizontal_scroll_step().get()),
		);
	}

	/// Scroll down by 1 line, but keep the current selection within view.
	pub fn scroll_down(&mut self, tree: &Tree<V>) {
		let Some(current_pos) = self
			.selected()
			.and_then(|v| return tree.get_node(v))
			.and_then(|v| return self.get_offset_of_node(tree, v.idx()))
		else {
			// no node is selected, or it is not valid anymore
			return;
		};

		self.set_vert_offset_down_clamped(current_pos, self.get_vertical_scroll_step().get());
	}

	/// Calculate & set the display offset for downwards scroll motion.
	///
	/// Will keep the given `node_offset` within view.
	fn set_vert_offset_down_clamped(&mut self, node_offset: usize, by: usize) {
		let old_offset = self.display_offset.get_vertical();

		let new_offset = (old_offset + by).min(node_offset.saturating_sub(self.preview_distance_vertical));

		self.display_offset.set_vertical(new_offset);
	}

	/// Scroll up by 1 line, but keep the current selection within view.
	pub fn scroll_up(&mut self, tree: &Tree<V>) {
		let Some(current_pos) = self
			.selected()
			.and_then(|v| return tree.get_node(v))
			.and_then(|v| return self.get_offset_of_node(tree, v.idx()))
		else {
			// no node is selected, or it is not valid anymore
			return;
		};

		self.set_vert_offset_up_clamped(current_pos, self.get_vertical_scroll_step().get());
	}

	/// Calculate & set the display offset for upwards scroll motion.
	///
	/// Will keep the given `node_offset` within view.
	fn set_vert_offset_up_clamped(&mut self, node_offset: usize, by: usize) {
		let area = self.last_tree_size.unwrap_or_default();
		let height_as_usize = usize::from(area.height.saturating_sub(1));

		let old_offset = self.display_offset.get_vertical();

		let new_offset = old_offset
			.saturating_sub(by)
			.max(node_offset.saturating_sub(height_as_usize) + self.preview_distance_vertical);

		self.display_offset.set_vertical(new_offset);
	}

	/// Clear all state related to the tree.
	///
	/// Clears `open` list and `selected` node.
	///
	/// Does **not** reset `offset` (in case new tree needs it).
	pub fn clear(&mut self) {
		self.open.clear();
		let _ = self.selected.take();
	}
}

/// Manually implemented because [`Size`] does not currently implement [`PartialOrd`].
///
/// See <https://github.com/ratatui/ratatui/issues/2204>
#[inline]
fn size_height_gt(left: Size, right: Size) -> bool {
	return left.height > right.height;
}

#[cfg(test)]
mod tests {
	use pretty_assertions::assert_eq;
	use tuirealm::ratatui::layout::Rect;

	use crate::{
		state::{
			Offset,
			TreeViewState,
		},
		types::{
			NodeIdx,
			Tree,
		},
	};

	/// Create a mock tree which contains 124 String node data children plus the root.
	fn create_tree() -> (Tree<String>, NodeIdx<String>, Vec<NodeIdx<String>>) {
		let mut tree = Tree::default();
		let idx = tree.push_root("0".to_string());
		let mut root_mut = tree.node_mut(idx);

		let mut child_idx = Vec::with_capacity(125);
		for value in (0..124).map(|v| return v.to_string()) {
			child_idx.push(root_mut.push_child(value));
		}

		return (tree, idx, child_idx);
	}

	/// Set the root node to be selected and open.
	///
	/// Also verifies some common state before / after.
	fn set_start_assumptions(state: &mut TreeViewState<String>, tree: &Tree<String>, root_idx: NodeIdx<String>) {
		assert_eq!(state.display_offset, Offset::default());

		state.select_next_down(tree);
		assert_eq!(state.display_offset, Offset::default());
		assert_eq!(state.selected, Some(root_idx));

		// by default the root node is NOT open
		assert!(!state.is_opened(root_idx));
		state.open(root_idx);
	}

	#[test]
	fn should_not_scroll_unnecessarily() {
		let (tree, root_idx, child_idx) = create_tree();

		let mut state = TreeViewState::default();
		let view_area = Rect::new(0, 0, 10, 10);
		state.set_last_size(view_area, &tree);

		set_start_assumptions(&mut state, &tree, root_idx);

		// On default settings, it shouldnt move until area.height(10)-preview_distance(2)-root_node(1) = 7 elements in
		for idx in 0..=6 {
			state.select_next_down(&tree);
			assert_eq!(state.display_offset, Offset::default());
			assert_eq!(
				state.selected,
				Some(*child_idx.get(idx).unwrap()),
				"child_idx down {idx}"
			);
		}

		state.select_next_down(&tree);
		assert_eq!(state.display_offset, Offset::new(0, 1));
		assert_eq!(state.selected, Some(*child_idx.get(7).unwrap()), "child_idx down 7");

		state.select_next_down(&tree);
		assert_eq!(state.display_offset, Offset::new(0, 2));
		assert_eq!(state.selected, Some(*child_idx.get(8).unwrap()), "child_idx down 8");

		// next up is still within view, so no need to scroll again
		state.select_next_up(&tree);
		assert_eq!(state.display_offset, Offset::new(0, 2));
		assert_eq!(state.selected, Some(*child_idx.get(7).unwrap()), "child_idx up 7");

		for idx in (3..=6).rev() {
			eprintln!("{idx}");
			state.select_next_up(&tree);
			assert_eq!(state.display_offset, Offset::new(0, 2));
			assert_eq!(state.selected, Some(*child_idx.get(idx).unwrap()), "child_idx up {idx}");
		}

		state.select_next_up(&tree);
		assert_eq!(state.display_offset, Offset::new(0, 1));
		assert_eq!(state.selected, Some(*child_idx.get(2).unwrap()), "child_idx up 2");

		state.select_next_up(&tree);
		assert_eq!(state.display_offset, Offset::default());
		assert_eq!(state.selected, Some(*child_idx.get(1).unwrap()), "child_idx up 1");

		state.select_next_up(&tree);
		assert_eq!(state.display_offset, Offset::default());
		assert_eq!(state.selected, Some(*child_idx.first().unwrap()), "child_idx up 0");

		state.select_next_up(&tree);
		assert_eq!(state.display_offset, Offset::default());
		assert_eq!(state.selected, Some(root_idx));
	}

	#[test]
	fn should_scroll_home_end() {
		let (tree, root_idx, child_idx) = create_tree();

		let mut state = TreeViewState::default();
		let view_area = Rect::new(0, 0, 10, 10);
		state.set_last_size(view_area, &tree);

		set_start_assumptions(&mut state, &tree, root_idx);

		// nothing should change as the root element is already selected
		state.select_first(&tree);
		assert_eq!(state.display_offset, Offset::default());
		assert_eq!(state.selected, Some(root_idx));

		state.select_last(&tree);
		assert_eq!(
			state.display_offset,
			// plus the root element, plus preview distance
			Offset::new(0, child_idx.len() + 1 + 2 - view_area.height as usize)
		);
		assert_eq!(state.selected, Some(*child_idx.last().unwrap()));
	}

	#[test]
	fn should_scroll_page() {
		let (tree, root_idx, child_idx) = create_tree();

		let mut state = TreeViewState::default();
		let view_area = Rect::new(0, 0, 10, 10);
		state.set_last_size(view_area, &tree);

		set_start_assumptions(&mut state, &tree, root_idx);

		// nothing should change as the root element is already selected
		state.select_pg_up(&tree);
		assert_eq!(state.display_offset, Offset::default());
		assert_eq!(state.selected, Some(root_idx));

		// at first pg_down, while not being on the last element, it should select the last currently visible element (minus preview distance)
		state.select_pg_down(&tree);
		assert_eq!(state.display_offset, Offset::default());
		assert_eq!(
			state.selected,
			// minus the root element, minus the preview distance minus 1 as index is 0 based
			Some(*child_idx.get(view_area.height as usize - 1 - 2 - 1).unwrap())
		);

		// currently on page-up with a vertical offset of 0 and not having one of the preview distance nodes focused,
		// it will select preview_distance+1 node
		state.select_pg_up(&tree);
		assert_eq!(state.display_offset, Offset::default());
		assert_eq!(state.selected, Some(*child_idx.get(1).unwrap()));

		// though another up should focus the root node
		state.select_pg_up(&tree);
		assert_eq!(state.display_offset, Offset::default());
		assert_eq!(state.selected, Some(root_idx));

		// get back in the down state
		state.select_pg_down(&tree);

		// another pg_down while already focused on the last visible node (minus preview distance) should scroll a page
		state.select_pg_down(&tree);
		let expected_offset = Offset::new(0, view_area.height as usize - 2 - 1);
		assert_eq!(state.display_offset, expected_offset);
		assert_eq!(
			state.selected,
			// minus the root element, minus the preview distance minus 1 as index is 0 based
			Some(
				*child_idx
					.get(expected_offset.get_vertical() + view_area.height as usize - 1 - 2 - 1)
					.unwrap()
			)
		);

		for idx in 0..15 {
			state.select_pg_down(&tree);
			let expected_offset = Offset::new(0, (view_area.height as usize - 2 - 1) * (idx + 2));
			assert_eq!(state.display_offset, expected_offset);
			assert_eq!(
				state.selected,
				// minus the root element, minus the preview distance minus 1 as index is 0 based
				Some(
					*child_idx
						.get(expected_offset.get_vertical() + view_area.height as usize - 1 - 2 - 1)
						.unwrap()
				),
				"pg_down {idx}"
			);
		}

		// we should be on the last page and last element
		state.select_pg_down(&tree);
		let expected_offset = Offset::new(0, child_idx.len() + 1 - view_area.height as usize + 2);
		assert_eq!(state.display_offset, expected_offset);
		assert_eq!(state.selected, Some(*child_idx.last().unwrap()), "pg_down last");

		// another one shouldnt change anything
		state.select_pg_down(&tree);
		let expected_offset = Offset::new(0, child_idx.len() + 1 - view_area.height as usize + 2);
		assert_eq!(state.display_offset, expected_offset);
		assert_eq!(state.selected, Some(*child_idx.last().unwrap()), "pg_down last");

		state.select_pg_up(&tree);
		let expected_offset = Offset::new(0, child_idx.len() + 1 - view_area.height as usize + 2);
		assert_eq!(state.display_offset, expected_offset);
		assert_eq!(
			state.selected,
			// minus the root element, plus the preview distance
			Some(*child_idx.get(expected_offset.get_vertical() - 1 + 2).unwrap()),
			"pg_down last up 1"
		);

		state.select_pg_up(&tree);
		let expected_offset = Offset::new(0, (view_area.height as usize - 2 - 1) * 16 - 2);
		assert_eq!(state.display_offset, expected_offset);
		assert_eq!(
			state.selected,
			// minus the root element, plus the preview distance
			Some(*child_idx.get(expected_offset.get_vertical() - 1 + 2).unwrap()),
			"pg_down last up 2"
		);
	}
}
