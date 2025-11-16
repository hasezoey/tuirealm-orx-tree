use orx_tree::NodeRef;

use crate::types::{
	Node,
	NodeIdx,
	NodeValue,
	Tree,
};

/// The main state-keeper
#[derive(Debug, Clone, PartialEq)]
pub struct TreeViewState<V: NodeValue> {
	selected: Option<NodeIdx<V>>,
	open:     Vec<NodeIdx<V>>,
}

impl<V> Default for TreeViewState<V>
where
	V: NodeValue,
{
	fn default() -> Self {
		return Self {
			selected: None,
			open:     Vec::default(),
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
	pub fn select(&mut self, node: Option<NodeIdx<V>>) {
		self.selected = node;
	}

	/// Get the current select value.
	pub fn selected(&self) -> Option<&NodeIdx<V>> {
		return self.selected.as_ref();
	}

	/// Get the next node downwards, where:
	/// - Base-case: select root node
	/// - If current node is not open, select next sibling downwards
	///   - If there is no next sibling, try next sibling of parent (recursive, until hitting Root node, which aborts)
	/// - If current note is open and has children, select first child
	pub fn get_next_node_down(&self, tree: &Tree<V>) -> Option<NodeIdx<V>> {
		if let Some(selected) = self.selected().and_then(|v| return tree.get_node(v)) {
			if let Some(nodeidx) = self.get_next_node_down_checked(&selected) {
				return Some(nodeidx);
			}
		}
		return Some(tree.get_root()?.idx());
	}

	/// Dont use this function directly, use [`get_next_node_down`].
	///
	/// Get the next node downwards, otherwise returning `None`, for order see [`get_next_node_down`].
	fn get_next_node_down_checked(&self, selected: &Node<'_, V>) -> Option<NodeIdx<V>> {
		// if the current node is open and has children, get the first child
		if self.is_opened(&selected.idx()) {
			if let Some(child) = selected.get_child(0) {
				return Some(child.idx());
			}
		}

		// otherwise get the next sibling
		return Self::get_next_sibling_down(selected);
	}

	/// Dont use this function directly, use [`get_next_node_down`].
	///
	/// Get the next node downwards where:
	/// - If current node is open, select first child
	/// - If current node is not open:
	///   - Select the next sibling, or
	///   - Select the next sibling of the parent (until hitting the root node, which returns `None`)
	fn get_next_sibling_down(selected: &Node<'_, V>) -> Option<NodeIdx<V>> {
		let sibling_idx = selected.sibling_idx();
		let parent = selected.parent()?;

		// If we are on the last sibling of the current node, try next silbing of parent
		if sibling_idx == parent.num_children().saturating_sub(1) {
			return Self::get_next_sibling_down(&parent);
		}

		// We are not at the last sibling yet, so get the next sibling
		return parent.get_child(sibling_idx + 1).map(|v| return v.idx());
	}

	/// Get the next node upwards, where:
	/// - Base-case: select root node
	/// - If parent has another sibling before, use that
	///   - If that sibling is open, use the last child of that (recursively)
	/// - If parent has no other sibling before, use parent
	pub fn get_next_node_up(&self, tree: &Tree<V>) -> Option<NodeIdx<V>> {
		if let Some(selected) = self.selected().and_then(|v| return tree.get_node(v)) {
			if let Some(nodeidx) = self.get_next_node_up_checked(&selected) {
				return Some(nodeidx);
			}
		}
		return Some(tree.get_root()?.idx());
	}

	/// Dont use this function directly, use [`get_next_node_up`].
	///
	/// Get the next node upwards where:
	/// - If parent has another sibling before, use that
	///   - If that sibling is open, use the last child of that (recursively)
	/// - If parent has no other sibling before, use parent
	fn get_next_node_up_checked(&self, selected: &Node<'_, V>) -> Option<NodeIdx<V>> {
		let sibling_idx = selected.sibling_idx();

		let parent = selected.parent()?;

		if sibling_idx == 0 {
			return Some(parent.idx());
		}

		if let Some(sibling) = parent.get_child(sibling_idx.saturating_sub(1)) {
			return Some(self.get_last_open_node_of(sibling).idx());
		}

		return None;
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
}
