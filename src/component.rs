//! Custom implementation of a tuirealm Tree View, specifically made for our usecase.

use std::num::NonZeroUsize;

use orx_tree::NodeRef;
use tuirealm::{
	AttrValue,
	Attribute,
	MockComponent,
	Props,
	State,
	StateValue,
	command::{
		Cmd,
		CmdResult,
		Direction,
		Position,
	},
	props::{
		Alignment,
		Borders,
		Color,
		Style,
	},
	ratatui::layout::Rect,
};

pub use crate::state::{
	PREVIEW_DISTANCE_DEFAULT,
	TreeViewState,
};
use crate::{
	props_ext::PropsExt,
	types::{
		Node,
		NodeIdx,
		NodeMut,
		NodeValue,
		Tree,
	},
	widget::{
		DEFAULT_INDENT,
		TreeWidget,
	},
};

/// Custom attributes for [`Attribute::Custom`].
pub mod attr {
	/// Attribute to control the indent width (`* depth`)
	pub const INDENT: &str = "indent";
	/// Attribute to control the message to display on a empty tree
	pub const EMPTY_TREE: &str = "empty-tree-text";
	/// Attribute to control the horizontal scroll stepping
	pub const HORIZ_SCROLL_STEP: &str = "horiz-scroll-step";
	/// Attribute to control the vertical scroll stepping
	pub const VERT_SCROLL_STEP: &str = "vert-scroll-step";
}

/// Custom commands for [`Cmd::Custom`] (/ [`TreeView::perform`]).
pub mod cmd {
	/// Command to run a "Page Down" selection
	pub const PG_UP: &str = "pg-down";
	/// Command to run a "Page Up" selection
	pub const PG_DOWN: &str = "pg-up";
}

/// The TUIRealm TreeView component
#[derive(Debug, Clone)]
#[must_use]
pub struct TreeView<V: NodeValue> {
	props: Props,
	tree:  Tree<V>,
	state: TreeViewState<V>,
}

impl<V> Default for TreeView<V>
where
	V: NodeValue,
{
	fn default() -> Self {
		return Self {
			props: Props::default(),
			tree:  Tree::default(),
			state: TreeViewState::default(),
		};
	}
}

impl<V> TreeView<V>
where
	V: NodeValue,
{
	/// Create a new instance with a initial tree.
	pub fn new_tree(tree: Tree<V>) -> Self {
		let this = Self::default().tree(tree);

		return this;
	}

	/// Set the tree for this TreeView.
	pub fn tree(mut self, tree: Tree<V>) -> Self {
		self.tree = tree;

		if let Some(root) = self.tree.get_root() {
			let rootidx = root.idx();
			self.state.open(rootidx);
			self.state.select(Some(rootidx));
		}

		return self;
	}

	/// Get the currently selected node, if there is one and is valid.
	fn get_selected_node(&self) -> Option<Node<'_, V>> {
		let idx = self.state.selected()?;

		return self.tree.get_node(idx);
	}

	/// Set the main foreground color for the tree.
	pub fn foreground(mut self, color: Color) -> Self {
		self.attr(Attribute::Foreground, AttrValue::Color(color));

		return self;
	}

	/// Set the main background color for the tree.
	pub fn background(mut self, color: Color) -> Self {
		self.attr(Attribute::Background, AttrValue::Color(color));

		return self;
	}

	/// Set the border style and color that surrounds the tree.
	pub fn border(mut self, border: Borders) -> Self {
		self.attr(Attribute::Borders, AttrValue::Borders(border));

		return self;
	}

	/// Set a custom indent size.
	///
	/// Default: [`DEFAULT_INDENT`]
	pub fn indent_size(mut self, indent: usize) -> Self {
		self.attr(Attribute::Custom(attr::INDENT), AttrValue::Length(indent));

		return self;
	}

	/// Set a title for the tree in the border.
	pub fn title<S: Into<String>>(mut self, title: S, align: Alignment) -> Self {
		self.attr(Attribute::Title, AttrValue::Title((title.into(), align)));

		return self;
	}

	/// Set the current curser selection color.
	pub fn highlight_color(mut self, color: Color) -> Self {
		self.attr(Attribute::HighlightedColor, AttrValue::Color(color));

		return self;
	}

	/// Set the current curser selection symbol.
	pub fn highlight_symbol<S: Into<String>>(mut self, val: S) -> Self {
		self.attr(Attribute::HighlightedStr, AttrValue::String(val.into()));

		return self;
	}

	/// Set custom text for when a tree is empty.
	///
	/// This can be used to set `Loading...` for example.
	/// If this text is not applicable anymore, it can be changed via [`.attr`](Self::attr), or add a root node,
	/// whichever is more applicable.
	///
	/// Default: [`DEFAULT_EMPTY_TREE_TEXT`](crate::widget::DEFAULT_EMPTY_TREE_TEXT)
	pub fn empty_tree_text<S: Into<String>>(mut self, val: S) -> Self {
		self.attr(Attribute::Custom(attr::EMPTY_TREE), AttrValue::String(val.into()));

		return self;
	}

	/// Set a custom horizontal scroll step.
	///
	/// This only applies to scrolling via [`Cmd::Scroll`].
	///
	/// Default: `1`
	pub fn scroll_step_horizontal(mut self, stepping: NonZeroUsize) -> Self {
		self.state.set_horizontal_scroll_step(stepping);

		return self;
	}

	/// Set a custom vertical scroll step.
	///
	/// This only applies to scrolling via [`Cmd::Scroll`].
	///
	/// Default: `1`
	pub fn scroll_step_vertical(mut self, stepping: NonZeroUsize) -> Self {
		self.state.set_vertical_scroll_step(stepping);

		return self;
	}

	/// Set a custom vertical preview distance.
	///
	/// This value keeps at least `distance` amount of element before / after the selection on scroll.
	/// Note that this distance is reduced when there is not enough area to display.
	///
	/// Default: [`PREVIEW_DISTANCE_DEFAULT`]
	pub fn preview_distance_vertical(mut self, distance: u16) -> Self {
		self.state.set_preview_distance_horizontal(distance);

		return self;
	}

	/// Get the currently selected node, if there is one and it still being valid.
	///
	/// This can be used as a [`Cmd::Submit`] substitute.
	pub fn get_current_selected_node(&self) -> Option<Node<'_, V>> {
		return self.state.selected().and_then(|v| return self.tree.get_node(v));
	}

	/// Get the current tree.
	pub fn get_tree(&self) -> &Tree<V> {
		return &self.tree;
	}

	/// Get the current tree mutably.
	/// Instead of this function, other functions like [`get_node_mut`] should be used instead.
	///
	/// # Safety
	///
	/// Running any function which invalidates indices will break the selection & open nodes.
	pub unsafe fn get_tree_mut(&mut self) -> &mut Tree<V> {
		return &mut self.tree;
	}

	/// Get the node for the given index.
	///
	/// A node is only returned if:
	/// - the node still exists in the tree
	/// - the idx is for a node in the *current* tree
	/// - no memory changes have happened and the node is still valid.
	///
	/// See [`NodeIdx`] documentation.
	pub fn get_node(&self, idx: &NodeIdx<V>) -> Option<Node<'_, V>> {
		return self.tree.get_node(idx);
	}

	/// Get the node for the given index mutably.
	///
	/// A node is only returned if:
	/// - the node still exists in the tree
	/// - the idx is for a node in the *current* tree
	/// - no memory changes have happened and the node is still valid.
	///
	/// See [`NodeIdx`] documentation.
	///
	///
	pub fn get_node_mut(&mut self, idx: &NodeIdx<V>) -> Option<NodeMut<'_, V>> {
		return self.tree.get_node_mut(idx);
	}
}

impl<V> MockComponent for TreeView<V>
where
	V: NodeValue,
{
	fn view(&mut self, frame: &mut tuirealm::Frame<'_>, area: Rect) {
		if !self.props.should_display() {
			return;
		}

		let foreground = self
			.props
			.get_or(Attribute::Foreground, AttrValue::Color(Color::Reset))
			.unwrap_color();
		let background = self
			.props
			.get_or(Attribute::Background, AttrValue::Color(Color::Reset))
			.unwrap_color();

		let title = tui_realm_stdlib::utils::get_title_or_center(&self.props);
		let empty_tree_text = self
			.props
			.get_ref(Attribute::Custom(attr::EMPTY_TREE))
			.and_then(AttrValue::as_string);

		let borders = self
			.props
			.get_or(Attribute::Borders, AttrValue::Borders(Borders::default()))
			.unwrap_borders();
		let focus = self
			.props
			.get_or(Attribute::Focus, AttrValue::Flag(false))
			.unwrap_flag();
		let inactive_style = self
			.props
			.get(Attribute::FocusStyle)
			.map(tuirealm::AttrValue::unwrap_style);
		let hg_color = self
			.props
			.get_or(Attribute::HighlightedColor, AttrValue::Color(foreground))
			.unwrap_color();
		let hg_str = self
			.props
			.get_ref(Attribute::HighlightedStr)
			.and_then(AttrValue::as_string);
		// dont have the highlight be too disrupting while not focused
		let hg_style = if focus {
			// TODO: consider making this more configurable
			Style::default().bg(hg_color).fg(Color::Black)
		} else {
			Style::default().fg(hg_color)
		};

		let indent = self
			.props
			.get_or(Attribute::Custom(attr::INDENT), AttrValue::Length(DEFAULT_INDENT))
			.unwrap_length();

		let block = tui_realm_stdlib::utils::get_block(borders, Some(&title), focus, inactive_style);

		// let block_inner_area = block.inner(area);

		let style = Style::default().fg(foreground).bg(background);

		let mut widget = TreeWidget::new(&self.tree)
			.block(block)
			.style(style)
			.hg_style(hg_style)
			.indent(indent);

		if let Some(hg_str) = hg_str {
			widget = widget.hg_str(hg_str);
		}
		if let Some(empty_tree_text) = empty_tree_text {
			widget = widget.empty_tree_text(empty_tree_text);
		}

		frame.render_stateful_widget(widget, area, &mut self.state);
	}

	fn query(&self, attr: tuirealm::Attribute) -> Option<tuirealm::AttrValue> {
		return match attr {
			Attribute::Custom(attr::HORIZ_SCROLL_STEP) => {
				return Some(AttrValue::Length(self.state.get_horizontal_scroll_step().get()));
			},
			Attribute::Custom(attr::VERT_SCROLL_STEP) => {
				return Some(AttrValue::Length(self.state.get_vertical_scroll_step().get()));
			},
			_ => self.props.get(attr),
		};
	}

	fn attr(&mut self, attr: tuirealm::Attribute, value: tuirealm::AttrValue) {
		match attr {
			Attribute::Custom(attr::HORIZ_SCROLL_STEP) => {
				let val = NonZeroUsize::new(value.unwrap_length()).unwrap();
				self.state.set_horizontal_scroll_step(val);
			},
			Attribute::Custom(attr::VERT_SCROLL_STEP) => {
				let val = NonZeroUsize::new(value.unwrap_length()).unwrap();
				self.state.set_vertical_scroll_step(val);
			},
			_ => self.props.set(attr, value),
		};
	}

	fn state(&self) -> tuirealm::State {
		// TODO: state values are not accurate or represent much other than "has something been selected"
		// maybe we can update it if "Any" becomes stable <https://github.com/veeso/tui-realm/pull/120>

		// get the actual node value to see if it is still valid within the tree
		if let Some(_node) = self.get_selected_node() {
			return State::One(StateValue::Bool(true));
		} else {
			return State::None;
		}
	}

	/// This Component implements the following Commands:
	/// - [`Cmd::Move`]:
	///   - [`Direction::Down`] & [`Direction::Up`]: change selection in that direction, if possible
	///   - [`Direction::Left`] & [`Direction::Right`]: open / close the current node, if possible
	/// - [`Cmd::Toggle`]: open / close the current node, if possible
	/// - [`Cmd::GoTo`]:
	///   - [`Position::Begin`]: change selection to be the root node
	///   - [`Position::End`]: change selection to be the last child of the last open node
	///   - [`Position::At`]: unimplemented as `usize` does not translate well to the tree
	/// - [`Cmd::Scroll`]:
	///   - [`Direction::Down`] & [`Direction::Up`]: scroll down / up without moving the selection; always keeps the selection within view
	///   - [`Direction::Left`] & [`Direction::Right`]: scroll left / right (scroll is unbounded)
	/// - [`Cmd::Custom`]:
	///   - [`cmd::PG_DOWN`]: change selection to be one page down (based on last known draw height)
	///   - [`cmd::PG_UP`]: change selection to be one page up (based on last known draw height)
	///
	/// Note that [`Cmd::Submit`] and [`Cmd::Delete`] are **NOT** implemented and need to be done manually (ex via [`get_current_selected_node`](Self::get_current_selected_node) on submit action).
	fn perform(&mut self, cmd: Cmd) -> CmdResult {
		match cmd {
			Cmd::Move(direction) => {
				match direction {
					Direction::Down => self.state.select_next_down(&self.tree),
					Direction::Left => {
						if let Some(nodeidx) = self.state.selected().copied() {
							self.state.close(&nodeidx);
						}
					},
					Direction::Right => {
						if let Some(nodeidx) = self.state.selected() {
							self.state.open(*nodeidx);
						}
					},
					Direction::Up => self.state.select_next_up(&self.tree),
				}
				return CmdResult::Changed(self.state());
			},
			Cmd::GoTo(position) => {
				match position {
					Position::Begin => self.state.select_first(&self.tree),
					Position::End => self.state.select_last(&self.tree),
					// tree does not have convenient usize indexing, so we ignore it for now
					Position::At(_) => (),
				};

				return CmdResult::Changed(self.state());
			},
			Cmd::Custom(cmd::PG_DOWN) => {
				self.state.select_pg_down(&self.tree);
				return CmdResult::Changed(self.state());
			},
			Cmd::Custom(cmd::PG_UP) => {
				self.state.select_pg_up(&self.tree);
				return CmdResult::Changed(self.state());
			},
			Cmd::Scroll(direction) => {
				match direction {
					Direction::Down => self.state.scroll_down(&self.tree),
					Direction::Up => self.state.scroll_up(&self.tree),
					Direction::Left => self.state.scroll_left(),
					Direction::Right => self.state.scroll_right(),
				}
				return CmdResult::Changed(self.state());
			},
			Cmd::Toggle => {
				if let Some(nodeidx) = self.state.selected().copied() {
					if self.state.is_opened(&nodeidx) {
						self.state.close(&nodeidx);
					} else {
						self.state.open(nodeidx);
					}

					return CmdResult::Changed(self.state());
				}

				return CmdResult::None;
			},
			// Cmd::Cancel => (),
			// Cmd::Toggle => (),
			// Cmd::Change => (),
			_ => return CmdResult::None,
			// explicitly unimplemented
			// Cmd::Submit => (),
			// Cmd::Delete => (),
		}
	}
}
