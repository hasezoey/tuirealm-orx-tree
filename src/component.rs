//! Custom implementation of a tuirealm Tree View, specifically made for our usecase.

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

pub use crate::state::TreeViewState;
use crate::{
	props_ext::PropsExt,
	types::{
		Node,
		NodeValue,
		Tree,
	},
	widget::{
		DEFAULT_INDENT,
		TreeWidget,
	},
};

// --- Custom Attributes
const ATTR_INDENT: &str = "attr-indent";

// --- Custom Commands
pub const CMD_PG_UP: &str = "cmd-pg-down";
pub const CMD_PG_DOWN: &str = "cmd-pg-up";

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
	pub fn with_tree(tree: Tree<V>) -> Self {
		let mut state = TreeViewState::default();

		if let Some(root) = tree.get_root() {
			let rootidx = root.idx();
			state.open(rootidx.clone());
			state.select(Some(rootidx));
		}

		return Self {
			tree,
			state,
			..Default::default()
		};
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
	/// Default: [`DEFAULT_INDENT`](crate::widget::DEFAULT_INDENT)
	pub fn indent_size(mut self, indent: usize) -> Self {
		self.attr(Attribute::Custom(ATTR_INDENT), AttrValue::Length(indent));

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
			.get_or(Attribute::Custom(ATTR_INDENT), AttrValue::Length(DEFAULT_INDENT))
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

		frame.render_stateful_widget(widget, area, &mut self.state);
	}

	fn query(&self, attr: tuirealm::Attribute) -> Option<tuirealm::AttrValue> {
		return self.props.get(attr);
	}

	fn attr(&mut self, attr: tuirealm::Attribute, value: tuirealm::AttrValue) {
		self.props.set(attr, value);
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

	fn perform(&mut self, cmd: Cmd) -> CmdResult {
		match cmd {
			Cmd::Move(direction) => {
				match direction {
					Direction::Down => self.state.select_next_down(&self.tree),
					Direction::Left => {
						if let Some(nodeidx) = self.state.selected() {
							self.state.close(&nodeidx.clone());
						}
					},
					Direction::Right => {
						if let Some(nodeidx) = self.state.selected() {
							self.state.open(nodeidx.clone());
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
			Cmd::Custom(CMD_PG_DOWN) => {
				self.state.select_pg_down(&self.tree);
				return CmdResult::Changed(self.state());
			},
			Cmd::Custom(CMD_PG_UP) => {
				self.state.select_pg_up(&self.tree);
				return CmdResult::Changed(self.state());
			},
			Cmd::Scroll(direction) => {
				match direction {
					Direction::Down => (),
					Direction::Up => (),
					Direction::Left => self.state.scroll_left(),
					Direction::Right => self.state.scroll_right(),
				}
				return CmdResult::Changed(self.state());
			},
			// Cmd::Submit => (),
			// Cmd::Delete => (),
			// Cmd::Cancel => (),
			// Cmd::Toggle => (),
			// Cmd::Change => (),
			// Cmd::Custom(_) => (),
			_ => return CmdResult::None,
		}
	}
}
