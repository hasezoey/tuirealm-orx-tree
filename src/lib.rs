//! ## TUIRealm Tree via `orx-tree`
//!
//! Example usage:
//!
//! ```no_run
//! # use tuirealm::{MockComponent, Component, NoUserEvent, Event};
//! # use tuirealm::event::{
//! # 	Key,
//! # 	KeyEvent,
//! # 	KeyModifiers,
//! # };
//! # use tuirealm::command::{
//! # 	Cmd,
//! #   CmdResult,
//! # 	Direction,
//! # };
//! # use tuirealm::props::{Alignment, Color, BorderType, Borders};
//! # use std::num::NonZeroUsize;
//! #
//! # #[derive(Debug, Clone, PartialEq)]
//! # enum Msg {
//! #  ForceRedraw
//! # }
//! type TreeView = tuirealm_orx_tree::component::TreeView<String>;
//!
//! #[derive(Debug, MockComponent)]
//! struct OurComponent {
//! 	component: TreeView
//! }
//!
//! impl OurComponent {
//! 	fn new() -> Self {
//! 		Self {
//! 			component: TreeView::default()
//! 				.background(Color::Reset)
//! 				.foreground(Color::White)
//! 				.border(
//! 					Borders::default()
//! 						.color(Color::LightBlue)
//! 						.modifiers(BorderType::Rounded),
//! 				)
//! 				.indent_size(2)
//! 				.scroll_step_horizontal(NonZeroUsize::new(2).unwrap())
//! 				.empty_tree_text("Loading...")
//! 				.title(" Library ", Alignment::Left)
//! 				.highlight_color(Color::Yellow)
//! 				.highlight_symbol(">"),
//! 		}
//! 	}
//! }
//!
//! impl Component<Msg, NoUserEvent> for OurComponent {
//!		fn on(&mut self, ev: Event<NoUserEvent>) -> Option<Msg> {
//! 		let result = match ev {
//! 			// selection
//!				Event::Keyboard(KeyEvent {
//!					code: Key::Left,
//!					modifiers: KeyModifiers::NONE,
//!				}) => self.perform(Cmd::Move(Direction::Left)),
//!				Event::Keyboard(KeyEvent {
//!					code: Key::Right,
//!					modifiers: KeyModifiers::NONE,
//!				}) => self.perform(Cmd::Move(Direction::Right)),
//!				Event::Keyboard(KeyEvent {
//!					code: Key::Down,
//!					modifiers: KeyModifiers::NONE,
//!				}) => self.perform(Cmd::Move(Direction::Down)),
//!				Event::Keyboard(KeyEvent {
//!					code: Key::Up,
//!					modifiers: KeyModifiers::NONE,
//!				}) => self.perform(Cmd::Move(Direction::Up)),
//!
//! 			// etc...
//!
//! 			_ => CmdResult::None,
//! 		};
//!
//! 		match result {
//! 			CmdResult::None => None,
//! 			_ => Some(Msg::ForceRedraw)
//! 		}
//! 	}
//! }
//! ```

pub mod component;
mod props_ext;
mod state;
pub mod types;
pub mod widget;

// re-exports
pub use orx_tree::{
	NodeMut,
	NodeRef,
	Side,
	traversal,
};

// TODO: tests
