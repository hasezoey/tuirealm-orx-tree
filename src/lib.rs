//! ## TUIRealm Tree via `orx-tree`
//!
//! Example usage:
//!
//! ```no_run
//! # use tuirealm_orx_tree::component::{TreeView};
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
//! #
//! type OurTree = TreeView<String>;
//!
//! #[derive(Debug, MockComponent)]
//! struct OurComponent {
//! 	component: OurTree
//! }
//! #
//! # #[derive(Debug, Clone, PartialEq)]
//! # enum Msg {
//! #  ForceRedraw
//! # }
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

#[macro_use]
extern crate log;

pub mod component;
mod props_ext;
mod state;
pub mod types;
pub mod widget;

// re-exports
pub use orx_tree::{
	NodeMut,
	NodeRef,
};
