use std::sync::Arc;
use std::sync::atomic::{
	AtomicUsize,
	Ordering,
};
use std::time::Duration;
use std::{
	cell::OnceCell,
	path::PathBuf,
};

use tui_realm_stdlib::{
	Label,
	Spinner,
};
use tuirealm::command::{
	Cmd,
	Direction,
	Position,
};
use tuirealm::event::{
	Key,
	KeyEvent,
	KeyModifiers,
};
use tuirealm::props::{
	Alignment,
	BorderType,
	Borders,
	Color,
};
use tuirealm::ratatui::layout::{
	Constraint,
	Layout,
};
use tuirealm::ratatui::text::{
	Line,
	ToLine,
};
use tuirealm::terminal::{
	CrosstermTerminalAdapter,
	TerminalBridge,
};
use tuirealm::{
	Application,
	Component,
	Event,
	EventListenerCfg,
	PollStrategy,
	Update,
};
use tuirealm::{
	MockComponent,
	command::CmdResult,
};
use tuirealm_orx_tree::component::{
	CMD_PG_DOWN,
	CMD_PG_UP,
};
use tuirealm_orx_tree::{
	component::TreeView,
	types::{
		NodeValue,
		Tree,
	},
};

// The Tree component interactions

#[derive(Debug)]
pub struct FSTreeData {
	path:   PathBuf,
	as_str: OnceCell<String>,
}

impl FSTreeData {
	pub fn new(path: PathBuf) -> Self {
		// TODO: dont panic
		assert!(path.file_name().is_some());
		return Self {
			path,
			as_str: OnceCell::default(),
		};
	}
}

impl NodeValue for FSTreeData {
	fn get_text(&self) -> Line<'_> {
		return self
			.as_str
			.get_or_init(|| return self.path.file_name().unwrap().to_string_lossy().to_string())
			.to_line();
	}
}

#[derive(Debug, MockComponent)]
pub struct FileSystemTree {
	component: TreeView<FSTreeData>,
}

impl Default for FileSystemTree {
	fn default() -> Self {
		return Self::new();
	}
}

impl FileSystemTree {
	pub fn new() -> Self {
		return Self {
			component: TreeView::default(),
		};
	}

	pub fn new_with_tree(tree: Tree<FSTreeData>) -> Self {
		let component = TreeView::with_tree(tree)
			.background(Color::Reset)
			.foreground(Color::White)
			.border(
				Borders::default()
					.color(Color::LightBlue)
					.modifiers(BorderType::Rounded),
			)
			// .inactive(Style::default().fg(Color::Gray))
			.indent_size(2)
			// .scroll_step(6)
			.title(" Library ", Alignment::Left)
			.highlight_color(Color::Yellow)
			.highlight_symbol(">");

		return Self { component };
	}
}

impl Component<Msg, UserEvents> for FileSystemTree {
	fn on(&mut self, ev: tuirealm::Event<UserEvents>) -> Option<Msg> {
		let result = match ev {
			// selection
			Event::Keyboard(KeyEvent {
				code: Key::Left,
				modifiers: KeyModifiers::NONE,
			}) => self.perform(Cmd::Move(Direction::Left)),
			Event::Keyboard(KeyEvent {
				code: Key::Right,
				modifiers: KeyModifiers::NONE,
			}) => self.perform(Cmd::Move(Direction::Right)),
			Event::Keyboard(KeyEvent {
				code: Key::Down,
				modifiers: KeyModifiers::NONE,
			}) => self.perform(Cmd::Move(Direction::Down)),
			Event::Keyboard(KeyEvent {
				code: Key::Up,
				modifiers: KeyModifiers::NONE,
			}) => self.perform(Cmd::Move(Direction::Up)),

			// quick selection movement
			Event::Keyboard(KeyEvent {
				code: Key::PageDown,
				modifiers: KeyModifiers::NONE,
			}) => self.perform(Cmd::Custom(CMD_PG_DOWN)),
			Event::Keyboard(KeyEvent {
				code: Key::PageUp,
				modifiers: KeyModifiers::NONE,
			}) => self.perform(Cmd::Custom(CMD_PG_UP)),
			Event::Keyboard(KeyEvent {
				code: Key::Home,
				modifiers: KeyModifiers::NONE,
			}) => self.perform(Cmd::GoTo(Position::Begin)),
			Event::Keyboard(KeyEvent {
				code: Key::End,
				modifiers: KeyModifiers::NONE,
			}) => self.perform(Cmd::GoTo(Position::End)),

			// scroll
			// Event::Keyboard(KeyEvent {
			// 	code: Key::Left,
			// 	modifiers: KeyModifiers::SHIFT,
			// }) => self.perform(Cmd::Scroll(Direction::Left)),
			// Event::Keyboard(KeyEvent {
			// 	code: Key::Right,
			// 	modifiers: KeyModifiers::SHIFT,
			// }) => self.perform(Cmd::Scroll(Direction::Right)),

			// Event::Keyboard(KeyEvent {
			// 	code: Key::Home,
			// 	modifiers: KeyModifiers::SHIFT,
			// }) => self.perform(Cmd::Custom(CMD_SCROLL_LINE_HOME)),
			// Event::Keyboard(KeyEvent {
			// 	code: Key::End,
			// 	modifiers: KeyModifiers::SHIFT,
			// }) => self.perform(Cmd::Custom(CMD_SCROLL_LINE_END)),
			Event::Keyboard(KeyEvent {
				code: Key::Char('q'),
				modifiers: KeyModifiers::NONE,
			}) => return Some(Msg::Quit),

			_ => CmdResult::None,
		};
		match result {
			CmdResult::None => return None,
			_ => return Some(Msg::ForceRedraw),
		}
	}
}

// The boilerplate TUIRealm code

/// Display basic navigation
#[derive(MockComponent)]
struct MessageLabel {
	component: Label,
}

impl Default for MessageLabel {
	fn default() -> Self {
		return Self {
			component: Label::default()
				.text("Quit with <Q>, move with arrow key, scroll with shift arrow keys")
				.foreground(Color::White),
		};
	}
}

impl Component<Msg, UserEvents> for MessageLabel {
	fn on(&mut self, _ev: Event<UserEvents>) -> Option<Msg> {
		return None;
	}
}

/// Indicator to know if something is still loading
#[derive(Debug, Clone)]
pub struct LoadTracker {
	active: Arc<AtomicUsize>,
}

impl Default for LoadTracker {
	fn default() -> Self {
		return Self {
			active: Arc::new(AtomicUsize::new(0)),
		};
	}
}

#[allow(dead_code)]
impl LoadTracker {
	pub fn increase_one(&self) {
		self.active.fetch_add(1, Ordering::AcqRel);
	}

	pub fn decrease_one(&self) {
		self.active.fetch_sub(1, Ordering::AcqRel);
	}

	/// Should the tracker spinner be visible?
	pub fn visible(&self) -> bool {
		return self.count() > 0;
	}

	pub fn count(&self) -> usize {
		return self.active.load(Ordering::Acquire);
	}
}

/// Indicator that something is still loading
#[derive(MockComponent)]
pub struct LoadIndicator {
	component: Spinner,
}

impl Default for LoadIndicator {
	fn default() -> Self {
		return Self::new();
	}
}

impl LoadIndicator {
	pub fn new() -> Self {
		return Self {
			component: Spinner::default()
				.foreground(Color::White)
				.background(Color::Reset)
				.sequence("⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏"),
		};
	}
}

impl Component<Msg, UserEvents> for LoadIndicator {
	fn on(&mut self, _: Event<UserEvents>) -> Option<Msg> {
		return None;
	}
}

#[derive(Debug, Clone, PartialEq)]
enum Msg {
	Quit,
	ForceRedraw,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum UserEvents {}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Id {
	Tree,
	Label,
	Bottom,
}

struct Model {
	pub app:      Application<Id, Msg, UserEvents>,
	pub quit:     bool,
	pub redraw:   bool,
	pub terminal: TerminalBridge<CrosstermTerminalAdapter>,

	pub tracker: LoadTracker,
}

impl Model {
	fn new(app: Application<Id, Msg, UserEvents>, adapter: TerminalBridge<CrosstermTerminalAdapter>) -> Self {
		return Self {
			app,
			quit: false,
			redraw: true,
			terminal: adapter,
			tracker: LoadTracker::default(),
		};
	}

	fn view(&mut self) {
		self.redraw = false;

		self.terminal
			.draw(|f| {
				let [label, tree, bottom_msg] =
					Layout::vertical([Constraint::Length(1), Constraint::Fill(1), Constraint::Length(1)])
						.areas(f.area());

				self.app.view(&Id::Label, f, label);
				self.app.view(&Id::Tree, f, tree);

				if self.tracker.visible() {
					self.app.view(&Id::Bottom, f, bottom_msg);
				}
			})
			.unwrap();
	}

	fn init_terminal(&mut self) {
		let original_hook = std::panic::take_hook();
		std::panic::set_hook(Box::new(move |panic| {
			Self::hook_reset_terminal();
			original_hook(panic);
		}));
		let _drop = self.terminal.enable_raw_mode();
		let _drop = self.terminal.enter_alternate_screen();
		// required as "enter_alternate_screen" always enabled mouse-capture
		let _drop = self.terminal.disable_mouse_capture();
		let _drop = self.terminal.clear_screen();
	}

	fn hook_reset_terminal() {
		let mut terminal_clone = TerminalBridge::new_crossterm().expect("Could not initialize terminal");
		let _drop = terminal_clone.disable_raw_mode();
		let _drop = terminal_clone.leave_alternate_screen();
	}
}

impl Update<Msg> for Model {
	fn update(&mut self, msg: Option<Msg>) -> Option<Msg> {
		let msg = msg?;
		self.redraw = true;

		match msg {
			Msg::ForceRedraw => (),
			Msg::Quit => self.quit = true,
		};

		return None;
	}
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
	let event_listener = EventListenerCfg::default()
		.crossterm_input_listener(Duration::from_millis(10), 10)
		.tick_interval(Duration::from_secs(1))
		.poll_timeout(Duration::from_millis(10));

	let mut app = Application::init(event_listener);

	app.mount(Id::Label, Box::new(MessageLabel::default()), vec![])?;
	app.mount(Id::Tree, Box::new(FileSystemTree::new()), vec![])?;
	app.mount(Id::Bottom, Box::new(LoadIndicator::new()), vec![])?;

	app.active(&Id::Tree)?;

	let mut model = Model::new(app, TerminalBridge::init_crossterm()?);

	model.init_terminal();

	// inital view to draw before first event is available
	model.view();

	while !model.quit {
		let messages = model.app.tick(PollStrategy::BlockCollectUpTo(10))?;
		model.redraw = true;

		for msg in messages {
			let mut msg = Some(msg);
			while msg.is_some() {
				msg = model.update(msg);
			}
		}

		if model.redraw {
			model.view();
		}
	}

	Model::hook_reset_terminal();

	return Ok(());
}
