use std::num::NonZeroUsize;
use std::path::Path;
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

use orx_tree::traversal::OverNode;
use orx_tree::{
	Dfs,
	NodeRef,
	Traverser,
};
use tokio::sync::mpsc::{
	UnboundedReceiver,
	UnboundedSender,
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
use tuirealm::listener::{
	ListenerResult,
	PollAsync,
};
use tuirealm::props::{
	Alignment,
	BorderType,
	Borders,
	Color,
	Style,
};
use tuirealm::ratatui::buffer::Buffer;
use tuirealm::ratatui::layout::{
	Constraint,
	Layout,
	Rect,
};
use tuirealm::ratatui::widgets::{
	Clear,
	Widget,
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
	Sub,
	SubClause,
	SubEventClause,
	Update,
};
use tuirealm::{
	MockComponent,
	command::CmdResult,
};
use tuirealm_orx_tree::component::cmd;
use tuirealm_orx_tree::types::{
	MotionDirection,
	NodeIdx,
};
use tuirealm_orx_tree::widget::{
	CHILD_INDICATOR_LENGTH,
	RenderIndicator,
	calc_area_for_value,
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
struct FSTreeData {
	/// The actual path of the node.
	path:       PathBuf,
	/// Store whether that path is a dir to show indicators & use for prefetching
	is_dir:     bool,
	/// Indicator if the we already send a request to fetch this directory
	is_loading: bool,
	/// The `path.file_name`'s string representation.
	///
	/// Lazily evaluated from `path`, only when it becomes necessary.
	// TODO: evaluate if it would be more performant to only cache if `path.file_name().to_str_lossy()` returns `Cow::Owned`.
	as_str: OnceCell<String>,
}

impl FSTreeData {
	pub fn new(path: PathBuf, is_dir: bool) -> Self {
		assert!(path.is_absolute());
		let cell = OnceCell::new();
		// Due to our expectation of the path not ending in `..`, we can assume
		// that there is always a file_name, EXCEPT on linux on the root ("/").
		// We *could* call `canonicalize` here again, but it is more likely the caller already has done that.
		if path.file_name().is_none() {
			let _ = cell.set("/".to_string());
		}

		return Self {
			path,
			is_dir,
			is_loading: false,
			as_str: OnceCell::default(),
		};
	}
}

/// Indicator when for directories when we already issued a load for it (and not have gotten a response back yet).
///
/// It should look like "⟳".
const LOADING_SYMBOL: &str = "\u{27F3}";

impl NodeValue for FSTreeData {
	fn render(&self, buf: &mut Buffer, area: Rect, offset: usize, style: Style) {
		// Unwrap should never panic here as we already check the case of there not being a file_name on instance creation.
		// The *only* possible way to currently get this panic is when using the default instance (which shouldnt be used).
		let res = self
			.as_str
			.get_or_init(|| return self.path.file_name().unwrap().to_string_lossy().to_string());

		NodeValue::render(res, buf, area, offset, style);
	}

	fn render_with_indicators(
		&self,
		buf: &mut Buffer,
		mut area: Rect,
		mut offset: usize,
		style: Style,
		_is_leaf: bool,
		is_opened: impl FnOnce() -> bool,
	) {
		if !self.is_dir {
			// not a directory

			// indent leaf nodes by what is taken up on the parent by the indicators, otherwise children and the parent would have the same visible indent
			let leaf_indent = CHILD_INDICATOR_LENGTH;
			let indent_area = calc_area_for_value(&mut offset, &mut area, usize::from(leaf_indent));
			Clear.render(indent_area, buf);
		} else if !self.is_loading {
			// directory that is not loading
			RenderIndicator::default().render(&mut offset, &mut area, buf, is_opened());
		} else {
			// directory that is loading
			RenderIndicator::new(LOADING_SYMBOL, "", 2).render(&mut offset, &mut area, buf, true);
		}

		self.render(buf, area, offset, style);
	}
}

#[derive(Debug, MockComponent)]
struct FileSystemTree {
	component: TreeView<FSTreeData>,

	tx_to_main:      TxToMain,
	loading_tracker: LoadTracker,
}

impl FileSystemTree {
	fn new_tree() -> TreeView<FSTreeData> {
		return TreeView::default()
			.background(Color::Reset)
			.foreground(Color::White)
			.border(
				Borders::default()
					.color(Color::LightBlue)
					.modifiers(BorderType::Rounded),
			)
			.indent_size(2)
			.scroll_step_horizontal(NonZeroUsize::new(2).unwrap())
			.empty_tree_text("Loading...")
			.title(" Filesystem ", Alignment::Left)
			.highlight_color(Color::Yellow)
			.highlight_symbol(">");
	}

	pub fn new(tx_to_main: TxToMain, tracker: LoadTracker) -> Self {
		return Self {
			component: Self::new_tree(),
			tx_to_main,
			loading_tracker: tracker,
		};
	}

	#[expect(unused)]
	pub fn new_with_tree(tree: Tree<FSTreeData>, tx_to_main: TxToMain, tracker: LoadTracker) -> Self {
		let component = Self::new_tree().tree(tree);

		return Self {
			component,
			tx_to_main,
			loading_tracker: tracker,
		};
	}

	/// Handle Left key.
	///
	/// This does one of:
	/// - close the currently selected node
	/// - if node is closed, move to parent node
	fn handle_left_key(&mut self) -> Option<Msg> {
		let selected_node = self.component.get_current_selected_node()?;

		if !selected_node.data().is_dir || !self.component.get_state().is_opened(selected_node.idx()) {
			// When the selected node is a file or a closed directory, move focus to upper directory
			self.perform(Cmd::Custom(tuirealm_orx_tree::component::cmd::SELECT_PARENT));
		} else {
			// Directory is selected, but still open, close it
			// "Direction::Left" closes the current node
			self.component.perform(Cmd::Move(Direction::Left));
		}

		return Some(Msg::ForceRedraw);
	}

	/// Handle Right key.
	///
	/// This does one of:
	/// - if directory, try to load directory, if not loaded
	/// - if directory and is loaded, open currently selected node
	fn handle_right_key(&mut self) -> Option<Msg> {
		let selected_node = self.component.get_current_selected_node()?;

		if selected_node.data().is_dir {
			if selected_node.num_children() > 0 {
				// Current node has children loaded, just open it.

				// "Direction::Right" opens the current node
				self.perform(Cmd::Move(Direction::Right));

				return Some(Msg::ForceRedraw);
			} else if !selected_node.data().is_loading {
				// Current node does not have any children and is not loading, trigger a load for it
				self.handle_reload_at(LIReloadPathData {
					path:         selected_node.data().path.clone(),
					change_focus: true,
				});
				return Some(Msg::ForceRedraw);
			} else {
				// Current node does not have any children is is loading, dont do anything
				return None;
			}
		} else {
			return None;
		}
	}

	/// Truncate `node`'s path to `root_node`'s path, then split `node`'s path by the separator, iterate over the non-empty components.
	///
	/// This assumes `node` contains `root_node`!
	fn split_components_root<'a>(root_node: &Path, node: &'a Path) -> impl Iterator<Item = std::path::Component<'a>> {
		return node.components().skip(root_node.components().count());
	}

	/// Handle reloading of the given path, potentially without changing root, but also change focus.
	///
	/// If necessary, load all paths in-between.
	fn handle_reload_at(&mut self, data: LIReloadPathData) {
		let path = data.path;
		let Some(root_node) = self.component.get_tree().get_root() else {
			// No root node, not reloading!
			return;
		};

		if !path.starts_with(&root_node.data().path) {
			// Given path is outside of tree root, not loading!
			return;
		}

		// because of the if above, we know the node is at least within the tree
		// so it is safe to use the root as the initial starting node.

		// this contains one of 3:
		// - the path of the node itself
		// - the root node's path
		// - the nearest directory node's path
		let mut nearest_path = &root_node.data().path;
		let mut nearest_idx = root_node.idx();
		let mut nearest_match = 0;

		let components_between_root_and_path: Vec<std::path::Component<'_>> =
			Self::split_components_root(&root_node.data().path, &path).collect();

		let mut traverser = Dfs::<OverNode>::new();
		// inital tree walker
		let walker = root_node.walk_with(&mut traverser);

		for node in walker {
			// exact match found, no need to further iterate
			if node.data().path == path {
				nearest_path = &node.data().path;
				nearest_idx = node.idx();
				break;
			}

			// The parent directory node will always contain the wanted path partially
			// skip everything else.
			// Otherwise it might decend into "root/to_delete/another" instead of wanted "root/dir/another".
			if !path.starts_with(&node.data().path) {
				continue;
			}

			for (idx, comp) in Self::split_components_root(&root_node.data().path, &node.data().path).enumerate() {
				let Some(gotten) = components_between_root_and_path.get(idx) else {
					break;
				};

				if *gotten == comp && idx > nearest_match {
					nearest_match = idx;
					nearest_path = &node.data().path;
					nearest_idx = node.idx();
				}
			}
		}

		let nearest_path = nearest_path.clone();

		let depth = components_between_root_and_path.len().saturating_sub(nearest_match);

		let focus_node = if data.change_focus { Some(path) } else { None };

		// unwrap is safe as we literally just gotten the idx from the tree
		// set current node to loading, to indicate such to the user
		self.component.get_node_mut(nearest_idx).unwrap().data_mut().is_loading = true;

		self.trigger_subload_with_focus(nearest_path, depth, focus_node);
	}

	/// Trigger a load for the given path, with the given depth.
	///
	/// This will send a [`UserEvents::TreeNodeReadySub`] and does not change the root, unless the
	/// given path *is* the root.
	fn trigger_subload_with_focus(&self, path: PathBuf, depth: usize, focus_node: Option<PathBuf>) {
		let tx = self.tx_to_main.clone();
		fs_scan_cb(self.loading_tracker.clone(), path, depth, move |vec| {
			let _ = tx.send(UserEvents::TreeNodeReadySub(LINodeReadySub { vec, focus_node }));
		});
	}

	/// Handle a full reload / potential change of the current tree root.
	///
	/// Also changes focus, if requested.
	fn handle_full_reload(&mut self, data: LIReloadData) -> Option<Msg> {
		let Some(path) = data
			.change_root_path
			.or_else(|| return self.get_root_path().map(Path::to_path_buf))
		else {
			// "No \"change_root_path\" and no current root, not reloading!"
			return None;
		};
		let focus_node = data
			.focus_node
			.or_else(|| return self.get_selected_path().map(Path::to_path_buf));

		self.component.clear_tree();

		self.trigger_load_with_focus(path, focus_node);

		return Some(Msg::ForceRedraw);
	}

	/// Trigger a load with a message to change the tree root to the given path.
	///
	/// This will make the current tree root be the new focused node.
	///
	/// This will send a [`UserEvents::TreeNodeReady`] and change the root to `path`.
	fn trigger_load_with_focus<P: Into<PathBuf>>(&self, scan_path: P, focus_node: Option<PathBuf>) {
		let path = scan_path.into();
		fs_scan(
			self.loading_tracker.clone(),
			path,
			2,
			self.tx_to_main.clone(),
			focus_node,
		);
	}

	/// Get the current root node's path, if there is one.
	fn get_root_path(&self) -> Option<&Path> {
		return self
			.component
			.get_tree()
			.get_root()
			.map(|v| return v.data().path.as_path());
	}

	/// Get the current selected node's path, if there is one.
	fn get_selected_path(&self) -> Option<&Path> {
		return self
			.component
			.get_current_selected_node()
			.map(|v| return v.data().path.as_path());
	}

	/// Get the [`NodeIdx`] of a given [`Path`], searches from current tree root.
	fn get_idx_of_path(&self, path: &Path) -> Option<NodeIdx<FSTreeData>> {
		let root_node = self.component.get_tree().get_root()?;

		let mut traverser = Dfs::<OverNode>::new();
		// inital tree walker
		let mut walker = root_node.walk_with(&mut traverser);

		return walker.find(|v| return v.data().path == path).map(|v| return v.idx());
	}

	/// Select, open all parents and open the given node.
	fn select_and_open_node(&mut self, idx: NodeIdx<FSTreeData>) {
		self.component.select(MotionDirection::Upwards, idx);
		self.component.open_all_parents(idx);
		// always open the selected node
		self.handle_right_key();
	}

	/// Apply the given data as the root of the tree, resetting the state of the tree.
	///
	/// This will always replace the root of the tree.
	#[expect(unsafe_code)]
	fn handle_ready(&mut self, data: LINodeReady) -> Msg {
		let vec = data.vec;
		let initial_node = data.focus_node;

		let initial_node = initial_node.or_else(|| return self.get_selected_path().map(Path::to_path_buf));

		let (_, tree) = recvec_to_tree(vec);

		self.component.clear_tree();
		// SAFETY: everything is already invalidated and cleared.
		*unsafe { self.component.get_tree_mut() } = tree;

		if let Some(initial_node) = initial_node {
			let idx = self.get_idx_of_path(&initial_node);
			if let Some(idx) = idx {
				self.select_and_open_node(idx);
			} else {
				// requested node is not within the tree, lets try to find the next nearest parent
				let mut remaining_path = initial_node.as_path();
				while let Some(parent) = remaining_path.parent()
					&& parent.starts_with(&self.component.get_tree().root().data().path)
				{
					if let Some(idx) = self.get_idx_of_path(parent) {
						self.select_and_open_node(idx);
						break;
					} else {
						remaining_path = parent;
					}
				}
			}
		} else {
			// always select the root node
			self.component.perform(Cmd::Move(Direction::Down));
			// always open the root node
			self.component.perform(Cmd::Move(Direction::Right));
		}

		return Msg::ForceRedraw;
	}

	/// Apply the given data at the path the data is, potentially without changing root.
	///
	/// This will replace the root if the given data is starting at the root path.
	#[expect(unsafe_code)]
	fn handle_ready_sub(&mut self, data: LINodeReadySub) -> Option<Msg> {
		let vec = data.vec;

		// let tree_mut = self.component.tree_mut().root_mut();
		let Some(root_path) = self.get_root_path() else {
			// TODO: should we apply it?
			// "No root path, not applying"
			return None;
		};

		if !vec.path.exists() {
			// Path does not exist anymore, for example after a paste / delete
			if let Some(idx) = self.get_idx_of_path(&vec.path) {
				// Unwrap is safe because we literally just searched it.
				self.component.get_node_mut(idx).unwrap().prune();
			}
		} else if root_path == vec.path {
			// the given data *is* the root, so we have to replace the whole tree
			self.component.clear_tree();
			// SAFETY: everything is already invalidated and cleared.
			*unsafe { self.component.get_tree_mut() } = recvec_to_tree(vec).1;

			// always select the root node
			self.component.perform(Cmd::Move(Direction::Down));
			// always open the root node
			self.component.perform(Cmd::Move(Direction::Right));
		} else {
			let Some(found_node_idx) = self.get_idx_of_path(&vec.path) else {
				//	"Ready node ({}) not found in tree ({})!",
				//	vec.path.display(),
				//	self.component.get_tree().root().data().path.display()
				return None;
			};

			// try to set a initially selected node
			if self.component.get_current_selected_node().is_none() {
				self.component.perform(Cmd::GoTo(Position::Begin));
			}

			// re-set focus to the removed node, if it had focus
			let is_node_selected = self.component.get_state().selected() == Some(found_node_idx);

			// Unwrap is safe, as we literally just searched the tree for this node
			let mut node_mut = self.component.get_node_mut(found_node_idx).unwrap();

			// TODO: consider using "NodeMut::replace" once available
			// see <https://github.com/orxfun/orx-tree/issues/189>
			let new_idx = node_mut.push_sibling_tree(tuirealm_orx_tree::Side::Left, recvec_to_tree(vec).1);
			node_mut.prune();
			// NOTE: we dont need to re-set "is_loading" as the full node gets overwritten with new data, which defaults to "false"

			if let Some(focus_node) = data.focus_node {
				let idx = self.get_idx_of_path(&focus_node);
				if let Some(idx) = idx {
					self.select_and_open_node(idx);
				} else {
					// requested node is not within the tree, lets try to find the next nearest parent
					let mut remaining_path = focus_node.as_path();
					while let Some(parent) = remaining_path.parent()
						&& parent.starts_with(&self.component.get_tree().root().data().path)
					{
						if let Some(idx) = self.get_idx_of_path(parent) {
							self.select_and_open_node(idx);
							break;
						} else {
							remaining_path = parent;
						}
					}
				}
			} else if is_node_selected {
				self.component.select(MotionDirection::NoMotion, new_idx);
			}

			// TODO: call tree changed?
		}

		return Some(Msg::ForceRedraw);
	}

	/// Handle all custom messages.
	#[expect(unreachable_patterns)]
	fn handle_user_events(&mut self, ev: UserEvents) -> Option<Msg> {
		// handle subscriptions
		return match ev {
			UserEvents::TreeNodeReady(data) => Some(self.handle_ready(data)),
			UserEvents::TreeNodeReadySub(data) => self.handle_ready_sub(data),
			_ => None,
		};
	}
}

impl Component<Msg, UserEvents> for FileSystemTree {
	fn on(&mut self, ev: tuirealm::Event<UserEvents>) -> Option<Msg> {
		if let Event::User(ev) = ev {
			return self.handle_user_events(ev);
		}

		let result = match ev {
			// selection
			Event::Keyboard(KeyEvent {
				code: Key::Left,
				modifiers: KeyModifiers::NONE,
			}) => match self.handle_left_key() {
				Some(msg) => return Some(msg),
				None => CmdResult::None,
			},
			Event::Keyboard(KeyEvent {
				code: Key::Right,
				modifiers: KeyModifiers::NONE,
			}) => match self.handle_right_key() {
				Some(msg) => return Some(msg),
				None => CmdResult::None,
			},
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
			}) => self.perform(Cmd::Custom(cmd::PG_DOWN)),
			Event::Keyboard(KeyEvent {
				code: Key::PageUp,
				modifiers: KeyModifiers::NONE,
			}) => self.perform(Cmd::Custom(cmd::PG_UP)),
			Event::Keyboard(KeyEvent {
				code: Key::Home,
				modifiers: KeyModifiers::NONE,
			}) => self.perform(Cmd::GoTo(Position::Begin)),
			Event::Keyboard(KeyEvent {
				code: Key::End,
				modifiers: KeyModifiers::NONE,
			}) => self.perform(Cmd::GoTo(Position::End)),

			// scroll
			Event::Keyboard(KeyEvent {
				code: Key::Left,
				modifiers: KeyModifiers::SHIFT,
			}) => self.perform(Cmd::Scroll(Direction::Left)),
			Event::Keyboard(KeyEvent {
				code: Key::Right,
				modifiers: KeyModifiers::SHIFT,
			}) => self.perform(Cmd::Scroll(Direction::Right)),
			Event::Keyboard(KeyEvent {
				code: Key::Down,
				modifiers: KeyModifiers::SHIFT,
			}) => self.perform(Cmd::Scroll(Direction::Down)),
			Event::Keyboard(KeyEvent {
				code: Key::Up,
				modifiers: KeyModifiers::SHIFT,
			}) => self.perform(Cmd::Scroll(Direction::Up)),

			// load more tree
			Event::Keyboard(KeyEvent {
				code: Key::Backspace,
				modifiers: KeyModifiers::NONE,
			}) => {
				if let Some(current_root) = self.get_root_path() {
					let parent = current_root.parent().unwrap_or(current_root);

					// only trigger a load if we are not at the root of the filesystem already
					if current_root != parent {
						self.trigger_load_with_focus(parent, Some(current_root.to_path_buf()));

						// to draw the load spinner
						return Some(Msg::ForceRedraw);
					}
				}

				// there is no special indicator or message; the download_tracker should force a draw once active
				CmdResult::None
			},
			Event::Keyboard(KeyEvent {
				code: Key::Enter,
				modifiers: KeyModifiers::NONE,
			}) => {
				if let Some(selected_node) = self.get_selected_path()
					&& selected_node.is_dir()
				{
					self.trigger_load_with_focus(selected_node, None);

					// to draw the load spinner
					return Some(Msg::ForceRedraw);
				}

				CmdResult::None
			},
			Event::Keyboard(KeyEvent {
				code: Key::Function(5),
				modifiers: KeyModifiers::NONE,
			}) => {
				self.handle_full_reload(LIReloadData {
					change_root_path: None,
					focus_node:       None,
				});

				return Some(Msg::ForceRedraw);
			},

			// Event::Keyboard(KeyEvent {
			// 	code: Key::Home,
			// 	modifiers: KeyModifiers::SHIFT,
			// }) => self.perform(Cmd::Custom(cmd::SCROLL_LINE_HOME)),
			// Event::Keyboard(KeyEvent {
			// 	code: Key::End,
			// 	modifiers: KeyModifiers::SHIFT,
			// }) => self.perform(Cmd::Custom(cmd::SCROLL_LINE_END)),
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

/// Execute a FS scan on a different thread.
///
/// Executes [`fs_dir_tree`] on a different thread and calls `cb` on finish.
fn fs_scan_cb<P: Into<PathBuf>, F>(tracker: LoadTracker, path: P, depth: usize, cb: F)
where
	F: FnOnce(RecVec) + Send + 'static,
{
	let path = path.into();
	std::thread::Builder::new()
		.name("FS tree scan".to_string())
		.spawn(move || {
			tracker.increase_one();
			let vec = fs_dir_tree(&path, depth);

			cb(vec);
			tracker.decrease_one();
		})
		.expect("Failed to spawn thread");
}

/// Execute a FS scan on a different thread.
///
/// Executes [`fs_dir_tree`] on a different thread and send a [`UserEvents::TreeNodeReady`] on finish
fn fs_scan<P: Into<PathBuf>>(tracker: LoadTracker, path: P, depth: usize, tx: TxToMain, focus_node: Option<PathBuf>) {
	fs_scan_cb(tracker, path, depth, move |vec| {
		let _ = tx.send(UserEvents::TreeNodeReady(LINodeReady { vec, focus_node }));
	});
}

/// Scan the given `path` for up to `depth`, and return a [`Node`](tuirealm_orx_tree::types::Node) tree.
///
/// Note: consider using [`fs_scan`] instead of this directly for running in a different thread.
#[inline]
pub fn fs_dir_tree(path: &Path, depth: usize) -> RecVec {
	return fs_dir_tree_inner(path, depth, None);
}

/// Scan the given `path` for up to `depth`, and return a [`Node`](tuirealm_orx_tree::types::Node) tree.
///
/// Note: consider using [`fs_scan`] instead of this directly for running in a different thread.
fn fs_dir_tree_inner(path: &Path, depth: usize, is_dir: Option<bool>) -> RecVec {
	let is_dir = is_dir.unwrap_or_else(|| return path.is_dir());
	let mut node = RecVec {
		path: path.to_path_buf(),
		is_dir,
		children: Vec::new(),
	};

	if depth > 0
		&& path.is_dir()
		&& let Ok(paths) = std::fs::read_dir(path)
	{
		let mut paths: Vec<(String, (PathBuf, bool))> = paths
			.filter_map(std::result::Result::ok)
			// filter out hidden files
			.filter(|p| return !p.file_name().to_string_lossy().starts_with('.'))
			.map(|v| {
				let sort_str = v.file_name().to_string_lossy().to_string();
				let is_dir = v.file_type().is_ok_and(|v| return v.is_dir());
				let path = v.path();
				return (sort_str, (path, is_dir));
			})
			.collect();

		// order directories before other, then byte compare (which is practically only works for ASCII)
		paths.sort_by(|a, b| return a.1.1.cmp(&b.1.1).then(a.0.cmp(&b.0)));

		for (_sort_str, (path, is_dir)) in paths {
			node.children.push(fs_dir_tree_inner(&path, depth - 1, Some(is_dir)));
		}
	}
	return node;
}

/// Convert a [`RecVec`] to a [`Node`](tuirealm_orx_tree::types::Node).
///
/// Returns the root nodeidx.
fn recvec_to_tree(vec: RecVec) -> (NodeIdx<FSTreeData>, Tree<FSTreeData>) {
	let mut tree = Tree::default();

	return (recvec_to_node_rec(vec, None, &mut tree), tree);
}

/// Convert the given `vec` to be child on `parent_node`.
///
/// If `parent_node` is `None` the new node will be pushed as the root.
fn recvec_to_node_rec(
	vec: RecVec,
	parent_node: Option<NodeIdx<FSTreeData>>,
	tree: &mut Tree<FSTreeData>,
) -> NodeIdx<FSTreeData> {
	let is_dir = vec.path.is_dir();
	let nodeidx = if let Some(idx) = parent_node {
		tree.get_node_mut(idx)
			.unwrap()
			.push_child(FSTreeData::new(vec.path, is_dir))
	} else {
		tree.push_root(FSTreeData::new(vec.path, is_dir))
	};

	for val in vec.children {
		recvec_to_node_rec(val, Some(nodeidx), tree);
	}

	return nodeidx;
}

/// Get all subscriptions for the [`FileSystemTree`] Component.
fn fs_subs() -> Vec<Sub<Id, UserEvents>> {
	return vec![
		Sub::new(
			SubEventClause::User(UserEvents::TreeNodeReady(LINodeReady::default())),
			SubClause::Always,
		),
		Sub::new(
			SubEventClause::User(UserEvents::TreeNodeReadySub(LINodeReadySub::default())),
			SubClause::Always,
		),
	];
}

// The boilerplate TUIRealm code

type TxToMain = UnboundedSender<UserEvents>;

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
		return LoadIndicator {
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

#[derive(Debug)]
struct PortRxMain(UnboundedReceiver<UserEvents>);

impl PortRxMain {
	pub fn new(rx_to_main: UnboundedReceiver<UserEvents>) -> Self {
		return Self(rx_to_main);
	}
}

#[tuirealm::async_trait]
#[allow(clippy::implicit_return)] // for some reason clippy wants to add "return" before "async"
impl PollAsync<UserEvents> for PortRxMain {
	async fn poll(&mut self) -> ListenerResult<Option<Event<UserEvents>>> {
		return match self.0.recv().await {
			Some(ev) => Ok(Some(Event::User(ev))),
			None => Ok(None),
		};
	}
}

#[derive(Debug, Clone, PartialEq)]
enum Msg {
	Quit,
	ForceRedraw,
}

/// Data for a Reload.
///
/// Leaving `change_root_path` as `None` will reload the current root, without changing paths.
#[derive(Clone, Debug, Eq, Default)]
pub struct LIReloadData {
	pub change_root_path: Option<PathBuf>,
	pub focus_node:       Option<PathBuf>,
}

/// `PartialEq` is only used for subscriptions.
impl PartialEq for LIReloadData {
	fn eq(&self, _other: &Self) -> bool {
		return true;
	}
}

/// Recursive structure which may contain more of itself.
///
/// This exists to send a (sub)tree across messages, without having the tree itself require [`Default`], [`Clone`] or [`Eq`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RecVec {
	pub path:     PathBuf,
	pub is_dir:   bool,
	pub children: Vec<RecVec>,
}

/// Data for a Reload At.
///
/// The path given is the one that is reloaded and also focused.
#[derive(Clone, Debug, Eq, Default)]
pub struct LIReloadPathData {
	pub path:         PathBuf,
	pub change_focus: bool,
}

/// `PartialEq` is only used for subscriptions.
impl PartialEq for LIReloadPathData {
	fn eq(&self, _other: &Self) -> bool {
		return true;
	}
}

/// Data for [`UserEvents::TreeNodeReady`].
///
/// The path given is the one that is reloaded and also optionally focused.
#[derive(Clone, Debug, Eq)]
pub struct LINodeReady {
	pub vec:        RecVec,
	pub focus_node: Option<PathBuf>,
}

/// Data returned from this should not be passed around.
impl Default for LINodeReady {
	fn default() -> Self {
		let bogus_recvec = RecVec {
			path:     PathBuf::new(),
			is_dir:   false,
			children: Vec::new(),
		};
		return Self {
			vec:        bogus_recvec,
			focus_node: None,
		};
	}
}

/// `PartialEq` is only used for subscriptions.
impl PartialEq for LINodeReady {
	fn eq(&self, _other: &Self) -> bool {
		return true;
	}
}

/// Data for [`UserEvents::TreeNodeReadySub`].
///
/// The path given is the one that is reloaded and also focused.
#[derive(Clone, Debug, Eq)]
pub struct LINodeReadySub {
	pub vec:        RecVec,
	pub focus_node: Option<PathBuf>,
}

/// Data returned from this should not be passed around.
impl Default for LINodeReadySub {
	fn default() -> Self {
		let bogus_recvec = RecVec {
			path:     PathBuf::new(),
			is_dir:   false,
			children: Vec::new(),
		};
		return Self {
			vec:        bogus_recvec,
			focus_node: None,
		};
	}
}

/// `PartialEq` is only used for subscriptions.
impl PartialEq for LINodeReadySub {
	fn eq(&self, _other: &Self) -> bool {
		return true;
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum UserEvents {
	/// A requested node is ready from loading.
	///
	/// Replaces the tree root.
	TreeNodeReady(LINodeReady),
	/// A requested node is ready to be reloaded within the current tree.
	///
	/// Does not replace the tree root.
	TreeNodeReadySub(LINodeReadySub),
}

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
	fn new(
		app: Application<Id, Msg, UserEvents>,
		adapter: TerminalBridge<CrosstermTerminalAdapter>,
		tracker: LoadTracker,
	) -> Self {
		return Self {
			app,
			quit: false,
			redraw: true,
			terminal: adapter,
			tracker,
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
	return actual_main();
}

#[tokio::main]
async fn actual_main() -> Result<(), Box<dyn std::error::Error>> {
	let (tx, rx) = tokio::sync::mpsc::unbounded_channel();

	let event_listener = EventListenerCfg::default()
		.with_handle(tokio::runtime::Handle::current())
		.async_crossterm_input_listener(Duration::ZERO, 5)
		.tick_interval(Duration::from_secs(1))
		.async_tick(true)
		.add_async_port(Box::new(PortRxMain::new(rx)), Duration::ZERO, 2);

	let mut app = Application::init(event_listener);
	let load_tracker = LoadTracker::default();

	app.mount(Id::Label, Box::new(MessageLabel::default()), vec![])?;
	app.mount(
		Id::Tree,
		Box::new(FileSystemTree::new(tx.clone(), load_tracker.clone())),
		// those are theoretically not necessary due it being the *only* focused component, but still for demo completeness
		fs_subs(),
	)?;
	app.mount(Id::Bottom, Box::new(LoadIndicator::default()), vec![])?;

	app.active(&Id::Tree)?;

	let mut model = Model::new(app, TerminalBridge::init_crossterm()?, load_tracker.clone());

	model.init_terminal();

	// trigger initial load of the tree
	let cwd = std::env::current_dir()?;
	fs_scan(load_tracker, cwd, 2, tx, None);

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
