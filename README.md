# TUIRealm Tree Component via `orx-tree`

[![Build status](https://github.com/hasezoey/tuirealm-orx-tree/actions/workflows/tests.yml/badge.svg)](https://github.com/hasezoey/tuirealm-orx-tree/actions)
[![crates.io](https://img.shields.io/crates/v/tuirealm-orx-tree.svg)](https://crates.io/crates/tuirealm-orx-tree)
[![dependency status](https://deps.rs/repo/github/hasezoey/tuirealm-orx-tree/status.svg)](https://deps.rs/repo/github/hasezoey/tuirealm-orx-tree)
[![MSRV](https://img.shields.io/badge/MSRV-1.88.0-blue)](https://releases.rs/docs/1.88.0/)

A Tree Component for [`tuirealm`](https://crates.io/crates/tuirealm) via [`orx-tree`](https://crates.io/crates/orx-tree).

## MSRV & Policy

The current MSRV is `1.88`.

The MSRV Policy is to base it on the highest required by `tuirealm` and `orx-tree`.

## Vs `tui-realm-treeview`

The Biggest most differential difference is the backing tree implementation. [`tui-realm-treeview`](https://crates.io/crates/tui-realm-treeview) uses [`orange-trees`](https://crates.io/crates/orange-trees/), which is key-value based, and access is done via id(which is just find) or find, whereas `tuirealm-orx-tree` uses [`orx-tree`](https://crates.io/crates/orx-tree), which only has values (to the user) and access is based on NodeIdx(pointers) or find.

Other differniating factors as of writing are:

- In `tuirealm-orx-tree`, node value is responsible for drawing its data
  - This makes it possible to customize indicators (open/closed/loading/error, etc)
- Thanks to `orx-tree`, the tree / node API more versatile
  - Especially not having the need to walk through the entire tree for getting a single node (NodeIdx are pointers)
- `tui-realm-treeview` did not have a built-in way to display a message when the tree is empty (ex. still loading)
- `tui-realm-treeview` did not have a way to properly handle Page-Up / Page-Down, instead only had fixed "scroll steps" (regardless of display size)

Most of my issues with `tui-realm-treeview` can be found in [this issue](https://github.com/veeso/tui-realm-treeview/issues/19).

## Usage

Documentation can be found in [docs.rs](https://docs.rs/tuirealm-orx-tree/latest/).

A full demo can be found in [examples/filesystem](examples/filesystem.rs).

The most basic points:

```rust
type TreeView = tuirealm_orx_tree::component::TreeView<String>;

#[derive(Debug, MockComponent)]
struct OurComponent {
  component: TreeView
}

impl OurComponent {
    fn new() -> Self {
        Self {
            component: TreeView::default()
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
                .title(" Library ", Alignment::Left)
                .highlight_color(Color::Yellow)
                .highlight_symbol(">"),
        }
    }
}

impl Component<Msg, NoUserEvent> for OurComponent {
    fn on(&mut self, ev: Event<NoUserEvent>) -> Option<Msg> {
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
            // etc...
            _ => CmdResult::None,
        };
        match result {
            CmdResult::None => None,
            _ => Some(Msg::ForceRedraw)
        }
    }
}
```
