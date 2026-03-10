# Changelog

## next

- Add a setting to control `inactive` border style, like `tui-realm-stdlib`
- Dont draw any `Clear`s as this completely resets everything, including styles
- Apply main-style to whole given component area, and let it be overwritten by more specific styles where necessary
- Update to tuirealm `3.3.0` and stdlib `3.1.0`

## 0.2.0

- Add `widget::Indicator` to draw a single indicator
- Rename `widget::RenderIndicator` to `widget::OrIndicators`
- Change Highlight Symbol to actually be drawn (fixes #1)
  - The Highlight Symbol draw width is configurable
  - The Highlight Symbol draw behavior is configurable between "Indent only selected"(`CombineIndent`) and "indent all"(`Static`)
  - The Highlight Symbol area style is configurable (by default uses common style)
- Add ability to set style for `widget::Indicator` and `widget::OrIndicators` (fixes #2)
- Apply common style for indent area
- Add option to customize indent area style

## 0.1.0

Inital version.
