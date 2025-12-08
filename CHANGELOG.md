# Changelog

## next

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
