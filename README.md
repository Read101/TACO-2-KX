# Taco2KX – TACO to KX Converter

<p align="center">
  <img src="images/Logo.png" alt="TACO to KX Logo">
</p>

## GUI Preview

<p align="center">
  <img src="images/screenshots.jpg" alt="App Screenshot">
</p>

## What is TACO to KX Converter?

**Taco2KX** A Windows desktop application that converts TACO (Tactical Overlay) marker files to the KX format. This tool streamlines the process of building custom JSON files for teleport locations for the KX tool.

### Features

- **TACO File Processing**: Drag-and-drop support for .taco archive files containing XML marker data
- **Coordinate Conversion**: Automatically converts TACO coordinates to KX format
- **Interactive Preview**: Visual list view showing converted coordinates with location information
- **Search & Filter**: Real-time filtering for both file lists and map data
- **JSON Export**: Export converted markers to JSON format for use with other tools (KX + OKTW)
- **String Replace**: Batch rename preview items using old/new pattern inputs
- **State Persistence**: Window/splitter positions and edit values are restored on launch.

## What’s New Since The Old Source

- **Refactored data vs UI logic**: parsing helpers moved into `Units/uDataHelpers.pas` and list view helpers into `Units/uUIHelpers.pas`.
- **Safer list view handling**: ownership of `ListItem.Data` is centralised; search lists store safe data and have cleanup helpers.
- **JSON export hardened**: built using `System.JSON`, with pretty formatting and safe numeric parsing.
- **Parsing stability**: guarded XML parsing, safer string extraction, and locale‐aware number conversions.
- **Memory leak fixes**: `TZipFile` and temporary `TStringList` objects are properly freed.
- **Progress responsiveness**: long operations periodically pump the message loop to keep UI responsive.
- **State persistence**: window size/position, splitters, tabs, and edit fields are saved to `Taco2KXLocation.ini`.
- **Codebase cleanup**: unused units removed, more consistent helper usage, and inline documentation added.

## Dependencies

- **None** beyond standard Delphi VCL.

## Changelog (Highlights)

- **UI state persistence**: window size/position, splitters, tabs, and edit fields saved to `Taco2KXLocation.ini`.
- **Safer list handling**: list view item ownership is centralised, and search lists are cleaned safely.
- **JSON export improvements**: `System.JSON` usage with pretty formatting and safe numeric parsing.
- **Parsing stability**: guarded XML parsing and locale-aware numeric conversion.
