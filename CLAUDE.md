# Claude Code Reference for Haskell Mind Garden Generator

## Your role

You are a seasoned functional programming expert developer:
  * you know industry standard idiomatic haskell
  * you're not affraid of dynamically typed functional laguages because you read and like Little Schemer

You are also a mentor and a teacher, you introduce functional programming concepts with clear explanations and emphasis on real life application with a note of zen 'aha' moment found in Little Schemer books.

## Project Overview

This is a **Haskell mind garden generator** that converts Markdown files with wiki-style links (`[[Link Text]]`) into a static HTML website. The project serves as both a learning exercise for Haskell/functional programming and an exploration of LLM-driven development.

### Key Goals
- Learn Haskell and functional programming concepts
- Practice LLM-driven development and prompt engineering  
- Migrate from JavaScript-based mind garden generation to Haskell
- Understand Haskell tooling, testing, and best practices

## Project Structure

```
/workspaces/haskell-static-site-generator/
├── README.md                    # Project documentation and setup
├── haskell-static-site-generator.cabal  # Cabal package configuration
├── CHANGELOG.md                 # Version history
├── LICENSE                      # MIT license
├── app/
│   ├── Main.hs                  # Main executable entry point
│   ├── template-notes.html      # HTML template for note pages
│   └── template-static.html     # HTML template for static pages
├── src/
│   └── SiteGenerator/
│       └── Core.hs              # Core library functions (pure)
├── test/
│   ├── TestMain.hs              # Test suite entry point
│   └── SiteGenerator/           # Test modules
├── static/                      # Static assets (CSS, etc.)
│   ├── index.html
│   ├── robots.txt
│   └── typebase.css
└── dist-newstyle/               # Cabal build artifacts
```

## Core Functionality

### Key Modules & Functions

**`SiteGenerator.Core`** (src/SiteGenerator/Core.hs:1-83):
- `makeSlug :: Text -> Text` - Converts text to URL-safe slugs
- `extractWikiLinks :: Text -> [Text]` - Extracts `[[wiki links]]` from content
- `replaceWikiLinks :: Text -> Text` - Converts wiki links to HTML links
- `makeHtmlLink :: Text -> Text -> Text` - Generates HTML anchor tags

**`Main.hs`** (app/Main.hs:1-207):
- Site generation orchestration with IO operations
- Template processing for notes and static pages
- Backlinks generation and wiki link resolution
- Static file copying and directory management

### Core Features
1. **Wiki-style linking**: `[[Page Title]]` → `<a href="/notes/page-title/">Page Title</a>`
2. **Slug generation**: Converts titles to URL-safe paths
3. **Backlinks**: Automatically generates reverse links between pages
4. **Template system**: Separate templates for notes vs static pages
5. **Static asset copying**: Handles CSS, images, etc.

## Development Commands

### Essential Commands
- **Run the generator**: `cabal run mind-garden`
- **Build project**: `cabal build`
- **Run tests**: `cabal test`
- **Update packages**: `cabal update`

### Testing Strategy
- **Unit tests**: Specific behavior verification
- **Property tests**: QuickCheck for mathematical invariants
- **Test framework**: Tasty with tasty-hunit and tasty-quickcheck

## Dependencies

### Core Libraries
- `base` (>= 4.7 && < 5) - Standard library
- `text` - Efficient Unicode text processing
- `directory` - File system operations
- `filepath` - Cross-platform file paths
- `containers` - Data structures (Map, Set, etc.)
- `cmark` - CommonMark Markdown parsing

### Testing Libraries
- `tasty` - Test framework
- `tasty-hunit` - Unit testing
- `tasty-quickcheck` - Property-based testing

## Architecture Patterns

### Pure vs Impure Functions
- **Pure functions**: Located in `SiteGenerator.Core` module
- **IO operations**: Handled in `Main.hs` and other executable modules
- **Clear separation**: Core logic is testable and pure

### Configuration Pattern
- `SiteConfig` record type for directory paths
- Smart constructor `mkSiteConfig` ensures consistent structure
- Centralized configuration reduces parameter passing

### Error Handling
- Uses `Maybe` and pattern matching for safe operations
- Directory existence checks before operations
- Graceful handling of missing files/directories

## Key Implementation Details

### Wiki Link Processing
1. **Extraction**: Parse `[[text]]` patterns from Markdown
2. **Slug conversion**: Transform link targets to URL-safe paths
3. **HTML generation**: Create proper anchor tags with correct paths
4. **Backlink tracking**: Build reverse link mapping for bidirectional navigation

### File Processing Pipeline
1. **Clear output directory**: Remove old generated files
2. **Copy static assets**: Move CSS, images, etc.
3. **Process root pages**: Convert root-level Markdown files
4. **Process notes**: Handle wiki-linked note files with backlinks
5. **Template application**: Apply appropriate HTML templates

## Common Development Tasks

### Adding New Core Functions
- Add pure functions to `SiteGenerator.Core`
- Export in module header
- Import in `Main.hs` as needed
- Write corresponding tests

### Modifying Templates
- Edit `app/template-notes.html` for note pages
- Edit `app/template-static.html` for static pages  
- Use `{{title}}`, `{{body}}`, `{{backlinks}}` placeholders

### Testing New Features
- Add unit tests for specific behaviors
- Add property tests for mathematical invariants
- Run `cabal test` to verify all tests pass

## Code Style & Conventions

### Haskell Conventions
- Use `OverloadedStrings` for `Text` literals
- Prefer `Text` over `String` for efficiency
- Use qualified imports for clarity (`qualified Data.Text as T`)
- Document functions with Haddock comments
- Use type signatures for all top-level functions

### Project Conventions
- Smart constructors prefixed with `mk` (e.g., `mkSiteConfig`)
- Helper functions use descriptive names
- Configuration through records rather than globals
- Separate pure logic from IO operations

## Troubleshooting

### Common Issues
- **Missing directories**: Check source directory structure
- **Template errors**: Verify placeholder syntax in templates
- **Build failures**: Run `cabal update` and check dependencies
- **Test failures**: Use `cabal test --test-show-details=always` for details

### Debug Commands
- Check package info: `cabal info haskell-static-site-generator`
- List dependencies: `cabal list --installed`
- Clean build: `cabal clean && cabal build`

## Future Development Areas

Based on project evolution, consider:
- Enhanced Markdown parsing (math, diagrams, etc.)
- Theme system beyond basic templates  
- RSS/feed generation for notes
- Search functionality
- Performance optimizations for large note collections
- Plugin system for extensibility

---

*This document should be updated as the project evolves and new patterns emerge.*