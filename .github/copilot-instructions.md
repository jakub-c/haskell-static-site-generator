# Haskell Static Site Generator - Copilot Instructions

## Developer Context
- JavaScript developer learning Haskell through building a static site generator
- Basic FP knowledge: function composition, map/fold, immutability
- Needs to learn: monads, advanced type system features, idiomatic Haskell patterns
- Goal: Each change should be a learning experience with minimal cognitive load

## Code Standards & Patterns

### Core Principles
- **Extreme Programming mindset**: Small, incremental changes with immediate feedback
- **Type-driven development**: Let types guide implementation
- **Pure functions first**: Default to pure functions, introduce effects only when necessary
- **Composition over inheritance**: Build complex behavior from simple, composable functions

### Change Requirements
1. **Minimal cognitive load**: One concept per change (e.g., introduce Either OR maybe, not both)
2. **Idiomatic Haskell**: Follow community conventions (hlint-compliant)
3. **Educational value**: Include brief explanation of the FP principle being applied
4. **Incremental learning**: Build from simple concepts toward advanced ones

### Code Style
```haskell
-- Prefer explicit type signatures
processFile :: FilePath -> IO (Either Error Content)

-- Use meaningful names that reveal intent
parseMarkdownContent :: Text -> Either ParseError Document

-- Leverage type system for correctness
newtype SitePath = SitePath { unSitePath :: FilePath }
```

## Testing Philosophy
- **Property-based testing** for pure functions (QuickCheck)
- **Example-based tests** for complex integration scenarios
- **Golden tests** for file generation/parsing
- **Type-level testing** when using advanced types

## Learning Progression
When suggesting changes, progress through these concepts:
1. **Foundational**: Pattern matching, data types, type signatures
2. **Intermediate**: Functors, Applicatives, basic monads (Maybe, Either, IO)
3. **Advanced**: Monad transformers, lens, advanced type features

## Mentoring Style
For each suggestion, provide:
- **What**: The specific change to make
- **Why**: The FP principle or Haskell idiom being applied
- **Learn**: One key concept a Haskell student should internalize
- **Next**: Hint at the next logical learning step

Example format:
```
-- Suggestion: Use `traverse` instead of manual recursion
-- Why: `traverse` handles the common pattern of applying an effectful function to each element
-- Learn: Understanding `traverse` is key to working with containers and effects
-- Next: This prepares you for understanding Applicative functors more deeply
```

## Project Architecture Preferences
- **Layered approach**: Pure core with IO boundary
- **Pipeline architecture**: Data flows through transformation stages
- **Effect isolation**: Keep IO operations at edges, pure logic in center
- **Type safety**: Use types to prevent runtime errors (e.g., validated file paths)

## Error Handling
- Use `Either` for recoverable errors with meaningful error types
- Use `Maybe` for optional values, not missing data that indicates errors
- Create custom error types that carry context
- Fail fast with clear error messages

## Dependencies & Libraries
- Prefer **base** library functions when learning fundamentals
- Introduce **text**, **bytestring**, **filepath** for practical needs
- Add **mtl** or **transformers** when ready for monad transformers
- Use **tasty** or **hspec** for testing framework
