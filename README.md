# Mind Garden Generator in Haskell

## Reasons for Starting This Project:

* Learn Haskell and try out functional programming in the real world
* Learn LLM-driven development (or vibe coding) and prompt engineering
* Move my current mind garden generator from a JS stack to Haskell
* Get a basic understanding of Haskell tooling and testing

## Running the Code

First, update the package index to download the latest package list from Hackage:

* `cabal update`

Then run the code:

* `cabal run mind-garden`

## Testing

This project uses both unit tests and property-based tests to ensure code quality:

**Run all tests:**
* `cabal test`

**Build the project:**
* `cabal build`

The test suite includes:
- **Unit tests**: Specific examples testing known behaviors
- **Property tests**: Mathematical invariants tested across hundreds of random inputs using QuickCheck
