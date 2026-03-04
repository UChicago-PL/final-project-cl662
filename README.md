# final-project-cl662

# mnk-game — Current Status (Stub)
├── README.md
├── app/
│ └── Main.hs
├── src/
│ ├── Game/
│ │ ├── Types.hs
│ │ ├── Board.hs
│ │ ├── Rules.hs
│ │ └── State.hs
│ ├── AI/
│ │ ├── Eval.hs
│ │ ├── Minimax.hs
│ │ └── SearchConfig.hs
│ ├── UI/
│ │ ├── Ansi.hs
│ │ ├── Render.hs
│ │ ├── Input.hs
│ │ └── Loop.hs
│ └── Util/
│ └── Pretty.hs
└── test/
├── Main.hs
├── GameRulesSpec.hs
├── BoardSpec.hs
└── MinimaxSpec.hs

- `src/` contains the library modules.
- `app/Main.hs` is the executable entrypoint.
- `test/Main.hs` is the test runner (hspec)