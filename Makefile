ghcid:
	ghcid --allow-eval --command="stack ghci" 

ghcid-test:
	ghcid --allow-eval --command "stack ghci sudoku:lib sudoku:test:sudoku-test --ghci-options=-fobject-code" --test "main"
