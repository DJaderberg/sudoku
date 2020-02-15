prod: src/*.elm
	npx elm make --optimize --output=assets/index.html src/Sudoku.elm

dev: src/*.elm
	npx elm-live src/Sudoku.elm
