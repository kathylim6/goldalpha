Test initial prototype

  $ cat > input.txt << EOF
  > 4
  > 0 0 1
  > 0 1
  > 0 1 2
  > quit
  > EOF

  $ dune exec ../bin/main.exe < input.txt
  Hello, welcome to Sudoku! Would you like to solve a 4x4 board, a 9x9 board, or a 16x16 board? Respond with (4), (9), or (16).
  Here is your board:
  ----------
  |. 3 |. 2|
  |. . |4 3|
  ----------
  |2 . |3 .|
  |3 4 |. .|
  ----------
  
  
  Enter row, column, and value (e.g., '0 1 5') or 'quit':
  
  Updated board:
  ----------
  |1 3 |. 2|
  |. . |4 3|
  ----------
  |2 . |3 .|
  |3 4 |. .|
  ----------
  
  
  Enter row, column, and value (e.g., '0 1 5') or 'quit':
  Invalid input format! Use: row col value
  
  Enter row, column, and value (e.g., '0 1 5') or 'quit':
  Error: Cannot modify an initial cell!
  
  Enter row, column, and value (e.g., '0 1 5') or 'quit':
  Thanks for playing!
