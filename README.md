A second bacteria problem solution, using Scala.

Tested with Scala 2.12 on Ubuntu.

To run:

1) Compile with $ scalac Bacteria.scala
2) Run with:
  - To run with data: $ scala Bacteria < data.txt
  - Or: $ echo -e "1,1\n2,1\n3,1\nend" | scala Bacteria
  - The output of the program can be piped to it to run more generations. EG: $ echo -e "1,1\n2,1\n3,1\nend | scala Bacteria | scala Bacteria # ...
  - For a set of test cases (shell script tested on Ubuntu): $ ./runAll
