benchmark-papers: A Tool to Execute Benchmark Papers
====================================================

This package contains a tool to support *executable benchmark papers*,
i.e., papers which contain program code (written in Curry)
to run the benchmarks shown in the paper. The tool is able
to run the benchmarks and include the results in the paper
so that the paper can be formatted with the new benchmark
results.


Examples showing the use of the system are in the
directory `examples` (see also the README there).


Some files in this package:

* `include/currycode.sty`: Definition of LaTeX macros to be included by
  `\usepackage{currycode}` into the LaTeX document containing
  Curry code to implement benchmarks.

* `src/ExecuteBenchmarkPaper.curry`: Implementation of the tool which extracts
  code snippets from a LaTeX documents, executes them, and creates a
  macro file containing the results of the code snippets to be
  included in the formatted document.

* `src/Benchmarks.curry`: Implementation of an embedded DSL to support
  the construction of benchmarks.

* `src/BenchmarkGoodies.curry`: Some goodies which might be helpful
  to format benchmark results, e.g., to translate them to LaTeX
  or produce graphics via gnuplot.
