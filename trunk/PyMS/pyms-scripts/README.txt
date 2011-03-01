-------------------------------------------------------------------------------
Scripts for use with PyMS.

Usage: $ python proc.py

Description:

1. proc-one-file-display.py

Processes a single file, and displays the peaks, ICs and TIC for that
file. Useful for initial setting of parameters to ensure good quality
peak picking

2. proc-expr.py

Batch processing of files. Files processes according to directory
structure with files within a specific folder considered to be from
the same group. Output is .expr files which can be used for alignment
by proc-algn.py, or for display by proc-display-all.py

3. proc-algn.py

Performs alignment by dynamic programming, using .expr files provided
in 'base_path'. Again directory structure should be same as
proc-expr.py, which it will be if proc-expr.py is used. Outputs two
.csv files in the base folder.

4. proc-display-all.py

Reads a data file and corresponding .expr file and displays ICs and
peaks.

5. proc-compare-ions.py

Used to compare the same Ion trace from two experiments, displays them
on screen.
