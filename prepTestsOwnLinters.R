pattern <- "^[^#\'\"]*library\\("

grep(pattern, "library(INWTutils)") # 1
grep(pattern, "  library(INWTutils)") # 1
grep(pattern, "mean(x); library(INWTutils)") # 1
grep(pattern, "mean(\\x); library(INWTutils)") # 1

grep(pattern, "#' library(INWTutils)") # 0
grep(pattern, "# plapla library(INWTutils)") # 0
grep(pattern, "plapla # library(INWTutils)") # 0
grep(pattern, "# Some other comment") # 0
grep(pattern, '"string with the word library"') # 0
grep(pattern, "'string with the word library'") # 0



# Double whitespace
pattern <- "[^#']+[^ ]+ {2,}"

grep(pattern, "x <-  3") # 1
grep(pattern, "  x <-  3") # 1
grep(pattern, "mean(x) # Comment  with double whitespace") # 1
grep(pattern, "    x <- 3 #  double") # 1
grep(pattern, "#'   \"# This is an example  document violating style conventions\",") # 1


grep(pattern, "    x <- 3") # 0
grep(pattern, "#'   \"# This is an example document violating style conventions\",") # 0
grep(pattern, "#   \"# This is an example document violating style conventions\",") # 0



