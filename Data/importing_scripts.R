setwd('~/RTutorial/Data') #set working directory

if(!exists("read_file", mode="function")) source("foo.R")

y <- foo(2); y
