# installing openMx
# https://community.rstudio.com/t/error-in-shlib-internal-args-c-14-standard-requested-but-cxx14-is-not-defined/16819/2
# dotR <- file.path(Sys.getenv("HOME"), ".R")
# if (!file.exists(dotR)) 
#   dir.create(dotR)
# M <- file.path(dotR, "Makevars.win")
# if (!file.exists(M)) 
#   file.create(M)
# cat("\nCXX14FLAGS=-O3 -Wno-unused-variable -Wno-unused-function",
#     "CXX14 = $(BINPREF)g++ -m$(WIN) -std=c++1y",
#     "CXX11FLAGS=-O3 -Wno-unused-variable -Wno-unused-function",
#     file = M, sep = "\n", append = TRUE)
