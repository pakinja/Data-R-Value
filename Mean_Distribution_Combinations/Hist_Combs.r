n <- 50
m <- 6
 

COMBINATIONS <- t(as.data.frame(combn(n,m)))

C_M <- apply(COMBINATIONS, 1, mean)

hist_all <-hist(C_M, breaks =length(unique(C_M)), col = "blue")
