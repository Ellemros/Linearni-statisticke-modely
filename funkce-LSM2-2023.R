# Skript obsahující funkce, které používáme k vypracování DÚ z LSM2

ScheffePS1 <- function(b, A, mod, alpha){
  X <- model.matrix(mod)
  Y.hat <- predict(mod, newdata = data.frame(angle = b[2]))
  n <- dim(X)[1]
  m <- dim(A)[1]
  p <- dim(A)[2]
  M.alpha <- sqrt(m * qf(df1 = m, df2 = n - p, p = 1 - alpha))
  s <- summary(mod)$sigma
  result1 <- c(Y.hat - M.alpha * s * 
                 sqrt(t(b) %*% A %*% solve(t(X) %*% X) %*% t(A) %*% b), 
               Y.hat + M.alpha * s * 
                 sqrt(t(b) %*% A %*% solve(t(X) %*% X) %*% t(A) %*% b))
  return(result1)
}

ScheffePS2 <- function(b, A, mod, alpha) {
  X <- model.matrix(mod)
  Y.hat <- predict(mod, newdata = data.frame(foot.L = b[2]))
  n <- dim(X)[1]
  m <- dim(A)[1]
  p <- dim(A)[2]
  M.alpha <- sqrt(m * qf(df1 = m, df2 = n - p, p = 1 - alpha))
  s <- summary(mod)$sigma
  result1 <- c(Y.hat - M.alpha * s * 
                 sqrt(t(b) %*% A %*% solve(t(X) %*% X) %*% t(A) %*% b),
               Y.hat + M.alpha * s * 
                 sqrt(t(b) %*% A %*% solve(t(X) %*% X) %*% t(A) %*% b))
  return(result1)
}
