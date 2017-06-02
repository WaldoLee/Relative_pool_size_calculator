######################################
pool_size_cal <- function(Xc_mean, Xc_sem, Xm_mean, Xm_sem, Yc_mean, Yc_sem, Ym_mean, Ym_sem) {
  set.seed(124)
  pool_CY_set <- vector("numeric")
  pool_CY_set_mean <- vector("numeric")
  pool_CY_set_sem <- vector("numeric")
  pool_MT_set <- vector("numeric")
  pool_MT_set_mean <- vector("numeric")
  pool_MT_set_sem <- vector("numeric")
  for (i in 1:10000) {
    Xc <- rnorm(1, Xc_mean, Xc_sem)
    Xm <- rnorm(1, Xm_mean, Xm_sem)
    Yc <- rnorm(1, Yc_mean, Yc_sem)
    Ym <- rnorm(1, Ym_mean, Ym_sem)
    pool_CY <- Xc/Yc
    pool_CY_set <- c(pool_CY_set, pool_CY)
    pool_MT <- Xm/Ym
    pool_MT_set <- c(pool_MT_set, pool_MT)
  }
  pool_CY_set_mean <- mean(pool_CY_set)
  pool_CY_set_sem <- sd(pool_CY_set)
  pool_MT_set_mean <- mean(pool_MT_set)
  pool_MT_set_sem <- sd(pool_MT_set)
  print(pool_CY_set_mean)
  print(pool_CY_set_sem)
  print(pool_MT_set_mean)
  print(pool_MT_set_sem)
}
######################################
pool_size_cal(0.394458874, 0.029876027, 0.216898955, 0.00369568, 0.255882353, 0.062391775, 0.918181818, 0.115708382)
