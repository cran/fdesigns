## ----eval=FALSE---------------------------------------------------------------
# tbounds <- c(0, 1)
# nruns <- 4
# npf <- 1
# dx <- c(0)
# knotsx <- list(c(0.25, 0.50, 0.75))
# nx <- rep(0, npf)
# for (j in 1:npf) {
#   nx[j] <- dx[j] + length(knotsx[[j]]) + 1
# }

## ----eval=FALSE---------------------------------------------------------------
# example1a <- pflm(formula = ~ x1, nsd = 100, mc.cores = 1,
#                   npf = npf, tbounds = tbounds,
#                   nruns = nruns, dx = dx, knotsx = knotsx,
#                   pars = c("power"), db = c(1),
#                   knotsb = list(c()), criterion = "SE",
#                   lambda = 0)

## ----eval=FALSE---------------------------------------------------------------
# print(example1a)

## ----eval=FALSE---------------------------------------------------------------
# example1a$design

## ----eval=FALSE---------------------------------------------------------------
# par(mfrow = c(2,2))
# plot(example1a, pf = 1)

## ----eval=FALSE---------------------------------------------------------------
# example1b <- pflm(formula = ~ x1, nsd = 100, mc.cores = 1,
#                   npf = npf, tbounds = tbounds,
#                   nruns = nruns, dx = dx, knotsx = knotsx,
#                   pars = c("power"), db = c(2),
#                   knotsb = list(c()), criterion = "SI",
#                   lambda = 0)
#                  				
# print(example1b)                 				

## ----eval=FALSE---------------------------------------------------------------
# example2 <- pflm(formula = ~ x1, nsd = 100, mc.cores = 1,
#                  npf = 1, tbounds = c(0, 1), nruns = 4,
#                  dx = dx, knotsx = knotsx,
#                  pars = c("power"), db = c(2),
#                  knotsb = list(c()),
#                  criterion = "WSE", lambda = 10)
# 
# print(example2)

## ----eval=FALSE---------------------------------------------------------------
# tbounds <- c(0, 1)
# nruns <- 12
# npf <- 4
# dx <- c(0, 0, 0, 0)
# knotsx <- list(c(0.25, 0.50, 0.75), c(), c(), c())
# nx <- rep(0, npf)
# for (j in 1:npf) {
#   nx[j] <- dx[j] + length(knotsx[[j]]) + 1
# }

## ----eval=FALSE---------------------------------------------------------------
# indd <- list()
# startd <- list()
# dlbound <- -1
# dubound <- 1
# nsd <- 50
# for (c in 1:nsd) {
#   set.seed(c)
#   for (i in 1:npf) {
#     indd[[i]] <- matrix(runif(nruns * nx[i], dlbound, dubound),
#                     nrow = nruns, ncol = nx[i])
#     names(indd)[i] <- paste0("x", i, sep="")
#   }
#   startd[[c]] <- indd
# }

## ----eval=FALSE---------------------------------------------------------------
# example3a <- pflm(formula = ~ x1 + x2 + x3 + x4, nsd = nsd,
#                   mc.cores = 1, npf = npf, tbounds = tbounds,
#                   nruns = nruns, startd = startd, dx = dx,
#                   knotsx = knotsx,
#                   pars = c("power", "power", "power", "power"),
#                   db = c(1, 0, 0, 0),
#                   knotsb = list(c(), c(), c(), c()),
#                   criterion = "A", lambda = 0, dlbound = dlbound,
#                   dubound = dubound, tol = 0.0001)

## ----eval=FALSE---------------------------------------------------------------
# print(example3a)

## ----eval=FALSE---------------------------------------------------------------
# tbounds <- c(0, 1)
# nruns <- 8
# npf <- 1
# dx <- c(0)
# knotsx <- list(c(0.125, 0.250, 0.375, 0.500,
#                  0.625, 0.750, 0.875))
# nx <- rep(0, npf)
# for (j in 1:npf) {
#   nx[j] <- dx[j] + length(knotsx[[j]]) + 1
# }

## ----eval=FALSE---------------------------------------------------------------
# example4 <- pfglm(formula = x1, nsd = 50, mc.cores = 1,
#                   npf = npf, tbounds = tbounds,
#                   nruns = nruns, dx = dx, knotsx = knotsx,
#                   pars = c("power"), db = c(1),
#                   knotsb = list(c()), criterion = "A",
#                   family = binomial, method = c("quadrature"),
#                   level = NULL, B = NULL,
#                   prior = list(mu = c(0), sigma2 = c(1)),
#                   dlbound = -1, dubound = 1)

## ----eval=FALSE---------------------------------------------------------------
# print(example4)

## ----eval=FALSE---------------------------------------------------------------
# tbounds <- c(0, 1)
# nruns <- 8
# npf <- 1
# dx <- c(1)
# knotsx <- list(c(0.20, 0.40, 0.60, 0.80))
# nx <- rep(0, npf)
# for (j in 1:npf) {
#   nx[j] <- dx[j] + length(knotsx[[j]]) + 1
# }

## ----eval=FALSE---------------------------------------------------------------
# set.seed(100)
# prmc <- function(B, Q){
#   matrix(rnorm(B * Q, mean = 0, sd = sqrt(2)), nrow = B, ncol = Q)
# }

## ----eval=FALSE---------------------------------------------------------------
# example5 <- pfglm(formula = ~ 1 + x1 + P(x1, 2), nsd = 1, mc.cores = 1,
#                   npf = 1, tbounds = tbounds, nruns = nruns,
#                   startd = NULL, dx = dx, knotsx = knotsx,
#                   pars = c("bspline", "bspline"), db = c(1, 1),
#                   knotsb = list(c(0.5), c(0.5)), lambda = 0,
#                   criterion = "D", family = poisson, method = c("MC"),
#                   level = NULL, B = 10000, prior = prmc, tol = 0.01)

