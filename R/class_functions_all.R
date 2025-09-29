print.flm <- function(x, ...){
  hr <- round(x$time %/% 3600)
  min <- round((x$time %% 3600) %/% 60)
  sec <- round((x$time %% 3600) %% 60)
  hr <- ifelse(hr < 10, paste("0", hr, sep = ""), hr)
  min <- ifelse(min < 10, paste("0", min, sep = ""), min)
  sec <- ifelse(sec < 10, paste("0", sec, sep = ""), sec)
  cat("The number of profile factors is: ", x$npf, "\n", sep = "")
  cat("\n")
  cat("The number of runs is: ", x$nruns, "\n", sep = "")
  cat("\n")
  cat("The loss function is: ", x$criterion, "\n", sep = "")
  cat("\n")
  cat("The objective value is: ", x$objval, "\n", sep = "")
  cat("\n")
  cat("The number of iterations is: ", x$nits, "\n", sep = "")
  cat("\n")
  cat("The computing elapsed time is: ", paste(hr, ":", min, ":", sec, sep = ""), "\n", sep = "")
}

summary.flm <- function(object, ...){
  print.flm(object)
}

plot.flm <- function(x, pf = NULL, ...){
  npf <- x$npf
  if (is.null(pf)) {
    ans <- readline("Which profile factor to plot? ")
    pf <- as.numeric(unlist(strsplit(ans, ",")))
  }
  if(pf > npf | pf < 1) stop("The profile factor choice must be a value 
          between 1 and the total number of profile factors npf")
  des <- x$design[[pf]]
  dpf <- x$dx[pf]
  if (identical(dpf, as.integer(0))) {
    pltype <- "s"
  }
  knotspf <- x$knotsx[[pf]]
  t <- x$tbounds
  seqt <- seq(t[1], t[2], length.out = 100)
  xt <- mybs(seqt, degree=dpf, interior.knots=knotspf, intercept=TRUE) %*% t(des)
  yl <- x$dbounds
  yldiff <- yl[2] - yl[1]
  seqyl <- seq(yl[1], yl[2], length.out = yldiff + 1)
  n <- x$nruns
  
  for (i in 1:n) {
    if (i==1) mymain <- paste0(i,"st run")
    if (i==2) mymain <- paste0(i,"nd run")
    if (i==3) mymain <- paste0(i,"rd run")
    if (i > 3) mymain <- paste0(i,"th run")
    
    if (dpf == 0) {
      stepy <- des[i,]
      stepx  <- stepfun(knotspf, stepy, f = 0)
      plot(stepx, xlab = "time", ylab = "x(t)", main = mymain, xlim = t, 
           ylim = yl, yaxt = "n", las = 1)
      axis(2, at = seqyl, las = 1)
    } else {
      plot(seqt, xt[,i], xlab = "time", ylab = "x(t)", main = mymain, xlim = t, 
           ylim = yl, type = "l", yaxt = "n", las = 1)
      axis(2, at = seqyl, las = 1)
    }
  }
}



print.fglm <- function(x, ...){
  hr <- round(x$time %/% 3600)
  min <- round((x$time %% 3600) %/% 60)
  sec <- round((x$time %% 3600) %% 60)
  hr <- ifelse(hr < 10, paste("0", hr, sep = ""), hr)
  min <- ifelse(min < 10, paste("0", min, sep = ""), min)
  sec <- ifelse(sec < 10, paste("0", sec, sep = ""), sec)
  cat("The number of profile factors is: ", x$npf, "\n", sep = "")
  cat("\n")
  cat("The number of runs is: ", x$nruns, "\n", sep = "")
  cat("\n")
  cat("The objective criterion is: ", x$criterion, "-optimality", "\n", sep = "")
  cat("\n")
  cat("The objective value is: ", x$objval, "\n", sep = "")
  cat("\n")
  cat("The number of iterations is: ", x$nits, "\n", sep = "")
  cat("\n")
  cat("The method of approximation is: ", x$method, "\n", sep = "")
  cat("\n")
  cat("The family distribution and the link function are: ", x$family[1], " and ", x$family[2], "\n", sep = "")
  cat("\n")
  cat("The computing elapsed time is: ", paste(hr, ":", min, ":", sec, sep = ""), "\n", sep = "")
}

summary.fglm <- function(object, ...){
  print.fglm(object)
}

plot.fglm <- function(x, pf = NULL, ...){
  npf <- x$npf
  if (is.null(pf)) {
    ans <- readline("Which profile factor to plot? ")
    pf <- as.numeric(unlist(strsplit(ans, ",")))
  }
  if(pf > npf | pf < 1) stop("The profile factor choice must be a value 
          between 1 and the total number of profile factors npf")
  des <- x$design[[pf]]
  dpf <- x$dx[pf]
  if (identical(dpf, as.integer(0))) {
    pltype <- "s"
  }
  knotspf <- x$knotsx[[pf]]
  t <- x$tbounds
  seqt <- seq(t[1], t[2], length.out = 100)
  xt <- mybs(seqt, degree=dpf, interior.knots=knotspf, intercept=TRUE) %*% t(des)
  yl <- x$dbounds
  yldiff <- yl[2] - yl[1]
  seqyl <- seq(yl[1], yl[2], length.out = yldiff + 1)
  n <- x$nruns
  
  for (i in 1:n) {
    if (i==1) mymain <- paste0(i,"st run")
    if (i==2) mymain <- paste0(i,"nd run")
    if (i==3) mymain <- paste0(i,"rd run")
    if (i > 3) mymain <- paste0(i,"th run")
    
    if (dpf == 0) {
      stepy <- des[i,]
      stepx  <- stepfun(knotspf, stepy, f = 0)
      plot(stepx, xlab = "time", ylab = "x(t)", main = mymain, xlim = t, 
           ylim = yl, yaxt = "n", las = 1)
      axis(2, at = seqyl, las = 1)
    } else {
      plot(seqt, xt[,i], xlab = "time", ylab = "x(t)", main = mymain, xlim = t, 
           ylim = yl, type = "l", yaxt = "n", las = 1)
      axis(2, at = seqyl, las = 1)
    }
  }
}



