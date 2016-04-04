if (!require(descr)){
  install.packages("descr")
} 
library(descr)
options(descr.plot=FALSE)

FREQ <- function (x, w, user.missing, plot = FALSE,...) {
    xlab <- attr(x, "label", TRUE)
    if (is.null(xlab)) 
        xlab <- deparse(substitute(x))
    if (is.factor(x) == FALSE) 
        x <- as.factor(x)
    xclass <- class(x)
    if (missing(w)) 
        w <- rep(1, length(x))
    nmiss <- sum(is.na(x))
    xlevels <- levels(x)
    l <- length(xlevels)
    hasna <- FALSE
    xv <- x
    if (nmiss) {
        hasna <- TRUE
        l <- l + 1
        xlevels[l] <- "NA's"
        x <- as.numeric(x)
        x[is.na(x)] <- l
        x <- factor(x, levels = 1:l, labels = xlevels)
    }
    xfreq <- tapply(w, x, sum, na.rm = TRUE)
    xfreq[is.na(xfreq)] <- 0
    xtotal <- sum(xfreq, na.rm = TRUE)
    xperc <- 100 * xfreq/xtotal
    ftab <- cbind(xfreq, xperc)
    cnames <- c(gettext("Freqüències", domain = "R-descr"), gettext("Percentatge", 
        domain = "R-descr"))
    xvfreq <- xfreq
    if (nmiss) {
        xvfreq[xlevels == "NA's"] <- NA
    }
    if (!missing(user.missing)) {
        user.missing <- paste("^", user.missing, "$", sep = "")
        for (lev in user.missing) {
            idx <- grep(lev, xlevels)
            if (length(idx)) 
                xvfreq[idx] <- NA
        }
    }
    if (nmiss || !missing(user.missing)) {
        xvtotal <- sum(xvfreq, na.rm = TRUE)
        xvperc <- 100 * xvfreq/xvtotal
        ftab <- cbind(ftab, xvperc)
        cnames <- c(cnames, gettext("Percentatge vàlid", domain = "R-descr"))
    }
    if (xclass[1] == "ordered") {
        if (nmiss || !missing(user.missing)) {
            xxvperc <- xvperc
            xxvperc[is.na(xxvperc)] <- 0
            xvcumsum <- cumsum(xxvperc)
            xvcumsum[is.na(xvperc)] <- NA
        }
        else xvcumsum <- cumsum(xperc)
        ftab <- cbind(ftab, xvcumsum)
        cnames <- c(cnames, gettext("Percentatge acumulat", domain = "R-descr"))
    }
    total <- apply(ftab, 2, sum, na.rm = TRUE)
    if (xclass[1] == "ordered") 
        total["xvcumsum"] <- NA
    ftab <- rbind(ftab, total)
    rnames <- levels(x)
    rnames[l + 1] <- gettext("Total", domain = "R-descr")
    colnames(ftab) <- cnames
    rownames(ftab) <- rnames
    attr(ftab, "xlab") <- xlab
    class(ftab) <- c("freqtable", "matrix")
    if (nmiss || !missing(user.missing)) 
        xdata.c <- xvfreq
    else xdata.c <- xfreq
    if (length(grep("^NA's$", names(xdata.c))) > 0) 
        xdata.c["NA's"] <- NA
    xdata.c <- xdata.c[!is.na(xdata.c)]
    if (nmiss || !missing(user.missing)) 
        xdata.p <- xvperc
    else xdata.p <- xperc
    if (length(grep("^NA's$", names(xdata.p))) > 0) 
        xdata.p["NA's"] <- NA
    xdata.p <- xdata.p[!is.na(xdata.p)]
    attr(ftab, "xdata.c") <- xdata.c
    attr(ftab, "xdata.p") <- xdata.p
    if (plot == TRUE) 
        plot.freqtable(ftab, ...)
    ftab
}

elmeudesc <- function(x,w=NULL){
  min <- min(x,na.rm=T)
  q1 <- quantile(x,probs=.25,na.rm=T)
  med <- quantile(x,probs=.5,na.rm=T)
  mit <- if (is.null(w)) {mean(x,na.rm=T)} else {weighted.mean(x,w,na.rm=T)}
  des <- sd(x,na.rm=T)
  q3 <- quantile(x,probs=.75,na.rm=T)
  max <- max(x,na.rm=T)
  n <- sum(!is.na(x))
  df <- data.frame(Mínim=min,Q1=q1,Mediana=med,Mitjana=mit,Desviació=des,Q3=q3,Màxim=max,N=n)
  row.names(df) <- ""
  return(round(df,1))
}

multfreqtable = function(data, question.prefix, valor) {
  # Find the columns with the questions
  a = grep(question.prefix, names(data),perl=T)
  # Find the total number of responses
  b = sum(data[, a] == valor,na.rm=T)
  # Find the totals for each question
  d = colSums(data[, a] == valor,na.rm=T)
  # Find the number of respondents
  e = sum(apply(data[,a],1,function(x)sum(any(!is.na(x)))),na.rm=T)
  # d + b as a vector. This is your overfall frequency 
  f = as.numeric(c(d, b))
  df <- data.frame(subpregunta = c(names(d), "Total"),
                   Respostes = f,
                   percentRespostes = (f/b)*100,
                   percentN = (f/e)*100 )
  names(df) <- c("","Respostes","% Respostes","% Responents")
  return(df)
}


fertaula <- function(df,x,y,seleccio=NULL){
  tau <- prop.table(table(df[,x],df[,y]),1)*100
  if (!is.null(seleccio)) tau <- tau[,seleccio]
  return(tau)
}


cat("\n","\n",
    "Conjunt propi de funcions",
    "\n",
    "https://github.com/jmanelsg/descriptius",
    "\n",
    rep("*",15),
    "\n",
    "FREQ(x, w, user.missing, plot = FALSE,...)",
    "\n",
    "elmeudesc(x,w=NULL)",
    "\n",
    "multfreqtable(data, question.prefix, valor)",
    "\n",
    "fertaula(df,x,y,seleccio=NULL)",
    "\n","\n"
    )

