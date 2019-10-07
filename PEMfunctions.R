UniPlot <- function(x,binwidth=.5){
  require(ggplot2)
  n = length(x)
  xbar = mean(x)
  std = sd(x)
  x <- data.frame(x=x)
  
  p1 <- ggplot(x, aes(x = x, mean = xbar, sd = std, binwidth = binwidth, n = n)) +
    theme_bw() +
    geom_histogram(binwidth = binwidth, colour = "white", fill = "cornflowerblue", size = 0.1) +
    stat_function(fun = function(x) dnorm(x, mean = xbar, sd = std) * n * binwidth, color = "darkred", size = 1) +
    geom_vline(xintercept=xbar,color="darkblue")
  
  p1.build <- ggplot_build(p1)
  ymax <- max(p1.build$data[[1]]$ymax)
  xcent <- xbar + .5*(max(x$x)-xbar)
  
  p1 + geom_text(x=xcent,y=ymax,label=paste("MEAN = ",round(xbar,2))) +
    geom_text(x=xcent,y=(ymax-.1*(ymax)),label=paste("SD = ",round(std,2)))
}

ggplotRegression <- function(fit){
  require(ggplot2)
  require(ggExtra)
  p <- ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    geom_smooth() +
    labs(title = paste("Corr = ",signif(sqrt(summary(fit)$r.squared),2),
                       "Adj R2 = ",signif(summary(fit)$adj.r.squared, 3),
                       "Intercept =",signif(fit$coef[[1]],3 ),
                       " Slope =",signif(fit$coef[[2]], 3),
                       " P =",signif(summary(fit)$coef[2,4], 3)))
  ggExtra::ggMarginal(p, type="histogram", xparams = list(col="blue", fill="orange"), yparams = list(col="orange", fill="blue"))
}


lmerICCest <- function(x,facet=NULL){
  tmp <- as.data.frame(VarCorr(x))[,c("grp","vcov")]
  out <- round(tmp$vcov[!is.na(match(tmp$grp,facet))]/sum(tmp$vcov),2)
  return(out)
}

