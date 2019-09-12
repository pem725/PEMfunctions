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

