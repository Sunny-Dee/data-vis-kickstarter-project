##Group project ##
install.packages("vcd")


library(ggplot2)
library(dplyr)
library(mosaic)
library(lubridate)
library(vcd)

## Read csv file
most.backed <- read.csv(file.choose())
head(most.backed, n=5)

##create subset
sub.df <- subset(most.backed, select = c(id, location, state, 
                                         amt.pledged.usd, goal.usd, Total.tiers, 
                                         Backers.first.5, Backers.second.5, 
                                         Backers.rest, Cluster.Membership))

head(sub.df)

##create scatter plot matrix
pairs(sub.df[, 4:8])

## put (absolute) correlations on the upper panels,
## with size proportional to the correlations.
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y, use="complete.obs"))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt, sep="")
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * (1 + r) / 2)
}

## put histograms on the diagonal
panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "white", ...)
}

##Regression lines
panel.lm <- function (x, y, col = par("col"), bg = NA, pch = par("pch"),
                      cex = 1, col.smooth = "black", ...) {
  points(x, y, pch = pch, col = col, bg = bg, cex = cex)
  abline(stats::lm(y ~ x), col = col.smooth, ...)
}

##put histograms on scatter plot
pairs(sub.df[, 4:9], pch = ".",
      upper.panel = panel.cor, 
      diag.panel = panel.hist)


## Read file
most.backed1 <- read.csv(file.choose())
head(most.backed1, n=5)


## categories
categories <- read.csv(file.choose())
top.countries <- read.csv(file.choose())
mid.countries <- read.csv(file.choose())
mosaic(~ Country + Category + amt.pledged.usd, data=mid.countries)
