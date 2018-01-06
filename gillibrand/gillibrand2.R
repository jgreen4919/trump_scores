# Script for "It Sure Looks Like Kirsten Gillibrand is Running for President"
# Published with Sean McElwee at: https://www.washingtonpost.com/news/monkey-cage/wp/2018/01/01/it-sure-looks-as-if-kirsten-gillibrand-is-running-for-president/
# Jon Green

library(ggplot2)
library(MASS)
library(cowplot)
library(car)

set.seed(12345)

# Read in the data
sen <- read.csv("~/Desktop/gillibrand/gillibrand_pub/S115_members_sean_17.csv", stringsAsFactors = FALSE)

# Basic descriptives
hist(sen$TRUMP.SCORE)
hist(sen$TRUMP.MARGIN)
hist(sen$TRUMP.PLUS.MINUS)
hist(sen$dim1)

# Subset down to the Democratic caucus
dems <- sen[sen$party_code %in% c(100,328),]

# numeric measure of state median income
dems$minc <- as.numeric(
  paste(substr(dems$Median.Income..Household...2016., start = 2, stop = 3),
        substr(dems$Median.Income..Household...2016., start = 5, stop = 7),
        sep = "")
)
dems$cycles <- (6-(2018 - dems$Class))/2 # Variable for how many cycles left until up for re-election

# Margin of victory in most recent election
dems$winmarg <- with(dems,
                     Last.Election.D- Last.Election.R)

# Standardize numeric variables of interest
stands <- data.frame(
  scale(
    subset(dems,
           select = c(dim1, dim2, 
                      winmarg,
                      minc,
                      Bachelor.s.Degree.or.Higher.Among.Age.25...2016.,
                      White..2010.,
                      Latino..2010.,
                      Black..2010.,
                      TRUMP.SCORE, 
                      TRUMP.MARGIN, 
                      TRUMP.PLUS.MINUS
           ))))
names(stands) <- c("std.dim1", "std.dim2",
                   "std.winmarg",
                   "std.minc", "std.ed",
                   "std.white", "std.latino",
                   "std.black",
                   "std.TRUMP.SCORE", 
                   "std.TRUMP.MARGIN", 
                   "std.TRUMP.PLUS.MINUS"
)
dems <- data.frame(cbind(dems, stands)) # Append standardized variables

# Visualize bivariate relationship
dwtm.mod <- summary(lm(dim1 ~ TRUMP.MARGIN, data = dems))
dwtm <- ggplot(dems, aes(x = TRUMP.MARGIN, y = dim1, col = (dems$Last.Name == "Gillibrand")))
ggdraw() +
  draw_plot(
    dwtm + 
      geom_text(aes(label=Last.Name))+
      geom_smooth(method = "lm")+
      scale_color_manual(name = "Senator",
                         breaks = c(FALSE, TRUE),
                         values = c("black","blue"),
                         labels = c("Others","Gillibrand"))+
      xlim(c(-35, 45))+
      annotate("text", x = 20, y = -.58, label = paste("R^2 = ", round(dwtm.mod$r.squared, 3), sep = ""))+
      xlab("Trump Margin")+
      ylab("DW-NOMINATE First Dimension")+
      ggtitle("Gillibrand's career voting record is in line with her state")+
      theme_bw(), 0, 0, 1, 1)+
  draw_label("Created by: Jon Green", 
             x = .78, y = .02, size = 8, alpha = 1)

tstm.mod <- summary(lm(TRUMP.SCORE ~ TRUMP.MARGIN, data = dems))
tstm <- ggplot(dems, aes(x = TRUMP.MARGIN, y = TRUMP.SCORE, col = (dems$Last.Name == "Gillibrand")))
ggdraw() +
  draw_plot(
    tstm + 
      geom_text(aes(label=Last.Name))+
      geom_smooth(method = "lm")+
      scale_color_manual(name = "Senator",
                         breaks = c(FALSE, TRUE),
                         values = c("black","blue"),
                         labels = c("Others","Gillibrand"))+
      xlim(c(-35, 45))+
      annotate("text", x = 20, y = .2, label = paste("R^2 = ", round(tstm.mod$r.squared, 3), sep = ""))+
      xlab("Trump Margin")+
      ylab("Trump Score")+
      ggtitle("Gillibrand's anti-Trump voting record is not in line with her state")+
      theme_bw(), 0, 0, 1, 1)+
  draw_label("Created by: Jon Green", 
             x = .78, y = .02, size = 8, alpha = 1)

d1ts.mod <- summary(lm(TRUMP.SCORE ~ dim1, data = dems))
d1ts.mod2 <- summary(lm(TRUMP.SCORE ~ dim1, data = dems[-which(dems$Last.Name == "Gillibrand"),]))
d1ts <- ggplot(dems, aes(x = dim1, y = TRUMP.SCORE, col = (dems$Last.Name == "Gillibrand")))
ggdraw() +
  draw_plot(
    d1ts + 
      geom_text(aes(label=Last.Name))+
      geom_smooth(method = "lm")+
      scale_color_manual(name = "Senator",
                         breaks = c(FALSE, TRUE),
                         values = c("black","blue"),
                         labels = c("Others","Gillibrand"))+
      xlim(c(-.8, -.05))+
      annotate("text", x = -.68, y = .45, label = paste("R^2 with Gillibrand: ", round(d1ts.mod$r.squared, 3), sep = ""))+
      annotate("text", x = -.668, y = .42, label = paste("R^2 without Gillibrand: ", round(d1ts.mod2$r.squared, 3), sep = ""))+
      xlab("DW-NOMINATE First Dimension")+
      ylab("Trump Score")+
      ggtitle("Gillibrand is more anti-Trump than her career voting record predicts")+
      theme_bw(), 0, 0, 1, 1)+
  draw_label("Created by: Jon Green", 
             x = .78, y = .02, size = 8, alpha = 1)

d1ts2 <- ggplot(dems[-which(dems$Last.Name == "Gillibrand"),], aes(x = dim1, y = TRUMP.SCORE))
ggdraw() +
  draw_plot(
    d1ts2 + 
      geom_text(aes(label=Last.Name))+
      geom_smooth(method = "lm")+
      xlim(c(-.8, -.05))+
      annotate("text", x = -.2, y = .1, label = paste("R^2 = ", round(d1ts.mod2$r.squared, 3), sep = ""))+
      xlab("DW-NOMINATE First Dimension")+
      ylab("Trump Score")+
      ggtitle("The relationship between DW-NOMINATE and Trump Score is improved without Gillibrand")+
      theme_bw(), 0, 0, 1, 1)+
  draw_label("Created by: Jon Green", 
             x = .78, y = .02, size = 8, alpha = 1)

# OLS
predscore <- lm(TRUMP.SCORE ~ std.dim1 + std.TRUMP.MARGIN,
                data = dems)
summary(predscore)

predscore.us <- lm(TRUMP.SCORE ~ dim1 + TRUMP.MARGIN,
                   data = dems)
psus <- summary(predscore.us)

# cluster standard errors by state
cl   <- function(dat,fm, cluster){
  require(sandwich, quietly = TRUE)
  require(lmtest, quietly = TRUE)
  M <- length(unique(cluster))
  N <- length(cluster)
  K <- fm$rank
  dfc <- (M/(M-1))*((N-1)/(N-K))
  uj  <- apply(estfun(fm),2, function(x) tapply(x, cluster, sum));
  vcovCL <- dfc*sandwich(fm, meat=crossprod(uj)/N)
  coeftest(fm, vcovCL) }
predscore.clust <- cl(fm = predscore, dat = dems,
                      cluster = dems$state_abbrev)

# regression diagnostics
sresid <- studres(predscore) 
hist(sresid, freq=FALSE, 
     main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40) 
yfit<-dnorm(xfit) 
lines(xfit, yfit)

ncvTest(predscore)

vif(predscore) # variance inflation factors 
sqrt(vif(predscore)) > 2

crPlots(predscore)
# Ceres plots 
ceresPlots(predscore)

durbinWatsonTest(predscore)

# Attach residuals to dataset
dems$resids <- predscore$residuals
dems$std.resids <- scale(dems$resids)
dems$resids.us <- predscore.us$residuals

# Plot residuals against quantities of interest
resplot.d1 <- ggplot(dems, aes(x = dim1, y = std.resids))
ggdraw() +
  draw_plot(
    resplot.d1 + 
      geom_smooth(method = "lm")+
      geom_point(data = dems[abs(dems$std.resids) < 1.5,]) +
      geom_text(data = dems[abs(dems$std.resids) >= 1.5,], aes(label=Last.Name))+
      xlab("DW-NOMINATE 1st Dimension")+
      ylab("Trump Score: Standardized Residual")+
      ggtitle("Gillibrand is a huge outlier")+
      theme_bw(), 0, 0, 1, 1)+
  draw_label("Created by: Jon Green", 
             x = .9, y = .02, size = 8, alpha = 1)

resplot.d1.us <- ggplot(dems, aes(x = dim1, y = resids.us))
resplot.d1.us + 
  geom_smooth(method = "lm")+
  geom_point(data = dems[abs(dems$resids.us) < .045,]) +
  geom_text(data = dems[abs(dems$resids.us) >= .045,], aes(label=Last.Name))+
  xlab("DW-NOMINATE 1st Dimension")+
  ylab("Model Residual")+
  ggtitle("Gillibrand is different")+
  theme_bw()

resplot.tm <- ggplot(dems, aes(x = TRUMP.MARGIN, y = std.resids))
resplot.tm + 
  geom_smooth(method = "lm")+
  geom_point(data = dems[abs(dems$std.resids) < 1.5,]) +
  geom_text(data = dems[abs(dems$std.resids) >= 1.5,], aes(label=Last.Name))+
  xlab("Trump Margin")+
  ylab("Standardized Residual")+
  ggtitle("Gillibrand is a huge outlier: statewide presidential vote")+
  theme_bw()

resplot.tm.us <- ggplot(dems, aes(x = TRUMP.MARGIN, y = resids.us))
resplot.tm.us + 
  geom_smooth(method = "lm")+
  geom_point(data = dems[abs(dems$resids.us) < .045,]) +
  geom_text(data = dems[abs(dems$resids.us) >= .045,], aes(label=Last.Name))+
  xlab("Trump Margin")+
  ylab("Model Residual")+
  ggtitle("Gillibrand is different")+
  theme_bw()

# Full leave-one-out validation
for(i in 1:nrow(dems)){
  modts <- lm(TRUMP.SCORE ~ dim1 + TRUMP.MARGIN,
              data = dems[-i,])
  modsumts <- summary(modts)
  
  dems$looR2[i] <- modsumts$adj.r.squared
  
  dems$tspred[i] <- predict(modts, newdata = dems[i,])
}
dems$ts.vs.replace <- with(dems, TRUMP.SCORE - tspred) # positive values = replacement would be more pro-Trump

tsvr <- ggplot(dems, aes(x = tspred, y = TRUMP.SCORE))
ggdraw() +
  draw_plot(
    tsvr + 
      geom_point(data = dems[abs(dems$ts.vs.replace) <= .1,]) +
      geom_text(data = dems[abs(dems$ts.vs.replace) > .1,], aes(label=Last.Name))+  
      geom_abline(slope = 1, intercept = 0)+
      ggtitle("Trump Score vs. Replacement Democrat")+
      xlab("Predicted Trump Score Based on All Other Democrats")+
      ylab("Actual Trump Score")+
      theme_bw(), 0, 0, 1, 1)+
  draw_label("Created by: Jon Green", 
             x = .9, y = .02, size = 8, alpha = 1)

tslooR2 <- ggplot(dems, aes(x = TRUMP.SCORE, y = looR2))
ggdraw() +
  draw_plot(
    tslooR2 + 
      geom_point(data = dems[dems$looR2 > .76 & dems$looR2 < .8,]) +
      geom_text(data = dems[dems$looR2 <= .76 | dems$looR2 >= .8,], aes(label=Last.Name))+
      geom_hline(yintercept = psus$adj.r.squared)+
      ggtitle("Model Fit When Excluding...")+
      xlab("Trump Score")+
      ylab("Adjusted R^2")+
      theme_bw(), 0, 0, 1, 1)+
  draw_label("Created by: Jon Green", 
             x = .9, y = .02, size = 8, alpha = 1)
