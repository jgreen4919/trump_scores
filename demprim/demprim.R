# Script for Trump Score Above Replacement
# Analysis appears in https://www.vice.com/en_us/article/paq49v/these-democratic-senators-should-be-afraid-for-their-jobs
# and https://www.vice.com/en_us/article/kzp8nz/these-house-democrats-should-be-scared-of-progressives
# Jon Green

library(tidyverse)
library(MASS)
library(cowplot)
library(car)
library(ggrepel)

# Both Vice pieces use Trump Scores as of Jan 1 2017. If you want to update them:
library(rvest)

h <- read_html('https://projects.fivethirtyeight.com/congress-trump-score/house/')

trump_score <- h %>% 
  html_nodes('table.member-list') %>% 
  map(html_table, fill = TRUE) %>% 
  set_names(c('Senate', 'House')) %>%
  map(set_names, 
      c('member', 'member_short', 'party', 'district', 'trump_score', 
        'trump_margin', 'predicted_score', 'trump_plus_minus'))

trump_score <- lapply(trump_score, function(x){
  mutate_all(x, na_if, '-')
})

ts <- c(rep("house", nrow(trump_score$House)), rep("senate", nrow(trump_score$Senate)))

trump_score$House <- data.frame(cbind(rep("house", nrow(trump_score$House)), trump_score$House))
trump_score$Senate <- data.frame(cbind(rep("senate", nrow(trump_score$Senate)), trump_score$Senate))

names(trump_score$House) <- c("chamber", names(trump_score$House)[2:ncol(trump_score$House)])
names(trump_score$Senate) <- names(trump_score$House)

trump_score <- data.frame(rbind(trump_score$House, trump_score$Senate))

set.seed(12345)

# Read in the data
source("~/Desktop/demprim/statedistarea.R") 
cong <- read.csv("~/Desktop/gillibrand/trump_scores/trump_scores/trump_score_full/trump-score.csv", stringsAsFactors = FALSE)
congd1 <- read.csv("~/Desktop/gillibrand/trump_scores/trump_scores/trump_score_full/cong_dim1.csv", stringsAsFactors = FALSE)

housm <- read.csv("~/Desktop/gillibrand/trump_scores/trump_scores/trump_score_full/housemargins.csv", stringsAsFactors = FALSE)
senm <- read.csv("~/Desktop/gillibrand/trump_scores/trump_scores/trump_score_full/senatemargins.csv", stringsAsFactors = FALSE)

cong$chamber <- tolower(cong$chamber) # set chamber to lowercase
cong$member_short <- tolower(cong$member_short) # set member names to lowercase

# clean names (remove special characters) for matching
cong$member_short <- gsub("^.*\\. ","", cong$member_short)
cong$member_short <- gsub("[*].*$","",cong$member_short)
cong$member_short <- gsub("á","a",cong$member_short)
cong$member_short <- gsub("é","e",cong$member_short)
cong$member_short <- gsub("’","'",cong$member_short)

cong$state <- factor(substr(cong$district, start = 1, stop = 2)) # extract state for matching

# clean names (remove special characters) for matching
congd1$member_short <- gsub("\xe7","a",congd1$member_short)
congd1$member_short <- gsub("\x83","e",congd1$member_short)
congd1$member_short <- gsub("’","'",congd1$member_short)
congd1$member_short <- iconv(congd1$member_short, from="UTF-8", to="LATIN1")
congd1$member_short <- tolower(congd1$member_short)

# recode party for matching
congd1$party[congd1$party == "Republican"] <- "R"
congd1$party[congd1$party == "Democrat"] <- "D"
congd1$party[congd1$party == "Independent"] <- "I"

# set state for matching
congd1$state <- unlist(sapply(congd1$state, function(x){
  stateabbv[which(statename == x)]
}))
congd1$state <- as.factor(congd1$state)

congd1$chamber <- tolower(congd1$chamber) # send chamber to lowercase

# merge trump score data with nominate data
congall <- merge(cong, congd1[,c("chamber", "party", "state", "member_short","nominate")], by = c("chamber", "party", "state", "member_short"))

# define caucus to put king/sanders in with dems
congall$caucus <- congall$party
congall$caucus[congall$member_short %in% c("sanders","king") & congall$chamber == "senate"] <- "D"

congall <- congall[!is.na(congall$trump_score),] # remove members with no Trump Score (appointees like Sessions, Zinke)

# remove incorrect duplicate rooneys from florida
congall <- congall[-c(which(congall$district == "FL-19" & congall$nominate < .5),
                      which(congall$district == "FL-17" & congall$nominate > .5)),]

# remove incorrect duplicate maloneys from new york
congall <- congall[-c(which(congall$district == "NY-12" & congall$nominate > -.3),
                      which(congall$district == "NY-18" & congall$nominate < -.3)),]

# remove incorrect duplicate greens from texas
congall <- congall[-c(which(congall$district == "TX-29" & congall$nominate < -.4),
                      which(congall$district == "TX-9" & congall$nominate > -.4)),]

# quick function to re-capitalize names for viz
simpleCap <- function(x) {
  s <- strsplit(x, "\\, |\\-| ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

congall$member_short <- sapply(congall$member_short, function(x){
  simpleCap(x)
})

# Remove special cases (Becerra was appointed elsewhere, Conyers resigned in disgrace, Gomez replaced Becerra)
congall <- congall[-which(congall$member_short %in% c("Becerra","Gomez","Conyers")),]

# Split out chambers
house <- congall[congall$chamber == "house",]
senate <- congall[congall$chamber == "senate",]

# fiddle with district/member names for matching and viz
senate$member_short <- gsub("cc","cC", senate$member_short)
house$member_short <- gsub("O'h","O'H", house$member_short)
house$member_short <- gsub("Mcg","McG", house$member_short)

# add leading zeroes to house districts with numbers less than ten
house$district <- with(house, ifelse(nchar(district) == 4, gsub("-","-0", district), district))

# merge chamber data with demos data
house <- merge(house, housm[,c("district", "birthyr","gender","race","religion","lgbt",
                               "demH16","repH16","clinton16","trump16",
                               "cens10white","cens10black","cens10latino","cens10aapi","cens10native","cens10other",
                               "acs16ba","acs16whiteba","medinc",
                               "popvote16","pop","popWhite","popBlack","popLatino","popAAPI","popNative","popOther",
                               "vap","vapWhite","vapBlack","vapLatino","vapAAPI","vapNative","vapOther")],
               by = "district")

senate <- merge(senate, senm[,c("inclast", "birthyr","gender","race","religion","lgbt",
                                   "demS16","repS16","clinton16","trump16",
                                   "cens10white","cens10black","cens10latino","cens10aapi","cens10native","cens10other",
                                   "acs16ba","acs16whiteba","medinc",
                                   "popvote16","pop","popWhite","popBlack","popLatino","popAAPI","popNative","popOther",
                                   "vap","vapWhite","vapBlack","vapLatino","vapAAPI","vapNative","vapOther")],
                by.x = "member_short",by.y = "inclast")

# calculate shares of voting age population by race
house$vapWshare <- with(house, vapWhite / vap)
house$vapBshare <- with(house, vapBlack / vap)
house$vapLshare <- with(house, vapLatino / vap)
house$vapAshare <- with(house, vapAAPI / vap)

senate$vapWshare <- with(senate, vapWhite / vap)
senate$vapBshare <- with(senate, vapBlack / vap)
senate$vapLshare <- with(senate, vapLatino / vap)
senate$vapAshare <- with(senate, vapAAPI / vap)

# append land area of constituencies 
house <- merge(house, distarea.al, by.x = "district", by.y = "distabbv")
senate <- merge(senate, statearea[,c("stateabbv","sqmiland")], by.x = "district", by.y = "stateabbv")

# calculate population density
house$popdens <- with(house, pop / land.area)
senate$popdens <- with(senate, pop / sqmiland)

# Subset down to the Democratic caucus
demH <- house[house$party == "D",]
demS <- senate[senate$caucus == "D",]

# Standardize numeric variables of interest
standsH <- data.frame(
  scale(
    subset(
      demH,select = c(nominate, trump_score, trump_margin,
                      demH16, repH16, cens10white, cens10black,
                      cens10latino, cens10aapi, cens10native,
                      cens10other, acs16ba, acs16whiteba,
                      medinc, popvote16, pop, popWhite, popBlack,
                      popLatino, popAAPI, popNative, popOther, vap,
                      vapWhite, vapBlack, vapLatino, vapAAPI, vapNative,
                      vapOther, vapWshare, vapBshare, vapLshare, vapAshare,
                      popdens)
      )
    )
  )
names(standsH) <- c("std.nominate", "std.trump_score","std.trump_margin",
                   "std.demH16", "std.repH16", "std.cens10white", "std.cens10black",
                   "std.cens10latino", "std.cens10aapi", "std.cens10native",
                   "std.cens10other", "std.acs16ba", "std.acs16whiteba",
                   "std.medinc", "std.popvote16", "std.pop", "popWhite", "std.popBlack",
                   "std.popLatino", "std.popAAPI", "std.popNative", "std.popOther", "std.vap",
                   "std.vapWhite", "std.vapBlack", "std.vapLatino", "std.vapAAPI", "std.vapNative",
                   "std.vapOther", "std.vapWshare","std.vapBshare","std.vapLshare","std.vapAshare",
                   "std.popdens")
demH <- data.frame(cbind(demH, standsH)) # Append standardized variables

# repeat for senate
standsS <- data.frame(
  scale(
    subset(
      demS,select = c(nominate, trump_score, trump_margin,
                      demS16, repS16, cens10white, cens10black,
                      cens10latino, cens10aapi, cens10native,
                      cens10other, acs16ba, acs16whiteba,
                      medinc, popvote16, pop, popWhite, popBlack,
                      popLatino, popAAPI, popNative, popOther, vap,
                      vapWhite, vapBlack, vapLatino, vapAAPI, vapNative,
                      vapOther, vapWshare, vapBshare, vapLshare, vapAshare,
                      popdens)
    )
  )
)
names(standsS) <- c("std.nominate", "std.trump_score","std.trump_margin",
                    "std.demS16", "std.repS16", "std.cens10white", "std.cens10black",
                    "std.cens10latino", "std.cens10aapi", "std.cens10native",
                    "std.cens10other", "std.acs16ba", "std.acs16whiteba",
                    "std.medinc", "std.popvote16", "std.pop", "popWhite", "std.popBlack",
                    "std.popLatino", "std.popAAPI", "std.popNative", "std.popOther", "std.vap",
                    "std.vapWhite", "std.vapBlack", "std.vapLatino", "std.vapAAPI", "std.vapNative",
                    "std.vapOther", "std.vapWshare","std.vapBshare","std.vapLshare","std.vapAshare",
                    "std.popdens")
demS <- data.frame(cbind(demS, standsS)) # Append standardized variables


# bivariate relationships
dwtmS <- ggplot(demS, aes(x = trump_margin, y = nominate))+ 
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)+
  xlab("Trump Margin")+
  ylab("DW-NOMINATE First Dimension")+
  theme_bw()
dwtmH <- ggplot(demH, aes(x = trump_margin, y = nominate))+ 
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)+
  xlab("Trump Margin")+
  ylab("DW-NOMINATE First Dimension")+
  theme_bw()

plot_grid(dwtmH, dwtmS, labels = c('House', 'Senate'),
          scale = 0.95)
          
tstmS <- ggplot(demS, aes(x = trump_margin, y = trump_score))+
  geom_point()+
  ylim(c(0,65))+
  geom_smooth(method = "lm", se = FALSE)+
  xlab("Trump Margin")+
  ylab("Trump Score")+
  theme_bw()
tstmH <- ggplot(demH, aes(x = trump_margin, y = trump_score))+
  geom_point()+
  ylim(c(0,65))+
  geom_smooth(method = "lm", se = FALSE)+
  xlab("Trump Margin")+
  ylab("Trump Score")+
  theme_bw()
plot_grid(tstmH, tstmS, labels = c('House', 'Senate'),
          scale = 0.95)

d1tsS <- ggplot(demS, aes(x = nominate, y = trump_score))+
  geom_point()+
  ylim(c(0,64))+
  geom_smooth(method = "lm", se = FALSE)+
  xlab("DW-NOMINATE First Dimension")+
  ylab("Trump Score")+
  theme_bw()
d1tsH <- ggplot(demH, aes(x = nominate, y = trump_score))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)+
  xlab("DW-NOMINATE First Dimension")+
  ylab("Trump Score")+
  theme_bw()
plot_grid(d1tsH, d1tsS, labels = c('House', 'Senate'),
          scale = 0.95)


# OLS
# simple bivariate
predscore.house <- lm(trump_score ~ std.trump_margin,
                data = demH)
psh <- summary(predscore.house)

# add demos
predscore.housedemos <- lm(trump_score ~ std.trump_margin +
                            std.vapWshare + std.acs16ba + std.medinc,
                      data = demH)
pshd <- summary(predscore.housedemos)

demH$trump16.2 <- demH$trump16^2 # square trump share to account for non-linearity in house data

# demos with squared trump share
predscore.housedemos.plus <- lm(trump_score ~ trump16.2 +
                             std.vapWshare + std.acs16ba + std.medinc,
                           data = demH)
pshdp <- summary(predscore.housedemos.plus) # Use this model

# senate model intro
predscore.senate <- lm(trump_score ~ std.nominate + std.trump_margin,
                       data = demS)
pss <- summary(predscore.senate)

# drop nominate and add state demos
predscore.senatedemos <- lm(trump_score ~ std.trump_margin +
                              std.vapWshare + std.acs16ba + std.medinc,
                       data = demS)
pssd <- summary(predscore.senatedemos)

# test adding population density
predscore.senatedemos.land <- lm(trump_score ~ std.trump_margin +
                              std.vapWshare + std.acs16ba + std.medinc+
                                log(popdens),
                            data = demS)
pssdl <- summary(predscore.senatedemos.land)

# repeat but with nominate as the outcome
dwscore.house <- lm(nominate ~ std.trump_margin +
                                  std.vapWshare + std.acs16ba + std.medinc,
                                data = demH)
dsh <- summary(dwscore.house)

# add population density
dwscore.house.land <- lm(nominate ~ std.trump_margin + 
                           std.vapWshare + std.acs16ba + std.medinc +
                           log(popdens), data = demH)
dshl <- summary(dwscore.house.land)

# run for senate
dwscore.sen <- lm(nominate ~ std.trump_margin + 
                      std.vapWshare + std.acs16ba + std.medinc, data = demS)
dss <- summary(dwscore.sen)

# add population density
dwscore.sen.land <- lm(nominate ~ std.trump_margin + 
                           std.vapWshare + std.acs16ba + std.medinc +
                           log(popdens), data = demS)
dssl <- summary(dwscore.sen.land)

# Attach residuals and standardized residuals for selected models to dataset
demH$resids <- predscore.house$residuals
demS$resids <- predscore.senate$residuals
demH$std.resids <- as.numeric(scale(demH$resids))
demS$std.resids <- as.numeric(scale(demS$resids))

demH$residsdemos <- predscore.housedemos.plus$residuals
demS$residsdemos <- predscore.senatedemos$residuals
demH$std.residsdemos <- as.numeric(scale(demH$residsdemos))
demS$std.residsdemos <- as.numeric(scale(demS$residsdemos))

demH$dwresids <- dwscore.house$residuals
demS$dwresids <- dwscore.sen$residuals
demH$std.dwresids <- as.numeric(scale(demH$dwresids))
demS$std.dwresids <- as.numeric(scale(demS$dwresids))

# Plot residuals against quantities of interest
resplot.d1H <- ggplot(demH, aes(x = nominate, y = std.resids))+
  geom_smooth(method = "lm", se = FALSE)+
  geom_point(data = demH[abs(demH$std.resids) < 2,]) +
  geom_text(data = demH[abs(demH$std.resids) >= 2,], aes(label=member_short))+
  xlab("DW-NOMINATE 1st Dimension")+
  ylab("Trump Score: Standardized Residual")+
  theme_bw()
resplot.d1S <- ggplot(demS, aes(x = nominate, y = std.resids))+
  geom_smooth(method = "lm", se = FALSE)+
  geom_point(data = demS[abs(demS$std.resids) < 2,]) +
  geom_text(data = demS[abs(demS$std.resids) >= 2,], aes(label=member_short))+
  xlab("DW-NOMINATE 1st Dimension")+
  ylab("Trump Score: Standardized Residual")+
  theme_bw()
plot_grid(resplot.d1H, resplot.d1S, labels = c('House', 'Senate'),
          scale = 0.95)

resplot.tmHd <- ggplot(demH, aes(x = trump_margin, y = std.residsdemos))+
  geom_smooth(method = "lm", se = FALSE)+
  geom_point(data = demH[abs(demH$std.residsdemos) < 1,]) +
  geom_text(data = demH[abs(demH$std.residsdemos) >= 1,], aes(label=member_short))+
  xlab("Trump Margin")+
  ylab("Trump Score: Standardized Residual")+
  theme_bw()
resplot.d1Sd <- ggplot(demS, aes(x = trump_margin, y = std.residsdemos))+
  geom_smooth(method = "lm", se = FALSE)+
  geom_point(data = demS[abs(demS$std.residsdemos) < 2,]) +
  geom_text(data = demS[abs(demS$std.residsdemos) >= 2,], aes(label=member_short))+
  xlab("Trump Margin")+
  ylab("Trump Score: Standardized Residual")+
  theme_bw()
plot_grid(resplot.tmHd, resplot.d1Sd, labels = c('House', 'Senate'),
          scale = 0.95)

# Full leave-one-out validation

# for each democratic senator
for(i in 1:nrow(demS)){ 
  # re-specify model with that senator removed from the dataset
  modts <- lm(trump_score ~ std.trump_margin +
                std.vapWshare + std.acs16ba + std.medinc,
              data = demS[-i,])
  
  # store the summary
  modsumts <- summary(modts) 
  
  # repeat with nominate as the outcome
  moddw <- lm(nominate ~ std.trump_margin +
                std.vapWshare + std.acs16ba + std.medinc,
              data = demS[-i,])
  modsumdw <- summary(moddw)
  
  # store the adjusted R2s
  demS$tslooR2[i] <- modsumts$adj.r.squared
  demS$dwlooR2[i] <- modsumdw$adj.r.squared
  
  # store the prediction for the missing senator based on this re-specification and the IVs from their row 
  demS$tspred[i] <- predict(modts, newdata = demS[i,])
  demS$dwpred[i] <- predict(moddw, newdata = demS[i,])
}

# repeat for the house
for(i in 1:nrow(demH)){
  modts <- lm(trump_score ~ trump16.2 +
                std.vapWshare + std.acs16ba + std.medinc,
              data = demH[-i,])
  modsumts <- summary(modts)
  
  moddw <- lm(nominate ~ trump16.2 +
                std.vapWshare + std.acs16ba + std.medinc,
              data = demH[-i,])
  modsumdw <- summary(moddw)
  
  demH$tslooR2[i] <- modsumts$adj.r.squared
  demH$dwlooR2[i] <- modsumdw$adj.r.squared
  
  demH$tspred[i] <- predict(modts, newdata = demH[i,])
  demH$dwpred[i] <- predict(moddw, newdata = demH[i,])
}

# compare R2s with each member missing to overall model
demH$tsR2vsr <- with(demH, tslooR2 - pshd$adj.r.squared)
demS$tsR2vsr <- with(demS, tslooR2 - pssd$adj.r.squared)

demH$dwR2vsr <- with(demH, dwlooR2 - pshd$adj.r.squared)
demS$dwR2vsr <- with(demS, dwlooR2 - pssd$adj.r.squared)

# compare predicted trump scores and nominates to observed
demH$ts.vs.replace <- with(demH, trump_score - tspred) # positive values = replacement would be more pro-Trump
demS$ts.vs.replace <- with(demS, trump_score - tspred) # positive values = replacement would be more pro-Trump

demH$dw.vs.replace <- with(demH, nominate - dwpred) # positive values = replacement would be more liberal
demS$dw.vs.replace <- with(demS, nominate - dwpred) # positive values = replacement would be more liberal

# add ranks for the vs.replace measures
demH <- as_tibble(demH) %>% mutate(tsvr.rank = dense_rank(desc(ts.vs.replace)),
                                   dwvr.rank = dense_rank(desc(dw.vs.replace)))
demS <- as_tibble(demS) %>% mutate(tsvr.rank = dense_rank(desc(ts.vs.replace)),
                                   dwvr.rank = dense_rank(desc(dw.vs.replace)))

# plots
tsvrH <- ggplot(demH, aes(x = tspred, y = trump_score))+
  geom_point(data = demH[abs(demH$ts.vs.replace) <= sd(demH$ts.vs.replace),]) +
  geom_text(data = demH[abs(demH$ts.vs.replace) > sd(demH$ts.vs.replace),], aes(label=member_short, size = 2))+ 
  geom_abline(slope = 1, intercept = 0)+
  xlab("Predicted Trump Score Based on All Other Democrats' District Demographics")+
  ylab("Actual Trump Score")+
  ggtitle("Trump Score Above Replacement: House Democrats")+
  guides(size = FALSE)+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 10, b = 0, l = 0)))

tsvrS <- ggplot(demS, aes(x = tspred, y = trump_score))+
  geom_point(data = demS[abs(demS$ts.vs.replace) <= 10,]) +
  geom_text(data = demS[abs(demS$ts.vs.replace) > 10,], aes(label=member_short))+  
  ylim(c(0, 65))+
  geom_abline(slope = 1, intercept = 0)+
  xlab("Predicted Trump Score Based on All Other Democrats' State Demographics")+
  ylab("Actual Trump Score")+
  theme_bw()

tsvrS2 <- ggplot(demS, aes(x = tspred, y = trump_score))+
  geom_point(data = demS[abs(demS$ts.vs.replace) <= 5,]) +
  geom_text(data = demS[abs(demS$ts.vs.replace) > 5,], aes(label=member_short))+  
  geom_abline(slope = 1, intercept = 0)+
  ggtitle("Trump Score Above Replacement: Senate")+
  xlab("Predicted Trump Score Based on All Other Democrats' State Demographics")+
  ylab("Actual Trump Score")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 10, b = 0, l = 0)))

dwvrH2 <- ggplot(demH, aes(x = dwpred, y = nominate))+
  geom_point(data = demH[abs(demH$dw.vs.replace) <= .13,]) +
  geom_text(data = demH[abs(demH$dw.vs.replace) > .13,], aes(label=member_short))+ 
  geom_abline(slope = 1, intercept = 0)+
  xlab("Predicted DW-NOMINATE Based on All Other Democrats' District Demographics")+
  ylab("Actual DW-NOMINATE")+
  ggtitle("DW-NOMINATE Above Replacement: House")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 10, b = 0, l = 0)))
dwvrS2 <- ggplot(demS, aes(x = dwpred, y = nominate))+
  geom_point(data = demS[abs(demS$dw.vs.replace) <= .1,]) +
  geom_text(data = demS[abs(demS$dw.vs.replace) > .1,], aes(label=member_short))+ 
  geom_abline(slope = 1, intercept = 0)+
  xlab("Predicted DW-NOMINATE Based on All Other Democrats' State Demographics")+
  ylab("Actual DW-NOMINATE")+
  ggtitle("DW-NOMINATE Above Replacement: Senate")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 10, b = 0, l = 0)))

tslooR2S <- ggplot(demS, aes(x = trump_score, y = tslooR2))+
  geom_point(data = demS[demS$tslooR2 > .57 & demS$tslooR2 < .62,]) +
  geom_text(data = demS[demS$tslooR2 <= .57 | demS$tslooR2 >= 62,], aes(label=member_short))+
  geom_hline(yintercept = pssd$adj.r.squared)+
  ggtitle("Model Fit When Excluding...")+
  xlab("Trump Score")+
  ylab("Adjusted R^2")+
  theme_bw()
tslooR2H <- ggplot(demH, aes(x = trump_score, y = tslooR2))+
  geom_point(data = demH[demH$tslooR2 < (pshdp$adj.r.squared+.02) & demH$tslooR2 > (pshdp$adj.r.squared - .02),]) +
  geom_text(data = demH[demH$tslooR2 >= (pshdp$adj.r.squared+.02) | demH$tslooR2 <= (pshdp$adj.r.squared-.02),], aes(label=member_short))+
  geom_hline(yintercept = pshdp$adj.r.squared)+
  ggtitle("Model Fit When Excluding...")+
  xlab("Trump Score")+
  ylab("Adjusted R^2")+
  theme_bw()
plot_grid(tslooR2H, tslooR2S, labels = c('House', 'Senate'),
          scale = 0.95)

dwlooR2S <- ggplot(demS, aes(x = nominate, y = dwlooR2))+
  geom_point(data = demS[demS$dwlooR2 > .33 & demS$dwlooR2 < .39,]) +
  geom_text(data = demS[demS$dwlooR2 <= .33 | demS$dwlooR2 >= .39,], aes(label=member_short))+
  geom_hline(yintercept = dss$adj.r.squared)+
  ggtitle("Model Fit When Excluding...")+
  xlab("Trump Score")+
  ylab("Adjusted R^2")+
  theme_bw()
dwlooR2H <- ggplot(demH, aes(x = nominate, y = dwlooR2))+
  geom_point(data = demH[demH$dwlooR2 > .439 & demH$dwlooR2 < .46,]) +
  geom_text(data = demH[demH$dwlooR2 <= .439 | demH$dwlooR2 >= .46,], aes(label=member_short))+
  geom_hline(yintercept = dsh$adj.r.squared)+
  ggtitle("Model Fit When Excluding...")+
  xlab("Trump Score")+
  ylab("Adjusted R^2")+
  theme_bw()
plot_grid(dwlooR2H, dwlooR2S, labels = c('House', 'Senate'),
          scale = 0.95)

ggplot(demS, aes(x = tspred, y = trump_score))+
  geom_point(data = demS[demS$ts.vs.replace < 5,]) +
  geom_text(data = demS[demS$ts.vs.replace >= 5,], aes(label=member_short))+  
  geom_abline(slope = 1, intercept = 0)+
  xlab("Predicted Trump Score Based on All Other Democrats' State Demographics")+
  ylab("Actual Trump Score")+
  ggtitle("Senate Democrats with the Highest Trump Scores Above Replacement")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 10, b = 0, l = 0)))

ggplot(demH, aes(x = tspred, y = trump_score))+
  geom_point(data = demH[demH$ts.vs.replace < 1.5*sd(demH$ts.vs.replace),]) +
  geom_text(data = demH[demH$ts.vs.replace >= 1.5*sd(demH$ts.vs.replace),], 
            aes(label=member_short), 
            size = 5)+  
  geom_abline(slope = 1, intercept = 0)+
  xlab("Predicted Trump Score Based on All Other Democrats' District Demographics")+
  ylab("Actual Trump Score")+
  ggtitle("House Democrats with the Highest Trump Scores Above Replacement")+
  guides(size = FALSE)+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 10, b = 0, l = 0)))

ggplot(demS, aes(x = tspred, y = trump_score))+
  geom_point(data = demS[demS$ts.vs.replace > -5,]) +
  geom_text(data = demS[demS$ts.vs.replace <= -5,], aes(label=member_short))+  
  geom_abline(slope = 1, intercept = 0)+
  xlab("Predicted Trump Score Based on All Other Democrats' State Demographics")+
  ylab("Actual Trump Score")+
  ggtitle("Senate Democrats with the Lowest Trump Scores Above Replacement")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 10, b = 0, l = 0)))

ggplot(demH, aes(x = tspred, y = trump_score))+
  geom_point(data = demH[demH$ts.vs.replace > -1.5*sd(demH$ts.vs.replace),]) +
  geom_text(data = demH[demH$ts.vs.replace <= -1.5*sd(demH$ts.vs.replace),], 
                  aes(label=member_short),
                  size = 5)+  
  geom_abline(slope = 1, intercept = 0)+
  xlab("Predicted Trump Score Based on All Other Democrats' District Demographics")+
  ylab("Actual Trump Score")+
  ggtitle("House Democrats with the Lowest Trump Scores Above Replacement")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 10, b = 0, l = 0)))

ggplot(demH[demH$cens10latino > 50,], aes(x = tspred, y = trump_score))+
  geom_point(data = demH[demH$cens10latino > 50 & abs(demH$ts.vs.replace) < 1.5*sd(demH$ts.vs.replace),]) +
  geom_text(data = demH[demH$cens10latino > 50 & abs(demH$ts.vs.replace) >= 1.5*sd(demH$ts.vs.replace),], 
            aes(label=member_short),
            size = 5)+  
  geom_abline(slope = 1, intercept = 0)+
  xlab("Predicted Trump Score Based on All Other Democrats' District Demographics")+
  ylab("Actual Trump Score")+
  ggtitle("Trump Scores Above Replacement for House Democrats Representing >50% Latino Districts")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 10, b = 0, l = 0)))


ggplot(demS, aes(x = dwpred, y = nominate))+
  geom_point(data = demS[demS$dw.vs.replace < .1,]) +
  geom_text(data = demS[demS$dw.vs.replace >= .1,], aes(label=member_short))+  
  geom_abline(slope = 1, intercept = 0)+
  xlab("Predicted DW-NOMINATE Based on All Other Democrats' State Demographics")+
  ylab("Actual DW-NOMINATE")+
  ggtitle("Senate Democrats with the Highest DW-NOMINATE Above Replacement")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 10, b = 0, l = 0)))

ggplot(demH, aes(x = dwpred, y = nominate))+
  geom_point(data = demH[demH$dw.vs.replace < .13,]) +
  geom_text(data = demH[demH$dw.vs.replace >= .13,], aes(label=member_short))+  
  geom_abline(slope = 1, intercept = 0)+
  xlab("Predicted DW-NOMINATE Based on All Other Democrats' District Demographics")+
  ylab("Actual DW-NOMINATE")+
  ggtitle("House Democrats with the Highest DW-NOMINATE Above Replacement")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 10, b = 0, l = 0)))

ggplot(demS, aes(x = dwpred, y = nominate))+
  geom_point(data = demS[demS$dw.vs.replace > -.1,]) +
  geom_text(data = demS[demS$dw.vs.replace <= -.1,], aes(label=member_short))+  
  geom_abline(slope = 1, intercept = 0)+
  xlab("Predicted DW-NOMINATE Based on All Other Democrats' State Demographics")+
  ylab("Actual DW-NOMINATE")+
  ggtitle("Senate Democrats with the Lowest DW-NOMINATE Above Replacement")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 10, b = 0, l = 0)))

ggplot(demH, aes(x = dwpred, y = nominate))+
  geom_point(data = demH[demH$dw.vs.replace > -.1,]) +
  geom_text(data = demH[demH$dw.vs.replace <= -.1,], aes(label=member_short))+  
  geom_abline(slope = 1, intercept = 0)+
  xlab("Predicted DW-NOMINATE Based on All Other Democrats' District Demographics")+
  ylab("Actual DW-NOMINATE")+
  ggtitle("House Democrats with the Lowest DW-NOMINATE Above Replacement")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 10, b = 0, l = 0)))

# rearrange with ranks
rankS <- dplyr::select(demS, member, state, trump_score, tspred, ts.vs.replace, tsvr.rank) %>% arrange(tsvr.rank)
rankH <- dplyr::select(demH, member, district, trump_score, tspred, ts.vs.replace, tsvr.rank) %>% arrange(tsvr.rank)
dwrankH <- dplyr::select(demH, member, district, nominate, dwpred, dw.vs.replace, dwvr.rank) %>% arrange(dwvr.rank)
dwrankS <- dplyr::select(demS, member, state, nominate, dwpred, dw.vs.replace, dwvr.rank) %>% arrange(dwvr.rank)

rankSd <- dplyr::select(demS, member, state, trump_score, tspred, ts.vs.replace, tsvr.rank) %>% arrange(desc(tsvr.rank))
rankHd <- dplyr::select(demH, member, district, trump_score, tspred, ts.vs.replace, tsvr.rank) %>% arrange(desc(tsvr.rank))
dwrankHd <- dplyr::select(demH, member, district, nominate, dwpred, dw.vs.replace, dwvr.rank) %>% arrange(desc(dwvr.rank))
dwrankSd <- dplyr::select(demS, member, state, nominate, dwpred, dw.vs.replace, dwvr.rank) %>% arrange(desc(dwvr.rank))

# round
rankS[,3:5] <- round(rankS[,3:5],0)
rankSd[,3:5] <- round(rankSd[,3:5],0)
rankH[,3:5] <- round(rankH[,3:5],0)
rankHd[,3:5] <- round(rankHd[,3:5],0)

dwrankS[,3:5] <- round(dwrankS[,3:5],3)
dwrankSd[,3:5] <- round(dwrankSd[,3:5],3)
dwrankH[,3:5] <- round(dwrankH[,3:5],3)
dwrankHd[,3:5] <- round(dwrankHd[,3:5],3)

# write csvs
setwd("~/Desktop/demprim")
write.csv(rankS, file = "sendem_tsvrrank.csv")
write.csv(rankH, file = "housedem_tsvrrank.csv")
write.csv(rankSd, file = "sendem_tsvr_revrank.csv")
write.csv(rankHd, file = "housedem_tsvr_revrank.csv")

write.csv(dwrankS, file = "sendem_dwvrrank.csv")
write.csv(dwrankH, file = "housedem_dwvrrank.csv")
write.csv(dwrankSd, file = "sendem_dwvr_revrank.csv")
write.csv(dwrankHd, file = "housedem_dwvr_revrank.csv")

# compare D and R
tstmHb <- ggplot(house, aes(x = trump_margin, y = trump_score, col = caucus))+
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)+
  xlab("Trump Margin")+
  ylab("Trump Score")+
  scale_color_manual(name = "",
                     breaks = c("D","R"),
                     values = c("blue","red"),
                     labels = c("Democrats","Republicans"))+
  theme_bw()+
  theme(legend.position = "bottom")
tstmSb <- ggplot(senate, aes(x = trump_margin, y = trump_score, col = caucus))+
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)+
  xlab("Trump Margin")+
  ylab("Trump Score")+
  scale_color_manual(name = "",
                     breaks = c("D","R"),
                     values = c("blue","red"),
                     labels = c("Democrats","Republicans"))+
  theme_bw()+
  theme(legend.position = "bottom")
plot_grid(tstmHb, tstmSb, labels = c('House', 'Senate'),
          scale = 0.95)

tstmSb2 <- ggplot(senate, aes(x = trump_margin, y = trump_score, col = caucus))+
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)+
  xlab("Trump Margin")+
  ylab("Trump Score")+
  ggtitle("Trump Score by Trump Margin: All Senate")+
  scale_color_manual(name = "",
                     breaks = c("D","R"),
                     values = c("blue","red"),
                     labels = c("Democrats","Republicans"))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 10, b = 0, l = 0)),
        legend.position = "bottom")
  
dwtmHb <- ggplot(house, aes(x = trump_margin, y = nominate, col = caucus))+
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)+
  ylim(c(-.8, 1))+
  xlab("Trump Margin")+
  ylab("DW-NOMINATE First Dimension")+
  scale_color_manual(name = "",
                     breaks = c("D","R"),
                     values = c("blue","red"),
                     labels = c("Democrats","Republicans"))+
  theme_bw()+
  theme(legend.position = "bottom")
dwtmSb <- ggplot(senate, aes(x = trump_margin, y = nominate, col = caucus))+
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)+
  ylim(c(-.8, 1))+
  xlab("Trump Margin")+
  ylab("DW-NOMINATE First Dimension")+
  scale_color_manual(name = "",
                     breaks = c("D","R"),
                     values = c("blue","red"),
                     labels = c("Democrats","Republicans"))+
  theme_bw()+
  theme(legend.position = "bottom")
plot_grid(dwtmHb, dwtmSb, labels = c('House', 'Senate'),
          scale = 0.95)

dwtmSb2 <- ggplot(senate, aes(x = trump_margin, y = nominate, col = caucus))+
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)+
  xlab("Trump Margin")+
  ylab("DW-NOMINATE")+
  ggtitle("DW-NOMINATE by Trump Margin: All Senate")+
  scale_color_manual(name = "",
                     breaks = c("D","R"),
                     values = c("blue","red"),
                     labels = c("Democrats","Republicans"))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 10, b = 0, l = 0)),
        legend.position = "bottom")

tstmHb2 <- ggplot(house, aes(x = trump_margin, y = trump_score, col = caucus))+
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)+
  xlab("Trump Margin")+
  ylab("Trump Score")+
  ggtitle("Trump Score by Trump Margin: All House")+
  scale_color_manual(name = "",
                     breaks = c("D","R"),
                     values = c("blue","red"),
                     labels = c("Democrats","Republicans"))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 10, b = 0, l = 0)),
        legend.position = "bottom")

# check correlation ratio
cor(house$nominate[house$caucus == "D"], house$trump_margin[house$caucus == "D"]) /
cor(house$nominate[house$caucus == "R"], house$trump_margin[house$caucus == "R"])
