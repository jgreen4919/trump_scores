# Trump score analysis senate house

library(ggplot2)
library(MASS)
library(cowplot)
library(car)

set.seed(12345)

statename <- c("Alabama","Alaska","Arizona","Arkansas","California",
               "Colorado","Connecticut","Delaware","District of Columbia",
               "Florida","Georgia","Hawaii","Idaho","Illinois",
               "Indiana","Iowa","Kansas","Kentucky","Louisiana",
               "Maine","Maryland","Massachusetts","Michigan",
               "Minnesota","Mississippi","Missouri","Montana",
               "Nebraska","Nevada","New Hampshire","New Jersey",
               "New Mexico","New York","North Carolina","North Dakota",
               "Ohio","Oklahoma","Oregon","Pennsylvania","Rhode Island",
               "South Carolina","South Dakota","Tennessee","Texas","Utah",
               "Vermont","Virginia","Washington","West Virginia",
               "Wisconsin","Wyoming")
stateabbv <- c("AL","AK","AZ","AR","CA","CO","CT","DE","DC",
               "FL","GA","HI","ID","IL","IN","IA","KS","KY","LA",
               "ME","MD","MA","MI","MN","MS","MO","MT",
               "NE","NV","NH","NJ","NM","NY","NC","ND",
               "OH","OK","OR","PA","RI","SC","SD","TN","TX","UT",
               "VT","VA","WA","WV","WI","WY")

# Read in the data
cong <- read.csv("~/Desktop/gillibrand/trump_score_full/trump-score.csv", stringsAsFactors = FALSE)
congd1 <- read.csv("~/Desktop/gillibrand/trump_score_full/cong_dim1.csv", stringsAsFactors = FALSE)

cong$chamber <- tolower(cong$chamber)

cong$member_short <- tolower(cong$member_short)
cong$member_short <- gsub("^.*\\. ","", cong$member_short)
cong$member_short <- gsub("[*].*$","",cong$member_short)

cong$state <- factor(substr(cong$district, start = 1, stop = 2))

congd1$member_short <- iconv(congd1$member_short, from="UTF-8", to="LATIN1")
congd1$member_short <- tolower(congd1$member_short)

congd1$party[congd1$party == "Republican"] <- "R"
congd1$party[congd1$party == "Democrat"] <- "D"
congd1$party[congd1$party == "Independent"] <- "I"

congd1$state <- unlist(sapply(congd1$state, function(x){
  stateabbv[which(statename == x)]
}))
congd1$state <- as.factor(congd1$state)

congd1$chamber <- tolower(congd1$chamber)

congall <- merge(cong, congd1[,c("chamber", "party", "state", "member_short","nominate")], by = c("chamber", "party", "state", "member_short"))

congall$caucus <- congall$party
congall$caucus[congall$member_short %in% c("sanders","king") & congall$chamber == "senate"] <- "D"

congall <- congall[!is.na(congall$trump_score),]

# Visualize
chamber_names <- c("house" = "House","senate" = "Senate")

tsd1 <- ggplot(congall, aes(x = nominate, y = trump_score, col = caucus))
tsd1 + geom_point() + facet_grid(chamber ~. , labeller = as_labeller(chamber_names)) +
  geom_smooth(method = "lm")+
  xlab("DW-NOMINATE First Dimension")+
  ylab("Trump Score")+
  scale_color_manual(name = "Caucus",
                     breaks = c("D","R"),
                     values = c("blue","red"),
                     labels = c("Democratic","Republican"))+
  ggtitle("Ideology is associated with Democratic, but not Republican, Trump Scores")


tstm <- ggplot(congall, aes(x = trump_margin, y = trump_score, col = caucus))
tstm + geom_point() + facet_grid(chamber ~. , labeller = as_labeller(chamber_names)) +
  geom_smooth(method = "lm")+
  xlab("Trump Margin")+
  ylab("Trump Score")+
  scale_color_manual(name = "Caucus",
                     breaks = c("D","R"),
                     values = c("blue","red"),
                     labels = c("Democratic","Republican"))+
  ggtitle("Democrats are more responsive to Trump's performance")+
  theme_bw()
