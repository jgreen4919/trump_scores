# state and congressional district stuff

# read in data for state and non-at-large cd land area
statearea <- read.csv("~/Desktop/demprim/statearea.csv", stringsAsFactors = FALSE)
distarea <- read.csv("~/Desktop/demprim/natl_landarea_cd_delim.txt", stringsAsFactors = FALSE)

statearea$sqmiland <- as.numeric(gsub(",","",statearea$sqmiland)) # cast square miles of land area as numeric
names(distarea) <- c("stcode","district","land.area") # rename columns to make common with other sets later

stcodes <- statearea$stcode # store fips

distarea <- merge(distarea, statearea[,c("state","stcode")], by = "stcode") # append state to district area

# build set of state name, abbv, and fips
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

states <- data.frame(cbind(statename, stateabbv))
states <- merge(states, statearea[,c("state","stcode")], by.x = "statename",by.y = "state")

# append state abbreviation to district area and build district abbreviation
distarea <- merge(distarea, states[,c("statename","stateabbv")], by.x = "state", by.y = "statename")
distarea$district <- with(distarea, ifelse(nchar(district) == 1, paste("0",district, sep = ""), district))
distarea$distabbv <- with(distarea, paste(stateabbv,"-", district, sep = ""))

# append state abbreviation to state area
statearea <- merge(statearea, states[,c("statename","stateabbv")], by.x = "state", by.y = "statename")

distarea.al <- rbind(distarea[,c(1,2)],statearea[c(2,8,27,35,42,46,51),c(1,2)]) # new district set with at large states added

# Add back district area data from non-at-large districts
for(i in 1:nrow(distarea)){
  distarea.al$district[i] <- distarea$district[i]
  distarea.al$land.area[i] <- distarea$land.area[i]
  distarea.al$district[i] <- distarea$district[i]
  distarea.al$stateabbv[i] <- distarea$stateabbv[i]
  distarea.al$district[i] <- distarea$district[i]
  distarea.al$distabbv[i] <- distarea$distabbv[i]
}

# add district area data from at large districts
distarea.al[which(distarea.al$state == "Alaska"), 3:6] <- c("AL",
                                                            statearea$sqmiland[which(statearea$state == "Alaska")],
                                                            "AK",
                                                            "AK-AL")
distarea.al[which(distarea.al$state == "Delaware"), 3:6] <- c("AL",
                                                              statearea$sqmiland[which(statearea$state == "Delaware")],
                                                              "DE",
                                                              "DE-AL")
distarea.al[which(distarea.al$state == "Montana"), 3:6] <- c("AL",
                                                             statearea$sqmiland[which(statearea$state == "Montana")],
                                                             "MT",
                                                             "MT-AL")
distarea.al[which(distarea.al$state == "North Dakota"), 3:6] <- c("AL",
                                                                  statearea$sqmiland[which(statearea$state == "North Dakota")],
                                                                  "ND",
                                                                  "ND-AL")
distarea.al[which(distarea.al$state == "South Dakota"), 3:6] <- c("AL",
                                                                  statearea$sqmiland[which(statearea$state == "South Dakota")],
                                                                  "SD",
                                                                  "SD-AL")
distarea.al[which(distarea.al$state == "Vermont"), 3:6] <- c("AL",
                                                             statearea$sqmiland[which(statearea$state == "Vermont")],
                                                             "VT",
                                                             "VT-AL")
distarea.al[which(distarea.al$state == "Wyoming"), 3:6] <- c("AL",
                                                             statearea$sqmiland[which(statearea$state == "Wyoming")],
                                                             "WY",
                                                             "WY-AL")

distarea.al <- distarea.al[,c(6,4)] # pare down to district abbreviation and land area
distarea.al$land.area <- as.numeric(gsub(",","", distarea.al$land.area ))

write.csv(distarea.al, "~/Desktop/demprim/district_land_area.csv")

# append to the rest of the house data
hmarg <- read.csv("~/Desktop/gillibrand/trump_scores/trump_scores/trump_score_full/housemargins.csv", stringsAsFactors = FALSE)
hmarg <- merge(hmarg, distarea.al, by.x = "district", by.y = "distabbv")
hmarg$popdens <- with(hmarg, pop / land.area)

write.csv(hmarg, "~/Desktop/demprim/housemargins.csv")

