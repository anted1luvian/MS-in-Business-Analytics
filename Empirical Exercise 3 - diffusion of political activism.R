#Packaging logistics
library(igraph)
library(tidyverse)
library(ggalt)
library(readxl)
library(data.table)
library(dplyr)
library(plm)
library(pglm)
library(varhandle)
library(panelAR)

#Import data
d_b <- read.csv('border_information.csv')
d_d <- read.csv('district_information.csv')
d_n <- read.csv('new_parties_in_each_district_by_candidate.csv')
d_r <- read.csv('rain_information.csv')

#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#++Data++#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#

#Remove factors from all datasets
d_b <- unfactor(d_b)
d_d <- unfactor(d_d)
d_n <- unfactor(d_n)
d_r <- unfactor(d_r)

#India did not hold general elections in 1985 - there are entries in the district dataset dated to 1985 because
  #Punjab and Assam delayed their elections from 1984 to 1985 because of internal unrest
d_d[d_d$year == 1985,]$year <- 1984
d_n[d_n$year == 1985,]$year <- 1984
d_r[d_r$year == 1985,]$year <- 1984

#NA removal
d_d <- d_d[!is.na(d_d),]
d_n <- d_n[!is.na(d_n),]
d_r <- d_r[!is.na(d_r),]

#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#++Q1++#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#

#Part A

#Create a list of election years and a list of all districts involved, according to the district dataset
election_year_list <- sort(unique(d_n$year))
district_list <- sort(unique(d_d$district))

#Create a new dataframe holding rain information, election cycle year, removing all entreis after 1999,
  #which is the final election year provided, and calculate whether a year's SPI is extreme (severe)
d_r2 <- as.data.table(copy(d_r))[,cycle_year := NA][d_r$year <= 1999]
year_match <- function(a){a <- election_year_list[max(which(a > election_year_list)) + 1]}
d_r2$cycle_year <- unlist(lapply(d_r2$year, function(a) a <- election_year_list[max(which(a > election_year_list)) + 1]))
d_r2$cycle_year[is.na(d_r2$cycle_year)] <- 1951
d_r2$severe <- as.numeric(unlist(lapply(d_r2$spi, function(a) a <- abs(a) > 1)))

#Calculate aggregate rainfall levels, mean SPI, and number of severe years for each district and for each election cycle
rain_list <- d_r2 %>% 
  group_by(district, cycle_year) %>% 
  summarise(rain = sum(rain, na.rm = TRUE), spi = mean(spi, na.rm = TRUE), severe = sum(severe, na.rm = TRUE))

#Part B

#Create graph and locate vertex neighbors
border_edge <- as.matrix(d_b)
border_graph <- graph_from_edgelist(border_edge, directed = FALSE)
distance <- distances(border_graph)
distance[is.infinite(distance)] <- dim(distance)[1]
neighbors_list <- cbind(rownames(rownames(distance)), lapply(seq_along(V(border_graph)), function(a) unique((neighbors(border_graph, V(border_graph)[a])$name))))

#Calculate the mean neighbor district rain level and number of severe years
neighbors <- data.frame(matrix(ncol = 4, nrow = 0))
for (i in 1:dim(distance)[1]){
  temp_list <- names(distance[,i][distance[,i] == 1])
  temp_df <- d_r2[d_r2$district %in% temp_list,]
  for (j in 1:length(election_year_list)){
    temp_df_2 <- temp_df[temp_df$cycle_year == election_year_list[j],]
    temp_rain <- mean(unlist(lapply(unique(temp_df_2$district), FUN = function(a) sum(na.omit(temp_df_2[temp_df_2$district == a,]$rain)))))
    temp_severe <- mean(unlist(lapply(unique(temp_df_2$district), FUN = function(a) sum(na.omit(abs(temp_df_2[temp_df_2$district == a,]$spi) > 1)))))
    neighbors <- rbind(unfactor(neighbors), c(rownames(distance)[i], election_year_list[j], temp_rain, temp_severe))
  }
}
colnames(neighbors) <- c('district', 'year', 'rain_neighbors', 'severe_neighbors')
neighbors[,-1] <- lapply(neighbors[,-1], FUN = as.numeric)
colnames(rain_list) <- c('district','year', 'rain', 'spi', 'severe')
rain_party_large <- unfactor(inner_join(rain_list, d_d, by = c('district', 'year')))
rain_party_lag <- unfactor(inner_join(rain_party_large, neighbors, by = c('district', 'year')))

#Create a key for election years and add it into rain_party_lag
length_list <- c(5)
for (i in 2:length(election_year_list)){length_list <- c(length_list, election_year_list[i] - election_year_list[i-1])}
rain_party_lag$cycle_length <- 0
for (i in 1:length(election_year_list)){
  rain_party_lag[rain_party_lag$year == election_year_list[i],]$cycle_length <- length_list[i]
}

#Prepare data for regression
reg_df1 <- as.data.table(copy(rain_party_lag))
reg_df1 <- reg_df1[reg_df1$district != 'Kanyakumari',][-which(reg_df1$district == 'Amritsar' & reg_df1$year == 1984)[2],]
reg_df1[, rain_lag := c(NA, rain[-.N]), by = district]
reg_df1[, rain_neighbors_lag := c(NA, rain_neighbors[-.N]), by = district]
reg_df1[, severe_lag := c(NA, severe[-.N]), by = district]
reg_df1[, severe_neighbors_lag := c(NA, severe_neighbors[-.N]), by = district]
reg_df1[, cycle_length_lag := c(NA, cycle_length[-.N]), by = district]

#Run regression and show
lag_reg1 <- plm(rain ~ rain_lag + rain_neighbors_lag + cycle_length, data = reg_df1[complete.cases(reg_df1),], 
                effect = "twoways", model = "within", index = "district")
summary(lag_reg1)

#Part C

#Prepare data for regression
reg_df2 <- copy(reg_df1)[reg_df1$rain != 0,]

#Run regression and show
lag_reg2 <- pglm(severe ~  severe_lag + severe_neighbors_lag + cycle_length, 
                 data = reg_df2, effect = "twoways", model = "within", 
                 index = "district", family = "poisson")
summary(lag_reg2)

#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#++Q2++#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#

#Prepare data for regression
reg_df3 <- as.data.frame(copy(reg_df2))

#Run regression and show
lag_reg3 <- panelAR(new_parties ~ severe + cycle_length + year, data = reg_df3, panelVar = 'district', 
                    timeVar = 'year', autoCorr = "psar1", panelCorrMethod = "phet", rho.na.rm = TRUE)
summary(lag_reg3)

#Run regressions for each type of party, using a loop and summarizing the results plus significane into a table
coef_table <- as.data.frame(matrix(ncol = 3, nrow = 0))
for (i in c(8:12,14:23)){
  temp_var <- colnames(reg_df3)[i]
  temp_reg <- summary(panelAR(get(temp_var) ~ severe + cycle_length + year, data = reg_df3, panelVar = 'district', 
                              timeVar = 'year', autoCorr = "psar1", panelCorrMethod = "phet", rho.na.rm = TRUE))$coefficients
  coef_table <- rbind(coef_table, c(temp_reg[,1],temp_reg[,4]))
}
rownames(coef_table) <- colnames(reg_df3)[c(8:12,14:23)]
colnames(coef_table) <- c('intercept', 'severe', 'cycle_length', 'year', 'intercept_sig', 'severe_sig', 'cycle_sig', 'year_sig')
coef_table <- coef_table[order(coef_table$severe),]
coef_table[,5:8] <- as.numeric(coef_table[,5:8] <= 0.05)

#Show table
coef_table

#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#++Q3++#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#
reg_df4 <- unfactor(copy(reg_df3))

lag_reg4 <- panelAR(new_parties ~ severe + severe_neighbors_lag + cycle_length + year, data = reg_df4, 
                    panelVar = 'district', timeVar = 'year', autoCorr = "psar1", panelCorrMethod = "phet", rho.na.rm = TRUE)
summary(lag_reg4)

#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#++Q4++#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#

#Part A

#Prepare data for regression
reg_df5 <- unfactor(copy(reg_df4))

#Run regression for all 3 scopes and show
lag_reg5_1 <- panelAR(new_parties_national_scope ~ severe + severe_neighbors_lag + cycle_length + year, data = reg_df5, 
                      panelVar = 'district', timeVar = 'year', autoCorr = "psar1", panelCorrMethod = "phet", rho.na.rm = TRUE)
summary(lag_reg5_1)

lag_reg5_2 <- panelAR(new_parties_state_scope ~ severe + severe_neighbors_lag + cycle_length + year, data = reg_df5, 
                      panelVar = 'district', timeVar = 'year', autoCorr = "psar1", panelCorrMethod = "phet", rho.na.rm = TRUE)
summary(lag_reg5_2)

lag_reg5_3 <- panelAR(new_parties_regional_scope ~ severe + severe_neighbors_lag + cycle_length + year, data = reg_df5, 
                      panelVar = 'district', timeVar = 'year', autoCorr = "psar1", panelCorrMethod = "phet", rho.na.rm = TRUE)
summary(lag_reg5_3)

#Part B

#Prepare data for regression
reg_df6 <- unfactor(copy(reg_df5))

#Run regression and show
lag_reg6 <- panelAR(political_concentration ~ severe + severe_neighbors_lag + cycle_length + year, data = reg_df6, 
                    panelVar = 'district', timeVar = 'year', autoCorr = "psar1", panelCorrMethod = "phet", rho.na.rm = TRUE)
summary(lag_reg6)

#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#++Q5++#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#

#Use loops and lapply to compute the number of new parties for each district and in each election cycle, having contested
  #before or not and with independent candidates removed (counted separately)
party_table_cross <- data.frame(matrix(ncol = 4, nrow = 0))
d_n2 <- as.data.table(copy(d_n))[,cycle_year := NA][d_n$year <= 1999]
d_n2$cycle_year <- unlist(lapply(d_n2$year, function(a) a <- election_year_list[max(which(a > election_year_list)) + 1]))
d_n2$cycle_year[is.na(d_n2$cycle_year)] <- 1951
d_n2 <- d_n2[,c(1,3,5,8)]
for (i in 1:dim(distance)[1]){
  temp_list <- unlist(neighbors_list[i])
  temp_df_a <- d_n2[d_n2$district %in% temp_list,]
  temp_df_b <- d_n2[d_n2$district == rownames(distance)[i],]
  for (j in election_year_list){
    temp_df_a2 <- temp_df_a[temp_df_a$cycle_year == j,]
    temp_df_b2 <- temp_df_b[temp_df_b$cycle_year == j,]
    temp_df2_yes <- temp_df_b2[temp_df_b2$party_name %in% temp_df_a2$party_name & temp_df_b2$party_name != 'Independent',]
    temp_df2_no <- temp_df_b2[!(temp_df_b2$party_name %in% temp_df_a2$party_name) & temp_df_b2$party_name != 'Independent',]
    temp_df2_yes_ind <- temp_df_b2[temp_df_b2$name %in% temp_df_a2$name & temp_df_b2$party_name == 'Independent',]
    temp_df2_no_ind <- temp_df_b2[!(temp_df_b2$name %in% temp_df_a2$name) & temp_df_b2$party_name == 'Independent',]
    if (sum(is.na(temp_df2_yes)) != 0){temp_counter_yes <- NA} else {temp_counter_yes <- dim(temp_df2_yes)[1]}
    if (sum(is.na(temp_df2_no)) != 0){temp_counter_no <- NA} else {temp_counter_no <- dim(temp_df2_no)[1]}
    if (sum(is.na(temp_df2_yes_ind)) != 0){temp_counter_yes_ind <- NA} else {temp_counter_yes_ind <- dim(temp_df2_yes_ind)[1]}
    if (sum(is.na(temp_df2_no_ind)) != 0){temp_counter_no_ind <- NA} else {temp_counter_no_ind <- dim(temp_df2_yes_ind)[1]}
    party_table_cross <- rbind(unfactor(party_table_cross), c(district_list[i], j, 
                                                              temp_counter_no, temp_counter_no_ind, temp_counter_yes, temp_counter_yes_ind))
  }
  if (i %% 50 == 0){print(i)}
}
colnames(party_table_cross) <- c('district', 'year', 'new', "new_independent", 'not_new', 'not_new_independent')
party_table_cross[,-1] <- lapply(party_table_cross[,-1], FUN = as.numeric)

#Prepare data for regression
reg_df7 <- unfactor(inner_join(reg_df6, party_table_cross, by = c('district', 'year')))

#Run Regression and show
lag_reg7 <- panelAR(new ~ severe + severe_neighbors_lag + cycle_length + year, data = reg_df7[,-20], 
                    panelVar = 'district', timeVar = 'year', autoCorr = "psar1", panelCorrMethod = "phet", rho.na.rm = TRUE)
summary(lag_reg7)

lag_reg8 <- panelAR(not_new ~ severe + severe_neighbors_lag + cycle_length + year, data = reg_df7, 
                    panelVar = 'district', timeVar = 'year', autoCorr = "psar1", panelCorrMethod = "phet", rho.na.rm = TRUE)
summary(lag_reg8)