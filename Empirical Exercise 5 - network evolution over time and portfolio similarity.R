#Packaging logistics
library(igraph)
library(tidyverse)
library(ggalt)
library(readxl)
library(data.table)
library(hhi)
library(survival)
library(dplyr)
library(varhandle)
library(plm)
library(Matrix)
library(rgl)
library(plot3D)
library(plotly)

#Import data
d1 <- fread('company_details.csv')
d2 <- fread('deal_details.csv')
d3 <- fread('investor_details.csv')
d4 <- fread('investors_and_deals.csv')

#Rename columns for consistency
colnames(d2)[which(colnames(d2) == 'CompanyId')] <- 'CompanyID'
colnames(d2)[which(colnames(d2) == 'DealId')] <- 'DealID'
colnames(d3)[which(colnames(d3) == 'InvestorId')] <- 'InvestorID'
colnames(d4)[which(colnames(d4) == 'Investor_Id')] <- 'InvestorID'
colnames(d4)[which(colnames(d4) == 'Deal_Id')] <- 'DealID'

#Data cleaning and merging
d3 <- d3[Investor_Type == 'Venture Capital']
temp_deal_year <- d2[, substr(Deal_Date, 8, 9) %>% as.integer]
temp_deal_year[which(temp_deal_year < 20)] <- temp_deal_year[which(temp_deal_year < 20)] + 2000
temp_deal_year[which(temp_deal_year >= 20 & temp_deal_year < 200)] <- 
  temp_deal_year[which(temp_deal_year >= 20 & temp_deal_year < 200)] + 1900
d2[, Deal_Year := temp_deal_year]

d_all <- merge(d1, d2, by = 'CompanyID') %>% merge(d4, by = 'DealID') %>% 
  merge(d3, by = 'InvestorID') %>% as.data.table %>% distinct
d_all[, co_investment := .N, by = .(DealID, Deal_Year)]
d_all <- d_all[Deal_Year >= 1990 & co_investment > 1]
d_all[, Investor_Age := Deal_Year - min(Deal_Year), by = .(InvestorID)]
d_all <- d_all[Investor_Age >= 0]
year_list <- d_all$Deal_Year %>% unique %>% sort
investor_list <- data.table(InvestorID = d_all$InvestorID %>% unique)[, InvestorIndex := c(1:.N)]

d_status <- d_all[, .(InvestorID, DealID, Lead_Investor, Deal_Year)] %>% 
  left_join(investor_list, by = 'InvestorID') %>% unique %>% as.data.table
investor_pairs <- left_join(d_status[, .(DealID, InvestorIndex)], d_status[, .(DealID, InvestorIndex)], by = 'DealID') %>% as.data.table
investor_pairs_sorted <- investor_pairs[, .(InvestorIndex.x, InvestorIndex.y)] %>% apply(MARGIN = 1, FUN = sort) %>% t
investor_pairs$InvestorIndex.x <- investor_pairs_sorted[,1]
investor_pairs$InvestorIndex.y <- investor_pairs_sorted[,2]
investor_pairs <- investor_pairs[InvestorIndex.x != InvestorIndex.y] %>% unique %>% 
  left_join(d_status[, .(DealID, Deal_Year)], by = 'DealID') %>% 
  left_join(d_status[, .(DealID, Lead_Investor, InvestorIndex)], by = c('InvestorIndex.x' = 'InvestorIndex', 'DealID')) %>% 
  left_join(d_status[, .(DealID, Lead_Investor, InvestorIndex)], by = c('InvestorIndex.y' = 'InvestorIndex', 'DealID')) %>% 
  unique %>% as.data.table
colnames(investor_pairs) <- c('DealID', 'InvestorIndex1', 'InvestorIndex2', 'Deal_Year', 'Lead_Investor1', 'Lead_Investor2')

investor_status <- data.table(InvestorIndex = d_status$InvestorIndex %>% unique)
for (i in year_list){
  temp_df <- investor_pairs[Deal_Year %in% c((i-5):i)]
  temp_proportions <- temp_df[, .(Lead1 = sum(Lead_Investor1)/.N, Lead2 = sum(Lead_Investor2)), by = .(InvestorIndex1, InvestorIndex2)]
  temp_edges <- rbind(temp_proportions[, .(InvestorIndex1, InvestorIndex2, Lead1)] %>% as.matrix %>% unname, 
                      temp_proportions[, .(InvestorIndex2, InvestorIndex1, Lead2)] %>% as.matrix %>% unname) %>% as.data.table
  temp_edges <- combn(temp_edges[, .(V1, V2)] %>% unlist %>% unique, m = 2) %>% t %>% as.data.table %>% 
    left_join(temp_edges, by = c('V1', 'V2'))
  temp_adj <- temp_edges %>% spread(V2, V3) %>% as.matrix
  temp_graph <- temp_adj %>% subset(select = -V1) %>% graph_from_adjacency_matrix(mode = 'directed')
  temp_status <- data.table(InvestorIndex = colnames(temp_adj)[-1] %>% as.integer, centr_eigen(temp_graph)$vector)
  colnames(temp_status)[2] <- i
  investor_status <- investor_status %>% left_join(temp_status, by = 'InvestorIndex')
  print(i)
}
investor_status_melt <- melt(investor_status, id.vars = 'InvestorIndex') %>% as.data.table
colnames(investor_status_melt)[which(colnames(investor_status_melt) %in% c('variable', 'value'))] <- c('Deal_Year', 'Status')
investor_status_melt[, Status_lag := c(NA, Status[-.N]), by = .(InvestorIndex)]

##############################################HHI##############################################
#Calculate Herfindahl-Herschman Index
d_hhi <- d_all[, .(InvestorID, Investor_Age, DealID, CompanyID, Primary_Industry_Code, 
                   Primary_Industry_Sector,Deal_Size, Deal_Year, Deal_Number, Deal_Type_1)] %>% 
  left_join(investor_list, by = 'InvestorID') %>%
  left_join(investor_status_melt %>% unfactor, by = c('InvestorIndex', 'Deal_Year')) %>% as.data.table
d_hhi <- d_hhi[!is.na(Status)] %>% distinct

investor_hhi <- data.table(InvestorIndex = d_status$InvestorIndex %>% unique)
investor_IT <- data.table(InvestorIndex = d_status$InvestorIndex %>% unique)
investor_early <- data.table(InvestorIndex = d_status$InvestorIndex %>% unique)
investor_originate <- data.table(InvestorIndex = d_status$InvestorIndex %>% unique)
investor_exits <- data.table(InvestorIndex = d_status$InvestorIndex %>% unique)
for (i in year_list){
  temp_df <- d_hhi[Deal_Year <= i]
  temp_proportions <- temp_df[, .(Sum = sum(Deal_Size, na.rm = TRUE)), 
                              by = .(Primary_Industry_Code, InvestorIndex)][, .(Proportion = 100 * Sum / sum(Sum)), 
                                                                            by = .(InvestorIndex)][!is.nan(Proportion)]
  temp_hhi <- temp_proportions[, .(sum(Proportion^2)), by = .(InvestorIndex)]
  temp_IT <- temp_df[, sum(Primary_Industry_Sector == 'Information Technology')/.N >= 0.5, by = .(InvestorIndex)]
  temp_early <- temp_df[, sum(Deal_Type_1 %in% c('Seed Round','Early Stage VC', 'Accelerator/Incubator', 
                                                 'Angel (individual)'))/.N >= 0.5, by = .(InvestorIndex)]
  temp_originate <- temp_df[, sum(Deal_Number == 1)/.N >= 0.5, by = .(InvestorIndex)]
  temp_exits <- temp_df[, sum(Deal_Type_1 %in% c('IPO','Merger/Acquisition', 'Buyout/LBO')), by = .(InvestorIndex)]
  colnames(temp_hhi)[2] <- i
  colnames(temp_IT)[2] <- i
  colnames(temp_early)[2] <- i
  colnames(temp_originate)[2] <- i
  colnames(temp_exits)[2] <- i
  investor_hhi <- investor_hhi %>% left_join(temp_hhi, by = 'InvestorIndex')
  investor_IT <- investor_IT %>% left_join(temp_IT, by = 'InvestorIndex')
  investor_early <- investor_early %>% left_join(temp_early, by = 'InvestorIndex')
  investor_originate <- investor_originate %>% left_join(temp_originate, by = 'InvestorIndex')
  investor_exits <- investor_exits %>% left_join(temp_exits, by = 'InvestorIndex')
  print(i)
}
investor_hhi <- investor_hhi %>% melt(id.vars = 'InvestorIndex') %>% as.data.table %>% unfactor
investor_IT <- investor_IT %>% melt(id.vars = 'InvestorIndex') %>% as.data.table %>% unfactor
investor_early <- investor_early %>% melt(id.vars = 'InvestorIndex') %>% as.data.table %>% unfactor
investor_originate <- investor_originate %>% melt(id.vars = 'InvestorIndex') %>% as.data.table %>% unfactor
investor_exits <- investor_exits %>% melt(id.vars = 'InvestorIndex') %>% as.data.table %>% unfactor
colnames(investor_hhi)[2:3] <- c('Deal_Year', 'HHI')
colnames(investor_IT)[2:3] <- c('Deal_Year', 'IT')
colnames(investor_early)[2:3] <- c('Deal_Year', 'Early')
colnames(investor_originate)[2:3] <- c('Deal_Year', 'Originate')
colnames(investor_exits)[2:3] <- c('Deal_Year', 'Exits')
investor_hhi <- investor_hhi[!is.na(HHI)]
investor_IT <- investor_IT[!is.na(IT)][, IT_lag := c(NA, IT[-.N]), by = .(InvestorIndex)]
investor_early <- investor_early[!is.na(Early)][, Early_lag := c(NA, Early[-.N]), by = .(InvestorIndex)]
investor_originate <- investor_originate[!is.na(Originate)][, Originate_lag := c(NA, Originate[-.N]), by = .(InvestorIndex)]
investor_exits <- investor_exits[!is.na(Exits)][, Exits_lag := c(NA, Exits[-.N]), by = .(InvestorIndex)]

d_hhi <- d_hhi %>% left_join(investor_hhi, by = c('InvestorIndex', 'Deal_Year')) %>% 
  left_join(investor_IT, by = c('InvestorIndex', 'Deal_Year')) %>%
  left_join(investor_early, by = c('InvestorIndex', 'Deal_Year')) %>% 
  left_join(investor_originate, by = c('InvestorIndex', 'Deal_Year')) %>% 
  left_join(investor_exits, by = c('InvestorIndex', 'Deal_Year')) %>% as.data.table %>% distinct

##############################################Portfolio Similarity##############################################
#Calculate portfolio similarity using Jaccard distances
J <- function(a){
  temp_cross <- tcrossprod(a)
  temp_ind <- which(temp_cross > 0, arr.ind = TRUE, useNames = FALSE)
  temp_rowsum <- rowSums(a)
  temp_cross_ind <- temp_cross[temp_ind]
  sparseMatrix(i = temp_ind[,1], j = temp_ind[,2], x = temp_cross_ind / 
                 (temp_rowsum[temp_ind[,1]] + temp_rowsum[temp_ind[,2]] - temp_cross_ind), dims = dim(temp_cross))
}

d_sector <- d_all[, .(InvestorID, DealID, Deal_Size, Deal_Year, Primary_Industry_Code, Primary_Industry_Sector)] %>%
  left_join(investor_list, by = 'InvestorID') %>% 
  left_join(data.table(Primary_Industry_Code = d_all$Primary_Industry_Code %>% unique)[, PICIndex := c(1:.N)], 
            by = 'Primary_Industry_Code') %>% distinct %>%
  left_join(data.table(Primary_Industry_Sector = d_all$Primary_Industry_Sector %>% unique)[, PISIndex := c(1:.N)], 
            by = 'Primary_Industry_Sector') %>% as.data.table %>% distinct

investor_niche <- data.table(InvestorID = d_sector$InvestorID %>% unique)
investor_similarity <- c()
for (i in year_list){
  temp_df <- d_sector[Deal_Year <= i]
  temp_active <- temp_df[Deal_Year == i]$InvestorID %>% unique
  temp_edges <- temp_df[InvestorID %in% temp_active, list(DealID = DealID, PICIndex = PICIndex, InvestorID = InvestorID)]
  temp_edges <- left_join(temp_edges, temp_edges, by = 'InvestorID')
  temp_edges <- temp_edges[temp_edges$DealID.x != temp_edges$DealID.y,]
  temp_edges <- data.table(temp_edges[, c(2,5)])
  colnames(temp_edges) <- c('PICIndex1', 'PICIndex2')
  if (dim(temp_edges)[1] != 0){
    temp_aff_industry <- dcast(temp_edges, PICIndex1 ~ PICIndex2, value.var = 'PICIndex2', fun.aggregate = length)
    temp_count <- dim(temp_aff_industry)[1]
    temp_industry <- temp_aff_industry[,1]
    temp_aff_industry <- as.numeric(as.matrix(temp_aff_industry[,-1])) %>% matrix(ncol = temp_count, byrow = TRUE)
    temp_jaccard_industry <- as.matrix(1 - (J(temp_aff_industry > 0)) + 0)
    temp_jaccard_industry <- cbind(temp_industry, temp_jaccard_industry) %>% as.data.table
    colnames(temp_jaccard_industry) <- c("PICIndex", unlist(temp_industry))
    temp_jaccard_industry_melt <- melt(temp_jaccard_industry, id.vars = 'PICIndex') %>% unfactor
    temp_jaccard_industry_melt <- temp_jaccard_industry_melt[PICIndex != variable]
    temp_jaccard_industry_melt <- temp_jaccard_industry_melt[temp_jaccard_industry_melt$PICIndex < temp_jaccard_industry_melt$variable,]
    temp_edges2 <- temp_df[InvestorID %in% temp_active, list(DealID = DealID, PICIndex = PICIndex, InvestorID = InvestorID)]
    temp_edges2 <- left_join(temp_edges2, temp_edges2, by='InvestorID') %>% as.data.table
    temp_edges2 <- temp_edges2[temp_edges2$DealID.x != temp_edges2$DealID.y]
    temp_distances <- temp_edges2 %>% left_join(temp_jaccard_industry_melt, by = 
                                                  c('PICIndex.x' = 'PICIndex','PICIndex.y' = 'variable'))
    temp_df2 <- na.omit(temp_distances) %>% as.data.table
    temp_distances <- temp_df2[, .(Count = .N), by = .(InvestorID, PICIndex.x, PICIndex.y, value)]
    temp_distances[, Niche_Width := 1 - (1/(1 + sum(value)/(.N - 1))), by = .(InvestorID)] %>% na.omit
    temp_distances <- temp_distances[, .(InvestorID, Niche_Width)] %>% distinct
    colnames(temp_distances)[2] <- i
    investor_niche <- investor_niche %>% left_join(temp_distances %>% distinct, by = 'InvestorID')
  }
  print(i)
}
for (i in year_list){
  temp_df <- d_sector[Deal_Year <= i]
  temp_active <- temp_df[Deal_Year == i]$InvestorID %>% unique
  temp_edges <- temp_df[InvestorID %in% temp_active, .(Count = .N), by = .(InvestorID, PISIndex)]
  if (dim(temp_edges)[1] != 0){
    temp_aff_sector <- temp_edges %>% spread(key = PISIndex, value = Count) %>% mutate_each(funs(replace(., which(is.na(.)), 0)))
    temp_investor <- temp_aff_sector[,1]
    temp_aff_sector <- as.matrix(temp_aff_sector[,-1])
    temp_jaccard_sector <- 1 - J(temp_aff_sector > 0)
    temp_cmd <- cbind(temp_investor, i, cmdscale(temp_jaccard_sector)) %>% as.data.table
    colnames(temp_cmd) <- c('InvestorID', 'Deal_Year', 'Coordinate1', 'Coordinate2')
    investor_similarity <- investor_similarity %>% rbind(temp_cmd)
  }
  print(i)
}
investor_niche <- investor_niche %>% melt(id.vars = 'InvestorID') %>% as.data.table %>% unfactor
colnames(investor_niche)[2:3] <- c('Deal_Year', 'Niche_Width')
investor_niche <- investor_niche[!is.na(Niche_Width)]
investor_similarity$Deal_Year <- investor_similarity$Deal_Year %>% as.integer

d_sector <- d_sector %>% left_join(investor_niche, by = c('InvestorID', 'Deal_Year')) %>% as.data.table
d_sector <- d_sector[!is.na(Niche_Width)]
d_sector[, Niche_Width_lag := c(NA, Niche_Width[-.N]), by = .(InvestorIndex)]

##############################################Medoids##############################################
#Calculate sector medoids ('medians') for each year, i.e. companies that 
#invest the most in that sector in that year

d_q1 <- merge(d_hhi, d_sector) %>% distinct
d_q1[, Status_lag_mean := mean(Status_lag), by = .(InvestorIndex)]
d_q1[, Originate_lag_mean := mean(Originate_lag), by = .(InvestorIndex)]
d_q1[, IT_lag_mean := mean(IT_lag), by = .(InvestorIndex)]
d_q1[, Early_lag_mean := mean(Early_lag), by = .(InvestorIndex)]
d_q1[, Investor_Age_mean := mean(Investor_Age), by = .(InvestorIndex)]

d_q2 <- copy(d_q1)

d_q3_temp <- copy(d_q1) %>% left_join(investor_similarity %>% distinct, by = c('InvestorID', 'Deal_Year')) %>% 
  distinct %>% as.data.table
d_q3_temp$Coordinate1 <- d_q3_temp$Coordinate1 %>% as.numeric
d_q3_temp$Coordinate2 <- d_q3_temp$Coordinate2 %>% as.numeric
investor_medoid_x <- data.table(PICIndex = d_q3_temp$PICIndex) %>% distinct
investor_medoid_y <- data.table(PICIndex = d_q3_temp$PICIndex) %>% distinct
for (i in year_list){
  temp_df <- d_q3_temp[Deal_Year <= i]
  temp_sums <- temp_df[, .(Sum = sum(Deal_Size, na.rm = TRUE)), by = .(PICIndex, InvestorID)]
  temp_max <- temp_sums[, .(InvestorID = InvestorID[1], Max = max(Sum)), by = .(PICIndex)]
  temp_x <- temp_max[, .(PICIndex, InvestorID)] %>% 
    left_join(investor_similarity[Deal_Year == i, .(InvestorID, Coordinate1)], by = 'InvestorID')
  temp_y <- temp_max[, .(PICIndex, InvestorID)] %>% 
    left_join(investor_similarity[Deal_Year == i, .(InvestorID, Coordinate2)], by = 'InvestorID')
  colnames(temp_x)[3] <- i
  colnames(temp_y)[3] <- i
  investor_medoid_x <- investor_medoid_x %>% left_join(temp_x[, -2], by = 'PICIndex') %>% as.data.table
  investor_medoid_y <- investor_medoid_y %>% left_join(temp_y[, -2], by = 'PICIndex') %>% as.data.table
  print(i)
}
investor_medoid_x <- investor_medoid_x %>% melt(id.vars = 'PICIndex') %>% unfactor %>% as.data.table
investor_medoid_y <- investor_medoid_y %>% melt(id.vars = 'PICIndex') %>% unfactor %>% as.data.table
colnames(investor_medoid_x)[2:3] <- c('Deal_Year', 'Medoid_Coordinate1')
colnames(investor_medoid_y)[2:3] <- c('Deal_Year', 'Medoid_Coordinate2')
investor_medoid_x <- investor_medoid_x[!is.na(Medoid_Coordinate1)]
investor_medoid_y <- investor_medoid_y[!is.na(Medoid_Coordinate2)]
investor_medoid_x$Medoid_Coordinate1 <- investor_medoid_x$Medoid_Coordinate1 %>% as.numeric
investor_medoid_y$Medoid_Coordinate2 <- investor_medoid_y$Medoid_Coordinate2 %>% as.numeric
investor_medoid <- merge(investor_medoid_x, investor_medoid_y, by = c('PICIndex', 'Deal_Year'), all = TRUE) %>% as.data.table
investor_medoid <- investor_medoid[!is.na(Medoid_Coordinate1)]
d_q3 <- left_join(d_q3_temp, investor_medoid, by = c('PICIndex', 'Deal_Year')) %>% distinct %>% as.data.table
d_q3[, Medoid_Distance := sqrt((Coordinate1 - Medoid_Coordinate1)^2 + (Coordinate2 - Medoid_Coordinate2)^2)]
d_q3[, Sum_Syndicate := sum(Medoid_Distance), by = .(DealID)][, Sum_Count := .N, by = .(DealID)]
d_q3[, Mean_Syndicate := (Sum_Syndicate - Medoid_Distance)/(Sum_Count - 1), by = .(InvestorIndex, Deal_Year)]
d_q3[, Mean_Self := mean(Medoid_Distance), by = .(InvestorIndex, Deal_Year)]
d_q3 <- d_q3 %>% subset(select = -c(Sum_Syndicate, Sum_Count)) %>% 
  left_join(d_all[, .(InvestorID, DealID, Deal_Year, Lead_Investor)], by = c('InvestorID', 'DealID', 'Deal_Year')) %>% 
  distinct %>% as.data.table
d_q3 <- d_q3[Lead_Investor == 1]

#---#---#---#---#---#---#---#---#---#---#---#---#---#---Q1---#---#---#---#---#---#---#---#---#---#---#---#---#---#

#Part A
reg1 <- plm(HHI ~ Status_lag + I(Status_lag^2) + Originate_lag + IT_lag + Early_lag + Investor_Age + Deal_Year, data = d_q1, 
            effect = "individual", model = "within", index = "InvestorIndex")
summary(reg1)

#Part B
reg2 <- glm(Niche_Width ~ Status_lag + I(Status_lag^2) + Originate_lag + IT_lag + Early_lag + Investor_Age + Deal_Year + 
              Status_lag_mean + Originate_lag_mean + IT_lag_mean + Early_lag_mean + Investor_Age_mean, data = d_q1, 
            family = quasibinomial(link = 'logit'))
summary(reg2)

#Part C
control <- 10
Status_lag_sample <- d_q1[!is.na(Status_lag) & Niche_Width >= 0 & Niche_Width <= 1][, .(Niche_Width, Status_lag)]
samples_index <- c(sample(1:dim(Status_lag_sample)[1], 5000 - 2*control), 
                   which(Status_lag_sample$Status_lag == min(Status_lag_sample$Status_lag))[1:control], 
                   which(Status_lag_sample$Status_lag == max(Status_lag_sample$Status_lag))[1:control])
Status_lag_sample <- Status_lag_sample[samples_index]
reg3 <- glm(Niche_Width ~ Status_lag + I(Status_lag^2), 
            data = d_q1[-samples_index, .(Niche_Width, Status_lag)][Niche_Width >= 0 & Niche_Width <= 1], 
            family = quasibinomial(link = 'logit'))
reg3_prediction <- predict(reg3, newdata = Status_lag_sample, type = 'response', se.fit = TRUE)
Status_predicted <- cbind(Status_lag_sample, Prediction = reg3_prediction$fit) %>% as.data.table
Status_predicted[, min := Prediction - reg3_prediction$se.fit*qnorm(0.975)][, max := Prediction + reg3_prediction$se.fit*qnorm(0.975)]
#plot(Status_lag_sample$Niche_Width, Status_lag_sample$Status_lag)

ggplot(data = Status_predicted) + 
  geom_smooth(aes(x = Status_lag, y = Prediction), se = TRUE)

#---#---#---#---#---#---#---#---#---#---#---#---#---#---Q2---#---#---#---#---#---#---#---#---#---#---#---#---#---#

#Part A
reg4 <- plm(Exits ~ Niche_Width_lag + Status_lag + Niche_Width_lag:Status_lag + Originate_lag + IT_lag + 
              Early_lag + Investor_Age + Deal_Year, data = d_q2, effect = "individual", model = "within", index = "InvestorIndex")
summary(reg4)

#Part B
reg5 <- glm(Exits ~ Niche_Width_lag + Status_lag, data = d_q2, family = 'poisson')
summary(reg5)

niche_lag_grid <- expand.grid(seq(0, 1, length.out = 200), seq(0, 1, length.out = 200)) %>% as.data.table
colnames(niche_lag_grid) <- c('Niche_Width_lag', 'Status_lag')
niche_lag_grid <- niche_lag_grid %>% data.table(Prediction = predict(reg5, newdata = niche_lag_grid, type = 'response'))

# regular 3d plot 
scatter3D(niche_lag_grid$Niche_Width_lag, niche_lag_grid$Status_lag, niche_lag_grid$Prediction)
plot3d(niche_lag_grid$Niche_Width_lag, niche_lag_grid$Status_lag, niche_lag_grid$Prediction)

reg5_contour <- plot_ly(niche_lag_grid, x = ~Status_lag, y = ~Niche_Width_lag, z = ~Prediction, type = 'contour', autocontour = FALSE, 
                        contours = list(end = max(niche_lag_grid$Prediction, na.rm = TRUE), 
                                        size = abs(max(niche_lag_grid$Prediction, na.rm = TRUE) - min(niche_lag_grid$Prediction, na.rm = TRUE))/20, 
                                        start = min(niche_lag_grid$Prediction, na.rm = TRUE), showlines = FALSE ), 
                        line = list(smoothing = 0.85), colorscale = "Greys" ) %>% #layout(font = cmodern) %>% 
  colorbar(len = 1, nticks = 10, title = "Estimated successful \n investments") %>% 
  layout(yaxis = list(title = "Niche width")) %>% layout(xaxis = list(title = "Status")) 

#---#---#---#---#---#---#---#---#---#---#---#---#---#---Q3---#---#---#---#---#---#---#---#---#---#---#---#---#---#

#Part A
reg6 <- plm(Mean_Syndicate ~ Status_lag + Mean_Self + Status_lag:Mean_Self + Originate_lag + IT_lag + Early_lag + Investor_Age + 
              Deal_Year, data = d_q3, effect = "individual", model = "within", index = "InvestorIndex")
summary(reg6)

#Part B
reg7 <- glm(Mean_Syndicate ~ Status_lag + Mean_Self, data = d_q3, family = quasibinomial(link = 'logit'))
summary(reg7)

niche_lag_grid2 <- expand.grid(seq(min(d_q3$Mean_Self[!is.na(d_q3$Mean_Self)]), max(d_q3$Mean_Self[!is.na(d_q3$Mean_Self)]), length.out = 200), 
                               seq(min(d_q3$Status_lag[!is.na(d_q3$Status_lag)]), max(d_q3$Status_lag[!is.na(d_q3$Status_lag)]), length.out = 200)) %>% as.data.table
colnames(niche_lag_grid2) <- c('Mean_Self', 'Status_lag')
niche_lag_grid2 <- niche_lag_grid2 %>% cbind(data.table(Prediction = predict(reg7, newdata = niche_lag_grid2, type = 'response')))

scatter3D(niche_lag_grid2$Status_lag, niche_lag_grid2$Mean_Self, niche_lag_grid2$Prediction)
plot3d(niche_lag_grid2$Status_lag, niche_lag_grid2$Mean_Self, niche_lag_grid2$Prediction)

reg7_contour <- plot_ly(niche_lag_grid2, x = ~Status_lag, y = ~Mean_Self, z = ~Prediction, type = 'contour', autocontour = FALSE, 
                        contours = list(end = max(niche_lag_grid2$Prediction, na.rm = TRUE), 
                                        size = abs(max(niche_lag_grid2$Prediction, na.rm = TRUE) - min(niche_lag_grid2$Prediction, na.rm = TRUE))/20, 
                                        start = min(niche_lag_grid2$Prediction, na.rm = TRUE), showlines = FALSE), 
                        line = list(smoothing = 0.85), colorscale = "Greys" ) %>%
  colorbar(len = 1, nticks = 10, title = "Mean Syndicated Partner Distances") %>% 
  layout(yaxis = list(title = "Mean Distance (self)")) %>% layout(xaxis = list(title = "Status")) 
