#Packaging logistics
library(igraph)
library(tidyverse)
library(ggalt)
library(readxl)
library(data.table)

library(survival)
library(dplyr)
library(ranger)

#Import data
d1 <- read.csv('Funding_events_7.14.csv')
d2 <- read_excel('Funding_events_7.14_page2.xlsx')
d3 <- read.csv('Venture_capital_firm_outcomes.csv')

#Unify column names, combine and change date format
colnames(d2) <- colnames(d1)
d_combined <- rbind(data.table(d1), data.table(d2))
d_combined$Deal.Date <- as.Date(d_combined$Deal.Date, '%m/%d/%y')

#Create list of dates, append list of investors, and then sort using date
inv_date <- subset(d_combined, select = c('Investors', 'Deal.Date'))
inv_date$Investors <- as.character(inv_date$Investors)
inv_date <- inv_date[order(inv_date$Deal.Date)]

#Swap out firms with names that may cause problems while parsing with commas, generate list-of-lists for investors, 
  #then parse the list-of-lists
firms <- inv_date[inv_date$Investors != '' & !is.null(inv_date$Investors) & !is.na(inv_date$Investors)]
firms$Investors <- gsub(', Inc|, Inc.|, LLC|, L[.]L[.]C|, L[.]L[.]C[.]|, Ltd|,Ltd.|, Ltd.|, Co|, Corp|, LP|, L[.]P[.]|, llc|, LTD', 
                        '', firms$Investors, ignore.case = TRUE)
inv_list <- strsplit(firms$Investors, split = ',')
dat_list <- firms$Deal.Date

#Generate edge list by locating all lists of investors with more than 1 investor, generating all possible pairs
  #of elements of each list; separately construct a corresponding list of dates for each edge and adjust format
null <- lapply(inv_list, length) > 1
firms_comb <- inv_list[null]
firms_comb <- lapply(firms_comb, combn, m = 2)
dates <- matrix(unlist(lapply(firms_comb, dim)), ncol = 2, byrow = TRUE)[,2]
count <- as.Date(as.double(rep(dat_list[null], dates)), origin = '1970-01-01')

##############################################--Q1--##############################################

#Create edge_list from combinations and graph
edge_list <- matrix(unlist(firms_comb), ncol = 2, byrow = TRUE)
edge_list <- edge_list[!duplicated(lapply(as.data.frame(t(edge_list), stringsAsFactors = FALSE), sort)),]
und_graph <- graph_from_edgelist(edge_list, directed = FALSE)

#Part A

#Calculate closeness, format into dataframe, sort and locate the most center VC firm
und_graph_clo <- as.data.frame(closeness(und_graph))
colnames(und_graph_clo) <- c('closeness')
und_graph_clo['firm'] <- rownames(und_graph_clo)
und_graph_clo <- und_graph_clo[order(und_graph_clo$closeness),]
center_firm <- und_graph_clo[und_graph_clo$closeness == max(und_graph_clo$closeness),]$firm

#Print and show
print(c('Center of the network: ', center_firm), quote = FALSE)

#Part B

#Calculate distances between all vertices, reset isolate distances to number of vertices, format into a
  #dataframe, and sort
distance <- distances(und_graph)
distance[distance == Inf] <- dim(distance)[1]
mean_distance <- as.data.frame(colSums(distance) / dim(distance)[1])
mean_distance['firm'] <- rownames(mean_distance)
colnames(mean_distance) <- c('mean', 'firm')
mean_distance <- mean_distance[order(mean_distance$mean),]

#Calculate mean distances
min_distance_firm <- mean_distance[mean_distance$mean == min(mean_distance$mean),]$firm
mean_distance_all <- mean(mean_distance$mean)

#Print, plot and show
print(c('Lowest mean distance firm: ', min_distance_firm, ', which has mean distance of ', 
        mean_distance[mean_distance$mean == min(mean_distance$mean),]$mean), quote = FALSE)
print(c('Average mean distance over all firms: ', mean_distance_all), quote = FALSE)

plot.igraph(und_graph, vertex.label=NA, #layout=layout.fruchterman.reingold, 
            vertex.label.color="black",
            edge.color='grey', vertex.size = 0.5, edge.arrow.size=1, edge.curved=FALSE)

##############################################--Q2--##############################################

#Create an edge list without de-duplication, append the dates to it, and then generate a sequence of relevant
  #months present in dataset
edge_list_dup <- matrix(unlist(firms_comb), ncol = 2, byrow = TRUE)
edge_list_wdate <- data.frame(edge_list_dup, count)
count_month <- seq.Date(from = count[1], to = tail(count[!is.na(count)], 1), by = 'month')

#Part A

#Construct an edge list containing entries up to the desired month and without NA dates, graph, and calculate
  #coreness scores
kcore1 <- c()
for (i in 1:length(unique(count_month))){
  temp_edge_list <- edge_list_wdate[(edge_list_wdate$count <= count_month[i]) & (!is.na(edge_list_wdate$count)),][,1:2]
  temp_edge_list <- as.matrix(temp_edge_list)
  temp_graph <- graph_from_edgelist(temp_edge_list, directed = FALSE)
  temp_kcore <- coreness(temp_graph)
  kcore1 <- c(kcore1, mean(temp_kcore))
}

#Plot and show
plot(kcore1, type = 'l', xlab = NA, ylab= NA)
title('Average Coreness Over Time')

#Part B

#Construct an edge list containing entries up to the desired month and without NA dates, de-duplilcate, graph,
  #and then calculate coreness scores
kcore2 <- c()
for (i in 1:length(unique(count_month))){
  temp_edge_list <- edge_list_wdate[(edge_list_wdate$count <= count_month[i]) & (!is.na(edge_list_wdate$count)),][,1:2]
  temp_edge_list <- temp_edge_list[!duplicated(temp_edge_list),]
  temp_edge_list <- as.matrix(temp_edge_list)
  temp_graph <- graph_from_edgelist(temp_edge_list, directed = FALSE)
  temp_kcore <- coreness(temp_graph)
  kcore2 <- c(kcore2, mean(temp_kcore))
}

#Plot and show
plot(kcore2, type = 'l', xlab = NA, ylab= NA)
title('Average Coreness Over Time (Unique Ties)')

#Part C

#Re-do part A: take into account tie decay, which for the first 60 months is just whatever is accumulated, and for
  #after the first 60 months is a moving window of precisely 60 months
kcore3 <- c()
for (i in 1:length(unique(count_month))){
  #If beyond the first 60 months, then aggregate all entires within a moving window of 60 months, based on the index i
  if (i %in% 61:length(unique(count_month))){
    temp_edge_list <- edge_list_wdate[(edge_list_wdate$count %in% count_month[i - 60]:count_month[i]),][,1:2]
    temp_edge_list <- as.matrix(temp_edge_list)
    temp_graph <- graph_from_edgelist(temp_edge_list, directed = FALSE)
    temp_kcore <- coreness(temp_graph)
    kcore3 <- c(kcore3, mean(temp_kcore))
  } else{
    #If in the first 60 months, simply aggregate all present entries
    temp_edge_list <- edge_list_wdate[(edge_list_wdate$count %in% count_month[1]:count_month[i]),][,1:2]
    temp_edge_list <- as.matrix(temp_edge_list)
    temp_graph <- graph_from_edgelist(temp_edge_list, directed = FALSE)
    temp_kcore <- coreness(temp_graph)
    kcore3 <- c(kcore3, mean(temp_kcore))
  }
}

#Plot and show
plot(kcore3, type = 'l', xlab = NA, ylab= NA)
title('Average Coreness Over Time (with Decay)')

##############################################--Q3--##############################################

#Part A

#Initialize empty vectors
con <- c()
range_min <- c()
prop <- c()

#Fill up the vectors: build the corresponding edge lists, graph them, sort them according to the eigenvector
  #centralities, generate all possible ideal coreness vectors, calculate all corresponding correlations, clean
  #up the correlations list, choose and record max, choose and record minimum of range of correlations, and
  #calculate and record proportion (which is just the number of 1 in the ideal coreness vector over vertex count)
for (i in 1:length(count_month)){
  #If beyond the first 60 months, then aggregate all entires within a moving window of 60 months, based on the index i
  if (i %in% 61:length(count_month)){
    temp_edge_list <- copy(edge_list_wdate)[(edge_list_wdate$count %in% count_month[i - 60]:count_month[i]),1:2]
    temp_edge_list <- as.matrix(temp_edge_list)
    temp_graph <- graph_from_edgelist(temp_edge_list, directed = FALSE)
    temp_eigen <- sort(centr_eigen(temp_graph)$vector)
    #Generate list-of-lists containing all possible ideal coreness vectors, from all possible partition blocks
      #Note that it is by definition sorted, so length-wise correlation computations are valid
    temp_list <- lapply(1:length(V(temp_graph)), function(x, y) c(rep(0, y - x), rep(1, x)), y = length(V(temp_graph)))
    #Calculate correlation between eigenvector centralities and each possible ideal coreness vector
    temp_cor_list <- unlist(lapply(temp_list, cor, y = temp_eigen))
    temp_cor_list_clean <- temp_cor_list[!is.infinite(temp_cor_list) & !is.na(temp_cor_list)]
    #Locate the index corresponding to max correlation, which is also the number of 1s in the corresponding ideal
      #coreness vector; that divided by vertex count is proportion
    temp_ind <- max(which(unlist(lapply(temp_list, cor, y = temp_eigen)) == max(temp_cor_list_clean)))
    if (length(temp_ind) == 0){temp_ind <- 0}
    con <- c(con, max(temp_cor_list_clean))
    range_min <- c(range_min, min(temp_cor_list_clean))
    prop <- c(prop, temp_ind / length(V(temp_graph)))
  } else {
    #If in the first 60 months, simply aggregate all present entries
    temp_edge_list <- copy(edge_list_wdate)[(edge_list_wdate$count %in% count_month[1]:count_month[i]),1:2]
    temp_edge_list <- as.matrix(temp_edge_list)
    temp_graph <- graph_from_edgelist(temp_edge_list, directed = FALSE)
    temp_eigen <- sort(centr_eigen(temp_graph)$vector)
    #Generate list-of-lists containing all possible ideal coreness vectors, from all possible partition blocks
      #Note that it is by definition sorted, so length-wise correlation computations are valid
    temp_list <- lapply(1:length(V(temp_graph)), function(x, y) c(rep(0, y - x), rep(1, x)), y = length(V(temp_graph)))
    #Calculate correlation between eigenvector centralities and each possible ideal coreness vector
    temp_cor_list <- unlist(lapply(temp_list, cor, y = temp_eigen))
    temp_cor_list_clean <- temp_cor_list[!is.infinite(temp_cor_list) & !is.na(temp_cor_list)]
    #Locate the index corresponding to max correlation, which is also the number of 1s in the corresponding ideal
      #coreness vector; that divided by vertex count is proportion
    temp_ind <- max(which(unlist(lapply(temp_list, cor, y = temp_eigen)) == max(temp_cor_list_clean)))
    if (length(temp_ind) == 0){temp_ind <- 0}
    con <- c(con, max(temp_cor_list_clean))
    range_min <- c(range_min, min(temp_cor_list_clean))
    prop <- c(prop, temp_ind / length(V(temp_graph)))
  }
}

#Plot, graph and show
plot(con, type = 'l', ylim = c(0,1), xlab = NA, ylab= NA)
title('Maximum Concentration Over Time')

plot(prop, type = 'l', ylim = c(0,1), xlab = NA, ylab= NA)
title('Proportion in Ideal (Theoretical) Core')

dumb <- as.data.frame(cbind(range_min, con))
ggplot(dumb, aes(x=range_min, xend=con, y=1:length(rownames(dumb)))) + geom_dumbbell() +
  ggtitle('Concentration Range Over Time')

#Part B

#Reconstruct the most recent accumulated list, then graph
recent_edge_list <- copy(edge_list_wdate)[(edge_list_wdate$count 
                                           %in% count_month[length(count_month) - 60]:count_month[length(count_month)]),1:2]
recent_edge_list <- as.matrix(temp_edge_list)
recent_graph <- graph_from_edgelist(recent_edge_list, directed = FALSE)

#Plot and show
plot.igraph(recent_graph, vertex.label=NA, #layout=layout.fruchterman.reingold, 
            vertex.label.color="black",
            edge.color='grey', vertex.size = 0.5, edge.arrow.size=1, edge.curved=FALSE)

##############################################--Q4--##############################################

#Initialize function that calculates in one batch several centrality measures, outputting the results as
  #a data table
centr_combo <- function(net){
  deg_in <- degree(net, mode = "in")
  deg_out <- degree(net, mode = "out")
  close <- closeness(net, mode = "total")
  betw <- betweenness(net)
  prank <- page_rank(net)$vector
  id <- V(net)$name
  core <- coreness(net)
  eig <- eigen_centrality(net)$value
  stats <- as.data.table(list(id = id, deg_in = deg_in, deg_out = deg_out, 
                              close = close, betw = betw, prank = prank, core = core, eig = eig))
  return(stats)
}

#Construct available investor list from the outcomes dataset
avail_inv_list <- unique(d3$firm_name)

#Generate year sequence
count_year <- seq.Date(from = count[1], to = tail(count[!is.na(count)], 1), by = 'year')

#Initialize empty dataframe and adjust formatting
cent_result <- data.frame(matrix(ncol = 9, nrow = 0))
colnames(cent_result) <- c('id', 'deg_in', 'deg_out', 'close', 'betw', 'prank', 'core', 'eig', 'year')
cent_result$year <- as.Date(cent_result$year)

#Generate the needed centrality measures, matched up with the corresponding years and company names
for (i in 1:length(count_year)){
  #If beyond the first 60 months, then aggregate all entires within a moving window of 60 months, based on the index i
  if (i*12 %in% 61:length(count_month)){
    temp_edge_list <- copy(edge_list_wdate)[(edge_list_wdate$count %in% count_month[(i-5)*12]:count_month[i * 12]),1:2]
    temp_edge_list <- as.matrix(temp_edge_list)
    temp_graph <- graph_from_edgelist(temp_edge_list, directed = FALSE)
    #Calculate, format and append
    temp_combo <- centr_combo(temp_graph)
    temp_combo$year <- count_year[i]
    cent_result <- rbind(cent_result, temp_combo)
  } else {
    #If in the first 60 months, simply aggregate all present entries
    temp_edge_list <- copy(edge_list_wdate)[(edge_list_wdate$count %in% count_month[1]:count_month[i * 12]),1:2]
    temp_edge_list <- as.matrix(temp_edge_list)
    temp_graph <- graph_from_edgelist(temp_edge_list, directed = FALSE)
    #Calculate, format and append
    temp_combo <- centr_combo(temp_graph)
    temp_combo$year <- count_year[i]
    cent_result <- rbind(cent_result, temp_combo)
  }
}

#Make a copy, just to be safe
cent_result2 <- copy(cent_result)

#Format the dataset cent_result2, which is a copy of centralization calculation results
cent_result2$year <- format(cent_result2$year, format = '%Y')
colnames(cent_result2) <- c('firm_name', 'deg_in', 'deg_out', 'close', 'betw', 'prank', 'core', 'eig', 'year')
d3_copy <- copy(d3)

#Formatting for the outcomes dataset, then join with cent_result2
d3_copy$year <- format(d3_copy$year, format = '%Y')
d3_copy$firm_name <- as.character(d3_copy$firm_name)
d4 <- inner_join(x = d3_copy, y = cent_result2, by = c('firm_name', 'year'))

#Part A

#Select only the meaningful columns
regression_data1 <- subset(d4, select = c('deg_in', 'deg_out', 'close', 'betw', 'prank', 'core', 
                                         'eig', 'successful_investments'))

#Run model, summarize, and calculate directionalities
mod1 <- glm(formula = successful_investments ~ . , data = regression_data1, family = poisson)
mod1_s <- summary(mod1)
directionality1 <- (mod1$coefficients[!is.na(mod1$coefficients)] > 0)

#Print and show
print(mod1_s)
print(directionality1)

#Part B

#Select only the meaningful columns
regression_data2 <- subset(d4, select = c('deg_in', 'deg_out', 'close', 'betw', 'prank', 'core',
                                          'eig', 'out_of_business'))

#Run model, summarize, and calculate directionalities
mod2 <- glm(formula = out_of_business ~ . , data = regression_data2, family = binomial)
mod2_s <- summary(mod2)
directionality2 <- (mod2$coefficients[!is.na(mod2$coefficients)] > 0)

#Print and show
print(mod2_s)
print(directionality2)