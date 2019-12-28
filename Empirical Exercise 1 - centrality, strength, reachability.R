#################################################################################################################################
#Packaging Logistics
library(igraph)
library(Matrix)
library(data.table)

#Import File
d <- read.csv("classroom_social_and_task_network.csv")
names(d) <- c('outbound', 'inbound', 'social', 'task')
d <- data.table(d)

#Separate into purely social and purely task ties
d_social <- subset(d, select = -task)
d_task <- subset(d, select = -social)

#Create affinity matrices (in the form of adjacency matrices) from edge lists, by hand
  #Initialize an empty matrix, 22x22
matemp <- matrix(0, nrow = 22, ncol = 22)

#Reorganize edge lists as affinity matrices
d_social_aff <- copy(matemp)
for (i in 1:nrow(d_social)){
  temp_row <- d_social[i]
  col_ind <- temp_row$inbound
  row_ind <- temp_row$outbound
  d_social_aff[row_ind, col_ind] <- temp_row$social
}

d_task_aff <- copy(matemp)
for (i in 1:nrow(d_social)){
  temp_row <- d_task[i]
  col_ind <- temp_row$inbound
  row_ind <- temp_row$outbound
  d_task_aff[row_ind, col_ind] <- temp_row$task
}

#Graph and plot both matrices: the relationship strengths will enter the graph objects as weights
d_social_graph <- graph.adjacency(as.matrix(d_social_aff), "directed", weighted = TRUE)
d_task_graph <- graph.adjacency(as.matrix(d_task_aff), "directed", weighted = TRUE)

plot.igraph(d_social_graph, vertex.label=V(d_social_graph), layout=layout.fruchterman.reingold, vertex.label.color="black",
            vertex.color = 'gold',
            edge.color="black", edge.width=E(d_social_graph)$weight, vertex.size = 12, edge.arrow.size=.3, edge.curved = FALSE)
plot.igraph(d_task_graph, vertex.label=V(d_task_graph), layout=layout.fruchterman.reingold, vertex.label.color="black",
            vertex.color = 'gold',
            edge.color="black", edge.width=E(d_task_graph)$weight, vertex.size = 12, edge.arrow.size=.3, edge.curved = FALSE)

#Functions for use below
#Function: testing strong triadic closure and violations
  #The inputs:
    #graph = the graph object to be entered
    #measure = the measure of center, e.g. mean or median, to use in determining relative strength
    #attribute = the attribute(s) of the graph to use
    #types = how many types of edges does the graph have
  #The outputs:
    #vertex = a list of vertices that violate strong triadic closure
    #triad = a list of triads, in terms of vertices, that violate strong triadic closure
STCV <- function(graph, measure, attribute, types){
  mat <- c()
  strength <- c()
  false_triad <- c()
  false_vertex <- c()
  #Reconstruct adjacency matrices and record relative strength
  for (i in 1:types){
    mat2 <- as.matrix(as_adjacency_matrix(graph, attr = attribute[i]))
    mat2[is.na(mat2)] <- 0
    criterion <- measure(get.edge.attribute(graph,attribute[i])[get.edge.attribute(graph, attribute[i]) != 0], na.rm=T)
    if (!is.null(strength)){strength <- strength + (mat2 > criterion)} else {strength <- mat2 > criterion}
  }
  #Any strong constitutes strong, so add transpose
  strength <- ((strength + t(strength)) > 0) + 0
  for (i in 1:types){
    if (!is.null(mat)){
      mat3 <- as.matrix(as_adjacency_matrix(graph, attr = attribute[i]))
      mat3[is.na(mat3)] <- 0
      mat <- mat + mat3
    }
    else{
      mat <- as.matrix(as_adjacency_matrix(graph, attr = attribute[i]))
      mat[is.na(mat)] <- 0
    }
  }
  #Check for violations
  for (j in 1:dim(mat)[1]){
    if (sum(strength[j,]) >= 2){
      check <- FALSE
      find_all <- combn(which(strength[j,] > 0), 2)
      for(i in 1:dim(find_all)[2]){
        if ((mat[find_all[1,i], find_all[2,i]]+mat[find_all[2,i], find_all[1,i]]) == 0){
          check <- TRUE
          false_triad <- c(false_triad, j, find_all[1,i], find_all[2,i])
        }
      }
      if (check){false_vertex <- c(false_vertex, j)}
    }
  }
  #Output
  if (!is.null(false_triad)){false_triad <- matrix(false_triad, ncol = 3, byrow=TRUE)}
  list(vertex = false_vertex, triad = false_triad)
}

#Function: graphing strong triadic closure and violations
  #The inputs:
    #graph = the graph object to be entered
    #measure = the measure of middle, e.g. mean or median, to use in determining relative strength
    #attribute = the attribute(s) of the graph to use
    #types = how many types of edges does the graph have
  #The output:
    #No explicit output but plots the graph, with violating vertices and edges in red
STCV.plot <- function(graph, measure, attribute, types){
  edge <- E(graph)
  vertex <- V(graph)
  edge_color <- c()
  vertex_color <- c()
  edge_violation <- c()
  #Call STCV first to generate a list of violating vertices and triads
  STCV_test <- STCV(graph, measure, attribute, types)
  for (i in 1:length(vertex)){
    if (i %in% STCV_test$vertex){vertex_color <- c(vertex_color, 'red')} else {vertex_color <- c(vertex_color, 'grey')}
  }
  #Record violations
  for (i in 1:nrow(STCV_test$triad)){
    edge_violation <- c(edge_violation, E(graph, P = c(STCV_test$triad[i,1], STCV_test$triad[i,2])))
    edge_violation <- c(edge_violation, E(graph, P = c(STCV_test$triad[i,1], STCV_test$triad[i,3])))
  }
  #Color edges using violation status
  for (i in 1:length(E(graph))){
    if (edge[i] %in% edge_violation){edge_color <- c(edge_color,'red')} else {edge_color <- c(edge_color, 'grey')}
  }
  #Plot
  plot.igraph(graph, vertex.label=vertex, layout=layout.fruchterman.reingold, vertex.color = vertex_color,
              vertex.label.color="black", edge.color=edge_color, edge.width=edge$weight,
              vertex.size = 12, edge.arrow.size=.3, edge.curved=FALSE)
}

#################################################################################################################################
#Q1.1: generate degrees and centrality measures
social_indegree <- degree(d_social_graph, mode = 'in')
task_indegree <- degree(d_task_graph, mode = 'in')
social_outdegree <- degree(d_social_graph, mode = 'out')
task_outdegree<- degree(d_task_graph, mode = 'out')

#Calculate using igraph functions
social_closeness <- closeness(d_social_graph, mode = 'total')
task_closeness <- closeness(d_task_graph, mode = 'total')

social_betweenness <- betweenness(d_social_graph)
task_betweenness <- betweenness(d_task_graph)

social_pagerank <- page_rank(d_social_graph)$vector
task_pagerank <- page_rank(d_task_graph)$vector

#Q1.2: correlation computations
cor_indegree <- cor(social_indegree, task_indegree)
cor_outdegree <- cor(social_outdegree, task_outdegree)

cor_closeness <- cor(social_closeness, task_closeness)

cor_betweenness <- cor(social_betweenness, task_betweenness)

cor_pagerank <- cor(social_pagerank, task_pagerank)

#Print and show
print(c('Indegree correlation: ', cor_indegree), quote = FALSE)
print(c('Outdegree correlation: ', cor_outdegree), quote = FALSE)
print(c('Closeness correlation: ', cor_closeness), quote = FALSE)
print(c('Betweenness correlation: ', cor_betweenness), quote = FALSE)
print(c('PageRank correlation: ', cor_pagerank), quote = FALSE)

#################################################################################################################################
#Q2: test for strong triadic closure

#Join social and task graphs, then count how many triads exist in total
both <- union(d_social_graph,d_task_graph)
triad_count <- sum(triad_census(both))

#Mean: call STCV to list violating vertices and triads, then plot accordingly
mean_check <- STCV(both, measure = mean, attribute=c('weight_1','weight_2'), types = 2)
STCV.plot(both, measure = mean, attri=c('weight_1','weight_2'), types = 2)

#Calculate programmatically, using mean:
  #how many vertices violate strong triadic closure
  #how many triads violate strong triadic closure
  #what proportion of traids violate strong triadic closure
mean_vertex_count <- length(mean_check$vertex)
mean_triad_count <- nrow(mean_check$triad)
mean_prop <- nrow(mean_check$triad)/triad_count

#Print and show
print(c('Number of violating vertices (mean): ', mean_vertex_count), quote = FALSE)
print(c('Number of violating triads (mean): ', mean_triad_count), quote = FALSE)
print(c('Proportion of violating triads (mean): ', mean_prop), quote = FALSE)

#Median: call STCV to list violating vertices and triads, then plot accordingly
median_check <- STCV(both, measure = median, attribute=c('weight_1','weight_2'), types = 2)
STCV.plot(both, measure = median, attri=c('weight_1','weight_2'), types = 2)

#Calculate programmatically, using median:
  #how many vertices violate strong triadic closure
  #how many triads violate strong triadic closure
  #what proportion of traids violate strong triadic closure
median_vertex_count <- length(median_check$vertex)
median_triad_count <- nrow(median_check$triad)
median_prop <- nrow(median_check$triad)/triad_count

#Print and show
print(c('Number of violating vertices (median): ', median_vertex_count), quote = FALSE)
print(c('Number of violating triads (median): ', median_triad_count), quote = FALSE)
print(c('Proportion of violating triads (median): ', median_prop), quote = FALSE)
print(median_vertex_count)
print(median_triad_count)
print(median_prop)

#################################################################################################################################
#Q3.1: edge level betweenness

#Use igraph functions to calculate edge level betweenness
edge_social_betweenness <- edge_betweenness(d_social_graph, e = E(d_social_graph), directed = TRUE, weights =NULL)
edge_task_betweenness <- edge_betweenness(d_task_graph, e = E(d_task_graph), directed = TRUE, weights =NULL)
edge_both_betweenness <- edge_betweenness(both, e = E(both), directed = TRUE, weights =NULL)

#Record social and task affinity strengths and set NA to 0
social_weight <- E(both)$weight_1
task_weight <- E(both)$weight_2
social_weight[is.na(social_weight)] <- 0
task_weight[is.na(task_weight)] <- 0

#Print and show
print('Edge-level social tie betweenness: ')
print(edge_social_betweenness)
print('Edge-level task tie betweenness: ')
print(edge_task_betweenness)
print('Edge-level combined tie betweenness: ')
print(edge_both_betweenness)

#Mean: record strong and weak ties, using mean
  #These are based on extracted graph attributes, which compared to the mean generates a boolean vector
social_strong_mean <- E(d_social_graph)$weight > mean(E(d_social_graph)$weight)
task_strong_mean <- E(d_task_graph)$weight > mean(E(d_task_graph)$weight)
social_weak_mean <- (E(d_social_graph)$weight < mean(E(d_social_graph)$weight)) & 
  (E(d_social_graph)$weight > 0)
task_weak_mean <- (E(d_task_graph)$weight < mean(E(d_task_graph)$weight)) & 
  (E(d_task_graph)$weight > 0)

#Calculate correlations, using mean
social_strong_cor_mean <- cor(edge_social_betweenness, social_strong_mean)
social_weak_cor_mean <- cor(edge_social_betweenness, social_weak_mean)
task_strong_cor_mean <- cor(edge_task_betweenness, task_strong_mean)
task_weak_cor_mean <- cor(edge_task_betweenness, task_weak_mean)

#Print and show
print(c('Correlation with strong social ties (mean): ', social_strong_cor_mean), quote = FALSE)
print(c('Correlation with weak social ties (mean): ', social_weak_cor_mean), quote = FALSE)
print(c('Correlation with strong task ties (mean): ', task_strong_cor_mean), quote = FALSE)
print(c('Correlation with weak task ties(mean): ', task_weak_cor_mean), quote = FALSE)

#Median: record strong and weak ties, using median
  #These are based on extracted graph attributes, which compared to the mean generates a boolean vector
social_strong_med <- E(d_social_graph)$weight > median(E(d_social_graph)$weight)
task_strong_med <- E(d_task_graph)$weight > median(E(d_task_graph)$weight)
social_weak_med <- (E(d_social_graph)$weight < median(E(d_social_graph)$weight)) & 
  (E(d_social_graph)$weight > 0)
task_weak_med <- (E(d_task_graph)$weight < median(E(d_task_graph)$weight)) & 
  (E(d_task_graph)$weight > 0)

#Calculate correlations, using median
social_strong_cor_med <- cor(edge_social_betweenness, social_strong_med)
social_weak_cor_med <- cor(edge_social_betweenness, social_weak_med)
task_strong_cor_med <- cor(edge_task_betweenness, task_strong_med)
task_weak_cor_med <- cor(edge_task_betweenness, task_weak_med)

#Print and show
print(c('Correlation with strong social ties (median): ', social_strong_cor_med), quote = FALSE)
print(c('Correlation with weak social ties (median): ', social_weak_cor_med), quote = FALSE)
print(c('Correlation with strong task ties (median): ', task_strong_cor_med), quote = FALSE)
print(c('Correlation with weak task ties(median): ', task_weak_cor_med), quote = FALSE)

#################################################################################################################################
#Q4: find number of pairs of nodes without walks, i.e. edges, between them

#Separately create adjacency matrices from affinity matrices, not using attributes;
  #then combine them and graph
d_social_adj <- (d_social_aff != 0) + 0
d_task_adj <- (d_task_aff != 0) + 0
together <- d_social_adj + d_task_adj
together_graph <- graph.adjacency(as.matrix(together), "directed", weighted = TRUE)

#Set up copies of social, task, and combined adjacency matrices
sum_social_temp <- copy(d_social_adj)
sum_task_temp <- copy(d_task_adj)
sum_both_temp <- copy(together)
mul_social_temp <- copy(d_social_adj)
mul_task_temp <- copy(d_task_adj)
mul_both_temp <- copy(together)

#Sum of powers, de-diagonalled, to calculate if walks of any (meaningful) lengths exist between
  #vertices of the matrices, by using the powers of the matrices; the powers range from 2, which is
  #1 more than itself, to 21, which is 1 less than 22; any power more than 21, including 22, is
  #necessarily going to involve repeated vertices and is therefore unnecessary
for (i in 2:21){
  mul_social_temp <- mul_social_temp %*% d_social_adj
  sum_social_temp <- sum_social_temp + mul_social_temp
}
for (i in 2:21){
  mul_task_temp <- mul_task_temp %*% d_task_adj
  sum_task_temp <- sum_task_temp + mul_task_temp
}
for (i in 2:21){
  mul_both_temp <- mul_both_temp %*% together
  sum_both_temp <- sum_both_temp + mul_both_temp
}

#To remove any diagonals from consideration, add a diagonal matrix of 1 and de-zero diagonals;
  #then calculate how many pairs of vertices, direction-distinct, have no walks between them
no_walk_social <- sum((sum_social_temp + diag(nrow = 22, ncol = 22)) == 0)
no_walk_task <- sum((sum_task_temp + diag(nrow = 22, ncol = 22)) == 0)
no_walk_together <- sum((sum_both_temp + diag(nrow = 22, ncol = 22)) == 0)

#Use distance_table to verify
no_walk_social_ver <- distance_table(d_social_graph)$unconnected
no_walk_task_ver <- distance_table(d_task_graph)$unconnected
no_walk_together_ver <- distance_table(together_graph)$unconnected

#Print and show
print(c('Unconnected social network pairs: ', no_walk_social), quote = FALSE)
print(c('Unconnected social network pairs (verification): ', no_walk_social_ver), quote = FALSE)
print(c('Unconnected task network pairs: ', no_walk_task), quote = FALSE)
print(c('Unconnected task network pairs (verification): ', no_walk_task_ver), quote = FALSE)
print(c('Unconnected combined network pairs: ', no_walk_together), quote = FALSE)
print(c('Unconnected combined network pairs (verification): ', no_walk_together_ver), quote = FALSE)

#################################################################################################################################
#Q5: network level centrality

#Generate two matrices, one with network-level degree 0 and one with 1
mat_0 <- matrix(0, nrow = 4, ncol = 4)
mat_0[c(1,4), 2:3] <- 1
mat_0[2:3, c(1,4)] <- 1

mat_1 <- matrix(0, nrow = 4, ncol = 4)
mat_1[1, 2:4] <- 1
mat_1[2:4, 1] <- 1

#Graph the matrices and then plot
mat_0_graph <- graph.adjacency(mat_0, 'undirected')
mat_1_graph <- graph.adjacency(mat_1, 'undirected')

plot.igraph(mat_0_graph, vertex.label=NA, layout=layout.fruchterman.reingold, vertex.label.color="black", vertex.color = 'gold',
            edge.color="black", edge.width=E(mat_0_graph)$weight, vertex.size = 12, edge.arrow.size=.3, edge.curved=FALSE)
plot.igraph(mat_1_graph, vertex.label=NA, layout=layout.fruchterman.reingold, vertex.label.color="black", vertex.color = 'gold',
            edge.color="black", edge.width=E(mat_1_graph)$weight, vertex.size = 12, edge.arrow.size=.3, edge.curved=FALSE)

#Using igraph functions, calculate network-level degree, closeness, and betweenness
mat_0_NL_degree <- centr_degree(mat_0_graph, normalized = TRUE, loop = FALSE)$centralization
mat_1_NL_degree <- centr_degree(mat_1_graph, normalized = TRUE, loop = FALSE)$centralization

mat_0_NL_closeness <- centr_clo(mat_0_graph, normalized = TRUE)$centralization
mat_1_NL_closeness <- centr_clo(mat_1_graph, normalized = TRUE)$centralization

mat_0_NL_betweenness <- centr_betw(mat_0_graph, normalized = TRUE)$centralization
mat_1_NL_betweenness <- centr_betw(mat_1_graph, normalized = TRUE)$centralization

#Print and show
print(c('0 matrix network-level degree: ', mat_0_NL_degree), quote = FALSE)
print(c('0 matrix network-level closeness: ', mat_0_NL_closeness), quote = FALSE)
print(c('0 matrix network-level betweenness: ', mat_0_NL_betweenness), quote = FALSE)
print(c('0 matrix network-level degree: ', mat_1_NL_degree), quote = FALSE)
print(c('0 matrix network-level closeness: ', mat_1_NL_closeness), quote = FALSE)
print(c('0 matrix network-level betweenness: ', mat_1_NL_betweenness), quote = FALSE)