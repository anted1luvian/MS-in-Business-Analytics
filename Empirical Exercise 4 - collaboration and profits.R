#Packaging logistics
library(igraph)
library(tidyverse)
library(ggalt)
library(readxl)
library(data.table)
library(plm)
library(pglm)
library(dplyr)
library(MASS)
library(proxy)
library(Matrix)
library(reshape2)
library(varhandle)
library(ggthemes)

#Import data
d_b <- fread('box_office_revenues.csv')
d_fc <- fread('film_cast_members.csv')
d_fk <- fread('film_keywords.csv')
d_p <- fread('producers_and_films.csv')
d_s <- fread('production_subsidiaries.csv')

#Classify as generalist or specialist: calculate how many films each company makes per year,
  #record the first year of appearance and use that as initial year of operation.
d_p <- d_p[pindex %in% d_fk$pindex & country == 'us']
d_p <- d_p[pindex %in% d_fk$pindex]
d_fk <- d_fk[pindex %in% d_p$pindex]
d_fc <- d_fc[pindex %in% d_p$pindex]

year_list <- sort(unique(d_p$year))
producer_list <- unique(d_p$prod_company)
producer_pcindex <- unique(d_p$pcindex)
film_pindex <- unique(d_fk$pindex)
cast_nconst <- unique(d_fc$nconst)

yield_list <- d_p[, .N, by = .(pcindex, year)]
colnames(yield_list)[3] <- 'yield'
producer_yield <- d_p[, .N, by = .(pcindex, year)] %>% spread(year, N, fill = 0)

#yield_table <- unique(left_join(d_p[, .(pcindex, prod_company)], producer_yield, by = 'pcindex'))
#write.csv(yield_table, 'C:/Users/Qiwei Hong/Desktop/MSBA4/673/EE4/yield_table.csv')

#Classify producers in each year as specialist or generalist, using production yield
producer_class <- data.table(producer_yield[,1], (producer_yield[,-1] > 1) + 0)

#Build a keywords list containing their age and an arbitrary index
keywords_list <- distinct(left_join(d_fk, d_p[,c(1,2,4)], by = 'pindex'))
keywords_list <- as.data.table(distinct(left_join(keywords_list, data.table(keyword = unique(d_fk$keyword), 
                          keyword_index = c(1:length(unique(d_fk$keyword)))), by = 'keyword')))[, first := min(year), 
                                                                                                by = .(keyword)][, age := year - first]

#List of combinations
d_fk_comb <- left_join(keywords_list[,c(1,5)], keywords_list[,c(1,5)], by = 'pindex')
d_fk_comb <- inner_join(d_fk_comb, unique(keywords_list[,c(1,3)]), by = 'pindex')
d_fk_comb <- as.data.table(d_fk_comb)[, first_combo := min(year), 
                                      by = .(keyword_index.x, keyword_index.y)][, age := year - first_combo]

#Identify the films
film_classification <- function(a){
  #1 = peripheral solo; 2 = central solo; 3 = central co; 4 = peripheral co; 5 = hybrid co
  temp_producers <- d_p[pindex == a]
  temp_year <- temp_producers$year[1]
  temp_df <- producer_class[pcindex %in% 
                              temp_producers$pcindex][, which(colnames(producer_class) == temp_year), with = FALSE]
  temp_sum <- sum(unlist(temp_df))
  temp_count <- length(unlist(temp_df))
  if (temp_sum == 0 & temp_count == 1){return(1)}
  if (temp_sum == 1 & temp_count == 1){return(2)}
  if (temp_sum == temp_count & temp_sum >= 2){return(3)}
  if (temp_sum == 0 & temp_count > 1){return(4)}
  if (temp_sum != temp_count & temp_count > 1 & temp_sum != 0){return(5)}
}
#Count number of new keywords and new combinations
new_count <- function(a, type){
  #if type = 0 then new keyword; if type = 1 then new combinations
  temp_df1 <- keywords_list[pindex == a]
  if (type == 0){
    temp_list_new <- temp_df1[age <= 3]
    if (dim(temp_list_new)[1] > 0){temp_count1 <- dim(temp_list_new)[1]} else {temp_count1 <- 0}
    return(temp_count1)
  }
  if (type == 1){
    temp_list_old <- temp_df1[age > 3]
    temp_list_combo <- d_fk_comb[pindex == a][keyword_index.x %in% temp_list_old$keyword_index & 
                                                keyword_index.y %in% temp_list_old$keyword_index & age <= 3]
    temp_count2 <- dim(temp_list_combo)[1]
    return(temp_count2)
  }
}
film_info <- data.table(pindex = film_pindex, type = sapply(film_pindex, FUN = film_classification))
film_info[, new_keyword := sapply(film_info$pindex, FUN = new_count, type = 0)]
film_info[, new_combination := sapply(film_info$pindex, FUN = new_count, type = 1)]
film_info[, new_total := .(new_keyword + new_combination)]
film_info[, year := sapply(film_info$pindex, FUN = function(a) d_p[pindex == a]$year[1])]

#Types of classes
d_p2 <- distinct(left_join(d_p, film_info, by = c('pindex', 'year')))
d_p2 <- as.data.table(distinct(left_join(d_p2, d_b, by = 'pindex')))

#Multidimensional scaling to compute (Jaccard) similarity between keywords used
J <- function(a){
  temp_cross <- tcrossprod(a)
  temp_ind <- which(temp_cross > 0, arr.ind = TRUE, useNames = FALSE)
  temp_rowsum <- rowSums(a)
  temp_cross_ind <- temp_cross[temp_ind]
  sparseMatrix(i = temp_ind[,1], j = temp_ind[,2], x = temp_cross_ind / 
                 (temp_rowsum[temp_ind[,1]] + temp_rowsum[temp_ind[,2]] - temp_cross_ind), dims = dim(temp_cross))
}

producer_distances <- c()
producer_coordinates <- c()
for (i in year_list){
  temp_aff <- dcast(keywords_list[year %in% c((i-2):i)][,c('pcindex', 'keyword_index')], 
                    pcindex ~ keyword_index, fill = 0, fun.aggregate = length)
  temp_pcindex <- temp_aff[,1]
  temp_jaccard <- as.matrix(1 - J(as.matrix(temp_aff[,-1]) > 0 + 0))
  temp_similarities <- cbind(pcindex = temp_pcindex, year = i, distance = 
                               apply(temp_jaccard, MARGIN = 1, FUN = function(a) mean(a[a != 1])))
  temp_mds <- cmdscale(temp_jaccard)
  producer_distances <- rbind(producer_distances, temp_similarities)
  producer_coordinates <- rbind(producer_coordinates, cbind(temp_pcindex, i, temp_mds))
  print(i)
}
producer_distances <- as.data.table(producer_distances)
producer_coordinates <- as.data.table(producer_coordinates)
colnames(producer_coordinates) <- c('pcindex', 'year', 'coordinate_1', 'coordinate_2')
producer_coordinates[,2] <- as.numeric(producer_coordinates$year)
producer_coordinates[,3] <- as.numeric(producer_coordinates$coordinate_1)
producer_coordinates[,4] <- as.numeric(producer_coordinates$coordinate_2)
producer_distances[,2] <- as.numeric(producer_distances$year)
producer_distances[,3] <- as.numeric(producer_distances$distance)

#Insert additional producer information
  #subsidiary is recast to 0 or 1
producer_info <- unfactor(merge(producer_pcindex, year_list))
colnames(producer_info) <- c('pcindex', 'year')
producer_info <- producer_info %>%
  left_join(d_p2[type == 1, .(peripheral_solo = .N), by = .(pcindex, year, type)][, .(pcindex, year, peripheral_solo)], by = c('pcindex', 'year')) %>%
  left_join(d_p2[type == 2, .(central_solo = .N), by = .(pcindex, year, type)][, .(pcindex, year, central_solo)], by = c('pcindex', 'year')) %>%
  left_join(d_p2[type == 3, .(central_co = .N), by = .(pcindex, year, type)][, .(pcindex, year, central_co)], by = c('pcindex', 'year')) %>%
  left_join(d_p2[type == 4, .(peripheral_co = .N), by = .(pcindex, year, type)][, .(pcindex, year, peripheral_co)], by = c('pcindex', 'year')) %>%
  left_join(d_p2[type == 5, .(hybrid_co = .N), by = .(pcindex, year, type)][, .(pcindex, year, hybrid_co)], by = c('pcindex', 'year')) %>%
  left_join(d_p2[, .(box = sum(total_box/release_coverage), year), by = .(pcindex, year)][, .(pcindex, year, box)], by = c('pcindex', 'year')) %>%
  left_join(d_p2[, .(new_keyword = sum(new_keyword)), by = .(pcindex, year)], by = c('pcindex', 'year')) %>%
  left_join(d_p2[, .(new_combination = sum(new_combination)), by = .(pcindex, year)], by = c('pcindex', 'year')) %>%
  left_join(d_p2[type %in% c(1,3), .(new_keyword_solo = sum(new_keyword)), by = .(pcindex, year)], by = c('pcindex', 'year')) %>%
  left_join(d_p2[type == 5, .(new_keyword_hybrid = sum(new_keyword)), by = .(pcindex, year)], by = c('pcindex', 'year')) %>%
  mutate_each(funs(replace(., which(is.na(.)), 0))) %>% as.data.table()
producer_info[, subsidiary := 
                pcindex %in% d_s$pcindex + 0][, since := min(year), by = .(pcindex)][, producer_age := year - since][producer_age >= 0]

cumulative_hybrid <- merge(producer_pcindex, year_list[year_list %in% producer_info$year]) %>% unfactor() %>% as.data.table()
colnames(cumulative_hybrid) <- c('pcindex', 'year')
for (i in unique(cumulative_hybrid$year)){
  cumulative_hybrid <- cumulative_hybrid %>%
    left_join(producer_info[year <= i, .(temp_year = sum(new_keyword_hybrid), year = max(year)), by = .(pcindex)], by = c('pcindex', 'year'))
  print(i)
}
colnames(cumulative_hybrid) <- c('pcindex', 'year', year_list[year_list %in% producer_info$year])
cumulative_hybrid <- cumulative_hybrid[,-2] %>% melt() %>% unfactor() %>% as.data.table()
colnames(cumulative_hybrid) <- c('pcindex', 'year', 'cumulative_hybrid')

producer_info <- producer_info %>% 
  left_join(producer_coordinates, by = c('pcindex', 'year')) %>% unique() %>%
  left_join(yield_list, by = c('pcindex', 'year')) %>% unique() %>%
  left_join(producer_distances, by = c('pcindex', 'year')) %>% 
  left_join(cumulative_hybrid, by = c('pcindex', 'year')) %>% as.data.table()
producer_info[, normalized_box := (box - mean(box))/sd(box), 
              by = .(year)][, new_total := new_keyword + new_combination][, new_total := new_keyword + new_combination]
producer_info <- producer_info[complete.cases(producer_info)]

#Identify cast member innovativeness
cast_list <- unfactor(merge(cast_nconst, year_list)) %>% as.data.table()
colnames(cast_list) <- c('nconst', 'year')
film_cast <- d_fc[pindex %in% film_pindex] %>% left_join(film_info, by = c('pindex', 'year')) %>% 
  left_join(d_p2[, .(pindex, year, pcindex)], by = c('pindex', 'year')) %>% as.data.table()
cast_list <- cast_list %>%
  left_join(film_cast[, .(new_keyword = sum(new_keyword)), by = .(nconst, year)], by = c('nconst', 'year')) %>%
  left_join(film_cast[, .(first_call = min(year)), by = .(nconst)], by = c('nconst')) %>% as.data.table()
cast_list <- cast_list[first_call <= year] %>% mutate_each(funs(replace(., which(is.na(.)), 0))) %>% as.data.table()

cast_info <- unfactor(merge(cast_nconst, year_list)) %>% as.data.table()
colnames(cast_info) <- c('nconst', 'year')
for (i in unique(cast_info$year)[-1]){
  cast_info <- cast_info %>%
    left_join(cast_list[year < i, .(temp_year = sum(new_keyword), year = max(year) + 1), by = .(nconst)], by = c('nconst', 'year'))
  print(i)
}
colnames(cast_info) <- c('nconst', 'year', year_list[-1])
cast_info <- cast_info[,-2] %>% melt() %>% as.data.table()
colnames(cast_info) <- c('nconst', 'year', 'cumulative_new_keyword')
cast_info <- cast_info[!is.na(cumulative_new_keyword)]
producer_cast <- film_cast[, .(pindex, pcindex, nconst)] %>% left_join(cast_info, by = 'nconst') %>% as.data.table()
producer_cast[, cast_inno_mean := mean(cumulative_new_keyword), by = .(pcindex, year)]
producer_cast <- producer_cast[, .(pcindex, year, cast_inno_mean, cast_inno_stdev)] %>% unique() %>% unfactor()

producer_cast_info <- producer_info %>% left_join(unfactor(producer_cast), by = c('pcindex', 'year')) %>% as.data.table()
producer_cast_info2 <- copy(producer_cast_info)
producer_cast_info2[, overall_inno := mean(cast_inno_mean), by = .(pcindex, year)]
producer_cast_info2 <- producer_cast_info2 %>% subset(select = -c(nconst, pindex, cumulative_new_keyword)) %>% unique()

#---#---#---#---#---#---#---#---#---#---#---#---#---#---Q1---#---#---#---#---#---#---#---#---#---#---#---#---#---#

#Part A
movies_by_type <- film_info %>% group_by(type, year) %>% 
                             summarize(mean_new_keyword = mean(new_keyword), mean_new_combination = mean(new_combination), 
                                       mean_new_total = mean(new_total)) %>% ungroup() %>% as.data.table()

movies_type_plot_keyword <- ggplot(data = movies_by_type, aes(x = year, y = mean_new_keyword, color = factor(type))) + 
  geom_line(size = 1.25) + scale_color_discrete(name = 'Movie Type') + ggtitle('Mean New Keywords by Type') + 
  labs(x = 'Year', y = 'New Keywords') + theme_economist_white() + theme(plot.title = element_text(hjust = 0.5))
movies_type_plot_combo <- ggplot(data = movies_by_type, aes(x = year, y = mean_new_combination, color = factor(type))) + 
  geom_line(size = 1.25) + scale_color_discrete(name = 'Movie Type') + ggtitle('Mean New Combinations (Pairs)') + 
  labs(x = 'Year', y = 'New Combinations') + theme_economist_white() + theme(plot.title = element_text(hjust = 0.5))
movies_type_plot_keyword
movies_type_plot_combo

#Part B
new_keyword_reg <- glm.nb(new_keyword ~ central_co + peripheral_co + hybrid_co + 
                            coordinate_1 + coordinate_2 + box + producer_age + subsidiary + factor(year), 
                          data = producer_info, offset(yield))
new_combination_reg <- glm.nb(new_combination ~ central_co + peripheral_co + hybrid_co + 
                                coordinate_1 + coordinate_2 + box + producer_age + subsidiary + factor(year), 
                              data = producer_info, offset(yield))
new_total_reg <- glm.nb(new_total ~ central_co + peripheral_co + hybrid_co + 
                          coordinate_1 + coordinate_2 + box + producer_age + subsidiary + factor(year), 
                        data = producer_info, offset(yield))
summary(new_keyword_reg)$coefficients[1:9,]
summary(new_combination_reg)$coefficients[1:9,]
summary(new_total_reg)$coefficients[1:9,]

#---#---#---#---#---#---#---#---#---#---#---#---#---#---Q2---#---#---#---#---#---#---#---#---#---#---#---#---#---#
keyword_visualization <- ggplot(producer_info, aes(distance, new_keyword)) +
  geom_smooth(method = "loess", se = TRUE) + labs(x = "Average Jaccard distance", y = "New keywords") + 
  theme_economist_white() + theme(plot.title = element_text(hjust = 0.5)) + ggtitle('LOESS Curve [new keywords]')
combination_visualization <- ggplot(producer_info, aes(distance, new_combination)) + 
  geom_smooth(method = "loess", se = TRUE) + labs(x = "Average Jaccard distance", y = "New combinations") + 
  theme_economist_white() + theme(plot.title = element_text(hjust = 0.5)) + ggtitle('LOESS Curve [new combinations]')
total_visualization <- ggplot(producer_info, aes(distance, new_combination)) + 
  geom_smooth(method = "loess", se = TRUE) + labs(x = "Average Jaccard distance", y = "New keywords and combinations") + 
  theme_economist_white() + theme(plot.title = element_text(hjust = 0.5)) + ggtitle('LOESS Curve [new keywords and combinations]')

keyword_visualization
combination_visualization
total_visualization

#---#---#---#---#---#---#---#---#---#---#---#---#---#---Q3---#---#---#---#---#---#---#---#---#---#---#---#---#---#
normalized_box_reg <- lm(normalized_box ~ central_co + peripheral_co + hybrid_co +
                           coordinate_1 + coordinate_2 + box + producer_age + subsidiary + factor(year), data = producer_info)
summary(normalized_box_reg)$coefficients[1:9,]

#---#---#---#---#---#---#---#---#---#---#---#---#---#---Q4---#---#---#---#---#---#---#---#---#---#---#---#---#---#

#Part A
risk_reg <- glm.nb(new_keyword_solo ~ cumulative_hybrid + central_co + peripheral_co + hybrid_co + 
                          coordinate_1 + coordinate_2 + normalized_box + producer_age + subsidiary + factor(year), 
                        data = producer_info, offset(yield))
summary(risk_reg)$coefficients[1:10,]

#Part B
normalized_box_reg <- lm(normalized_box ~ new_keyword + new_combination + cumulative_hybrid + 
                           central_co + peripheral_co + hybrid_co +
                           coordinate_1 + coordinate_2 + producer_age + subsidiary + factor(year), data = producer_info)
summary(normalized_box_reg)$coefficients[1:9,]

#---#---#---#---#---#---#---#---#---#---#---#---#---#---EC---#---#---#---#---#---#---#---#---#---#---#---#---#---#
cast_inno_mean_reg <- lm(cast_inno_mean ~ central_co + peripheral_co + hybrid_co + coordinate_1 + coordinate_2 + 
                      box + producer_age + subsidiary + factor(year), data = producer_cast_info2)
summary(cast_inno_mean_reg)$coefficients[1:9,]
