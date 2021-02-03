library(tidyverse)
library(FNN)
fg <- read_csv('fg-pitcher.csv') #import some fangraphs 

fg[is.na(fg)] <- 0 #set empty values to zero 
fg.names.data <- tolower(colnames(fg)) #clean column names
colnames(fg)  <- fg.names.data
names(fg)

#pick features and scale the data
fg_select <- fg %>%
  select(c(3:15)) %>% mutate_if(is.numeric, scale)

knn_features <- get.knn(fg_select,k=3)

index <- data.frame(knn_features$nn.index) #similar pitchers
scores <- data.frame(knn_features$nn.dist) #euclidean distance

#A function to standardize euclidean distances and then make 1000 most similar
s = sort(rexp(100))
range01 <- function(x){(x-min(x))/(max(x)-min(x))}
score <- round(1000-(range01(scores)*1000),0)
s <- score %>% rename(sim_score_1=X1,sim_score_2=X2,sim_score_3=X3)

#here's the final dataset
fg_final <- cbind(fg,index,s)

#Create an index number and make a lookup table
fg_final$row_num <- seq.int(nrow(fg_final))
fg_lookup <- fg_final %>% select(name,row_num)
head(fg_lookup)

#Join the data
fg_sim <- fg_final %>%
  inner_join(fg_lookup,by=c("X1"="row_num")) %>%
  inner_join(fg_lookup,by=c("X2"="row_num")) %>%
  inner_join(fg_lookup,by=c("X3"="row_num")) %>%
  rename(sim_pitcher_1=name.y) %>%
  rename(sim_pitcher_2=name.x.x) %>%
  rename(sim_pitcher_3=name.y.y) %>%
  rename(name=name.x) %>% 
  select(name,team,sim_pitcher_1,sim_score_1,
         sim_pitcher_2,sim_score_2,
         sim_pitcher_3,sim_score_3,
         everything()) 

#Print results
fg_sim %>% write_csv('similarity-score-results.csv')

#Print short results
fg_sim %>% select(name,team,sim_pitcher_1,sim_score_1,
                    sim_pitcher_2,sim_score_2,
                    sim_pitcher_3,sim_score_3) %>% write_csv('similar-stuff.csv')

