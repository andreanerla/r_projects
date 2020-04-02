library(tidyverse)
library(gridExtra)
interactions <- read.csv("../input/food-com-recipes-and-user-interactions/RAW_interactions.csv", stringsAsFactors = F)
recipes <- read.csv("../input/food-com-recipes-and-user-interactions/RAW_recipes.csv", stringsAsFactors = F)
head(recipes)
head(interactions)
rec_and_inter <- merge(recipes, interactions, by.x = "id", by.y = "recipe_id", all.x = "true")
head(rec_and_inter)
sapply(rec_and_inter, function(x) {sum(is.na(x))})
unique_r_and_i <- rec_and_inter[!duplicated(rec_and_inter$id),]
unique_r_and_i <- unique_r_and_i[!(unique_r_and_i$rating=="0"), ]
unique_r_and_i %>%
  ggplot(aes(rating))+
  geom_bar(stat = "count", fill = "royalblue")+
  theme_minimal()+
  ggtitle("Distribution of Recipes' Ratings")
min_r_and_i <- unique_r_and_i %>%
  group_by(minutes) %>%
  summarise(n = n())%>%
  filter(n>10)
min_r_and_i %>%
  filter(minutes < 900)%>%
  ggplot(aes(minutes, n))+
  geom_bar(stat = "identity", fill = "darkgreen")+
  theme_minimal()+
  ggtitle("Distribution of Recipes by Minutes of Preparation")
min_r_and_i %>%
  filter(minutes < 150)%>%
  ggplot(aes(minutes, n))+
  geom_bar(stat = "identity", fill = "darkgreen")+
  theme_minimal()+
  ggtitle("Distribution of Recipes by Minutes of Preparation")
unique_r_and_i <- unique_r_and_i %>%
  filter(minutes < 20000)

unique_r_and_i$rating <- as.factor(unique_r_and_i$rating) 

unique_r_and_i%>%
  ggplot(aes(rating, minutes))+
  geom_boxplot()+
  scale_y_log10()+
  ggtitle("Distribution of Minutes of Preparation by Rating")
unique_r_and_i$velocity <- 
  ifelse(unique_r_and_i$minutes >= 0 & unique_r_and_i$minutes < 10, 'really fast',
         ifelse(unique_r_and_i$minutes >= 10 & unique_r_and_i$minutes < 20, 'fast',
                ifelse(unique_r_and_i$minutes >= 20 & unique_r_and_i$minutes < 50, 'average',
                       ifelse(unique_r_and_i$minutes >= 50 & unique_r_and_i$minutes < 80, 'slow',
                              'really slow'))))
unique_r_and_i$rating <- as.numeric(unique_r_and_i$rating) 

r_i_velocity <- unique_r_and_i %>%
  group_by(velocity)%>%
  summarise(mean = mean(rating))
r_i_velocity %>%
  ggplot(aes(reorder(velocity, -mean), mean))+
  geom_bar(stat = "identity", fill = "darkred")+
  coord_cartesian(ylim = c(4.25, 4.75))+
  geom_label(aes(label = round(mean, 2)))+
  theme_minimal()+
  ggtitle("Average Rating per Velocity Category")
unique_r_and_i%>%
  ggplot(aes(n_ingredients))+
  geom_bar(stat = "count", fill = "darkgreen")+
  theme_minimal()+
  ggtitle("Distribution of Number of Ingredients per Recipe")
r_i_ingredients <- unique_r_and_i %>%
  filter(n_ingredients < 25)%>%
  group_by(n_ingredients)%>%
  summarise(mean = mean(rating))
r_i_ingredients %>%
  ggplot(aes(n_ingredients, mean))+
  geom_bar(stat = "identity", fill = "darkblue")+
  coord_cartesian(ylim = c(4.25, 4.75))+
  ggtitle("Average Rating per Number of Ingredients")+
  theme_minimal()
unique_n_i <- unique_r_and_i %>%
  mutate(nutrition = str_replace_all(nutrition, "\\[|\\]", "")) %>%
  separate(nutrition, into = c("calories", "total_fat", "sugar", "sodium", "protein", "sat_fat", "carbs"), sep = ",")

unique_n_i$calories <- as.numeric(unique_n_i$calories)
unique_n_i$total_fat <- as.numeric(unique_n_i$total_fat)
unique_n_i$sodium <- as.numeric(unique_n_i$sodium)
unique_n_i$sugar <- as.numeric(unique_n_i$sugar)
unique_n_i$protein <- as.numeric(unique_n_i$protein)
unique_n_i$sat_fat <- as.numeric(unique_n_i$sat_fat)
unique_n_i$carbs <- as.numeric(unique_n_i$carbs)
calories_rating <- unique_n_i %>%
  group_by(rating)%>%
  summarise(mean = mean(calories))

total_fat_rating <- unique_n_i %>%
  group_by(rating)%>%
  summarise(mean = mean(total_fat))

sodium_rating <- unique_n_i %>%
  group_by(rating)%>%
  summarise(mean = mean(sodium))

sugar_rating <- unique_n_i %>%
  group_by(rating)%>%
  summarise(mean = mean(sugar))

protein_rating <- unique_n_i %>%
  group_by(rating)%>%
  summarise(mean = mean(protein))

sat_fat_rating <- unique_n_i %>%
  group_by(rating)%>%
  summarise(mean = mean(sat_fat))
carbs_rating <- unique_n_i %>%
  group_by(rating)%>%
  summarise(mean = mean(carbs))
plot_cal <- calories_rating %>%
  ggplot(aes(rating, mean))+
  geom_bar(stat = 'identity', fill="blue")+
  geom_label(aes(label = round(mean, 0)))+
  theme_minimal()+
  ggtitle("Average Calories per Rating")+
  xlab(element_blank())+
  ylab(element_blank())

plot_fat <- total_fat_rating %>%
  ggplot(aes(rating, mean))+
  geom_bar(stat = 'identity', fill="blue")+
  geom_label(aes(label = round(mean, 0)))+
  theme_minimal()+
  ggtitle("Average Fats per Rating")+
  xlab(element_blank())+
  ylab(element_blank())
plot_sodium <- sodium_rating %>%
  ggplot(aes(rating, mean))+
  geom_bar(stat = 'identity', fill="blue")+
  geom_label(aes(label = round(mean, 0)))+
  theme_minimal()+
  ggtitle("Average Sodium per Rating")+
  xlab(element_blank())+
  ylab(element_blank())


plot_sugar <- sugar_rating %>%
  ggplot(aes(rating, mean))+
  geom_bar(stat = 'identity', fill="blue")+
  geom_label(aes(label = round(mean, 0)))+
  theme_minimal()+
  ggtitle("Average Sugar per Rating")+
  xlab(element_blank())+
  ylab(element_blank())
plot_protein <- protein_rating %>%
  ggplot(aes(rating, mean))+
  geom_bar(stat = 'identity', fill="blue")+
  geom_label(aes(label = round(mean, 0)))+
  theme_minimal()+
  ggtitle("Average Proteins per Rating")+
  xlab(element_blank())+
  ylab(element_blank())

plot_sat_fat <- sat_fat_rating %>%
  ggplot(aes(rating, mean))+
  geom_bar(stat = 'identity', fill="blue")+
  geom_label(aes(label = round(mean, 0)))+
  theme_minimal()+
  ggtitle("Average Saturated Fats per Rating")+
  xlab(element_blank())+
  ylab(element_blank())
plot_carbs <- carbs_rating %>%
  ggplot(aes(rating, mean))+
  geom_bar(stat = 'identity', fill="blue")+
  geom_label(aes(label = round(mean, 0)))+
  theme_minimal()+
  ggtitle("Average Carbs per Rating")+
  xlab(element_blank())+
  ylab(element_blank())

grid.arrange(plot_cal, plot_fat, plot_sodium, plot_carbs, nrow = 2)
grid.arrange(plot_sat_fat, plot_sugar, plot_protein, nrow = 2)
