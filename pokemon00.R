getwd()
read.csv("pokemon.csv") -> pokemon
library(dplyr)

pokemon %>% select(-1) -> pokemon

#renaming of columns
colnames(pokemon)[2] <- "Primary_Type"
colnames(pokemon)[3] <- "Secondary_Type"
colnames(pokemon)[5] <- "Health_Points"
colnames(pokemon)[8] <- "Special_Attack"
colnames(pokemon)[9] <- "Special_Defense"

#changing to factors
as.factor(pokemon$isLegendary) -> pokemon$isLegendary
as.factor(pokemon$hasMegaEvolution) -> pokemon$hasMegaEvolution
as.factor(pokemon$hasGender) -> pokemon$hasGender

#getting water pokemon
pokemon %>% filter(Primary_Type == "Water") -> water_pokemon
water_pokemon %>% filter(Secondary_Type == "Psychic") -> water_psychic_pokemon
range(water_psychic_pokemon$Defense)
water_psychic_pokemon %>% filter(Defense == 110) -> my_water_pokemon
View(my_water_pokemon)

#select grass pokemon
pokemon %>% filter(Primary_Type == "Grass") -> grass_pokemon
grass_pokemon %>% filter(Secondary_Type == "Poison") -> grass_poison_pokemon
range(grass_poison_pokemon$Speed)
grass_poison_pokemon %>% filter(Speed == 90) -> my_grass_pokemon
View(my_grass_pokemon)

#select fire pokemon
pokemon %>% filter(Primary_Type == "Fire") -> fire_pokemon
fire_pokemon %>% filter(Secondary_Type == "Fighting") -> fire_fighting_pokemon
range(fire_fighting_pokemon$Attack)
fire_fighting_pokemon %>% filter(Attack == 123) -> my_fire_pokemon
rbind(my_water_pokemon, my_grass_pokemon, my_fire_pokemon)
rbind(my_water_pokemon, my_grass_pokemon, my_fire_pokemon) ->my_pokemons
View(my_pokemons)

#splitting data to test and training sets
library(caTools)
sample.split(pokemon$Attack, SplitRatio = 0.65) ->split_index
subset(pokemon, split_index == T) ->train1
subset(pokemon, split_index == F) -> test1

#modelling (attack vs. defense)
lm(Attack~Defense, data = train1) -> mod_regress
predict(mod_regress, test1) -> result_regress
cbind(Actual = test1$Attack, Predicted = result_regress) -> final_data
as.data.frame(final_data)->final_data
View(final_data)

#error
error <- (final_data$Actual - final_data$Predicted)
final_data <- cbind(error, final_data)
view(final_data)
View(final_data)
rmse1 <- sqrt(mean(final_data$error^2))
View(rmse1)
cbind(final_data, rmse1)


#model2
lm(Attack~Defense + Speed + Health_Points + Weight_kg, data = train1) -> mod_regress2
predict(mod_regress2, test1) -> result_regress2
cbind(Actual = test1$Attack, Predicted = result_regress2) ->final_data2
as.data.frame(final_data2) ->final_data2
View(final_data2)
final_data2 <- cbind(final_data2, error2)
#error2
error2 <- (final_data2$Actual - final_data2$predicted)
final_data2 <- cbind(final_data2, error2)
#error2
error2 <- (final_data2$Actual - final_data2$Predicted)
final_data2 <- cbind(final_data2, error2)
View(final_data2)
rmse2 <- sqrt(mean(final_data2$error^2))
rmse2
#model2
lm(Attack~Defense + Speed + Health_Points + Weight_kg + Special_Defense, data = train1) -> mod_regress2
lm(Attack~Defense + Speed + Health_Points + Weight_kg + Special_Defense, data = train1) -> mod_regress2
predict(mod_regress2, test1) -> result_regress2
cbind(Actual = test1$Attack, Predicted = result_regress2) ->final_data2
as.data.frame(final_data2) ->final_data2
View(final_data2)
#error2
error2 <- (final_data2$Actual - final_data2$Predicted)
final_data2 <- cbind(final_data2, error2)
View(final_data2)
rmse2 <- sqrt(mean(final_data2$error^2))
rmse2
lm(Attack~Defense + Speed + Health_Points + Weight_kg + Special_Defense + isLegendary, data = train1) -> mod_regress2
predict(mod_regress2, test1) -> result_regress2
cbind(Actual = test1$Attack, Predicted = result_regress2) ->final_data2
as.data.frame(final_data2) ->final_data2
View(final_data2)
#error2
error2 <- (final_data2$Actual - final_data2$Predicted)
final_data2 <- cbind(final_data2, error2)
View(final_data2)
rmse2 <- sqrt(mean(final_data2$error^2))
rmse2
#classification if legendary
sample.split(pokemon$isLegendary, SplitRatio = 0.65) -> split_values
subset(pokemon, split_values == T) -> train3
subset(pokemon, split_values == F) ->test3
nrow(train3)
nrow(test3)
rpart(isLegendary~. , data = train3) -> mod1
library(rpart)
rpart(isLegendary~. , data = train3) -> mod1
predict(mod1, test3, type = "class") -> result1
table(test3$isLegendary, result1)
library(caret)
install_packages(caret)
install.packages(caret)
install.packages("caret")
library(caret)
confusionMatrix(table(test3$isLegendary, result1))