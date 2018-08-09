library(tidyverse)

df <- read_tsv("foodFacts.tsv")
base_dados_completa <- df

df <- select(df, -c(url, creator, created_t, last_modified_t, last_modified_datetime, packaging, 
                    packaging_tags, brands_tags, categories_tags, categories_en, origins_tags, 
                    manufacturing_places, manufacturing_places_tags, labels_tags, labels_en, emb_codes,
                    emb_codes_tags, first_packaging_code_geo, cities_tags, purchase_places, stores, 
                    countries_tags, ingredients_from_palm_oil_tags, 
                    ingredients_that_may_be_from_palm_oil_n, ingredients_that_may_be_from_palm_oil, 
                    ingredients_that_may_be_from_palm_oil_tags))

df <- select(df, -c(quantity, brands, categories, origins, labels, cities, countries, 
                    allergens, allergens_en, traces, traces_tags, traces_en, no_nutriments, additives_n, 
                    additives, additives_tags, additives_en, ingredients_from_palm_oil_n, 
                    ingredients_from_palm_oil, states, states_tags, states_en, image_url, image_small_url))
# pegando somente s??dio, macronutrientes e calorias

df <- select(df, c(product_name, main_category_en, energy_100g, fat_100g,'saturated-fat_100g', "trans-fat_100g",
                  '-sucrose_100g', '-fructose_100g', 'fiber_100g', proteins_100g, salt_100g,
                  'vitamin-a_100g', 'beta-carotene_100g', 'vitamin-d_100g', 'vitamin-e_100g', 'vitamin-k_100g', 'vitamin-c_100g', 'vitamin-b1_100g',
                   'vitamin-b2_100g', 'vitamin-b6_100g', 'vitamin-b9_100g', 'vitamin-b12_100g', potassium_100g, iron_100g, 
                   magnesium_100g, zinc_100g, copper_100g))

df <- select(df, c(8, 64, 66, 67, 100, 102, 112, 113, 117, 159))

# retirando as entradas com valores faltantes e invalidos
df <- na.omit(df)
df[apply(sapply(df, is.finite), 1, all),]
df <- df %>% distinct(product_name, .keep_all = TRUE)

backup_variavel_inicial <- df

wss <- (nrow(df[2:9])-1)*sum(apply(df[2:9],2,var))
for (i in 2:15) wss[i] <- sum(kmeans(df[2:9],
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

library(vegan)

spe.KM.cascade <- cascadeKM(df[2:9], inf.gr=2, sup.gr=10, iter=100, criterion="ssi")
plot(spe.KM.cascade, sortg=TRUE)

set.seed(7)
 
grp <- kmeans(df[2:9], 6)

aggregate(df, by=list(grp$cluster), FUN = mean)
mydata <- data.frame(df, grp$cluster)

ggplot(subset(mydata, grp.cluster == 1)) +
    geom_bar(aes(x = nutrition.score.fr_100g))

ggplot(subset(mydata, grp.cluster == 2)) +
    geom_bar(aes(x = nutrition.score.fr_100g))

ggplot(subset(mydata, grp.cluster == 3)) +
    geom_bar(aes(x = nutrition.score.fr_100g))

ggplot(subset(mydata, grp.cluster == 4)) +
    geom_bar(aes(x = nutrition.score.fr_100g))

ggplot(subset(mydata, grp.cluster == 5)) +
    geom_bar(aes(x = nutrition.score.fr_100g))

ggplot(subset(mydata, grp.cluster == 6)) +
    geom_bar(aes(x = nutrition.score.fr_100g))

mean(subset(mydata, grp.cluster == 1)$nutrition.score.fr_100g)
# 16.16444

mean(subset(mydata, grp.cluster == 2)$nutrition.score.fr_100g)
# 5.59194

mean(subset(mydata, grp.cluster == 3)$nutrition.score.fr_100g)
# 17.45789

mean(subset(mydata, grp.cluster == 4)$nutrition.score.fr_100g)
# 1.173245

mean(subset(mydata, grp.cluster == 5)$nutrition.score.fr_100g)
# 12.01537

mean(subset(mydata, grp.cluster == 6)$nutrition.score.fr_100g)
# 10.07524
 
var(subset(mydata, grp.cluster == 1)$nutrition.score.fr_100g)
#[1] 78.84504

var(subset(mydata, grp.cluster == 2)$nutrition.score.fr_100g)
#[1] 46.01164

var(subset(mydata, grp.cluster == 3)$nutrition.score.fr_100g)
#[1] 44.10276

var(subset(mydata, grp.cluster == 4)$nutrition.score.fr_100g)
#[1] 41.27278

var(subset(mydata, grp.cluster == 5)$nutrition.score.fr_100g)
#[1] 15.06139

var(subset(mydata, grp.cluster == 6)$nutrition.score.fr_100g)
#[1] 56.38992

features = df[3:9]

pie(colSums(features[grp$cluster == 1]), cex = 0.5)
pie(colSums(features[grp$cluster == 2]), cex = 0.5)
pie(colSums(features[grp$cluster == 3]), cex = 0.5)
pie(colSums(features[grp$cluster == 4]), cex = 0.5)
pie(colSums(features[grp$cluster == 5]), cex = 0.5)
pie(colSums(features[grp$cluster == 6]), cex = 0.5) 
features = df[c(3, 6, 7, 8)]

pie(colSums(feature[grp$cluster == 1]), cex = 0.5)
pie(colSums(features[grp$cluster == 2]), cex = 0.5)
pie(colSums(features[grp$cluster == 3]), cex = 0.5)
pie(colSums(features[grp$cluster == 4]), cex = 0.5)
pie(colSums(features[grp$cluster == 5]), cex = 0.5)
pie(colSums(features[grp$cluster == 6]), cex = 0.5)

# an??lise de resultados
df <- base_dados_completa
df <- select(df, c(8, 55, 56, 64, 66, 67, 100, 102, 112, 113, 117, 159))
df <- na.omit(df)
df <- df %>% distinct(product_name, .keep_all = TRUE)

features <- df[c(5, 8, 9, 10, 11)]

pie(colSums(features[df$pnns_groups_1 == "Fish Meat Eggs",]), cex = 0.5)
pie(colSums(features[df$pnns_groups_1 == "Milk and dairy products",]), cex = 0.5)
pie(colSums(features[df$pnns_groups_2 == "Fruits",]), cex = 0.5)

df[df$pnns_groups_2 == "vegetables", ]$pnns_groups_2 <- "Vegetables"
pie(colSums(features[df$pnns_groups_2 == "Vegetables",]), cex = 0.5)

df[df$pnns_groups_2 == "Biscuits and cakes", "pnns_groups_2"] <- "Sweets"
df[df$pnns_groups_2 == "Pizza pies and quiche", "pnns_groups_2"] <- "Sweets"
df[df$pnns_groups_2 == "Appetizers", "pnns_groups_2"] <- "Sweets"
df[df$pnns_groups_2 == "Sweetened beverages", "pnns_groups_2"] <- "Sweets"
df[df$pnns_groups_2 == "unknown", "pnns_groups_2"] <- "Sweets"
df[df$pnns_groups_2 == "Chocolate products","pnns_groups_2"] <- "Sweets"
df[df$pnns_groups_2 == "Dairy desserts","pnns_groups_2"] <- "Sweets"
df[df$pnns_groups_2 == "Fats","pnns_groups_2"] <- "Sweets"
df[df$pnns_groups_2 == "Dressings and sauces", "pnns_groups_2"] <- "Sweets"
pie(colSums(features[df$pnns_groups_2 == "Sweets",]), cex = 0.5)

df[df$pnns_groups_2 == "Bread", "pnns_groups_2"] <- "Grains"
df[df$pnns_groups_2 == "Cereals", "pnns_groups_2"] <- "Grains"
df[df$pnns_groups_2 == "pastries", "pnns_groups_2"] <- "Grains"
pie(colSums(features[df$pnns_groups_2 == "Grains",]), cex = 0.5)

# sobre a proteina
mean(subset(df, pnns_groups_1 == "Fish Meat Eggs")$energy_100g)
# 749.51
mean(subset(df, pnns_groups_1 == "Fish Meat Eggs")$fat_100g)
# 9.62
mean(subset(df, pnns_groups_1 == "Fish Meat Eggs")$carbohydrates_100g)
#4.15
mean(subset(df, pnns_groups_1 == "Fish Meat Eggs")$proteins_100g)
#18.14
mean(subset(df, pnns_groups_1 == "Fish Meat Eggs")$fiber_100g)
#0.26
mean(subset(df, pnns_groups_1 == "Fish Meat Eggs")$"saturated-fat_100g")
#2.55
mean(subset(df, pnns_groups_1 == "Fish Meat Eggs")$"trans-fat_100g")
#0.02
mean(subset(df, pnns_groups_1 == "Fish Meat Eggs")$salt_100g)
#1.17
mean(subset(df, pnns_groups_1 == "Fish Meat Eggs")$"nutrition-score-fr_100g")
#3.93
# sobre os laticinios

mean(subset(df, pnns_groups_1 == "Milk and dairy products")$energy_100g)
# 701.93
mean(subset(df, pnns_groups_1 == "Milk and dairy products")$fat_100g)
# 10.46
mean(subset(df, pnns_groups_1 == "Milk and dairy products")$carbohydrates_100g)
# 9.80
mean(subset(df, pnns_groups_1 == "Milk and dairy products")$proteins_100g)
# 9.64
mean(subset(df, pnns_groups_1 == "Milk and dairy products")$fiber_100g)
# 0.14
mean(subset(df, pnns_groups_1 == "Milk and dairy products")$"saturated-fat_100g")
# 6.30
mean(subset(df, pnns_groups_1 == "Milk and dairy products")$"trans-fat_100g")
# 0.04
mean(subset(df, pnns_groups_1 == "Milk and dairy products")$salt_100g)
# 0.61
mean(subset(df, pnns_groups_1 == "Milk and dairy products")$"nutrition-score-fr_100g")
# 5.29

# sobre as frutas
mean(subset(df, pnns_groups_2 == "Fruits")$energy_100g)
# 532
mean(subset(df, pnns_groups_2 == "Fruits")$fat_100g)
# 1.60
mean(subset(df, pnns_groups_2 == "Fruits")$carbohydrates_100g)
# 26.36
mean(subset(df, pnns_groups_2 == "Fruits")$proteins_100g)
# 0.69
mean(subset(df, pnns_groups_2 == "Fruits")$fiber_100g)
# 4.32
mean(subset(df, pnns_groups_2 == "Fruits")$"saturated-fat_100g")
# 1.01
mean(subset(df, pnns_groups_2 == "Fruits")$"trans-fat_100g")
# 0.00
mean(subset(df, pnns_groups_2 == "Fruits")$salt_100g)
# 0.03
mean(subset(df, pnns_groups_2 == "Fruits")$"nutrition-score-fr_100g")
# -1.46

#sobre os vegetais
mean(subset(df, pnns_groups_2 == "Vegetables")$energy_100g)
# 240.81
mean(subset(df, pnns_groups_2 == "Vegetables")$fat_100g)
# 1.55
mean(subset(df, pnns_groups_2 == "Vegetables")$carbohydrates_100g)
# 8.51
mean(subset(df, pnns_groups_2 == "Vegetables")$proteins_100g)
# 1.77
mean(subset(df, pnns_groups_2 == "Vegetables")$fiber_100g)
# 1.99
mean(subset(df, pnns_groups_2 == "Vegetables")$"saturated-fat_100g")
# 0.48
mean(subset(df, pnns_groups_2 == "Vegetables")$"trans-fat_100g")
# 0.00
mean(subset(df, pnns_groups_2 == "Vegetables")$salt_100g)
# 0.9
mean(subset(df, pnns_groups_2 == "Vegetables")$"nutrition-score-fr_100g")
# -3.62

#sobre os doces
mean(subset(df, pnns_groups_2 == "Sweets")$energy_100g)
# 1515.96
mean(subset(df, pnns_groups_2 == "Sweets")$fat_100g)
# 19.38
mean(subset(df, pnns_groups_2 == "Sweets")$carbohydrates_100g)
# 41.25
mean(subset(df, pnns_groups_2 == "Sweets")$proteins_100g)
# 6.87
mean(subset(df, pnns_groups_2 == "Sweets")$fiber_100g)
# 3.18
mean(subset(df, pnns_groups_2 == "Sweets")$"saturated-fat_100g")
# 7.06
mean(subset(df, pnns_groups_2 == "Sweets")$"trans-fat_100g")
# 0.15
mean(subset(df, pnns_groups_2 == "Sweets")$salt_100g)
# 1.81
mean(subset(df, pnns_groups_2 == "Sweets")$"nutrition-score-fr_100g")
# 12.64

#sobre os graos
mean(subset(df, pnns_groups_2 == "Grains")$energy_100g)
# 1408.85
mean(subset(df, pnns_groups_2 == "Grains")$fat_100g)
# 6.34
mean(subset(df, pnns_groups_2 == "Grains")$carbohydrates_100g)
# 60.58
mean(subset(df, pnns_groups_2 == "Grains")$proteins_100g)
# 9.57
mean(subset(df, pnns_groups_2 == "Grains")$fiber_100g)
# 3.74
mean(subset(df, pnns_groups_2 == "Grains")$"saturated-fat_100g")
# 2.29
mean(subset(df, pnns_groups_2 == "Grains")$"trans-fat_100g")
# 0.01
mean(subset(df, pnns_groups_2 == "Grains")$salt_100g)
# 1.28
mean(subset(df, pnns_groups_2 == "Grains")$"nutrition-score-fr_100g")
# 3.65




