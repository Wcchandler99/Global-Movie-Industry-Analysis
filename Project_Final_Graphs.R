#Source: https://www.kaggle.com/stefanoleone992/imdb-extensive-dataset
#---------------------
#Loading Packages:
library(tidyverse)
library("mapproj")
library(RColorBrewer)
library(streamgraph)
#---------------------
#Loading Data & Setting Prefrences:
setwd("D:Working_Directory/Bigger_archive")
options(scipen = 999)
movies2 <- read_csv("IMDb_movies.csv")
#----------------------
#Removing companies with less than n movies
    #can't remove more than 259 movies
#n = 800
#movies2 <- data.frame(movies2)
#table.company <- table(movies2$production_company)
#movies2 <- movies2[movies2$production_company %in% names(table.company)[table.company > n],] #https://stackoverflow.com/questions/19412337/count-rows-for-selected-column-values-and-remove-rows-based-on-count-in-r
#-------------------
#unique(movies2$production_company)
#Top 6: (+699 movies)
    #[1] "Paramount Pictures"        "Universal Pictures"        "Warner Bros."              "Metro-Goldwyn-Mayer (MGM)"
    #[5] "Columbia Pictures"         "Twentieth Century Fox" 
#Top 12: (+199 movies)
    #[1] "Paramount Pictures"                    "Universal Pictures"                    "Warner Bros."                         
    #[4] "Metro-Goldwyn-Mayer (MGM)"             "Columbia Pictures"                     "Mosfilm"                              
    #[7] "RKO Radio Pictures"                    "Twentieth Century Fox"                 "Republic Pictures (I)"                
    #[10] "Universal International Pictures (UI)" "New Line Cinema"                       "Canal+"
#Big enough?
(ncol(movies2)*4)*(nrow(movies2)/100) #https://spark.apache.org/docs/1.6.2/api/R/ncol.html
# n < 1283 CHECK!
#--------------------
#Cleaning Genre Variable:
movies2$genre[-length(movies2$genre)] <- paste0(movies2$genre[-length(movies2$genre)], ',')     
movies2$genre <- gsub("Musical", "Music", movies2$genre)
movies2$genre1 <- gsub(",.*", "", movies2$genre)
movies2$genre2 <- sub("^.*?,", "", movies2$genre)
movies2$genre3 <- sub("^.*?,", "", movies2$genre2)
movies2$genre2 <- gsub(",.*", "", movies2$genre2)
movies2$genre3 <- gsub(",", "", movies2$genre3)
movies2$genre2 <- trimws(movies2$genre2)
movies2$genre3 <- trimws(movies2$genre3)
movies2$country <- gsub(",.*", "", movies2$country)
#-------------------
colors <- c("Drama" = "#A46422", "Comedy" = "#800080", "Romance" = "#E06F8B", "Action" = "#BE2633",
            "Thriller" = "#493C2B", "Crime" = "#9D9D9D", "Horror" = "#1B2632", "Adventure" = "#EB8931",
            "Mystery" = "#005784", "Family" = "#31A2F2", "Fantasy" = "#44891A", "Music" = "#D2B48C",
            "Sci-Fi" = "#A3CE27", "Biography" = "#2F484E", "History" = "#B2DCEF", "War" = "#000000", 
            "Animation" = "#CBC3E3", "Western" = "#918151", "Sport" = "#FFFF00", "Film-Noir" = "#A9A9A9",
            "NA" = "#FFFFFF")
#-------------------
colors.us <- c("Drama" = "#A46422", "Comedy" = "#800080", "Romance" = "#E06F8B", "Action" = "#BE2633",
               "Horror" = "#1B2632", "Thriller" = "#493C2B", "Crime" = "#9D9D9D", "Adventure" = "#EB8931",
                "Mystery" = "#005784", "Sci-Fi" = "#A3CE27", "Music" = "#D2B48C", "Family" = "#31A2F2", 
               "Fantasy" = "#44891A", "Western" = "#918151", "Biography" = "#2F484E", "History" = "#B2DCEF", 
               "War" = "#000000", "Film-Noir" = "#A9A9A9", "Animation" = "#CBC3E3", "Sport" = "#FFFF00",
                "NA" = "#FFFFFF")
#-----------------
colors.streamgraph <- c("Action" = "#BE2633","Adventure" = "#EB8931", "Animation" = "#CBC3E3", "Biography" = "#2F484E",
                        "Comedy" = "#800080", "Crime" = "#9D9D9D", "Drama" = "#A46422", "Family" = "#31A2F2", 
                        "Fantasy" = "#44891A", "Film-Noir" = "#A9A9A9", "History" = "#B2DCEF", "Horror" = "#1B2632", 
                        "Music" = "#D2B48C","Mystery" = "#005784",  "Romance" = "#E06F8B", "Sci-Fi" = "#A3CE27", 
                        "Sport" = "#FFFF00", "Thriller" = "#493C2B", "War" = "#000000", "Western" = "#918151",  
                        "NA" = "#FFFFFF")
#-----------------
colors.India <- c("Drama" = "#A46422", "Action" = "#BE2633", "Comedy" = "#800080", "Romance" = "#E06F8B",
            "Thriller" = "#493C2B", "Crime" = "#9D9D9D", "Mystery" = "#005784", "Family" = "#31A2F2",
            "Horror" = "#1B2632", "Music" = "#D2B48C", "Adventure" = "#EB8931", "Fantasy" = "#44891A", 
             "Biography" = "#2F484E", "History" = "#B2DCEF", "Sport" = "#FFFF00", "Sci-Fi" = "#A3CE27", 
            "Animation" = "#CBC3E3", "War" = "#000000","NA" = "#FFFFFF")
#-------------------
order <- c("Drama", "Comedy", "Romance", "Action", "Thriller", "Crime", "Horror", "Adventure", "Mystery", "Family", "Fantasy", "Music", "Sci-Fi", "Biography", "History", "War", "Animation", "Western", "Sport", "Film-Noir", NA)
count <- c(1:21)
order.matrix <- data.frame(order, count)
#-------------
order_us <- c("Drama", "Comedy", "Romance", "Action", 
              "Horror", "Thriller", "Crime", "Adventure", 
              "Mystery", "Sci-Fi","Music","Family", 
              "Fantasy", "Western","Biography", "History", 
              "War", "Film-Noir", "Animation",  "Sport",
              NA)
count <- c(1:21)
order_us.matrix <- data.frame(order_us, count)
#-------------
order_us.2000 <- c("Drama", "Comedy","Horror", "Thriller", "Action", "Romance",  "Crime", "Adventure", "Mystery", "Sci-Fi", "Family", "Fantasy", "Music", "Biography", "Animation",  "Sport", "History", "Western", "War",   NA)
count <- c(1:20)
order_us.2000.matrix <- data.frame(order_us.2000, count)
#---------------
order_India <- c("Drama", "Action","Comedy", "Romance", "Thriller", "Crime", "Family", "Mystery","Horror", "Music", "Adventure", "Fantasy", "Biography", "History", "Sport","Sci-Fi", "Animation", "War",   NA)
count <- c(1:19)
order_India.matrix <- data.frame(order_India, count)
#---------------
order_India.2000 <- c("Drama", "Comedy", "Action", "Romance", "Thriller", "Crime", "Mystery", "Family", "Horror", "Music", "Adventure", "Fantasy", "Biography", "History", "Sport","Sci-Fi", "Animation", "War", NA)
count <- c(1:19)
order_India.2000.matrix <- data.frame(order_India.2000, count)
#-------------
#########################################################################################################################
#Pie Chart of Countries
country.colors <- brewer.pal(4, name = "Set2")
count.country <- movies2 %>% count(country, name = "count.country")
count.country <- count.country[!(is.na(count.country$country) | count.country$country==""), ]#https://stackoverflow.com/questions/9126840/delete-rows-with-blank-values-in-one-particular-column
count.country[is.na(count.country)] <- 0
count.country <- count.country[order(-count.country$count.country),]
count.country$country <- factor(count.country$country, count.country$country)
pie(count.country$count.country, labels = count.country$country, col = country.colors)
#----------
#Genre Graphs:
#Pie chart of world genres by count
count.genre1 <- movies2 %>% filter(genre1 != "Adult") %>% filter(genre1 != "Documentary") %>% filter(genre1 != "Reality-TV") %>% filter(genre1 != "News") %>% count(genre1, name = "count.genre1") %>% rename(genre = genre1)
count.genre2 <- movies2 %>% filter(genre1 != "Adult") %>% filter(genre1 != "Documentary") %>% filter(genre1 != "Reality-TV") %>% filter(genre1 != "News") %>% count(genre2, name = "count.genre2") %>% rename(genre = genre2)
count.genre3 <- movies2 %>% filter(genre1 != "Adult") %>% filter(genre1 != "Documentary") %>% filter(genre1 != "Reality-TV") %>% filter(genre1 != "News") %>% count(genre3, name = "count.genre3") %>% rename(genre = genre3)
count.genre1 <- count.genre1[!(is.na(count.genre1$genre) | count.genre1$genre==""), ]#https://stackoverflow.com/questions/9126840/delete-rows-with-blank-values-in-one-particular-column
count.genre2 <- count.genre2[!(is.na(count.genre2$genre) | count.genre2$genre==""), ]
count.genre3 <- count.genre3[!(is.na(count.genre3$genre) | count.genre3$genre==""), ]
count.genre <- merge(count.genre1, count.genre2, by = "genre", all = TRUE)
count.genre <- merge(count.genre, count.genre3, by = "genre", all = TRUE)
count.genre[is.na(count.genre)] <- 0
count.genre <- count.genre %>% mutate(count.genre = count.genre1 + count.genre2 + count.genre3)
count.genre <- count.genre[order(-count.genre$count.genre),]
count.genre$genre <- factor(count.genre$genre, count.genre$genre)
pie(count.genre$count.genre, labels = count.genre$genre, col = colors)
#---------------
#Area Chart of world count
count.genre.year.genre1 <- movies2 %>% filter(year <= 2019) %>% group_by(year, genre1) %>% count(genre1, name = "count.genre1")
count.genre.year.genre2 <- movies2 %>% filter(year <= 2019) %>% group_by(year, genre2) %>% count(genre2, name = "count.genre2")
count.genre.year.genre3 <- movies2 %>% filter(year <= 2019) %>% group_by(year, genre3) %>% count(genre3, name = "count.genre3")
count.genre.year.genre1 <- count.genre.year.genre1[!(is.na(count.genre.year.genre1$genre1) | count.genre.year.genre1$genre1==""), ]#https://stackoverflow.com/questions/9126840/delete-rows-with-blank-values-in-one-particular-column
count.genre.year.genre2 <- count.genre.year.genre2[!(is.na(count.genre.year.genre2$genre2) | count.genre.year.genre2$genre2==""), ]
count.genre.year.genre3 <- count.genre.year.genre3[!(is.na(count.genre.year.genre3$genre3) | count.genre.year.genre3$genre3==""), ]
count.genre.year.genre <- merge(count.genre.year.genre1, count.genre.year.genre2, by.x = c("genre1", "year"), by.y = c("genre2", "year"), all = TRUE)
count.genre.year.genre <- merge(count.genre.year.genre, count.genre.year.genre3, by.x = c("genre1", "year"), by.y = c("genre3","year"), all = TRUE)
count.genre.year.genre <- count.genre.year.genre[!(is.na(count.genre.year.genre$year) | count.genre.year.genre$year == ""),]
count.genre.year.genre[is.na(count.genre.year.genre)] <- 0
count.genre.year.genre <- count.genre.year.genre %>% mutate(count.genre = count.genre1 + count.genre2 + count.genre3) %>% mutate(percentage.count = count.genre/sum(count.genre))
count.genre.year.genre <- count.genre.year.genre[c("genre1", "year", "count.genre")]
count.genre.year.genre <- count.genre.year.genre %>% pivot_wider(names_from = genre1, values_from = count.genre, values_fill = 0)
count.genre.year.genre <- count.genre.year.genre %>% pivot_longer(names_to = "genre1", values_to = "count.genre", Action:Western)
count.genre.year.genre <- merge(count.genre.year.genre, order.matrix, by.x = "genre1", by.y = "order", all = TRUE)
count.genre.year.genre <- na.omit(count.genre.year.genre)
count.genre.year.genre <- count.genre.year.genre[with(count.genre.year.genre, order(count, year)),]
count.genre.year.genre$genre1 <- factor(count.genre.year.genre$genre1, levels = rev(unique(count.genre.year.genre$genre1)))
ggplot(count.genre.year.genre, aes(year, count.genre, fill = genre1)) +
    geom_area()+ 
    ggtitle("Area chart of world movie count by genre by year") + 
    scale_fill_manual(values = colors, breaks = order)
#--------------------
#World Map of Countries by number 1 Genre by movie count :
country.votes.genre1 <- movies2 %>% group_by(country, genre1) %>% count(genre1, name = "count.genre1") %>% rename(genre = genre1)
country.votes.genre2 <- movies2 %>% group_by(country, genre2) %>% count(genre2, name = "count.genre2") %>% rename(genre = genre2)
country.votes.genre3 <- movies2 %>% group_by(country, genre3) %>% count(genre3, name = "count.genre3") %>% rename(genre = genre3)
country.votes.genre1 <- country.votes.genre1[!(is.na(country.votes.genre1$genre) | country.votes.genre1$genre==""), ]
country.votes.genre2 <- country.votes.genre2[!(is.na(country.votes.genre2$genre) | country.votes.genre2$genre==""), ]
country.votes.genre3 <- country.votes.genre3[!(is.na(country.votes.genre3$genre) | country.votes.genre3$genre==""), ]
country.votes.genre <- merge(country.votes.genre1, country.votes.genre2, by = c("country", "genre"), all = TRUE)
country.votes.genre <- merge(country.votes.genre, country.votes.genre3, by = c("country", "genre"), all = TRUE)
country.votes.genre[is.na(country.votes.genre)] <- 0
country.votes.genre <- country.votes.genre %>% mutate(count.genre = count.genre1 + count.genre2 + count.genre3)
country.max <- country.votes.genre %>% group_by(country) %>% summarise(max.genre = max(count.genre))
country.genre <- merge(country.votes.genre, country.max, by.x = c("country", "count.genre"), by.y = c("country", "max.genre"))
country.max1 <- country.genre %>% group_by(country) %>% summarise(max.genre1 = max(count.genre1))
country.genre <- merge(country.genre, country.max1, by.x = c("country", "count.genre1"), by.y = c("country", "max.genre1"))
world_map <- map_data("world")
world.map.genre <- left_join(world_map,country.genre, by = c("region" = "country"))
ggplot(world.map.genre) + 
    geom_polygon(aes(long, lat, group = group, fill = genre), colour = "white") + 
    scale_fill_manual(values = colors, breaks = order) + 
    ggtitle("World Map of Countries by most produced movie genre")
#------------------------------
#World Map of Countries by number 1 Genre by movie count (excluding Drama):
country.votes.genre1 <- movies2 %>% group_by(country, genre1) %>% filter(genre1 != "Drama") %>% count(genre1, name = "count.genre1") %>% rename(genre = genre1)
country.votes.genre2 <- movies2 %>% group_by(country, genre2) %>% filter(genre2 != "Drama") %>% count(genre2, name = "count.genre2") %>% rename(genre = genre2)
country.votes.genre3 <- movies2 %>% group_by(country, genre3) %>% filter(genre3 != "Drama") %>% count(genre3, name = "count.genre3") %>% rename(genre = genre3)
country.votes.genre1 <- country.votes.genre1[!(is.na(country.votes.genre1$genre) | country.votes.genre1$genre==""), ]
country.votes.genre2 <- country.votes.genre2[!(is.na(country.votes.genre2$genre) | country.votes.genre2$genre==""), ]
country.votes.genre3 <- country.votes.genre3[!(is.na(country.votes.genre3$genre) | country.votes.genre3$genre==""), ]
country.votes.genre <- merge(country.votes.genre1, country.votes.genre2, by = c("country", "genre"), all = TRUE)
country.votes.genre <- merge(country.votes.genre, country.votes.genre3, by = c("country", "genre"), all = TRUE)
country.votes.genre[is.na(country.votes.genre)] <- 0
country.votes.genre <- country.votes.genre %>% mutate(count.genre = count.genre1 + count.genre2 + count.genre3)
country.max <- country.votes.genre %>% group_by(country) %>% summarise(max.genre = max(count.genre))
country.genre <- merge(country.votes.genre, country.max, by.x = c("country", "count.genre"), by.y = c("country", "max.genre"))
country.max1 <- country.genre %>% group_by(country) %>% summarise(max.genre1 = max(count.genre1))
country.genre <- merge(country.genre, country.max1, by.x = c("country", "count.genre1"), by.y = c("country", "max.genre1"))
world_map <- map_data("world")
world.map.genre <- left_join(world_map,country.genre, by = c("region" = "country"))
ggplot(world.map.genre) + 
    geom_polygon(aes(long, lat, group = group, fill = genre), colour = "white") + 
    scale_fill_manual(values = colors, breaks = c("Drama", "Comedy", "Romance", "Action", "Thriller", "Crime", "Horror", "Adventure", "Mystery", "Family", "Fantasy", "Music", "Sci-Fi", "Biography", "History", "War", "Animation", "Western", "Sport", "Film-Noir", NA)) + 
    ggtitle("World Map of Countries by most produced movie genre (excluding Drama")
#---------------
#World Map of Countries by number 1 Genre by movie count (excluding Drama & Comedy):
country.votes.genre1 <- movies2 %>% group_by(country, genre1) %>% filter(genre1 != "Drama") %>% filter(genre1 != "Comedy") %>% count(genre1, name = "count.genre1") %>% rename(genre = genre1)
country.votes.genre2 <- movies2 %>% group_by(country, genre2) %>% filter(genre2 != "Drama") %>% filter(genre2 != "Comedy") %>% count(genre2, name = "count.genre2") %>% rename(genre = genre2)
country.votes.genre3 <- movies2 %>% group_by(country, genre3) %>% filter(genre3 != "Drama") %>% filter(genre3 != "Comedy") %>% count(genre3, name = "count.genre3") %>% rename(genre = genre3)
country.votes.genre1 <- country.votes.genre1[!(is.na(country.votes.genre1$genre) | country.votes.genre1$genre==""), ]
country.votes.genre2 <- country.votes.genre2[!(is.na(country.votes.genre2$genre) | country.votes.genre2$genre==""), ]
country.votes.genre3 <- country.votes.genre3[!(is.na(country.votes.genre3$genre) | country.votes.genre3$genre==""), ]
country.votes.genre <- merge(country.votes.genre1, country.votes.genre2, by = c("country", "genre"), all = TRUE)
country.votes.genre <- merge(country.votes.genre, country.votes.genre3, by = c("country", "genre"), all = TRUE)
country.votes.genre[is.na(country.votes.genre)] <- 0
country.votes.genre <- country.votes.genre %>% mutate(count.genre = count.genre1 + count.genre2 + count.genre3)
country.max <- country.votes.genre %>% group_by(country) %>% summarise(max.genre = max(count.genre))
country.genre <- merge(country.votes.genre, country.max, by.x = c("country", "count.genre"), by.y = c("country", "max.genre"))
country.max1 <- country.genre %>% group_by(country) %>% summarise(max.genre1 = max(count.genre1))
country.genre <- merge(country.genre, country.max1, by.x = c("country", "count.genre1"), by.y = c("country", "max.genre1"))
world_map <- map_data("world")
world.map.genre <- left_join(world_map,country.genre, by = c("region" = "country"))
ggplot(world.map.genre) + 
    geom_polygon(aes(long, lat, group = group, fill = genre), colour = "white") + 
    scale_fill_manual(values = colors, breaks = order) + 
    ggtitle("World Map of Countries by most produced movie genre (excluding Drama & Comedy)")
#---------------
#####################################################################################################################################
#Genre Country Profile USA:
#Pie char of USA genres by count
count.genre1 <- movies2 %>% filter(country == "USA") %>% count(genre1, name = "count.genre1") %>% rename(genre = genre1)
count.genre2 <- movies2 %>% filter(country == "USA") %>% count(genre2, name = "count.genre2") %>% rename(genre = genre2)
count.genre3 <- movies2 %>% filter(country == "USA") %>% count(genre3, name = "count.genre3") %>% rename(genre = genre3)
count.genre1 <- count.genre1[!(is.na(count.genre1$genre) | count.genre1$genre==""), ]#https://stackoverflow.com/questions/9126840/delete-rows-with-blank-values-in-one-particular-column
count.genre2 <- count.genre2[!(is.na(count.genre2$genre) | count.genre2$genre==""), ]
count.genre3 <- count.genre3[!(is.na(count.genre3$genre) | count.genre3$genre==""), ]
count.genre <- merge(count.genre1, count.genre2, by = "genre", all = TRUE)
count.genre <- merge(count.genre, count.genre3, by = "genre", all = TRUE)
count.genre[is.na(count.genre)] <- 0
count.genre <- count.genre %>% mutate(count.genre = count.genre1 + count.genre2 + count.genre3)
count.genre <- count.genre[order(-count.genre$count.genre),]
count.genre$genre <- factor(count.genre$genre, count.genre$genre)
#ggplot(count.genre, aes(genre, count.genre)) + 
#    geom_col() + 
#    ggtitle("Barplot of number of movies by genre from USA") + 
#    xlab("genre") + 
#    ylab("Number of movies")
pie(count.genre$count.genre, labels = count.genre$genre, col = colors.us)
#---------------
#Area Chart of USA count post 2000
count.genre.year.genre1 <- movies2 %>% filter(year >= 2000) %>% filter(year <= 2019) %>% filter(country == "USA") %>% group_by(year, genre1) %>% count(genre1, name = "count.genre1")
count.genre.year.genre2 <- movies2 %>% filter(year >= 2000) %>% filter(year <= 2019) %>% filter(country == "USA") %>% group_by(year, genre2) %>% count(genre2, name = "count.genre2")
count.genre.year.genre3 <- movies2 %>% filter(year >= 2000) %>% filter(year <= 2019) %>% filter(country == "USA") %>% group_by(year, genre3) %>% count(genre3, name = "count.genre3")
count.genre.year.genre1 <- count.genre.year.genre1[!(is.na(count.genre.year.genre1$genre1) | count.genre.year.genre1$genre1==""), ]#https://stackoverflow.com/questions/9126840/delete-rows-with-blank-values-in-one-particular-column
count.genre.year.genre2 <- count.genre.year.genre2[!(is.na(count.genre.year.genre2$genre2) | count.genre.year.genre2$genre2==""), ]
count.genre.year.genre3 <- count.genre.year.genre3[!(is.na(count.genre.year.genre3$genre3) | count.genre.year.genre3$genre3==""), ]
count.genre.year.genre <- merge(count.genre.year.genre1, count.genre.year.genre2, by.x = c("genre1", "year"), by.y = c("genre2", "year"), all = TRUE)
count.genre.year.genre <- merge(count.genre.year.genre, count.genre.year.genre3, by.x = c("genre1", "year"), by.y = c("genre3","year"), all = TRUE)
count.genre.year.genre <- count.genre.year.genre[!(is.na(count.genre.year.genre$year) | count.genre.year.genre$year == ""),]
count.genre.year.genre[is.na(count.genre.year.genre)] <- 0
count.genre.year.genre <- count.genre.year.genre %>% mutate(count.genre = count.genre1 + count.genre2 + count.genre3) %>% mutate(percentage.count = count.genre/sum(count.genre))
count.genre.year.genre <- count.genre.year.genre[c("genre1", "year", "count.genre")]
count.genre.year.genre <- count.genre.year.genre %>% pivot_wider(names_from = genre1, values_from = count.genre, values_fill = 0)
count.genre.year.genre <- count.genre.year.genre %>% pivot_longer(names_to = "genre1", values_to = "count.genre", Action:Western)
count.genre.year.genre <- merge(count.genre.year.genre, order_us.2000.matrix, by.x = "genre1", by.y = "order_us.2000", all = TRUE)
count.genre.year.genre <- na.omit(count.genre.year.genre)
count.genre.year.genre <- count.genre.year.genre[with(count.genre.year.genre, order(count, year)),]
count.genre.year.genre$genre1 <- factor(count.genre.year.genre$genre1, levels = rev(unique(count.genre.year.genre$genre1)))
ggplot(count.genre.year.genre, aes(year, count.genre, fill = genre1)) +
    geom_area()+ 
    ggtitle("Area chart of US movie count by genre by year post 2000") + 
    scale_fill_manual(values = colors, breaks = order_us.2000)
#---------------------
#Genre Country Profile India:
#Pie char of India genres by count
count.genre1 <- movies2 %>% filter(year >= 2000) %>% filter(country == "India") %>% count(genre1, name = "count.genre1") %>% rename(genre = genre1)
count.genre2 <- movies2 %>% filter(year >= 2000) %>% filter(country == "India") %>% count(genre2, name = "count.genre2") %>% rename(genre = genre2)
count.genre3 <- movies2 %>% filter(year >= 2000) %>% filter(country == "India") %>% count(genre3, name = "count.genre3") %>% rename(genre = genre3)
count.genre1 <- count.genre1[!(is.na(count.genre1$genre) | count.genre1$genre==""), ]#https://stackoverflow.com/questions/9126840/delete-rows-with-blank-values-in-one-particular-column
count.genre2 <- count.genre2[!(is.na(count.genre2$genre) | count.genre2$genre==""), ]
count.genre3 <- count.genre3[!(is.na(count.genre3$genre) | count.genre3$genre==""), ]
count.genre <- merge(count.genre1, count.genre2, by = "genre", all = TRUE)
count.genre <- merge(count.genre, count.genre3, by = "genre", all = TRUE)
count.genre[is.na(count.genre)] <- 0
count.genre <- count.genre %>% mutate(count.genre = count.genre1 + count.genre2 + count.genre3)
count.genre <- count.genre[order(-count.genre$count.genre),]
count.genre$genre <- factor(count.genre$genre, count.genre$genre)
pie(count.genre$count.genre, labels = count.genre$genre, col = colors.India)
#----------------
#Area Chart of India count post 2000
count.genre.year.genre1 <- movies2 %>% filter(year >= 2000) %>% filter(year <= 2019) %>% filter(country == "India") %>% group_by(year, genre1) %>% count(genre1, name = "count.genre1")
count.genre.year.genre2 <- movies2 %>% filter(year >= 2000) %>% filter(year <= 2019) %>% filter(country == "India") %>% group_by(year, genre2) %>% count(genre2, name = "count.genre2")
count.genre.year.genre3 <- movies2 %>% filter(year >= 2000) %>% filter(year <= 2019) %>% filter(country == "India") %>% group_by(year, genre3) %>% count(genre3, name = "count.genre3")
count.genre.year.genre1 <- count.genre.year.genre1[!(is.na(count.genre.year.genre1$genre1) | count.genre.year.genre1$genre1==""), ]#https://stackoverflow.com/questions/9126840/delete-rows-with-blank-values-in-one-particular-column
count.genre.year.genre2 <- count.genre.year.genre2[!(is.na(count.genre.year.genre2$genre2) | count.genre.year.genre2$genre2==""), ]
count.genre.year.genre3 <- count.genre.year.genre3[!(is.na(count.genre.year.genre3$genre3) | count.genre.year.genre3$genre3==""), ]
count.genre.year.genre <- merge(count.genre.year.genre1, count.genre.year.genre2, by.x = c("genre1", "year"), by.y = c("genre2", "year"), all = TRUE)
count.genre.year.genre <- merge(count.genre.year.genre, count.genre.year.genre3, by.x = c("genre1", "year"), by.y = c("genre3","year"), all = TRUE)
count.genre.year.genre <- count.genre.year.genre[!(is.na(count.genre.year.genre$year) | count.genre.year.genre$year == ""),]
count.genre.year.genre[is.na(count.genre.year.genre)] <- 0
count.genre.year.genre <- count.genre.year.genre %>% mutate(count.genre = count.genre1 + count.genre2 + count.genre3) %>% mutate(percentage.count = count.genre/sum(count.genre))
count.genre.year.genre <- count.genre.year.genre[c("genre1", "year", "count.genre")]
count.genre.year.genre <- count.genre.year.genre %>% pivot_wider(names_from = genre1, values_from = count.genre, values_fill = 0)
count.genre.year.genre <- count.genre.year.genre %>% pivot_longer(names_to = "genre1", values_to = "count.genre", Action:War)
count.genre.year.genre <- merge(count.genre.year.genre, order_India.2000.matrix, by.x = "genre1", by.y = "order_India.2000", all = TRUE)
count.genre.year.genre <- na.omit(count.genre.year.genre)
count.genre.year.genre <- count.genre.year.genre[with(count.genre.year.genre, order(count, year)),]
count.genre.year.genre$genre1 <- factor(count.genre.year.genre$genre1, levels = rev(unique(count.genre.year.genre$genre1)))
ggplot(count.genre.year.genre, aes(year, count.genre, fill = genre1)) +
    geom_area()+ 
    ggtitle("Area chart of India movie count by genre by year post 2000") + 
    scale_fill_manual(values = colors, breaks = order_India.2000)
#----------------