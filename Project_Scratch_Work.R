#Source: https://www.kaggle.com/danielgrijalvas/movies
#install.packages("dplyr")
library(tidyverse)
library(dplyr)
#---------------
setwd("D:Working_Directory/archive")
dir()
options(scipen = 999)
#------------------
#Loading Data
movies <- read_csv("movies.csv")

disney <- read_csv("Disney_Studios.csv")
#--------------
#removing Na's
#movies <- na.omit(movies) #https://stackoverflow.com/questions/4862178/remove-rows-with-all-or-some-nas-missing-values-in-data-frame
#removed cause it may have been discriminating foreign movies cause their budgets where na
#-----------------
#removing companies with less than n movies
#can't remove more than 259 movies
n = 100
movies <- data.frame(movies)
table.company <- table(movies$company)
movies <- movies[movies$company %in% names(table.company)[table.company > n],] #https://stackoverflow.com/questions/19412337/count-rows-for-selected-column-values-and-remove-rows-based-on-count-in-r
#-------------------
#Companys with over 100 movies
#[1] "Columbia Pictures Corporation"(Sony)   "Paramount Pictures"(ViaComCBS)          "Twentieth Century Fox Film Corporation"(Disney)
#[4] "Walt Disney Pictures"(Disney)          "Universal Pictures"(Comcast)            "New Line Cinema"(Warner Bros)(At&T)                       
#[7] "Warner Bros."(AT&T)                    "Touchstone Pictures"(Disney)            "Metro-Goldwyn-Mayer (MGM)"(MGM Holdings)             
#[10] "Columbia Pictures"(Sony)                
#Big enough?
(ncol(movies)*4)*(nrow(movies)/100) #https://spark.apache.org/docs/1.6.2/api/R/ncol.html
# n > 258 CHECK!
#----------------------
#Just some math
help(eapply)
#----------------------
#Barplot of frequency of movies by company
unique(movies$company)
movies.company <- movies %>% count(company, name = "count.movies") #https://dplyr.tidyverse.org/reference/count.html

movies.company <- movies.company %>% count(count.movies, name = "count.company")
ggplot(movies.company) + geom_col(aes(count.company, count.movies)) + ylim(0,25)
#----------------------
#Barplot of number of companies with x amount of movies
ggplot(movies.company) + 
  geom_col(aes( count.movies, count.company)) + 
  ggtitle("Barplot of number of companies with x amount of movies") + 
  xlab("Number of Movies") + 
  ylab("Number of Companies")

ggplot(movies.company) + 
  geom_col(aes( count.movies, log(count.company, base = exp(.1)))) + 
  ylim(0,25) + 
  ggtitle("Barplot of number of companies with log(x) amount of movies")+ 
  xlab("Number of Movies") + 
  ylab("Number of Companies")
#Barplot of number of companys who released movies by year
movies$year <- as.factor(movies$year)
movies.company.year <- movies  %>% count(company, year, name = "count.movies")
company.year <- movies.company.year %>% count(year, name = "count.company")
ggplot(company.year, aes(year, count.company)) + 
  geom_col() + 
  ggtitle("Barplot of number of companys who released movies by year") + 
  xlab("Year") + 
  ylab("Number of companies who released a movie")
#----------------------
#Want to show the story of the movie market by showing how the number of movies relesaed by companys and the number of companys releasing movies in general changes
#Show how Disney has "taken over" the "Main stream movie market"
#Show how disneys take over/netflix/lockdown has lead to an increase in "foreign movie market"/"Indie movie market"
#While it seems like disney is taking over the movie market at the same time/the actual reality is the movie
# market is bouncing back/evovling/expanding with more "indie" movies
#-------------------------
#Barplot of total number of votes by year
movies$year <- as.factor(movies$year)
votes.year <- movies  %>% group_by(year) %>% summarise(total.votes = sum(votes))
ggplot(votes.year, aes(year, total.votes)) + 
  geom_col() + 
  ggtitle("Barplot of total number of votes by year") + 
  xlab("Year") + 
  ylab("Number of Votes")
#---------------------
#Boxplots by year of number of votes by company
#excluding outliers above y = m 
m = 200000
movies$year <- as.factor(movies$year)
votes.company.year <- movies  %>% group_by(company, year) %>% summarise(total.votes = sum(votes)) #http://www.sthda.com/english/wiki/ggplot2-box-plot-quick-start-guide-r-software-and-data-visualization

ggplot(votes.company.year) + 
  geom_boxplot(aes(year, total.votes)) + 
  ylim(0,m) +#https://ggplot2.tidyverse.org/reference/lims.html
  ggtitle("Boxplots by year of number of votes by company") + 
  xlab("Year") + 
  ylab("Number of votes by company")

#----------------------
#Barplot of number of countries movies are from by year
movies$year <- as.factor(movies$year)
movies.country.year <- movies  %>% count(country, year, name = "count.movies")
country.year <- movies.country.year %>% count(year, name = "count.country")
ggplot(country.year, aes(year, count.country)) + 
  geom_col() + 
  ggtitle("Barplot of number of countries movies are from by year") + 
  xlab("Year") + 
  ylab("Number of Countries who released movies")
#-----------------------
#Boxplot of votes by country by year (exluding the number one outlier(probably america))
movies$year <- as.factor(movies$year)
votes.country.year <- movies  %>% group_by(country, year) %>% summarise(total.votes = sum(votes))
ggplot(votes.country.year) + geom_boxplot(aes(year, total.votes)) + 
  ylim(0,3000000) + 
  ggtitle("Boxplot of votes by country by year (exluding the number one outlier(probably america))") + 
  xlab("Year") + 
  ylab("Number of Votes by Country")
#-------------------------
#Voters per movie
movies$year <- as.factor(movies$year)
avg.votes.year <- movies %>% group_by(year) %>% summarise(avg.votes = mean(votes))
ggplot(avg.votes.year, aes(year, avg.votes)) + 
  geom_col() + 
  ggtitle("Barplot of average number of votes by year") + 
  xlab("Year") + 
  ylab("Average Number of Votes")
#------------------------------
#Boxplots of votes by year by movie
movies$year <- as.factor(movies$year)
ggplot(movies) + 
  geom_boxplot(aes(year, votes))
#---------------------------------
#Barplot of number of US companys who released movies by year
movies$year <- as.factor(movies$year)
movies.UScompany.year <- movies  %>% filter(country == "USA") %>% count(company, year, name = "count.movies")
UScompany.year <- movies.UScompany.year %>% count(year, name = "count.company")
ggplot(UScompany.year, aes(year, count.company)) + 
  geom_col()
#----------------
#Barplot of number of NON US companys who released movies by year
movies$year <- as.factor(movies$year)
movies.nonUScompany.year <- movies  %>% filter(country != "USA") %>% count(company, year, name = "count.movies")
nonUScompany.year <- movies.nonUScompany.year %>% count(year, name = "count.company")
ggplot(nonUScompany.year, aes(year, count.company)) + 
  geom_col()
#--------------------
#Boxplot of average number of votes per country
avg.votes.country <- movies %>% group_by(country) %>% summarise(avg.votes = mean(votes))
avg.votes.country <- avg.votes.country[order(-avg.votes.country$avg.votes),]
avg.votes.country$country <- factor(avg.votes.country$country, avg.votes.country$country)
ggplot(avg.votes.country, aes(country, avg.votes)) + 
  geom_col() + 
  ggtitle("Barplot of average number of votes by country") + 
  xlab("Country") + 
  ylab("Average Number of Votes") +
  theme(axis.text.x = element_text(angle = 45, hjust=1))
#-------------
#Boxplot of total number of votes per country
total.votes.country <- movies %>% group_by(country) %>% summarise(total.votes = sum(votes))
total.votes.country <- total.votes.country[order(-total.votes.country$total.votes),]
total.votes.country$country <- factor(total.votes.country$country, total.votes.country$country)
ggplot(total.votes.country, aes(country, total.votes)) + 
  geom_col() + 
  ggtitle("Barplot of total number of votes by country") + 
  xlab("Country") + 
  ylab("Number of Votes") +
  theme(axis.text.x = element_text(angle = 45, hjust=1))
#--------------
#Boxplot of log total number of votes per country
total.votes.country <- movies %>% group_by(country) %>% summarise(total.votes = sum(votes))
total.votes.country <- total.votes.country[order(-total.votes.country$total.votes),]
total.votes.country$country <- factor(total.votes.country$country, total.votes.country$country)
ggplot(total.votes.country, aes(country, log(total.votes))) + 
  geom_col() + 
  ggtitle("Barplot of log total number of votes by country") + 
  xlab("Country") + 
  ylab("Log Number of Votes") +
  theme(axis.text.x = element_text(angle = 45, hjust=1))
#---------------------
#Boxplot of total number of votes per country by year
y = 2001
total.votes.country <- movies %>%  filter(year == y) %>% group_by(country)  %>% summarise(total.votes = sum(votes))
total.votes.country <- total.votes.country[order(-total.votes.country$total.votes),]
total.votes.country$country <- factor(total.votes.country$country, total.votes.country$country)
ggplot(total.votes.country, aes(country, total.votes)) + 
  geom_col() + 
  ggtitle("Barplot of total number of votes by country in year y") + 
  xlab("Country") + 
  ylab("Number of Votes") +
  theme(axis.text.x = element_text(angle = 45, hjust=1))
#-------------------
#Scatter plot of all countries (except USA) total votes by year
total.votes.year.country <- movies %>% filter(year != "2016") %>% filter(country != "USA") %>% group_by(year, country) %>% summarise(total.votes = sum(votes))
total.votes.year.country <- total.votes.year.country[order(-total.votes.year.country$total.votes),]
total.votes.year.country$country <- factor(total.votes.year.country$country, total.votes.year.country$country)
ggplot(total.votes.year.country, aes(year, total.votes)) + 
  geom_point() + 
  geom_text(aes(label = country, hjust = 1, vjust = 1))
#---------------
#Scatter plot of select Countries total votes by year
countrys = c("UK", "France", "India", "Germany", "China", "New Zealand", "Spain", "Italy", "Ireland", "Japan", "Canada", "Australia",
             "Denmark", "Taiwan", "Mexico", "Brazil", "South Korea", "South Africa", "Hong Kong", "Sweden")
total.votes.year.country <- movies %>% filter(year != "2016") %>% filter(country %in% countrys) %>% group_by(year, country) %>% summarise(total.votes = sum(votes))
total.votes.year.country <- total.votes.year.country[order(-total.votes.year.country$total.votes),]
total.votes.year.country$country <- factor(total.votes.year.country$country, total.votes.year.country$country)
ggplot(total.votes.year.country, aes(year, total.votes)) + 
  geom_point() + 
  geom_text(aes(label = country, hjust = 1, vjust = 1))
#---------------
#Barplots by year of every countries log total number of votes
total.votes.year.country <- movies %>% filter(year != "2016") %>% group_by(year, country) %>% summarise(total.votes = sum(votes))
total.votes.year.country <- total.votes.year.country[order(-total.votes.year.country$total.votes),]
total.votes.year.country$country <- factor(total.votes.year.country$country, total.votes.year.country$country)
ggplot(total.votes.year.country, aes(country, log(total.votes))) + 
  geom_col() + 
  facet_wrap(~ year) +
  theme(axis.text.x = element_text(angle = 45, hjust=1))
#------------------
#Scatter plots by year of every country's log total number of votes
total.votes.year.country <- movies %>% filter(year != "2016") %>% group_by(year, country) %>% summarise(total.votes = sum(votes))
total.votes.year.country <- total.votes.year.country[order(-total.votes.year.country$total.votes),]
total.votes.year.country$country <- factor(total.votes.year.country$country, total.votes.year.country$country)
ggplot(total.votes.year.country, aes(country, log(total.votes))) + 
  geom_point() + 
  facet_grid(~year) +
  theme(axis.text.x = element_text(angle = 45, hjust=1))
#----------------
#Boxplots of votes by country by movie
movies$country <- as.factor(movies$country)
ggplot(movies) + 
  geom_boxplot(aes(country, votes))
#----------------------
########################################################################################################################
#-----------------------------------------------------------------------------------------------------------------------
########################################################################################################################
#Source: https://www.kaggle.com/stefanoleone992/imdb-extensive-dataset
#install.packages("devtools")
#install.packages("streamgraph")
#devtools::install_github("hrbrmstr/streamgraph")

library(tidyverse)
library("mapproj")
library(RColorBrewer)
library(streamgraph)
library("devtools")
#----
setwd("D:Working_Directory/Bigger_archive")
options(scipen = 999)

display.brewer.all()


movies2 <- read_csv("IMDb_movies.csv")

disney <- read_csv("Disney_Studios.csv")

#------------
ratings <- read_csv("IMDB_ratings.csv")
names <- read_csv("IMDB_names.csv")
tp <- read_csv("IMDB_title_principals.csv")
#-----------------
#movies2$country1 <- gsub(",.*", "", movies2$country)
#movies2$country2 <- sub("^.*?,", "", movies2$country)
#movies2$country3 <- sub("^.*?,", "", movies2$country2)
#movies2$country2 <- gsub(",.*", "", movies2$country2)
#---------------
#movies2$language1 <- gsub(",.*", "", movies2$language)
#movies2$language2 <- sub("^.*?,", "", movies2$language)
#movies2$language3 <- sub("^.*?,", "", movies2$language2)
#movies2$language2 <- gsub(",.*", "", movies2$language2)
#--------------https://stackoverflow.com/questions/28028110/insert-character-at-end-of-string-in-r-except-for-the-last-element
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
#Company Graphs:
#----------------------
#Barplot of number of companies with x amount of movies
movies2.company <- movies2 %>% count(production_company, name = "count.movies") #https://dplyr.tidyverse.org/reference/count.html
movies2.company <- movies2.company %>% count(count.movies, name = "count.company")
ggplot(movies2.company) + 
  geom_col(aes( count.movies, count.company)) + 
  ggtitle("Barplot of number of companies with x amount of movies") + 
  xlab("Number of Movies") + 
  ylab("Number of Companies")

ggplot(movies2.company) + 
  geom_col(aes(log(count.movies), count.company)) + 
  ylim(0,25) + 
  ggtitle("Barplot of number of companies with log(x) amount of movies")+ 
  xlab("Number of Movies") + 
  ylab("Number of Companies")
#-------------------------
#Barplot of number of companys who released movies2 by year
movies2$year <- as.factor(movies2$year)
movies2.company.year <- movies2  %>% count(production_company, year, name = "count.movies2")
company.year <- movies2.company.year %>% count(year, name = "count.company")
ggplot(company.year, aes(year, count.company)) + 
  geom_col() + 
  ggtitle("Barplot of number of companys who released movies2 by year") + 
  xlab("Year") + 
  ylab("Number of companies who released a movie") +
  theme(axis.text.x = element_text(angle = 45, hjust=1))
#-------------------------
#Boxplots by year of log number of votes by company
movies2$year <- as.factor(movies2$year)
votes.company.year <- movies2  %>% group_by(production_company, year) %>% summarise(total.votes = sum(votes)) #http://www.sthda.com/english/wiki/ggplot2-box-plot-quick-start-guide-r-software-and-data-visualization

ggplot(votes.company.year) + 
  geom_boxplot(aes(year, log(total.votes))) + 
  ggtitle("Boxplots by year of number of votes by company") + 
  xlab("Year") + 
  ylab("Number of votes by company")+
  theme(axis.text.x = element_text(angle = 45, hjust=1))
#----------------------
#Barplot of number of US companys who released movies by year
movies2$year <- as.factor(movies2$year)
movies2.UScompany.year <- movies2  %>% filter(country == "USA") %>% count(production_company, year, name = "count.movies2")
UScompany.year <- movies2.UScompany.year %>% count(year, name = "count.company")
ggplot(UScompany.year, aes(year, count.company)) + 
  geom_col() + 
  ggtitle("Barplot of number of US companys who released movies by year") + 
  xlab("Year") + 
  ylab("US companies who released movies")+
  theme(axis.text.x = element_text(angle = 45, hjust=1))
#-----------------------
#Barplot of number of NON US companys who released movies2 by year
movies2$year <- as.factor(movies2$year)
movies2.nonUScompany.year <- movies2  %>% filter(country != "USA") %>% count(production_company, year, name = "count.movies2")
nonUScompany.year <- movies2.nonUScompany.year %>% count(year, name = "count.company")
ggplot(nonUScompany.year, aes(year, count.company)) + 
  geom_col() + 
  ggtitle("Barplot of number of NON US companys who released movies by year") + 
  xlab("Year") + 
  ylab("Non-US companies who released movies")+
  theme(axis.text.x = element_text(angle = 45, hjust=1))
#-----------------------
#Disney:
production_company <- movies2 %>% summarise(production_company, year) #Joey
disney.merge <- merge(production_company, disney, by = "production_company", all = TRUE) #Joey
disney.merge <- disney.merge %>% mutate(disney.owned = ifelse(year >= acquisition & year <= till,1,0)) #Joey
movies2 <- merge(movies2, disney.merge, by = c("production_company", "year"), all = TRUE) #Joey
movies2["disney.owned"][is.na(movies2["disney.owned"])] <- 0 #Joey
movies2 <- movies2[!duplicated(movies2$imdb_title_id), ] #Joey
votes.year <- movies2  %>% group_by(year, disney.owned) %>% summarise(total.votes = sum(votes))
ggplot(votes.year, aes(x = year, y = total.votes, fill = disney.owned)) + 
  geom_col() + 
  ggtitle("Barplot of total number of votes by year") + 
  xlab("Year") + 
  ylab("Number of Votes")
#------------------------
movies2$year <- as.factor(movies2$year)
movies2.UScompany.year <- movies2 %>% count(production_company, year, name = "count.movies2")
ggplot(movies2.UScompany.year, aes(year, count.movies2, fill = production_company)) + 
  geom_area()

#-----------------------------------------------------------------------------------------------
#Vote Graphs:
#--------------------
#Barplot of total number of votes by year
movies2$year <- as.factor(movies2$year)
votes.year <- movies2  %>% group_by(year) %>% summarise(total.votes = sum(votes))
ggplot(votes.year, aes(year, total.votes)) + 
  geom_col() + 
  ggtitle("Barplot of total number of votes by year") + 
  xlab("Year") + 
  ylab("Number of Votes") +
  theme(axis.text.x = element_text(angle = 45, hjust=1))
#---------------------
#Voters per movie
movies2$year <- as.factor(movies2$year)
avg.votes.year <- movies2 %>% group_by(year) %>% summarise(avg.votes = mean(votes))
ggplot(avg.votes.year, aes(year, avg.votes)) + 
  geom_col() + 
  ggtitle("Barplot of average number of votes per movie by year") + 
  xlab("Year") + 
  ylab("Average Number of Votes per movie")+
  theme(axis.text.x = element_text(angle = 45, hjust=1))
#------------------------------
#Boxplots of log votes by movie by year
movies2$year <- as.factor(movies2$year)
ggplot(movies2) + 
  geom_boxplot(aes(year, log(votes))) + 
  ggtitle("Boxplots of log votes by movie by year") + 
  xlab("Year") + 
  ylab("Log Number of Votes per movie")+
  theme(axis.text.x = element_text(angle = 45, hjust=1))
#-------------------------------------------------------------------------------
#Country Graphs:
#---------------------
#Barplot of number of countries movies2 are from by year
movies2$year <- as.factor(movies2$year)
movies2.country1.year <- movies2  %>% count(country, year, name = "count.movies2")
country1.year <- movies2.country1.year %>% count(year, name = "count.country1")
ggplot(country1.year, aes(year, count.country1)) + 
  geom_col() + 
  ggtitle("Barplot of number of countries movies2 are from by year") + 
  xlab("Year") + 
  ylab("Number of Countries who released movies2")+
  theme(axis.text.x = element_text(angle = 45, hjust=1))
#-----------------------
#Boxplot of log votes by country1 by year
movies2$year <- as.factor(movies2$year)
votes.country1.year <- movies2  %>% group_by(country1, year) %>% summarise(total.votes = sum(votes))
ggplot(votes.country1.year) + geom_boxplot(aes(year, log(total.votes))) + 
  ggtitle("Boxplot of log votes by country1 by year)") + 
  xlab("Year") + 
  ylab("Log Number of Votes by country1")+
  theme(axis.text.x = element_text(angle = 45, hjust=1))
#-------------------------
#Barplot of average number of votes per country1
movies2$country1 <- gsub(",.*", "", movies2$country1)
avg.votes.country1 <- movies2 %>% group_by(country1) %>% summarise(avg.votes = mean(votes))
avg.votes.country1 <- avg.votes.country1[order(-avg.votes.country1$avg.votes),]
avg.votes.country1$country1 <- factor(avg.votes.country1$country1, avg.votes.country1$country1)
ggplot(avg.votes.country1, aes(country1, log(avg.votes))) + 
  geom_col() + 
  ggtitle("Barplot of log average number of votes by country1") + 
  xlab("country1") + 
  ylab("log Average Number of Votes")
#-------------
#Barplot of total number of votes per country1
movies2$country1 <- gsub(",.*", "", movies2$country1)
total.votes.country1 <- movies2 %>% group_by(country1) %>% summarise(total.votes = sum(votes))
total.votes.country1 <- total.votes.country1[order(-total.votes.country1$total.votes),]
total.votes.country1$country1 <- factor(total.votes.country1$country1, total.votes.country1$country1)
ggplot(total.votes.country1, aes(country1, total.votes)) + 
  geom_col() + 
  ggtitle("Barplot of total number of votes by country1") + 
  xlab("country1") + 
  ylab("Number of Votes")+
  theme(axis.text.x = element_text(angle = 45, hjust=1))
#--------------
#Barplot of log total number of votes per country1
movies2$country1 <- gsub(",.*", "", movies2$country1)
total.votes.country1 <- movies2 %>% group_by(country1) %>% summarise(total.votes = sum(votes))
total.votes.country1 <- total.votes.country1[order(-total.votes.country1$total.votes),]
total.votes.country1$country1 <- factor(total.votes.country1$country1, total.votes.country1$country1)
ggplot(total.votes.country1, aes(country1, log(total.votes))) + 
  geom_col() + 
  ggtitle("Barplot of log total number of votes by country1") + 
  xlab("country1") + 
  ylab("Log Number of Votes") +
  theme(axis.text.x = element_text(angle = 45, hjust=1))
#---------------------
#Boxplot of total number of votes per country1 by year
movies2$country1 <- gsub(",.*", "", movies2$country1)
y = 2001
total.votes.country1 <- movies2 %>%  filter(year == y) %>% group_by(country1)  %>% summarise(total.votes = sum(votes))
total.votes.country1 <- total.votes.country1[order(-total.votes.country1$total.votes),]
total.votes.country1$country1 <- factor(total.votes.country1$country1, total.votes.country1$country1)
ggplot(total.votes.country1, aes(country1, total.votes)) + 
  geom_col() + 
  ggtitle("Barplot of total number of votes by country1 in year y") + 
  xlab("country1") + 
  ylab("Number of Votes") +
  theme(axis.text.x = element_text(angle = 45, hjust=1))
#-------------------
#Scatter plot of all countries (except USA and UK) total votes by year
movies2$country1 <- gsub(",.*", "", movies2$country1)
total.votes.year.country1 <- movies2  %>% filter(country1 != "USA") %>% filter(country1 != "UK") %>% group_by(year, country1) %>% summarise(total.votes = sum(votes))
total.votes.year.country1 <- total.votes.year.country1[order(-total.votes.year.country1$total.votes),]
total.votes.year.country1$country1 <- factor(total.votes.year.country1$country1, total.votes.year.country1$country1)
ggplot(total.votes.year.country1, aes(year, total.votes)) + 
  geom_point() + 
  geom_text(aes(label = country1, hjust = 1, vjust = 1)) +
  theme(axis.text.x = element_text(angle = 45, hjust=1))
#---------------
#Scatter plot of select Countries total votes by year
movies2$country1 <- gsub(",.*", "", movies2$country1)
country1s = c("UK", "France", "India", "Germany", "China", "New Zealand", "Spain", "Italy", "Ireland", "Japan", "Canada", "Australia",
             "Denmark", "Taiwan", "Mexico", "Brazil", "South Korea", "South Africa", "Hong Kong", "Sweden")
total.votes.year.country1 <- movies2 %>% filter(year != "2016") %>% filter(country1 %in% country1s) %>% group_by(year, country1) %>% summarise(total.votes = sum(votes))
total.votes.year.country1 <- total.votes.year.country1[order(-total.votes.year.country1$total.votes),]
total.votes.year.country1$country1 <- factor(total.votes.year.country1$country1, total.votes.year.country1$country1)
ggplot(total.votes.year.country1, aes(year, total.votes)) + 
  geom_point() + 
  geom_text(aes(label = country1, hjust = 1, vjust = 1))
#--------------
movies2$country1 <- gsub(",.*", "", movies2$country1)
country1s = c("Australia", "Austria", "Belgium", "Brazil", "Canada", "China", "France", "Germany", "India", "Ireland", "Italy", "Japan", "Korea", "Mexico", "Netherlands", "New Zealand", "Russia", "South Korea", "South Africa", "Spain", "UK", "USA")
total.votes.year.country1 <- movies2 %>% filter(country1 %in% country1s) %>% group_by(year, country1) %>% summarise(total.votes = sum(votes))
total.votes.year.country1 <- total.votes.year.country1[order(-total.votes.year.country1$total.votes),]
total.votes.year.country1$country1 <- factor(total.votes.year.country1$country1, total.votes.year.country1$country1)
ggplot(total.votes.year.country1, aes(year, total.votes, fill = country1)) +
  geom_area()
#------------------------------------------------------------------------------
#By Genre:
unique(movies2$genre)
#---------------
#Bar char of would genres by votes
total.votes.genre1 <- movies2 %>% group_by(genre1) %>% summarise(total.votes1 = sum(votes))
total.votes.genre2 <- movies2 %>% group_by(genre2) %>% summarise(total.votes2 = sum(votes))
total.votes.genre3 <- movies2 %>% group_by(genre3) %>% summarise(total.votes3 = sum(votes))
total.votes.genre1 <- total.votes.genre1[!(is.na(total.votes.genre1$genre1) | total.votes.genre1$genre1==""), ]#https://stackoverflow.com/questions/9126840/delete-rows-with-blank-values-in-one-particular-column
total.votes.genre2 <- total.votes.genre2[!(is.na(total.votes.genre2$genre2) | total.votes.genre2$genre2==""), ]
total.votes.genre3 <- total.votes.genre3[!(is.na(total.votes.genre3$genre3) | total.votes.genre3$genre3==""), ]
total.votes.genre <- merge(total.votes.genre1, total.votes.genre2, by.x = c("genre1"), by.y = c("genre2"), all = TRUE)
total.votes.genre <- merge(total.votes.genre, total.votes.genre3, by.x = c("genre1"), by.y = c("genre3"), all = TRUE)
total.votes.genre[is.na(total.votes.genre)] <- 0
total.votes.genre <- total.votes.genre %>% mutate(total.votes = total.votes1 + total.votes2 + total.votes3)
total.votes.genre <- total.votes.genre[order(-total.votes.genre$total.votes),]
total.votes.genre$genre <- factor(total.votes.genre$genre, total.votes.genre$genre)
ggplot(total.votes.genre, aes(genre, total.votes)) + 
  geom_col() + 
  ggtitle("Barplot of total number of votes by genre") + 
  xlab("genre") + 
  ylab("Number of Votes")
#--------------
#Bar char of world genres by count
count.genre1 <- movies2 %>% count(genre1, name = "count.genre1") %>% rename(genre = genre1)
count.genre2 <- movies2 %>% count(genre2, name = "count.genre2") %>% rename(genre = genre2)
count.genre3 <- movies2 %>% count(genre3, name = "count.genre3") %>% rename(genre = genre3)
count.genre <- merge(count.genre1, count.genre2, by = "genre", all = TRUE)
count.genre <- merge(count.genre, count.genre3, by = "genre", all = TRUE)
count.genre <- na.omit(count.genre)
count.genre <- count.genre %>% mutate(count.genre = count.genre1 + count.genre2 + count.genre3)

count.genre <- count.genre[order(-count.genre$count.genre),]
count.genre$genre <- factor(count.genre$genre, count.genre$genre)
ggplot(count.genre, aes(genre, count.genre)) + 
  geom_col() + 
  ggtitle("Barplot of number of movies by genre") + 
  xlab("genre") + 
  ylab("Number of movies")
#----------------
#Stream Graph of world votes
total.votes.year.genre1 <- movies2 %>% filter(year <= 2019) %>% group_by(year, genre1) %>% summarise(total.votes1 = sum(votes))
total.votes.year.genre2 <- movies2 %>% filter(year <= 2019) %>% group_by(year, genre2) %>% summarise(total.votes2 = sum(votes))
total.votes.year.genre3 <- movies2 %>% filter(year <= 2019) %>% group_by(year, genre3) %>% summarise(total.votes3 = sum(votes))

total.votes.year.genre1 <- total.votes.year.genre1[!(is.na(total.votes.year.genre1$genre1) | total.votes.year.genre1$genre1==""), ]#https://stackoverflow.com/questions/9126840/delete-rows-with-blank-values-in-one-particular-column
total.votes.year.genre2 <- total.votes.year.genre2[!(is.na(total.votes.year.genre2$genre2) | total.votes.year.genre2$genre2==""), ]
total.votes.year.genre3 <- total.votes.year.genre3[!(is.na(total.votes.year.genre3$genre3) | total.votes.year.genre3$genre3==""), ]
total.votes.year.genre <- merge(total.votes.year.genre1, total.votes.year.genre2, by.x = c("genre1", "year"), by.y = c("genre2", "year"), all = TRUE)
total.votes.year.genre <- merge(total.votes.year.genre, total.votes.year.genre3, by.x = c("genre1", "year"), by.y = c("genre3", "year"), all = TRUE)
total.votes.year.genre[is.na(total.votes.year.genre)] <- 0
total.votes.year.genre <- total.votes.year.genre %>% mutate(total.votes = total.votes1 + total.votes2 + total.votes3)

total.votes.year.genre <- total.votes.year.genre[order(-total.votes.year.genre$total.votes),]
total.votes.year.genre$genre <- factor(total.votes.year.genre$genre, levels = rev(unique(total.votes.year.genre$genre)), ordered = TRUE)

total.votes.year.genre %>% streamgraph("genre1", "total.votes", "year", offset = "zero", interpolate = 'cardinal')




#ggplot(total.votes.year.genre, aes(year, total.votes, fill = genre1)) +
#  geom_area()+ 
#  ggtitle("Area chart of world total votes per genre by year")


#ggplot(total.votes.year.genre, aes(year, percentage.votes, fill = genre)) +
#  geom_area()+ 
#  ggtitle("Area chart of world percentage votes per genre by year")
#--------------
#Area Chart by COunt of genres
genres <- c("Action", "Comedy", "Drama", "Crime", "Adventure", "Animation", "Biography", "Horror")
movies2.genre.year <- movies2 %>% filter(genre %in% genres) %>% count(genre, year, name = "count.movies2") %>% mutate(percentage.count = count.movies2/sum(count.movies2))
ggplot(movies2.genre.year, aes(year, count.movies2, fill = genre)) +
  geom_area()+ 
  ggtitle("Area chart of world movie count by genre by year")

ggplot(movies2.genre.year, aes(year, percentage.count, fill = genre)) +
  geom_area()+ 
  ggtitle("Area chart of world percentage movie count by genre by year")
#-----------------------
genres <- c("Action", "Comedy", "Drama", "Crime", "Adventure", "Animation", "Biography", "Horror")
total.votes.year.genre <- movies2 %>% filter(genre %in% genres) %>% group_by(year, genre) %>% summarise(total.votes = sum(votes)) %>% mutate(percentage.votes = total.votes/sum(total.votes))
total.votes.year.genre <- total.votes.year.genre[order(-total.votes.year.genre$total.votes),]
total.votes.year.genre$genre <- factor(total.votes.year.genre$genre, total.votes.year.genre$genre)
#line Chart of total votes by genre
ggplot(total.votes.year.genre, aes(year, total.votes, color = genre)) + 
  geom_line()
#line chart of percentage votes by genre
ggplot(total.votes.year.genre, aes(year, percentage.votes, color = genre)) + 
  geom_line()
#-------------
#Genre Country Profile USA:
#Bar char of USAn genres by count
count.genre <- movies2 %>% filter(country == "USA") %>% count(genre, name = "count.genre")
count.genre <- count.genre[order(-count.genre$count.genre),]
count.genre$genre <- factor(count.genre$genre, count.genre$genre)
ggplot(count.genre, aes(genre, count.genre)) + 
  geom_col() + 
  ggtitle("Barplot of number of movies by genre from USA") + 
  xlab("genre") + 
  ylab("Number of movies")
#--------------
genres <- c("Action", "Comedy", "Drama", "Crime", "Adventure", "Animation", "Biography", "Horror")
total.votes.year.genre <- movies2 %>% filter(genre %in% genres) %>% filter(country == "USA") %>% group_by(year, genre) %>% summarise(total.votes = sum(votes)) %>% mutate(percentage.votes = total.votes/sum(total.votes))
total.votes.year.genre$genre <- factor(total.votes.year.genre$genre, total.votes.year.genre$genre)
ggplot(total.votes.year.genre, aes(year, percentage.votes, fill = genre)) +
  geom_area()+ 
  ggtitle("Area chart of percentage votes of movies by genre from USA")

ggplot(total.votes.year.genre, aes(year, total.votes, fill = genre)) +
  geom_area() + 
  ggtitle("Area chart of total votes of movies by genre from USA")

ggplot(total.votes.year.genre, aes(year, total.votes, color = genre)) + 
  geom_line()
#------------
genres <- c("Action", "Comedy", "Drama", "Crime", "Adventure", "Animation", "Biography", "Horror")
count.genre.year.genre <- movies2 %>% filter(genre %in% genres) %>% filter(country == "USA") %>% filter(year <= 2019) %>% group_by(year, genre) %>% count(genre, name = "count.genre") %>% mutate(percentage.genre = count.genre/sum(count.genre))
count.genre.year.genre <- count.genre.year.genre[order(-count.genre.year.genre$count.genre),]
count.genre.year.genre$genre <- factor(count.genre.year.genre$genre, count.genre.year.genre$genre)

ggplot(count.genre.year.genre, aes(year, percentage.genre, fill = genre)) +
  geom_area() + 
  ggtitle("Area chart of percentage count of movies by genre from USA")

ggplot(count.genre.year.genre, aes(year, count.genre, fill = genre)) +
  geom_area() + 
  ggtitle("Area chart of count of movies by genre from USA")
#---------
#Stream Graph of US
genres <- c("Action", "Comedy", "Drama", "Crime", "Adventure", "Animation", "Biography", "Horror")
count.genre.year.genre <- movies2 %>% 
  filter(genre %in% genres) %>% filter(country == "USA") %>% filter(year <= 2019) %>% 
  group_by(year, genre) %>% 
  count(genre, name = "count.genre") %>% 
  mutate(percentage.genre = count.genre/sum(count.genre))

count.genre.year.genre <- count.genre.year.genre[order(-count.genre.year.genre$count.genre),]
count.genre.year.genre$genre <- factor(count.genre.year.genre$genre, count.genre.year.genre$genre)

count.genre.year.genre %>% streamgraph("genre", "count.genre", "year", offset = "zero", interpolate = 'cardinal') %>% sg_legend(show = FALSE)

#---------
#Genre Country Profile INDIA:
#Bar char of Indian genres by count
count.genre <- movies2 %>% filter(country == "India") %>% count(genre, name = "count.genre")
count.genre <- count.genre[order(-count.genre$count.genre),]
count.genre$genre <- factor(count.genre$genre, count.genre$genre)
ggplot(count.genre, aes(genre, count.genre)) + 
  geom_col() + 
  ggtitle("Barplot of number of movies by genre from India") + 
  xlab("genre") + 
  ylab("Number of movies")
#--------------
genres <- c("Action", "Comedy", "Drama", "Crime", "Adventure", "Animation", "Biography", "Horror")
total.votes.year.genre <- movies2 %>% filter(genre %in% genres) %>% filter(country == "India") %>% group_by(year, genre) %>% summarise(total.votes = sum(votes)) %>% mutate(percentage.votes = total.votes/sum(total.votes))
total.votes.year.genre$genre <- factor(total.votes.year.genre$genre, total.votes.year.genre$genre)
ggplot(total.votes.year.genre, aes(year, percentage.votes, fill = genre)) +
  geom_area()

ggplot(total.votes.year.genre, aes(year, total.votes, fill = genre)) +
  geom_area()

ggplot(total.votes.year.genre, aes(year, total.votes, color = genre)) + 
  geom_line()
#------------
genres <- c("Action", "Comedy", "Drama", "Crime", "Adventure", "Animation", "Biography", "Horror")
count.genre.year.genre <- movies2 %>% filter(genre %in% genres) %>% filter(country == "India") %>% filter(year <= 2019) %>% group_by(year, genre) %>% count(genre, name = "count.genre") %>% mutate(percentage.genre = count.genre/sum(count.genre))
count.genre.year.genre <- count.genre.year.genre[order(-count.genre.year.genre$count.genre),]
count.genre.year.genre$genre <- factor(count.genre.year.genre$genre, count.genre.year.genre$genre)

ggplot(count.genre.year.genre, aes(year, percentage.votes, fill = genre)) +
  geom_area()

ggplot(count.genre.year.genre, aes(year, count.genre, fill = genre)) +
  geom_area()
#---------
#Genre Country Profile GERMANY:
#Bar char of Germany genres by count
count.genre <- movies2 %>% filter(country == "Germany")%>% filter(year <= 2019) %>% count(genre, name = "count.genre")
count.genre <- count.genre[order(-count.genre$count.genre),]
count.genre$genre <- factor(count.genre$genre, count.genre$genre)
ggplot(count.genre, aes(genre, count.genre)) + 
  geom_col() + 
  ggtitle("Barplot of number of movies by genre from Germany") + 
  xlab("genre") + 
  ylab("Number of movies")
#--------------
genres <- c("Action", "Comedy", "Drama", "Crime", "Adventure", "Animation", "Biography", "Horror")
total.votes.year.genre <- movies2 %>% filter(genre %in% genres) %>% filter(country == "Germany")%>% filter(year <= 2019) %>% group_by(year, genre) %>% summarise(total.votes = sum(votes)) %>% mutate(percentage.votes = total.votes/sum(total.votes))
total.votes.genre <- total.votes.genre[order(-total.votes.genre$total.votes),]
total.votes.year.genre$genre <- factor(total.votes.year.genre$genre, total.votes.year.genre$genre)
ggplot(total.votes.year.genre, aes(year, percentage.votes, fill = genre)) +
  geom_area()

ggplot(total.votes.year.genre, aes(year, total.votes, fill = genre)) +
  geom_area()

ggplot(total.votes.year.genre, aes(year, total.votes, color = genre)) + 
  geom_line()
#--------------
genres <- c("Action", "Comedy", "Drama", "Crime", "Adventure", "Animation", "Biography", "Horror")
count.genre.year.genre <- movies2 %>% filter(genre %in% genres) %>% filter(country == "Germany")%>% filter(year <= 2019) %>% group_by(year, genre) %>% count(genre, name = "count.genre") %>% mutate(percentage.genre = count.genre/sum(count.genre))
count.genre.year.genre <- count.genre.year.genre[order(-count.genre.year.genre$count.genre),]
count.genre.year.genre$genre <- factor(count.genre.year.genre$genre, count.genre.year.genre$genre)

ggplot(count.genre.year.genre, aes(year, percentage.votes, fill = genre)) +
  geom_area()

ggplot(count.genre.year.genre, aes(year, count.genre, fill = genre)) +
  geom_area()
#------------
#Genre Country Profile Mongolia:
#Bar char of Mongolia genres by count
count.genre1 <- movies2 %>% filter(country == "Mongolia") %>% count(genre1, name = "count.genre1") %>% rename(genre = genre1)
count.genre2 <- movies2 %>% filter(country == "Mongolia") %>% count(genre2, name = "count.genre2") %>% rename(genre = genre2)
count.genre3 <- movies2 %>% filter(country == "Mongolia") %>% count(genre3, name = "count.genre3") %>% rename(genre = genre3)
count.genre1 <- count.genre1[!(is.na(count.genre1$genre) | count.genre1$genre==""), ]#https://stackoverflow.com/questions/9126840/delete-rows-with-blank-values-in-one-particular-column
count.genre2 <- count.genre2[!(is.na(count.genre2$genre) | count.genre2$genre==""), ]
count.genre3 <- count.genre3[!(is.na(count.genre3$genre) | count.genre3$genre==""), ]
count.genre <- merge(count.genre1, count.genre2, by = "genre", all = TRUE)
count.genre <- merge(count.genre, count.genre3, by = "genre", all = TRUE)
count.genre[is.na(count.genre)] <- 0
count.genre <- count.genre %>% mutate(count.genre = count.genre1 + count.genre2 + count.genre3)
count.genre <- count.genre[order(-count.genre$count.genre),]
count.genre$genre <- factor(count.genre$genre, count.genre$genre)
ggplot(count.genre, aes(genre, count.genre)) + 
  geom_col() + 
  ggtitle("Barplot of number of movies by genre from Mongolia") + 
  xlab("genre") + 
  ylab("Number of movies")
#-----------
#Genre Country Profile Saudi Arabia:
#Bar char of Saudi Arabia genres by count
count.genre1 <- movies2 %>% filter(country == "Saudi Arabia") %>% count(genre1, name = "count.genre1") %>% rename(genre = genre1)
count.genre2 <- movies2 %>% filter(country == "Saudi Arabia") %>% count(genre2, name = "count.genre2") %>% rename(genre = genre2)
count.genre3 <- movies2 %>% filter(country == "Saudi Arabia") %>% count(genre3, name = "count.genre3") %>% rename(genre = genre3)
count.genre1 <- count.genre1[!(is.na(count.genre1$genre) | count.genre1$genre==""), ]#https://stackoverflow.com/questions/9126840/delete-rows-with-blank-values-in-one-particular-column
count.genre2 <- count.genre2[!(is.na(count.genre2$genre) | count.genre2$genre==""), ]
count.genre3 <- count.genre3[!(is.na(count.genre3$genre) | count.genre3$genre==""), ]
count.genre <- merge(count.genre1, count.genre2, by = "genre", all = TRUE)
count.genre <- merge(count.genre, count.genre3, by = "genre", all = TRUE)
count.genre[is.na(count.genre)] <- 0
count.genre <- count.genre %>% mutate(count.genre = count.genre1 + count.genre2 + count.genre3)
count.genre <- count.genre[order(-count.genre$count.genre),]
count.genre$genre <- factor(count.genre$genre, count.genre$genre)
ggplot(count.genre, aes(genre, count.genre)) + 
  geom_col() + 
  ggtitle("Barplot of number of movies by genre from Saudi Arabia") + 
  xlab("genre") + 
  ylab("Number of movies")
#-----------
#Genre Country Profile JAPAN:
#Bar char of Japan genres by count
count.genre1 <- movies2 %>% filter(country == "Japan") %>% count(genre1, name = "count.genre1") %>% rename(genre = genre1)
count.genre2 <- movies2 %>% filter(country == "Japan") %>% count(genre2, name = "count.genre2") %>% rename(genre = genre2)
count.genre3 <- movies2 %>% filter(country == "Japan") %>% count(genre3, name = "count.genre3") %>% rename(genre = genre3)
count.genre1 <- count.genre1[!(is.na(count.genre1$genre) | count.genre1$genre==""), ]#https://stackoverflow.com/questions/9126840/delete-rows-with-blank-values-in-one-particular-column
count.genre2 <- count.genre2[!(is.na(count.genre2$genre) | count.genre2$genre==""), ]
count.genre3 <- count.genre3[!(is.na(count.genre3$genre) | count.genre3$genre==""), ]
count.genre <- merge(count.genre1, count.genre2, by = "genre", all = TRUE)
count.genre <- merge(count.genre, count.genre3, by = "genre", all = TRUE)
count.genre[is.na(count.genre)] <- 0
count.genre <- count.genre %>% mutate(count.genre = count.genre1 + count.genre2 + count.genre3)
count.genre <- count.genre[order(-count.genre$count.genre),]
count.genre$genre <- factor(count.genre$genre, count.genre$genre)
ggplot(count.genre, aes(genre, count.genre)) + 
  geom_col() + 
  ggtitle("Barplot of number of movies by genre from Japan") + 
  xlab("genre") + 
  ylab("Number of movies")
#-----------------
#Stream Graph of votes
genres <- c("Action", "Comedy", "Drama", "Crime", "Adventure", "Animation", "Biography", "Horror")
total.votes.year.genre1 <- movies2 %>% filter(genre1 %in% genres) %>% filter(country == "Japan") %>% filter(year <= 2019) %>% group_by(year, genre1) %>% summarise(total.votes1 = sum(votes))
total.votes.year.genre2 <- movies2 %>% filter(genre2 %in% genres) %>% filter(country == "Japan") %>% filter(year <= 2019) %>% group_by(year, genre2) %>% summarise(total.votes2 = sum(votes))
total.votes.year.genre3 <- movies2 %>% filter(genre3 %in% genres) %>% filter(country == "Japan") %>% filter(year <= 2019) %>% group_by(year, genre3) %>% summarise(total.votes3 = sum(votes))
total.votes.year.genre1 <- total.votes.year.genre1[!(is.na(total.votes.year.genre1$genre1) | total.votes.year.genre1$genre1==""), ]#https://stackoverflow.com/questions/9126840/delete-rows-with-blank-values-in-one-particular-column
total.votes.year.genre2 <- total.votes.year.genre2[!(is.na(total.votes.year.genre2$genre2) | total.votes.year.genre2$genre2==""), ]
total.votes.year.genre3 <- total.votes.year.genre3[!(is.na(total.votes.year.genre3$genre3) | total.votes.year.genre3$genre3==""), ]
total.votes.year.genre <- merge(total.votes.year.genre1, total.votes.year.genre2, by.x = c("genre1", "year"), by.y = c("genre2", "year"), all = TRUE)
total.votes.year.genre <- merge(total.votes.year.genre, total.votes.year.genre3, by.x = c("genre1", "year"), by.y = c("genre3", "year"), all = TRUE)
total.votes.year.genre[is.na(total.votes.year.genre)] <- 0
total.votes.year.genre <- total.votes.year.genre %>% mutate(total.votes = total.votes1 + total.votes2 + total.votes3)


#total.votes.year.genre <- total.votes.year.genre[order(-total.votes.year.genre$total.votes),]
#total.votes.year.genre$genre <- factor(total.votes.year.genre$genre, levels = rev(unique(total.votes.year.genre$genre)), ordered = TRUE)

total.votes.year.genre %>% streamgraph("genre1", "total.votes", "year", offset = "zero", interpolate = 'cardinal')
#Areachart of percentage & of total votes of movies by genre from Japan

#ggplot(total.votes.year.genre, aes(year, percentage.votes, fill = genre)) +
#  geom_area() + 
#  ggtitle("Areachart of percentage of total votes of movies by genre from Japan")

ggplot(total.votes.year.genre, aes(year, total.votes, fill = genre1)) +
  geom_area(alpha = 1/2) + 
  stat_smooth(
    geom = 'area', method = 'loess', span = 1/3,
    alpha = 1/2, fill = "red") + 
  labs(title = "Smooth with `span = 1/3`") +
  ggtitle("Areachart of total votes of movies by genre from Japan")
#---------------------
#Stream Graph of count of japan with all genres
count.genre.year.genre1 <- movies2  %>% filter(country == "Japan") %>% group_by(year, genre1) %>% count(genre1, name = "count.genre1")
count.genre.year.genre2 <- movies2  %>% filter(country == "Japan") %>% group_by(year, genre2) %>% count(genre2, name = "count.genre2")
count.genre.year.genre3 <- movies2  %>% filter(country == "Japan") %>% group_by(year, genre3) %>% count(genre3, name = "count.genre3")
count.genre.year.genre1 <- count.genre.year.genre1[!(is.na(count.genre.year.genre1$genre1) | count.genre.year.genre1$genre1==""), ]#https://stackoverflow.com/questions/9126840/delete-rows-with-blank-values-in-one-particular-column
count.genre.year.genre2 <- count.genre.year.genre2[!(is.na(count.genre.year.genre2$genre2) | count.genre.year.genre2$genre2==""), ]
count.genre.year.genre3 <- count.genre.year.genre3[!(is.na(count.genre.year.genre3$genre3) | count.genre.year.genre3$genre3==""), ]
count.genre.year.genre <- merge(count.genre.year.genre1, count.genre.year.genre2, by.x = c("genre1", "year"), by.y = c("genre2", "year"), all = TRUE)
count.genre.year.genre <- merge(count.genre.year.genre, count.genre.year.genre3, by.x = c("genre1", "year"), by.y = c("genre3","year"), all = TRUE)
count.genre.year.genre[is.na(count.genre.year.genre)] <- 0
count.genre.year.genre <- count.genre.year.genre %>% mutate(count.genre = count.genre1 + count.genre2 + count.genre3)



#count.genre.year.genre <- count.genre.year.genre[order(-count.genre.year.genre$count.genre),]
#count.genre.year.genre$genre1 <- factor(count.genre.year.genre$genre1, levels = rev(unique(total.votes.year.genre$genre1)), ordered = TRUE)

count.genre.year.genre %>% streamgraph("genre1", "count.genre", "year", offset = "zero", interpolate = 'cardinal') %>% sg_legend(show = TRUE)

#Areachart of percentage count of movies by genre from Japan
#ggplot(count.genre.year.genre) +
#  geom_area(aes(year, percentage.genre, fill = genre), position = 'stack') + 
#  ggtitle("Areachart of percentage count of movies by genre from Japan")

#ggplot(count.genre.year.genre) +
#  geom_line(aes(year, percentage.genre, fill = genre), position = 'stack') + 
#  ggtitle("Areachart of percentage count of movies by genre from Japan")

ggplot(count.genre.year.genre, aes(year, count.genre, fill = genre1)) +
  geom_area() + 
  ggtitle("Areachart of total count of movies by genre from Japan")

ggplot(count.genre.year.genre, aes(year, count.genre, color = genre1)) +
  geom_line() + 
  ggtitle("Areachart of total count of movies by genre from Japan")
#---------------------

#---------------
#Genre Country Profile Brazil:
#Bar char of Brazil genres by count
count.genre <- movies2 %>% filter(country == "Brazil") %>% count(genre1, name = "count.genre")
count.genre <- count.genre[order(-count.genre$count.genre),]
count.genre$genre1 <- factor(count.genre$genre1, count.genre$genre1)
ggplot(count.genre, aes(genre1, count.genre)) + 
  geom_col() + 
  ggtitle("Barplot of number of movies by genre from Brazil") + 
  xlab("genre") + 
  ylab("Number of movies")
#-----------
#Areachart of percentage & of total votes of movies by genre from Brazil
genres <- c("Action", "Comedy", "Drama", "Crime", "Adventure", "Animation", "Biography", "Horror")
total.votes.year.genre <- movies2 %>% filter(genre %in% genres) %>% filter(country == "Brazil")%>% filter(year <= 2019) %>% group_by(year, genre) %>% summarise(total.votes = sum(votes)) %>% mutate(percentage.votes = total.votes/sum(total.votes))
total.votes.year.genre <- total.votes.year.genre[order(-total.votes.year.genre$total.votes),]
total.votes.year.genre$genre <- factor(total.votes.year.genre$genre, total.votes.year.genre$genre)
ggplot(total.votes.year.genre, aes(year, percentage.votes, fill = genre)) +
  geom_area() + 
  ggtitle("Areachart of percentage of total votes of movies by genre from Brazil")

ggplot(total.votes.year.genre, aes(year, total.votes, fill = genre)) +
  geom_area() + 
  ggtitle("Areachart of total votes of movies by genre from Brazil")
#----------
#Areachart of percentage count of movies by genre from Brazil
genres <- c("Action", "Comedy", "Drama", "Crime", "Adventure", "Animation", "Biography", "Horror")
count.genre.year.genre <- movies2 %>% filter(genre %in% genres) %>% filter(country == "Brazil")%>% filter(year <= 2019) %>% group_by(year, genre) %>% count(genre, name = "count.genre") %>% mutate(percentage.genre = count.genre/sum(count.genre))
count.genre.year.genre <- count.genre.year.genre[order(-count.genre.year.genre$count.genre),]
count.genre.year.genre$genre <- factor(count.genre.year.genre$genre, count.genre.year.genre$genre)
count.genre.year.genre <- data.frame(count.genre.year.genre)

ggplot(count.genre.year.genre) +
  geom_area(aes(year, percentage.genre, fill = genre), position = 'stack') + 
  ggtitle("Areachart of percentage count of movies by genre from Brazil")

ggplot(count.genre.year.genre) +
  geom_line(aes(year, percentage.genre, fill = genre), position = 'stack') + 
  ggtitle("Areachart of percentage count of movies by genre from Brazil")

ggplot(count.genre.year.genre, aes(year, count.genre, fill = genre)) +
  geom_area() + 
  ggtitle("Areachart of total count of movies by genre from Brazil")

ggplot(count.genre.year.genre, aes(year, count.genre, color = genre)) +
  geom_line() + 
  ggtitle("Areachart of total count of movies by genre from Brazil")
#-----------------
#Stream Graph of votes
genres <- c("Action", "Comedy", "Drama", "Crime", "Adventure", "Animation", "Biography", "Horror")
total.votes.year.genre <- movies2 %>% filter(genre %in% genres) %>% filter(country == "Brazil")%>% filter(year <= 2019) %>% group_by(year, genre) %>% summarise(total.votes = sum(votes)) %>% mutate(percentage.votes = total.votes/sum(total.votes))
total.votes.year.genre <- total.votes.year.genre[order(-total.votes.year.genre$total.votes),]
total.votes.year.genre$genre <- factor(total.votes.year.genre$genre, levels = rev(unique(total.votes.year.genre$genre)), ordered = TRUE)

total.votes.year.genre %>% streamgraph("genre", "total.votes", "year", offset = "zero", interpolate = 'cardinal')
#---------------------
#Stream Graph of count of Brazil with limited genres
genres <- c("Action", "Comedy", "Drama", "Animation", "Horror")
count.genre.year.genre <- movies2 %>% filter(genre %in% genres) %>% filter(country == "Brazil")%>% filter(year <= 2019) %>% group_by(year, genre) %>% count(genre, name = "count.genre") %>% mutate(sum.genre = sum(count.genre))
count.genre.year.genre <- count.genre.year.genre[order(-count.genre.year.genre$count.genre),]
count.genre.year.genre$genre <- factor(count.genre.year.genre$genre, levels = rev(unique(total.votes.year.genre$genre)), ordered = TRUE)
#count.genre.year.genre <- data.frame(count.genre.year.genre)

count.genre.year.genre %>% streamgraph("genre", "count.genre", "year", offset = "zero", interpolate = 'cardinal') %>% sg_legend(show = TRUE)
#---------------------
#Stream Graph of count of Brazil with all genres
count.genre.year.genre <- movies2 %>% filter(country == "Brazil")%>% filter(year <= 2019) %>% group_by(year, genre) %>% count(genre, name = "count.genre") %>% mutate(sum.genre = sum(count.genre))
count.genre.year.genre <- count.genre.year.genre[order(-count.genre.year.genre$count.genre),]
count.genre.year.genre$genre <- factor(count.genre.year.genre$genre, levels = rev(unique(total.votes.year.genre$genre)), ordered = TRUE)

count.genre.year.genre %>% streamgraph("genre", "count.genre", "year", interpolate = 'cardinal') %>% sg_legend(show = TRUE)


#-----------
#Genre Country Profile Czech Republic:
#Bar char of Czech genres by count
count.genre1 <- movies2 %>% filter(country == "Czech Republic") %>% count(genre1, name = "count.genre1") %>% rename(genre = genre1)
count.genre2 <- movies2 %>% filter(country == "Czech Republic") %>% count(genre2, name = "count.genre2") %>% rename(genre = genre2)
count.genre3 <- movies2 %>% filter(country == "Czech Republic") %>% count(genre3, name = "count.genre3") %>% rename(genre = genre3)
count.genre1 <- count.genre1[!(is.na(count.genre1$genre) | count.genre1$genre==""), ]#https://stackoverflow.com/questions/9126840/delete-rows-with-blank-values-in-one-particular-column
count.genre2 <- count.genre2[!(is.na(count.genre2$genre) | count.genre2$genre==""), ]
count.genre3 <- count.genre3[!(is.na(count.genre3$genre) | count.genre3$genre==""), ]
count.genre <- merge(count.genre1, count.genre2, by = "genre", all = TRUE)
count.genre <- merge(count.genre, count.genre3, by = "genre", all = TRUE)
count.genre[is.na(count.genre)] <- 0
count.genre <- count.genre %>% mutate(count.genre = count.genre1 + count.genre2 + count.genre3)
count.genre <- count.genre[order(-count.genre$count.genre),]
count.genre$genre <- factor(count.genre$genre, count.genre$genre)
ggplot(count.genre, aes(genre, count.genre)) + 
  geom_col() + 
  ggtitle("Barplot of number of movies by genre from Czech Republic") + 
  xlab("genre") + 
  ylab("Number of movies")
#-------------------------
#Bar char of Czech genres by votes
total.votes.genre1 <- movies2 %>% filter(country == "Czech Republic") %>% group_by(genre1) %>% summarise(total.votes1 = sum(votes))
total.votes.genre2 <- movies2 %>% filter(country == "Czech Republic") %>% group_by(genre2) %>% summarise(total.votes2 = sum(votes))
total.votes.genre3 <- movies2 %>% filter(country == "Czech Republic") %>% group_by(genre3) %>% summarise(total.votes3 = sum(votes))
total.votes.genre1 <- total.votes.genre1[!(is.na(total.votes.genre1$genre1) | total.votes.genre1$genre1==""), ]#https://stackoverflow.com/questions/9126840/delete-rows-with-blank-values-in-one-particular-column
total.votes.genre2 <- total.votes.genre2[!(is.na(total.votes.genre2$genre2) | total.votes.genre2$genre2==""), ]
total.votes.genre3 <- total.votes.genre3[!(is.na(total.votes.genre3$genre3) | total.votes.genre3$genre3==""), ]
total.votes.genre <- merge(total.votes.genre1, total.votes.genre2, by.x = c("genre1"), by.y = c("genre2"), all = TRUE)
total.votes.genre <- merge(total.votes.genre, total.votes.genre3, by.x = c("genre1"), by.y = c("genre3"), all = TRUE)
total.votes.genre[is.na(total.votes.genre)] <- 0
total.votes.genre <- total.votes.genre %>% mutate(total.votes = total.votes1 + total.votes2 + total.votes3)
total.votes.genre <- total.votes.genre[order(-total.votes.genre$total.votes),]
total.votes.genre$genre <- factor(total.votes.genre$genre, total.votes.genre$genre)
ggplot(total.votes.genre, aes(genre, total.votes)) + 
  geom_col() + 
  ggtitle("Barplot of total number of votes by genre from Czech Republic") + 
  xlab("genre") + 
  ylab("Number of Votes")
#---------
display.brewer.all()
#----------
colors = c("Action" = "#FF0000", "Adventure" = "#00FFFF", "Animation" = "#32cd32", "Biography" = "#FFA500", "Comedy" = "#800080", "Crime" = "#FFFF00	", "Drama" = "#FFFFFF", "Family" = "#89CFF0	", "Horror" = "#000000", "Musical" = "#30D5C8", "Mystery" = "#00008b", "Romance" = "#ffddf4", "Sci-Fi" = "#008000", "Thriller" = "#808080	", "War" = "#964B00", "NA" = "#8B0000")

colors = c("Action" = "#FF0000", "Adventure" = "#00FFFF", "Animation" = "#32cd32", 
           "Biography" = "#FFA500", "Comedy" = "#800080", "Crime" = "#FFFF00	", 
           "Drama" = "#FFFFFF", "Sci-Fi" = "#008000", "NA" = "#8B0000")
#---------------
#World Map of Countries by number 1 Genre by total votes:
country.votes.genre1 <- movies2 %>% group_by(country, genre1) %>% summarise(total.votes1 = sum(votes)) 
country.votes.genre2 <- movies2 %>% group_by(country, genre2) %>% summarise(total.votes2 = sum(votes))
country.votes.genre3 <- movies2 %>% group_by(country, genre3) %>% summarise(total.votes3 = sum(votes))
country.votes.genre1 <- country.votes.genre1[!(is.na(country.votes.genre1$genre1) | country.votes.genre1$genre1==""), ]
country.votes.genre2 <- country.votes.genre2[!(is.na(country.votes.genre2$genre2) | country.votes.genre2$genre2==""), ]
country.votes.genre3 <- country.votes.genre3[!(is.na(country.votes.genre3$genre3) | country.votes.genre3$genre3==""), ]
country.votes.genre <- merge(country.votes.genre1, country.votes.genre2, by.x = c("genre1", "country"), by.y = c("genre2", "country"), all = TRUE)
country.votes.genre <- merge(country.votes.genre, country.votes.genre3, by.x = c("genre1", "country"), by.y = c("genre3", "country"), all = TRUE)
country.votes.genre[is.na(country.votes.genre)] <- 0
country.votes.genre <- country.votes.genre %>% mutate(total.votes = total.votes1 + total.votes2 + total.votes3)
country.max <- country.votes.genre %>% group_by(country) %>% summarise(max.votes = max(total.votes))
country.genre <- merge(country.votes.genre, country.max, by.x = c("country", "total.votes"), by.y = c("country", "max.votes"))
world_map <- map_data("world")
world.map.genre <- left_join(world_map,country.genre, by = c("region" = "country"))
ggplot(world.map.genre) + 
  geom_polygon(aes(long, lat, group = group, fill = genre1), colour = "black") +
  ggtitle("World Map of Countries by most popular movie genre produced that country") +
  scale_colour_manual(values = colors)
#---------------
#World Map of Countries by number 1 Genre by total votes (excluding Drama & Comedy):
country.votes.genre1 <- movies2 %>% group_by(country, genre1) %>% filter(genre1 != "Drama") %>% filter(genre1 != "Comedy") %>% summarise(total.votes1 = sum(votes)) 
country.votes.genre2 <- movies2 %>% group_by(country, genre2) %>% filter(genre2 != "Drama") %>% filter(genre2 != "Comedy") %>% summarise(total.votes2 = sum(votes))
country.votes.genre3 <- movies2 %>% group_by(country, genre3) %>% filter(genre3 != "Drama") %>% filter(genre3 != "Comedy") %>% summarise(total.votes3 = sum(votes))
country.votes.genre1 <- country.votes.genre1[!(is.na(country.votes.genre1$genre1) | country.votes.genre1$genre1==""), ]
country.votes.genre2 <- country.votes.genre2[!(is.na(country.votes.genre2$genre2) | country.votes.genre2$genre2==""), ]
country.votes.genre3 <- country.votes.genre3[!(is.na(country.votes.genre3$genre3) | country.votes.genre3$genre3==""), ]
country.votes.genre <- merge(country.votes.genre1, country.votes.genre2, by.x = c("genre1", "country"), by.y = c("genre2", "country"), all = TRUE)
country.votes.genre <- merge(country.votes.genre, country.votes.genre3, by.x = c("genre1", "country"), by.y = c("genre3", "country"), all = TRUE)
country.votes.genre[is.na(country.votes.genre)] <- 0
country.votes.genre <- country.votes.genre %>% mutate(total.votes = total.votes1 + total.votes2 + total.votes3)
country.max <- country.votes.genre %>% group_by(country) %>% summarise(max.votes = max(total.votes))
country.genre <- merge(country.votes.genre, country.max, by.x = c("country", "total.votes"), by.y = c("country", "max.votes"))
world_map <- map_data("world")
world.map.genre <- left_join(world_map,country.genre, by = c("region" = "country"))
ggplot(world.map.genre) + 
  geom_polygon(aes(long, lat, group = group, fill = genre1), colour = "black") +
  ggtitle("World Map of Countries by most popular movie genre produced that country") +
  scale_colour_manual(values = colors)
#--------------------
#World Map of Countries by number 2 Genre by total votes:
country.votes.genre2 <- movies2 %>% group_by(country, genre2) %>% summarise(total.votes = sum(votes)) 
country.max <- country.votes.genre2 %>% group_by(country)  %>% summarise(max.votes = max(total.votes))
country.genre2 <- merge(country.votes.genre2, country.max, by.x = c("country", "total.votes"), by.y = c("country", "max.votes"))
world_map <- map_data("world")
world.map.genre2 <- left_join(world_map,country.genre2, by = c("region" = "country"))
ggplot(world.map.genre2) + 
  geom_polygon(aes(long, lat, group = group, fill = genre2), colour = "black")# + 
  #scale_fill_brewer(palette = "Set1")
#--------------
#World Map of Countries by number 3 Genre by total votes:
country.votes.genre3 <- movies2 %>% group_by(country, genre3) %>% summarise(total.votes = sum(votes)) 
country.max <- country.votes.genre3 %>% group_by(country)  %>% summarise(max.votes = max(total.votes))
country.genre3 <- merge(country.votes.genre3, country.max, by.x = c("country", "total.votes"), by.y = c("country", "max.votes"))
world_map <- map_data("world")
world.map.genre3 <- left_join(world_map,country.genre3, by = c("region" = "country"))
ggplot(world.map.genre3) + 
  geom_polygon(aes(long, lat, group = group, fill = genre3), colour = "black") #+ 
  #scale_fill_brewer(palette = "Set1")
#------------------------
#World Map of Countries by number 1 Genre by movie count:
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
world_map <- map_data("world")
world.map.genre <- left_join(world_map,country.genre, by = c("region" = "country"))
ggplot(world.map.genre) + 
  geom_polygon(aes(long, lat, group = group, fill = genre), colour = "black") + 
  scale_fill_brewer(palette = "Set1") + 
  ggtitle("World Map of Countries by most produced movie genre")
#------------------------------
#World Map of Countries by number 1 Genre by movie count (excluding Drama & Comedy:
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
world_map <- map_data("world")
world.map.genre <- left_join(world_map,country.genre, by = c("region" = "country"))
ggplot(world.map.genre) + 
  geom_polygon(aes(long, lat, group = group, fill = genre), colour = "black") + 
  #scale_fill_brewer(palette = "Set3") + 
  ggtitle("World Map of Countries by most produced movie genre")
#---------------
#Europe^^
genre <- c("Biography", "Fantasy", "Sport", "History", "Animation", 
           "Western", "Adventure", "Musical", "Romance", "Sci-Fi", "Mystery",
           "Horror")
europe <- c("Austria","Belgium","Bulgaria","Croatia","Cyprus",
                   "Czech Rep.","Denmark","Estonia","Finland","France",
                   "Germany","Greece","Hungary","Ireland","Italy","Latvia",
                   "Lithuania","Luxembourg","Malta","Netherlands","Poland",
                   "Portugal","Romania","Slovakia","Slovenia","Spain",
                   "Sweden","United Kingdom", "Switzerland")
europe_map <- map_data("world", region = europe)
europe.map.genre <- left_join(europe_map,country.genre, by = c("region" = "country"))

ggplot(europe.map.genre) + 
  geom_polygon(aes(long, lat, group = group, fill = genre), colour = "black") +
  scale_fill_brewer(palette = "Set1") + 
  ggtitle("Europe Map of Countries by most produced movie genre")
#----------------
colors = c("Action" = "#FF0000", "Adventure" = "#00FFFF", "Animation" = "#32cd32", "Biography" = "#FFA500", "Crime" = "#FFFF00	", "Family" = "#89CFF0	", "Horror" = "#000000", "Musical" = "#30D5C8", "Mystery" = "#00008b", "Romance" = "#ffddf4", "Sci-Fi" = "#008000", "Thriller" = "#808080	", "War" = "#964B00")
#World Map of Countries by number 1 Genre (except drama & Comedy) by movie count:
country.votes.genre <- movies2 %>% filter(genre != "Drama") %>% filter(genre != "Comedy") %>% group_by(country, genre) %>% count(genre, name = "count.genre") 
country.max <- country.votes.genre %>% group_by(country)  %>% summarise(max.genre = max(count.genre))
country.genre <- merge(country.votes.genre, country.max, by.x = c("country", "count.genre"), by.y = c("country", "max.genre"))
world_map <- map_data("world")
world.map.genre <- left_join(world_map,country.genre, by = c("region" = "country"))
ggplot(world.map.genre) + 
  geom_polygon(aes(long, lat, group = group, fill = genre), colour = "black") +
  ggtitle("World Map of Countries by most produced movie genre (except drama & Comedy)") +
  scale_colour_manual(values = colors)
#----------------
#World Map of Countries by number 2 Genre by movie count:
country.votes.genre2 <- movies2 %>% group_by(country, genre2) %>% count(genre2, name = "count.genre") 
country.max <- country.votes.genre2 %>% group_by(country)  %>% summarise(max.genre = max(count.genre))
country.genre2 <- merge(country.votes.genre2, country.max, by.x = c("country", "count.genre"), by.y = c("country", "max.genre"))
world_map <- map_data("world")
world.map.genre2 <- left_join(world_map,country.genre2, by = c("region" = "country"))
ggplot(world.map.genre2) + 
  geom_polygon(aes(long, lat, group = group, fill = genre2), colour = "black") + 
  scale_fill_brewer(palette = "Set1")
#--------------
#World Map of Countries by number 3 Genre by movie count:
country.votes.genre3 <- movies2 %>% group_by(country, genre3) %>% count(genre3, name = "count.genre") 
country.max <- country.votes.genre3 %>% group_by(country)  %>% summarise(max.genre = max(count.genre))
country.genre3 <- merge(country.votes.genre3, country.max, by.x = c("country", "count.genre"), by.y = c("country", "max.genre"))
world_map <- map_data("world")
world.map.genre3 <- left_join(world_map,country.genre3, by = c("region" = "country"))
ggplot(world.map.genre3) + 
  geom_polygon(aes(long, lat, group = group, fill = genre3), colour = "black") + 
  scale_fill_brewer(palette = "Set1")
#------------------------------------------------------------------------------
#Language graphs:
#Barplot of number of languages with x amount of movies
movies2$language <- gsub(",.*", "", movies2$language)
movies2.language <- movies2 %>% filter(language != "English") %>% count(language, name = "count.movies") #https://dplyr.tidyverse.org/reference/count.html
movies2.language <- movies2.language %>% count(count.movies, name = "count.language")
ggplot(movies2.language) + 
  geom_col(aes( count.movies, count.language)) + 
  ggtitle("Barplot of number of languages with x amount of movies") + 
  xlab("Number of Movies") + 
  ylab("Number of languages")

ggplot(movies2.language) + 
  geom_col(aes( count.movies, log(count.language, base = exp(.1)))) + 
  ylim(0,25) + 
  ggtitle("Barplot of number of languages with log(x) amount of movies")+ 
  xlab("Number of Movies") + 
  ylab("Number of languages")
#--------------------------
#Barplot of number of languages who released movies2 by year
movies2$language <- gsub(",.*", "", movies2$language)
movies2$year <- as.factor(movies2$year)
movies2.language.year <- movies2  %>% count(language, year, name = "count.movies2")
language.year <- movies2.language.year %>% count(year, name = "count.language")
ggplot(language.year, aes(year, count.language)) + 
  geom_col() + 
  ggtitle("Barplot of number of languages who released movies2 by year") + 
  xlab("Year") + 
  ylab("Number of companies who released a movie") +
  theme(axis.text.x = element_text(angle = 45, hjust=1))
#----------------------------
#Boxplots by year of log number of votes by language
movies2$language <- gsub(",.*", "", movies2$language)
movies2$year <- as.factor(movies2$year)
votes.language.year <- movies2  %>% group_by(language, year) %>% summarise(total.votes = sum(votes)) #http://www.sthda.com/english/wiki/ggplot2-box-plot-quick-start-guide-r-software-and-data-visualization

ggplot(votes.language.year) + 
  geom_boxplot(aes(year, log(total.votes))) + 
  ggtitle("Boxplots by year of number of votes by language") + 
  xlab("Year") + 
  ylab("Number of votes by language") +
  theme(axis.text.x = element_text(angle = 45, hjust=1))
#-----------------------
#Boxplot of log votes by language by year (exluding the number one outlier(probably america))
movies2$language <- gsub(",.*", "", movies2$language)
movies2$year <- as.factor(movies2$year)
votes.language.year <- movies2  %>% group_by(language, year) %>% summarise(total.votes = sum(votes))
ggplot(votes.language.year) + geom_boxplot(aes(year, log(total.votes))) + 
  ggtitle("Boxplot of log votes by language by year)") + 
  xlab("Year") + 
  ylab("Log Number of Votes by language")+
  theme(axis.text.x = element_text(angle = 45, hjust=1))
#-------------------------
#Scatter plot of all language (except USA) total votes by year
movies2$language <- gsub(",.*", "", movies2$language)
total.votes.year.language <- movies2 %>% filter(language != "English") %>% group_by(year, language) %>% summarise(total.votes = sum(votes))
total.votes.year.language <- total.votes.year.language[order(-total.votes.year.language$total.votes),]
total.votes.year.language$language <- factor(total.votes.year.language$language, total.votes.year.language$language)
ggplot(total.votes.year.language, aes(year, log(total.votes))) + 
  geom_point() + 
  geom_text(aes(label = language, hjust = 1, vjust = 1)) +
  theme(axis.text.x = element_text(angle = 45, hjust=1))
#---------------
#Scatter plot of select language (except USA) total votes by year
movies2$language <- gsub(",.*", "", movies2$language)
languages = c("French", "Japanese", "Italian", "Spanish", "Hindi", "Korean", "German", "Portugese", "Swedish")
total.votes.year.language <- movies2 %>% filter(language == languages) %>% group_by(year, language) %>% summarise(total.votes = sum(votes))
total.votes.year.language <- total.votes.year.language[order(-total.votes.year.language$total.votes),]
total.votes.year.language$language <- factor(total.votes.year.language$language, total.votes.year.language$language)
ggplot(total.votes.year.language, aes(year, total.votes)) + 
  geom_point() + 
  geom_text(aes(label = language, hjust = 1, vjust = 1)) +
  theme(axis.text.x = element_text(angle = 45, hjust=1))
#-------------------
#Genre Language Profile English:
genres <- c("Action", "Comedy", "Drama", "Crime", "Adventure", "Animation", "Biography", "Horror")
total.votes.year.genre <- movies2 %>% filter(genre %in% genres) %>% filter(language == "English") %>% group_by(year, genre) %>% summarise(total.votes = sum(votes)) %>% mutate(percentage.votes = total.votes/sum(total.votes))
total.votes.year.genre$genre <- factor(total.votes.year.genre$genre, total.votes.year.genre$genre)
ggplot(total.votes.year.genre, aes(year, percentage.votes, fill = genre)) +
  geom_area()

ggplot(total.votes.year.genre, aes(year, total.votes, fill = genre)) +
  geom_area()

ggplot(total.votes.year.genre, aes(year, total.votes, color = genre)) + 
  geom_line()

total.votes.genre <- movies2 %>% filter(language == "English") %>% group_by(genre) %>% summarise(total.votes = sum(votes))
total.votes.genre <- total.votes.genre[order(-total.votes.genre$total.votes),]
total.votes.genre$genre <- factor(total.votes.genre$genre, total.votes.genre$genre)
ggplot(total.votes.genre, aes(genre, total.votes)) + 
  geom_col() + 
  ggtitle("Barplot of total number of votes by genre") + 
  xlab("genre") + 
  ylab("Number of Votes")
#-------------
#Genre Language Profile Spanish:
genres <- c("Action", "Comedy", "Drama", "Crime", "Adventure", "Animation", "Biography", "Horror")
total.votes.year.genre <- movies2 %>% filter(genre %in% genres) %>% filter(language == "Spanish") %>% group_by(year, genre) %>% summarise(total.votes = sum(votes)) %>% mutate(percentage.votes = total.votes/sum(total.votes))
total.votes.year.genre$genre <- factor(total.votes.year.genre$genre, total.votes.year.genre$genre)
ggplot(total.votes.year.genre, aes(year, percentage.votes, fill = genre)) +
  geom_area()

ggplot(total.votes.year.genre) +
  geom_area(aes(year, total.votes, fill = genre))

ggplot(total.votes.year.genre, aes(year, total.votes, color = genre)) + 
  geom_line()

total.votes.genre <- movies2 %>% filter(language == "Spanish") %>% group_by(genre) %>% summarise(total.votes = sum(votes))
total.votes.genre <- total.votes.genre[order(-total.votes.genre$total.votes),]
total.votes.genre$genre <- factor(total.votes.genre$genre, total.votes.genre$genre)
ggplot(total.votes.genre, aes(genre, total.votes)) + 
  geom_col() + 
  ggtitle("Barplot of total number of votes by genre") + 
  xlab("genre") + 
  ylab("Number of Votes")
#-----------------
#Genre Language Profile Chinese:
genres <- c("Action", "Comedy", "Drama", "Crime", "Adventure", "Animation", "Biography", "Horror")
total.votes.year.genre <- movies2 %>% filter(genre %in% genres) %>% filter(language == "Chinese") %>% group_by(year, genre) %>% summarise(total.votes = sum(votes)) %>% mutate(percentage.votes = total.votes/sum(total.votes))
total.votes.year.genre$genre <- factor(total.votes.year.genre$genre, total.votes.year.genre$genre)
ggplot(total.votes.year.genre) +
  geom_area(aes(year, percentage.votes, fill = genre))+ 
  scale_fill_brewer(palette = "Set1")

ggplot(total.votes.year.genre) +
  geom_area(aes(year, total.votes, fill = genre))+ 
  scale_fill_brewer(palette = "Set1")

ggplot(total.votes.year.genre, aes(year, total.votes, color = genre)) + 
  geom_line()

total.votes.genre <- movies2 %>% filter(language == "Chinese") %>% group_by(genre) %>% summarise(total.votes = sum(votes))
total.votes.genre <- total.votes.genre[order(-total.votes.genre$total.votes),]
total.votes.genre$genre <- factor(total.votes.genre$genre, total.votes.genre$genre)
ggplot(total.votes.genre, aes(genre, total.votes)) + 
  geom_col() + 
  ggtitle("Barplot of total number of votes by genre") + 
  xlab("genre") + 
  ylab("Number of Votes")
#--------------------
#German
total.votes.genre <- movies2 %>% filter(language == "German") %>% group_by(genre) %>% summarise(total.votes = sum(votes))
total.votes.genre <- total.votes.genre[order(-total.votes.genre$total.votes),]
total.votes.genre$genre <- factor(total.votes.genre$genre, total.votes.genre$genre)
ggplot(total.votes.genre, aes(genre, total.votes)) + 
  geom_col() + 
  ggtitle("Barplot of total number of votes by genre") + 
  xlab("genre") + 
  ylab("Number of Votes")
#------------
#Japanese
total.votes.genre <- movies2 %>% filter(language == "Japanese") %>% group_by(genre) %>% summarise(total.votes = sum(votes))
total.votes.genre <- total.votes.genre[order(-total.votes.genre$total.votes),]
total.votes.genre$genre <- factor(total.votes.genre$genre, total.votes.genre$genre)
ggplot(total.votes.genre, aes(genre, total.votes)) + 
  geom_col() + 
  ggtitle("Barplot of total number of votes by genre in Japan") + 
  xlab("genre") + 
  ylab("Number of Votes")
#------------
###################################################################
#THESE DONT REALLY WORK BUT THEY DO
#Barplots by year of every countries log total number of votes
total.votes.year.country <- movies2 %>% filter(year != "2016") %>% group_by(year, country) %>% summarise(total.votes = sum(votes))
total.votes.year.country <- total.votes.year.country[order(-total.votes.year.country$total.votes),]
total.votes.year.country$country <- factor(total.votes.year.country$country, total.votes.year.country$country)
ggplot(total.votes.year.country, aes(country, log(total.votes))) + 
  geom_col() + 
  facet_wrap(~ year)# +
#theme(axis.text.x = element_text(angle = 45, hjust=1))
#------------------
#Scatter plots by year of every country's log total number of votes
total.votes.year.country <- movies2 %>% filter(year != "2016") %>% group_by(year, country) %>% summarise(total.votes = sum(votes))
total.votes.year.country <- total.votes.year.country[order(-total.votes.year.country$total.votes),]
total.votes.year.country$country <- factor(total.votes.year.country$country, total.votes.year.country$country)
ggplot(total.votes.year.country, aes(country, log(total.votes))) + 
  geom_point() + 
  facet_grid(~year) +
  theme(axis.text.x = element_text(angle = 45, hjust=1))
#----------------
#Boxplots of votes by country by movie
movies2$country <- as.factor(movies2$country)
ggplot(movies2) + 
  geom_boxplot(aes(country, votes))
################################################################################################################################
#SCRATCH WORK:
#-----------

movies2$country <- gsub(",.*", "", movies2$country)
movies2$language <- gsub(",.*", "", movies2$language)








total.votes.year.country <- movies2 %>% filter(year != "2016") %>% filter(country != "USA") %>% group_by(year, country) %>% summarise(total.votes = sum(votes))
total.votes.year.country <- total.votes.year.country[order(-total.votes.year.country$total.votes),]
total.votes.year.country$country <- factor(total.votes.year.country$country, total.votes.year.country$country)
ggplot(total.votes.year.country, aes(year, total.votes)) + 
  geom_point() + 
  geom_text(aes(label = country, hjust = 1, vjust = 1)) +
  theme(axis.text.x = element_text(angle = 45, hjust=1))


#Barplot of total number of votes by year with diseny percentage
#disney <- c("Walt Disney Pictures", "Disneynature", "Marvel Studios", "Lucasfilm", "Industrial Light & Magic", "Skywalker Sound", "20th Century Studios", "20th Digital Studio", "20th Century Family", "Searchlight Pictures","Walt Disney Animation Studios", "Pixar", "20th Century Animation","Buena Vista Motion Pictures Group","Walt Disney Motion Pictures Group","Touchstone Pictures","Hollywood Pictures","Caravan Pictures","Miramax Films", "Dimension Films","Fox 2000 Pictures", "Skellington Productions", "Disney Circle 7 Animation", "ImageMovers Digital", "Disneytoon Studios","Blue Sky Studios","Walt Disney Television", "Touchstone Television")

#movies.disney <- movies %>% mutate(disney.owned = case_when((company %in% disney) ~ 1))

#movies$year <- as.factor(movies$year)

#votes.year <- movies.disney  %>% group_by(year, disney.owned) %>% summarise(total.votes = sum(votes))

#ggplot(votes.year, aes(x = year, y = total.votes, fill = disney.owned)) + 
#  geom_col() + 
#  ggtitle("Barplot of total number of votes by year") + 
#  xlab("Year") + 
#  ylab("Number of Votes")
#---------------------------
#Barplot of total number of votes by year with diseny percentage
#disney <- c("Walt Disney Pictures", "Disneynature", "Marvel Studios", "Lucasfilm", "Industrial Light & Magic", 
#            "Skywalker Sound", "20th Century Studios", "20th Digital Studio", "20th Century Family", "Searchlight Pictures",
#            "Walt Disney Animation Studios", "Pixar", "20th Century Animation","Buena Vista Motion Pictures Group",
#            "Walt Disney Motion Pictures Group","Touchstone Pictures","Hollywood Pictures","Caravan Pictures",
#            "Miramax Films", "Dimension Films","Fox 2000 Pictures", "Skellington Productions", "Disney Circle 7 Animation",
#            "ImageMovers Digital", "Disneytoon Studios","Blue Sky Studios","Walt Disney Television", "Touchstone Television",
#            "The Muppets Studio", "Kingdom Comics", "Fox VFX Lab")
# & year >= disney$`Year of Aquisition` & year <= disney$`Ownded till`
#company <- movies$company
#company <- data.frame(company)
#disney <- data.frame(disney)
#disney <- rbind.fill(disney, company)

#movies <- data.frame(movies)
#movies2$newcol <- ifelse(movies2$production_company %in% disney$production_company,ifelse(movies2$year >= disney$acquisition,1,0),0)

#movies.disney <- merge(movies2, disney, by = "production_company", all = TRUE)
#movies.disney$acquisition[is.na(movies.disney$acquisition)] <- 0
#movies.disney$till[is.na(movies.disney$till)] <- 0
#movies.disney <- movies.disney %>% mutate(disney_owned = ifelse(year >= disney$acquisition & year <= disney$till, 1, 0))
#movies.disney <- distinct(movies.disney)
# & (disney$acquisition <= year) & (disney$till >= year)
#& year >= disney$acquisition & year <= disney$till
movies2 <- read_csv("IMDb_movies.csv")

disney <- read_csv("Disney_Studios.csv")

movies2.unique <- unique(movies2$imdb_title_id)
#disney <- data.frame(disney)

#disney$disney_owned <- 1

#movies2 <- left_join(movies2, disney) %>% mutate(disney_owned = replace(disney_owned, is.na(disney_owned), NA))

#movies2.disney <- movies2 %>% 
#  mutate(disney.owned = ifelse(
#    (production_company %in% disney$production_company) & (between(movies2$year, disney$acquisition, disney$till)) ,1, 0))

#movies2.disney <- movies2 %>% mutate(disney.owned = ifelse(production_company == disney$production_company,1,NA))


#movies2.disney$disney_owned <- ifelse(movies2.disney$year >= disney$acquisition & movies2.disney$year <= disney$till, 1, NA)

#movies2.disney <- movies2 %>% 
#  mutate(disney.owned.year = ifelse(year >= disney$acquisition & year <= disney$till,1, 0))



movies$year <- as.factor(movies$year)
movies.company.year <- movies  %>% count(company, year, name = "count.movies")
company.year <- movies.company.year %>% count(year, name = "count.company")
ggplot(company.year, aes(year, count.company)) + 
  geom_col() + 




movies %>% group_by(company, year == "1999")

movies.company <- movies %>% group_by(company, year) %>% summarise(total = sum(budget)) %>% filter(total > 0) %>% arrange(company, year)

matrix <- tapply(movies$budget, list(Company = movies$company, Year = movies$year), sum)
matrix <- as_tibble(matrix, rownames = "Company")

movies.company.tidy <- matrix %>% pivot_longer(cols = -Company, names_to = "Year", values_to = "Total")

ggplot(movies.company.tidy) + 
  geom_tile(aes(Company, Year, fill = Total))



hist(movies.company$total)
ggplot(movies.company) + 
  geom_col(aes(year, total))





hist(movies$budget)
boxplot(movies$budget)

hist(movies$gross)
boxplot(movies$gross)

hist(movies$runtime)
boxplot(movies$runtime)

hist(movies$votes)
boxplot(movies$votes)

hist(movies$score)
boxplot(movies$score)

hist(movies$year)
boxplot(movies$year)