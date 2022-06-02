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
library(tidyverse)
library(dplyr)

setwd("D:Working_Directory/Bigger_archive")
options(scipen = 999)
dir()
movies2 <- read_csv("IMDb_movies.csv")

disney <- read_csv("Disney_Studios.csv")
#----------------------
movies2.company <- movies2 %>% count(production_company, name = "count.movies") #https://dplyr.tidyverse.org/reference/count.html
movies2.company <- movies2.company %>% count(count.movies, name = "count.company")
#Barplot of number of companies with x amount of movies
ggplot(movies2.company) + 
  geom_col(aes( count.movies, count.company)) + 
  ggtitle("Barplot of number of companies with x amount of movies") + 
  xlab("Number of Movies") + 
  ylab("Number of Companies")

ggplot(movies2.company) + 
  geom_col(aes( count.movies, log(count.company, base = exp(.1)))) + 
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
#Barplot of number of countries movies2 are from by year
movies2$year <- as.factor(movies2$year)
movies2.country.year <- movies2  %>% count(country, year, name = "count.movies2")
country.year <- movies2.country.year %>% count(year, name = "count.country")
ggplot(country.year, aes(year, count.country)) + 
  geom_col() + 
  ggtitle("Barplot of number of countries movies2 are from by year") + 
  xlab("Year") + 
  ylab("Number of Countries who released movies2")+
  theme(axis.text.x = element_text(angle = 45, hjust=1))
#-----------------------
#Boxplot of log votes by country by year
movies2$year <- as.factor(movies2$year)
votes.country.year <- movies2  %>% group_by(country, year) %>% summarise(total.votes = sum(votes))
ggplot(votes.country.year) + geom_boxplot(aes(year, log(total.votes))) + 
  ggtitle("Boxplot of log votes by country by year)") + 
  xlab("Year") + 
  ylab("Log Number of Votes by Country")+
  theme(axis.text.x = element_text(angle = 45, hjust=1))
#-------------------------
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
#---------------------------------
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
#----------------
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
#--------------------
#Barplot of average number of votes per country
movies2$country <- gsub(",.*", "", movies2$country)
avg.votes.country <- movies2 %>% group_by(country) %>% summarise(avg.votes = mean(votes))
avg.votes.country <- avg.votes.country[order(-avg.votes.country$avg.votes),]
avg.votes.country$country <- factor(avg.votes.country$country, avg.votes.country$country)
ggplot(avg.votes.country, aes(country, log(avg.votes))) + 
  geom_col() + 
  ggtitle("Barplot of log average number of votes by country") + 
  xlab("Country") + 
  ylab("log Average Number of Votes")
#-------------
#Barplot of total number of votes per country
movies2$country <- gsub(",.*", "", movies2$country)
total.votes.country <- movies2 %>% group_by(country) %>% summarise(total.votes = sum(votes))
total.votes.country <- total.votes.country[order(-total.votes.country$total.votes),]
total.votes.country$country <- factor(total.votes.country$country, total.votes.country$country)
ggplot(total.votes.country, aes(country, total.votes)) + 
  geom_col() + 
  ggtitle("Barplot of total number of votes by country") + 
  xlab("Country") + 
  ylab("Number of Votes")
#--------------
#Barplot of log total number of votes per country
movies2$country <- gsub(",.*", "", movies2$country)
total.votes.country <- movies2 %>% group_by(country) %>% summarise(total.votes = sum(votes))
total.votes.country <- total.votes.country[order(-total.votes.country$total.votes),]
total.votes.country$country <- factor(total.votes.country$country, total.votes.country$country)
ggplot(total.votes.country, aes(country, log(total.votes))) + 
  geom_col() + 
  ggtitle("Barplot of log total number of votes by country") + 
  xlab("Country") + 
  ylab("Log Number of Votes")# +
  #theme(axis.text.x = element_text(angle = 45, hjust=1))
#---------------------
#Boxplot of total number of votes per country by year
movies2$country <- gsub(",.*", "", movies2$country)
y = 2001
total.votes.country <- movies2 %>%  filter(year == y) %>% group_by(country)  %>% summarise(total.votes = sum(votes))
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
movies2$country <- gsub(",.*", "", movies2$country)
total.votes.year.country <- movies2 %>% filter(year != "2016") %>% filter(country != "USA") %>% filter(country != "UK") %>% group_by(year, country) %>% summarise(total.votes = sum(votes))
total.votes.year.country <- total.votes.year.country[order(-total.votes.year.country$total.votes),]
total.votes.year.country$country <- factor(total.votes.year.country$country, total.votes.year.country$country)
ggplot(total.votes.year.country, aes(year, total.votes)) + 
  geom_point() + 
  geom_text(aes(label = country, hjust = 1, vjust = 1)) +
  theme(axis.text.x = element_text(angle = 45, hjust=1))
#---------------
#Scatter plot of select Countries total votes by year
movies2$country <- gsub(",.*", "", movies2$country)
countrys = c("UK", "France", "India", "Germany", "China", "New Zealand", "Spain", "Italy", "Ireland", "Japan", "Canada", "Australia",
             "Denmark", "Taiwan", "Mexico", "Brazil", "South Korea", "South Africa", "Hong Kong", "Sweden")
total.votes.year.country <- movies2 %>% filter(year != "2016") %>% filter(country %in% countrys) %>% group_by(year, country) %>% summarise(total.votes = sum(votes))
total.votes.year.country <- total.votes.year.country[order(-total.votes.year.country$total.votes),]
total.votes.year.country$country <- factor(total.votes.year.country$country, total.votes.year.country$country)
ggplot(total.votes.year.country, aes(year, total.votes)) + 
  geom_point() + 
  geom_text(aes(label = country, hjust = 1, vjust = 1))
#----------------------
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