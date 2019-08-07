

library(tidyverse) # Multiple packages
library(ggthemes) # Visualization themes
library(gridExtra) # Grids for visualizations
library(lubridate) # Working with dates


setwd('C:/Users/Symon/Desktop/BoxOffice/BoxOffice')
train_data <- read_csv('train.csv')
test_data <- read_csv('test.csv')

train_data <- train_data %>% mutate(budget = replace(budget, budget == '0', NA))

train_data$homepage[!is.na(train_data$homepage)] <- "YES"
train_data$homepage[is.na(train_data$homepage)] <- "NO"


imdb <- str_replace(train_data$imdb_id, "tt", '')
train_data["imdb_id"] <- imdb
train_data$release_date <- parse_date_time2(train_data$release_date, "mdy",
                            cutoff_2000 = 20)

train_data <- train_data %>% separate(release_date, c("Year", "Month", "Day"))

sum(is.na(train_data$runtime))
which(is.na(train_data$runtime))
train_data <- train_data %>% drop_na(runtime)

train_data$tagline[!is.na(train_data$tagline)] <- "Yes"
train_data$tagline[is.na(train_data$tagline)] <- "NO"


train_data$collection_name <- str_extract(train_data$belongs_to_collection, 
pattern = "(?<=name\\'\\:\\s{1}\\').+(?=\\'\\,\\s{1}\\'poster)")


train_data$Franchise[!is.na(train_data$collection_name)] <- "YES"
train_data$Franchise[is.na(train_data$collection_name)] <- "No"

train_data$prod_country <- str_extract(string = train_data$production_countries, 
                                      pattern = "[:upper:]+")

genres_matching_point <- "Comedy|Horror|Action|Drama|Documentary|Science Fiction|
              Crime|Fantasy|Thriller|Animation|Adventure|Mystery|War|Romance|Music|
              Family|Western|History|TV Movie|Foreign"

train_data$main_genre <- str_extract(train_data$genres, genres_matching_point)





train_data$budget_range[train_data$budget <= 5.10e+06] <- "Low" 
train_data$budget_range[train_data$budget > 5.10e+06 & train_data$budget <= 4.00e+07 ] <- "Med" 
train_data$budget_range[train_data$budget > 4.00e+07] <- "Big" 

train_data$budget_range <- as.factor(train_data$budget_range)
ggplot(train_data, aes(train_data$budget, train_data$revenue, color = train_data$budget_range)) + geom_point() + geom_smooth()


# There are very few Low budget movies, Most of them are medium or high budget movie
# Budget seems very related with revenue. More budget seems to earn more.

train_data$revenue_range[train_data$revenue <= 6.677e+07] <- "Bad" 
train_data$revenue_range[train_data$revenue > 6.677e+07] <- "Good" 

table(train_data$budget_range, train_data$revenue_range)

# Out of every 5 BigBudget 4 will do good
# Only 4% Low buget can do good
# 30% mediumbudget movie doing good
# Money Makes Money

ggplot(train_data, aes(train_data$budget, train_data$popularity, color = train_data$revenue_range)) + geom_point()

#popularity have a correlation with budget but not as much as revenue
#There is a sweetspot where low budget movie seems to have good revenue and good popularity: These movies will be interesting to study

ggplot(train_data, aes(train_data$runtime, train_data$revenue, color = train_data$budget_range)) + geom_col()

# runtime is important, people tend to spend money on movies ranging from 90 min - 145 min
# Most of the bigbudget and medium budget movie try to make movie in this range
# This says something about our attention span, isn't it?

Median_budget <- train_data %>% group_by(train_data$budget_range) %>% summarise(median_revenue=median(revenue))
Median_revenue <- train_data %>% group_by(train_data$budget_range) %>% summarise(median_budget=median(budget))

Median <- merge(Median_revenue, Median_budget, by = "train_data$budget_range")


train_data %>% filter(budget_range == "Big") %>% arrange(desc(revenue)) %>% select(title, revenue) %>% head(10)

train_data %>% filter(budget_range == "Med") %>% arrange(desc(revenue)) %>% select(title, revenue) %>% head(10)

train_data %>% filter(budget_range == "Low") %>% arrange(desc(revenue)) %>% select(title, revenue) %>% head(10)
                                                                                                            
                                                                                                            
# The top 10 movies by earning for low budget criteria seems very interesting: 6 of them horror, 4 of them commedy
# If you get less money in the movie business, either bring good wit or crazy vison to scare people off
# Franchise movies dominate big budget genre
# Seems like medium budget movie can hold more creativity and experiments


train_data %>% filter(budget_range == "Big") %>% arrange(desc(revenue)) %>% select(title, revenue) %>% tail(10)

# I am somewhat familiar with movies: Never heard about any of these movies: seems well justified why they are failure despite being High budget

language_number <- train_data %>% group_by(original_language) %>% summarise(ct = n()) %>% arrange(desc(ct)) %>% head(7)
language_number$original_language <- as.factor(language_number$original_language)

ggplot(language_number, aes(language_number$original_language, language_number$ct)) + geom_col() 

#As expected, there is the monopoly of english language, Othere signifacnt languages are french, russian, hindi, spanish and italian

ggplot(train_data, aes(original_language, color = revenue_range)) + geom_bar() + coord_flip()

# By global standard, other languages other than english are not successful that much except few language like japanese, hindi, french, tr, zh, de
# russian language film seems to suffer a lot; Giant land with small population effect, I guess

ggplot(train_data, aes(revenue, popularity, color = homepage)) + geom_point()

# The graph might be misleading: seems like having homepage have clear effect on being the movie popular and successful
# Actually most of the successful or popular movies are high budget movies: can afford to build or care about having a homepage
# Now I see why Domain or field knowledge matters !

time <- train_data %>% separate(release_date, c("Year", "Month" , "Date"))

top_years <- time %>%  group_by(Year) %>% summarise(ct=n()) %>% arrange(desc(ct)) %>% head(20)

ggplot(top_years, aes(Year, ct)) + geom_col() + coord_flip()

# In the top 20 movieproducing years, 17 of them is in this century
# Good time for movie
ggplot(subset(time, !is.na(time$budget_range)), aes(Month, color = budget_range)) + geom_bar()

# Big Budget movie comes more in two time period: May-June & November-December

ggplot(train_data, aes(fct_infreq(train_data$main_genre))) + geom_bar() + coord_flip()
ggplot(train_data, aes(fct_infreq(train_data$main_genre),  color = revenue_range)) + geom_bar(position = "fill") + coord_flip()


# Drama, comedy, action overwhels other genre
# But Revenue wise Adventure, Science fic  & animation beats everybody

ggplot(train_data, aes(fct_infreq(prod_country), color = budget_range)) + geom_bar() + coord_flip()

# Uncle Sam is in top in every category

pop1 <- train_data %>% na.omit() %>% select(popularity, collection_name) %>% group_by(collection_name) %>% arrange(desc(popularity)) %>% head(20)

ggplot(pop1, aes(fct_inorder (collection_name), popularity)) + geom_col() + coord_flip()

# top 20 popular franchise


ggplot(train_data, aes(Franchise, color = revenue_range)) + geom_bar()

# This is one interesting plot, looks like probability of being a winner is much higher for a franchise movie





