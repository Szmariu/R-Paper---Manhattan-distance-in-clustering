######DATA IMPORT######
# Importing main data table, NaN are ommited (about 10% of all data)
mainTable <- na.omit(read.csv("data/clean_googleplaystore.csv",header=TRUE))

# Import the ratings, NaN are ommited (about 30% of all data)
ratingsTable <- na.omit(read.csv("data/clean_googleplaystore_user_reviews.csv",header=TRUE))

# Some basic summary
str(mainTable)
summary(mainTable)
