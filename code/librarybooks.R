
setwd("C:/dev/code/challenge/code")
library(dplyr)
library(caret)


# import the files
books = read.csv("../data/books_out.csv", stringsAsFactors = FALSE)
borrowers = read.csv("../data/borrowers_out.csv", stringsAsFactors = FALSE)
rentals =  read.csv("../data/rentals_out.csv",stringsAsFactors = FALSE)


# change some colnames so that dfs can be merged. 
colnames(books)[1] = "bookid"
colnames(rentals)[c(1,2,3)] = c("rentalid","bookid","borrowerid")
colnames(borrowers)[2] = "borrowerid"


# merge all the df's so that all the potential variables are present in one df
mergedf = merge(rentals,books)
mergedf = merge(mergedf,borrowers)


# turn date columns into dates- might as well do them all
mergedf <- mergedf %>% mutate_at(vars(out,returned,publication_date,DoB), as.Date,format = "%Y-%m-%d")


# see how long book was out for, add a loan length col
mergedf = mutate(mergedf, loanlength = returned - out)


# now add a late column
mergedf = mutate(mergedf, late = (ifelse(loanlength > rental_period, "L","NL")))


# delete some cols which arent required - see discussion document
mergedfcut = mergedf[,-c(1,2,3,4,5,6,11,13,16,17)]


# but I see subject is a mix of upper and loewr case, which will create extra factors
mergedfcut = mergedfcut %>% mutate_at(vars(subject), toupper)


# subject has repeats, e.g,CHILDRENS, CHILDRENS NOVELS, CHILDRENS BOOKS
# sort out the rpts
mergedfcut$subject = substr(mergedfcut$subject,0,3)



#### model starts here #####
myControl <- trainControl(
  method = "cv",
  number = 10,
  summaryFunction = twoClassSummary,
  classProbs = TRUE, 
  verboseIter = TRUE
)
model <- train(late ~ ., mergedfcut, method = "glm",
               trControl = myControl)

#####end of model ############

# print model results
model

