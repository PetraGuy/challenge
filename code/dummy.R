
bookid = c(1:10)
borrowerid = c(1,2,3,4,5)
subject = c("a","b","c")
period = c(7,30)
gender = c("m","f")
rentalid = c(1:20)
late = c("y","n")



books = data.frame(bookID = bookid,
                   subject = sample(subject, length(bookid), replace = TRUE),
                   period = sample(period,length(bookid), replace = TRUE))

borrowers = data.frame(borrowerID = borrowerid,
                       gender = sample(gender,length(borrowerid), replace = TRUE))


                       
rentals = data.frame(rentalID = rentalid ,
                     borrowerID = rep(borrowerid, each = 4),
                     bookID = rep(sample(bookid,5),4),
                     late = sample(late,length(rentalid), replace = TRUE))

#merge df

merge1 = merge(rentals, borrowers)
merge2 = merge(merge1,books)

#in merge 2 late can be used as response
#binary response therefore use glm, binomial
#we know some variables are not interesting, like rentalid, so remove

df = subset(merge2, select = -rentalID)

model = glm(late~., family = binomial(link = "logit"), data = df)
                     
v1 = c(10:1)
v2 = c(5:15)
df =  as.data.frame(cbind(v1,v2))

mutate(df, late = ifelse(v2>v1, "l","n"))

model = randomForest(formula = late~.,
                     data = train,
                     mtry = 8,
                     nodesize = hyper_grid$nodesize[i] ,
                     sampsize = hyper_grid$sampsize[i] )


#NB, this is not a great method as for e.g. historical novels and historical are now the
#same. Probaly ned to create some sort of mapping

#note that bookid and borrowerid should be factors, also, since everything except book length
#is a factor, is it faster to do this before running glm
#remove length as is a number

#tempdf = mergedfcut%>%select(-c(length,late))

##convert remaining to factors
#cols = colnames(tempdf)
#tempdf = tempdf %<>% mutate_at(cols, funs(factor(.)))

#stick length and late back on

#length = mergedfcut$length
#late = mergedfcut$late
#finaldf = as.data.frame(cbind(length,tempdf,late))

