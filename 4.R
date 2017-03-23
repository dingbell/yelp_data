library(RMySQL)
yelp_db = dbConnect(MySQL(), 
                    user='l27ren', password='abcd1234', 
                    dbname='db656_l27ren', 
                    host='marmoset04.shoshin.uwaterloo.ca')
#list of the tables
dbListTables(yelp_db)

# get the review length and how other user peceive this review
SQL <- paste("select LENGTH(text) as review_length, 
             useful, funny, cool
             from review limit 100;")
SQL <- gsub(pattern='\n',replacement="",x=SQL)
reviw_DB<- dbGetQuery(yelp_db, SQL)

#Associate Rule Mining
library(arules)
# devide the review length to several categories
review=reviw_DB
for (i in 1:dim(review)[2]){
  review[,i]=discretize(review[,i],
                        method="interval",
                        categories=2)
}


lhs_string=paste("review_length=", levels(review$review_length)[1], sep="")
for (i in 2:nlevels(review$review_length)){
  lhs_string=c(lhs_string ,paste("review_length=", levels(review$review_length)[i], sep=""))
}

# find association rules with review_length as lhs
rules <- apriori(review,
                 parameter = list(maxlen=2, minlen=2,
                                  supp = 0.01, conf = 0.5, 
                                  target = "rules"),
                 appearance = list(
                                   lhs=lhs_string, 
                                   default="rhs"
                                   ),
                 control = list(verbose=F))
rules.sorted <- sort(rules, by="lift")
inspect(rules.sorted)

#plot rules
library(arulesViz)
plot(rules)
