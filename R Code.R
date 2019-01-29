



######## Basic analysis of data ########


##  Number of product people usually buy? ##
x <- read.csv("main_dataset.csv")
hist(x$order_item_id,  main="No. of product customer usually buy", 
     xlab="No. of product in order", ylab = "No. of order",
     labels = TRUE,xlim = c(1,25), ylim = c(1,150000),
     col=c("blue","red"), border = c("red","blue"))


##  How much price product people generally buy? ##
hist(x$price, xlim = c(0,7500), ylim = c(0,150000), labels = TRUE,
     main = "Price", xlab = "price of product", ylab = "frequency",
     col = c("blue","red"))


##  which product cagegory people buy most? ##
library(plyr)
x$product_category_name_english[x$product_category_name_english==""] <- NA
z <- count(x$product_category_name_english)
z <- data.frame(z)
c <- z[order(-z$freq),]
c1 <- head(c, 6)
c2 <- as.vector(c1)
#barplot(c2$freq, name.arg=c2$x,xlab = "product category", 
# ylab = "frequency", col = "red",main = "top selling category", 
# las=2)
barplot(c2$freq,names.arg =c2$x, col = "red", main = "top_selling_category",
        ylim = c(0,10000))


## Show heatmap of Hour, Day(name), NO. of order item  ##
library(ggplot2)
y <- read.csv("Q4.csv")
###ggplot(data = y, aes(x = Day.name. , y = Hour)) 
###+ geom_tile(aes(fill = No_of_order_item))


ggplot(y, aes(x = Day.name. ,y = Hour,fill = No_of_order_item))+ 
  geom_tile(colour = "white") +  
  scale_fill_gradient2(low = "#1B7837", mid = "white", high = "#762A83")
q <- sqldf("select Day_name, Hour, count(No_of_order_item) from y 
           group by Day_name, Hour")


##  Which payment type customers generally prefar? ##
library(plyr)
a <- read.csv("olist_order_payments_dataset.csv")
b <- count(a, "payment_type")
b <- data.frame(b)
b <- as.vector(b)
barplot(b$freq, names.arg = b$payment_type, 
        col = "green", xlab = "type_of_payment_method", 
        ylab = "freq")


## From which state maximum order are placed ##
x <- read.csv("main_dataset.csv")
mytable <- table(x$customer_state)
mytable <- data.frame(mytable)
mytable1 <- mytable[order(-mytable$Freq),]
mytable1 <- head(mytable1, 5)
mytable2 <- as.vector(mytable1)
barplot(mytable2$Freq, names.arg = mytable2$Var1, ylab = "Freq", col = "blue",
        xlab = "states")


## Analysis of which product  category more buy in which state? ##
library(reshape2)		
x <- read.csv("main_dataset.csv")
x$product_category_name_english[x$product_category_name_english==""] <- NA
y <-data.frame(x$product_category_name_english, x$customer_state)
t <- dcast(y, x.product_category_name_english ~ x.customer_state)
View(t)


## Analysis of average price of order per month and total volume of ##
## order per month                                                  ##
x <- read.csv("main_dataset.csv")
library(sqldf)
par(mfrow = c(1,2))
q1 <- sqldf("select Month, avg(price) from x group by Month")
barplot(q1$`avg(price)`, names.arg = q1$Month, ylim = c(0,140), las = 2,
        main = "Average price of order per month", col = rainbow(7))
q2 <- sqldf("select Month, count(order_item_id) from x group by Month")
barplot(q2$`count(order_item_id)`, names.arg = q2$Month, ylim = c(0,11000),
        main = "Total order per month", col = heat.colors(12), las = 2)


## Is there any correlation between No order item, ##
## product name lenght, product description lenght?   ##
library(ppcor)
x <- read.csv("main_dataset.csv")
f <- data.frame(x$price, x$product_height_cm, x$product_weight_g, 
                x$product_length_cm, x$product_width_cm)
View(f)
#is.na(f)
newdata <- na.omit(f)
pcor(newdata)


## Can we predict freight_value of product on the bases of give independent variables ##
x <- read.csv("main_dataset.csv")
model <- lm(freight_value ~ price+product_weight_g+product_height_cm+product_width_cm
            +product_length_cm,  data = x)
summary(model)

model1 <- lm(freight_value ~ price+product_weight_g+product_height_cm+product_length_cm,
             data = x)
summary(model1)


## Considering large sample(100) at 5% alpha can we say that average price is less ##
## than 125. 			                                                                 ##
library(BSDA)
x <- read.csv("main_dataset.csv")
y <- data.frame(x$price)
z <- y[sample(nrow(x), 100),]
z.test(z, alternative = "less", mu = 125, sigma.x = sd(z))


## Considering a sample of 20, At 1% alpha can we say that average ##
## freight price is 20.                                            ##
y <- data.frame(x$freight_value)
z <- y[sample(nrow(x), 20),]
t.test(z, alternative = "two.sided", mu = 20 )


## Time series analysis of total monthly sales ##
library(sqldf)
library(forecast)
x <- read.csv("main_dataset.csv")
q1 <- sqldf("Select Year, Month, sum(price) from x group by Year, Month")
q2 <- q1[-c(1,2,3,24,25),]
t <- c(109972.2, 236403.5, 353695.6, 334965.1, 471276.8, 412475.1, 463464.7, 
       533206.1, 567502.9, 613179.9, 927517.8, 701119.3,877797.2,784237.4,907818.5,
       916707.4,899991.3,792678.3,822212.3,798399.9)
myts <- ts(t, start = c(2017, 1), end = c(2018, 8), frequency = 12,
           deltat = 1/12)
color1.mean <- HoltWinters(x = myts, beta = FALSE, gamma = FALSE)
color1.mean$fitted
plot.ts(myts)
lines(color1.mean$fitted[,1], col="green")
color2.mean <- forecast(color1.mean, h=2)
color2.mean


#### analysis for revire_comment_length vs review_score ## 
r <- read.csv("olist_order_reviews_dataset.csv")
review_comment_length <- as.integer(r$review_score)
review_score <- as.integer(r$No..of.word.in.comment)
boxplot(review_score ~ review_comment_length, xlab = "review_score", 
        ylab = "review_comment_length", notch = FALSE, varwidth = TRUE, 
        col = c("blue", "red", "green", "orange", "purple"))


#### sentiment analysis of review comment of MJ state having vote less than equal to 3 ##
library(tm)
library(wordcloud)
library(RColorBrewer)
speech = "C:/Users/Admin/Data_main/MJ review comment.txt"
s_txt = readLines(speech)
s <- Corpus(VectorSource(s_txt))
s_data <- tm_map(s,stripWhitespace)
s_data <- tm_map(s_data, tolower)
s_data <- tm_map(s_data, removeNumbers)
s_data <- tm_map(s_data, removePunctuation)
s_data <- tm_map(s_data, removeWords, stopwords('pt'))
tm_s <- TermDocumentMatrix(s_data)
tmd1 <- as.matrix(tm_s)
v = sort(rowSums(tmd1), decreasing = TRUE)
wordcloud(s_data, scale=c(5, 0.5), max.words = 200,random.order = FALSE,
          rot.per = 0.35, use.r.layout = FALSE,colors = brewer.pal(8, "Dark2"))
