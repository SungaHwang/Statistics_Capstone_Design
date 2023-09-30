movie <- read.csv("movie.csv", header=T)
head(movie)
tail(movie)
str(movie)





install.packages("ggplot2")

library(ggplot2)



title <- movie$영화명
distributor <- movie$배급사
distributor <- as.factor(distributor)
str(distributor)
release_time <- movie$개봉일
release_time
type <- movie$영화유형
type
sales <- movie$전국매출액
sales
sales_seoul <- movie$서울매출액
sales_seoul
audience_seoul <- movie$서울관객수
audience_seoul
screen <- movie$전국스크린수
screen
genre <- movie$장르
genre
class <- movie$등급
class
sort <- movie$영화구분
sort

#종속변수: sales

screen_sales <- ggplot(movie, aes(x=screen, y=sales))
screen_sales+geom_line()

str(movie)

ggplot(movie,aes(x=exp(screen),y=sales)) + geom_point()

ggplot(movie,aes(x=exp(screen),y=sales)) + geom_point() + geom_smooth(method = "lm")


plot(log(sales)~ log(screen), data=movie, pch=19, col='blueviolet', xlab='전국스크린수', ylab='전국매출액', main='전국매출액과 스크린수의 관계')


plot(log(sales)~ log(screen), data=movie, pch=19, col='blueviolet', xlab='전국스크린수', ylab='전국매출액', main='전국매출액과 스크린수의 관계')

#종속변수: sales_seoul

#distributor
distributor_sales <- ggplot(movie,aes(x=distributor, y=sales_seoul))
distributor_sales+geom_point()

# distributor 별 sales_seoul 평균
data_d <- data.frame(distributor, sales_seoul)
data_d
data2_d <- aggregate(sales_seoul~distributor, data_d, mean)
data2_d

str(data2_d)

ggplot(data2_d, aes(x=distributor, y=sales_seoul, fill=rownames(data2_d)))+ geom_bar(stat='identity')

#release_time
release_time_sales <- ggplot(movie2,aes(x=release_time, y=sales_seoul))
release_time_sales+geom_point()

ggplot(data=movie2,mapping=aes(x=release_time,y=sales_seoul, group=1))+
  geom_line(col="red")

#type
type_sales <- ggplot(movie2,aes(x=type, y=sales_seoul))
type_sales+geom_point()


# type 별 sales_seoul 평균
data_t <- data.frame(type, sales_seoul)
data_t
data2_t <- aggregate(sales_seoul~type, data_t, mean)
data2_t

str(data2_t)

ggplot(data2_t, aes(x=type, y=sales_seoul, fill=rownames(data2_t)))+ geom_bar(stat='identity')

# audience_seoul
audience_seoul_sales <- ggplot(movie2,aes(x=audience_seoul, y=sales_seoul))
audience_seoul_sales+geom_point()+geom_smooth(method="lm",se=F)


new_audience_seoul<- subset(movie2,audience_seoul<=500000)
new_audience_seoul
audience_seoul_sales_new <- ggplot(new_audience_seoul,aes(x=서울관객수, y=서울매출액))
audience_seoul_sales_new+geom_point()+geom_smooth(method="lm",se=F)


model_audience_seoul <- lm(sales_seoul~audience_seoul,data=movie2)
model_audience_seoul


#genre
genre_sales <- ggplot(movie2,aes(x=genre, y=sales_seoul))
genre_sales+geom_point()


# genre별 sales_seoul 평균
data_g <- data.frame(genre, sales_seoul)
data_g
data2_g <- aggregate(sales_seoul~genre, data_g, mean)
data2_g

str(data2_g)

ggplot(data2_g, aes(x=genre, y=sales_seoul, fill=rownames(data2_g)))+ geom_bar(stat='identity')

#class
class_sales <- ggplot(movie2,aes(x=class, y=sales_seoul))
class_sales+geom_point()


# class 별 sales_seoul 평균
data_c <- data.frame(class, sales_seoul)
data_c
data2_c <- aggregate(sales_seoul~class, data_c, mean)
data2_c

str(data2_c)

ggplot(data2_c, aes(x=class, y=sales_seoul, fill=rownames(data2_c)))+ geom_bar(stat='identity')

#sort
sort_sales <- ggplot(movie2,aes(x=sort, y=sales_seoul))
sort_sales+geom_point()


# sort 별 sales_seoul 평균
data_s <- data.frame(sort, sales_seoul)
data_s
data2_s <- aggregate(sales_seoul~sort, data_s, mean)
data2_s

str(data2_s)

ggplot(data2_s, aes(x=sort, y=sales_seoul, fill=rownames(data2_s)))+ geom_bar(stat='identity')

#이상치 제거
install.packages("outlers")
library(outliers)
normal <- movie[outlier(movie$score,logical=TRUE)==FALSE,]

# 장르별
as.factor(movie$'장르')
str(movie)

ggplot(movie,aes(x='배급사',y='전국매출액')) + geom_point() + geom_smooth(method = "lm") + facet_grid(. ~ '장르')


ggplot(movie,aes(x='배급사',y='전국매출액')) + geom_point() + geom_smooth(method = "lm")


plot('전국매출액'~ 배급사, data=movie, pch=19, col='blueviolet', xlab='전국스크린수', ylab='전국매출액', main='전국매출액과 스크린수의 관계')

str(movie)

result1 <- lm(sales~distributor)
summary(result1)
plot(sales~distributor)

ggplot(movie, aes(x=배급사,y=전국매출액)) + geom_point() + geom_smooth(method = "lm") + facet_grid(. ~ 장르)

ggplot(movie,aes(x=전국스크린수,y=전국매출액)) + geom_point() + geom_smooth(method = "lm") + facet_grid(. ~ 장르)

ggplot(movie,aes(x=전국관객수,y=전국매출액)) + geom_point() + geom_smooth(method = "lm") + facet_grid(. ~ 장르)

# 등급별
ggplot(movie,aes(x=전국관객수,y=전국매출액)) + geom_point() + geom_smooth(method = "lm") + facet_grid(. ~ 등급)


ggplot(movie,aes(x=전국관객수,y=전국매출액)) + geom_point() + geom_smooth(method = "lm") + facet_grid(. ~ 등급)

ggplot(movie,aes(x=전국스크린수,y=전국매출액)) + geom_point() + geom_smooth(method = "lm") + facet_grid(. ~ 등급)

ggplot(movie,aes(x=배급사,y=전국매출액)) + geom_point() + geom_smooth(method = "lm") + facet_grid(. ~ 등급)

#국적
ggplot(movie,aes(x=전국관객수,y=전국매출액)) + geom_point() + geom_smooth(method = "lm") + facet_grid(. ~ 국적)

ggplot(movie,aes(x=전국스크린수,y=전국매출액)) + geom_point() + geom_smooth(method = "lm") + facet_grid(. ~ 국적)

ggplot(movie,aes(x=배급사,y=전국매출액)) + geom_point() + geom_smooth(method = "lm") + facet_grid(. ~ 국적)

as.numeric(distributor)
str(movie)
