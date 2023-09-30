install.packages('ggplot2')
install.packages('lubridate')
install.packages('reshape2')
install.packages('dplyr')
library('ggplot2')
library('lubridate')
library('reshape2')
library('dplyr')

movie <- read.csv("movie_final.csv", header=T)
head(movie)
tail(movie)
str(movie)


movie2 <- subset(movie, 국적=="한국")
movie2

tail(movie2)
str(movie2)

movie3 <- select(movie,'개봉일','서울매출액')
movie3
head(movie3)
str(movie3)


corona <- read.csv("corona.csv", header=T)
head(corona)
str(corona)


x<-cbind(corona, corona_month=month(corona$date))
head(x)
y<-cbind(corona, corona_ymonth=substr(x$date, 1, 7))
tail(y)

corona_n<-(y %>%
        group_by(corona_ymonth) %>%  
        summarise(n=n()))
corona_n

xx<-cbind(movie3, movie_month=month(movie3$'개봉일'))
head(xx)
xx

xx2<- select(xx,'서울매출액','movie_month')
xx2

yy<-cbind(movie3, movie_ymonth=substr(xx$'개봉일', 1, 7))
head(yy)

yy2<- select(yy,'서울매출액','movie_ymonth')
yy2

movie_mean<-(yy %>%
        group_by(movie_ymonth) %>%
        summarise(mean=mean(서울매출액)))
movie_mean


corona_n
movie_mean

corona_n1 <- corona_n %>%
  rename(month=corona_ymonth)
corona_n1

movie_mean1 <- movie_mean %>%
  rename(month=movie_ymonth)
movie_mean1

final <- inner_join(corona_n1, movie_mean1, by=('month'))
head(final)
str(final)


plot.ts(cbind(corona=final$n, movie=final$mean), main="서울 코로나 추이와 영화 매출 비교")


lm <- lm(mean~n, final)
lm
summary(lm)
