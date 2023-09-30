movie <- read.csv("movie_final.csv", header=T)
head(movie)
tail(movie)
str(movie)


title <- movie2$영화명
distributor <- movie2$배급사
distributor <- as.factor(distributor)
str(distributor)
release_time <- movie2$개봉일
release_time
type <- movie2$영화유형
type
sales_seoul <- movie2$서울매출액
sales_seoul
audience_seoul <- movie2$서울관객수
audience_seoul
genre <- movie2$장르
genre
class <- movie2$등급
class
sort <- movie2$영화구분
sort

# 전국매출액, 전국스크린수, 전국관객수,배급사, 장르, 등급, 영화구분

regression1 <- lm('전국매출액' ~ '전국스크린수', data=movie)
regression1


regression <- lm(전국매출액~ 전국스크린수+ 전국관객수+ 배급사+ as.factor(장르)+ as.factor(등급) + as.factor(영화구분), data= movie)

regression
summary(regression)


states <- as.data.frame(movie[,c("전국매출액","배급사","개봉일","국적","전국스크린수","전국관객수","영화구분","등급","장르")])
head(states)
fit <- lm(전국매출액 ~ 배급사+개봉일+국적+전국스크린수+전국관객수+영화구분+등급+장르, data = states)
fit
summary(fit)


plot(movie$"전국매출액", movie$"배급사")
