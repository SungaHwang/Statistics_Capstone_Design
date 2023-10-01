data <- read_excel('movie_n2.xlsx')
attach(data); str(data)

table(data$genre2)

#drama - ok

drama <- subset(data, genre2 == 'drama' & num_screen <= 600)
dramalm <- lm(sales ~ distribution + view + num_screen + country, 
              data = drama)
summary(dramalm)

#romance - ok

romance <- subset(data, genre2 == 'romance')
romancelm <- lm(sales ~ distribution + view + num_screen + country + grade, 
                data = romance)
summary(romancelm)

#animation - retry

animation <- subset(data, genre2 == 'animation' & num_screen <= 600
                    & grade == 0)
animationlm <- lm(sales ~ distribution + view + num_screen + country, 
                data = animation)
summary(animationlm)

#action - ok

action <- subset(data, genre2 == 'action')
actionlm <- lm(sales ~view + num_screen + country, 
                data = action)
summary(actionlm)

#comedy - not ok

comedy <- subset(data, genre2 == 'comedy' & grade == 0)
comedylm <- lm(sales ~ distribution + view + num_screen + country, 
                data = comedy)
summary(comedylm)

#criminal - not ok (too small data)

criminal <- subset(data, genre2 == 'criminal', num_screen <= 500)
criminallm <- lm(sales ~ distribution + view + num_screen + country + grade, 
                data = criminal)
summary(criminallm)

#others - ok

others <- subset(data, genre2 == 'others' & grade == 0)
otherslm <- lm(sales ~ distribution + view + num_screen + country, 
                data = others)
summary(otherslm)


#-------(cut line)-------

plot(sales ~ num_screen, data = drama)
num_screen2 <- drama$num_screen^2
plot(sales ~ num_screen2, data = drama)

drama$country

plot(sales ~ num_screen, data = animation)
table(animation$grade)

table(action$distribution)

table(comedy$distribution)
table(comedy$grade)
table(comedy$country)

table(criminal$grade)
table(criminal$country)
plot(sales ~ num_screen, data = criminal)

table(others$grade)
