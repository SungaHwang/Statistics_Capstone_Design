library('ggplot2')
library('lubridate')
library('reshape2')
library('dplyr')
library('readxl')

corona<-read.csv("corona_data.csv",header=T)
corona<-cbind(corona,month=substr(corona$Ȯ����,3,7))
str(corona)

movie<-read_excel("newmovie.xlsx")
movie<-cbind(movie,month=substr(movie$������,3,7))


attach(movie)

wholebox<-���������...9/������ũ����
seoulbox<-��������...13/������ũ����
movie<-cbind(movie,wholebox,seoulbox)

detach(movie)

corona_num <- corona %>% group_by(month) %>%
  summarise(cor_num = n()) %>%
  as.data.frame()

movie_num <- movie %>% group_by(month) %>%
  summarise(aud_sum = sum(���������),
            sale_sum = sum(��������...13),
            box_sum = sum(seoulbox)) %>%
  as.data.frame()

month_num <- merge(corona_num, movie_num, by='month', all=TRUE)
month_num <- na.omit(month_num)

attach(month_num)
as.factor(month_num$month)

options("scipen"=999)

ggplot(month_num,aes(x=month, group=1))+
  geom_line(aes(y=cor_num),color='Tomato')+
  geom_line(aes(y=aud_sum*1/250),color='Light Sky Blue')+
  scale_y_continuous(sec.axis = sec_axis(~.*250))+
  ggtitle("���� �ڷγ� Ȯ���ڼ��� �������� �������")

ggplot(month_num,aes(x=month, group=1))+
  geom_line(aes(y=cor_num),color='Tomato')+
  geom_line(aes(y=sale_sum*1/20000),color='Dark Green')+
  scale_y_continuous(sec.axis = sec_axis(~.*20000))+
  ggtitle("���� �ڷγ� Ȯ���ڼ��� ������ �������")

ggplot(month_num,aes(x=month, group=1))+
  geom_line(aes(y=cor_num),color='Tomato')+
  geom_line(aes(y=box_sum*1/35),color='Violet Red')+
  scale_y_continuous(sec.axis = sec_axis(~.*35))+
  ggtitle("���� �ڷγ� Ȯ���ڼ��� ���༺���� �������")

audlm <- lm(aud_sum~cor_num,data=month_num)
summary(audlm)

salelm <- lm(sale_sum~cor_num,data=month_num)
summary(salelm)

boxlm <- lm(box_sum~cor_num,data=month_num)
summary(boxlm)

