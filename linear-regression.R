# Удаление вех переменных

rm(list = ls())

# 1. Установка и подключение необходимых пакетов

install.packages("foreign")
install.packages("ggplot2")
install.packages("labeling")
install.packages("digest")

library("foreign")
library("dplyr")
library("ggplot2")
library("labeling")
library("digest")

my_data <- read.csv("C:/Users/79199/Desktop/New folder/haddad.csv")
data1 <- select(my_data, x1, y1)
data2 <- select(my_data, x2, y2)
data3 <- select(my_data, x3, y3)
data4 <- select(my_data, x4, y4)
x1    <- data1$x1
y1    <- data1$y1
x2    <- data2$x2
y2    <- data2$y2
x3    <- data3$x3
y3    <- data3$y3
x4    <- data4$x4
y4    <- data4$y4

#-----------------------data1-------------------------

#Построение диаграммы рассеяния с помощью функции ggplot
g1 <- ggplot(data1, aes(y = y1, x = x1)) + geom_point()
g1

mdl1 <- lm(data = data1, y1 ~ x1)
mdl1
summary(mdl1)
g1 + stat_smooth(method = "lm")
cor(x1, y1, method = c("pearson"))
cor.test(x1, y1, method=c("pearson"))
#-----------------------data2-------------------------
#Построение диаграммы рассеяния с помощью функции ggplot
g2 <- ggplot(data2, aes(y = y2, x = x2)) + geom_point()
g2

mdl2 <- lm(data = data2, y2 ~ x2)
mdl2
summary(mdl2)
g2 + stat_smooth(method = "lm")
cor(x2, y2, method = c("pearson"))
cor.test(x2, y2, method=c("pearson"))
#-----------------------data3-------------------------
#Построение диаграммы рассеяния с помощью функции ggplot
g3 <- ggplot(data3, aes(y = y3, x = x3)) + geom_point()
g3

mdl3 <- lm(data = data3, y3 ~ x3)
mdl3
summary(mdl3)
g3 + stat_smooth(method = "lm")
cor(x3, y3, method = c("pearson"))
cor.test(x3, y3, method=c("pearson"))
#-----------------------data4-------------------------
#Построение диаграммы рассеяния с помощью функции ggplot
g4 <- ggplot(data4, aes(y = y4, x = x4)) + geom_point()
g4

mdl4 <- lm(data = data4, y4 ~ x4)
mdl4
summary(mdl4)
g4 + stat_smooth(method = "lm")
cor(x4, y4, method = c("pearson"))
cor.test(x4, y4, method=c("pearson"))


