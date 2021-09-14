library(cir)
library(tibble)
library(readr)
library(ggplot2)
Data <- read_csv("D:/Проекты/Язык R/Заметки на habr/З7 - Изотоническая регрессия/Dataforfigure8.csv", 
                           col_names = FALSE)
nam<-t(Data[,1])
Base<-as.data.frame(t(Data[,2:24]))
colnames(Base)<-nam
ggplot(data=Base, aes(x=Base$log_dose, y=Base$Probability_of_infection_1)) +
  geom_point() + xlab("Логарифм дозы") + ylab("Вероятность инфицирования")
x1<-Base[,2]
names(x1)<-c()
x2<-Base[,1]
names(x2)<-c()
dat<-doseResponse(y=x1,x=x2) # Получение доверительных интервалов для значений у
quick1<-quickIsotone(dat)  # Быстрая регрессия - получение доверительных интервалов для значений у
ggplot(data=quick1, aes(x=x, y=y)) +
  geom_point() + xlab("Логарифм дозы") + ylab("Вероятность инфицирования") + 
  geom_line(data=quick1,aes(x=x, y=lower90conf))+
  geom_line(data=quick1,aes(x=x, y=upper90conf))
slow1<-cirPAVA(dat,full=TRUE) # Построение регрессии по алгоритму CIR
slow1$output # Предсказанные значения
slow1$input # Исходные данные
slow1$shrinkage # Сокращенные данные для построения уравнения
ggplot(data=quick1, aes(x=x, y=y)) +
  geom_point() + xlab("Логарифм дозы") + ylab("Вероятность инфицирования") + 
  geom_line(data=slow1$shrinkage,aes(x=x, y=y), color = "green")

int1=isotInterval(slow1) 
int1_0<-isotInterval(slow1,narrower=FALSE)
int1_0 # Расчет доверительных интервалов по готовой модели
ggplot(data=quick1, aes(x=x, y=y)) +
  geom_point() + xlab("Логарифм дозы") + ylab("Вероятность инфицирования") + 
  geom_line(data=int1_0,aes(x=quick1$x, y=ciLow))+
  geom_line(data=int1_0,aes(x=quick1$x, y=ciHigh))

quickIsotone(dat, outx = c(2.15,7.75)) # Предсказание с доверительными интервалами для значения
