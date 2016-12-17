require(colorspace)
colors_A <- sequential_hcl(2)
require(Amelia)
missmap(data_train, col = colors_A, legend=FALSE)
require(ggplot2)
require(gridExtra)
data_train %<>% transform(., Survived = as.factor(Survived),
                          Pclass = as.factor(Pclass), 
                          Sex = as.factor(Sex),
                          Embarked = as.factor(Embarked),
                          SibSp = as.numeric(SibSp))
colours <- rainbow_hcl(4, start = 30, end = 300)

ggbar <- ggplot(data_train) + geom_bar(stat = "bin", width=.6, fill= colours[3], colour="black") +
  guides(fill=FALSE) + ylab(NULL)
g1 <- ggbar + aes(x = factor(Survived, labels = c("Погиб", "Выжил"))) + 
  ggtitle("Распределение погибших\n и спасшихся пассажиров") + xlab(NULL)
g2 <- ggbar + aes(x = factor(Pclass, labels = c("Первый", "Второй", "Третий"))) + 
  ggtitle("Распределение пассажиров\n по классам обслуживания") + xlab(NULL)
g3 <- ggbar + aes(x = factor(Sex, labels = c("Женщина", "Мужчина"))) + 
  ggtitle("Распределение пассажиров между полами") + xlab(NULL)
g4 <- ggbar + aes(x = as.factor(SibSp)) + 
  ggtitle("Распределение пассажиров по сумме\n 'супруг + братья и сёстры на борту корабля'") + 
  xlab(NULL)
g5 <- ggbar + aes(x = as.factor(Parch)) + 
  ggtitle("Распределение пассажиров по сумме\n 'родители + дети на борту'") + xlab(NULL)
g6 <- ggbar + aes(x = factor(Embarked, labels = c("Cherbourg", "Queenstown", "Southampton"))) +
  ggtitle("Распределение пассажиров\n по пункту отправления") + 
  xlab(NULL)

gghist <- ggplot(data_train) + geom_histogram(fill= colours[4]) + guides(fill=FALSE) + ylab(NULL)
g7 <- gghist + aes(x = Age) + xlab(NULL) + ggtitle("Распределение пассажиров по возрастам")
g8 <- gghist + aes(x = Fare) + xlab(NULL) + ggtitle("Распределение пассажиров\n по стоимости билетов") 
grid.arrange(g1, g2, g3, g4, g5, g6, g7, g8, ncol = 2, nrow=4)
ggbar <- ggplot(data_train) + geom_bar(stat = "bin", width=.6)
ggbar + aes(x = factor(Pclass, labels = c("Первый", "Второй", "Третий")),
            fill = factor(Survived, labels = c("Погиб", "Выжил"))) + 
  scale_fill_manual (values=colours[]) +
  guides(fill=guide_legend(title=NULL)) + 
  ylab(NULL) + xlab("Класс каюты")
ggbar + aes(x = factor(Sex, labels = c("Женщина", "Мужчина")),
            fill = factor(Survived, labels = c("Погиб", "Выжил"))) +
  scale_fill_manual (values=colours[]) +
  guides(fill=guide_legend(title=NULL)) + 
  ylab(NULL) + xlab("Пол пассажира")
ggbar + aes(x = factor(Embarked, labels = c("Cherbourg", "Queenstown", "Southampton")),
            fill = factor(Survived, labels = c("Погиб", "Выжил"))) +
  scale_fill_manual (values=colours[]) +
  guides(fill=guide_legend(title=NULL)) + 
  ylab(NULL) + xlab("Порт отправления")
ggbar + aes(x = factor(Embarked, labels = c("Cherbourg", "Queenstown", "Southampton")),
            fill = factor(Pclass, labels = c("Первый", "Второй", "Третий"))) +
  scale_fill_manual (values=colours[]) +
  guides(fill=guide_legend(title="Класс каюты")) + 
  ylab(NULL) + xlab("Порт отправления")
ggplot(data_train, aes(x = factor(Survived, labels = c("Погиб", "Выжил")), 
                       y = Age, fill = factor(Survived, labels = c("Погиб", "Выжил")))) +
  geom_boxplot() + scale_fill_manual (values=colours[]) +
  guides(fill=guide_legend(title=NULL)) + 
  ylab(NULL) + xlab(NULL)
ggplot(data_train, aes(x = factor(Pclass, labels = c("Первый", "Второй", "Третий")), 
                       y = Age, fill = factor(Pclass))) + 
  geom_boxplot() + scale_fill_manual (values=colours) + 
  ylab("Возраст") + xlab("Класс каюты") + guides(fill=FALSE)
ggplot(data_train, aes(x = factor(Title, 
                                  c("Capt","Col","Major","Sir","Lady","Rev",
                                    "Dr","Don","Jonkheer","Countess","Mrs", 
                                    "Ms","Mr","Mme","Mlle","Miss","Master")), 
                       y = Age)) + geom_boxplot(fill= colours[3]) + guides(fill=FALSE) +
  guides(fill=guide_legend(title=NULL)) + ylab("Возраст") + xlab(NULL)
Surv_rate_family <- data_train %>% group_by(Family = SibSp + Parch) %>% 
  summarise(Rate = mean(as.numeric(as.character(Survived))))
ggplot(Surv_rate_family, aes(x = as.factor(Family), y = Rate)) + 
  geom_bar(stat = "identity", width=.6, fill= colours[3]) +
  xlab("Кол-во родственников на борту корабля") + ylab("Процент выживаемости")
data_train$isFamily <- as.factor(as.numeric(data_train$Family > 0))
ggplot( data_train, aes(x=factor(isFamily, labels =c("Нет", "Есть")),y=as.numeric(as.character(Survived))) ) +
  stat_summary( fun.y = mean, ymin=0, ymax=1, geom="bar", size=4, fill= colours[2]) + 
  ylab("Процент выживаемости") + xlab("Наличие родственников на борту корабля")