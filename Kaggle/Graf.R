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
g1 <- ggbar + aes(x = factor(Survived, labels = c("�����", "�����"))) + 
  ggtitle("������������� ��������\n � ��������� ����������") + xlab(NULL)
g2 <- ggbar + aes(x = factor(Pclass, labels = c("������", "������", "������"))) + 
  ggtitle("������������� ����������\n �� ������� ������������") + xlab(NULL)
g3 <- ggbar + aes(x = factor(Sex, labels = c("�������", "�������"))) + 
  ggtitle("������������� ���������� ����� ������") + xlab(NULL)
g4 <- ggbar + aes(x = as.factor(SibSp)) + 
  ggtitle("������������� ���������� �� �����\n '������ + ������ � ����� �� ����� �������'") + 
  xlab(NULL)
g5 <- ggbar + aes(x = as.factor(Parch)) + 
  ggtitle("������������� ���������� �� �����\n '�������� + ���� �� �����'") + xlab(NULL)
g6 <- ggbar + aes(x = factor(Embarked, labels = c("Cherbourg", "Queenstown", "Southampton"))) +
  ggtitle("������������� ����������\n �� ������ �����������") + 
  xlab(NULL)

gghist <- ggplot(data_train) + geom_histogram(fill= colours[4]) + guides(fill=FALSE) + ylab(NULL)
g7 <- gghist + aes(x = Age) + xlab(NULL) + ggtitle("������������� ���������� �� ���������")
g8 <- gghist + aes(x = Fare) + xlab(NULL) + ggtitle("������������� ����������\n �� ��������� �������") 
grid.arrange(g1, g2, g3, g4, g5, g6, g7, g8, ncol = 2, nrow=4)
ggbar <- ggplot(data_train) + geom_bar(stat = "bin", width=.6)
ggbar + aes(x = factor(Pclass, labels = c("������", "������", "������")),
            fill = factor(Survived, labels = c("�����", "�����"))) + 
  scale_fill_manual (values=colours[]) +
  guides(fill=guide_legend(title=NULL)) + 
  ylab(NULL) + xlab("����� �����")
ggbar + aes(x = factor(Sex, labels = c("�������", "�������")),
            fill = factor(Survived, labels = c("�����", "�����"))) +
  scale_fill_manual (values=colours[]) +
  guides(fill=guide_legend(title=NULL)) + 
  ylab(NULL) + xlab("��� ���������")
ggbar + aes(x = factor(Embarked, labels = c("Cherbourg", "Queenstown", "Southampton")),
            fill = factor(Survived, labels = c("�����", "�����"))) +
  scale_fill_manual (values=colours[]) +
  guides(fill=guide_legend(title=NULL)) + 
  ylab(NULL) + xlab("���� �����������")
ggbar + aes(x = factor(Embarked, labels = c("Cherbourg", "Queenstown", "Southampton")),
            fill = factor(Pclass, labels = c("������", "������", "������"))) +
  scale_fill_manual (values=colours[]) +
  guides(fill=guide_legend(title="����� �����")) + 
  ylab(NULL) + xlab("���� �����������")
ggplot(data_train, aes(x = factor(Survived, labels = c("�����", "�����")), 
                       y = Age, fill = factor(Survived, labels = c("�����", "�����")))) +
  geom_boxplot() + scale_fill_manual (values=colours[]) +
  guides(fill=guide_legend(title=NULL)) + 
  ylab(NULL) + xlab(NULL)
ggplot(data_train, aes(x = factor(Pclass, labels = c("������", "������", "������")), 
                       y = Age, fill = factor(Pclass))) + 
  geom_boxplot() + scale_fill_manual (values=colours) + 
  ylab("�������") + xlab("����� �����") + guides(fill=FALSE)
ggplot(data_train, aes(x = factor(Title, 
                                  c("Capt","Col","Major","Sir","Lady","Rev",
                                    "Dr","Don","Jonkheer","Countess","Mrs", 
                                    "Ms","Mr","Mme","Mlle","Miss","Master")), 
                       y = Age)) + geom_boxplot(fill= colours[3]) + guides(fill=FALSE) +
  guides(fill=guide_legend(title=NULL)) + ylab("�������") + xlab(NULL)
Surv_rate_family <- data_train %>% group_by(Family = SibSp + Parch) %>% 
  summarise(Rate = mean(as.numeric(as.character(Survived))))
ggplot(Surv_rate_family, aes(x = as.factor(Family), y = Rate)) + 
  geom_bar(stat = "identity", width=.6, fill= colours[3]) +
  xlab("���-�� ������������� �� ����� �������") + ylab("������� ������������")
data_train$isFamily <- as.factor(as.numeric(data_train$Family > 0))
ggplot( data_train, aes(x=factor(isFamily, labels =c("���", "����")),y=as.numeric(as.character(Survived))) ) +
  stat_summary( fun.y = mean, ymin=0, ymax=1, geom="bar", size=4, fill= colours[2]) + 
  ylab("������� ������������") + xlab("������� ������������� �� ����� �������")