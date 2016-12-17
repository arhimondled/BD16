data_test$Cabin <- recode(data_test$Cabin, "'' = NA")
data_test$Embarked <- recode(data_test$Embarked, "'' = NA")
data_test %<>% transform(.,Pclass = as.factor(Pclass), 
                         Sex = as.factor(Sex),
                         Embarked = as.factor(Embarked),
                         SibSp = as.numeric(SibSp))
data_test$Embarked[is.na(data_test$Embarked)] <- "S"
data_test$Title <-  data_test$Name %>% str_extract(., "\\w+\\.") %>% str_sub(.,1, -2)


data_test %>% group_by(Title) %>% 
  summarise(count = n(), Missing = sum(is.na(Age)), Mean = round(mean(Age, na.rm = T), 2))
impute.mean.test <- function (impute_col, filter_var, var_levels) {
  for (lev in var_levels) { 
    impute_col[(filter_var == lev) & is.na(impute_col)] <-
      mean_title$Mean[mean_title$Title == lev]
    #mean(impute_col[filter_var == lev], na.rm = T)
  }
  return (impute_col)
}
data_test$Age <- impute.mean.test(data_test$Age, data_test$Title, c("Ms", "Master", "Mrs", "Miss", "Mr"))

data_test$Fare[data_test$Fare == 0] <- NA
data_test$Fare <- impute.mean(data_test$Fare, data_test$Pclass, as.numeric(levels(data_test$Pclass)))
data_test$Title <- change.titles(data_test, 
                                 c("Capt", "Col", "Don", "Dr", 
                                   "Jonkheer", "Lady", "Major", 
                                   "Rev", "Sir", "Countess", "Dona"),
                                 "Aristocratic")
data_test$Title <- change.titles(data_test, c("Ms"), 
                                 "Mrs")
data_test$Title <- change.titles(data_test, c("Mlle", "Mme"), "Miss")
data_test$Title <- as.factor(data_test$Title)

data_test$Family <- data_test$SibSp + data_test$Parch
data_test$isFamily <- as.factor(as.numeric(data_test$Family > 0))
data_test$isCabin <- factor(ifelse(is.na(data_test$Cabin),0,1))
data_test %<>% select(PassengerId, Pclass, Sex, Age, Fare, Embarked, Title, Family, isFamily, isCabin)
data_test$Pclass %<>% revalue(., c("1"="First", "2"="Second", "3"="Third"))
data_test$Sex %<>% revalue(., c("female"="Female", "male"="Male"))
data_test$isFamily %<>% revalue(., c("0"="No", "1"="Yes"))
data_test$isCabin %<>% revalue(., c("0"="No", "1"="Yes"))

Survived <- predict(svm.tune, newdata = data_test)
Survived <- revalue(Survived, c("Survived" = 1, "Died" = 0))
predictions <- as.data.frame(Survived)
predictions$PassengerId <- data_test$PassengerId
write.csv(predictions[,c("PassengerId", "Survived")], 
          file="Titanic_predictions.csv", row.names=FALSE, quote=FALSE)