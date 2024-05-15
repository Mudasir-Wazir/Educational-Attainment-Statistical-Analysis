library(readxl)
d <- read_excel("C:/Users/mudas/OneDrive/Desktop/BAIS/SDM/Final Project/College Distance Data 1.xlsx")
View(d)
str(d)

d$Column1 = NULL

summary(d)


table(d$gender)
barplot(table(d$ethnicity))
table(d$fcollege)
table(d$mcollege)
table(d$home)
table(d$urban)
table(d$income)
table(d$region)
barplot(table(d$education),xlab = "Number of years of Education ", ylab = "Frequency",main = "Bar Plot of Education Attainment")

hist(d$score)

hist(d$unemp)
hist(log(d$unemp))

hist(log(d$distance)) #use log in GLM 

hist(d$education,xlab = "Number of years of Education ", ylab = "Frequency", main = "Histogram of Education Attainment")

hist(d$tuition)
hist(d$wage)


library(lattice)
histogram(~education|ethnicity*gender, xlab ="Number of years of Education ",main = "Histogram of Education Attainment across ethnicities and gender)", data=d)
histogram(~education|income, xlab ="Number of years of Education ",main = "Histogram of Education Attainment across Income levels",data=d)
histogram(~education|fcollege*mcollege, xlab ="Number of years of Education ", main = "Histogram of Education Attainment by whether Parents graduated college",data=d) 
histogram(~education|home, xlab ="Number of years of Education ",main = "Histogram of Education Attainment by whether family owns a Home", data=d) 



library("PerformanceAnalytics")
chart.Correlation(d[c(8:12,3)]) #


# Convert character columns to factors
for(col in names(d)) {
  if(is.character(d[[col]])) {
    d[[col]] <- factor(d[[col]])
  }
}


#Poisson Model

poisson = glm(education ~ . ,family = poisson(link = log) ,data = d)
summary(poisson)

library(AER)
dispersiontest(poisson) #suggests underdispersion variance < mean
mean(d$education)
var(d$education)

library(DescTools)
VIF(poisson) #No multicollinearity



qpoisson <- glm(education ~ ., family=quasipoisson (link=log), data=d) #Quasi-Poisson model may counter under dispersion
summary(qpoisson)
 


library(stargazer)
stargazer(poisson, qpoisson, type="text", title="Comparison of Models", single.row=TRUE)

exp(cbind(coef(poisson), coef(qpoisson)))

dwtest(qpoisson) #No Auto-Correlation 




#Logit Models

d$education <- factor(d$education,ordered=TRUE) #converting to ordered factor type


set.seed(79077374)
samplesize = floor(0.75*nrow(d))   # Create train and test data sets
index <- sample(seq_len(nrow(d)), size=samplesize)
train <- d[index,]
test  <- d[-index,]


library(MASS)                                     
ol <- polr(education ~ . , data=train, Hess=TRUE)# Ordered logit model
summary(ol)
coeftest(ol)# t-tests of coefficients



predicted <- predict(ol, test)
table(test$education, predicted)    # Confusion matrix
mean(as.character(test$education) != as.character(predicted))  # Classification error


ol2 = polr(education ~ .+ fcollege*mcollege, data= train, Hess=TRUE)
summary(ol2)
coeftest(ol2)
exp(coeftest(ol2$coefficients))

VIF(ol2)


predicted <- predict(ol2, test)
table(test$education, predicted)   # Confusion matrix
mean(as.character(test$education) != as.character(predicted))  # Classification error is less than ol model

stargazer(poisson, qpoisson,ol,ol2, type="text", title="Comparison of Models",  out= "final models.html" , single.row=TRUE)
AIC(poisson, qpoisson,ol,ol2)

library(lmtest) 
lrtest(ol, ol2) 


library("effects")                                

plot(Effect(focal.predictors="income", ol2))
plot(Effect(focal.predictors="ethnicity", ol2))
plot(Effect(focal.predictors="fcollege", ol2))
plot(Effect(focal.predictors="mcollege", ol2))

