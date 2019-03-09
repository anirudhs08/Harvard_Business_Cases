# Analysis of MBA SALARIES
# NAME: Anirudh Satyam
# EMAIL: anirudhs08@gmail.com
# COLLEGE / COMPANY: PES University, Bangalore

#subset of people who have a job(have a salary)
#excludes entries where salary is 0,998 or 999
got_a_job <- mba[mba$salary!=0 & mba$salary!=998 & mba$salary!=999, ]

#each of the variables are analysed
summary(got_a_job)

#a corrgram is generated for better visual representation of correlation
library(corrgram)
corrgram(got_a_job, order=TRUE, lower.panel=panel.shade,
           +          upper.panel=panel.pie, text.panel=panel.txt)

#Hypothesis 1:
#Males get a higher starting salary compared to females.

aggregate(got_a_job$salary, by=list(got_a_job$sex) , mean)

#T TEST
#H0: Females and males have the same salary. Difference between the mean salary of females and mean salary of males is 0
#H1: Males have a higher salary compared to the females. Difference between the mean salary of females and mean salary of males is not 0.

t.test(got_a_job$salary, got_a_job$sex)

#boxplot on the dependency of salary on sex
boxplot(salary ~ sex, data=got_a_job, 
        +         xlab="Sex (male=1, female=0)", ylab="Salary")

#chi square test
mytable <- xtabs(~salary+sex, data=got_a_job)
chisq.test(mytable)

#Hypothesis 2:
#People who have English as their first language earn a better salary than other people.

aggregate(got_a_job$salary, by=list(got_a_job$frstlang) , mean)
#T TEST
#H0: People who have English as first language as well as people who have other languages as first language have the same salary. Difference between the mean salary of both of them is 0
#H1: People who have English as first language have a higher salary compared to the people who do not have English as their first language. Difference between the mean salary of both of them is not 0.
t.test(got_a_job$salary, got_a_job$frstlang)

#a boxplot on the dependency of salary on first language
boxplot(salary ~ frstlang, data=got_a_job, 
        +         xlab="First language (English=1, Others=0)", ylab="Salary")

#chi square test
mytable <- xtabs(~salary+frstlang, data=got_a_job)
chisq.test(mytable)

#SUMMARY OF THE ABOVE ANALYSIS USING CONTINGENCY TABLES
xtabs(~sex+frstlang, got_a_job)

#Regression model
#MODEL 1
#The salary depends on the age and the work experience.

cor(got_a_job$salary, got_a_job$age)
cor(got_a_job$salary, got_a_job$work_yrs)

fit <- lm(salary~sex+work_yrs,data=got_a_job)
summary(fit)

#original values of salary present in data set
got_a_job$salary

#predicted values according to the regression model
fitted(fit)

#residual values of the model
residuals(fit)

#confidence interval of the model parameters
confint(fit)

#MODEL 2
#Dependency of salary on age only

fit <- lm(salary~age,data=got_a_job)
summary(fit)


#original values of salary present in data set
got_a_job$salary

#predicted values according to the regression model
fitted(fit)

#residual values of the model
residuals(fit)

#confidence interval of the model parameters
confint(fit)

#MODEL 3
#Dependency of salary on work experience only

fit <- lm(salary~work_yrs,data=got_a_job)
summary(fit)

#original values of salary present in data set
got_a_job$salary

#predicted values according to the regression model
fitted(fit)

#residual values of the model
residuals(fit)

#confidence interval of the model parameters
confint(fit)




#this data set eliminates salaries having 998,999
clean <- mba[mba$salary!=998 & mba$salary!=999, ]

#a new column is added to the data frame which has 1 if the person has a job and 0 if that
#person does not have a job
job <- ifelse(clean$salary==0,0,1)
clean <- cbind(clean,job)

#Does gender(sex) play a role in getting/not getting a job?

mytable <- xtabs(~job+sex, data=clean)
addmargins(mytable)
#among people who have job, how many are male and female
prop.table(mytable,1)

#among people of a particular sex, how many have jobs
prop.table(mytable,2)

#Null hypothesis is that the job and the sex are independent
#Alternate hypothesis is that the job and the sex are not independent


chisq.test(mytable)

#Does English as first language play a role in getting/not getting a job?
mytable <- xtabs(~job+frstlang, data=clean)
addmargins(mytable)

#among people who have job, trying to analyse % of english first language people
prop.table(mytable,1)

#among people who have english as first language, trying to find out how many have a job
prop.table(mytable,2)

#Null hypothesis is that job and first language are independent
#alternate hypothesis is that the job and the first language are not independent
chisq.test(mytable)


#Logistic Regression

train <- clean[1:174,]
test <- clean[175:193,]

#for the linear model
model <- glm(job ~age+work_yrs.,family=binomial(link='logit'),data=train)
summary(model)

anova(model, test="Chisq")

#accuracy of the fitted values that are used for testing after being trained
fitted.results <- predict(model,newdata=subset(test,select=c(1,10)),type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != test$job)
print(paste('Accuracy',1-misClasificError))


