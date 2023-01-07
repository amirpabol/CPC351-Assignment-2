#libraries
library(ggplot2)
library(graphics)
library(dplyr)
library(tidyverse)
library(forcats)

# Insert Data
students_performance  <- read.csv("StudentsPerformance.csv")

# Changing datatype for gender, race, education and lunch as factor
students_performance$gender <- as.factor(students_performance$gender)
students_performance$race.ethnicity <- as.factor(students_performance$race.ethnicity)
students_performance$parental.level.of.education <- as.factor(students_performance$parental.level.of.education)
students_performance$lunch <- as.factor(students_performance$lunch)
students_performance$test.preparation.course <- as.factor(students_performance$test.preparation.course)

# To check any missing value
sum(is.na(students_performance$gender))
sum(is.na(students_performance$race.ethnicity))
sum(is.na(students_performance$parental.level.of.education))
sum(is.na(students_performance$lunch))
sum(is.na(students_performance$test.preparation.course))
sum(is.na(students_performance$math.score))
sum(is.na(students_performance$reading.score))
sum(is.na(students_performance$Writing.score))

# To remove the lunch variable as it is not important
students_performance$lunch <- NULL

# To rename the columns
colnames(students_performance) <- c("STU_Gender", "STU_Ethnic", "PAR_Education", "PREP_CourseStatus", "Maths", "Reading", "Writing")

# QUESTION 2 by Amir Azizi
# Create a pie chart
ethnic_counts <- table(students_performance$STU_Ethnic)
pie(ethnic_counts, main="Distribution of Students According to Ethnicity")

# Create a bar chart
ggplot(students_performance, aes(x=STU_Ethnic, y=STU_Gender, fill=STU_Gender)) +
  geom_bar(stat="identity")+
  theme(axis.text.y = element_blank())+
  xlab("Ethnicity") +
  ylab("Gender")+
  ggtitle("Distribution of gender in each ethnicity")



ggplot(students_performance, aes(x=PREP_CourseStatus, y=PAR_Education, fill=PAR_Education)) +
  geom_bar(stat="identity")+
  theme(axis.text.y = element_blank())+
  xlab("Preparation Course Status") +
  ylab("Parental Level of Education")+
  ggtitle("Relationship between Preparation Course Status 
          and Parental Level of Education")

#Question 3 by Nicolas Chuang

#plotting distribution of marks
plot(density(students_performance$Maths), main="Distribution of marks", col="red", xlab="Marks")
lines(density(students_performance$Reading), col="blue")
lines(density(students_performance$Writing), col="black")
legend(x="topleft", legend=c("Maths", "Reading","Writing"), 
       fill = c("red","blue","black"))

math_marks <- students_performance[order(students_performance$Maths),]
#plot box and whisker plot of math marks of each group
boxplot(Maths~STU_Ethnic,
        data=math_marks,
        main="Different boxplots of Maths marks for each ethnicity",
        xlab="Ethnicity",
        ylab="Marks",
        col="orange",
        border="brown"
)

#Referred from https://www.datamentor.io/r-programming/box-plot/#:~:text=In%20R%2C%20boxplot%20(and%20whisker,numeric%20vectors%20as%20its%20components.

read_marks <- students_performance[order(students_performance$Reading),]
#plot box and whisker plot of reading marks of each group
boxplot(Reading~STU_Ethnic,
        data=read_marks,
        main="Different boxplots of Reading marks for each ethnicity",
        xlab="Ethnicity",
        ylab="Marks",
        col="orange",
        border="brown"
)

write_marks <- students_performance[order(students_performance$Writing),]
#plot box and whisker plot of Writing marks of each group
boxplot(Writing~STU_Ethnic,
        data=write_marks,
        main="Different boxplots of Writing marks for each ethnicity",
        xlab="Ethnicity",
        ylab="Marks",
        col="orange",
        border="brown"
)

#Q4 by Ikmal Hakim
#Does parental level of education affect students’ performance in general context? Explain your
#answer with appropriate visuals.

# create a vector of education levels in the desired order
education_order <- c("high school", "some high school", "some college", "associate's degree", "bachelor's degree", "master's degree")

# convert the PAR_Education variable to a factor with the levels in the desired order
students_performance$PAR_Education <- factor(students_performance$PAR_Education, levels = education_order)

# create a bar chart showing the mean Maths scores by parental education level
ggplot(data = students_performance, aes(x = PAR_Education, y = Maths)) +
  geom_col() +
  labs(title = "Mean Math Scores by Parental Education Level", x = "Parental Education Level", y = "Mean Math Score")

# create a bar chart showing the mean Reading scores by parental education level
ggplot(data = students_performance, aes(x = PAR_Education, y = Reading)) +
  geom_col() +
  labs(title = "Mean Writing Scores by Parental Education Level", x = "Parental Education Level", y = "Mean Writing Score")

# create a bar chart showing the mean Writing scores by parental education level
ggplot(data = students_performance, aes(x = PAR_Education, y = Writing)) +
  geom_col() +
  labs(title = "Mean Writing Scores by Parental Education Level", x = "Parental Education Level", y = "Mean Writing Score")



#Q5 by Ikmal Hakim
#Does parental level of education affect students’ performance in specific context (based on
#ethnicity)? Explain your answer with appropriate visuals.

# create a vector of colors to use for each ethnicity
colors <- c("red", "blue", "green", "orange", "purple")

# create a barplot of the math scores by ethnicity and parental education level
barplot(tapply(students_performance$Maths, list(students_performance$STU_Ethnic, 
                                                students_performance$PAR_Education), mean),
        xlab = "Ethnicity", ylab = "Math Score",
        main = "Bar Chart of Math Score by Parental Level of Education and Ethnicity", 
        beside = TRUE,
        col = colors)

# add a legend to the barplot
legend("bottomright", legend = levels(students_performance$STU_Ethnic), fill = colors)

# QUESTION 6 by Amir Azizi

students_performance$total_score <- students_performance$Maths + students_performance$Reading + students_performance$Writing

# Calculate the mean total scores for each preparation course status
MEANSCORES <- students_performance %>%
  group_by(PREP_CourseStatus) %>%
  summarize(mean_score=mean(total_score))

# Create a bar plot
ggplot(MEANSCORES, aes(x=PREP_CourseStatus, y=mean_score, fill=PREP_CourseStatus)) +
  geom_col() +
  ggtitle("Effect of Preparation Course Status on Performance")+
  xlab("Preparation Course Status") +
  ylab("Perfomance")


# Calculate the mean performance scores for each preparation course status
mean_scores <- students_performance %>%
  group_by(PREP_CourseStatus) %>%
  summarize(MathMean=mean(Maths),
            ReadingMean=mean(Reading),
            WritingMean=mean(Writing))

# Create a bar plot
ggplot(mean_scores, aes(x=PREP_CourseStatus, y=MathMean, fill=PREP_CourseStatus)) +
  geom_col() +
  ggtitle("Effect of Preparation Course Status on Performance(Maths)")+
  xlab("Preparation of Course Status") +
  ylab("Math Score")

ggplot(mean_scores, aes(x=PREP_CourseStatus, y=ReadingMean, fill=PREP_CourseStatus)) +
  geom_col() +
  ggtitle("Effect of Preparation Course Status on Performance(Reading)")+
  xlab("Preparation of Course Status") +
  ylab("Reading Scores")

ggplot(mean_scores, aes(x=PREP_CourseStatus, y=WritingMean, fill=PREP_CourseStatus)) +
  geom_col() +
  ggtitle("Effect of Preparation Course Status on Performance(Writing)")+
  xlab("Preparation of Course Status") +
  ylab("Writing Scores")


#QUESTION 7 by Amir Azizi

MEANSCORES <- students_performance %>%
  group_by(STU_Gender) %>%
  summarize(mean_score=mean(total_score))

ggplot(MEANSCORES, aes(x=STU_Gender, y=mean_score, fill=STU_Gender)) +
  geom_col() +
  ggtitle("Effect of Student Gender on Performance")+
  xlab("Student Gender") +
  ylab("Perfomance")

# Calculate the mean math scores for each student gender
Mean_Scores <- students_performance %>%
  group_by(STU_Gender) %>%
  summarize(MathScore=mean(Maths),
            ReadingScore=mean(Reading),
            WritingScore=mean(Writing))

# Create a bar plot
ggplot(Mean_Scores, aes(x=STU_Gender, y=MathScore, fill=STU_Gender)) +
  geom_col() +
  ggtitle("Relationship between Student Gender and Math Scores")+
  xlab("Student Gender") +
  ylab("Math Scores")

# Create a bar plot
ggplot(Mean_Scores, aes(x=STU_Gender, y=ReadingScore, fill=STU_Gender)) +
  geom_col() +
  ggtitle("Relationship between Student Gender and Reading Scores")+
  xlab("Student Gender") +
  ylab("Reading Scores")

# Create a bar plot
ggplot(Mean_Scores, aes(x=STU_Gender, y=WritingScore, fill=STU_Gender)) +
  geom_col() +
  ggtitle("Relationship between Student Gender and Writing Scores")+
  xlab("Student Gender") +
  ylab("Writing Scores")

ggplot(students_performance, aes(x=PREP_CourseStatus, y=STU_Ethnic, fill=STU_Ethnic)) +
  geom_bar(stat = "identity") +
  theme(axis.text.y = element_blank())+
  ggtitle("Relationship between Student Ethnicity 
          and Course Preparation Status")+
  xlab("Preparation for Course Status")+
  ylab("Student Ethnicity")

