#libraries
library(ggplot2)
library(graphics)
library(dplyr)

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
sum(is.na(students_performance$writing.score))

# To remove the lunch variable as it is not important
students_performance$lunch <- NULL

# To rename the columns
colnames(students_performance) <- c("STU_Gender", "STU_Ethnic", "PAR_Education", "PREP_CourseStatus", "Maths", "Reading", "Writing")

#Q4 
#Does parental level of education affect students’ performance in general context? Explain your
#answer with appropriate visuals.
barplot(tapply(students_performance$Maths, students_performance$PAR_Education, mean), xlab = "Parental Level of Education", ylab = "Average Math Score", main = "Bar Chart of Average Math Score by Parental Level of Education")
barplot(tapply(students_performance$Reading, students_performance$PAR_Education, mean), xlab = "Parental Level of Education", ylab = "Average Reading Score", main = "Bar Chart of Average Reading Score by Parental Level of Education")
barplot(tapply(students_performance$Writing, students_performance$PAR_Education, mean), xlab = "Parental Level of Education", ylab = "Average Writing Score", main = "Bar Chart of Average Writing Score by Parental Level of Education")

#Q5
#Does parental level of education affect students’ performance in specific context (based on
#ethnicity)? Explain your answer with appropriate visuals.
barplot(tapply(students_performance$Maths, list(students_performance$STU_Ethnic, 
                                                students_performance$PAR_Education), mean), xlab = "Ethnicity", 
                                                ylab = "Math Score", main = "Bar Chart of Math Score by Parental Level of Education and Ethnicity", 
                                                beside = TRUE)
