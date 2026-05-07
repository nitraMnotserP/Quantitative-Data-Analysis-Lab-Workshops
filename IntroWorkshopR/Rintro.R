##Always remember to set or verify your working directory (slides 23 & 24)
setwd("/Users/prestonmartin/Documents/QQDAL/Workshops/IntroWorkshopR")

################################################################################

##Basic operators in R- Run each line of code (Slide 20)
5 + 1
14 - 9
2 > -5 ##this will return a TRUE or FALSE as we are using logical operators
3 > 4

##Assigning a single value to object x (slide 21)
x <- 12
x ##calling the object name will print the contents of the object

##Multiplying object x by 2 and saving product as new object z
x*2
z <- x*2
z

################################################################################	

##Create a vector of five Numeric values (slide 28)
q <- c(93, 27, 42, 75, 81)
q 

##Create a vector of five String values
color <- c('red', 'blue', 'green' , "yellow" , "orange")
color

# Create a matrix with two rows, and three columns (slide 29)
mat1 <- matrix(c(1,2,3,4,5,6), nrow = 2, ncol = 3)
mat1
mat1[1,3] ##row, column
mat1[1,]
mat1[,3]

##Creates three vectors (ID, score, gender) and assigns them to data frame (df) using data.frame() function (slide 30)
id <- c(1,2,3,4,5)
score <- c(88,75,92,81,86)
gender <- c("male", "male", "female", "male", "female")
df <- data.frame(id, score, gender)
df
View(df)

##examine a specific vector within the data frame using the $
df$id ##extractor 
df$score
df$gender


##Create another vector, age, and add it to the data frame
age <- c(9, 11, 10, 8, 12) #create object 'age'

#the following two lines are equivalent (both add vector (age) to data frame)
df$age <- age

df$age <- c(9, 11, 10, 8, 12)

################################################################################		

##Call up mean help documentation, examine arguments for mean() (slide 34)
?mean

##Use the mean() function to calculate the mean of score as a stand alone vector
mean(score)

##You can also calculate means of variables within data frame
mean(df$score)

################################################################################	
##Create vector with missing values (NA), calculate the mean
test_scores <- c(95, 88, 75, 68, 98, NA, 82, 86, 79, NA)
mean(test_scores) ##This will return NA because we need to address the missing values

##Set the argument na.rm to TRUE in order to preclude all missing cases from analysis
mean(test_scores, na.rm = TRUE)


##############################################################################################	
##Call up cor help documentation, examine arguments for cor() (slide 36)
?cor

##Create vector  with missing values (NA), calculate the correlation with above vector
test_study <- c(10, 7, 6.5, NA, 9, 8, NA, 5, 7, 3)
cor(test_scores, test_study) ##this command will give NA because missing values are present but not specified
cor(test_study, test_scores, use = "complete.obs")

##############################################################################################

##Now we will read in our data set from the HSLS (slide 21)
?read.csv
hsls <- read.csv("WorkshopData.csv", header = TRUE)

##Next, lets make sure our data was read in correctly
str(hsls) ##print the structure of the data.frame
head(hsls) ##print first six rows of all columns
nrow(hsls) ##count the number of overall rows
dim(hsls) ##print the dimensions of your data frame (rows, columns)

##to set our data types (slide 26)
hsls$STU_ID <- as.factor(hsls$STU_ID)
hsls$BIPOC <- as.factor(hsls$BIPOC) ##categorical
hsls$FEMALE <- as.factor(hsls$FEMALE) ##categorical
hsls$X1TXMTSCOR <- as.numeric(hsls$X1TXMTSCOR)##numeric
hsls$X1SES <- as.numeric(hsls$X1SES)##numeric
hsls$X1MTHID <- as.numeric(hsls$X1MTHID)##numeric
hsls$X1MTHUTI <- as.numeric(hsls$X1MTHUTI)##numeric
hsls$X1MTHEFF <- as.numeric(hsls$X1MTHEFF)##numeric


##############################################################################################

##calculate descriptive statistics for all variables in the data set
install.packages("psych") ##moving forward you will not use this line; instead you will only use the line 116
library(psych)

##check N/As (slide 35)
?summary
summary(hsls)

##create data frame of just continuous variables
hslsContinuous <- data.frame(hsls$X1TXMTSCOR, hsls$X1SES, hsls$X1MTHID, hsls$X1MTHUTI, hsls$X1MTHEFF)

##compute descriptive statistics (slide 35)
?describe
results <- describe(hslsContinuous)
results

##to examine continuous variables, we plot histograms using the hist() function (slide 35)
hist(hsls$X1TXMTSCOR)
hist(hsls$X1MTHID)
hist(hsls$X1MTHUTI)
hist(hsls$X1MTHEFF)

##to examine categorical data we can use contingency tables (slide 35)
BIPOCtable <- table(hsls$BIPOC) ##1 = BIPOC 0 = White
BIPOCtable

FEMALEtable <- table(hsls$FEMALE) ##1 = Female 0 = Male
FEMALEtable

##2x2 contingency table for group membership (slide 35)
BIPOCxFEMALE <- table(hsls$BIPOC, hsls$FEMALE)
BIPOCxFEMALE

######Male##Female
#White#1822##1787
#BIPOC#1558##1486

##############################################################################################

##using the "psych" package's cor.test function (slide 36)
?cor.test

cor.test(hsls$X1TXMTSCOR, hsls$X1SES, method = "pearson")
cor.test(hsls$X1TXMTSCOR, hsls$X1MTHID)
cor.test(hsls$X1TXMTSCOR, hsls$X1MTHUTI)
cor.test(hsls$X1TXMTSCOR, hsls$X1MTHEFF)

detach(package:psych) ##detach packages when done using them to prevent errors with others
##############################################################################################
#build regression models using the lm() function and store the result in an object (slide 37)

##simple regression
?lm
mod1 <- lm(X1TXMTSCOR ~ FEMALE, data = hsls) ##Math theta score regressed on Female dummy variable
mod1 ##prints only beta coefficients
summary(mod1) #summary() prints all information about the model

##multiple regression| Dependent: Math theta score 
mod2 <- lm(X1TXMTSCOR ~ X1SES + FEMALE + X1MTHID + X1MTHUTI + BIPOC + X1MTHEFF, data=hsls)  
mod2
summary(mod2)



