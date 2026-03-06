##set our working directory
setwd("/Users/prestonmartin/Documents/QQDAL/Workshops/DataVizWorkshop")

##Load or install the ggplot2 package
install.packages("ggplot2")
install.packages("tidyverse")
library(ggplot2)
library(tidyverse)

##Load the preset data from ggplot2 package called 'mpg'
data(mpg)

##to see what the columns represent, see:
??ggplot2::mpg

##examine the structure of this data set (11 columns[variables] and 243 rows [observations])
str(mpg) 

##Pass data argument to the ggplot()
ggplot(data = mpg)

#Pass Mapping argument to the ggplot() 
p <- ggplot(mpg, aes(x = cty, y = hwy))
p
##Layers: add geom_point() to tell ggplot what kind of graphical display to use at this layer
p + geom_point() 

##add regression line and a title
p + geom_point() +
  geom_smooth(formula = y ~ x, method = "lm") +
  ggtitle("Miles per Gallon: City and Highway")

##Add X and y Axis labels
p + geom_point() +
  geom_smooth(formula = y ~ x, method = "lm") +
  ggtitle("Miles per Gallon: City and Highway") +
  xlab("City MPG") +
  ylab("Highway MPG") 

##Use faceting on the plot to make it a multivariate plot: Drive type
p + geom_point() +
  geom_smooth(formula = y ~ x, method = "lm") +
  ggtitle("Miles per Gallon: City and Highway") +
  xlab("City MPG") +
  ylab("Highway MPG") +
  facet_grid(rows = vars(drv)) 

##Add another layer of faceting: year and drive type
p + geom_point() +
  geom_smooth(formula = y ~ x, method = "lm") +
  ggtitle("Miles per Gallon: City and Highway") +
  xlab("City MPG") +
  ylab("Highway MPG") +
  facet_grid(year ~ drv) ##add second level of faceting: Year ~ drive type


##Now lets take it back a few steps to simpler plot. Using the cord_flip() we can flip our x and y axis
p + geom_point() +
  geom_smooth(formula = y ~ x, method = "lm") +
  ggtitle("Miles per Gallon: City and Highway") +
  xlab("City MPG") +
  ylab("Highway MPG") 

p + geom_point() +
  geom_smooth(formula = y ~ x, method = "lm") +
  ggtitle("Miles per Gallon: City and Highway") +
  xlab("City MPG") +
  ylab("Highway MPG") +
  coord_flip() ##flip the X to the Y axis, and Y to the X axis


##Lets create another multivariate plot using the same variables as before but with addition of the colour argument
p.2 <- ggplot(mpg, aes(cty, hwy, colour = class)) + ##colour = [variable name] by which you want the dots colored
       geom_point() 
p.2 

##Then we will add customization features to the plot such as changing the legend location, 
##and edit the color and point size of the X axis
p.2 + ggtitle("Miles per Gallon: City and Highway") +
      xlab("City MPG") +
      ylab("Highway MPG") +
      theme_minimal() + ##removed grey background form plot
      theme(
        legend.position = "top", ##move the plot legend to the top
        axis.line = element_line(linewidth = 2.0), ##change the width of the X axis line
        axis.line.x.bottom = element_line(colour = "blue") ##change the color  of the X axis
      )


##Now lets turn to our provided data set; this is  a subset of American students from the 
##2022 Programme for International Student Assessment (PISA) data:

##read in the data file
pisa <- read.csv("Data_Viz_WorkShop.csv")

##check pur data and set our data types
str(pisa)
summary(pisa)
##As you can see, we have 6 variables of type integer, 8 variables of type numeric, 
##and 3 variables of type character.
##However, we want to set specific variables to the appropriate types:

##Factors (categorical): School ID, Student ID, Gender, SES.cat, Immg
##Ordered Factors: Edu.Exp
##Numeric (continuous): SES, Math.Self.Eff, Math.Anxiety, Feel.Safe, Grade, Math.Score, Science.Score, Reading.Score

##Factors (categorical):
pisa$School_ID <- as.factor(pisa$School_ID)
pisa$Student_ID <- as.factor(pisa$Student_ID)
pisa$Grade.Repeated <- as.factor(pisa$Grade.Repeated)
pisa$Gender <- as.factor(pisa$Gender)
pisa$SES.cat <- as.factor(pisa$SES.cat)
pisa$Immg <- as.factor(pisa$Immg)
##Ordered:
pisa$Edu.Exp <- as.ordered(pisa$Edu.Exp)

##Numeric:
pisa$Math.Self.Eff <- as.numeric(pisa$Math.Self.Eff)
pisa$Grade <- as.numeric(pisa$Grade)
pisa$Math.Score <- as.numeric(pisa$Math.Score)
pisa$Reading.Score <- as.numeric(pisa$Reading.Score)
pisa$Science.Score <- as.numeric(pisa$Science.Score)

str(pisa)

##for ease of use, we will use list-wise deletion remove rows (participants) with missing values
pisa.listwise <- pisa %>% ##pass the 'pisa' object to the 'drop_na()' using %>% and store it in the new 'pisa.listwise' object
                        drop_na()

##Histograms

##I prefer to pass the data frame ('pisa.listwise') 
##to the ggplot function using %>% rather than the data argument.

##The following two code blocks do the same thing:
##with data argument:
ggplot(pisa.listwise, aes(x = SES)) + 
  geom_histogram()

##piping data to ggplot function. 
pisa.listwise %>% ##Command + shift + m will give you pipe operator 
  ggplot(aes(x = SES)) + 
  geom_histogram() 

##Moving forward, we will pipe the data so that the code is more readable

##lets add some definition to our plot by mutually setting our bin size and apply color to the histogram 
pisa.listwise %>% 
  ggplot(aes(x = SES)) + 
  geom_histogram(bins = 50, color="black", fill = "lightpink", alpha=0.9) ##color changes outlines and, fill changes fill color and alpha is the opacity


##Next, lets change our X and y Axis Labels to our histogram and lower our opacity
pisa.listwise %>% 
  ggplot(aes(x = SES)) + 
  geom_histogram(bins = 50, color="black", fill = "lightpink", alpha=0.5) +
  ggtitle("Histogram of Continuous SES") + #add title
  xlab("Socioeconomic Status") + ##add label to x axis
  ylab("Frequency of Observations") ##add label to y axis

##Perhaps we want to examine a histogram for several groups within our data set. 
##To do this we just need to add the 'fill' argument in the aes()
##In this case, our continuous variable is math score and the categorical variable is SES group
pisa.listwise %>% 
  ggplot(aes(x=Math.Score, fill=SES.cat)) + ##fill argument here
  geom_histogram(color="black",bins = 50, alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#85D4E3", "#F4B5BD", "#9C969A")) + ##these colors are part of the 'wesanderson' color palette collection
  theme_classic() + ##change our theme to classic (it was theme_grey as default)
  ggtitle("Histogram of Math Achivement") + 
  xlab("Mathematics Score") +
  ylab("Frequency of Observation") 

##Now lets look at the distribution of a categorical variable using a bar chart
##create basic bar chart
pisa.listwise %>% 
      ggplot(aes(x = SES.cat)) + 
      geom_bar() ##initiates bar chart

##now lets add some color to the plot
pisa.listwise %>% 
  ggplot(aes(x = SES.cat)) +
  geom_bar(color="black", fill = "steelblue") ##fill the bars with the color 'steelblue' and give them a 'black' outline

##We can also customize the font of the titles and labels
pisa.listwise %>% 
  ggplot(aes(x = SES.cat)) +
  geom_bar(color="black", fill = "steelblue") +
  labs(title = "Socioeconomic Group Distribution", ##add title
       subtitle = "PISA 2022") + ##add subtitle
  theme(text = element_text(size = 16, family = "Comic Sans MS")) ##edit the fonts


##Box plots

##examine box plot for Science Score across all participants
pisa.listwise %>% 
  ggplot(aes(y = Science.Score)) + 
  geom_boxplot() 

##notice we still have an X axis that is meaningless on this box plot--we need to remove this
pisa.listwise %>% 
  ggplot(aes(y = Science.Score)) + 
  geom_boxplot() +
  scale_x_continuous(breaks = NULL) + ##remove x axis
  theme(axis.title.x = element_blank()) ##remove x axis title

####Now lets customize our box plot and change the color, size, and shape of our outliers in the plot
pisa.listwise %>% 
  ggplot(aes(y = Science.Score)) + 
  scale_x_continuous(breaks = NULL) + 
  theme(axis.title.x = element_blank()) + 
  geom_boxplot(outlier.shape = 2, outlier.colour = "red", outlier.size = 4)  + ##these arguments change the shape, color and size of the outliers 
  labs(title = "Box plot with edited outliers")

##Next we can make the box plot a bivariate box pot by adding an X variable argument in the aes() argument
pisa.listwise %>% 
  ggplot(aes(x = SES.cat, y = Science.Score)) +
  geom_boxplot(width = 0.5) ##width argument changes the width of the box plots


##we can customize this further by changing out data point shapes
pisa.listwise %>% 
  ggplot(aes(x = SES.cat, y = Science.Score)) +
  geom_boxplot(width = 0.5, outlier.shape = 23) ##outlier.shape argument changes the dots


##If you want to plot the mean in your box plot, use the stat_summary function:
pisa.listwise %>% 
  ggplot(aes(x = SES.cat, y = Science.Score)) +
  geom_boxplot(width = 0.5, outlier.shape = 23) + 
  stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "pink") ##we can also add color to it with fill argument


##Pie charts

##In situations like creating a pie chart that takes proportions data, the tidyverse family of packages is extremely helpful
##Important Note: ggplot does not have a built in geom for pie charts; So we use the coord_polar function to transform a bar chart

pisa.listwise %>% 
  group_by(Immg) %>% # sets up the grouping variable
  summarise(n = n()) %>% ##count the rows
  mutate(freq = n / sum(n), ##calculate proportion
         prop = freq*100) %>% ##proportion * 100 for the plot
  ggplot(aes(x = "", y = prop, fill = Immg)) + ##requires X argument so use ""
  geom_bar(stat = "identity", color = 'black') +
  theme_void() ##remove theme from plot

pisa.listwise %>% 
  group_by(Immg) %>% 
  summarise(n = n()) %>% 
  mutate(freq = n / sum(n),
         prop = freq*100) %>% 
  ggplot(aes(x = "", y = prop, fill = Immg)) + 
  geom_bar(stat = "identity", color = 'black') +
  coord_polar("y") + ##this transforms bar chart to a pie chart
  theme_void() 



##Bivariate plots: Let's stat with the bivariate bar charts 

##Specifically, bar charts where mean math score is examined across Educational expectation categories 

##Basic bar charts where y axis is a mean score
pisa.listwise %>% 
  group_by(Edu.Exp) %>% # sets up the grouping variable
  summarize(Math.Achievement = mean(Math.Score)) %>% # calculates the mean math score for each group
  ggplot(aes(x = Edu.Exp, y = Math.Achievement)) + # setting up x and y values for graphing
  geom_bar(stat = "identity") # plotting data points on the graph--"identity" means use the values as they are as opposed to the default which is counts 


##Let add color to the plot
pisa.listwise %>% 
  group_by(Edu.Exp) %>% 
  summarize(Math.Achievement = mean(Math.Score)) %>%
  ungroup() %>% 
  ggplot(aes(x = Edu.Exp, y = Math.Achievement, fill = Edu.Exp)) + #fill argument adds the color: these are default 
  geom_bar(stat = "identity")

##Now lets calculate the 95% Confidence Interval and put them on the plot
pisa.listwise %>% 
  group_by(Edu.Exp) %>%
  summarise( 
    n=n(),
    mean = mean(Math.Score), ##mean math score
    sd=sd(Math.Score)) %>% ##SD math score
  mutate(se=sd/sqrt(n))  %>% ##SE math score
  mutate(ic=se * qt((1-0.05)/2 + .5, n-1)) %>% ##CI for math score
  ggplot(aes(x = Edu.Exp, y = mean, fill = Edu.Exp)) + 
  geom_bar(stat="identity", alpha=0.5) + ##alpha = opacity of main bars
  geom_errorbar(aes(ymin=mean-ic, ymax=mean+ic), width=0.4, colour="orange", alpha=0.9, size=1.0) + ##alpha = opacity of error bars
  ggtitle("Bar Chart Using 95% Confidence Interval")


##We can also turn the bar chart on it's side using coord_flip() 
pisa.listwise %>% 
  group_by(Edu.Exp) %>%
  summarise( 
    n=n(),
    mean = mean(Math.Score), ##mean math score
    sd=sd(Math.Score)) %>% ##SD math score
  mutate(se=sd/sqrt(n))  %>% ##SE math score
  mutate(ic=se * qt((1-0.05)/2 + .5, n-1)) %>% ##CI for math score
  ggplot(aes(x = Edu.Exp, y = mean, fill = Edu.Exp)) + 
  geom_bar(stat="identity", alpha=0.9) + ##alpha = opacity of main bars
  geom_errorbar(aes(ymin=mean-ic, ymax=mean+ic), width=0.4, colour="orange", alpha=0.9, size=1.0) + ##alpha = opacity of error bars
  ggtitle("Bar Chart Using 95% Confidence Interval") +
  coord_flip() ##flip the chart


##Categorical vs Categorical: Segmented Bar Chart (100 percent bar chart)
pisa.listwise %>% 
ggplot(aes(x = Edu.Exp, fill = Gender)) + 
  geom_bar(position = "fill") + ##position = "fill" dictates this type of bar chart
  labs(y = "Proportion of Students", x = "Educaitonal Expectations")

#Categorical vs Categorical: Stacked Bar Chart 
pisa.listwise %>% 
ggplot(aes(x = Edu.Exp, fill = Gender)) + 
  geom_bar(position = "stack") + #position = "stack" dictates this type of bar chart
  scale_fill_manual(values=c("#0B775E", "#5F5647")) ##pick two colors

##Categorical vs Categorical: Grouped Bar Chart
pisa.listwise %>% 
  ggplot(aes(x = Edu.Exp, fill = Gender)) + 
  geom_bar(position = "dodge") + #position = "dodge" dictates this type of bar chart
  scale_fill_brewer(palette = "Set2") # use scale_fill_xxx to chose the desired color palette
  
##Quantitative vs Quantitative: Scatter Plot
##Create basic scatter plot
pisa.listwise %>% 
  ggplot(aes(x = Reading.Score, y = Math.Score)) +
  geom_point() ##creates the scatterplot

##Change the size and shape of the data points in the scatter plot
pisa.listwise %>% 
  ggplot(aes(x = Reading.Score, y = Math.Score)) +
  geom_point(size = 0.50, shape = 24) ##size changes diameter and shape chages the form of the dots

##If we want to examine a scatter plot grouped by gender with the shapes differing between group 
pisa.listwise %>% 
  ggplot(aes(x = Reading.Score, y = Math.Score, shape = Gender, colour = Gender)) + ##Shape and color arguments assigns different shape and colors 
  geom_point(size = 2.0) + ##make the data points bigger for easy viewing 
  scale_colour_brewer(palette = "Set3") ##change the color of the points using built in sets

##add a single linear regression line to a scatter plot
pisa.listwise %>% 
  ggplot(aes(x = Reading.Score, y = Math.Score)) + 
  geom_point(size = 2.0) + 
  stat_smooth(method = lm) ##there are several methods to choose from but for now we will stick with the linear regression line

##add a linear regression line for each Educational expectation to a scatter plot
pisa.listwise %>% 
  ggplot(aes(x = Reading.Score, y = Math.Score, colour = Edu.Exp)) +  ##add a colour for each level of 'Edu.Exp
  geom_point(size = 2.0) + 
  stat_smooth(method = lm) +
  scale_colour_brewer(palette = "Set1") ##another built in color set

##Quantitative vs Quantitative: Heat Map
install.packages("ggcorrplot")
library(ggcorrplot)

##This package only accepts a correlation matrix as it input so we will reduce our data to only continuous variables using the select()
pisa.2 <- pisa.listwise %>% 
          select(Math.Score, Math.Self.Eff, Math.Anxiety, Reading.Score, 
                 Feel.Safe, SES, Science.Score)
##Create correlation matrix
cors <- cor(pisa.2)

cors ##look at the correlation matrix

ggcorrplot(cors) ##create the heat map


##Linear Regression Diagnostics
install.packages("gglm")
install.packages("effects")
library(gglm)
library(effects)

##calculate a multiple linear regression 
mod1 <- lm(Math.Score ~ SES.cat + Math.Self.Eff + Immg + Gender + Math.Anxiety + SES.cat*Gender + Gender*Math.Anxiety, data = pisa.listwise)
summary(mod1)

# Plot the four main diagnostic plots
gglm(mod1)

#Residuals vs. fitted: non-linearity, unequal error variances, and outliers
##Normal Q-Q: normal distributionof residuals
##Scale-Location: homoscedasticity
##Residual vs. Leverage: outliers

##we can edit these plots individually by passing the model object to ggplot function
mod1 %>%  ##pass them model object to ggplot()
  ggplot() +
  stat_normal_qq(alpha = 1) + ##this parameter is changed to the correct stat_XXX_XX() for each of the plots--see gglm package
  theme_bw() + # add a clean theme 
  labs(title = "Normal Q-Q Plot for the linear model") + # change the title
  theme(plot.title = element_text(hjust = 0.5)) # center the title

##Plot the effects of each predictor
plot(allEffects(mod1))


##We can do the same thing for a Logistic Regression
pisa.listwise$lowEXP <- ifelse(pisa.listwise$Edu.Exp <= 6, 1, 0)
table(pisa.listwise$lowEXP)

mod3 <- glm(lowEXP ~ SES + Gender + Math.Self.Eff + Math.Anxiety , family = "binomial",
         data = pisa.listwise)
summary(mod3)

plot(allEffects(mod3))


##Clustered slopes 

pisa.listwise %>% 
  ggplot(mapping = aes(x = Math.Score, y = SES)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, fullrange = TRUE)

pisa.listwise %>% 
  ggplot(mapping = aes(x = SES, y = Math.Score, colour = factor(School_ID))) +
  geom_point() +
  geom_smooth(mapping = aes(group = School_ID), method = "lm", se = FALSE, fullrange = TRUE) +
  labs(colour = "School_ID")




