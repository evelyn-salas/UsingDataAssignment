#################
#### PART 1 #####
#################

#1 
tas <- c("Coco", "Molly", "Yunwei", "YuYu", "Saurav", "Gavin", "Yubing", "George")

#2
rnorm(8, mean=3.6, sd= 0.4)
math_grades <- c(2.884261, 4.016653, 3.722160, 3.143390, 3.234215, 3.170579, 3.420344, 3.739486)

#3
rnorm(8, mean=3.8, sd=0.25)
spanish_grades <- c(3.841336, 3.610103, 3.800366, 3.629139, 4.151342, 3.876796, 3.502766, 3.379076)

#4
ta_grades <- data.frame(tas, math_grades, spanish_grades, stringsAsFactors = FALSE)


#5
colnames_string <- c("tas","math_grades", "spanish_grades")
colnames(ta_grades) <- colnames_string
COL_NAMES <- paste(colnames_string, collapse = ", ")


table_description <- paste("The grade data frame has 8 rows and 3 cols:", COL_NAMES)
print(table_description)


#6
colnames_singular <- c("ta", "math_grade", "spanish_grade")
colnames(ta_grades) <- colnames_singular

#7
ta_grades$better_at_math <- ta_grades$math_grade > ta_grades$spanish_grade

#8
print(ta_grades[ta_grades$ta == "YuYu",])

#9
num_better_at_math <- nrow(ta_grades[ta_grades$better_at_math == TRUE,])
print(num_better_at_math)

#10

ta_grades[ta_grades$math_grade > 4, "math_grade"] <- 4

ta_grades[ta_grades$spanish_grade > 4, "spanish_grade"] <- 4


#11
write.csv(ta_grades,"ta_grades.csv", row.names = FALSE)
#getwd()
#View("ta_grades.csv")

#################
#### PART 2 #####
#################

#1
data("Titanic")
View(Titanic)

#2
is.data.frame(Titanic)

#3
titanic_df <- as.data.frame(Titanic)

#4
children <- titanic_df[ titanic_df$Age == "Child",]

#5
num_children <- sum(nrow(children))
print(num_children)


#6
non_survivors <- titanic_df[titanic_df$Survived == "No","Freq"]
print(non_survivors)
max(non_survivors)

most_loses <- titanic_df[titanic_df$Freq == 670,]
print(most_loses)


#7
#expects class and returns string with the survival rate for men, women, and children

calc_survival_rate <- function(class) {
  subframe <- data.frame(titanic_df[titanic_df$Class == class, 1:5])
  male_class <- data.frame(subframe[subframe$Sex == "Male" & subframe$Age == "Adult", 1:5])
  total_male <- sum(male_class$Freq)
  total_male_survived <- sum(male_class[male_class$Survived == "Yes", 5])
  male_percent <- round((total_male_survived/total_male)*100)
  
  female_class <- data.frame(subframe[subframe$Sex == "Female" & subframe$Age == "Adult", 1:5])
  total_female <- sum(female_class$Freq)
  total_female_survived <- sum(female_class[female_class$Survived == "Yes", 5])
  child_class <- data.frame(subframe[subframe$Age == "Child", 1:5])
  total_child <- sum(child_class$Freq)
  total_child_survived <- sum(child_class[child_class$Survived == "Yes", 5])
  female_child_percent <- round(((total_child_survived+ total_female_survived)/(total_child + total_female))*100)
  
  string <- paste("of", class, "class", male_percent,"% of men survived and", female_child_percent, "% of women and children survived.")
  
  return(string)
}


#8
calc_survival_rate("1st")
calc_survival_rate("2nd")
calc_survival_rate("3rd")

#9

##1. The upper class had a higher survival rate than the lower and middle classes.
##2. Men had much lower survival rates than women and children. 

#################
#### PART 3 #####
#################

#1
## Sources: V7 by Mattias Lingren, IHME, and UN.

#2
life_exp_df <- read.csv("data/life_expectancy_years.csv", stringsAsFactors = FALSE)
View(life_exp_df)

#3
#expects column, returns the average of the values in the column
get_col_mean <- function(column) {
  mean(column, na.rm = TRUE)
}

#4
get_col_mean(life_exp_df$X2018)

#5
wolrd_averages <-lapply(life_exp_df[, -1], get_col_mean)



#6
world_change_18 <- (get_col_mean(life_exp_df$X2018)) - (get_col_mean(life_exp_df$X1918))
print(world_change_18)

#7
life_exp_df$recent_change <- life_exp_df$X2016 - life_exp_df$X1996

#8
num_small_gain <- print(sum(nrow(life_exp_df[life_exp_df$recent_change <= 0 ,])))
print(num_small_gain)

#9
most_improved <- life_exp_df[life_exp_df$recent_change == max(life_exp_df$recent_change), "country"]
print(most_improved)

#10
#expects country name and data.frame, returns change in life exp. from 1968 to 2018
get_country_change <- function(country, data.frame) {
  (life_exp_df[life_exp_df$country == country, "X2018"]) - (life_exp_df[life_exp_df$country == country, "X1968"])
}

#11
print(get_country_change("Haiti", data.frame))
  
#12
#expects two countries and a data frame, will return another data frame comparing the two countries
compare_countries <- function(country_a, country_b, data_frame) {
  country <- c(country_a, country_b)
  
  country_a_2018 <- life_exp_df[life_exp_df$country == country_a, "X2018"]
  country_a_recent_change <- life_exp_df[life_exp_df$country == country_a, "recent_change"]
  
  country_b_2018 <- life_exp_df[life_exp_df$country == country_b, "X2018"]
  country_b_recent_change <- life_exp_df[life_exp_df$country == country_b, "recent_change"]
  
  X2018 <- c(country_a_2018, country_b_2018)
  recent_change <- c(country_a_recent_change, country_a_recent_change)
  
  data.frame(country, X2018, recent_change, stringsAsFactors = FALSE)
}

  
#13
us_vs_cuba <- compare_countries("United States", "Cuba", life_exp_df)
print(us_vs_cuba)
  

