# This is just a comment
# some example of R as calculator

2+3
2+3
(2 + 3)/5
pi
pi*3
exp(.7)
log(2)
?log
log(exp(3))
log(1)


x = 2 + 5
y = 4
y <- 4
# how to change the value of x
x = 5
# hor to remove the object x
rm(x)

# create a vector
x = c(1, 2, 3, 4, 5)
log(x)

a = "Hi!"
??concatenate
?paste
paste("High", "This is my name:", "Alessio")

library(tidyverse)
orca = read.table("http://www.stats4life.se/data/oralca.txt")
?read.table


# have a view of the data
View(orca)
orca1 = as_data_frame(orca)

# to print the data in the console
orca1
# to get the number of obs and variables
dim(orca1)
nrow(orca1)
ncol(orca1)

# to get the structure of an object
str(orca1)
head(orca1, 4)
tail(orca1)

# which variable are in the data
names(orca1)


# create a new variable
orca1 = mutate(orca1, 
       dead = event != "Alive", 
       time_days = time*365.25,
       age_extreme = age <= 20 | age > 70)

# selecting only some variables
select(orca1, stage, age, time, dead)

orca_sub = filter(orca1, 
                  stage == "IV" | stage == "III",
                  age >= 20 & age <= 80)

# how to sort a data set
arrange(orca_sub, desc(time), age, sex)


# The pipe operator %>% simplifies your life
orca_final = orca %>%
  as_data_frame() %>%
  mutate(dead = event != "Alive",
         time_days = time*365.25,
         age_extreme = age <= 20 | age > 70) %>%
  select(stage, age, time, dead) %>%
  filter(stage == "IV" | stage == "III",
         age >= 20 & age <= 80) %>%
  arrange(desc(time), age)

# how count how many missing data are present in our data set

# how many obs are complete (i.e. no missing values in any column)
sum(complete.cases(orca1))
which(!complete.cases(orca1))


orca_wrong <- orca1 %>%
  mutate(
    age = replace(age, age < 20, NA)
  )
sum(complete.cases(orca_wrong))

which(!complete.cases(orca_wrong))                    


## Summarise plot

# a quick summary of the data swt
summary(orca1)

# let's summarize a continuous variable: age
orca1$age
summary(orca1$age)
# too long to write
summary(select(orca1, age))

quantile(orca1$age, c(.1, .5, .7, .9))
mean(orca1$age)
mean(orca_wrong$age, na.rm = T)
sd(orca1$age)
?sd
help(sd)
mad(orca1$age)


# an example of a plot withh ggplot
p_ex1 = ggplot(orca1, aes(x = age, y = time)) +
  geom_point(aes(shape = event, color = event)) +
  geom_smooth(se = F)

# how to change even more parameters (Nil's question)
ggplot(orca1, aes(x = age, y = time)) +
  geom_point(aes(shape = event, color = event)) +
  geom_smooth(se = F) +
  # change the default for shapes (?pch) and color
  scale_color_manual(values = c("green", "purple", "orange")) +
  scale_shape_manual(values = c(16, 4, 8)) +
  theme_classic()


library(plotly)
ggplotly(p_ex1)

ggplot(orca1, aes(age, group = stage)) + 
  geom_histogram(aes(fill = stage)) +
  facet_grid(stage ~ .) +
  theme_bw()

# let's check the density
ggplot(orca1, aes(x = age, y = ..density..)) +
  geom_histogram() +
  geom_density()

?geom_histogram
?shapiro.test
# how to perform a normality test
shapiro.test(orca1$age)
