library(tidyverse)
library(here)

# Read in data and clean up column names
df <- read.table("lizrds.dat") %>% 
  rename(mass = V1, svl = V2, type = V3) 

df <- df[-1,] %>% 
  mutate(mass = as.numeric(mass), 
         svl = as.numeric(svl))

df_transform <- df %>% 
  mutate(mass = log(mass),
         svl = log(svl))


# ------------------------ Part 1 ------------------------ 

part1 <- function() {
  print(shapiro.test(df$mass))
  
  print(shapiro.test(df$svl))
  
}

# Here we can see the the data is infact not normal and more log normal
temp <- ggplot(df,aes(x = mass, fill = type)) +
  geom_histogram() +
  labs(
    title = "Histogram of Lizard Mass",
    x = "Mass of Lizard (g)",
    y = "No. Per Bucket"
  )

temp1 <- ggplot(df,aes(x = svl, fill = type)) +
  geom_histogram() +
  labs(
    title = "Histogram of Lizard SVL",
    x = "Snout-vent Length (mm)",
    y = "No. Per Bucket"
  )


# ------------------------ Part 2 ------------------------ 
#Perform Wilcoxon Rank Sum Test on Mass and SVL based on Group
part2 <- function() {
  #Two ways of doing it
  #Mass
  print(wilcox.test(mass ~ type, data = df))
  
  W1 <- sum(outer(filter(df, type == 0)$mass, filter(df, type == 1)$mass, "<"))
  
  (1-pwilcox(W1-1,20,40))*2
  
  #SVL
  print(wilcox.test(svl ~ type, data = df, exact = FALSE)) #Deal with TIEs
}


# Can't do the same second method to confirm as that does not deal with ties, the regular 
# Wilcoxon test is sufficient

# ------------------------ Part 3 ------------------------

part3 <- function() {
  
  print(shapiro.test(df_transform$mass))
  
  print(shapiro.test(df_transform$svl))
}




# ------------------------ Part 4 ------------------------

part4 <- function() {
  print(t.test(mass ~ type, data = df_transform))
  
  
  print(t.test(svl ~ type, data = df_transform))
}


# ------------------------ Part 5 ------------------------

# Here we can see the the data is infact not normal and more log normal


temp2 <- ggplot(df_transform,aes(x = mass, fill = type)) +
  geom_histogram() +
  labs(
    title = "Histogram of Lizard Mass",
    x = "Log of Mass of Lizard (g)",
    y = "No. Per Bucket"
  )

temp3 <- ggplot(df_transform,aes(x = svl, fill = type)) +
  geom_histogram() +
  labs(
    title = "Histogram of Lizard SVL",
    x = "Log of Snout-vent Length (mm)",
    y = "No. Per Bucket"
  )

temp2
temp3

# ------------------------ Part 6 ------------------------

cor(df$mass[df$type == 0], df$svl[df$type == 0])

cor(df$mass[df$type == 1], df$svl[df$type == 1])


cor(df_transform$mass[df_transform$type == 0], df_transform$svl[df_transform$type == 0])

cor(df_transform$mass[df_transform$type == 1], df_transform$svl[df_transform$type == 1])

#Show how taking the log made it more linear thus more correlated as the corr() function works on a lienar
#basis
ggplot(df, aes(x = mass, y = svl)) +
  geom_point()

ggplot(df_transform, aes(x = mass, y = svl)) +
  geom_point()


















