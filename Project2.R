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
temp <- ggplot(df,aes(x = mass)) +
  geom_histogram() +
  labs(
    title = "Histogram of Lizard Mass",
    x = "Mass of Lizard (g)",
    y = "No. Per Bucket"
  )

temp1 <- ggplot(df,aes(x = svl)) +
  geom_histogram() +
  labs(
    title = "Histogram of Lizard SVL",
    x = "Snout-vent Length (mm)",
    y = "No. Per Bucket"
  )
temp
temp1

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
  ) + theme_bw()

temp3 <- ggplot(df_transform,aes(x = svl, fill = type)) +
  geom_histogram() +
  labs(
    title = "Histogram of Lizard SVL",
    x = "Log of Snout-vent Length (mm)",
    y = "No. Per Bucket"
  ) + theme_bw()

temp2
temp3

# ------------------------ Part 6 ------------------------

cor(df$mass[df$type == 0], df$svl[df$type == 0])
cor(df_transform$mass[df_transform$type == 0], df_transform$svl[df_transform$type == 0])

cor(df$mass[df$type == 1], df$svl[df$type == 1])
cor(df_transform$mass[df_transform$type == 1], df_transform$svl[df_transform$type == 1])

#Show how taking the log made it more linear thus more correlated as the corr() function works on a lienar
#basis
ggplot(df, aes(x = mass, y = svl)) +
  geom_point() +
  labs(
    x = "mass",
    y = "svl"
  )

ggplot(df_transform, aes(x = mass, y = svl)) +
  geom_point() +
  labs(
    x = "log(mass)",
    y = "log(svl)"
  )


# ------------------------ Part 7 ------------------------


f.hotel2<-function(x1,x2)
{
  # data matrices x1 and x2 for the two groups
  # Compute the dimensions of the data matrices
  p<-dim(x1)[2]
  n1<-dim(x1)[1]
  n2<-dim(x2)[1]
  
  # Compute sample means and covariances
  
  m1<-apply(x1,2,mean)
  s1<-var(x1)
  m2<-apply(x2,2,mean)
  s2<-var(x2)
  cat("xbar1=",m1," s1=",s1," xbar2=",m2," s2=",s2,fill=T)
  # Compute the pooled estimate of covariance matrix
  s<-((n1-1)*s1+(n2-1)*s2)/(n1+n2-2)
  # Compute the two sample Hotelling T-squared test
  T2<-((n1*n2)/(n1+n2))*t(m1-m2)%*%solve(s)%*%(m1-m2)
  # Compute F-value, compute df and the p-value
  fval<-(n1+n2-p-1)*T2/(p*(n1+n2-2))
  df1<-p
  df2<-n1+n2-p-1
  pval<-1-pf(fval,df1,df2)
  cat("T-squared=",T2,fill=TRUE)
  df<-cbind(df1,df2)
  cat("F-value=",fval," df=",df," p-value=",pval,fill=T)
}

# vars are: C_mass, C_svl, S_mass, and S_svl
############# Hotelling two-sample test.
df_type0Trans <- df_transform %>%
  filter(type == 0)

df_type1Trans <- df_transform %>%
  filter(type == 1)

          
f.hotel2(df_type0Trans[,1:2],df_type1Trans[,1:2])


ggplot(df_transform, aes(x = mass, y = svl, color = type)) +
  geom_point() +
  labs(
    x = "log(mass)",
    y = "log(svl)"
  )










