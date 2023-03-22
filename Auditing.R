library(dplyr)
library(ggpubr)
library(ggplot2)
library(stargazer)


#Q1
#import data
bankauditors <- read.csv("C:/beifen/RA 2022/Chicago accounting/bankauditors.csv")

df = bankauditors

attach(df)

# Change the names in a unified way
df$auditorname = tolower(df$auditorname)

df$auditorname[substr(df$auditorname, 1, 8) == "deloitte"]  = "deloitte" 

df$auditorname[substr(df$auditorname, 1, 5) == "ernst"]  = "ey" 

df$auditorname[substr(df$auditorname, 1, 4) == "kpmg"]  = "kpmg" 

df$auditorname[substr(df$auditorname, 1, 5) == "price"]  = "pwc" 


# Creating a binary variable if big4 is hired
df$big4 <- ifelse(df$auditorname %in% c("deloitte", "ey", "kpmg","pwc"),
  1,  # if condition is met, put 1
  0   # else put 0
)


#regress choice of big4 on being publicly listed
OLS1 <- lm(big4 ~ public, data= df )
summary(OLS1)

stargazer(OLS1)

#Q2

#Computing shares

Sumall = sum(df$totalassets)
Sumbig4 = sum(df[which(df$big4==1), 6])
share = Sumbig4/Sumall

#Computing shares by year
A1 = df %>%
      group_by(year) %>%
       summarise(Freq = sum(totalassets))
  
#Computing shares that with big4 auditors by year
A2 = df %>%
  group_by(year) %>%
  filter(big4 == 1) %>% 
  summarise(Freq = sum(totalassets))

A3 = df %>%
  group_by(year) %>%
  summarise(Freq = sum(public))

M1 = merge(A1, A2, by = "year") 
M1$share = M1$Freq.y/M1$Freq.x

M1 = merge(M1, A3, by = "year") 

#plot the time trend of shares

plot(M1$year, M1$share, type="b", pch=19, col="red", xlab="year", ylab="share")


#Computing shares by state

A4 = df %>%
  group_by(auditorstate) %>%
  summarise(Freq = sum(totalassets))

A5 = df %>%
  group_by(auditorstate) %>%
  filter(big4 == 1) %>% 
  summarise(Freq = sum(totalassets))

M2 = merge(A4,A5,by = "auditorstate",all=TRUE)
M2[is.na(M2)]<-0
M2$share = M2$Freq.y/M2$Freq.x


#plot the state difference of shares

barplot(height=M2$share, names=M2$auditorstate,las=2,cex.names=.5)





#Q3

#subsample of last year

df2 <- df[which(df$year==2015), ]

df2total = sum(df2$totalassets)

#Number of Distinct Banks in each group

B1 <- df2 %>%                             
     group_by(bankstate) %>%
     summarise(nbanks = n_distinct(bankId))

#Number of Distinct Public Banks in each group

B2 <- df2 %>%                             
  group_by(bankstate) %>%
  filter(public == 1) %>% 
  summarise(npublic = n_distinct(bankId))

#Sum of Total Bank Assets in each group

B3 <- df2 %>%                             
  group_by(bankstate) %>%
  summarise(totass = sum(totalassets))

#Average of Bank Assets in each group

B4 <- df2 %>%                             
  group_by(bankstate) %>%
  summarise(avass = mean(totalassets))

#Maximum of Bank Assets in each group

B5 <- df2 %>%                             
  group_by(bankstate) %>%
  summarise(maxass = max(totalassets))

B6 <- df2 %>%                             
  group_by(bankstate) %>%
  filter(public == 1) %>% 
  summarise(totasspb = sum(totalassets))

#merge and fill missing value with 0
df_list <- list(B1, B2, B3,B4,B5,B6)
df_new = Reduce(function(x, y) merge(x, y,by = "bankstate",all=TRUE), df_list)

df_new[is.na(df_new)]<-0

#compute Fraction of Total Bank Assets held by Public Banks in each group

df_new$ratiopublic = df_new$totasspb/df_new$totass

#compute Total Assets in the group as a fraction of the total assets of all banks
df_new$ratiototal = df_new$totass/df2total

df_new = select(df_new, -7)
df_new = df_new[-1,]

write.csv(df_new,"bankstatedata.csv", row.names = FALSE)


