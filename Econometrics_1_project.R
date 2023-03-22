library(lmtest)
library("tidyverse")
library("sf")
library(strucchange)
library(car)
library("knitr")
library("haven")
library("stargazer")
library("broom")
library("ggpubr")
library("ggplot2")
library("ivreg")
library("sandwich")

#Question 1
electricity <- read_dta("C:/PSE Economics/Econometrics 1/Homework/electricity.dta")

attach(electricity)

# Get descriptive statistics

summary(electricity)

# OLS estimates

fit1 = lm(log(TC)~ log(Q)+log(PL)+log(PF)+log(PK),data = electricity )

summary(fit1)

# Estimate the restricted model

fit2 = lm(I(log(TC)-log(PF))~ log(Q)+I(log(PL)-log(PF))+I(log(PK)-log(PF)),data = electricity )

summary(fit2)

coeftest(fit1, df = Inf)

vcov(fit1)

cor(PF,East)

# Estimate model 2b

fit3 = lm(log(TC)~ log(Q)+log(PL)+log(PF)+log(PK)+ East,data = electricity )

summary(fit3)

#Do structural shift test

eastfirm <- subset(electricity, East == 1, select = c("TC","Q","PL","PF","PK"))
  
fit4 = lm(log(TC)~ log(Q)+log(PL)+log(PF)+log(PK),data = eastfirm )

summary(fit4)

westfirm <- subset(electricity, East == 0, select = c("TC","Q","PL","PF","PK"))

fit5 = lm(log(TC)~ log(Q)+log(PL)+log(PF)+log(PK),data = westfirm )

summary(fit5)

electricity_sort = electricity[order(East),]

attach(electricity_sort)

sctest(log(TC) ~ log(Q)+log(PL)+log(PF)+log(PK), type = "Chow", point = 74)

#plot residuals and overlaying with normal

fit1res = resid(fit1)

plot1 <- ggplot(electricity, aes(x=fit1res)) +
  geom_histogram() +
  theme_classic() +
  labs(x = "Residual")

plot1

plot2 <- ggplot(electricity, aes(x=fit1res)) +
  geom_histogram() +
  theme_classic() +
  labs(x = "Residual")+
  stat_function(fun = dnorm, n = 145, args = list(mean = 0, sd = 1)) + ylab("") +
  scale_y_continuous(breaks = NULL)

binwidth = 0.05
n = 145
ggplot(electricity, aes(x = fit1res, binwidth = binwidth, n = n)) +
  theme_bw() +
  geom_histogram(binwidth = binwidth, 
                 colour = "white", fill = "cornflowerblue", size = 0.1) +
  stat_function(fun = function(x) dnorm(x, mean = 0, sd = 1) * n * 0.05,
                color = "darkred", size = 1)

plot(fitted(fit1), resid(fit1), xlab='Fitted Values', ylab='Residuals')

plot(lm(log(TC)~ log(Q)+log(PL)+log(PF)+log(PK),data = electricity ))

# BP TEST
bptest(fit1)

#wHITE TEST
bptest(fit1,~fitted(fit1)+I(fitted(fit1)^2))


# WLS estimation
wt <- 1 / lm(abs(fit1$residuals) ~ fit1$fitted.values)$fitted.values^2



fitwls <- lm(log(TC)~ log(Q)+log(PL)+log(PF)+log(PK),data = electricity,weights=wt)

summary(fitwls)

#Question 2

beveridge <- read.csv("C:/PSE Economics/Econometrics 1/Homework/beveridge.csv")


# OLS estimation of model 3
fit6 = lm(urate ~ vrate,data = beveridge )

summary(fit6)

#Get NW standard errors

coeftest(fit6,vcov=NeweyWest(fit6,verbose=T))

#DW test

durbinWatsonTest(fit6)

dwtest(fit6)

bgtest(fit6, order=1, data=beveridge)

bgtest(fit6, order=2, data=beveridge)

bgtest(fit6, order=3, data=beveridge)

#Question 3.1

df <- read_dta("C:/PSE Economics/Econometrics 1/Homework/taxrate.dta")

# OLS of model 4 and compute the effect for a municipality

Model4 = lm(growth_business_crea ~ tax_rate + logpop + sqlogpop, data = df )

summary(Model4)

mean(df$pop,na.rm = T)

Effect1 = (0.109-0.01*log(34628))/100
Effect2 = (0.109 - 0.01*log(200000))/100

#plot and check relevance

plot3 <- ggplot(df, aes(x = party, y = tax_rate)) + 
  geom_point(size = 1) + 
  geom_smooth(method='lm') +
  theme_classic() 

plot3

plot4 <- ggplot(df, aes(x = share_busown, y = tax_rate)) + 
  geom_point() + 
  geom_smooth(method='lm') +
  theme_classic() 

plot4

plot5 <- ggplot(df, aes(x = avg_dep_taxrate, y = tax_rate)) + 
  geom_point() + 
  geom_smooth(method='lm') +
  theme_classic() 

plot5

corr1 <- cor.test(df$share_busown, df$tax_rate,
                  method = "pearson")
corr1

corr2 <- cor.test(df$avg_dep_taxrate, df$tax_rate,
                  method = "pearson")
corr2


df_1 <- df %>% 
  select(party, tax_rate) %>%
  group_by(party) %>%
  summarise(average_local_tax_rate = mean(tax_rate, na.rm = T)) %>%
  ungroup ()


df_1$party <- factor(df_1$party, levels=c('Right','Others','Left','Far Left'), ordered=TRUE) 
plot6 <- ggplot(data = df_1, aes(party, average_local_tax_rate)) +
  geom_bar(stat = "identity", fill="steelblue") +
  theme_classic() +
  labs(x = "Party affiliation", y = "Average local tax rate",title = "Barplot of the average local tax rate by party affiliation") + 
  geom_text(aes(label=round(average_local_tax_rate,digits = 3)), vjust = 1.5, colour = "yellow", position = position_dodge(.8), size = 5) 
plot6


# 1st stage estimation

simIV_1ststage_1 = lm(tax_rate ~ party + logpop + sqlogpop , data = df ) ##  'party' as iV

simIV_1ststage_2 = lm(tax_rate ~ share_busown+ logpop + sqlogpop , data = df ) ##  'share_busown' as iV

simIV_1ststage_3 = lm(tax_rate ~ avg_dep_taxrate + logpop + sqlogpop , data = df ) ##  'avg_dep_taxrate' as iV

# Get fitted value of of three 1st stage regressions for endogenous value 

tax_rate_hat_party_IV <- simIV_1ststage_1$fitted.values
tax_rate_hat_share_busown_IV <- simIV_1ststage_2$fitted.values
tax_rate_hat_avg_dep_taxrate_IV <- simIV_1ststage_3$fitted.values

# 2nd stage estimation

simIV_2ndstage_1 <- lm(growth_business_crea ~ tax_rate_hat_party_IV + logpop + sqlogpop, data = df)
simIV_2ndstage_2 <- lm(growth_business_crea ~ tax_rate_hat_share_busown_IV + logpop + sqlogpop, data = df)
simIV_2ndstage_3 <- lm(growth_business_crea ~ tax_rate_hat_avg_dep_taxrate_IV + logpop + sqlogpop, data = df)

#Estimate model 4a and 4b
Model4a <- ivreg(growth_business_crea ~ tax_rate + logpop + sqlogpop | party + share_busown +  avg_dep_taxrate + logpop + sqlogpop , data = df )

Model4b <- ivreg(growth_business_crea ~ tax_rate + logpop + sqlogpop | party + avg_dep_taxrate + logpop + sqlogpop , data = df )

summary(Model4a,test = TRUE) 
summary(Model4b, test = TRUE)

#Question 3.2

df3.2 <- read_dta("C:/PSE Economics/Econometrics 1/Homework/enterprise.dta")

df3.2_treated <- df3.2 %>% #focus only on the 13 treated municipalities
  filter(ez == "1") %>%
  select(year, dep, code_com, lib_com, ez, post, prob_newemp, ) %>%
  group_by(ez) %>%
  ungroup ()

# Estimate the effect of post
Model5 <- lm(prob_newemp ~ post, data = df3.2_treated )
summary(Model5)


#compare the mean value of probability to find a job of both groups in 1997
df3.2_CS <- df3.2 %>%
  filter(year == "1997") %>%
  select(year, dep, code_com, lib_com, ez, post, prob_newemp, ) %>%
  group_by(ez) %>%
  summarise(Mean_prob_job_finding = mean(prob_newemp, na.rm = T)) %>% 
  ungroup ()

CS_Estimate = 0.1557675 - 0.1918927

df3.2_BA <- df3.2 %>%
  filter(ez == "1") %>%
  select(year, dep, code_com, lib_com, ez, post, prob_newemp, ) %>%
  group_by(post) %>%
  summarise(Mean_prob_job_finding = mean(prob_newemp, na.rm = T)) %>%
  ungroup ()

BA_Estimate = 0.1459451 - 0.1810429

df3.2_control <- df3.2 %>%
  filter(ez == "0") %>% 
  select(year, dep, code_com, lib_com, ez, post, prob_newemp) %>%
  group_by(post) %>%
  summarise(Mean_prob_job_finding = mean(prob_newemp, na.rm = T)) %>% 
  ungroup ()


df3.2_did <- df3.2 %>%
  select(year, dep, code_com, lib_com, ez, post, prob_newemp,tx_nodip, tx_tech, tx_univ, tx25, unemprate) %>%
  mutate(did = ez*post) %>%
  group_by(dep) %>%
  ungroup()


#DiD estimate
Model_Q3_DiD <- lm(prob_newemp ~ ez + post + did + dep, data = df3.2_did )

df3.2_Q4 <- df3.2_did %>%
  filter(year == "1994") %>%
  select(year, dep, code_com, lib_com, ez, post, prob_newemp, tx_nodip, tx_tech, tx_univ, tx25) %>%
  group_by(ez) %>%
  summarise_at(c("tx_nodip", "tx_tech", "tx_univ", "tx25"),
               list(avg = ~mean(.)), 
               na.rm=T) %>%   
  ungroup ()


#DiD estimate with additional controls
Model_Q4_DiD <- lm(prob_newemp ~ ez + post + did + dep + tx_nodip + tx_univ, data = df3.2_did )

summary(Model_Q4_DiD)


Model_Q5_DiD <- lm(prob_newemp ~ ez + post + did + dep + tx_nodip + tx_univ + unemprate, data = df3.2_did )

summary(Model_Q5_DiD)

df3.2_pretreat <- df3.2_did %>%
  filter(year <= "1996") %>% 
  select(year, dep, code_com, lib_com, ez, post, prob_newemp, did) %>%
  mutate(pretreatment = ez*year) %>%
  group_by(dep) %>%
  ungroup()

df3.2_paratrend <- df3.2_did

# Get mean of prob_newemp by year, conditional on 'ez'

mean_prob_newemp= aggregate(df3.2_paratrend$prob_newemp, list(df3.2_paratrend$year,df3.2_paratrend$ez == 1), mean)

names(mean_prob_newemp) = c("Year","Treatment","average_probability")
mean_prob_newemp$Group[1:6] = "Control municipalities"
mean_prob_newemp$Group[7:12] = "treatment municipalities"

# Plot the evolution of prob_newemp

plot7 <- ggplot(mean_prob_newemp, aes(Year, average_probability, group=Group,color=Group, shape=Group))+
  geom_point()+
  geom_line()+
  geom_vline(xintercept = 1997) +
  labs(x="Year", y="average probability to find a job")
plot7

Model_Q6_comtrend <- lm(prob_newemp ~ ez + year + pretreatment , data = df3.2_pretreat)

summary(Model_Q6_comtrend)