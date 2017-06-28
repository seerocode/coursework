<<<<<<< HEAD
library(ggplot2)
library(plyr)
library(tidyverse)
library(lubridate)

setwd("~/coursework/week3/")
loans <- read_csv("Loan_payments_data.csv")


#### Clean data
loans <- loans %>%
  mutate(effective_date = as.Date(effective_date, "%m/%d/%Y"),
         due_date = as.Date(due_date, "%m/%d/%Y"),
         paid_off_time = as.Date(paid_off_time, "%m/%d/%Y %H:%M"),
         education = factor(education, levels = c("High School or Below", "college", "Bechalor", "Master or Above")))

#across a specific level of education, what is the percentage of people that pay their loan off
loans %>%
  select(loan_status, education) %>% group_by(education, loan_status) %>% summarise(count = n()) %>% 
  mutate(percentage = count/sum(count)) %>%
  ggplot() +
  geom_bar(aes(x=loan_status, y=percentage, fill=education), stat = "identity", position = "dodge") +
  scale_y_continuous(labels = scales::percent)


loans %>%
  select(loan_status, education) %>% group_by(education, loan_status) %>% summarise(count = n()) %>%
  ggplot() +
  geom_point(aes(x=loan_status, y=percentage, fill=education), stat = "identity", position = "dodge") +
  scale_y_continuous(labels = scales::percent)

# principal on x and percentage 
loans %>% filter(loan_status=="PAIDOFF") %>% mutate(days_used = paid_off_time - effective_date + 1) %>%
mutate(perc_days = days_used/as.numeric(due_date - effective_date + 1)) %>% 
  View

#Some plots (e.g. ggplot's) can be saved and then shown via print()
#Others like plot(), we'd have to save the output to a graphics file
#(But you should use ggplot anyways!)


save(pval, plt, file="outputs.RData")
=======
library(ggplot2)
df = data.frame(id = 1:100)
df$x = df$id/10
df$e = rnorm(100)
df$y = 2*df$x + df$e
ols = lm(y~x, data=df)
pval = summary(ols)$coefficients[2,4]

#Some plots (e.g. ggplot's) can be saved and then shown via print()
#Others like plot(), we'd have to save the output to a graphics file
#(But you should use ggplot anyways!)
plt = ggplot(data=df, aes(x=x, y=y))+geom_point()

save(pval, plt, file="outputs.RData")
>>>>>>> 4c1311f18b80773052258b9264b35cee6a613a58
