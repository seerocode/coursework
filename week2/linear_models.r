
library(tidyverse)
library(scales)

library(modelr)
options(na.action = na.warn)

theme_set(theme_bw())
options(repr.plot.width=4, repr.plot.height=3)

users <- read_tsv(gzfile('users.tsv.gz'))
head(users)

# histogram of the label/regressor variable:
ggplot(users, aes(x = daily.views)) +
  geom_histogram(bins = 50) +
  scale_x_log10(label=comma, breaks=10^(0:ceiling(log10(max(users$daily.views))))) +
  scale_y_continuous(label = comma) +
  xlab('Daily pageviews') +
  ylab('')
ggsave(filename='figures/daily_pageviews_dist.pdf', width=4, height=4)

# scatter plot of page views by age and gender
ggplot(data = users, aes(x = age, y = daily.views)) +
  geom_point() +
  facet_wrap(~ gender) +
  xlab('Age') +
  ylab('Daily pageviews')
ggsave(filename='figures/daily_pageviews_by_age_and_gender.pdf', width=8, height=4)

#compare number of rows before and after
nrow(users)
users <- filter(users, daily.views > 0)
nrow(users)

# table of age and gender and their median daily views
views_by_age_and_gender <- users %>%
  filter(age <= 90) %>%
  group_by(age, gender) %>%
  summarize(count = n(),
            median_dv = median(daily.views))
head(views_by_age_and_gender)

#
options(repr.plot.width=6, repr.plot.height=3)
ggplot(views_by_age_and_gender, aes(x = age, y = median_dv, color = gender)) +
  geom_line(aes(linetype=gender)) +
  geom_point(aes(size = count)) +
  xlab('Age') + 
  ylab('Daily pageviews') +
  scale_size_area(guide = F) +
  theme(legend.title=element_blank())
ggsave(filename='figures/median_daily_pageviews_by_age_and_gender.pdf', width=8, height=4)

# model data for users between 18 and 65
model_data <- filter(users, age >= 18 & age <= 65)

# linear model of the relationship btwn age and daily views (incorrect model)
options(repr.plot.width=4, repr.plot.height=3)
ggplot(model_data, aes(x = age, y = daily.views)) +
  geom_smooth(method = "lm") +
  scale_y_log10(breaks = 1:100)

# linera model of daily vages to age
model <- lm(log10(daily.views) ~ age, model_data)
model

# see summary(model) for more information, including standard errors and p-values for these estimates
M <- model.matrix(log10(daily.views) ~ age, model_data)
head(M)

plot_data <- model_data %>%
  distinct(age)
plot_data$predicted <- 10^predict(model, plot_data)
head(plot_data)

plot_data <- model_data %>%
  data_grid(age) %>%
  add_predictions(model) %>%
  mutate(pred = 10^pred)
head(plot_data)

ggplot(plot_data, aes(x = age, y = pred)) +
  geom_line()

plot_data <- model_data %>%
  group_by(age) %>%
  summarize(count = n(),
            geom_mean_daily_views = 10^(mean(log10(daily.views)))) %>%
  add_predictions(model) %>%
  mutate(pred = 10^pred)
head(plot_data)

ggplot(plot_data, aes(x = age, y = pred)) +
  geom_line(aes(y = pred)) +
  geom_point(aes(y = geom_mean_daily_views, size = count)) +
  scale_size_area(guide = F)

model <- lm(log10(daily.views) ~ age + I(age^2), model_data)
model


M <- model.matrix(log10(daily.views) ~ age + I(age^2), model_data)
head(M)

plot_data <- model_data %>%
  group_by(age) %>%
  summarize(count = n(),
            geom_mean_daily_views = 10^(mean(log10(daily.views)))) %>%
  add_predictions(model) %>%
  mutate(pred = 10^pred)

ggplot(plot_data, aes(x = age, y = pred)) +
  geom_line(aes(y = pred)) +
  geom_point(aes(y = geom_mean_daily_views, size = count)) +
  scale_size_area(guide = F)

form <- as.formula(log10(daily.views) ~ gender + age + I(age^2))
M <- model.matrix(form, model_data)
model <- lm(form, model_data)
head(M)
model

options(repr.plot.width=6, repr.plot.height=3)
plot_data <- model_data %>%
  group_by(age, gender) %>%
  summarize(count = n(),
            geom_mean_daily_views = 10^(mean(log10(daily.views)))) %>%
  add_predictions(model) %>%
  mutate(pred = 10^pred)

ggplot(plot_data, aes(x = age, y = pred, color = gender)) +
  geom_line(aes(y = pred)) +
  geom_point(aes(y = geom_mean_daily_views, size = count)) +
  scale_size_area(guide = F)

form <- as.formula(log10(daily.views) ~ gender * (age + I(age^2)))
M <- model.matrix(form, model_data)
model <- lm(form, model_data)
head(M)
model

plot_data <- model_data %>%
  group_by(age, gender) %>%
  summarize(count = n(),
            geom_mean_daily_views = 10^(mean(log10(daily.views)))) %>%
  add_predictions(model) %>%
  mutate(pred = 10^pred)

options(repr.plot.width=6, repr.plot.height=3)
ggplot(plot_data, aes(x = age, y = pred, color = gender)) +
  geom_line(aes(y = pred)) +
  geom_point(aes(y = geom_mean_daily_views, size = count)) +
  scale_size_area(guide = F)
ggsave(filename='figures/modeled_daily_pageviews_by_age_and_gender.pdf', width=8, height=4)
