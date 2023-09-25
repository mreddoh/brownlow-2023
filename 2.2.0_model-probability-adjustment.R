
# Load packages ----
library(tidyverse)
library(here)
#library(DescTools)
#library(fitdistrplus)


# Correct for continuous regression prediction, for discrete outcome ----
score.2.votes <- full_out %>% 
  mutate(predicted_grp = cut(predicted_votes_raw, breaks = c(-Inf,seq(-1,3,0.1),Inf))) %>%
  group_by(predicted_grp) %>%
  mutate(min_score = round(min(predicted_votes_raw),2)) %>%
  group_by(min_score, predicted_grp,brownlow_votes) %>%
  summarise(n=n()) %>%
  pivot_wider(names_from = brownlow_votes, names_prefix = "v", values_from = n) %>% 
  arrange(predicted_grp)

score.2.votes$Total <- rowSums(score.2.votes[,3:6], na.rm = T)

score.2.votes <- score.2.votes %>% 
  mutate(p.0v = v0 / Total,
         p.1v = v1 / Total,
         p.2v = v2 / Total,
         p.3v = v3 / Total)

# Plot distributions ----

score.2.votes %>% ungroup() %>% 
  dplyr::select(min_score,p.0v,p.1v,p.2v,p.3v) %>% 
  pivot_longer(p.0v:p.3v, names_to = "vote_prob", values_to = "values") %>% 
    ggplot(aes(x=min_score, y=values)) +
      geom_smooth(aes(colour = vote_prob))


# * Fit probability distribution to model predictions for 1 vote ----

df <- score.2.votes %>% 
  rename(x=min_score,y=p.1v) %>% 
  ungroup() %>% 
  dplyr::select(x,y) %>% 
  filter(!row_number() %in% c(24,25,26))

# fit1 <- lm(y~x, data=df)
# fit2 <- lm(y~poly(x,2,raw=TRUE), data=df)
# fit3 <- lm(y~poly(x,3,raw=TRUE), data=df)
# fit4 <- lm(y~poly(x,4,raw=TRUE), data=df)
fit5 <- lm(y~poly(x,5,raw=TRUE), data=df)
# fit6 <- lm(y~poly(x,6,raw=TRUE), data=df)
# fit7 <- lm(y~poly(x,7,raw=TRUE), data=df)
# fit8 <- lm(y~poly(x,8,raw=TRUE), data=df)

#define x-axis values
x_axis <- c(-Inf,seq(-1,4,0.1),Inf)

#create a scatterplot of x vs. y
plot(df$x, df$y, pch=19, xlab='x', ylab='y', xlim = c(-1,4))


#add curve of each model to plot
# lines(x_axis, predict(fit1, data.frame(x=x_axis)), col='green')
lines(x_axis, predict(fit2, data.frame(x=x_axis)), col='lightblue')
lines(x_axis, predict(fit3, data.frame(x=x_axis)), col='purple')
lines(x_axis, predict(fit4, data.frame(x=x_axis)), col='blue')
lines(x_axis, predict(fit5, data.frame(x=x_axis)), col='red')
lines(x_axis, predict(fit6, data.frame(x=x_axis)), col='orange') # YES!
lines(x_axis, predict(fit7, data.frame(x=x_axis)), col='green')
lines(x_axis, predict(fit8, data.frame(x=x_axis)), col='violet')

fit5
###### Fit 5!
v1.fit = fit5


# * Fit probability distribution to model predictions for 2 votes ----

df <- score.2.votes %>% 
  rename(x=min_score,y=p.2v) %>% 
  ungroup() %>% 
  dplyr::select(x,y) %>% 
  filter(!row_number() %in% c(19,28,30,31,32,33,34)) %>% 
  add_row(x = 3, y = 0.05)

fit1 <- lm(y~x, data=df)
fit2 <- lm(y~poly(x,2,raw=TRUE), data=df) # still not great...
fit3 <- lm(y~poly(x,3,raw=TRUE), data=df)
fit4 <- lm(y~poly(x,4,raw=TRUE), data=df)
fit5 <- lm(y~poly(x,5,raw=TRUE), data=df)
fit6 <- lm(y~poly(x,6,raw=TRUE), data=df)
fit7 <- lm(y~poly(x,7,raw=TRUE), data=df)

#define x-axis values
x_axis <- c(-Inf,seq(-2,3.75,0.1),Inf)

#create a scatterplot of x vs. y
plot(df$x, df$y, pch=19, xlab='x', ylab='y', xlim = c(-1,4))

#add curve of each model to plot
lines(x_axis, predict(fit1, data.frame(x=x_axis)), col='green')
lines(x_axis, predict(fit2, data.frame(x=x_axis)), col='red')
lines(x_axis, predict(fit3, data.frame(x=x_axis)), col='purple')
lines(x_axis, predict(fit4, data.frame(x=x_axis)), col='blue')
lines(x_axis, predict(fit5, data.frame(x=x_axis)), col='orange')
lines(x_axis, predict(fit6, data.frame(x=x_axis)), col='brown')
lines(x_axis, predict(fit7, data.frame(x=x_axis)), col='lightblue')

fit4
###### Fit 4 is best
v2.fit = fit4


# * Fit probability distribution to model predictions for 3 votes ----

score.2.votes <- full_out %>% 
  mutate(predicted_grp = cut(predicted_votes_raw, breaks = c(-Inf,seq(-1,4,0.1),Inf))) %>%
  group_by(predicted_grp) %>%
  mutate(min_score = round(min(predicted_votes_raw),2)) %>%
  group_by(min_score, predicted_grp,brownlow_votes) %>%
  summarise(n=n()) %>%
  pivot_wider(names_from = brownlow_votes, names_prefix = "v", values_from = n) %>% 
  arrange(predicted_grp)

score.2.votes$Total <- rowSums(score.2.votes[,3:6], na.rm = T)

score.2.votes <- score.2.votes %>% 
  mutate(p.0v = v0 / Total,
         p.1v = v1 / Total,
         p.2v = v2 / Total,
         p.3v = v3 / Total)

df <- score.2.votes %>% 
  rename(x=min_score,y=p.3v) %>% 
  ungroup() %>% 
  dplyr::select(x,y) %>% 
  filter(!row_number() %in% c(29,30,31,36))


fit1 <- lm(y~x, data=df)
fit2 <- lm(y~poly(x,2,raw=TRUE), data=df)
fit3 <- lm(y~poly(x,3,raw=TRUE), data=df)
fit4 <- lm(y~poly(x,4,raw=TRUE), data=df)
fit5 <- lm(y~poly(x,5,raw=TRUE), data=df)
fit6 <- lm(y~poly(x,6,raw=TRUE), data=df)
fit7 <- lm(y~poly(x,7,raw=TRUE), data=df)


#define x-axis values
x_axis <- c(-Inf,seq(-2,4,0.1),Inf)

#create a scatterplot of x vs. y
plot(df$x, df$y, pch=19, xlab='x', ylab='y', xlim = c(-1,4))


#add curve of each model to plot
lines(x_axis, predict(fit1, data.frame(x=x_axis)), col='green')
lines(x_axis, predict(fit2, data.frame(x=x_axis)), col='red')
lines(x_axis, predict(fit3, data.frame(x=x_axis)), col='purple')
lines(x_axis, predict(fit4, data.frame(x=x_axis)), col='blue')
lines(x_axis, predict(fit5, data.frame(x=x_axis)), col='orange')
lines(x_axis, predict(fit6, data.frame(x=x_axis)), col='brown')
lines(x_axis, predict(fit7, data.frame(x=x_axis)), col='lightblue')

fit5
###### Fit 4 is best
v3.fit = fit5


# Check overall stacked probabilities ----

x_axis <- c(-Inf,seq(-2,4,0.1),Inf)

test <- as_tibble(x = x_axis) %>% 
      mutate(v1.prob = predict(v1.fit,data.frame(x=x_axis)),
             v2.prob = predict(v2.fit,data.frame(x=x_axis)),
             v3.prob = predict(v3.fit,data.frame(x=x_axis))) %>% 
      dplyr::select(value,v1.prob,v2.prob,v3.prob) %>% 
      mutate(v1.prob = case_when(value > 2.45 ~ 0.025, value < -0.05 ~ 0, T ~ v1.prob), # that's fine
             v2.prob = case_when(value > 2.80 ~ 0.075, value < -0.05 ~ 0, T ~ v2.prob), # that's probably not fine, needs longer tail...
             v3.prob = case_when(value > 3.5 ~ 0.975, value < -0.05 ~ 0, T ~ v3.prob) # that's probably fine
             ) %>% 
      mutate(v0.prob = 1-(v1.prob + v2.prob + v3.prob)) %>% 
      mutate(total.p = v0.prob + v1.prob + v2.prob + v3.prob)

test %>% 
  pivot_longer(c(v0.prob,v1.prob,v2.prob,v3.prob), names_to = "v.prob", values_to = "values") %>% 
  ggplot(aes(x = value, y = values, fill = v.prob)) + geom_area(position = 'stack')

test %>% 
  pivot_longer(c(v0.prob,v1.prob,v2.prob,v3.prob), names_to = "v.prob", values_to = "values") %>% 
  ggplot(aes(x = value, y = values, fill = v.prob)) + geom_line()


save(v1.fit, file = here("model-files","model-pr-adjustments","v1_fit.rdata"))
save(v2.fit, file = here("model-files","model-pr-adjustments","v2_fit.rdata"))
save(v3.fit, file = here("model-files","model-pr-adjustments","v3_fit.rdata"))


# Maybe I just need the 3 vote pdf.