library(dplyr, warn.conflicts = FALSE)
library(ggplot2)
library(farr)
library(lfe)       # For felm()
library(tidyr)     # For pivot_wider()
library(knitr)     # For kable()

sg_format <- "html"

test_scores

test_scores %>% 
  select(id, treat) %>%
  distinct() %>%
  group_by(id) %>%
  summarize(n_treat_vals = n()) %>%
  ungroup() %>%
  count(n_treat_vals)

test_scores %>%
  summarize(prop_treat = mean(treat))

test_scores %>% count(grade, post)

#to look at a plot of the data

test_scores %>%
  group_by(grade, treat) %>%
  summarize(score = mean(score),
            .groups = "drop") %>%
  ggplot(aes(x = grade, y = score, colour = treat)) +
  geom_line()



fm_dd <- lm(score ~ treat * post, data = test_scores,
            subset = grade %in% 6:7L)
summary(fm_dd)




fm_aux_size <- lm(size ~ big_n + cfo + lev + mtb + 
                    factor(fyear)  * (inv_at + I(d_sale - d_ar) + ppe),
                  data = comp, na.action = na.exclude)

fm_aux_ta <- lm(ta ~ big_n + cfo + lev + mtb + 
                  factor(fyear)  * (inv_at + I(d_sale - d_ar) + ppe),
                data = comp, na.action = na.exclude)

aux_data <- tibble(size = resid(fm_aux_size),
                   ta = resid(fm_aux_ta))
fm_aux <- lm(ta ~ size, data = aux_data)

# includes all data, along with a line of best fit and a smoothed curve of best fit. 

aux_data %>%
  filter(!is.na(size), !is.na(ta)) %>%
  ggplot(aes(x = size, y = ta)) + 
  geom_point() +
  geom_abline(color = "red") +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cs"))



#truncate the value of ta at −1 and +  1 and produce a second plot
aux_data %>%
  filter(!is.na(size), !is.na(ta)) %>%
  filter(abs(ta) < 1) %>%
  ggplot(aes(x = size, y = ta)) + 
  geom_point() +
  geom_abline(color = "red") +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cs"))

