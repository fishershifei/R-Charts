library(tidyverse)
library(s)


lifetime_earn <- readr::read_csv(url('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/lifetime_earn.csv'))
student_debt <- readr::read_csv(url('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/student_debt.csv'))
retirement <- readr::read_csv(url('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/retirement.csv'))
home_owner <- readr::read_csv(url('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/home_owner.csv'))
race_wealth <- readr::read_csv(url('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/race_wealth.csv'))
income_time <- readr::read_csv(url('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/income_time.csv'))
income_limits <- readr::read_csv(url('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/income_limits.csv'))
income_aggregate <- readr::read_csv(url('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/income_aggregate.csv'))
income_distribution <- readr::read_csv(url('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/income_distribution.csv'))
income_mean <- readr::read_csv(url('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/income_mean.csv'))



home_owner %>% 
  ggplot(aes(x=year,y=home_owner_pct)) + 
  geom_line(aes(color = race)) +
  geom_vline(xintercept = 2008,color="red")


income_aggregate %>%
  pivot_wider(names_from = "income_quintile",values_from = "income_share") %>% view()

income_aggregate %>%
  ggplot(aes(x=year, y=income_share)) +
  geom_line(aes(color=income_quintile)) +
  facet_wrap(~race)

# Explor the data
lifetime_earn %>%
  ggplot(aes(x=lifetime_earn, y=race, fill=gender)) +
  geom_col(position = "dodge") +
  scale_x_continuous(labels = dollar)


student_debt %>%
  mutate(race = fct_reorder(race, -loan_debt_pct)) %>%
  ggplot(aes(x=year, y=loan_debt,color=race)) +
  geom_line() +
  expand_limits(y=0) +
  scale_y_continuous(labels = dollar) +
  labs(y= "Average loan debt")

student_debt %>%
  mutate(race = fct_reorder(race, -loan_debt_pct)) %>%
  ggplot(aes(x=year, y=loan_debt_pct,color=race)) +
  geom_line() +
  expand_limits(y=0) +
  scale_y_continuous(labels = percent) +
  labs(y= "% of families with student load debt")


retirement %>%
  mutate(race = fct_reorder(race, -retirement)) %>%
  ggplot(aes(x=year, y=retirement,color=race)) +
  geom_line() +
  expand_limits(y=0) +
  scale_y_continuous(labels = dollar) +
  labs(y= "Average family liquid retirement saving")



home_owner %>%
  mutate(race = fct_reorder(race, -home_owner_pct)) %>%
  ggplot(aes(x=year, y=home_owner_pct,color=race)) +
  geom_line() +
  expand_limits(y=0) +
  scale_y_continuous(labels = percent) +
  labs(y= "% of home ownership for families")


# similar syntext, we'll use a function
plot_by_race <- function(data, column, labels = dollar){
  data %>%
    mutate(race = fct_reorder(race, -{{column}})) %>%
    ggplot(aes(x=year, y={{column}},color=race)) +
    geom_line() +
    expand_limits(y=0) +
    scale_y_continuous(labels = labels)
}

student_debt %>%
  plot_by_race(loan_debt,labels = dollar) +
  labs(y= "Average loan debt")


student_debt %>%
  plot_by_race(loan_debt_pct,labels= percent) +
  labs(y= "Average loan debt")

retirement %>%
  plot_by_race(retirement, labels =dollar) +
  labs(y= "Average family liquid retirement saving")



home_owner %>%
  plot_by_race(home_owner_pct, labels = percent) +
  labs(y= "% of home ownership for families")

# re-write the function, to extend the inputs
plot_by_race <- function(data, column, labels = dollar,...){
  last_year <- data %>%
    group_by(race) %>%
    top_n(1, year)
  
  data %>%
    mutate(race = fct_reorder(race, -{{column}}, last)) %>%
    ggplot(aes(x=year, y={{column}},color=race,...)) +
    geom_line() +
    geom_text(aes(label=race,color= NULL),
              hjust=0,data=last_year) +
    expand_limits(y=0,
                  x=2020) +
    scale_y_continuous(labels = labels)+
    labs(x="Year",
         color="Race") +
    theme(legend.position = "none")
}

student_debt %>%
  plot_by_race(loan_debt,labels = dollar) +
  labs(y= "Average loan debt")


student_debt %>%
  plot_by_race(loan_debt_pct,labels= percent) +
  labs(y= "Average loan debt")

retirement %>%
  plot_by_race(retirement, labels =dollar) +
  labs(y= "Average family liquid retirement saving")



home_owner %>%
  plot_by_race(home_owner_pct, labels = percent) +
  labs(y= "% of home ownership for families")


race_wealth %>%
  plot_by_race(wealth_family,lty=type)

race_wealth %>%
  plot_by_race(wealth_family) +
  expand_limits(x=2025)+
  facet_wrap(~type, scales = "free_y")

income_time %>%
  mutate(percentile = fct_reorder(percentile, -income_family, last)) %>%
  ggplot(aes(x=year, y=income_family,color=percentile)) +
  geom_line() +
  labs(y="Family level income",
       x = "Year") +
  expand_limits(y=0) +
  scale_y_continuous(labels=dollar)


income_time %>%
  spread(percentile, income_family) %>%
  ggplot(aes(x=year, y=`50th`, ymin = `10th`, ymax = `90th`)) +
  geom_ribbon(alpha = 0.25) +
  geom_line() +
  labs(y="Family level income (median with 10th and 90th percentiles)",
       x = "Year") +
  expand_limits(y=0) +
  scale_y_continuous(labels=dollar)

#不幸的是，电脑上装不了这个包
library(plotly)
p <- income_limits %>%
  filter(dollar_type =="2019 Dollars",
         !str_detect(race,"or in Combination")) %>%
  distinct(race, year, income_quintile,.keep_all = TRUE) %>%
  mutate(income_quintile =fct_reorder(income_quintile,-income_dollars)) %>%
  ggplot(aes(year,income_dollars, color=income_quintile)) +
  geom_line() +
  facet_wrap(~race) +
  scale_y_continuous(labels=dollar) +
  labs(y="Income quintile",
       color = "")

ggplotly(p)

income_aggregate %>% 
  group_by(year,race) %>%
  filter(income_quintile !="Top 5%",
        !str_detect(race,"Combination")) %>%
  mutate(income_share = income_share/100,
         income_quintile = fct_inorder(income_quintile)) %>%
  ggplot(aes(year, income_share, fill = income_quintile)) +
  geom_area() +
  facet_wrap(~race) +
  scale_y_continuous(labels= percent) +
  labs(y = "% share of income")


income_distribution %>% 
  group_by(year,race) %>%
  filter(!str_detect(race,"Combination")) %>%
  mutate(income_distribution = income_distribution/100,
         income_bracket = fct_inorder(income_bracket)) %>%
  ggplot(aes(year, income_distribution, fill = income_bracket)) +
  geom_area() +
  facet_wrap(~race) +
  scale_y_continuous(labels= percent) +
  labs(y = "% income distribution")
