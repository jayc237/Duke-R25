my_balance <- function(P, r, m, n) {
  P * (1 + r/n)^m
}

my_distance <- function(x1, x2, y1, y2){
  sqrt((x1 - x2)^2 + (y1 - y2)^2)
}

my_distance2 <- function(x, y){
  sqrt(sum((x - y)%>%(x - y)))
}

library(tidyverse)

mtcars %>% .$mpg %>% mean()

table3 %>% separate(rate, into = c("cases", "population"), sep = "/") %>%
  mutate(cases = as.integer(cases)) %>% mutate(population = as.integer(population))

table4a %>% gather(key = year, `1999`:`2000`, value = cases) -> table4a_fixed

table4b %>% gather(key = year, `1999`:`2000`, value = population) -> table4b_fixed


full_join(table4a_fixed, table4b_fixed, by = c("country","year"))

table2 %>% spread(type, count)

###############################################

diamonds %>% filter(carat > 2) %>% filter(cut == "Ideal" | cut == "Premium")

diamonds$cut %>% unique()

diamonds %>% select(cut) %>% unique()

diamonds %>% filter(cut!= "Fair")

diamonds %>% filter(str_detect(cut, "Good"))

diamonds %>% mutate(ifelse(carat > 2 & (cut == "Ideal" | cut == "Premium"), 1, 0))

diamonds %>% mutate(Class = ifelse(carat > 2 & (cut == "Ideal" | cut == "Premium"), 1, 0)) %>%
  filter(carat > 2 & cut == "Ideal") %>% sample_n(10)

diamonds %>% transmute(ifelse(carat > 2 & (cut == "Ideal" | cut == "Premium"), 1, 0))

diamonds %>% group_by(cut) %>% summarize_if(is.numeric, mean, na.rm=TRUE)

diamonds %>% group_by(cut) %>% summarize(price, mean, na.rm = TRUE)


diamonds %>% group_by(color) %>% summarise(n = n())