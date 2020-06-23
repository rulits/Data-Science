library(tidyverse)
papers <- as_tibble(read_csv("C:/Users/o/Documents/CitesforSara.csv"))
papers_select<-select(papers,journal, year, cites, title, au1)
count(filter(papers_select, cites >= 100))
papers2=group_by(papers_select,journal)
econometrica=filter(papers2, journal == 'Econometrica')
sum(econometrica$cites)
distinct_vector = papers_select$au1
x <- c(1, 5, 4, 9, 0)
successes<-rbinom(1000, 8, 0.2)
hist(successes)
dbinom(7, size=10, prob=0.65)
pbinom(7, size=10, prob=0.65)
1-pbinom(6, size=10, prob=0.65)+dbinom(6, size=10, prob=0.65)

binom_draws <- as_tibble(data.frame(successes))

estimated_pf <- binom_draws %>%
  group_by(______) %>%
  _____(n=n()) %>%
  mutate(freq=n/sum(______))

ggplot(estimated_pf, aes(x=successes, y=freq)) +
  geom_col() +
  ylab("Estimated Density")