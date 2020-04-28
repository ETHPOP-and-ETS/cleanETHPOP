
library(ggplot2)

out_ethpop_sex_year <-
  clean_outmigrants %>%
  group_by(sex, year) %>%
  summarise(pop = sum(outmigrants))

in_ethpop_sex_year <-
  clean_inmigrants %>%
  group_by(sex, year) %>%
  summarise(pop = sum(inmigrants))


# in- vs out-migration

ggplot(out_ethpop_sex_year, aes(x = year, y = pop, group = sex)) +
  geom_line() +
  theme_bw() +
  geom_line(data = in_ethpop_sex_year, aes(x = year, y = pop, colour = sex))


# by ethnic group ----

clean_outmigrants %>%
  group_by(sex, ETH.group, year) %>%
  summarise(pop = sum(outmigrants)) %>%
  filter( sex == "F") %>%
  ggplot(aes(x = year, y = pop, colour = ETH.group, group = ETH.group)) +
  geom_line() +
  theme_bw()

clean_inmigrants %>%
  group_by(sex, ETH.group, year) %>%
  summarise(pop = sum(inmigrants)) %>%
  filter( sex == "F") %>%
  ggplot(aes(x = year, y = pop, colour = ETH.group, group = ETH.group)) +
  geom_line() +
  theme_bw()
