library(readr)
library(dplyr)

by_tag_year <- read_csv(file = "by_tag_year.csv")
getwd()
print(by_tag_year)

# determining the fraction of questions that have a tag

by_tag_year_fraction <- mutate(by_tag_year, fraction = number/year_total)
print(by_tag_year_fraction)

# filtering for R tags

r_over_Time = filter(by_tag_year_fraction, tag == "r")
print(r_over_Time)

library(ggplot2)

r_over_time = ggplot(r_over_Time, aes(x = year, y = fraction)) + geom_line()
print(r_over_time)

# comparing three tags: R, dplyr, ggplot2

selected_tags <- c("r", "dplyr", "ggplot2")

selected_tags_over_time <- by_tag_year_fraction %>%
  filter(tag %in% selected_tags)

ggplot(selected_tags_over_time, aes(x = year,
                                    y = fraction,
                                    color = tag)) +geom_line()

sorted_tags <- by_tag_year %>% group_by(tag) %>% 
  summarize (tag_total = sum(number)) %>% arrange(desc(tag_total))

print(sorted_tags)

# filtering the largest tags and plotting in line plot
highest_tags <- head(sorted_tags$tag)

by_tag_sub<- by_tag_year_fraction %>% filter(tag %in% highest_tags)

ggplot(by_tag_sub, aes(x = year, y = fraction, color = tag)) + geom_line()

# filtering tags for android, ios, windows-phone
my_tags <- c("android", "ios", "windows-phone")

by_tag_subset <- by_tag_year_fraction %>%
  filter(tag %in% my_tags)

ggplot(by_tag_subset, aes(x = year,
                          y = fraction,
                          color = tag)) +
  geom_line()