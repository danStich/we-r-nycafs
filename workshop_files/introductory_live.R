# Introductory live session code ----
1 + 1

my_sum = 1 + 1
print(my_sum)
my_sum

letters
LETTERS

query <- letters == LETTERS
query


# These are vectors

# Atomic vectors
mixed = c(1, "a")
mixed

str(mixed)

data(iris)

# Fish stats ----
# This is how we read data into R
otsego <- read.csv(file = "data/physical.csv",
                   stringsAsFactors = FALSE
                   )

# Say hello to your data!
View(otsego)

# Descriptive stats
names(otsego)

# How to access a column?
do_conc = otsego$do_mgl
do_conc

head(do_conc, 10)

# Calculate mean?
avg = mean(otsego$do_mgl, na.rm = TRUE)
avg

sd(otsego$do_mgl, na.rm = TRUE)

hist(otsego$do_mgl)
boxplot(do_mgl ~ year, data = otsego)

# Let's do some tidy data
library(tidyverse)

# (1) Let's break it down. Get October > 40 m
hypo <- filter(otsego, 
               month == 10,
               depth >= 40
               )

# Have another look at the new data set
hist(hypo$do_mgl)
boxplot(do_mgl ~ year, data = hypo)

# (2) Make a grouped data set
hypo_grouped <- group_by(hypo, year)

# (3) Summarize the dataset to get averages by year
avgs = summarize(hypo_grouped,
          avg = mean(do_mgl, na.rm = T)
          )

# OR...you could do 1, 2, and 3 above all at once 
# with the "pipe" operator (%>%)
avgs <- otsego %>%
  filter(month ==10, depth >= 40) %>%
  group_by(year) %>%
  summarize(avg = mean(do_mgl, na.rm = TRUE))

plot(avgs)

# Let's make a nicer graph
ggplot(hypo_grouped, 
       aes(x = factor(year), y = do_mgl)) +
  geom_boxplot() +
  xlab("Years") +
  ylab("Dissolved oxygen (mg/L") +
  theme_bw() +
  theme(
    axis.title.y = element_text(vjust = 3)
  )

# Options?
?theme

# These options take some getting used to. Check out the variety
# of these plots and more on the w-r-nycafs website examples!
# https://danstich.github.io/we-r-nycafs
