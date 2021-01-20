
## -------------------------------------------
library(tidyverse)



## --------------------------------------------------------------------------
otsego <- read.csv(file = "data/physical.csv", header = TRUE)



## ---- eval = FALSE---------------------------------------------------------
## ls()
## 


## --------------------------------------------------------------------------
# Insert code for this here
# This is a reminder for me to have people
# do this, not a leftover reminder to put code here.
# Do it. I'll demonstrate, don't worry!



## ---- message = FALSE------------------------------------------------------
# This will do it all at once!
ann <- # Front-end object assignment
  otsego %>% # Pass otsego to the group_by function
  group_by(year) %>% # Group by year and pass to summarize
  summarize(avg = mean(do_mgl))


## ---------------------------------------------
tempdf <- group_by(otsego, year) # Create data frame to hold grouped data
ann <- summarize( tempdf, avg = mean(do_mgl) ) # Summarize mean do_mgl by year



## ---- message = FALSE------------------------------------------------------
ann <- # Front-end object assignment
  otsego %>% # Pass otsego to the group_by function
  group_by(year) %>% # Group by year and pass to summarize
  summarize(avg = mean(do_mgl, na.rm = TRUE))


## ---------------------------------------------------------------
## which(is.na(...))


## ---------------------------------------------
bp <- ggplot(otsego, aes(x = year, y = do_mgl)) +
  geom_boxplot(aes(group = year))

print(bp)


## ---------------------------------------------
sp <- ggplot(otsego, aes(x = depth, y = do_mgl)) +
  geom_point()

print(sp)


## --------------------------------------------------------------------------
hypo <- # Front-end object assignment
  otsego %>% # Pass otsego to the subset function
  subset(depth > 40 & month == 10) # Select depths > 40 m in October


## ---------------------------------------------
ggplot(hypo, aes(x = year, y = do_mgl)) +
  geom_boxplot(mapping = aes(group = year))



## --------------------------------------------------------------------------
hypo <- hypo %>%
  subset(year > 1988)



## --------------------------------------------------------------------------
hypo$alewife <- "present"


## --------------------------------------------------------------------------
hypo$alewife[hypo$year > 2010] <- "absent"



## --------------------------------------------------------------------------
# Insert code for this here
# This is a reminder for me to have people
# do this, not a leftover reminder to put code here.
# Do it. I'll demonstrate, don't worry!



## ---------------------------------------------
ggplot(hypo, aes(x = alewife, y = do_mgl, fill = alewife)) +
  geom_boxplot()



## ---------------------------------------------
ggplot(hypo, aes(x = do_mgl, fill = alewife)) +
  geom_histogram()



## ---------------------------------------------
ggplot(hypo, aes(x = alewife, y = do_mgl, fill = alewife)) +
  geom_violin()



## ---------------------------------------------
ggplot(hypo, 
       aes(x = alewife, y = do_mgl, color = alewife, fill = alewife)) +
  geom_violin(alpha = 0.10) +
  geom_jitter(alpha = 0.20)



## ---------------------------------------------
ggplot(hypo, 
       aes(x = alewife, y = do_mgl, color = alewife, fill = alewife)) +
  geom_violin(alpha = 0.10, draw_quantiles = 0.50) +
  geom_jitter(alpha = 0.20)



## ---------------------------------------------
ggplot(hypo, 
       aes(x = alewife, y = do_mgl, color = alewife, fill = alewife)) +
  geom_violin(alpha = 0.10, draw_quantiles = 0.50) +
  geom_jitter(alpha = 0.20) +
  scale_x_discrete(labels = c("Absent", "Present")) +
  xlab("Alewife presence or absence") +
  ylab("Dissolved oxygen (mg/l)") +
  labs(fill = "Alewife", color = "Alewife") +
  theme(axis.title.x = element_text(vjust = -1),
        axis.title.y = element_text(vjust = 3)
        )



## ---------------------------------------------
ggplot(hypo, 
       aes(x = alewife, y = do_mgl, color = alewife, fill = alewife)) +
  geom_violin(alpha = 0.10, draw_quantiles = 0.50) +
  geom_jitter(alpha = 0.20) +
  scale_x_discrete(labels = c("Absent", "Present")) +
  xlab("Alewife presence or absence") +
  ylab("Dissolved oxygen (mg/l)") +
  labs(fill = "Alewife", color = "Alewife") +
  theme_bw() +
  theme(axis.title.x = element_text(vjust = -1),
        axis.title.y = element_text(vjust = 3),
        panel.grid = element_blank()
        )



## ---------------------------------------------
ggplot(hypo, 
       aes(x = alewife, y = do_mgl, color = alewife, fill = alewife)) +
  geom_violin(alpha = 0.10, draw_quantiles = 0.50) +
  geom_jitter(alpha = 0.20) +
  scale_x_discrete(labels = c("Absent", "Present")) +
  scale_fill_manual(values = c("gray40", "black")) +
  scale_color_manual(values = c("gray40", "black")) +
  xlab("Alewife presence or absence") +
  ylab("Dissolved oxygen (mg/l)") +
  labs(fill = "Alewife", color = "Alewife") +
  theme_bw() +
  theme(axis.title.x = element_text(vjust = -1),
        axis.title.y = element_text(vjust = 3),
        panel.grid = element_blank()
        )



## ---------------------------------------------
ggplot(hypo, 
       aes(x = alewife, y = do_mgl, color = alewife, fill = alewife)) +
  geom_boxplot(alpha = 0.10, width = 0.3) +
  geom_jitter(alpha = 0.20, width = .1) +
  scale_x_discrete(labels = c("Absent", "Present")) +
  scale_fill_manual(values = c("gray40", "black")) +
  scale_color_manual(values = c("gray40", "black")) +
  xlab("Alewife presence or absence") +
  ylab("Dissolved oxygen (mg/l)") +
  labs(fill = "Alewife", color = "Alewife") +
  theme_bw() +
  theme(axis.title.x = element_text(vjust = -1),
        axis.title.y = element_text(vjust = 3),
        panel.grid = element_blank()
        )



## ---------------------------------------------
ggplot(hypo, 
       aes(x = do_mgl, color = alewife, fill = alewife)) +
  geom_histogram(alpha = 0.20) +
  scale_fill_manual(values = c("gray40", "black")) +
  scale_color_manual(values = c("gray40", "black")) +
  ylab("Frequency of observation") +
  xlab("Dissolved oxygen (mg/l)") +
  labs(fill = "Alewife", color = "Alewife") +
  theme_bw() +
  theme(axis.title.x = element_text(vjust = -1),
        axis.title.y = element_text(vjust = 3),
        panel.grid = element_blank()
        )



## ---------------------------------------------
ggplot(hypo, 
       aes(x = do_mgl, color = alewife, fill = alewife)) +
  geom_histogram(alpha = 0.20) +
  scale_fill_manual(values = c("gray40", "black")) +
  scale_color_manual(values = c("gray40", "black")) +
  ylab("Frequency of observation") +
  xlab("Dissolved oxygen (mg/l)") +
  labs(fill = "Alewife", color = "Alewife") +
  facet_wrap(~alewife) +
  theme_bw() +
  theme(axis.title.x = element_text(vjust = -1),
        axis.title.y = element_text(vjust = 3),
        panel.grid = element_blank()
        )


