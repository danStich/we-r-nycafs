# Get some packages
library(tidyverse)
library(FSA)
library(nlstools)
library(plotrix)

# Read in some data
# Grass carp data 
grasscarp <- read.csv("data/grasscarp.csv",
                      stringsAsFactors = FALSE
                      )

# Length-frequency histograms
hist(grasscarp$tl)


# Back to the basics
ggplot(grasscarp, aes(tl)) +
  geom_histogram(binwidth = 10)

# Prettier?
ggplot(grasscarp, aes(tl)) +
  geom_histogram(binwidth = 10) +
  xlab("Total length (mm)") +
  ylab("Frequency") +
  scale_y_continuous(limits = c(0, 16), expand = c(0.01, 0)) + 
  scale_x_continuous(limits = c(0, 1300)) +
  theme_bw() +
  theme(
    axis.title.x = element_text(vjust = -1)
  )

# ?theme for options


# LF histograms in FSA
psdPlot(formula = ~tl, data = grasscarp, 
        species = "Grass Carp", w = 10)

psdPlot(formula = ~tl, data = grasscarp, 
        species = "Grass Carp", w = 10, psd.add = FALSE)


?psdPlot

psdVal()

# Calculate PSD
psdCalc(formula = ~ tl,
        data = grasscarp,
        species = "Grass Carp"
        )

# Tic-tac-toe
# Simulated Bass-Bluegill system
prey <- c(45, 30, 55)
pred <- c(25, 10, 37)

# ?tictactoe

tictactoe()
# plotCI is from plotrix package!
# Predator PSD
plotCI(prey[1], pred[1],
       li = prey[2], ui = prey[3],
       err = "x", add = TRUE
       )
# Prey PSD
plotCI(prey[1], pred[1],
       li = pred[2], ui = pred[3],
       err = "y", add = TRUE
       )


# Non-linear 
# Define von Bert 
vbmod <- tl ~ Linf * ( 1 - exp(-K * (age - t0)) )

# "Hard way"
starts <- vbStarts(formula = tl ~ age,
                   data = grasscarp
                   )
starts


starts$Linf

vb_fit1 <- nls(vbmod, data = grasscarp, start = starts)

# Summarize that model!
summary(vb_fit1)



# Let's grab a predefined growth function
vbo <- vbFuns("typical")

# Fit the model
vb_fit <- nls(tl ~ vbo(age, Linf, K, t0),
              data = grasscarp,
              start = starts
              )

# Summarize it
summary(vb_fit)

# Bootstrap this 
boot_fit <- nlsBoot(vb_fit)


# Make predict
boot_preds <- data.frame(
  predict(boot_fit, vbo, t = sort(unique(grasscarp$age))
  )
)

head(boot_preds)

names(boot_preds) <- c("age", "fit", "lwr", "upr")

grasscarp_preds <- merge(grasscarp, boot_preds, by = "age")

ggplot(grasscarp_preds, aes(x = age, y = tl)) +
  geom_jitter(width = 0.1, alpha = 0.20, size = 2) +
  geom_line(aes(y = fit)) + 
  geom_ribbon(
    aes(x = age, ymin = lwr, ymax = upr, color = NULL),
    alpha = 0.3
  ) +
  theme_bw() + 
  xlab("Age") +
  ylab("Total length (mm)")
  
  














