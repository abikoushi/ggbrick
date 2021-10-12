library(ggplot2)
library(ggbrick)

ggplot(data = iris) +
  geom_brick(aes(y = Sepal.Length, x=Species), binwidth = 0.1)

ggplot(data = iris) +
  geom_brick(aes(y = Sepal.Length, x=Species), binwidth = 0.5)

ggplot(data = iris) +
  geom_brick(aes(y = Sepal.Length, x=Species), binwidth = 0.5, fill = "black")

ggplot(data = mpg,aes(y = cty, x=factor(year), fill=factor(cyl))) +
  geom_brick(binwidth = 1)

ggplot(data = mpg,aes(y = cty, x=factor(year), fill=factor(cyl))) +
  geom_brick(binwidth = 1, stackgroups = FALSE, alpha = 0.5)

ggplot(data = mpg,aes(y = cty, x=factor(year), fill=factor(cyl))) +
  geom_brick(binwidth = 1, stackgroups = FALSE, alpha = 0.5,
             stackdir = "centerwhole", position = position_dodge(0.5))

ggplot(data = diamonds, aes(x = color, y=carat, colour=cut)) +
  geom_brick(binwidth=0.2) +
  coord_flip()

ggplot(data = iris,aes(y = Sepal.Length, x=Species)) +
  geom_brick(binwidth = 0.1, stackdir = "centerwhole")+
  stat_summary(fun.y = median, fun.ymin = median, fun.ymax = median,
               geom = "crossbar")

