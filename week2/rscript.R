library(ggplot2)
library(tidyverse)
library(palmerpenguins)

penguins |> ggplot(aes(x=species)) + 
  geom_bar()
penguins |> 
  ggplot(aes(x=species, fill=sex)) + 
  geom_bar()
penguins |> ggplot(aes(x=species, fill=sex)) + geom_bar(position = position_stack())
penguins |> ggplot(aes(x=species, fill=sex)) + geom_bar(position = position_dodge())
penguins |> ggplot(aes(x=species, fill=island)) + geom_bar(position = position_dodge())
penguins |> ggplot(aes(x=species, fill=sex)) + geom_bar(position = position_dodge())

gg <- ggplot(data=KS, aes(x=main_category, fill=state)) 
gg <- gg + geom_bar(position="fill")
gg <- gg + geom_text(aes(label = paste0(100*pcnt,"%"),y=labelpos),size = 3)
gg <- gg + scale_y_continuous(labels = scales::percent)
print(gg)

## scale
## Example 1
# Default scatter plot
sp <- ggplot(cars, aes(x = speed, y = dist)) +
  geom_point()
sp
# Log transformation using scale_xx()
# possible values for trans : 'log2', 'log10','sqrt'
sp + scale_x_continuous(trans='log2') +
  scale_y_continuous(trans='log2')




# Sqrt transformation
sp + scale_y_sqrt()
# Reverse coordinates
sp + scale_y_reverse() 


## Example 2

## default
ggplot(mpg, aes(displ, hwy)) + 
  geom_point(aes(colour = class))

## default
ggplot(mpg, aes(displ, hwy)) + 
  geom_point(aes(colour = class)) +
  scale_x_continuous() + 
  scale_y_continuous() + 
  scale_colour_discrete()

ggplot(mpg, aes(displ, hwy)) + 
  geom_point(aes(colour = class)) + 
  scale_x_continuous(name = "x axis label") +
  scale_y_continuous(name = "y axis label")

ggplot(mpg, aes(displ, hwy)) + 
  geom_point(aes(colour = class)) +
  scale_x_sqrt() + 
  scale_colour_brewer()

ggplot(mpg, aes(displ, hwy)) + 
  geom_point(aes(colour = class)) +
  scale_x_sqrt() + 
  scale_colour_brewer(type="qua", palette=3)


## Axis customization
ggplot(penguins, aes(x=bill_length_mm, 
                     y=body_mass_g)) +
  geom_point() 

ggplot(penguins, aes(x=bill_length_mm,
                     y=body_mass_g)) +
  geom_point() +
  scale_y_continuous(breaks = seq(0, 6000, 500),
                     labels = scales::comma) +
  scale_x_continuous(limits = c(0, 70))

## Label customization


ggplot(penguins, 
       aes(x=bill_length_mm, 
           y=body_mass_g,
           col=island)) +
  geom_point() +
  scale_color_manual(values = c("red", "blue",
                                "green"), 
                     name = "Groups", 
                     labels = c("Biscoe", 
                                "Dream",
                                "Torgersen"))
#Transformations  
p1 <- ggplot(penguins, 
       aes(x=bill_length_mm, 
           y=body_mass_g,
           col=island)) +
  geom_point()

p2 <- p1 + scale_y_log10()
p2

## Customize transformations

p1 + 
  scale_x_continuous(trans = scales::reciprocal_trans())
