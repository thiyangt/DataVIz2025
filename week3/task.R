library(elephants)
data(elephants)
View(elephants)

library(ggplot2)
ggplot(data = elephants, 
       aes(x=Age_Category)) + 
  geom_bar() + 
  facet_wrap(vars(Gender))

ggplot(data = elephants, 
       aes(x=Age_Category,
           fill=Gender)) + 
  geom_bar() + 
  facet_wrap(vars(Gender))


library(ggplot2)
library(RColorBrewer)

# Assuming elephants is your data frame
# Create the bar chart with facets and apply the Dark2 color scale
ggplot(data = elephants, aes(x = Age_Category, fill = Age_Category)) + 
  geom_bar() + 
  facet_wrap(vars(Gender)) + 
  scale_fill_brewer(palette = "Dark2") + 
  theme_minimal() +
  labs(x = "Age Category", y = "Count", fill = "Age Category")


library(ggplot2)
library(dplyr)
library(scales)

## Attempt 1
# Assuming elephants is your data frame
# Create the percentage bar chart
ggplot(data = elephants, aes(x = Age_Category,
                             fill = Gender)) + 
  geom_bar(position = "fill") + 
  labs(y = "Percentage", x = "Age Category", 
       fill = "Gender") + 
  theme_minimal()

# Attempt 2
ggplot(data = elephants, aes(x = Age_Category, fill = Gender)) + 
  geom_bar(position = "fill") + 
 scale_y_continuous(labels = percent_format()) + 
  labs(y = "Percentage", x = "Age Category", fill = "Gender") + 
  theme_minimal()


## Attempt 3
library(ggplot2)
library(dplyr)
library(scales)

# Assuming elephants is your data frame
# Calculate the proportions for each category within each Age_Category and Gender
elephants_prop <- elephants %>%
  group_by(Age_Category, Gender) %>%
  summarise(count = n()) %>%
  mutate(prop = count / sum(count))

# Create the stacked percentage bar chart with labels
ggplot(data = elephants_prop, 
       aes(x = Age_Category, y = prop, 
           fill = Gender)) + 
  geom_bar(stat = "identity", position = "fill") + 
  scale_y_continuous(labels = percent_format()) + 
  labs(y = "Percentage", x = "Age Category",
       fill = "Gender") + 
  theme_minimal() +
  geom_text(aes(label = scales::percent(prop, 
                                        accuracy = 1), 
                y = prop), 
            position = position_fill(vjust = 0.5))

## Answers - students
elephants |> 
  count(Gender, Age_Category) |> 
  ggplot(aes(x = Gender, y = Age_category)) + 
  geom_tile(aes(fill = n))






## Task 2
library(drone)
data(worldbankdata)
g1 <- ggplot(worldbankdata, 
       aes(x=Cooking,
           y=Electricity,
           col=Region)) + 
  geom_point() + 
  facet_wrap(vars(Region), 
             scales = "free_y")

library(plotly)
ggplotly(g1)




















