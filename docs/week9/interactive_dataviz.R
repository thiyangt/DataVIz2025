library(plotly)
library(tidyverse)
library(palmerpenguins)
library(plotly)
library(crosstalk)
library(htmltools)
library(DT)


# Prepare data
penguins_clean <- penguins %>%
  filter(!is.na(bill_length_mm),
         !is.na(flipper_length_mm),
         !is.na(body_mass_g)) %>%
  mutate(body_mass_kg = body_mass_g / 1000)

# turns your dataset into a linked interactive object
#So: Plot Filters (slider, dropdown, etc.) ALL talk to each other.

penguins_shared <- SharedData$new(penguins_clean)

# Plot
plot_penguins <- ggplot(penguins_shared,
                        aes(x = bill_length_mm,
                            y = flipper_length_mm)) +
  geom_point(aes(color = species,
                 size = body_mass_kg,
                 text = paste0(
                   "Species: ", species,
                   "<br>Island: ", island,
                   "<br>Body mass (g): ", body_mass_g,
                   "<br>Bill length: ", bill_length_mm,
                   "<br>Flipper length: ", flipper_length_mm
                 )),
             alpha = 0.6) +
  scale_size(range = c(1, 8), name = "Body mass (kg)") +
  theme_minimal()

# Interactive layout
# Creates a two-row layout:
bscols(
  widths = c(12, 12),
  filter_slider(    # Top	Bottom Slider
    "body_mass_g",
    "Body Mass (g)",
    penguins_shared,
    column = ~body_mass_g,
    step = 50,
    width = "100%"
  ),
  ggplotly(plot_penguins, tooltip = "text") # Interactive plot
)

####
bscols(
  filter_select("species", "Species", penguins_shared, ~species),
  filter_slider("body_mass_g", "Body Mass", penguins_shared, ~body_mass_g),
  ggplotly(plot_penguins)
)

## 2###########################
# Cross talk plot
devtools::install_github("rstudio/crosstalk")
library(crosstalk)
devtools::install_github("jcheng5/d3scatter")
library(d3scatter)
d3scatter(iris, ~Petal.Length, ~Petal.Width, ~Species)
shared_iris <- SharedData$new(iris)
bscols(
    d3scatter(shared_iris, ~Petal.Length, ~Petal.Width, ~Species, width="100%", height=300),
    d3scatter(shared_iris, ~Sepal.Length, ~Sepal.Width, ~Species, width="100%", height=300)
   )

## crosstalk
library(crosstalk)
# define a shared data object
d <- SharedData$new(mtcars)
# make a scatterplot of disp vs mpg
scatterplot <- plot_ly(d, x = ~mpg, y = ~disp) %>%
  add_markers(color = I("navy"))
# define two subplots: boxplot and scatterplot
subplot(
  # boxplot of disp
  plot_ly(d, y = ~disp) %>% 
    add_boxplot(name = "overall", 
                color = I("navy")),
  # scatterplot of disp vs mpg
  scatterplot, 
  shareY = TRUE, titleX = T) %>% 
  layout(dragmode = "select")

##############################
library(crosstalk)
library(leaflet)
library(DT)

# Wrap data frame in SharedData
sd <- SharedData$new(quakes[sample(nrow(quakes), 100),])

# Create a filter input
filter_slider("mag", "Magnitude", sd, column=~mag, step=0.1, width=250)

# Use SharedData like a dataframe with Crosstalk-enabled widgets
bscols(
  leaflet(sd) %>% addTiles() %>% addMarkers(),
  datatable(sd, extensions="Scroller", style="bootstrap", class="compact", width="100%",
            options=list(deferRender=TRUE, scrollY=300, scroller=TRUE))
)



