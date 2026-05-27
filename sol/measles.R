library(dplyr)
library(ggplot2)

# Read the data 
measles = readr::read_csv("https://raw.githubusercontent.com/holtzy/R-graph-gallery/refs/heads/master/measles_data_1928-2011.csv")

# Show the first few rows of the data
print(head(measles))

# Prepare the data: rename column and sort states alphabetically
mdf <- measles %>% 
  mutate(State = factor(State, levels = rev(sort(unique(State)))))

cols <- c(
  colorRampPalette(c("#e7f0fa", "#c9e2f6", "#95cbee", "#0099dc", "#4ab04a", "#ffd73e", "#eec73a", "#e29421"))(20),
  colorRampPalette(c("#e29421", "#f05336", "#ce472e"))(80)
)

ggplot(mdf, aes(x = Year, y = State, fill = value)) + 
  geom_tile(colour = "white", linewidth = 0.5, 
            width = .9, height = .9) + 
  theme_minimal() +
  scale_fill_gradientn(colours = cols,
                       limits = c(0, 4000),
                       breaks = seq(0, 4000, by = 1000),
                       labels = c("0k", "1k", "2k", "3k", "4k"),
                       na.value = rgb(246/255, 246/255, 246/255),
                       guide = guide_colourbar(ticks = TRUE, 
                                               nbin = 50,
                                               barheight = .5, 
                                               label = TRUE, 
                                               barwidth = 10,
                                               title = "Cases per million",
                                               title.position = "top",
                                               title.hjust = 0.5)) +
  scale_x_continuous(expand = c(0,0), 
                     breaks = seq(1930, 2010, by = 10),
                     limits = c(1928, 2012)) +
  geom_vline(xintercept = 1963, color = "black", size = 0.5) +
  theme(legend.position = c(.5, -.13),
        legend.direction = "horizontal",
        legend.text = element_text(colour = "grey20"),
        plot.margin = grid::unit(c(.5,.5,1.5,.5), "cm"),
        axis.text.y = element_text(size = 6, family = "Helvetica", 
                                   hjust = 1),
        axis.text.x = element_text(size = 8),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank(),
        title = element_text(hjust = -.07, face = "bold", vjust = 1, 
                             family = "Helvetica"),
        text = element_text(family = "Helvetica")) +
  ggtitle("Measles cases per million by US state, 1928-2011") +
  annotate("text", label = "Vaccine introduced", x = 1963, y = 53, 
           vjust = 0.9, hjust = -0.1, size = I(3), family = "Helvetica") +
  xlab("") + ylab("")

