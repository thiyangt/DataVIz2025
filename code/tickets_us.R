#Tickets issued for parking violations in the city of Philadelphia, 
#Pennsylvania in 2017

#tutorial: https://thisisdaryn.netlify.app/post/making-a-calendar-visualization-with-ggplot2/

library(tidyverse)

philly <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-03/tickets.csv")
View(philly)

## Getting total number of tickets
library(lubridate)

#The code below creates a variable 
# week_increment that is set to 1 for 
# each day that is either 
# the 1st of the month or a Sunday.

daily_totals <- philly |>
  mutate(issue_date = as_date(issue_datetime)) |>
  group_by(issue_date) |>
  summarise(Tickets = n(),
            Fines = sum(fine, na.rm = TRUE)) |>
  ungroup() |>
  mutate(wday = str_sub(weekdays(issue_date), 1, 3),
         month_day = day(issue_date),
         month = month(issue_date),
         week_increment = ifelse(month_day == 1 | wday == "Sun", 1, 0)) |>
  group_by(month) |> 
  mutate(week = cumsum(week_increment),
         text_month = months(issue_date)) |>
  ungroup()

## Setting factor levels 
wday_vec <- c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")
daily_totals$wday <- factor(daily_totals$wday, levels = wday_vec)
month_vec <- c("January", "February", "March", "April", "May", "June",
               "July", "August", "September", "October", "November", "December") 
daily_totals$text_month <- factor(daily_totals$text_month, 
                                  levels =  month_vec)


## Creating the plot
library(scales) # to get the dollar values formatted
library(plotly)

daily_totals <- daily_totals %>% 
  mutate(fine_txt = paste0("$",formatC(Fines, format="f", big.mark=",", digits = 0)),
                                        desc = paste(wday, text_month, month_day, 2017, "\n", fine_txt))


philly_calendar <- ggplot(daily_totals, 
                          aes(x = wday,
                              y = week,
                              text = desc)) + 
  geom_tile(aes(fill = Fines), colour = "white") +
  facet_wrap(~text_month, scales = "free") + 
  scale_y_reverse() + 
  theme_minimal() + 
  scale_fill_viridis_c(labels = dollar) + 
  scale_x_discrete(position = "top") + 
  ylab("") + xlab("") + labs(fill = "Fines Issued") + 
  ggtitle("Philadelphia Parking Violations (2017)") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        strip.text.x = element_text(
          size = 12, color = "black", face = "bold"), 
        plot.title = element_text(size = 14, hjust = 0.5))
philly_calendar
#ggplotly(philly_calendar, tooltip = "desc")

#----------------
## Emission data - Visualise Natural Gas production
emissions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-05-21/emissions.csv')
emissions_stat <- emissions |> 
  group_by(year, commodity)|>
  summarize(value = sum(production_value)) |>
  ungroup()

library(viridis)
graph_em <- emissions_stat |>
  ggplot(aes(year, value, group=commodity,
             fill=commodity,
             col=commodity)) +
  geom_area(alpha = 0.5) +
  geom_line(aes(x=year, y=value,
                group=commodity)) +
  scale_fill_viridis(discrete = TRUE,
                     option = "H")  +
  scale_color_viridis(discrete = TRUE,
                      option = "H") 
graph_em


graph_em2 <- emissions_stat |>
  ggplot(aes(year, value, 
             group=commodity,
             fill=commodity,
             color=commodity
)) +
  geom_area() +
  geom_line(aes(x=year, y=value,
                group=commodity)) 
graph_em2 + 
  scale_x_continuous(
    limits=c(1930, 2022)) +
  scale_fill_brewer(palette = "Set1")+
  scale_color_brewer(palette = "Set1")
