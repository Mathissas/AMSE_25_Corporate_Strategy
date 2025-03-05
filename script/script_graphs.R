# This script analyzes data on exported leather bags from France to every other 
# countries over the period 2013-2023.

library(tidyverse)
library(here)
library(rnaturalearth)
library(sf)
library(scales)
setwd(here())
data <- read_csv("export_data.csv")
world <- ne_countries(scale = "medium", returnclass = "sf")
world <- world %>%
  mutate(partnerISO = sov_a3)



data2 <- data %>%
  select(partnerISO, partnerDesc, primaryValue, refYear, primaryValue) %>%
  group_by(partnerISO) %>%
  arrange(refYear) %>%
  mutate(growth = (primaryValue - lag(primaryValue))/lag(primaryValue),
         Year = as.character(refYear)) %>%
  left_join(world, by = "partnerISO")
  
plot_export <- ggplot(data2 %>% filter(partnerDesc == "World"), aes(x = Year, y = primaryValue, group = 1)) +
  geom_bar(stat = "identity", fill = "orange") +
  scale_y_continuous(labels = dollar_format()) +
  theme_bw() +
  labs(title = "Value of French Exports of Lather Luxury Items", x = NULL, y = "Value of Global Exports") + 
  theme(
    text = element_text(color = "black", family = "serif"),
    axis.text = element_text(color = "black"),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "transparent", color = NA),  # Supprime le fond du panel
    plot.background = element_rect(fill = "transparent", color = NA)
  )

ggsave("plot_1.png", plot = plot_export, width = 5, height = 4, dpi = 300)


data3 <- data2 %>% filter(partnerDesc %in% c("China"))

plot_asia <- ggplot(data3, aes(x = Year, y = primaryValue, group = 1)) +
  geom_line(color = "orange") + 
  scale_y_continuous(labels = dollar_format()) +
  theme_bw() +
  labs(title = "Export Value of Lather Items to China", x = NULL, y = "Value of Exports") + 
  theme(
    text = element_text(color = "black", family = "serif"),
    axis.text = element_text(color = "black"),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA)
  )

ggsave("plot_2.png", plot = plot_asia, width = 5, height = 4, dpi = 300)


data4 <- data2 %>% filter(partnerDesc %in% c("India"))

india <- ggplot(data4, aes(x = Year, y = primaryValue, group = 1)) +
  geom_line(color = "orange") + 
  scale_y_continuous(labels = dollar_format()) +
  theme_bw() +
  labs(title = "Export Value of Lather Items to India", x = NULL, y = "Value of Exports") + 
  theme(
    text = element_text(color = "black", family = "serif"),
    axis.text = element_text(color = "black"),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA)
  )

data5 <- data2 %>% filter(partnerDesc %in% c("Brazil"))

brazil <- ggplot(data5, aes(x = Year, y = primaryValue, group = 1)) +
  geom_line(color = "orange") + 
  scale_y_continuous(labels = dollar_format()) +
  theme_bw() +
  labs(title = "Export Value of Lather Items to Brazil", x = NULL, y = "Value of Exports") + 
  theme(
    text = element_text(color = "black", family = "serif"),
    axis.text = element_text(color = "black"),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA)
  )


ggsave("plot_4.png", plot = india, width = 6, height = 3, dpi = 300)
ggsave("plot_5.png", plot = brazil, width = 6, height = 3, dpi = 300)

