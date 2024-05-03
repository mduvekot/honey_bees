# https://www.linkedin.com/posts/data-literacy_how-to-make-a-beeswarm-plot-in-rawgraphs-activity-7191851449209782273-wX9F/?utm_source=share&utm_medium=member_ios

library(colorspace)
library(ggbeeswarm)
library(ggtext)
library(readxl)
library(tidyverse)


# check that the bee data directory exists
if (!dir.exists("bee_data")) {
  dir.create("bee_data")
}

# download "https://dataliteracy.com/wp-content/uploads/2021/05/Stressors-on-Bee-Colonies-in-USA-2.xlsx" to our data directory
URL <- "https://dataliteracy.com/wp-content/uploads/2021/05/Stressors-on-Bee-Colonies-in-USA-2.xlsx"
download.file(URL, "bee_data/Stressors-on-Bee-Colonies-in-USA-2.xlsx", mode = "wb")

# read the data
df <- read_excel("bee_data/Stressors-on-Bee-Colonies-in-USA-2.xlsx") %>% 
  # rename the colum Other to "*Other"
  rename(`*Other` = Other) %>% 
  pivot_longer(
    cols = -State, 
    names_to = "Stressor", 
    values_to = "% of Colonies Affected"
  )
# order the stressors by their maximum value
stressor_levels <- df %>% 
  summarise(.by = Stressor, max = max(`% of Colonies Affected`)) %>% 
  arrange(desc(max)) %>% 
  pull(Stressor)

df <- df %>% 
  mutate(Stressor = factor(Stressor, levels = rev(stressor_levels)))  

# constants
phi = (1 + sqrt(5)) / 2
swarm_method = "compactswarm"
swarm_priority = "ascending"
swarm_corral = "omit"
swarm_cex = 2.5
swarm_width = 0.9

ggplot(df, aes(x = `% of Colonies Affected`, y = Stressor))+
  coord_cartesian(clip = "off")+
  labs(title = "Stressors on Honey Bee Colonies in the United States from April to June 2020",
       subtitle = "By state for colonies with five or more colonies. Varroa Mites are a persisent stressor across the US and can cause honey bee colonies to collapse by spreading viruses and feeding on the fat reserves of adults and larvae",
       caption = "&ast; Other include weather, starvation, insufficient forage, queen failure, hive damage/destoyed, etc. <br>Source: hhtps://usda.library.cornell.edu/concern/publications/rn301137d"
  )+
  geom_beeswarm(
    aes(
      color = `% of Colonies Affected`,
      fill = `% of Colonies Affected`),
    method = swarm_method,
    priority = swarm_priority,
    corral = swarm_corral, 
    cex = swarm_cex,
    side = 0,
    shape = 21,
    size = 6) +
  geom_text(
    aes(label = State),
    family = "Source Sans Pro",
    position = position_beeswarm(
      method = swarm_method,
      priority = swarm_priority,
      cex = swarm_cex,
      corral = swarm_corral,
      corral.width  = swarm_width
    ),
    size = 2, hjust = 0.5, color = "black"
  )+
  # this is silly
  # geom_text(
  #   aes(),
  #   label = "🐝",
  #   position = position_beeswarm(
  #     method = swarm_method,
  #     priority = swarm_priority,
  #   ),
  #   size = 12/.pt, hjust = 0.5, vjust = 0.5, color = "black")+
  
  scale_color_gradient(
    low = lighten("#e4e3e2", amount = .4), 
    high = lighten("#f5a623", amount = 0.4)
  )+
  scale_fill_gradient(
    low= "#e4e3e2", high = "#f5a623"
  )+
  scale_x_continuous(
    expand = c(0, 0),
    limits = c(0, 70),
    labels = scales::label_percent(
      accuracy = 1, scale = 1, 
      suffix = c("%", "", "", "", "", "", "", "", "", "", "", "", "", "", "%", "", "", "", "", "")
    ), 
    breaks = seq(0, 100, 5))+
  guides(
    fill = guide_colorbar(
      title = "Percent of Colonies Affected",
      display = "gradient",
      draw.llim = TRUE,
      draw.ulim = TRUE),
    color = "none"
  )+
  theme_void()+
  theme(
    text = element_text(family = "Source Sans Pro", size = unit(10, "pt")),
    plot.margin = margin(12, 24, 12, 12, "pt"),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    # aspect.ratio = (1+5^(1/2))/2-1,
    #aspect.ratio = 1/phi,
    
    plot.title = element_textbox_simple(margin = margin(0, 0, 12, 0, "pt"), face = "bold", size = rel(1.6)),
    plot.subtitle = element_textbox_simple(margin = margin(0, 0, 12, 0, "pt"), size = rel(1.0)),
    plot.caption = element_textbox_simple( margin = margin(12, 0, 0, 0, "pt"),  hjust = 0),
    axis.text.y.left = element_text( margin = margin(0, 12, 0, 0, "pt"), size = rel(1), hjust = 1),
    axis.title.x.bottom = element_text(margin = margin(2, 0, 0, 0, "pt"), size = rel(1), hjust = 0),
    axis.text.x.bottom = element_text( margin = margin(2, 0, 0, 0, "pt"), size = rel(1), hjust = 0),
    axis.line.x.bottom = element_line(color = "#7f7f7f"),
    axis.ticks.x.bottom = element_line(color = "#7f7f7f"),
    axis.ticks.length.x = unit(2, "pt"),
    plot.background = element_rect(fill = "#fefefe"),
    legend.position = c(1,0.025),
    legend.justification = c(1,0),
    legend.box.just = "top",
    legend.direction = "horizontal",
    legend.key.height = unit(0.5, "line"),
    legend.text = element_text(size = rel(.8)),
    legend.title = element_text(size = rel(.8)),
    legend.title.position = "top",
    legend.ticks = element_blank(),
    panel.grid.major.y = element_line(color = "#dfdfdf"),
  )

ggsave("honey_bees.png", 
       plot = last_plot(),
       width = 8, 
       height = 8/phi,
       units = "in",
       dpi = 150,
       bg = "#ffffff")

browseURL("honey_bees.png")




