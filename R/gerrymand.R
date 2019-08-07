load("~/gerrymandr.RData")

##

library(tidyverse)
library(magrittr)

##

elections <- data.table::fread("e6311_pre1948.csv", check.names = TRUE)
elections <- data.table::fread("e6311_post1948.csv", check.names = TRUE)

##

library(data.table)
library(tidyverse)
library(wkb)
library(sf)

##

length(unique(elections$year))
unique(elections$year)[2]

##

simpler <- tibble()

for (i in 1:length(unique(elections$year))){
  
  session <- unique(elections$year)[i]
  
  iteration <-
    elections %>%
    filter(year == session) %>%
    mutate(wkb = hex2raw(wkb)) %>%
    rename(geometry = wkb) %>%
    st_as_sf()
  
  simpler <- bind_rows(simpler, iteration)
  
}

##

glimpse(simpler)

##

onehalf <- simpler
twohalf <- simpler

##

glimpse(onehalf)
glimpse(twohalf)

##

both <-
  bind_rows(onehalf %>%
              select(-inc, -inc.1) %>%
              mutate(ending = str_sub(year, 4, 4)) %>%
              filter(ending == "2"),
            twohalf %>%
              select(-inc, -inc.1) %>%
              mutate(ending = str_sub(year, 4, 4)) %>%
              filter(ending == "2")) %>%
  st_as_sf()

##

both <- 
  both %>%
  group_by(year, geom_uid) %>%
  slice(1) %>%
  ungroup()

##

glimpse(both)

##

both <- 
  both %>%
  filter(year == 1952 & state_name == "alabama") %>%
  mutate(year = case_when(year == 1952 ~ 1962)) %>%
  rbind(both)


##

library(lwgeom)

##

pps <-
  both %>%
  st_set_crs(4326) %>%
  st_transform(102003) %>%
  mutate(peri = st_perimeter(geometry)) %>%
  mutate(area = st_area(geometry)) %>%
  mutate(pps = (area * (4 * pi)) / (peri ^ 2)) %>%
  mutate(pps = as.numeric(pps))

##

glimpse(pps)

##

library(classInt)
library(scales)

##

quant_pps <- 
  pps %>%
  use_series(pps) %>%
  classIntervals(n = 10, style = 'quantile') %>%
  use_series(brks)

##

lab <- c(round(rescale(quant_pps, to = c(0, 10)), 2)) %>% as_tibble() %>% pull()
pal <- read_csv("purples.txt", col_names = FALSE) %>% pull(X1)

##

guide_discrete <-
  guide_legend(direction = "vertical",
               keywidth = unit(1, units = "mm"),
               keyheight = unit(100 / length(lab), units = "mm"),
               title.position = 'right',
               label.position = 'left',
               title.hjust = 0.5,
               label.hjust = 1,
               ncol = 1,
               bycol = TRUE)

##

theme_map <- function () {
  theme_void() + 
    theme(plot.background = element_rect(fill = 'white', colour = 'white'),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.line.x = element_blank(),
          axis.line.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          plot.title = element_text(face = 'bold', colour = 'grey50'),
          plot.subtitle =  element_text(face = 'plain', colour = 'black', size = 15),
          plot.margin = margin(5, 5, 5, 5),
          strip.text = element_text(face = 'bold', colour = 'white'),
          panel.grid.major = element_line(size = NA), 
          panel.grid.minor = element_line(size = NA),
          legend.title = element_text(face = 'bold', angle = 270),
          legend.text = element_text(angle = 270)
    )
  
}

##

plot_data <- 
  pps %>% 
  filter(state_name != "alaska" & state_name != "hawaii") %>%
  mutate(group = factor(cut(as.numeric(pps), c(quant_pps))))

##

p <-
  ggplot(plot_data %>%
           filter(year == 1962)) +
  geom_sf(aes(fill = group, colour = group), size = 0) +
  scale_fill_manual(values = rev(pal),
                    labels = c(lab[1:10]),
                    breaks = unique(plot_data$group),
                    na.translate = FALSE,
                    name = "Polsby-Popper (rescaled)",
                    guide = guide_discrete) +
  scale_color_manual(values = rev(pal),
                     labels = c(lab[1:10]),
                     breaks = unique(plot_data$group),
                     na.translate = FALSE,
                     name = "Polsby-Popper (rescaled)",
                     guide = guide_discrete) +
  theme_map()

p

##

ggsave(p, "pass.png", height = 6, width = 8, dpi = 300)

##

library(magick)

##

glimpse(plot_data)

##

plot_simplified <- st_simplify(plot_data, 10000, preserveTopology = TRUE)
plot_simplified <- filter(plot_simplified, year != 1902)

##

img <- image_graph(800, 500, res = 300)

datalist <- split(plot_simplified, plot_simplified$year)

out <- lapply(datalist, function(data){
  p <-
    ggplot(data) +
    geom_sf(aes(fill = group, colour = group), size = 0) +
    scale_fill_manual(values = rev(pal),
                      labels = c(lab[1:10]),
                      na.translate = FALSE,
                      name = "Polsby-Popper (rescaled)",
                      guide = guide_discrete) +
    scale_color_manual(values = rev(pal),
                       labels = c(lab[1:10]),
                       na.translate = FALSE,
                       name = "Polsby-Popper (rescaled)",
                       guide = guide_discrete) +
    theme_map() + 
    ggtitle(data$year)
  
  print(p)
  
})

dev.off()

##

animation <- image_animate(img, fps = 1)
image_write(animation, "gerrymandr.gif")

##