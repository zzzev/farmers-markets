library(tidyverse)
library(ggplot2)
library(statebins)
library(showtext)

showtext.auto()
font_add_google("Lato", "lato")
font_add_google("Lato", "latol", regular.wt = 100)
theme_update(text = element_text(family = "lato"))

data <- read_csv("farmers-markets.csv", guess_max = 3000)

process_logicals <- function(df) df %>%
  gather("key", "value", -1) %>%
  mutate(value = map_lgl(value, ~ .x == "Y"))

# Cast Y/N fields to logical
payment_data <- data %>%
  select(1, 24:28) %>%
  process_logicals

food_data <- data %>%
  select(1, 29:58) %>%
  process_logicals

fms_without_data <- food_data %>%
  group_by(FMID) %>%
  summarise(n = sum(value, na.rm = TRUE)) %>%
  filter(n == 0)

ordered_foods <- food_data %>%
  group_by(key) %>%
  summarise(n = sum(value, na.rm = TRUE)) %>%
  arrange(desc(n)) %>%
  pull(key)

food_data <- food_data %>%
  spread(2, 3)

data %>%
  anti_join(fms_without_data) %>%
  filter(!(State %in% c("Puerto Rico", "Virgin Islands"))) %>%
  select(-c(24:58)) %>%
  left_join(food_data) %>%
  select(FMID, State, 25:54) %>%
  gather("food", "value", -c(1, 2)) %>%
  group_by(State, food) %>%
  summarise(n = sum(value, na.rm = TRUE),
            in_state = n(),
            p = n / in_state) %>%
  ggplot(aes(fct_relevel(food, ordered_foods), State, fill = p)) +
    geom_tile() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))

data %>%
  anti_join(fms_without_data) %>%
  filter(!(State %in% c("Puerto Rico",
                        "Virgin Islands",
                        "Hawaii",
                        "Alaska"))) %>%
  filter(x < -50) %>%
  select(-c(24:58)) %>%
  left_join(food_data) %>%
  select(x, y, colnames(food_data)) %>%
  gather("food", "value", -c(1:3)) %>%
  filter(value) %>%
  ggplot(aes(x, y)) +
    geom_point(alpha = 0.05, size = 1, shape = 20) +
    coord_map() +
    theme_void() +
    facet_wrap(vars(food))

per_state_foods <- data %>%
  anti_join(fms_without_data) %>%
  filter(x < -50) %>%
  select(-c(24:58)) %>%
  left_join(food_data) %>%
  select(State, colnames(food_data)) %>%
  gather("food", "value", -c(1:3)) %>%
  group_by(State, food) %>%
  summarise(n = sum(value, na.rm = TRUE),
            p = n / n())

food_whitelist <- per_state_foods %>%
  group_by(food) %>%
  summarise(iqr = IQR(p)) %>%
  arrange(desc(iqr)) %>%
  pull(food)

per_state_foods %>%
  filter(food %in% c("Nuts", "Crafts", "Maple", "Seafood")) %>%
  ggplot(aes(state = State, fill = p)) +
    geom_statebins() +
    scale_fill_viridis_c(labels =
                           scales::number_format(scale = 100, suffix = "%")) +
    theme_statebins() +
    coord_fixed() +
    facet_wrap(vars(food), strip.position = "bottom") +
    theme(strip.background = element_blank(),
          strip.text = element_text(size = 14),
          panel.spacing = unit(0.2, "in"),
          plot.title = element_text(hjust = 0, size = 20,
                                    margin = margin(b = 30)),
          plot.subtitle = element_text(hjust = 0.5),
          plot.caption = element_text(hjust = 0.5, margin = margin(t = 40)),
          legend.position = c(1, 1),
          legend.background = element_blank(),
          legend.direction = "horizontal",
          legend.justification = c(1, 0),
          legend.title = element_text(margin = margin(b = 10, r = 4)),
          legend.margin = margin(b = 30)) +
    labs(title = "Regional Farmer's\nMarket Specialties",
         fill = "Farmer's Markets in State with Vendor",
         caption = "Data: USDA    -    Visualization by @zzzev")

ggsave("regional-farmers-specialties.png", width = 8, height = 8, dpi = 1080 / 8)
ggsave("regional-farmers-specialties.pdf", width = 8, height = 8)
