
# 1. Prefix ---------------------------------------------------------------

# libraries
library(tidyverse)
library(patchwork)
library(MetBrewer)

# data
dat <- tidytuesdayR::tt_load("2022-03-22")

# all datasets - only the ones used are displayed
# applicants <- dat$applicants
# births <- dat$births
# lifetables <- dat$lifetables
# maorinames <-  dat$maorinames
# nz_births <- dat$nz_births


# 2. Data cleaning --------------------------------------------------------

# check names for all datasets
lst(dat$babynames, dat$nz_names) %>% 
  map(~names(.x))

# add country column, harmonize names
babynames <- 
  dat$babynames %>% 
  add_column(country = "US")

nz_names <- 
  dat$nz_names %>% 
  janitor::clean_names() %>% 
  rename(n = count) %>% 
  add_column(country = "NZ")


# check names for all datasets
lst(babynames, nz_names) %>% 
  map(~names(.x))


# adding proportions per name
# to nz dataset
nz_births <- 
  dat$nz_births %>%
  drop_na() %>% 
  janitor::clean_names()

nz_names <- 
nz_names %>% 
  left_join(., select(nz_births, year, female, male)) %>% 
  mutate(prop = case_when(sex == "Female" ~ n/female, 
                          sex == "Male" ~ n/male, 
                          TRUE ~ NA_real_)) %>% 
  select(-female, -male)



# 3. Combine datasets -----------------------------------------------------


# bind both datasets together and filter only years with 
# data for both countries
both <- 
  bind_rows(babynames, nz_names) %>% 
  filter(year %in% pull(count(nz_births, year),1) &
           year %in% pull(count(babynames, year),1))

# create a subset with only the defined family names (and their assigned gender)
famnames <- 
  both  %>% 
  filter(str_detect(name, "Catherine$|Anouk$|Marius$|Jonas$|Noah$")) %>% 
  mutate(name = fct_relevel(as_factor(name), c("Catherine", "Anouk", "Marius", 
                                               "Jonas", "Noah")), 
         sex = as_factor(str_to_lower(sex)) %>% fct_recode(., 
                                                           "m" = "male", 
                                                           "f" = "female")) %>% 
  filter(!(sex == "m" & name == "Catherine"),
         !(sex == "m" & name == "Anouk"),
         !(sex == "f" & name == "Marius"),
         !(sex == "f" & name == "Jonas"),
         !(sex == "f" & name == "Noah"), 
         country == "US")

# for fun - catch the opposite combination of name/sex too
famnames_opp <- 
  both  %>% 
  filter(str_detect(name, "Catherine$|Anouk$|Marius$|Jonas$|Noah$")) %>% 
  mutate(name = fct_relevel(as_factor(name), c("Catherine", "Anouk", "Marius", 
                                               "Jonas", "Noah")), 
         sex = as_factor(str_to_lower(sex)) %>% fct_recode(., 
                                                           "m" = "male", 
                                                           "f" = "female")) %>% 
  filter(!(sex == "f" & name == "Catherine"),
         !(sex == "f" & name == "Anouk"),
         !(sex == "m" & name == "Marius"),
         !(sex == "m" & name == "Jonas"),
         !(sex == "m" & name == "Noah"))


# 4. Plotting -------------------------------------------------------------

# setting colours

# print(MetBrewer::met.brewer("Derain", 5))
# names_cols <- c("Catherine" = "#efc86e",
#                 "Anouk" = "#97c684",
#                 "Marius" = "#6f9969",
#                 "Jonas" = "#808fe1",
#                 "Noah" = "#5c66a8")

# define colour palette according to 
# rtist::oldenburg
names_cols <- c("Catherine" = "#95B1C9",
                "Anouk" = "#263656",
                "Marius" = "#698946",
                "Jonas" = "#F8D440",
                "Noah" = "#C82720")

# circular plot
circ_plot <- 
famnames %>% 
  group_by(year, name, country, sex) %>% 
  summarise(n = sum(n)) %>% 
  ggplot(., aes(x = year, y = n, fill = name)) +
  geom_col(position = "stack", color = "white") +
  scale_x_continuous(breaks = seq(1970, 2018, 2), 
                     labels = seq(1970, 2018, 2), 
                     name = NULL) +
  scale_y_continuous(name = NULL) +
  scale_fill_manual(values = names_cols) +
  # MetBrewer::scale_fill_met_d("Derain") +
  # paletteer::scale_fill_paletteer_d("rtist::oldenburg", direction = 1) +
  coord_polar(start = 0) +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(), 
        panel.grid.major.x = element_line(linetype = "dotted", 
                                          color = "grey60"), 
        legend.position = "none")
circ_plot


# ----
# two traditional barplots
# names appearing the most
n_by_year <- 
  famnames %>% 
    filter(name %in% c("Catherine", "Noah")) %>% 
    ggplot(., aes(x = year, y = n, fill = name)) +
    geom_col(position = "stack", color = "white") +
    scale_x_continuous(name = NULL, 
                       breaks = c(1970, 1982, 1994, 2006, 2017)) +
    scale_y_continuous(name = NULL, 
                       breaks = c(0, 20000),
                       sec.axis = dup_axis()) +
    scale_fill_manual(values = c(names_cols[1], names_cols[5])) +
    guides(fill = guide_legend(keywidth = 1, 
                               keyheight = 5)) +
    theme_minimal() +
    theme(panel.grid = element_blank(), 
          legend.position = "right", 
          legend.title = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x.top = element_blank()) +
  annotate("text", x = 2012, y = 20900, label = "n=21,415") + 
  annotate("curve", 
           x = 2012, xend = 2014.5, y = 21300, yend = 21470,
           curvature = -.5, arrow = arrow(length = unit(2, "mm")))
n_by_year


# names appearing the least
other_n_by_year <- 
  famnames %>% 
  filter(!(name %in% c("Catherine", "Noah"))) %>% 
  ggplot(., aes(x = year, y = -n, fill = name)) +
  geom_col(position = "stack", color = "white") +
  scale_x_continuous(name = NULL, 
                     breaks = c(1970, 1982, 1994, 2006, 2017),
                     sec.axis = dup_axis()) +
  scale_y_continuous(name = NULL,
                     breaks = c(0, 1300),
                     sec.axis = dup_axis(trans = ~.*-1)) +
  scale_fill_manual(name = "Name counts in US\n(1970 - 2017)",
                    values = names_cols[2:4]) +
  guides(fill = guide_legend(reverse = TRUE, 
                             keywidth = 1,
                             keyheight = 5)) +
  theme_minimal() +
  theme(panel.grid = element_blank(), 
        legend.position = "right", 
        legend.title = element_text(face = "bold.italic",
                                    hjust = .5,
                                    vjust = 1),
        axis.text.y = element_blank(),
        axis.text.x = element_blank()) +
  annotate("text", x = 2004, y = -1230, label = "n=1,340") +
  annotate("curve", 
           x = 2004, xend = 2007.5, y = -1270, yend = -1330,
           curvature = .5, arrow = arrow(length = unit(2, "mm")))
other_n_by_year


res <- 
(other_n_by_year +  
    plot_layout(widths = c(2,1), ncol = 1)) /
(n_by_year + 
   plot_layout(widths = c(2,1), ncol = 1)) +
  inset_element(circ_plot, 
                left = .5, 
                bottom = 1, 
                right = 0.1, 
                top = 1.1, 
                on_top = TRUE) +
  plot_layout(guides = "collect") +
  plot_annotation(title = NULL,
                  caption = "@cathblatter | #Tidytuesday | 2022-03-22")

res

# ggsave(filename = paste0("2022/w12/", lubridate::today(), "-tt-names_derain.png"),
#        device = "png",
#        width =  12,
#        height = 8)




# 5. Other plots ----------------------------------------------------------


