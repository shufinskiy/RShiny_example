rollmean_func <- function(data, number) {
  data %>%
    mutate(E_OFF_RATING = round(rollmeanr(E_OFF_RATING, k = number, fill = NA), digits = 1)) %>%
    mutate(E_DEF_RATING = round(rollmeanr(E_DEF_RATING, k = number, fill = NA), digits = 1)) %>%
    mutate(E_NET_RATING = round(rollmeanr(E_NET_RATING, k = number, fill = NA), digits = 1)) %>%
    na.omit()
}

data_two_plots <- function(data){
  data %>%
    mutate(GAME_DATE = as.Date(GAME_DATE)) %>%
    select(-E_NET_RATING) %>%
    melt(., id.vars = 1:2, measure.vars = 3:4,
         variable.name = "RATING", value.name = "VALUE") %>%
    ggplot(., aes(GAME_DATE, VALUE, color = RATING)) +
    geom_line() +
    geom_point() +
    scale_color_viridis_d(end = 0.5, option = "D") +
    theme_tufte() +
    labs(title = paste0("Estimate offense and defense rating ", 
                        unique(data$TEAM_NAME), " in season 2018-19")) +
    theme(axis.title.x = element_blank(),
          plot.title = element_text(hjust = 0.5),
          legend.position = "bottom")
}
