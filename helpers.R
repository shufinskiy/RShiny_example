rollmean_func <- function(data) {
  data %>%
    mutate(E_OFF_RATING = rollmeanr(E_OFF_RATING, k = 10, fill = NA)) %>%
    mutate(E_DEF_RATING = rollmeanr(E_DEF_RATING, k = 10, fill = NA)) %>%
    mutate(E_NET_RATING = rollmeanr(E_NET_RATING, k = 10, fill = NA)) %>%
    na.omit()
}
