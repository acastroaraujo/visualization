
theme_custom <- function(base_family = "Avenir Next Condensed", fill = "white", ...) {
  theme_minimal(base_family = base_family, ...) %+replace% 
    theme(plot.title = element_text(face = "bold", margin = margin(0, 0, 5, 0), hjust = 0, size = 13), 
          plot.subtitle = element_text(face = "italic", margin = margin(0, 0, 5, 0), hjust = 0), 
          plot.background = element_rect(fill = fill, size = 0), 
          complete = TRUE, 
          axis.title.x = element_text(margin = margin(15, 0, 0, 0)), 
          axis.title.y = element_text(angle = 90, margin = margin(0, 20, 0, 0)), 
          strip.text = element_text(face = "italic", colour = "white"), 
          strip.background = element_rect(fill = "#4C4C4C"))
}

theme_set(theme_custom(fill = "#FFFCF9"))