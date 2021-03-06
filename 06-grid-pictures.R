
# Sources of inspiration:
# https://infrahumano.github.io/exterior/2020/04/04/noche.html
# https://r-spatial.github.io/sf/articles/sf1.html

library(tidyverse)
library(sf)

outfolder <- "06-grid-pictures/"
if (!dir.exists(outfolder)) dir.create(outfolder)

coin_toss <- function() {
  sample(c(TRUE, FALSE), size = 1, prob = c(0.5, 0.5))
}

N <- 200
set.seed(123)
output <- map(seq(0, N + 1), function(j) {
  v_sequence <- if (coin_toss()) seq(0, N, 2) else seq(1, N, 2)
  h_sequence <- if (coin_toss()) seq(0, N, 2) else seq(1, N, 2)
  v_lines <- purrr::map(v_sequence, function(i) matrix(c(j, j, i, i + 1), nrow = 2))  
  h_lines <- purrr::map(h_sequence, function(i) matrix(c(i, i + 1, j, j), nrow = 2))
  return(sf::st_multilinestring(append(v_lines, h_lines)))
}) %>% 
  tibble::enframe() %>% 
  sf::st_as_sf()

ggplot() + 
  geom_sf(data = output, color = "steelblue") +
  theme_void()

ggsave(str_glue("{outfolder}pic-1.png"), device = "png", dpi = "print")

chunks <- output$value %>% st_union() %>% st_polygonize() %>% st_cast()

ggplot() + 
  geom_sf(data = output, color = "steelblue") +
  geom_sf(data = chunks, fill = "steelblue", color = "steelblue") +
  theme_void() + 
  theme(plot.background = element_rect(fill = "pink", color = "pink"))

ggsave(str_glue("{outfolder}pic-2.png"), device = "png", dpi = "print")

colors <- sample(flatten_chr(wesanderson::wes_palettes), size = length(chunks), replace = TRUE)
chunks_and_colors <- tibble(chunks, colors) %>% sf::st_as_sf()

ggplot() + 
  geom_sf(data = chunks_and_colors, aes(fill = colors), 
          color = NA, show.legend = FALSE) +
  geom_sf(data = st_union(output), color = "white", size = 0.2, 
          lineend = "round", linejoin = "bevel") +
  theme_void() + 
  theme(plot.background = element_rect(fill = "pink", color = "pink"))

ggsave(str_glue("{outfolder}pic-3.png"), device = "png", dpi = 400, 
       bg = "pink") ## hidden ... argument


