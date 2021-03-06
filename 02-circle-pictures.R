# https://www.youtube.com/watch?v=hkCakDslpXM
# https://www.youtube.com/watch?v=qhbuKbxJsk8

outfolder <- "02-circle-pictures/"
if (!dir.exists(outfolder)) dir.create(outfolder)

library(tidyverse)
library(sf)

circle <- st_buffer(st_point(c(0, 0)), 10)

mandala <- function(circle, i = 2, s = 1) {
  
  if (i %% 1 != 0 | i <= 0) stop("i must be a positive <<integer>>", call. = FALSE)
  
  many_circles <- circle %>% 
    st_sfc() %>% 
    st_cast("POINT") %>%
    st_buffer(10 / s)
  
  index <- seq(1, length(many_circles), i)[-1]
  
  many_circles[index] %>% 
    map(pluck, 1) %>% 
    list() %>% 
    st_multipolygon()
  
}

draw_circle_mandala <- function(circle, i = 3, s = 1) {
  
  ggplot() + 
    geom_sf(data = mandala(circle, i, s), fill = "#424146", color = "antiquewhite", size = 0.2) +
    geom_sf(data = circle * 0.5, fill = "#424146", color = NA, alpha = 0.8) +
    theme_void() + 
    theme(panel.background = element_rect(fill = "antiquewhite", color = "antiquewhite"))
  
}


# example -----------------------------------------------------------------

draw_circle_mandala(circle, i = 3, s = 1)
ggsave(str_glue("{outfolder}circle-mandala.png"), device = "png", dpi = "print", bg = "antiquewhite")

# temporary directory -----------------------------------------------------

temp <- tempfile()
dir.create(temp)

# sequence of plots -------------------------------------------------------

s_seq <- c(seq(1, 14, 0.5), seq(13.5, 1.5, -0.5))
i_seq <- c(seq(3, 30, 1), seq(29, 3, -1))

gg_list1 <- map(s_seq, draw_circle_mandala, circle = circle, i = 3)
gg_list2 <- map(i_seq, draw_circle_mandala, circle = circle, s = 1) 

# gifs --------------------------------------------------------------------

paths1 <- file.path(temp, paste0("pic-", seq_along(gg_list1), ".png"))
walk2(paths1, gg_list1, ggsave, device = "png", bg = "antiquewhite", 
      width = 5, height = 5)

library(magick)  

paths1 %>% 
  map(image_read) %>% 
  image_join() %>% 
  image_animate(fps = 5) %>%
  image_write(str_glue("{outfolder}circle-mandala-1.gif"))

paths2 <- file.path(temp, paste0("pic-", seq_along(gg_list2), ".png"))
walk2(paths2, gg_list2, ggsave, device = "png", bg = "antiquewhite", 
      width = 5, height = 5)

paths2 %>% 
  map(image_read) %>% 
  image_join() %>% 
  image_animate(fps = 5) %>%
  image_write(str_glue("{outfolder}circle-mandala-2.gif"))

i_seq <- c(seq(1, 20, 1), seq(20, 1, -1))
s_seq <- c(seq(1, 0.5, length = 20), seq(0.5, 1, length = 20))

gg_list3 <- map2(i_seq, s_seq, draw_circle_mandala, circle = circle)

paths3 <- file.path(temp, paste0("pic-", seq_along(gg_list3), ".png"))
walk2(paths3, gg_list3, ggsave, device = "png", bg = "antiquewhite", 
      width = 5, height = 5)

paths3 %>% 
  map(image_read) %>% 
  image_join() %>% 
  image_animate(fps = 5) %>%
  image_write(str_glue("{outfolder}circle-mandala-3.gif"))


