library(hexSticker)
library(magick)
library(magrittr)

img <- image_read(here::here("man", "figures", "lasso-img.jpg"))
res <- img %>%
  image_convert("png") %>%
  image_resize("1080 x 200") %>%
  image_fill(color = "none") %>%
  image_annotate("bolasso",
                 size = 10,
                 degrees = 35,
                 location = "+85+147", color="black")

res <- sticker(filename = here::here("man", "figures", "logo.png"),
               white_around_sticker = FALSE,
               res,
               package = "",
               s_x = 0.97,
               s_y = 1.04,
               s_width = 1.7,
               s_height = 14,
               h_fill = "#ffffff",
               h_color = "#A9A9A9")

save_sticker(sticker = res,
             filename = here::here("man", "figures", "logo.png"))
