# Code to generate base layer of fertile hex sticker

# To finish the sticker, use a separate image processing
# program to add "Rproj-logo.png" as a second layer on top
# of the sticker (centered on top of the copier)

library(hexSticker)
sticker("data-raw/copier.png",
        package="fertile",
        p_size=12,
        s_x=1,
        s_y=.75,
        s_width=.4,
        filename="data-raw/fertile-hex.png",
        h_fill="#F9A602",
        h_color="#000000")
