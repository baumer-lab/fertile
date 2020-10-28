
# Code to generate 6 reproducibility badges

library(hexSticker)

# Badge 1: Good File Structure


sticker("data-raw/good-structure.png",
        package="Project Structure",
        p_size=5,
        s_x=1,
        s_y=.75,
        s_width=.5,
        h_fill="#f9690e",
        h_color="#f39c12",
        filename="data-raw/structure-badge.png")


# Badge 2: Documentation


sticker("data-raw/documentation.png",
        package="Documentation",
        p_size=5,
        s_x=1,
        s_y=.75,
        s_width=.4,
        h_fill="#2e856e",
        h_color="#5ca08e",
        filename="data-raw/documentation-badge.png")


# Badge 3: File Paths

sticker("data-raw/paths.png",
        package="File Paths",
        p_size=6,
        s_x=1,
        s_y=.75,
        s_width=.4,
        h_fill="#10a5f5",
        h_color="#0c71e0",
        filename="data-raw/paths-badge.png")


# Badge 4: Tidy Files


sticker("data-raw/tidy-files.png",
        package="Tidy Files",
        p_size=6,
        s_x=1,
        s_y=.75,
        s_width=.5,
        h_fill="#ffd700",
        h_color="#ffb404",
        filename="data-raw/tidy-badge.png")


# Badge 5: Randomness


sticker("data-raw/randomness.png",
        package="Randomness",
        p_size=6,
        s_x=1,
        s_y=.75,
        s_width=.6,
        h_fill="#b53737",
        h_color="#8e1600",
        filename="data-raw/randomness-badge.png")


# Badge 6: Style


sticker("data-raw/code-style.png",
        package="Code Style",
        p_size=6,
        s_x=1,
        s_y=0.8,
        s_width=.8,
        h_fill="#000000",
        h_color="#909090",
        filename="data-raw/style-badge.png")

