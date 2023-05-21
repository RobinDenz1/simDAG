library(hexSticker)
library(ggplot2)
library(ggforce)
library(sysfonts)

## calculate line for arrows given circles
get_arrow_from_a_to_b <- function(a, b, data) {

  x0_vec <- data$x0
  names(x0_vec) <- data$label

  y0_vec <- data$y0
  names(y0_vec) <- data$label

  r_vec <- data$r
  names(r_vec) <- data$label

  x1 <- x0_vec[a]
  y1 <- y0_vec[a]

  x2 <- x0_vec[b]
  y2 <- y0_vec[b]

  r1 <- r_vec[a]
  r2 <- r_vec[b]#data$r[data$label %in% b]

  delta_y <- y2 - y1
  delta_x <- x2 - x1
  L <- sqrt(delta_x^2 + delta_y^2)
  r1L <- r1 / L
  r2L <- r2 / L

  arrow_dat <- data.frame(
    x = x1 + delta_x * r1L,
    xend =  x2 - delta_x * r2L,
    y = y1 + delta_y * r1L,
    yend = y2 - delta_y * r2L
  )

  return(arrow_dat)
}

# node placement
plotdata <- data.frame(label=c("E", "C", "B", "A", "D"),
                       x0=c(0, 0.1, -0.8, 0.7, 0.9),
                       y0=c(0, 0.8, 1.2, 1.7, 0.4),
                       r=0.2)

# arrow placement
arrow_data <- get_arrow_from_a_to_b(a=c("A", "A", "C", "B", "D"),
                                    b=c("B", "C", "E", "E", "E"),
                                    data=plotdata)

# custom colors
uvic_blue <- "#005493"    #RGB: 0-84-147
uvic_yellow <- "#F5AA1C"    #RGB: 245-170-28
uvic_red <- "#C63527"    #RGB: 198-53-39
uvic_blue_dark <- "#002754"  #RGB: 0-39-84

# create DAG plot
p <- ggplot(plotdata, aes(x0=x0, y0=y0, r=r)) +
  geom_circle(fill=uvic_red, color=uvic_yellow, linewidth=0.1) +
  geom_text(aes(x=x0, y=y0, label=label), color=uvic_yellow, size=10) +
  xlim(c(-1.8, 1.8)) +
  geom_segment(data=arrow_data, aes(x=x, xend=xend, y=y, yend=yend),
               inherit.aes=FALSE, color=uvic_yellow,
               arrow=arrow(length=unit(0.06, "cm"), type="closed"),
               linewidth=0.4) #+
  #theme_void() +
  #theme_transparent()

# custom font
sysfonts::font_add_google("Roboto Slab", "roboto_slab")

# create sticker
s <- hexSticker::sticker(p,
                         package="simDAG",
                         p_size=20,
                         p_x=1,
                         p_y=1.5,
                         p_color=uvic_yellow,
                         s_x=1,
                         s_y=0.82,
                         s_width=2,
                         s_height=1,
                         filename="logo.png",
                         h_fill=uvic_blue_dark,
                         h_color=uvic_yellow,
                         p_family="roboto_slab",
                         p_fontface="bold")
plot(s)




