\name{plot.simDT}
\alias{plot.simDT}

\title{
Create a Simple Flowchart for a Discrete-Time Simulation
}
\description{
Given a \code{simDT} object obtained with the \code{\link{sim_discrete_time}} function, plots a relatively simple flowchart of how the simulation was performed. Shows only some general information extracted from the \code{tx_nodes}.
}
\usage{
\method{plot}{simDT}(x,
     increase_t_text="Increase t by 1",
     max_t_text=paste0("t <= ", x$max_t, "?"),
     box_width=0.35,
     box_height=0.23,
     box_hdist=1,
     box_vdist=1,
     box_alpha=0.5,
     box_linetype="solid",
     box_linewidth=0.5,
     box_border_colors=NULL,
     box_fill_colors=NULL,
     box_text_color="black",
     box_text_alpha=1,
     box_text_angle=0,
     box_text_family="sans",
     box_text_fontface="plain",
     box_text_size=5,
     box_text_lineheight=1,
     arrow_line_type="solid",
     arrow_line_width=0.5,
     arrow_line_color="black",
     arrow_line_alpha=1,
     arrow_head_angle=30,
     arrow_head_size=0.3,
     arrow_head_unit="cm",
     arrow_head_type="closed",
     arrow_left_pad=0.3,
     hline_width=0.5,
     hline_type="dashed",
     hline_color="black",
     hline_alpha=1,
     ...)
}
\arguments{
  \item{x}{
A \code{simDT} object created using the \code{sim_discrete_time} function.
  }
  \item{increase_t_text}{
A single character string specifying the text in the second box from the top on the left.
  }
  \item{max_t_text}{
A single character string specifying the text in the last box on the left.
  }
  \item{box_width}{
A single positive number specifying the width of the boxes.
  }
  \item{box_height}{
A single positive number specifying the height of the boxes.
  }
  \item{box_hdist}{
A single positive number specifying the horizontal distance of the left and the right boxes.
  }
  \item{box_vdist}{
A single positive number specifying the vertical distance of the boxes.
  }
  \item{box_alpha}{
A single number between 0 and 1 specifying the transparency level of the boxes.
  }
  \item{box_linetype}{
A single positive number specifying the linetype of the box outlines.
  }
  \item{box_linewidth}{
A single positive number specifying the width of the box outlines.
  }
  \item{box_border_colors}{
A character vector of length two specifying the colors of the box outlines. Set to \code{NULL} (default) to use \code{ggplot2} default colors.
  }
  \item{box_fill_colors}{
A character vector of length two specifying the colors of the inside of the boxes. Set to \code{NULL} (default) to use \code{ggplot2} default colors.
  }
  \item{box_text_color}{
A single character string specifying the color of the text inside the boxes.
  }
  \item{box_text_alpha}{
A single number between 0 and 1 specifying the transparency level of the text inside the boxes.
  }
  \item{box_text_angle}{
A single positibe number specifying the angle of the text inside the boxes.
  }
  \item{box_text_family}{
A single character string specifying the family of the text inside the boxes. May be one of \code{"sans", "serif", "mono"}.
  }
  \item{box_text_fontface}{
A single character string specifying the fontface of the text inside the boxes. May be one of \code{"plain", "bold", "italic", "bold.italic"}.
  }
  \item{box_text_size}{
A single number specifying the size of the text inside the boxes.
  }
  \item{box_text_lineheight}{
A single number specifying the lineheight of the text inside the boxes.
  }
  \item{arrow_line_type}{
A single character string specifying the linetype of the arrows.
  }
  \item{arrow_line_width}{
A single positive number specifying the line width of the arrows.
  }
  \item{arrow_line_color}{
A single character string specifying the color of the arrows.
  }
  \item{arrow_line_alpha}{
A single number between 0 and 1 specifying the transparency level of the arrows.
  }
  \item{arrow_head_angle}{
A single number specifying the angle of the arrow heads.
  }
  \item{arrow_head_size}{
A single number specifying the size of the arrow heads. The unit is defined by the \code{arrow_head_size} argument.
  }
  \item{arrow_head_unit}{
A single chracter string specifying which unit to use when specifying the \code{arrow_head_size} argument. Defaults to \code{"cm"}.
  }
  \item{arrow_head_type}{
A single chracter string specifying which type of arrow head to use. See \code{?arrow} for more details.
  }
  \item{arrow_left_pad}{
A single positive number specifying the distance between the left boxes and the arrow line to the left of it.
  }
  \item{hline_width}{
A single number specifying the width of the horizontal lines between the left and right boxes.
  }
  \item{hline_type}{
A single character string specifying the linetype of the horizontal lines between the left and right boxes.
  }
  \item{hline_color}{
A single character string specifying the color of the horizontal lines between the left and right boxes.
  }
  \item{hline_alpha}{
A single number between 0 and 1 specifying the transparency level of the horizontal lines between the left and right boxes.
  }
  \item{...}{
Currently not used.
  }
}
\details{
The resulting flowchart includes two columns of boxes next to each other. On the left side it always starts with the same two boxes: a box about the creation of the initial data and a box about increasing the simulation time by 1. Netx, there will be a box for each time-varying variable in the \code{simDT} object. Afterwards there is another box which asks if the maximum simulation time was reached. An arrow to the left that points back to the second box from the top indicates the iterative nature of the simulation process. The right column of boxes includes additional information about the boxes on the left.

Although there are quite a few arguments to this function, it is still rather fixed in nature. One cannot add further boxes or arrows in a simple way. The general structure may also not be changed. It may be useful to visualize a general idea of the simulation flow, but it may be too limited for usage in scientific publications if the simulation is more complex.

The graphic is created using the \code{ggplot2} package and the output is a standard \code{ggplot} object. This means that the user can change the result using standard \code{ggplot} syntax (adding more stuff, changing geoms, ...).
}
\value{
Returns a \code{ggplot} object.
}
\author{
Robin Denz
}
\examples{
# TODO: write some
}
