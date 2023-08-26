###
# utilities
"%||%" <- ggplot2:::"%||%"
dapply <- ggplot2:::dapply
###
draw_key_grid <- function(data, params, size) {
  grid::rectGrob(0.5, 0.5, 0.5, 0.5,
                 gp = grid::gpar(
                   col = alpha(data$colour, data$alpha),
                   fill = alpha(data$fill, data$alpha)
                 )
  )
}

geom_brick <- function(mapping = NULL, data = NULL,
                      position = "identity",
                      ...,
                      bins = NULL,
                      binwidth = NULL,
                      binaxis = "y",
                      binpositions = "all",
                      stackdir = "up",
                      stackratio = 1,
                      dotsize = 1,
                      stack_scale = 0.85,
                      stackgroups = TRUE,
                      width = 0.9,
                      drop = FALSE,
                      na.rm = FALSE,
                      show.legend = NA,
                      inherit.aes = TRUE) {
  # If identical(position, "stack") or position is position_stack(), tell them
  # to use stackgroups=TRUE instead. Need to use identical() instead of ==,
  # because == will fail if object is position_stack() or position_dodge()
  if (!is.null(position) &&
      (identical(position, "stack") || (inherits(position, "PositionStack"))))
    message("position=\"stack\" doesn't work properly with geom_dotplot. Use stackgroups=TRUE instead.")
  # if (is.null(binwidth) && !is.null(bins)){binwidth <- diff(range) / bins}
  layer(
    data = data,
    mapping = mapping,
    stat = ggplot2::StatBindot,
    geom = GeomBrick,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    # Need to make sure that the binaxis goes to both the stat and the geom
    params = list(
      binaxis = binaxis,
      #bins = bins,
      binwidth = binwidth,
      binpositions = binpositions,
      width = width,
      drop = drop,
      stackdir = stackdir,
      stackratio = stackratio,
      dotsize = dotsize,
      stack_scale = stack_scale,
      stackgroups = stackgroups,
      na.rm = na.rm,
      ...
    )
  )
}

GeomBrick <- ggplot2::ggproto("GeomBrick", ggplot2::Geom,
                    required_aes = c("x", "y"),
                    non_missing_aes = c("size", "shape"),

                    default_aes = ggplot2::aes(colour = "grey30", fill = "white", alpha = NA, stroke = 1, linetype = "solid"),

                    setup_data = function(data, params) {
                      data$width <- data$width %||%
                        params$width %||% (resolution(data$x, FALSE) * 0.9)

                      # Set up the stacking function and range
                      if (is.null(params$stackdir) || params$stackdir == "up") {
                        stackdots <- function(a)  a - .5
                        stackaxismin <- 0
                        stackaxismax <- 1
                      } else if (params$stackdir == "down") {
                        stackdots <- function(a) -a + .5
                        stackaxismin <- -1
                        stackaxismax <- 0
                      } else if (params$stackdir == "center") {
                        stackdots <- function(a)  a - 1 - max(a - 1) / 2
                        stackaxismin <- -.5
                        stackaxismax <- .5
                      } else if (params$stackdir == "centerwhole") {
                        stackdots <- function(a)  a - 1 - floor(max(a - 1) / 2)
                        stackaxismin <- -.5
                        stackaxismax <- .5
                      }
                      # Fill the bins: at a given x (or y), if count=3, make 3 entries at that x
                      # if(!is.null(data$count)){
                      data <- data[rep(1:nrow(data), data$count), ]
                      # }

                      # Next part will set the position of each dot within each stack
                      # If stackgroups=TRUE, split only on x (or y) and panel; if not stacking, also split by group
                      # plyvars <- params$binaxis %||% "x"
                      plyvars <- params$binaxis
                      plyvars <- c(plyvars, "x", "PANEL")
                      if (is.null(params$stackgroups) || !params$stackgroups)
                        plyvars <- c(plyvars, "group")

                      # Within each x, or x+group, set countidx=1,2,3, and set stackpos according to stack function
                      data <- dapply(data, plyvars, function(xx) {
                        xx$countidx <- 1:nrow(xx)
                        xx$stackpos <- stackdots(xx$countidx)
                        # xx$stackpos <- xx$countidx
                        xx
                      })

                      # Set the bounding boxes for the dots
                      if (is.null(params$binaxis) || params$binaxis == "x") {
                        # ymin, ymax, xmin, and xmax define the bounding rectangle for each stack
                        # Can't do bounding box per dot, because y position isn't real.
                        # After position code is rewritten, each dot should have its own bounding box.
                        data$xmin <- data$x - data$binwidth / 2
                        data$xmax <- data$x + data$binwidth / 2
                        data$ymin <- stackaxismin
                        data$ymax <- stackaxismax
                        data$y    <- 0

                      } else if (params$binaxis == "y") {
                        # ymin, ymax, xmin, and xmax define the bounding rectangle for each stack
                        # Can't do bounding box per dot, because x position isn't real.
                        # xmin and xmax aren't really the x bounds, because of the odd way the grob
                        # works. They're just set to the standard x +- width/2 so that dot clusters
                        # can be dodged like other geoms.
                        # After position code is rewritten, each dot should have its own bounding box.
                        data <- dapply(data, c("group", "PANEL"), transform,
                                       ymin = min(y) - binwidth[1] / 2,
                                       ymax = max(y) + binwidth[1] / 2)

                        data$xmin <- data$x + data$width * stackaxismin
                        data$xmax <- data$x + data$width * stackaxismax
                        # Unlike with y above, don't change x because it will cause problems with dodging
                      }
                      data$maxcount <- max(data$countidx)
                      data
                    },


                    draw_group = function(data, panel_params, coord, na.rm = FALSE,
                                          binaxis = "x", stackdir = "up", stackratio = 1,
                                          dotsize = 1, stack_scale = 0.9, stackgroups = FALSE) {
                      if (!coord$is_linear()) {
                        warning("geom_brick does not work properly with non-linear coordinates.")
                      }

                      tdata <- coord$transform(data, panel_params)

                      # Swap axes if using coord_flip
                      if (inherits(coord, "CoordFlip"))
                        binaxis <- ifelse(binaxis == "x", "y", "x")

                      if (binaxis == "x") {
                        stackaxis = "y"
                        height <-  stack_scale * ( dotsize / tdata$maxcount[1] ) / (max(panel_params$y.range) - min(panel_params$y.range))
                        width <- dotsize * tdata$binwidth[1] / (max(panel_params$x.range) - min(panel_params$x.range))
                      } else if (binaxis == "y") {
                        stackaxis = "x"
                        height <- dotsize * tdata$binwidth[1] / (max(panel_params$y.range) - min(panel_params$y.range))
                        width <- stack_scale * ( dotsize / tdata$maxcount[1] ) / (max(panel_params$x.range) - min(panel_params$x.range))

                      }

                      ggplot2:::ggname("geom_grid",
                                       rectstackGrob(stackaxis = stackaxis, x = tdata$x, y = tdata$y,
                                                     height = height, width = width,
                                                     stackposition = tdata$stackpos, stackratio = stackratio,
                                                     default.units = "npc",
                                                     gp = grid::gpar(col = alpha(tdata$colour, tdata$alpha),
                                                                     fill = alpha(tdata$fill, tdata$alpha),
                                                                     lwd = tdata$stroke, lty = tdata$linetype))
                      )
                    },

                    draw_key = draw_key_grid
)

rectstackGrob <- function(
  x = unit(0.5, "npc"),     # x pos of the dotstack's origin
  y = unit(0.5, "npc"),     # y pos of the dotstack's origin
  stackaxis = "y",
  width = unit(1, "npc"),
  height = unit(1, "npc"),
  stackposition = 0,        # Position of each dot in the stack, relative to origin
  stackratio = 1,           # Stacking height of dots (.75 means 25% dot overlap)
  default.units = "npc", name = NULL, gp = grid::gpar(), vp = NULL)
{
  if (!grid::is.unit(x))
    x <- grid::unit(x, default.units)
  if (!grid::is.unit(y))
    y <- grid::unit(y, default.units)
  if (!grid::is.unit(height))
    height <- grid::unit(height, default.units)
  if (!grid::is.unit(width))
    width <- grid::unit(width, default.units)

  grid::grob(x = x, y = y, stackaxis = stackaxis,
             width = width, height = height,
             stackposition = stackposition, stackratio = stackratio,
             name = name, gp = gp, vp = vp, cl = "rectstackGrob")
}


makeContext.rectstackGrob <- function(x, recording = TRUE) {
  # Need absolute coordinates because when using npc coords with circleGrob,
  # the radius is in the _smaller_ of the two axes. We need the radius
  # to instead be defined in terms of the non-stack axis.
  xmm <- grid::convertX(x$x, "mm", valueOnly = TRUE)
  ymm <- grid::convertY(x$y, "mm", valueOnly = TRUE)

  if (x$stackaxis == "x") {
    width <- grid::convertY(x$width, "mm", valueOnly = TRUE)
    height <- grid::convertY(x$height, "mm", valueOnly = TRUE)
    xpos <- xmm + width * (x$stackposition * x$stackratio + (1 - x$stackratio) / 2)
    ypos <- ymm
  } else if (x$stackaxis == "y") {
    width <- grid::convertX(x$width, "mm", valueOnly = TRUE)
    height <- grid::convertX(x$height, "mm", valueOnly = TRUE)
    xpos <- xmm
    ypos <- ymm + height * (x$stackposition * x$stackratio + (1 - x$stackratio) / 2)
  }

  grid::rectGrob(
    x = xpos, y = ypos, width = width, height = height,
    default.units = "mm",
    name = x$name, gp = x$gp, vp = x$vp
  )
}
