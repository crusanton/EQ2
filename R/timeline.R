utils::globalVariables(c("MONTH", "DAY", "DEATHS", "EQ_PRIMARY","ymd", "years", "YEAR", "days",
                         "DATE", "LATITUDE", "LONGITUDE","LOCATION_NAME", "layer"))


#' Plot timeline earthquakes
#
#' @return mapped aesthetics for the Geom layer
geom_point <- ggplot2::ggproto("GeomTimeline",
                                ggplot2::Geom,
                                required_aes = c("x"),
                                default_aes  = ggplot2::aes(shape = 20, color = "black", size = 1, alpha = 0.6),
                                draw_key     = ggplot2::draw_key_point,
                                draw_panel   = function(data, panel_scales, coord) {
                                               coords <- coord$transform(data, panel_scales)
                                               if(is.null(coords$y)){ coords$y  = 0.3}
                                                points <- grid::pointsGrob(x    = coords$x,
                                                                           y    = coords$y,
                                                                           size = grid::unit(coords$size, "mm"),
                                                                           pch  = coords$shape,
                                                                           gp   = grid::gpar(alpha = coords$alpha,col = coords$color))
                                                coords$xmin = min(coords$x)
                                                coords$xmax = max(coords$x)
                                                lower <- grid::segmentsGrob(x0  = coords$xmin,
                                                                            x1  = coords$xmax,
                                                                            y0  = coords$y,
                                                                            y1  = coords$y,
                                                                            gp  = grid::gpar(alpha = 0.6, lwd = 1,col = "grey80"))
                                                base  <- grid::segmentsGrob(x0  = 0,
                                                                            x1  = 1,
                                                                            y0  = 0,
                                                                            y1  = 0,
                                                                            gp = grid::gpar(alpha = coords$alpha))
                                                grid::gTree(children = grid::gList(points, lower, base))} # draw_panel
) # geom_point


#' Plot timeline earthquakes
#'
#' @param mapping      mapping parameter, leave as NULL
#' @param data         data parameter, leave as NULL
#' @param stat         stat parameter
#' @param position     position parameter
#' @param show.legend  show.legend parameter
#' @param na.rm        na.rm parameter
#' @param inherit.aes  inherit.aes parameter
#' @param ...          other parameters
#'
#' @return Earthquake data Clean
#'
#' @examples
#' \dontrun{
#' geom_timeline(eq_data)
#' }
#' @export
geom_timeline <- function(mapping = NULL, data = NULL, stat = "identity",position = "identity", show.legend = NA,
                          na.rm = FALSE, inherit.aes = TRUE, ...) {
  layer(data        = data,
        mapping     = mapping,
        stat        = stat,
        geom        = geom_point,
        position    = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params      = list(na.rm = na.rm, ...))
} # geom_timeline


#' text annotation
#' @rdname ggplot2-ggproto
#'
#' @export
geo_timeline_point <- ggplot2::ggproto("GeomTimelineLabel", ggplot2::Geom,
                                       required_aes = c("x", "label"),
                                       default_aes  = ggplot2::aes(n_max = 10),
                                       draw_key     = ggplot2::draw_key_point,
                                       draw_panel   = function(data, panel_scales, coord) {
                                                               coords <- coord$transform(data, panel_scales)
                                                               if(is.null(coords$y)){ coords$y  = 0.3}
                                                               if(is.null(coords$n_max)){ coords$n_max  = 10}
                                                               n_max = coords$n_max[1]
                                                               coords_n_max <- coords %>% group_by(y) %>% top_n(n=n_max[1], wt = size)
                                                               text_HQ <- grid::textGrob(label = coords_n_max$label,
                                                                                         x = coords_n_max$x,
                                                                                         y = coords_n_max$y*1.1,vjust = 0, hjust = 0, rot = 45)
                                                               lines_HQ <- grid::segmentsGrob(x0 = coords_n_max$x,
                                                                                              y0 = coords_n_max$y,
                                                                                              x1 = coords_n_max$x,
                                                                                              y1 = coords_n_max$y*1.1,
                                                                                              gp = grid::gpar(alpha = 0.6, lwd = 1,col = "grey80"))
                                                               grid::gTree(children = grid::gList(text_HQ, lines_HQ))   }   # draw_panel
) # geo_timeline_point


#' Plot timeline for earthquakes
#'
#' @inheritParams geom_timeline
#'
#' @examples
#' \dontrun{
#' geom_timelinelabel(eq_data)
#' }
#' @export
geom_timelinelabel <- function(mapping = NULL, data = NULL, stat = "identity",position = "identity", show.legend = NA,
                               na.rm = FALSE, inherit.aes = TRUE, ...) {
  layer(data        = data,
        mapping     = mapping,
        stat        = stat,
        geom        = geo_timeline_point,
        position    = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params      = list(na.rm = na.rm, ...))
} # geom_timelinelabel
