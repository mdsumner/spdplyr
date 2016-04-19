



#' @importFrom ggplot2 ggproto Stat
StatNested <- ggplot2::ggproto("StatNested", ggplot2::Stat,
                     compute_group = function(data, scales) {
                       data
                     },
                     
                     required_aes = c("x", "y", "group", "fill")
)

#' Title
#'
#' @inheritParams ggplot2::stat_bin
#'
#' @return
#' @export
#' @importFrom ggplot2 layer
#' @examples
stat_nested <- function(mapping = NULL, data = NULL, geom = "polygon",
                       position = "identity", na.rm = FALSE, show.legend = NA, 
                       inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatNested, data = data, mapping = mapping, geom = geom, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

#' @importFrom ggplot2 ggproto Stat GeomPolygon
#' @importFrom grid pathGrob gpar
#' @importFrom scales alpha
#' @references http://qiita.com/kohske/items/9272e29a75d32416ff5e
GeomHolygon <- ggproto(
  "GeomHolygon", 
  ggplot2::GeomPolygon,
  extra_params = c("na.rm", "rule"),
  draw_panel = function(data, scales, coordinates, rule) {
    n <- nrow(data)
    if (n == 1) 
      return(zeroGrob())
    
    munched <- coord_munch(coordinates, data, scales)
    munched <- munched[order(munched$group), ]
    
    first_idx <- !duplicated(munched$group)
    first_rows <- munched[first_idx, ]
    
    ggplot2:::ggname(
      "geom_holygon", 
      pathGrob(munched$x, munched$y, default.units = "native", 
               id = munched$group, rule = rule, 
               gp = gpar(col = first_rows$colour, 
                         fill = alpha(first_rows$fill, first_rows$alpha), 
                         lwd = first_rows$size * .pt, 
                         lty = first_rows$linetype)))
  }
)



#' Title
#'
#' @inheritParams ggplot2::layer
#' @param na.rm remove NA
#' @param rule winding or evenodd as per \code{\link[graphics]{polypath}} 
#' @param ... passed to \code{\link[ggplot2]{layer}}
#' @return
#' @export
#'
#' @examples
geom_holygon <- function (mapping = NULL, data = NULL, stat = "identity", position = "identity", 
                          na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, rule = "winding", ...) {
  ggplot2::layer(data = data, mapping = mapping, stat = stat, geom = GeomHolygon, 
                 position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
                 params = list(na.rm = na.rm , rule = rule, ...))
}




#' Title
#'
#' @inheritParams ggplot2::fortify
#' @return
#' @export
#'
#' @examples
fortify.nsp_df <- function(model, data, ...) {
  unnest(unnest(model, ...))
}

# Title
#
# @param data 
# @param mapping 
# @param ... 
# @param environment 
#
# @return
# @export
#
# @examples
# ggplot.nsp_df <- function (data, mapping = aes(x = x, y = y, group = branch,  fill = branch), ..., environment = parent.frame()) {
#   ggplot(fortify.nsp_df(data, ...), mapping, environment = environment)
# }


# GeomSpatialPath <- ggproto("GeomSpatialPath", Geom,
#                           # required_aes = c("x", "y"),
#                            default_aes = aes(x = "x", y = "y", 
#                                              fill = "grey", col = "black", 
#                                              group = "branch", fill = "object"),
#                            draw_key = draw_key_polygon,
#                            
#                            draw_group = function(data, panel_scales, coord) {
#                              coords <- coord$transform(data, panel_scales)
#                              grid::pathGrob(
#                                coords$x, coords$y,
#                                fill = coords$fill,
#                                col = coords$col, 
#                                gp = grid::gpar(col = coords$colour)
#                              )
#                            }
# )
# 
# geom_sp_path <- function(mapping = NULL, data = NULL, stat = "identity",
#                               position = "identity", na.rm = FALSE, show.legend = NA, 
#                               inherit.aes = TRUE, ...) {
#   layer(
#     geom = GeomSpatialPath, mapping = mapping,  data = data, stat = stat, 
#     position = position, show.legend = show.legend, inherit.aes = inherit.aes,
#     params = list(na.rm = na.rm, ...)
#   )
# }
# 
# 
# 
# GeomSimplePath<- ggproto("GeomSimplePath", Geom,
#                              #required_aes = c("x", "y"),
#                              # 
#                              # default_aes = aes(
#                              #   colour = NA, fill = "grey20", size = 0.5,
#                              #   linetype = 1, alpha = 1
#                              # ),
#                              # 
#                              # draw_key = draw_key_polygon,
#                              default_aes = aes(x = "x", y = "y", 
#                                                 col = "black", 
#                                                group = "branch", fill = "grey", alpha = 1),
#                              draw_key = draw_key_polygon,
#                              draw_panel= function(data, panel_scales, coord) {
#                                print(data)
#                                 data <- unnest(unnest(data))
#                               
#                                n <- nrow(data)
#                                if (n <= 2) return(grid::nullGrob())
#                                
#                                coords <- coord$transform(data, panel_scales)
#                                # A polygon can only have a single colour, fill, etc, so take from first row
#                                first_row <- coords[1, , drop = FALSE]
#                               
#                                grid::pathGrob(
#                                  coords$x, coords$y, 
#                                  default.units = "native",
#                                  gp = grid::gpar(
#                                    col = first_row$colour,
#                                    fill = scales::alpha(first_row$fill, first_row$alpha),
#                                    #lwd = first_row$size * .pt,
#                                    #lwd = 1,
#                                    #lty = first_row$linetype
#                                  )
#                                )
#                              }
# )
# geom_simple_path <- function(mapping = NULL, data = NULL, stat = "identity",
#                                 position = "identity", na.rm = FALSE, show.legend = NA, 
#                                 inherit.aes = TRUE, ...) {
#   layer(
#     geom = GeomSimplePath, mapping = mapping, data = data, stat = stat, 
#     position = position, show.legend = show.legend, inherit.aes = inherit.aes,
#     params = list(na.rm = na.rm, ...)
#   )
# }
# 
# # ggplot.nsp_df <- function (data = NULL, mapping = aes(), ..., environment = parent.frame()) {
# #   NextMethod("ggplot", unnest(unnest(data)), mapping = aes(x, y, group = branch, fill = object))
# # }
# ggplot(xa[, c("NAME", "object", "Object")]) + 
# #+ aes(x, y, group = branch, fill = object) 
#  geom_simple_path()
# 
