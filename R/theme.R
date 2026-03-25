#' jtbdtools ggplot2 theme
#'
#' A clean, minimal theme for JTBD visualizations. Based on `theme_minimal()`
#' with refined typography and grid lines.
#'
#' @param base_size Base font size (default: 12)
#'
#' @return A ggplot2 theme object
#' @export
#'
#' @family visualization
#'
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(mpg, wt)) + geom_point() + theme_jtbd()
theme_jtbd <- function(base_size = 12) {
  theme_minimal(base_size = base_size) %+replace%
    theme(
      text = element_text(family = ""),
      panel.grid.major = element_line(color = "#DAE1E7", linewidth = 0.4),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.text = element_text(size = base_size * 0.85),
      axis.text.x = element_text(margin = margin(t = 5)),
      axis.text.y = element_text(margin = margin(r = 5)),
      axis.title = element_text(size = base_size * 1.1),
      axis.line = element_line(color = "#2C3E50", linewidth = 0.4),
      axis.title.y = element_text(margin = margin(r = 10), hjust = 0.5),
      axis.title.x = element_text(margin = margin(t = 10), hjust = 0.5),
      plot.title = element_text(size = base_size * 1.3, face = "bold", margin = margin(b = 8)),
      plot.subtitle = element_text(size = base_size * 0.95, color = "#606F7B", margin = margin(b = 12)),
      plot.caption = element_text(size = base_size * 0.7, color = "#3D4852", margin = margin(t = 10)),
      legend.position = "bottom",
      legend.title = element_text(size = base_size * 0.85, face = "bold"),
      legend.text = element_text(size = base_size * 0.8),
      strip.text = element_text(size = base_size * 0.95, face = "bold", margin = margin(b = 5))
    )
}

#' JTBD color palette
#'
#' Returns a named color palette for JTBD opportunity classifications.
#'
#' @return A named character vector of colors
#' @export
#'
#' @examples
#' jtbd_colors()
jtbd_colors <- function() {
  c(
    "underserved"     = "#E74C3C",
    "appropriately"   = "#95A5A6",
    "overserved"      = "#3498DB",
    "high_opportunity" = "#E74C3C",
    "low_opportunity"  = "#2ECC71",
    "importance"       = "#2C3E50",
    "satisfaction"     = "#16A085",
    "opportunity"      = "#E74C3C"
  )
}
