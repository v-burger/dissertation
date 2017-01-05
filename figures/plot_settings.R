rm(list = ls())

require('ggplot2')
library('grid')
require('gridExtra')
library('gtable')
library('Rmisc')
library('sysfonts')
library('Hmisc')
library('reshape2')
library('dplyr')
library('Cairo')


color.palette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
color.low <- color.palette[7]
color.high <- color.palette[4]
color.highlight <- 'black'

linetypes.for <- function(values) {
  seq(1, length(unique(values)))
}

label.area <- 'Area'
label.methodology <- 'Method'
label.appears <- 'Appears'

# 2.2 done
# label.tdch <- expression(paste('DCH timeout ', T[DCH], ' ', (s)))
# label.ea <- expression(paste('Mean inter-packet time E[A] (s)'))
# label.power.drain <- 'Power drain PD'
# label.signalling.intensity <- 'Signalling intensity SI'
# label.signalling.frequency <- 'Signalling frequency SF'
# label.application <- 'Application'
# label.evaluation.type <- 'Model'
# label.cA <- expression(paste('Coefficient of variation ', c[A]))
# label.interarrival.time <- expression(paste('Packet inter-arrival time ', A, ' (s)'))
# label.interarrival.time.cdf <- expression(P(A <= a))
# label.lag <- 'Lag length'
# label.interarrival.sample.autocorrelation <- 'Sample autocorrelation\n of inter-arrival times'
# label.page.load.time <- 'Page load time t (s)'
# label.qoe <- 'Mean Opinion Score MOS'


unit.labeller <- function(unit) {
  passed.unit <- substitute(unit)
  
  function(variable, value) {
    do.call(expression, lapply(levels(value)[value], function(value) {
      bquote(paste(.(value), ' ', .(passed.unit)))
    }))
  }
}

font.size <- 8
#font.size <- 10
annotation.font.size <- (4/15) * font.size
font.family <- 'Linux Biolinum O'
#font.family <- 'Helvetica'
annotation.font <- font.family

plot_options = theme(
  plot.margin = unit(c(0.2, 0, 0, 0), "cm"),
  legend.position = "bottom",
  legend.direction = "horizontal",
  legend.box = "vertical",
  legend.key.height = unit(0.4, "cm"),
  legend.margin = unit(0.0, "cm"),
  text         = element_text(family=font.family, size = font.size),
  axis.title.x = element_text(family=font.family, size = font.size),
  axis.title.y = element_text(family=font.family, size = font.size)
)

units <- "cm"

pageWidth <- 9.5
rowHeight <- 6

adjust.legend.spacing <- function(plot, spacing = unit(-.4, 'lines')) {
  data <- ggplot_build(plot)
  gtable <- ggplot_gtable(data)
  lbox <- which(sapply(gtable$grobs, paste) == "gtable[guide-box]")
  guide <- gtable$grobs[[lbox]]
  if(length(guide$heights) == 5) {
    gtable$grobs[[lbox]]$heights <- unit.c(guide$heights[1:2],
                                           unit(-.4,'lines'),
                                           guide$heights[4:5])
  }
  grid.draw(gtable)
  plot <- arrangeGrob(gtable)
}

save.full.row.plot <- function(plot, filename = commandArgs(TRUE)[1]) {
  plot <- plot + plot_options + theme(legend.box = "vertical")
  #plot <- adjust.legend.spacing(plot)
  print(filename)
  ggsave(filename, plot=plot,height = rowHeight, width = pageWidth, units = units, device=cairo_pdf)
  #embed_fonts(filename, outfile=filename)
}

save.full.double.row.plot <- function(plot, filename = commandArgs(TRUE)[1]) {
  plot <- plot + plot_options + theme(legend.box = "vertical")
  #plot <- adjust.legend.spacing(plot)
  print(filename)
  ggsave(filename, plot=plot,height = rowHeight * 1.5, width = pageWidth, units = units, device=cairo_pdf)
  #embed_fonts(filename, outfile=filename)
}



notation.si <- function(label) {
  label <- format(label, scientific = TRUE)
  label <- gsub("e", "%*%10^", label)
  parse(text=label)
}
