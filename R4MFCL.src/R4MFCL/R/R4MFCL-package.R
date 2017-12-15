#'  R tools for Multifan-cl
#'  @importFrom grDevices col2rgb colorRampPalette dev.cur dev.new dev.off dev.size gray grey hsv rainbow rgb savePlot win.graph windows x11
#'  @importFrom graphics abline arrows axis barplot box boxplot image layout layout.show legend lines mtext par plot points polygon segments symbols text title
#'  @importFrom stats aggregate aggregate.data.frame dnorm lowess median na.omit quantile rnorm sd
#'  @importFrom utils count.fields data read.csv read.table setInternet2 stack write.table
#'
#' @examples
#'
#' \dontrun{
#' # read in the report file using SS_output
#' myreplist <- SS_output(dir='c:/SS/simple/')
#'
#' # make a collection of plots using SS_plots
#' SS_plots(replist=myreplist)
#' }
#'
NULL
## quiets concerns of R CMD check re: the .'s that appear in pipelines
## and the "n" that is produced by dplyr::count() in a pipeline
if(getRversion() >= "2.15.1")  utils::globalVariables(c(".", "n"))
