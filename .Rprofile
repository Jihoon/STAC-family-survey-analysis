options(java.parameters = "-Xmx16g") 

library(ggplot2)
library(tidyr)
library(plyr)
library(dplyr)
library(stringr)
library(openxlsx)
library(XLConnect)

xlcFreeMemory()
detach("package:stats", unload=TRUE)  # Conflicts for filter() namespace


# Some helpful function definitions
.ls.objects <- function (pos = 1, pattern, order.by,
                         decreasing=FALSE, head=FALSE, n=5) {
  napply <- function(names, fn) sapply(names, function(x)
    fn(get(x, pos = pos)))
  names <- ls(pos = pos, pattern = pattern)
  obj.class <- napply(names, function(x) as.character(class(x))[1])
  obj.mode <- napply(names, mode)
  obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
  obj.prettysize <- napply(names, function(x) {
    capture.output(format(utils::object.size(x), units = "auto")) })
  obj.size <- napply(names, object.size)
  obj.dim <- t(napply(names, function(x)
    as.numeric(dim(x))[1:2]))
  vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
  obj.dim[vec, 1] <- napply(names, length)[vec]
  out <- data.frame(obj.type, obj.size, obj.dim)
  names(out) <- c("Type", "Size", "Rows", "Columns")
  if (!missing(order.by))
    out <- out[order(out[[order.by]], decreasing=decreasing), ]
  if (head)
    out <- head(out, n)
  out
}
# shorthand
lsos <- function(..., n=10) {
  .ls.objects(..., order.by="Size", decreasing=TRUE, head=TRUE, n=n)
}

view <- function(data, autofilter=TRUE) {
  # data: data frame
  # autofilter: whether to apply a filter to make sorting and filtering easier
  open_command <- switch(Sys.info()[['sysname']],
                         Windows= 'open',
                         Linux  = 'xdg-open',
                         Darwin = 'open')
  require(XLConnect)
  temp_file <- paste0(tempfile(), '.xlsx')
  wb <- XLConnect::loadWorkbook(temp_file, create = TRUE)
  XLConnect::createSheet(wb, name = "temp")
  XLConnect::writeWorksheet(wb, data, sheet = "temp", startRow = 1, startCol = 1)
  if (autofilter) setAutoFilter(wb, 'temp', aref('A1', dim(data)))
  XLConnect::saveWorkbook(wb, )
  system(paste(open_command, temp_file))
}

mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
  condition <- eval(substitute(condition), .data, envir)
  .data[condition, ] <- .data[condition, ] %>% mutate(...)
  .data
}

unit.vector <- function(idx, len) {
  a <- as.vector(matrix(0, len, ncol=1))
  a[idx] <- 1
  return(a)
}

.First <- function(){

}