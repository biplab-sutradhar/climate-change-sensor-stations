# works_with_R("3.1.1", dplyr="0.2")

## Parse the first occurance of pattern from each of several strings
## using (named) capturing regular expressions, returning a matrix
## (with column names).
str_match_perl <- function(string,pattern){
  stopifnot(is.character(string))
  stopifnot(is.character(pattern))
  stopifnot(length(pattern)==1)
  parsed <- regexpr(pattern,string,perl=TRUE)
  captured.text <- substr(string,parsed,parsed+attr(parsed,"match.length")-1)
  captured.text[captured.text==""] <- NA
  captured.groups <- if(is.null(attr(parsed, "capture.start"))){
    NULL
  }else{
    do.call(rbind,lapply(seq_along(string),function(i){
      st <- attr(parsed,"capture.start")[i,]
      if(is.na(parsed[i]) || parsed[i]==-1)return(rep(NA,length(st)))
      substring(string[i],st,st+attr(parsed,"capture.length")[i,]-1)
    }))
  }
  result <- cbind(captured.text,captured.groups)
  colnames(result) <- c("",attr(parsed,"capture.names"))
  result
}

## Index <- read.fwf("CRUTEM.4.3.0.0.station_files/Index",
##                   widths=c(6, 23, 15, 7, 7, 6, 5, 5))
Index.lines <-
  readLines("CRUTEM.4.3.0.0.station_files/Index", encoding="latin1")
stopifnot(nchar(Index.lines) == 74)
pattern <-
  paste0("(?<Number>[0-9]{6})",
         " {3}",
         "(?<Name>.{20})",
         "(?<Country>.{15})",
         "(?<Lat>.{7})",
         "(?<Long>.{7})",
         "(?<Height>.{6})",
         "(?<StartYear>.{5})",
         "(?<EndYear>.{5})")
Index.mat <- str_match_perl(Index.lines, pattern)
stopifnot(!is.na(Index.mat))
strip <- function(x){
  gsub("^[ ]*", "", gsub("[ ]*$", "", x))
}
types <-
  c(Number=as.integer,
    Name=as.character,
    Country=as.character,
    Lat=as.numeric,
    Long=as.numeric,
    Height=as.integer,
    StartYear=as.integer,
    EndYear=as.integer)
Index.list <- list()
for(col.name in names(types)){
  type.fun <- types[[col.name]]
  Index.list[[col.name]] <- type.fun(strip(Index.mat[, col.name]))
}
all.stations <- do.call(data.frame, Index.list) 
stopifnot(!is.na(all.stations))
subset(all.stations, Lat < -90)
subset(all.stations, Name == "name")

library(dplyr)  # make sure dplyr is loaded
stations <- all.stations %>% filter(Lat > -90)


save(stations, file="stations.RData")
