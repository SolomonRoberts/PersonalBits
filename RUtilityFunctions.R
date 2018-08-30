#Filepath is 
#source("R:/BI/Tools/R/R Utility Functions.R")
#.libPaths("R:/BI/Tools/R/R Shared Package Repository")
library(data.table)
library(readxl)
library(ggplot2)
library(iterators)
library(plyr)

options(scipen = 20)


right = function (string, char){
  substr(string,nchar(string)-(char-1),nchar(string))
}

left = function (string,char){
  substr(string,1,char) 
}


#Consule custom binary functions using a hack to save typing brackets by typing
#object %v% cabbage instead of View(object) 
`%v%` = function(a,b)(View(a))
`%n%` = function(a,b)(names(a))



get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

getZScore = function(input){
  input[is.infinite(input) == TRUE] = NA
  stdev = sd(input, na.rm = TRUE)
  meanValue = mean(input, na.rm = TRUE)
  output = sapply(input,function(x){return((x - meanValue)/stdev)})
  output[is.nan(output)] = NA
  attr(output,"stdev") = stdev
  attr(output,"meanValue") = meanValue 
  return(output)
}


#Function to reverse a vector of Z scores, where the Z scores have attributes of ht eoriginal data 
reverseZScore = function(input){
 mean = attr(input,"meanValue")
 sdev = attr(input,"stdev")
  output = sapply(input, function(x){
     return(mean + (x *  sdev))
  })
   return(output)
}



#Function to reverse a Z score. If the entire vector of Z scores is 
reverseZScoreSingleAttributes = function(datapoint){
  return((datapoint * attr(datapoint,"stdev")) + attr(datapoint,"meanValue"))
}

  
  
  


cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}

#platforms = data.frame(PlatformKey = c(4,5,7), PlatformName = c("TTD","AppNexus","DBM"))

reverselog_trans <- function(base = exp(1)) {
  trans <- function(x) -log(x, base)
  inv <- function(x) base^(-x)
  trans_new(paste0("reverselog-", format(base)), trans, inv, 
            log_breaks(base = base), 
            domain = c(1e-100, Inf))
}



#Utility function to do with lists of lists of lists of dataframes, use recursion to navigate down the list to find the end node of the dataframe, and then callthe
# desired function on the dataframe, not the list of datafrmaes. 
recurse = function(x,funtSent){
  if( class(x)[1] %in% c("data.frame","data.table")){
    return(funtSent(x))
  }
  else(return(lapply(x,recurse,funtSent)))
}



#Utility function to do with lists of lists of lists of dataframes, use recursion to navigate down the list to find the end node of the dataframe, and then callthe
# desired function on the dataframe, not the list of datafrmaes. 
recurseGGplot = function(x,funtSent){
  if( class(x)[1] == "gg"){
    return(funtSent(x))
  }
  else(return(lapply(x,recurseGGplot,funtSent)))
}


#Utility function to do with lists of lists of lists of linear models, use recursion to navigate down the list to find the end node of the dataframe, and then callthe
# desired function on the lm, not the list of lm  
recurseLM = function(x,funtSent){
  if( class(x)[[1]] == "lm"){
    return(lapply(x, funtSent))
  }
  else(return(lapply(x,recurseLM,funtSent)))
}




#Utility function to get the column names of first datatable in a nested lists of nested datatables. 
recurseNames = function(x,funtSent){
  if( class(x)[1] %in% c("data.table","data.frame")){
    return(names(x))
  }
  else(return(recurseNames(x[[1]])))
}

#Function to take in datatables, and then for each datable, add a column that gives the name of that datatable in the list
#as a heading of that datatble. 
dimensionTables = function(tables){
  headings = names(tables)
  tableHeadings = recurseNames(tables)
  headingInt = 1 + length(Filter(function(x){return(left(x,7) == "heading")}, tableHeadings ))
  for(x in 1:length(tables)){
    tables[[x]] = recurse(tables[[x]], function(input){
      input[[eval(paste0("heading",headingInt))]] = headings[x]
      return(input)
    })
  }
  return(tables)
}


#Function to take a symetrical nested list of nested data tables of any level of nestedness, and collapse into a single datatable, 
#where the labels of different nestedness are available as column names 
collapseList = function(input){
  if(class(input[[1]]) %in% c("data.table", "data.frame"))
  {input = dimensionTables(input)
  return(rbindlist(input))
  }else{
    input = dimensionTables(input)
    return(rbindlist(lapply(input, collapseList)))
  }
}



#Extended version of grid.arrange for multiple plots where they share the same legend 
grid_arrange_shared_legend <- function(..., ncol = length(list(...)), nrow = 1, position = c("bottom", "right")) {
  
  plots <- list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x + theme(legend.position="none"))
  gl <- c(gl, ncol = ncol, nrow = nrow)
  
  combined <- switch(position,
                     "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                            legend,
                                            ncol = 1,
                                            heights = unit.c(unit(1, "npc") - lheight, lheight)),
                     "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                           legend,
                                           ncol = 2,
                                           widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
  grid.newpage()
  grid.draw(combined)
  
}

#Utility function for GGplot for where you have a grob with mutlipel plots on it, each plot uses the same dimensions, 
#and you want there to be just one legend
grid_arrange_shared_legend_list <- function(plots, ncol = length(list(...)), nrow = 1, position = c("bottom", "right")) {
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x + theme(legend.position="none"))
  gl <- c(gl, ncol = ncol, nrow = nrow)
  
  combined <- switch(position,
                     "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                            legend,
                                            ncol = 1,
                                            heights = unit.c(unit(1, "npc") - lheight, lheight)),
                     "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                           legend,
                                           ncol = 2,
                                           widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
  grid.newpage()
  grid.draw(combined)
  
}

#Save a GG plot to image with anti-aliasing at 40cm by 20cm and dpi of 300. By default, it will use the ggplot title as the filename 
#unless you specify an alternative title. 
defaultSave= function(plot, title = ""){
  if(title == ""){
    title = paste0(plot$labels$title,".png")
    title = gsub("\n","", title)
    title = gsub("/", "-", title)
      ggsave(title,plot = plot, width = 40, height = 20, units = "cm", dpi = 300, type = "cairo-png")
  }  else {
  ggsave(paste0(title, ".png"),plot = plot, width = 40, height = 20, units = "cm", dpi = 300, type = "cairo-png") 
}
}


#Take in a list of ggplot objects and a filepath, and defaultSave all the plots to that location. 
savePlots = function(plots, path){
  home = getwd()
  dir.create(path)
  setwd(path)
  recurseGGplot(plots, defaultSave)
  setwd(home)
}


#Take in an nth nested list of ggplot objects, and save them to desk with a folder structure that replicates the list's hierarchy
structuredSave = function(plots, path){
  if(sum(class(plots[[1]]) == "gg") > 0){
    savePlots(plots,path)
    return(NULL)
  }
  home = getwd()
  setwd(path)
  folders = names(plots)
  paths = lapply(folders, function(x)(paste0(path, "/", x)))
  lapply(folders, dir.create)
  mapply(function(x,y){structuredSave(x,y)}, plots, paths, SIMPLIFY =FALSE)
  setwd(home)
}



#Get a named list of dataframes and write them to CSV
writeListFiles = function(datatables, fileNameString = ""){
  fileNames = names(datatables)
  for(x in 1:length(datatables)){
    write.csv(datatables[[x]], paste0(fileNameString, " ", fileNames[x], ".csv"), row.names = FALSE, na = "")
  }
}


#Used by createTimestamp, take in a DeliveryTimeKey as in fact impression where it is a time of day stored as 1213 or 41 for 41 minutes past minute
# and pad it out into format of "0041"
padtimekey = function(timekey){
  timekey = as.character(timekey)
  if(nchar(timekey) ==4){
    return(timekey)
  }else if (nchar(timekey) == 3){
    return(paste0("0",timekey))
  }else if (nchar(timekey) == 2){
    return(paste0("00",timekey))
  }else {return(paste0("000",timekey))}
}


#USed by createTimeStamp, take a padded time key, then convert it from "0643" to the number of seconds that have occured after six hours and 43 minutes in the day. 
paddedTimeKeyToInt = function(timekey){
  hours = as.numeric(left(timekey, 2))
  minutes = as.numeric(right(timekey,2))
  return(((hours * 60) + minutes) * 60 )
}






#Function to take in the deliverydatekey and deliverytimekey and return the integer that represents that time. 
createTimestamp = function(datekeys, timekeys){
  dateObjects = as.POSIXct(datekeys, format =  "%Y-%m-%d")
  dateInts = as.integer(dateObjects)
  timekeyStrings = sapply(timekeys, padtimekey)
  timekeyInts = sapply(timekeyStrings, paddedTimeKeyToInt)
  timestamp = dateInts + timekeyInts
  return(timestamp)
}


#Function to create a processing cluster using the R parallel package. Abstracts away the process of detecting the number of cores, 
#and will create a cluster whch uses all the cores of your desktop and keeps one strange 
createClusterObject = function(){
  coreNumber = detectCores() - 1
  cl = makeCluster(coreNumber)
  return(list(coreNumber = coreNumber, cl = cl))
}

#GGplot utility function for fomatting titles to have the new line characters at appropriate places. Takes in the title string
#and a linewidth number of characters, and then formats the string so that each single line is less than that character limit. 
enterTitles = function(string, charLimit){
  if(nchar(string) <= charLimit){
    return(string)
  }
 enterTimes = nchar(string) / charLimit  
 lines  = strwrap(string, nchar(string) / enterTimes)
 return(paste0(lines, collapse = "\n"))
}


#Format Tableau headings 
#When exporting a Tableau to Excel as crosstab, the column names are formatted with spaces as "Client Conversions". This is slightly
#awkward to deal with in R, so this function takes in a dataframe and then returns that dataframe with all the columns renamed in the way of
#"ClientConversions"
formatTableauHeadings = function(dataset){
  headings = names(dataset)
  headings = sapply(headings, function(x)(gsub(" ","", x)))
  headings = sapply(headings, function(x)(gsub(":","", x)))
  names(dataset) = headings
  return(dataset)
  
  
}

#Takein a dataframe and a vector of target dimensions, and then produce a summary table which is the sum of all numeric
#columns in that dataframe grouped by those target dimensions. 
rollSum  = function(input, dimensions){
  for (x in 1:length(dimensions)){
    input[[eval(dimensions[x])]] = as.character(input[[eval(dimensions[x])]])
  }
  numericColumns = which(lapply(input,class) %in% c("integer", "numeric")) 
  output = input[,lapply(.SD, sum, na.rm = TRUE), by = eval(dimensions), .SDcols = numericColumns]
  return(output)
}

#Takein a dataframe and a vector of target dimensions, and then produce a summary table which is the mean of all numeric
#columns in that dataframe grouped by those target dimensions. 
rollMean  = function(input, dimensions){
  for (x in 1:length(dimensions)){
    input[[eval(dimensions[x])]] = as.character(input[[eval(dimensions[x])]])
  }
  numericColumns = which(lapply(input,class) == "numeric") 
  output = input[,lapply(.SD, mean, na.rm = TRUE), by = eval(dimensions), .SDcols = numericColumns]
  return(output)
}


#Function to take in integer N, and provide N hexadeicmal colour code strings, of the colours across evenly spaced bits across
#the specturm. 
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}




unlistFrames = function(input){
  #BUG IN THIS FUNCTION, THAT IF A STRING FIELD HAS A "." IN IT, IT THEN OVERSPLITS IT 
  while(class(input[[1]]) == "list"){
    input = unlist(input, recursive =FALSE)
  }
  listNames = names(input)
  
  listNames = lapply(listNames, function(x)(unlist(strsplit(x,"\\."))))
  output = mapply(function(inputFrame,names){
    for(x in 1:length(names)){
      columnName = paste0("level",x)
      inputFrame[[eval(columnName)]] = names[x]
    }
    return(inputFrame)
  }, input, listNames, SIMPLIFY = FALSE)
  output = rbindlist(output, fill = TRUE)
  return(output)
}


unlistFrames2 = function(input){
  #BUG IN THIS FUNCTION, THAT IF A STRING FIELD HAS A "." IN IT, IT THEN OVERSPLITS IT 
  while(class(input[[1]]) == "list"){
    input = unlist(input, recursive =FALSE)
  }
  listNames = names(input)
  #listNames = gsub("\\.", "", listNames)
  listNames = lapply(listNames, function(x)(unlist(strsplit(x,"\\."))))
  output = mapply(function(inputFrame,names){
    for(x in 1:length(names)){
      columnName = paste0("level",x)
      inputFrame[[eval(columnName)]] = names[x]
    }
    return(inputFrame)
  }, input, listNames, SIMPLIFY = FALSE)
  output = rbindlist(output, fill = TRUE)
  return(output)
}



#Function to take in a directory where a series of homogenous CSV files are, and then read all the CSV files and rbindlist them together 
homoReadCSV = function(directory){
  current = getwd()
  setwd(directory)
  files = dir()
  CSVs = Filter(function(x)(right(x,4) == ".csv"), files)
  dataset = rbindlist(lapply(CSVs, fread))
  setwd(current)
  return(dataset)
}


#Filter an Nth nested list of dataframes, to filter out dataframes that are empty or less than a given rowcount, 
#and return only the items taht meet the conditon 
recursiveRowCountFilter = function(dataList, rowLimits = 0){
  if(class(dataList) %in% c("data.table","data.frame")){
    if (nrow(dataList) > rowLimits){
      return(dataList)
    }
    else {
      return(NULL)
    }
  }
  else{
    dataList = lapply(dataList, recursiveRowCountFilter, rowLimits) 
    #dataList = Filter(function(x)(!(is.null(x))), dataList)
    dataList = Filter(function(x)(length(x) > 0), dataList)
  }
  return(dataList)
}

rNames = function(listFrames){
 if(class(listFrames) %in% c("data.table","data.frame")){
   return(names(listFrames))}
  else{
    return(rNames(listFrames[[1]]))
  }
}


rView = function(listFrames){
  if(class(listFrames) %in% c("data.table","data.frame")){
    return(View(listFrames))}
  else{
    return(rView(listFrames[[1]]))
  }
}


#Function to merge two dataframes based on common column names
generalMerge = function(left, right, commonHeadings = NULL){
  if(is.null(commonHeadings)){
  leftHeadings = names(left)
  rightHeadings = names(right)
  commonHeadings = leftHeadings[leftHeadings %in% rightHeadings]
  }
  left$concat = left[[eval(commonHeadings[1])]]
  right$concat = right[[eval(commonHeadings[1])]]
  right[[eval(commonHeadings[1])]] = NULL 
  for(x in 2:length(commonHeadings)){
    left$concat = paste0(left$concat, left[[eval(commonHeadings[[x]])]])
    right$concat = paste0(right$concat, right[[eval(commonHeadings[[x]])]])
    right[[eval(commonHeadings[x])]] = NULL
  }
  combined = merge(x = left, y = right, by =  "concat")
  combined$concat = NULL
  return(combined)
}

#Iterative Split function that takes in a dataframe and splits it in the order of the vector of dimensions provided in sequence
multipleSplit = function(input, dimensions){
  output = input
  for( x in 1:length(dimensions)){

    output = recurse(output, function(y)(split(y,y[[eval(dimensions[x])]]))) 
    
  }
  return(output)
}


customLabel = function(x){
  if(max(x, na.rm = TRUE) > 10000){
    x = scales::comma(x)
  } else if (max(x, na.rm = TRUE) <= 1){
    x = scales::percent(x)
  } else{
    x = x
  }
  return(x)
}


fancy_scientific <- function(l) {
  # turn in to character string in scientific notation
  l <- format(l, scientific = TRUE)
  # quote the part before the exponent to keep all the digits
  l <- gsub("^(.*)e", "'\\1'e", l)
  # turn the 'e+' into plotmath format
  l <- gsub("e", "%*%10^", l)
  # return this as an expression
  parse(text=l)
}