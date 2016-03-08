#!/usr/bin/env Rscript
## Options/util functions
printf <- function(fmt, ..., file = ""){cat(sprintf(fmt,...), file = file)}
puts <- function(...){cat(sprintf("%s",...))}
agrepl <- function(...){any(grepl(...))}
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}
swapcol <- function(data, a, b){
    a.index <- which(colnames(data) == a)
    b.index <- which(colnames(data) == b)
    temp <- data[[a.index]]
    data[[a.index]] <- data[[b.index]]
    data[[b.index]] <- temp
    colnames(data)[[a.index]] <- b
    colnames(data)[[b.index]] <- a
    return(data)
}
agree.factor <- function(data){
    factor(data, levels = c("Strongly Disagree","Disagree",
                            "Agree","Strongly Agree"),
           ordered = TRUE)
}
yesno.factor <- function(data){
    factor(data, levels = c("No", "Not sure", "Yes"),
           ordered = TRUE)
}
yesno.bool.factor <- function(data){
    factor(data, levels = c("No", "Yes"),
           ordered = TRUE)
}
yn.bool.factor <- function(data){
    factor(data, levels = c("N", "Y"),
           ordered = TRUE)
}
days.factor <- function(data){
    factor(data, levels = c("0 days","1-2 days","3-5 days","6-9 days",
                            "10-19 days","20-29 days","All 30 days"),
           ordered = TRUE)
}
grade.factor <- function(data){
    grades <- sub("grade ([1-9][12]?)", "\\1", data, ignore.case = TRUE);
    factor(grades, levels = seq(1:12), ordered = TRUE);
}
gender.factor <- function(data){
    factor(data, levels = c("Male", "Female"), ordered = TRUE)
}
order.factors <- function(data){
    if(!is.factor(data)){
        return(data)
    } else {
        ##this is a really lazy way to do this
        ##Also it mostly assumes multiple factors don't share catagories
        if(agrepl("agree", levels(data), ignore.case = TRUE)){
            return(agree.factor(data));
        } else if(agrepl("days", levels(data), ignore.case = TRUE)){
            return(days.factor(data));
        } else if(agrepl("yes", levels(data), ignore.case = TRUE)){
            if(agrepl("not sure", levels(data), ignore.case = TRUE)){
                return(yesno.factor(data));
            } else {
                return(yesno.bool.factor(data));
            }            
        } else if(agrepl("grade", levels(data), ignore.case = TRUE)){
            return(grade.factor(data))
        } else if(agrepl("male", levels(data), ignore.case = TRUE)){
            return(gender.factor(data))
        } else if(agrepl("y", levels(data), ignore.case = TRUE)){
            return(yn.bool.factor(data));
        } else {
            return(data);
        }
    }
}
order.numeric <- function(data){
    temp <- order.factors(data)
    if(!identical(temp, data)){
        return(as.numeric(order.factors(data)))
    } else {
        return(temp)
    }
}
args <- commandArgs(TRUE)
for(file in args){
    outfile = sub(".csv$", "_converted.csv", file)
    data = read.csv(file)
    ## Normalize
    colnames(data) <- tolower(colnames(data))
    if(agrepl("grade", colnames(data))){
        data$grade = grade.factor(data$grade)
    }
    data <- lapply(data, order.numeric)
    write.csv(data, outfile, na = "", row.names = FALSE)
}
