#!/usr/bin/env Rscript
## Options/util functions
library(gtools) #for permutations
args <- commandArgs(TRUE)
printf <- function(fmt, ..., file = ""){cat(sprintf(fmt,...), file = file)}
puts <- function(...){cat(sprintf("%s",...))}
agrepl <- function(...){any(grepl(...))}
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
lmapply <- function(fun,...){
    mapply(fun,...,SIMPLIFY = FALSE, USE.NAMES = FALSE)
}
agree.factor <- function(data){
    na.replace(factor(data, levels = c("Strongly Disagree","Disagree",
                                       "Neither", "Agree","Strongly Agree"),
                      ordered = TRUE), "Neither")
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
                            "10-19 days","20-29 days","30 days"),
           ordered = TRUE)
}
season.factor <- function(data){
    factor(data, levels = c("Fall", "Winter"), ordered = TRUE)
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
        } else if(agrepl("male", levels(data), ignore.case = TRUE)){
            return(gender.factor(data));
        } else if(agrepl("y", levels(data), ignore.case = TRUE)){
            return(yn.bool.factor(data));
        } else if(agrepl("fall", levels(data), ignore.case = TRUE)){
            return(season.factor(data));
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
#options(show.signif.stars = FALSE)

## Load and format data
##winter <- read.csv(args)
data <- read.csv(args)
## grade shouldn't be a numeric value
data$grade <- factor(data$grade, ordered = TRUE)

## Start / end indices of the dependent and indepent variables

data.start <- which(names(data) == "alcohol")
data.end <- length(data)

dv.start <- data.start
dv.end <- which(names(data) == "parentcontracts")

iv.start <- dv.end+1
iv.end <- length(data)

data[c(data.start:data.end)] <- lapply(data[c(data.start:data.end)],
                                         order.factors)
data.factors <- data
data[c(data.start:data.end)] <- lapply(data[c(data.start:data.end)],
                                         as.numeric)
# data <- na.omit(data) ## Get rid of NAs

pre.indices <- which(data$pre == 1)
post.indices <- which(data$pre == 0)

winter.indices <- which(data$season == "winter")
fall.indices <- which(data$season == "fall")

winter.pre.indices <- intersect(winter.indices, pre.indices)
winter.post.indices <- intersect(winter.indices, post.indices)
winter.pre.skipped.indices <- which(Reduce(function(x,y){x&y},
                                           lapply(data[c(dv.start:dv.end)],is.na)))

fall.pre.indices <- intersect(fall.indices, pre.indices)
fall.post.indices <- intersect(fall.indices, post.indices)

male.indices <- which(data$gender == "Male")
female.indices <- which(data$gender == "Female")

fall.post.avg <- lapply(data[c(data.start:data.end)],
                        function(x){mean(na.omit(x[fall.post.indices]))})

data[winter.pre.skipped.indices,data.start:data.end] <- fall.post.avg

na.indices <- which(Reduce(function(x,y){x|y},
                           lapply(data[c(dv.start:dv.end)],is.na)))


## Now get rid of NAs (-na.indices, gives all rows but those with NAs)
data <- data[-na.indices,]

fall.prepost.t <- lapply(data[c(data.start:data.end)],
                         function(x){t.test(x[fall.pre.indices],
                                            x[fall.post.indices],
                                            var.equal = FALSE)})

winter.prepost.t <- lapply(data[c(data.start:data.end)],
                         function(x){t.test(x[winter.pre.indices],
                                            x[winter.post.indices],
                                            var.equal = FALSE)})
data.prepost.t <- lapply(data[c(data.start:data.end)],
                         function(x){t.test(x[fall.pre.indices],
                                            x[winter.post.indices],
                                            var.equal = FALSE)})
second.prepost.t <- mapply(
    function(x,y){fall.post.avg = rep(y, length(x[winter.post.indices]));
        winter.post = x[winter.post.indices];
        t.test(fall.post.avg, winter.post,var.equal = FALSE)},
    SIMPLIFY = FALSE, USE.NAMES = TRUE,
    data[c(data.start:data.end)], fall.post.avg)


fall.prepost.signif <- Filter(function(x){x$p.value < .1}, fall.prepost.t)
winter.prepost.signif <- Filter(function(x){x$p.value < .1}, winter.prepost.t)
data.prepost.signif <- Filter(function(x){x$p.value < .1}, data.prepost.t)
second.prepost.signif <- Filter(function(x){x$p.value < .1}, second.prepost.t)

print("T tests for Fall pre - Fall post")
print(fall.prepost.signif)
print("T tests for Winter pre - Winter post")
print(winter.prepost.signif)
print("T tests for Fall pre - Winter post")
print(data.prepost.signif)
print("T tests for Average fall post - Winter post")
print(second.prepost.signif)

## write.csv(data, sub(".csv$","_clean.csv",args[1]),na = "", row.names = FALSE)
## data.gender.t <- lapply(data[c(data.start:data.end)],
##                         function(x){t.test(x[male.indices],x[female.indices])})
## data.gender.signif <- Filter(function(x){x$p.value < .2}, data.gender.t)

## grades <- c(9:12)
## grade.indices <- as.vector(sapply(grades, function(x){which(data$grade == x)}))
## data.grades.t <- lapply(data[data.start:data.end],function(x){
##     apply(combinations(4,2), 1, function(y){
##         eval(bquote(t.test(x[unlist(grade.indices[.(y[1])])],
##                            x[unlist(grade.indices[.(y[2])])])))})})

## temp <- lapply(data.grades.t, function(x){Filter(function(y){y$p.value < .2},x)})
## data.grades.signif <- temp[lapply(temp,length)>0]

## basic.indices <- c(data.start:(data.start+8))
## parents.indices <- grep("^parents", names(data))
## coaches.indices <- grep("^coaches", names(data))
## social.indices <- grep("^social", names(data))
## community.indices <- grep("^community", names(data))
## contract.indices <- grep("contract", names(data))
## ## This is kind of a hack, we figure out which columns corspond to 30 day usage
## ## by testing the number of possible values for each column

## use.indices <- (which(as.vector(lapply(data[c(data.start:data.end)],
##                                        function(x){max(na.omit(x))>=7}),
##                                 mode="logical"))+(data.start-1))


## most.indices <- c(parents.indices, coaches.indices,
##                   social.indices,community.indices,
##                   contract.indices,basic.indices, use.indices)
## other.indices <- setdiff(c(data.start:data.end), most.indices)

## ## Generate right hand side of regression formulas
## parents.rhs <- paste(names(data)[parents.indices], collapse=" + ")
## coaches.rhs <- paste(names(data)[coaches.indices], collapse=" + ")
## social.rhs <- paste(names(data)[social.indices], collapse=" + ")
## community.rhs <- paste(names(data)[community.indices], collapse=" + ")
## contract.rhs <- paste(names(data)[contract.indices], collapse=" + ")
## basic.rhs <- paste(names(data)[basic.indices], collapse=" + ")
## other.rhs <- paste(names(data)[other.indices], collapse=" + ")
## rhs <- c(parents.rhs, coaches.rhs, social.rhs, community.rhs,
##          contract.rhs, basic.rhs, other.rhs)
## use.lm <- function(use){
##     lapply(rhs,
##            function(x){
##                eval(bquote(lm(.(as.formula(sprintf("%s ~ %s",use,x))),
##                               data = data)))
##            })
## }
## reg <- lapply(names(data)[use.indices], use.lm)
## anovas <- lapply(unlist(reg, recursive = FALSE), anova)


## print("Significant values from pre-post t tests:")
## print(data.post.signif)
## print("Significant values from male-female t tests:")
## print(data.gender.signif)
## print("Significant values from grade based t tests:")
## print(data.grades.signif)
## print("Regressions:")
## print(reg)
## print("ANOVAs:")
## print(anovas)
## print("Stepwise regression:")
## lapply(unlist(reg, recursive = FALSE), function(x){printf("Starting regression\n\n")
##     step(x)
##     printf("Done Regression\n\n")})
