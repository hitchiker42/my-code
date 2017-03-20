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
agree.factor <- function(data){
##    data[which(data == "")] = "Neither"
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
                            "10-19 days","20-29 days","All 30 days"),
           ordered = TRUE)
}
grade.factor <- function(data){
    grades <- sub("grade ([1-9][12]?)", "\\1", data, ignore.case = TRUE);
    factor(grades, levels = seq(1:12), ordered = TRUE);
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
#options(show.signif.stars = FALSE)

## Load and format data
##winter <- read.csv(args)
winter <- read.csv("winter_pre_post_clean.csv")
## Normalize variable names to lowercase
colnames(winter)[which(colnames(winter) == "sleep")] <- "sleep.2"
colnames(winter)[which(colnames(winter) == "Sleep")] <- "sleep"
colnames(winter) <- tolower(colnames(winter))
## grade shouldn't be a numeric value
winter$grade <- grade.factor(winter$grade)
## ## move post to the second column (which is a copy of grade for some reason)
## ## and remove the extra column
## winter <- swapcol(winter, "x", "post")
## winter <- swapcol(winter, "grade", "post")
## winter <- subset(winter, select = -c(x))

## Replace NA with 0 for the "pre" column
winter$pre[is.na(winter$pre)] <- 0

data.start <- 6
data.end <- (length(winter)-1)

winter[c(data.start:data.end)] <- lapply(winter[c(data.start:data.end)],
                                         order.factors)
winter.factors <- winter
winter[c(data.start:data.end)] <- lapply(winter[c(data.start:data.end)],
                                         as.numeric)
winter <- na.omit(winter) ## Get rid of NAs

pre.indices <- which(winter$pre == 1)
post.indices <- which(winter$pre == 0)
winter.post.t <- lapply(winter[c(data.start:data.end)],
                      function(x){t.test(x[pre.indices],x[post.indices],
                                         var.equal = FALSE, alternative = "less")})
winter.post.signif <- Filter(function(x){x$p.value < .2}, winter.post.t)

male.indices <- which(winter$gender == "Male")
female.indices <- which(winter$gender == "Female")
winter.gender.t <- lapply(winter[c(data.start:data.end)],
                        function(x){t.test(x[male.indices],x[female.indices])})
winter.gender.signif <- Filter(function(x){x$p.value < .2}, winter.gender.t)

grades <- c(9:12)
grade.indices <- as.vector(sapply(grades, function(x){which(winter$grade == x)}))
winter.grades.t <- lapply(winter[data.start:data.end],function(x){
    apply(combinations(4,2), 1, function(y){
        eval(bquote(t.test(x[unlist(grade.indices[.(y[1])])],
                           x[unlist(grade.indices[.(y[2])])])))})})

temp <- lapply(winter.grades.t, function(x){Filter(function(y){y$p.value < .2},x)})
winter.grades.signif <- temp[lapply(temp,length)>0]

basic.indices <- c(data.start:(data.start+8))
parents.indices <- grep("^parents", names(winter))
coaches.indices <- grep("^coaches", names(winter))
social.indices <- grep("^social", names(winter))
community.indices <- grep("^community", names(winter))
contract.indices <- grep("contract", names(winter))
## This is kind of a hack, we figure out which columns corspond to 30 day usage
## by testing the number of possible values for each column

use.indices <- (which(as.vector(lapply(winter[c(data.start:data.end)],
                                       function(x){max(na.omit(x))>=7}),
                                mode="logical"))+(data.start-1))


most.indices <- c(parents.indices, coaches.indices,
                  social.indices,community.indices,
                  contract.indices,basic.indices, use.indices)
other.indices <- setdiff(c(data.start:data.end), most.indices)

## Generate right hand side of regression formulas
parents.rhs <- paste(names(winter)[parents.indices], collapse=" + ")
coaches.rhs <- paste(names(winter)[coaches.indices], collapse=" + ")
social.rhs <- paste(names(winter)[social.indices], collapse=" + ")
community.rhs <- paste(names(winter)[community.indices], collapse=" + ")
contract.rhs <- paste(names(winter)[contract.indices], collapse=" + ")
basic.rhs <- paste(names(winter)[basic.indices], collapse=" + ")
other.rhs <- paste(names(winter)[other.indices], collapse=" + ")
rhs <- c(parents.rhs, coaches.rhs, social.rhs, community.rhs,
         contract.rhs, basic.rhs, other.rhs)
use.lm <- function(use){
    lapply(rhs,
           function(x){
               eval(bquote(lm(.(as.formula(sprintf("%s ~ %s",use,x))),
                              data = winter)))
           })
}
reg <- lapply(names(winter)[use.indices], use.lm)
anovas <- lapply(unlist(reg, recursive = FALSE), anova)


print("Significant values from pre-post t tests:")
print(winter.post.signif)
print("Significant values from male-female t tests:")
print(winter.gender.signif)
print("Significant values from grade based t tests:")
print(winter.grades.signif)
print("Regressions:")
print(reg)
print("ANOVAs:")
print(anovas)
print("Stepwise regression:")
lapply(unlist(reg, recursive = FALSE), function(x){printf("Starting regression\n\n")
    step(x)
    printf("Done Regression\n\n")})
