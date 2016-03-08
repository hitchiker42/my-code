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
    factor(data, levels = c("Strongly Disagree","Disagree",
                            "Agree","Strongly Agree"),
           ordered = TRUE)
}
yesno.factor <- function(data){
    factor(data, levels = c("No", "Not sure", "Yes"),
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
        #this is a really lazy way to do this
        if(any(grepl("Agree",levels(data)))){
            return(agree.factor(data));
        } else if(any(grepl("days",levels(data)))){
            return(days.factor(data));
        } else if(any(grepl("Yes",levels(data)))){
            return(yesno.factor(data));
        } else if(any(grepl("grade", levels(data), ignore.case = TRUE))){
            return(grade.factor(data))
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
fall <- read.csv("fall_prepost.csv")
## Normalize variable names to lowercase
colnames(fall) <- tolower(colnames(fall))
## grade shouldn't be a numeric value
fall$grade <- grade.factor(fall$grade)
## move post to the second column (which is a copy of grade for some reason)
## and remove the extra column
fall <- swapcol(fall, "x", "post")
fall <- swapcol(fall, "grade", "post")
fall <- subset(fall, select = -c(x))

fall[c(5:length(fall))] <- lapply(fall[c(5:length(fall))], order.factors)
fall.factors <- fall
fall[c(5:length(fall))] <- lapply(fall[c(5:length(fall))], as.numeric)

pre.indices <- which(fall$post == 0)
post.indices <- which(fall$post == 1)
fall.post.t <- lapply(fall[c(5:length(fall))],
                      function(x){t.test(x[pre.indices],x[post.indices],
                                         var.equal = FALSE)})
fall.post.signif <- Filter(function(x){x$p.value < .2}, fall.post.t)

male.indices <- which(fall$gender == "Male")
female.indices <- which(fall$gender == "Female")
fall.gender.t <- lapply(fall[c(5:length(fall))],
                        function(x){t.test(x[male.indices],x[female.indices])})
fall.gender.signif <- Filter(function(x){x$p.value < .2}, fall.gender.t)

grades <- c(9:12)
grade.indices <- as.vector(sapply(grades, function(x){which(fall$grade == x)}))
fall.grades.t <- lapply(fall[5:length(fall)],function(x){
    apply(combinations(4,2), 1, function(y){
        eval(bquote(t.test(x[unlist(grade.indices[.(y[1])])],
                           x[unlist(grade.indices[.(y[2])])])))})})

temp <- lapply(fall.grades.t, function(x){Filter(function(y){y$p.value < .2},x)})
fall.grades.signif <- temp[lapply(temp,length)>0]

basic.indices <- c(5:13)
parents.indices <- grep("^parents", names(fall))
coaches.indices <- grep("^coaches", names(fall))
social.indices <- grep("^social", names(fall))
community.indices <- grep("^community", names(fall))
contract.indices <- grep("contract", names(fall))
use.indices <- (which(as.vector(lapply(fall[c(5:length(fall))],
                                      function(x){max(x)>5}),mode="logical"))+4)

most.indices <- c(parents.indices, coaches.indices,
                  social.indices,community.indices,
                  contract.indices,basic.indices, use.indices)
other.indices <- setdiff(c(5:length(fall)), most.indices)

## Generate right hand side of regression formulas
parents.rhs <- paste(names(fall)[parents.indices], collapse=" + ")
coaches.rhs <- paste(names(fall)[coaches.indices], collapse=" + ")
social.rhs <- paste(names(fall)[social.indices], collapse=" + ")
community.rhs <- paste(names(fall)[community.indices], collapse=" + ")
contract.rhs <- paste(names(fall)[contract.indices], collapse=" + ")
basic.rhs <- paste(names(fall)[basic.indices], collapse=" + ")
other.rhs <- paste(names(fall)[other.indices], collapse=" + ")
rhs <- c(parents.rhs, coaches.rhs, social.rhs, community.rhs,
         contract.rhs, basic.rhs, other.rhs)
reg <- lapply(names(fall)[use.indices],
             function(use){
                 lapply(rhs,
                        function(x){
                            eval(bquote(lm(.(as.formula(sprintf("%s ~ %s",use,x))),
                                           data = fall)))})})
anovas <- lapply(unlist(reg, recursive = FALSE), anova)

print("Significant values from pre-post t tests:")
print(fall.post.signif)
print("Significant values from male-female t tests:")
print(fall.gender.signif)
print("Significant values from grade based t tests:")
print(fall.grades.signif)
print("Regressions:")
print(reg)
print("anovas:")
print(anovas)
