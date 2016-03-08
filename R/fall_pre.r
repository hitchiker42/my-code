#!/usr/bin/env Rscript
capwords <- function(s, strict = FALSE) {
    cap <- function(s) paste(toupper(substring(s, 1, 1)),
    {s <- substring(s, 2); if(strict) tolower(s) else s},
    sep = "", collapse = " " )
    sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}
args <- commandArgs(TRUE)
png.w <- png.h <- 640
agree.labels <- c("Strongly Disagree","Disagree", "Agree","Strongly Agree")
yesno.labels <- c("No", "Not sure", "Yes")
days.labels <- c("0 days","1-2 days","3-5 days","6-9 days",
                 "10-19 days","20-29 days","All 30 days")
convert.factor <- function(x){
    if(is.factor(x)){return(x)}
    max.val = max(x,na.rm=T)
    if(max.val == 7){
        return(factor(x,levels=seq(1:max.val),labels=days.labels, ordered=TRUE))
    } else if(max.val == 4){
        return(factor(x,levels=seq(1:max.val),labels=agree.labels, ordered=TRUE))
    } else if(max.val == 3){
        return(factor(x,levels=seq(1:max.val),labels=yesno.labels, ordered=TRUE))
    } else {
        return(x)
    }
}

gen.plot <- function(var, data){

    var.name <- capwords(deparse(substitute(var)))
    col <- eval(substitute(var),data)
    temp <- as.data.frame(tapply(col, list(data$dose,data$grade),
                                 function(y){prop.table(table(y))*100}))
    by.grade <- lapply(temp,function(x){do.call(rbind, x)})
    png(sprintf("%s.png",var.name), width = png.w, height = png.h)
    par(mfrow = c(2,2))
    for(i in seq(1:4)){
        grade <- i+8
        loc <- if(sum(by.grade[[i]][1:3,1]) >
                  sum(by.grade[[i]][1:3,ncol(by.grade[[i]])])){
                   "topright"
               } else {
                   "topleft"
               }
        barplot(by.grade[[i]], col=c("black","green","purple"),
                legend.text = c("0","1","2"), beside = T,
                ylab = "percent", main = var.name,
                sub = paste(grade,"th grade",sep=""),
                names.arg = levels(col), args.legend = list(x=loc))
    }
    dev.off()
}
gen.plot.1 <- function(var, data){

    var.name <- capwords(deparse(substitute(var)))
    col <- eval(substitute(var),data)
    temp <- as.data.frame(tapply(col, list(data$dose,data$grade),
                                 function(y){prop.table(table(y))*100}))
    by.grade <- lapply(temp,function(x){do.call(rbind, x)})
    png(sprintf("%s_1.png",var.name),width = png.w, height = png.h)
    par(mfrow = c(2,2))
    for(i in seq(1:4)){
        grade <- i+8
        loc <- if(sum(by.grade[[i]][1:3,1]) >
                  sum(by.grade[[i]][1:3,ncol(by.grade[[i]])])){
                   "topright"
               } else {
                   "topleft"
               }
        ymin <- trunc(min(by.grade[[i]][,1])/10)*10
        ymax <- min(100, ceiling(max(by.grade[[i]][,1])/10)*10)
        barplot(by.grade[[i]][1:3,1], col=c("black","green","purple"),
                legend.text = c("0","1","2"), beside = T,
                ylab = "percent", main = var.name, ylim = c(ymin,ymax),
                sub = paste(grade,"th grade",sep=""), xpd = F,
                names.arg = head(levels(col),n=1),
                args.legend = list(x=loc))
    }
    dev.off()
}
data <- read.csv(args)
colnames(data)[which(colnames(data) == "sleep")] <- "sleep.2"
colnames(data)[which(colnames(data) == "Sleep")] <- "sleep"
colnames(data) <- tolower(colnames(data))
data$grade <- factor(data$grade, ordered=TRUE)
data$dose <- factor(data$dose, ordered=TRUE)
data <- as.data.frame(lapply(data, convert.factor))
vars <- c("sleep","alcohol","marijuana","drugs","meals","tobacco")
lapply(Map(as.name,vars), function(x){eval(bquote(gen.plot(.(x),data)))})
lapply(Map(as.name,vars), function(x){eval(bquote(gen.plot.1(.(x),data)))})
