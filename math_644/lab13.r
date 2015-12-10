#!/usr/bin/env Rscript
printf = function(fmt, ..., file = ""){cat(sprintf(fmt,...), file = file)}
puts = function(...){cat(sprintf("%s",...))}
options(show.signif.stars=F)
opt.data = read.csv("doe_data.csv")
opt.data$opt = factor(opt.data$opt)
attach(opt.data)
formula = time ~ opt*compiler
opt.aov = aov(formula)

