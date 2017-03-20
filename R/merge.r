#!/usr/bin/env Rscript
fall <- read.csv("fall_prepost.csv")
colnames(fall) <- tolower(colnames(fall))
fall$grade <- factor(fall$grade, ordered = TRUE)

winter <- read.csv("winter_prepost.csv")
colnames(winter) <- tolower(colnames(winter))
winter$grade <- factor(winter$grade, ordered = TRUE)

fall$x <- NULL
fall$post <- !fall$post
colnames(fall)[colnames(fall) == "post"] <- "pre"

winter$pre[is.na(winter$pre)] <- 0
winter$pre <- as.logical(winter$pre)

fall$fall.y.n <- NA
fall$survey <- NA

winter$season <- "winter"
fall$season <- "fall"

both <- rbind(fall,winter)
write.csv(both, "all_prepost.csv", na = "", row.names = FALSE, quote = FALSE)
# union(setdiff(names(fall),names(winter)),setdiff(names(winter),names(fall)))
