printf = function(fmt, ..., file = ""){cat(sprintf(fmt,...), file = file)}
options(show.signif.stars=F)
roadway.temps = load.csv("roadway.csv")
png("roadway_scatter.png")
plot(roadway.temps)
abline(roadway.lm)
dev.off()
cat(summary(roadway.lm))
