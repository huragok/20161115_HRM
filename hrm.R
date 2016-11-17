library(gdata)
library(zoo)
library(reshape2)
library(ggplot2)
library(plyr)

#filename <- "2016-11-10 134734"
filename <- "2016-11-15 105610"


df <- read.xls(paste(filename,".xls", sep=""), sheet="Melt Curve Raw Data", pattern="Fluorescence")
df[df == ""] <- NA
df$Well.Position=droplevels(na.locf(df$Well.Position))
#str(df)
#str(sapply(df, function(x) sum(is.na(x))))

n.temperature <- 100;
temperature.interpolation <- seq(min(df$Temperature), max(df$Temperature), length=n.temperature)

normalize <- function(chunk) {
  str(chunk)
  temperature <- chunk$Temperature
  fluorescence <- chunk$Fluorescence
  fluorescence <- (fluorescence - min(fluorescence)) / (max(fluorescence) - min(fluorescence))
  approx(temperature, fluorescence, temperature.interpolation, method="linear", rule = 2)$y
}

# Plot the Normalized figure
normalized <- do.call(cbind.data.frame, lapply(split(df,df$Well.Position), normalize))
normalized$Temperature <- temperature.interpolation
normalized.melt <- melt(normalized, id.vars = "Temperature", variable.name = "Well", 
                        value.name = "Normalized")

ggplot(normalized.melt, aes(x=Temperature, y=Normalized, color=Well)) +
  theme_bw() +
  geom_line()
ggsave(paste(filename, "_normalized.pdf", sep=""), width=8, height=5)

# Plot the Overlaid figure
overlaid <- ddply(normalized.melt, .(Temperature), summarise, Overlaid=mean(Normalized))
ggplot(overlaid, aes(x=Temperature, y=Overlaid)) +
  theme_bw() +
  geom_line()
ggsave(paste(filename, "_overlaid.pdf", sep=""), width=8, height=5)

# Plot the Delta Normalized & Overlaid figure

idx.temperature.critical <- which.min(abs(temperature.interpolation - 80))
fluorescence.critical <-unlist(normalized[idx.temperature.critical, !(colnames(normalized) == "Temperature")])
which.median <- function(x) which.min(abs(x - median(x)))
well.ref <- names(which.median(fluorescence.critical))

delta <- normalized;
delta[, !(colnames(delta) == "Temperature")] <- delta[, !(colnames(delta) == "Temperature")] - delta[, well.ref]
delta.melt <- melt(delta, id.vars = "Temperature", variable.name = "Well", 
                        value.name = "Delta")
ggplot(delta.melt, aes(x=Temperature, y=Delta, color=Well)) +
  theme_bw() +
  geom_line()
ggsave(paste(filename, "_delta.pdf", sep=""), width=8, height=5)
