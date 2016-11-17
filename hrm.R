library(gdata)
library(zoo)
library(reshape2)
library(ggplot2)
library(plyr)

# Parameter settings:
filenames <- c("1", "2", "3") # Filenames
n.temperature <- 50 # Number of points on which to interpolate the F-T curves
min.temperature <- 82.5 # Minimum temperature, on which the fluorescence is mapped to 1
max.temperature <- 95 # Maximum temperature, on which the fluorescence is mapped to 0.temperature <- 
ref.temperature <- 89 # Reference temperature, select the well position whose fluorescence is the median among all wells at this temperature 

# Import the data and assemble the dataframe
df.all <- data.frame(Filename=character(),
                 Well.Position=character(), 
                 Temperature=double(),
                 Fluorescence=double(),
                 stringsAsFactors=TRUE) # The data may be in multiple files

drops <- c("Well", "Reading", "Derivative")
for (filename in filenames) {
  df <- read.xls(paste(filename,".xls", sep=""), sheet="Melt Curve Raw Data", pattern="Fluorescence", stringsAsFactors=FALSE, na.strings="")
  df <- df[, !(names(df) %in% drops)]
  df$Well.Position <- na.locf(df$Well.Position)
  
  #df$Well.Position <- droplevels(na.locf(df$Well.Position))
  #df$Well.Position <- paste(df$Well.Position, filename, sep="-")
  df$Filename <- filename
  df.all <- rbind(df.all, df)
}
df.all$Well.Position <- as.factor(df.all$Well.Position)
df.all$Filename <- as.factor(df.all$Filename)

# Interpolation and normalization
temperature.interpolation <- seq(min.temperature, max.temperature, length=n.temperature)

normalize <- function(chunk) {
  # Interpolated fluorescence
  fluorescence <- approx(chunk$Temperature, chunk$Fluorescence, temperature.interpolation, method="linear", rule = 2)$y
  # Normalized fluorescence
  (fluorescence - min(fluorescence)) / (max(fluorescence) - min(fluorescence))
}

# Plot the Normalized figure
normalized <- do.call(cbind.data.frame, lapply(split(df.all,df.all[,c("Well.Position", "Filename")],sep="-"), normalize))
normalized$Temperature <- temperature.interpolation
normalized.melt <- melt(normalized, id.vars = "Temperature", variable.name = "FilenameWellPosition", 
                        value.name = "Normalized")
normalized.melt$Well.Position <- as.factor(substr(normalized.melt$FilenameWellPosition, 1, 3))
normalized.melt$Filename <- as.factor(substr(normalized.melt$FilenameWellPosition, 5, 5))

# Plot the Normalized figure
ggplot(normalized.melt, aes(x=Temperature, y=Normalized, color=FilenameWellPosition)) +
  theme_bw() +
  geom_line()
ggsave(paste("all", "_normalized.pdf", sep=""), width=8, height=5)

for (w in levels(normalized.melt$Well.Position)) {
  ggplot(normalized.melt[normalized.melt$Well.Position==w,], aes(x=Temperature, y=Normalized, color=Filename)) +
    theme_bw() +
    geom_line()
  ggsave(paste(w, "_normalized.pdf", sep=""), width=8, height=5)
}

# Plot the Overlaid figure
overlaid <- ddply(normalized.melt, .(Temperature), summarise, Overlaid=mean(Normalized))
ggplot(overlaid, aes(x=Temperature, y=Overlaid)) +
  theme_bw() +
  geom_line()
ggsave(paste("all", "_overlaid.pdf", sep=""), width=8, height=5)

for (w in levels(normalized.melt$Well.Position)) {
  overlaid <- ddply(normalized.melt[normalized.melt$Well.Position==w,], .(Temperature), summarise, Overlaid=mean(Normalized))
  ggplot(overlaid, aes(x=Temperature, y=Overlaid)) +
    theme_bw() +
    geom_line()
  ggsave(paste(w, "_overlaid.pdf", sep=""), width=8, height=5)
}

# Plot the Delta Normalized & Overlaid figure
idx.temperature.critical <- which.min(abs(temperature.interpolation - ref.temperature))
fluorescence.critical <-unlist(normalized[idx.temperature.critical, !(colnames(normalized) == "Temperature")])
which.median <- function(x) which.min(abs(x - median(x)))
well.ref <- names(which.median(fluorescence.critical))

delta <- normalized;
delta[, !(colnames(delta) == "Temperature")] <- delta[, !(colnames(delta) == "Temperature")] - delta[, well.ref]
delta.melt <- melt(delta, id.vars = "Temperature", variable.name = "FilenameWellPosition", 
                        value.name = "Delta")
ggplot(delta.melt, aes(x=Temperature, y=Delta, color=FilenameWellPosition)) +
  theme_bw() +
  geom_line()
ggsave(paste("all", "_delta.pdf", sep=""), width=8, height=5)

for (w in levels(normalized.melt$Well.Position)) {
  df.w = df.all[df.all$Well.Position==w,]
  normalized.w <- do.call(cbind.data.frame, lapply(split(df.w,df.w$Filename), normalize))
  normalized.w$Temperature <- temperature.interpolation
  fluorescence.critical <-unlist(normalized.w[idx.temperature.critical, !(colnames(normalized.w) == "Temperature")])
  which.median <- function(x) which.min(abs(x - median(x)))
  well.ref <- names(which.median(fluorescence.critical))
  
  delta <- normalized.w
  delta[, !(colnames(delta) == "Temperature")] <- delta[, !(colnames(delta) == "Temperature")] - delta[, well.ref]
  delta.melt <- melt(delta, id.vars = "Temperature", variable.name = "Filename", 
                     value.name = "Delta")
  ggplot(delta.melt, aes(x=Temperature, y=Delta, color=Filename)) +
    theme_bw() +
    geom_line()
  ggsave(paste(w, "_delta.pdf", sep=""), width=8, height=5)
}
