files <- list.files(pattern = "*.csv")
data = NULL

for (i in 1:length(files)) {
    cat(sprintf("Reading file %d of %d . . .   ", i, length(files)), end="\r")
    if (is.null(data)) {
        data <- read.csv(files[i])
    } else {
        data <- rbind(data, read.csv(files[i]))
    }
}
cat("Done!", paste(rep(" ", 50), collapse = ""))

write.csv(data, "data.csv", row.names = F)
