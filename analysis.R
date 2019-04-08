# Process raw data
files <- list.files("./data", pattern = "*.csv", full.names = T)
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

# Compute streaks
n_test = 100
data <- read.csv("data.csv")
players <- unique(data$player)
for (p in 1:length(players)) {
    cat("Processing player", p, "of", length(players), end="     \r")
    player <- players[p]
    rows <- data[data$player == player,]
    rows$test <- 0
    rows[1:n_test, "test"] <- 1
    rows$streak <- 1
    rows$win_streak <- 0
    rows$lose_streak <- 0
    rows$draw_streak <- 0
    rows$streak_daily <- 1
    rows$win_streak_daily <- 0
    rows$lose_streak_daily <- 0
    rows$draw_streak_daily <- 0
    
    for (i in (nrow(rows) - 1): 1) {
        if (rows[i, "result"] == rows[i + 1, "result"]) {
            rows[i, "streak"] <- rows[i + 1, "streak"] + 1
            if (rows[i, "date"] == rows[i + 1, "date"]) {
                rows[i, "streak_daily"] <- rows[i + 1, "streak_daily"] + 1
            }
        }
    }
    
    rows$win_streak <- rows$streak * (rows$result == "Won")
    rows$draw_streak <- rows$streak * (rows$result == "Draw")
    rows$lose_streak <- rows$streak * (rows$result == "Lost")
    rows$win_streak_daily <- rows$streak_daily * (rows$result == "Won")
    rows$draw_streak_daily <- rows$streak_daily * (rows$result == "Draw")
    rows$lose_streak_daily <- rows$streak_daily * (rows$result == "Lost")
    
    # Shift streaks to predict the next trial
    rows$streak <- c(rows$streak[-1], 1)
    rows$win_streak <- c(rows$win_streak[-1], 0)
    rows$draw_streak <- c(rows$draw_streak[-1], 0)
    rows$lose_streak <- c(rows$lose_streak[-1], 0)
    rows$win_streak_daily <- c(rows$win_streak_daily[-1], 0)
    rows$draw_streak_daily <- c(rows$draw_streak_daily[-1], 0)
    rows$lose_streak_daily <- c(rows$lose_streak_daily[-1], 0)
    
    data[data$player == player, "test"] = rows$test
    data[data$player == player, "streak"] = rows$streak
    data[data$player == player, "win_streak"] = rows$win_streak
    data[data$player == player, "draw_streak"] = rows$draw_streak
    data[data$player == player, "lose_streak"] = rows$lose_streak
    data[data$player == player, "win_streak_daily"] = rows$win_streak_daily
    data[data$player == player, "draw_streak_daily"] = rows$draw_streak_daily
    data[data$player == player, "lose_streak_daily"] = rows$lose_streak_daily
}
rm(rows, players, files, i, n_test, p, player)

# Prepare variables
data$win <- data$result == "Won"
data$draw <- data$result == "Draw"
data$lose <- data$result == "Lost"

# Simple bayesian
win_win <- merge(aggregate(win~win_streak, data, mean), aggregate(win~win_streak, data, length), by = 1)
colnames(win_win) <- c("streak", "p", "n")
win_lose <- merge(aggregate(win~lose_streak, data, mean), aggregate(win~lose_streak, data, length), by = 1, suffixes = "_count")
colnames(win_lose) <- c("streak", "p", "n")

win_win$se <- sqrt(win_win$p*(1-win_win$p)/win_win$n)
win_win$lwr <- win_win$p - qnorm(0.975) * win_win$se
win_win$upr <- win_win$p + qnorm(0.975) * win_win$se

win_lose$se <- sqrt(win_lose$p*(1-win_lose$p)/win_lose$n)
win_lose$lwr <- win_lose$p - qnorm(0.975) * win_lose$se
win_lose$upr <- win_lose$p + qnorm(0.975) * win_lose$se

png("streaks.png", width = 1500, height = 1000, res=200)
par(mar=c(5.1, 4.1, 1.1, 1.1))
x = 1:14
y = win_win$p[x+1]
lwr = win_win$lwr[x+1]
upr = win_win$upr[x+1]
clr = rgb(0, 0.5, 0)
plot(x, y, type="l", xlab = "Streak", ylab = "Win Probability", ylim = c(0.15, 0.9), col=clr)
points(x, y, pch=19, col=clr)
segments(x, lwr, x, upr, col=clr)
segments(x-0.05, lwr, x+0.05, lwr, col=clr)
segments(x-0.05, upr, x+0.05, upr, col=clr)
x = 1:9
y = win_lose$p[x+1]
lwr = win_lose$lwr[x+1]
upr = win_lose$upr[x+1]
clr = rgb(0.8, 0, 0)
lines(x, y, col=clr)
points(x, y, pch=19, col=clr)
segments(x, lwr, x, upr, col=clr)
segments(x-0.05, lwr, x+0.05, lwr, col=clr)
segments(x-0.05, upr, x+0.05, upr, col=clr)
abline(a=0.5, b = 0, col=rgb(0, 0, 0, 0.2), lty=2)
legend("right", c("Winning streak", "Losing streak"), lty=1, pch=19, col=c(rgb(0, 0.5, 0), rgb(0.8, 0, 0)), inset=0.01)
graphics.off()

# Player analysis
player = unique(data$player)
fire = sapply(player, function(x) { m <- summary(lm(scale(win)~scale(win_streak), data[data$player==x,])); coef(m)[c(2, 4)]*c(1, qt(0.975, m$df[2])) })
tilt = sapply(player, function(x) { m <- summary(lm(scale(win)~scale(lose_streak), data[data$player==x,])); coef(m)[c(2, 4)]*c(1, qt(0.975, m$df[2])) })
s = data.frame(player,fire=fire[1,], fire_e=fire[2,],tilt=tilt[1,],tilt_e=tilt[2,])
s[,-1] = round(s[,-1]*100)
s$fire_desc = paste(s$fire, "±", s$fire_e)
s$tilt_desc = paste(s$tilt, "±", s$tilt_e)
View(s)
