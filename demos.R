
# interpm

source('interpm.R')

dat <- data.frame(time = 1:30, a = rnorm(30), b = rnorm(30), c = rnorm(30))
dat[5:10, -1] <- NA
dat[20:22, 'a'] <- NA

dat
dat2 <- interpm(dat, 'time', c('a', 'b', 'c'))
dat2
