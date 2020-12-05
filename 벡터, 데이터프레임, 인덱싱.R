# p40
vec <- c(6, 1, 3, 6, 10, 5)

df <- data.frame(name = c("John", "Paul", "George", "Rimgo"),
                 birth = c(1940, 1942, 1943, 1940),
                 instrument = c("guitar", "bass", "guiter", "drumns"))
# p41
vec[2]
vec[c(5, 6)]
vec[-c(5,6)]
vec[vec>5]

df
df[c(2, 4), 3]
df[ , 1]
df[ , "instrument"]
df$instrument

# p47
vec
#vec(1:4)
vec[1:4]
#vec[-1:4]
vec[-(1:4)]
#vec[3,4,5]
vec[c(3, 4, 5)]

# p55
df
# "john" ?ùÑ ÏµúÎ?Ä?ïú ÎßéÏù¥ Ï∂îÏ∂ú?ïòÍ∏?
df[1, 1]
df[c(1), "name"]
df[1, "name"]
df[c(1), c("name")]
df[-c(2:4), c("name")]
df[-c(2, 3, 4), c("name")]
df[-c(2, 3, 4), c(1)]
df[-c(2, 3, 4), -c(2, 3)]
df[-c(2, 3, 4), -c(2:3)]
df[c(1), -c(2, 3)]
df[c(TRUE, FALSE, FALSE, FALSE), c(TRUE, FALSE, FALSE)]
df[c(TRUE, FALSE, FALSE, FALSE), c("name")]
df[c(TRUE, FALSE, FALSE, FALSE), c(1)]
df[c(TRUE, FALSE, FALSE, FALSE), -c(2, 3)]
df[c(1), c(TRUE, FALSE, FALSE)]
