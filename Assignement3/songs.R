songs = read.csv("songs.csv")

# Part 1 Understanding the data
nrow(songs[songs$year == 2010,])
nrow(songs[songs$artistname == "Michael Jackson",])
songs[songs$artistname == "Michael Jackson" & songs$Top10 == 1,]$songtitle
unique(songs$timesignature)
table(songs$timesignature)
songs[which.max(songs$tempo),"songtitle"]

# Part 2 Building the model
songs_train = songs[songs$year < 2010,]
songs_test = songs[songs$year == 2010,]

nonvars = c("year", "songtitle", "artistname", "songID", "artistID")
songs_train = songs_train[ , !(names(songs_train) %in% nonvars) ]
songs_test = songs_test[ , !(names(songs_test) %in% nonvars) ]
SongsLog1 = glm(Top10 ~ ., data=songs_train, family=binomial)
summary(SongsLog1)
