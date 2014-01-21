# Original source code from:http://gettinggeneticsdone.blogspot.ca/2012/07/plotting-frequency-of-twitter-hashtag.html

# Use imagemagick to stitch together. Imagemagick must be installed in your path.
# montage ismb-frequency.png ismb-users.png -tile 1x -geometry -0-0 montage.png
# system("montage ismb-frequency.png ismb-users.png -tile 1x -geometry -0-0 montage.png")

#------------------------------------ Using the ROAuth package ---------------------------------------#
# ## If you can get this to work it's a bit more flexible and doesn't have the 1500/day limit as above.
# ## Using ROAuth will theoretically allow you to retrieve more than 1500 tweets with a single query.
# ## The current version on cran, 0.9.1, has known problems. Supposedly rolling back to 0.9.0 would work,
# ## and it did return TRUE after registerTwitterOAuth(cred) after the handshake, but I kept getting
# ## forbidden errors when trying to retrieve more than 1500. Also happened with version 0.9.2.
# 
# ## Install 0.9.1 from CRAN
# install.packages("ROAuth")
# install.packages("twitteR")
# install.packages("RCurl")
library(ROAuth)
library(twitteR)
library(RCurl)
# 
# ## Using ROAuth 0.9.2 from source
# download.file("http://geoffjentry.hexdump.org/ROAuth_0.9.2.tar.gz", destfile="ROAuth_0.9.2.tar.gz")
# install.packages("ROAuth_0.9.2.tar.gz", repos = NULL, type="source")
# library(ROAuth)
# 
options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
cred <- OAuthFactory$new(consumerKey="YOUR_KEY_HERE",
                         consumerSecret="YOUR_KEY_HERE",
                         requestURL="https://api.twitter.com/oauth/request_token",
                         accessURL="https://api.twitter.com/oauth/access_token",
                         authURL="https://api.twitter.com/oauth/authorize")
cred$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl") )
# Checks that you are authorised
registerTwitterOAuth(cred)
# 
# tweets1 <- searchTwitter("#bigdata", since="2014-01-15", until="2014-01-20", n=9999)
# tweets2 <-  searchTwitter("#datascience", since="2014-01-15", until="2014-01-20", n=9999)
# tweets3 <- searchTwitter("#analytics", since="2014-01-15", until="2014-01-20", n=9999)
# tweets1 <- twListToDF(tweets1)
# tweets2 <- twListToDF(tweets2)
# tweets3 <- twListToDF(tweets3)
# tweets <- rbind(tweets1,tweets2,tweets3)
# tweets
# ## Continue as above

# install.packages("ggplot2")
library(ggplot2)

tweets <- list()
dates <- paste("2014-01-",14:21,sep="") # catching tweets between the 15th to the 21st of January 2014
for (i in 2:length(dates)) {
  print(paste(dates[i-1], dates[i]))
  tweets <- c(tweets, searchTwitter("#bigdata", since=dates[i-1], until=dates[i], n=1500))
  tweets <- c(tweets, searchTwitter("#datascience", since=dates[i-1], until=dates[i], n=1500))
  tweets <- c(tweets, searchTwitter("#analytics", since=dates[i-1], until=dates[i], n=1500))
}

# Convert the list to a data frame
tweets <- twListToDF(tweets)
tweets <- unique(tweets)

# To ensure accuracy, make sure that there were no more than 1500 tweets in a single day.
# If there are 1500 on any single day, then you're truncating that day's tweets, and you'll
# need to try to get ROAuth (below) working.
tweets$date <- format(tweets$created, format="%Y-%m-%d")
table(tweets$date)

# Getting rid of spam tweets from a specific user:
# tweets <- tweets[which(tweets$screenName!="sciencestream"), ]

# Make a table of the number of tweets per user
d <- as.data.frame(table(tweets$screenName))
d <- d[order(d$Freq, decreasing=T), ]
names(d) <- c("User","Tweets")
head(d)

twitter_subset <- subset(tweets, date %in% c("2014-01-19","2014-01-20"))

# Plot the table above for the top 40
png("bigdata-users.png", w=400, h=600)
par(mar=c(5,10,2,2))
with(d[rev(1:40), ], barplot(tweets, names=User, horiz=T, las=1, main="Top 40: Tweets per User", 
                             xlab="Tweets containing #bigdata #datascience #analytics", col=1))
dev.off()

# Plot the frequency of tweets over time in two hour windows
# Modified from http://michaelbommarito.com/2011/03/12/a-quick-look-at-march11-saudi-tweets/
minutes <- 120
ggplot(data=twitter_subset, aes(x=created)) + 
  geom_bar(aes(fill=..count..), binwidth=5*minutes) + 
  scale_x_datetime("Date") + 
  scale_y_continuous("Frequency") +
  labs(title="        #bigdata #datascience #analytics 
       Tweet Frequency Jan 19-20, 2014") +
  theme(plot.title = element_text(size=20, face="bold"), axis.title = element_text(size=14, face="bold"))
ggsave(file='bigdata-frequency.png', width=7, height=7, dpi=100)
