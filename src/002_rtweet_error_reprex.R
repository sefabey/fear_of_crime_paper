rtweet::read_twitter_csv("data/slang_tweets.csv")


library(rtweet)

rtweet::lookup_coords("UK")
rtweet::lookup_coords("London")
rtweet::lookup_coords("michigan")
rtweet::lookup_coords("California")

lookup_coords("England, UK")

search_tweets("london", n=50, geocode = lookup_coords("London"))
search_tweets("london", n=50, geocode = lookup_coords("London, UK"))


ggmap::geocode("houston texas")
ggmap::geocode("London")


reprex::reprex(si=T)


identical(Sys.getenv("GOOGLE_MAPS_KEY"),rtweet:::find_google_geocode_key())
identical("AIzaSyCap9hNxpT3mZBvzWXWobleOpoEjpUx8Tw",rtweet:::find_google_geocode_key())

reprex::reprex(si=T)
