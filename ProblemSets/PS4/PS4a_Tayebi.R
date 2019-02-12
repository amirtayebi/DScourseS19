system("wget -O 'http://api.fantasy.nfl.com/v1/players/stats?statType=seasonStats&season=2010&week=1&format=json' ")



library("jsonlite")


system('cat nflstats.json')


mydf <- fromJSON('nflstats.json')

class(mydf$players)


head(mydf$players)
