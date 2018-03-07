rd <- fromJSON(readLines(url, warn="F"))
rd.views <- rd$daily_views 
df <- as.data.frame(unlist(rd.views))

getData <- function(url){
    raw.data <- readLines(url, warn="F") 
    rd  <- fromJSON(raw.data)
    rd.views <- rd$daily_views 
    rd.views <- unlist(rd.views)
    rd <- as.data.frame(rd.views)
    rd$date <- rownames(rd)
    rownames(rd) <- NULL
    return(rd)
}


#scrapers

require(RCurl)
require(XML)

guardianScraper <- function(url){
    SOURCE <-  getURL(url,encoding="UTF-8") # Specify encoding when dealing with non-latin characters
    PARSED <- htmlParse(SOURCE)
    title <- xpathSApply(PARSED, "//h1[contains(@itemprop,'headline')]",xmlValue)
    author <- xpathSApply(PARSED, "//time[@itemprop='datePublished']",xmlValue)
    time  <- xpathSApply(PARSED, "//time[@itemprop='datePublished']/@datetime")
    tags <- unique(xpathSApply(PARSED, "//a[@rel='tag']",xmlValue))
    text <- xpathSApply(PARSED, "//div[@id='article-body-blocks']/p",xmlValue)
    return(list(title=title,
                author=author,
                time=time,
                tags=paste(tags,collapse="|")
                ,text=paste(text,collapse="|")))
}



