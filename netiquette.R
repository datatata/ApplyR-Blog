count.google.hits <- function(query) {
# call Google Search and extract the number of hits

 url <- paste('http://www.google.com/search?q=', query, sep="")
 tmp <- readLines(url, warn=FALSE)
 writeLines(tmp, "c:\\temp\\pokus.html")

 pattern <- ".*<div id=resultStats>[A-Za-z ]*([0-9 ,]*) results<nobr>  \\([0-9.]* seconds\\).*"
 count.line <- grep(pattern,tmp)[1]
 hits <- sub(pattern, "\\1", tmp[[count.line]])

 hits.as.number <- as.numeric(gsub(",","",hits))
 return(hits.as.number)
}

hours <- seq(from=24, to=6*24, by=24)
# hours to be googled
results <- rep(0, length(hours))
names(results) <- as.character(hours)

for (X in hours) {
 query <- paste('netiquette', 'recommends', 'to', 'respond', 'to', 'email', 
                'within', paste('"',X,'+hours"', sep=""),sep="+")
 results[X/24] <- count.google.hits(query)
}

barplot(results, 
        main="Netiquette recommends to respond to email within X hours",
        ylab="Number of hits in Google Search", 
        xlab="X", 
        col=rainbow(6)
       )

