editsgisKML <- function(xx){
	ind <- which(a == "<h1>통계정보</h1>")
	sind <- which(a == "<Placemark>")
	eind <- which(a == "</Placemark>")

	seind <- sapply(ind, function(x) which(sind < x & eind > x))
	sind <- sind[seind]
	eind <- eind[seind]
	rmind <- c(sapply(1:length(sind), function(x) sind[x]:eind[x]))

	popind <- which(substring(a, 1, 6) == "총인구(명)")
	popval <- as.numeric(sapply(a[popind], function(xx) substring(xx, gregexpr(":", xx)[[1]]+2, gregexpr("<br>", xx)[[1]]-1)))

	a2 <- a[-rmind]
	sind <- min(which(a2 == "<Placemark>"))
	eind <- max(which(a2 == "</Placemark>"))
	a2 <- a2[sind:eind]
	return(list(pop=popval, kml=a2))
}


#suppressWarnings(a <- readLines("~/Dropbox/CensusSHP/1123067.kml"))
#editsgisKML(a)
load("censuscode.RData")
tmpcode <- subset(censuscode, substring(censuscode[,5], 1, 4) == "1123")

dlist <- dir("CensusSHP", pattern="^1123[0-9]*.kml$", full.names=TRUE)
pops <- c()
library(fields)
cols <- rev(tim.colors(10))
rgb2bgrK <- function(x){
	b <- substring(x, 6, 7)
	g <- substring(x, 4, 5)
	r <- substring(x, 2, 3)
	paste("9F", b, g, r, sep="")
}
#cols <- rep(rgb(0,1,1), 10)
cols <- rgb2bgrK(cols)
suppressWarnings(a <- readLines(dlist[1]))
headval <- a[1:(which(a == "<Placemark>")[1]-1)]
ind <- which(substring(headval, 1, 17) == "<Style id=\"level_")
headval[ind] <- paste("<Style id=\"level_", 1:10, "\"><LineStyle><color>ffffffff</color><width>3</width></LineStyle><PolyStyle><color>",
					  cols, "</color></PolyStyle></Style>", sep="")
		
write(paste(headval, collapse="\n"), file="test.kml")

for(i in 1:length(dlist)){
	suppressWarnings(a <- readLines(dlist[i]))
	bb <- editsgisKML(a)
	pops <- c(pops, bb$pop)
	kmls <- paste(bb$kml, collapse="\n")
	write(kmls, file="test.kml", append = TRUE)
}
write("</Document>\n</kml>", file="test.kml", append = TRUE)

#bks <- as.numeric(quantile(pops, probs = seq(0, 1, 0.1)))
#bks[1] <- bks[1] - 1
#gpval <- as.numeric(cut(pops, bks))

bks <- as.numeric(quantile(pops, probs = c(.9, 1)))
bks <- c(seq(min(pops)-1, bks[1],, 10), bks[-1])
gpval <- as.numeric(cut(pops, bks))
#gpval <- as.numeric(cut(pops, 10))

suppressWarnings(a <- readLines("test.kml"))
ind <- which(substring(a, 1, nchar("<styleUrl>#level_")) == "<styleUrl>#level_")
a[ind] <- paste("<styleUrl>#level_", gpval, "</styleUrl>", sep="")

ind2 <- which(substring(a, 1, 6) == "<name>")
tmpa <- a[ind2]
tmpa <- gsub("<name>", "", tmpa)
tmpa <- gsub("</name>", "", tmpa)
locname <- tmpcode[match(tmpa, tmpcode[,5]), 4]
locname1 <- paste("<name>", paste(locname, " (", pops, "명)", sep=""), "</name>", sep="")
a[ind2] <- locname1 
write(paste(a, collapse="\n"), file="test.kml")
