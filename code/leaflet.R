# leaflet.R
# Michael Davidson 
# July 3, 2016

rm(list=ls())

library(data.table)
library(sp)
library(rgeos)
library(maptools)
library(rgdal)
library(geojsonio)
library(RJSONIO)
library(ggplot2)

source("~/Dropbox/Code/lguMergePrep.R")

# --------------------------- #
# --- Prep Data for Plots --- #
# --------------------------- #

# Electoral Data
pres <- readRDS("~/Dropbox/Philippine Electoral Data/General Elections/output/2016 Precinct Level/President.rds")
	
	# Clean Variable Class
	pres[,`:=`(votes=as.numeric(votes),
		ballotsCast=as.numeric(ballotsCast),
		votersRegisteredLVG=as.numeric(votersRegisteredLVG))]

	# Aggregate to Municipal Level
	temp <- pres[,.(province=province[1], 
		municipality=municipality[1], 
		mProvince=mProvince[1],
		mMunicipality=mMunicipality[1],
		provMunBrgy=provMunBrgy[1],
		votes=sum(votes, na.rm=TRUE),
		ballotsCast=sum(ballotsCast, na.rm=TRUE),
		votersRegisteredLVG=sum(votersRegisteredLVG, na.rm=TRUE)),
	by=.(munCode,name)]


	# Long to Wide
	temp <- dcast(temp, munCode + province + municipality + mProvince + mMunicipality ~ name , value.var = "votes")

	temp <- as.data.frame(temp)

	# Identify Percentages, Mun Winner
	temp[,6:11] <- temp[,6:11]/rowSums(temp[,6:11])


	colnames(temp) <- gsub(pattern="[[:punct:]]", replacement="", x=unlist(lapply(strsplit(x=colnames(temp), split="\\s"), function(x){x[1]})))

	f <- apply(temp[,6:11], 1, which.max)


		f[unlist(lapply(f, function(x){length(x)==0}))] <- NA
	temp$winner <- as.factor(colnames(temp)[6:11][unlist(f)])

# Shapefiles 

	# Read the geojson as SP object
	urlMuni <- "https://raw.githubusercontent.com/opengovt/openroads-boundaries/master/polygons/or_Municipality.json"

	muni <- geojson_read(urlMuni, method = "local", what="sp")

	# Create a DF from the SP object
	muni@data$id <- rownames(muni@data) # give it a unique id first (optional)
		muni@data$provMun <- paste0(muni@data[,"NAME"], muni@data[,"NAME_2"])
		muni@data$provMun <- lguMergePrep2(muni@data$provMun)

		munif <- fortify(muni, region="provMun")

		# For Leaflet
		muni@data <- merge(muni@data, temp, by.x="provMun", by.y="provMun", all.x=TRUE, all.y=FALSE)

	# Add the electoral data to the new DF just generated
		# prep common name
		# Electoral Data
			temp <- as.data.table(temp)
			temp[,provMun:=lguMergePrep(paste0(municipality,province))]
			
			mapData <- merge(x=munif, y=temp, by.x="id", by.y="provMun", stringsAsFactors=F) # merge in the data we care about

	# Attach the DF bearing the new data back to the SP object, creating a ggplot friendly data frame
		PRO <- merge(muni@data, mapData, by.x="provMun", by.y="id")
		muni@data <- merge(muni@data, mapData, by.x="provMun", by.y="id")

	# Sort the ggplot friendly frame by the order variable, avoiding those crazy lines
	PRO <- PRO[order(PRO$order),]
	PRO$winner <- as.factor(PRO$winner)




# ----------------------- #
# ------- ggplot -------- #
# ----------------------- #
g <- ggplot() + geom_polygon(aes(x=long, y=lat, group=group, fill=winner), data=PRO) + coord_equal() + xlab("Longitude") + ylab("latitude") + theme(legend.position = c(0.8, 0.7))
g <- g + scale_fill_brewer(type = "qual", palette = 3, direction = 1) + guides(fill=guide_legend(title=NULL)) 
ggsave(filename="~/Desktop/pres_mun.png", plot=g, width=5, height=8)













# ------------------------- #
# ------- Leaflet --------- #
# ------------------------- #
rm(list=ls())

library(data.table)
library(leaflet)
library(RColorBrewer)
library(sp)
library(rgeos)
library(maptools)
library(rgdal)
library(geojsonio)
library(RJSONIO)
library(plyr)

source("~/Dropbox/Code/lguMergePrep.R")

# --------------------------- #
# --- Prep Data for Plots --- #
# --------------------------- #

# Electoral Data
pres <- readRDS("~/Dropbox/Philippine Electoral Data/General Elections/output/2016 Precinct Level/President.rds")
	
	# Clean Variable Class
	pres[,`:=`(votes=as.numeric(votes),
		ballotsCast=as.numeric(ballotsCast),
		votersRegisteredLVG=as.numeric(votersRegisteredLVG))]

	# Aggregate to Municipal Level
	temp <- pres[,.(province=province[1], 
		municipality=municipality[1], 
		mProvince=mProvince[1],
		mMunicipality=mMunicipality[1],
		provMunBrgy=provMunBrgy[1],
		votes=sum(votes, na.rm=TRUE),
		ballotsCast=sum(ballotsCast, na.rm=TRUE),
		votersRegisteredLVG=sum(votersRegisteredLVG, na.rm=TRUE)),
	by=.(munCode,name)]


	# Long to Wide
	temp <- dcast(temp, munCode + province + municipality + mProvince + mMunicipality ~ name , value.var = "votes")

	temp <- as.data.frame(temp)

	# Identify Percentages, Mun Winner
		temp[,6:11] <- temp[,6:11]/rowSums(temp[,6:11])
		# Winner Percentage
		temp$winPer <- apply(as.data.frame(temp)[,6:11], 1, max)

	colnames(temp) <- gsub(pattern="[[:punct:]]", replacement="", x=unlist(lapply(strsplit(x=colnames(temp), split="\\s"), function(x){x[1]})))

		# Who Won
		f <- apply(temp[,6:11], 1, which.max)
			f[unlist(lapply(f, function(x){length(x)==0}))] <- NA
		temp$winner <- as.factor(colnames(temp)[6:11][unlist(f)])

	# Remove
	temp <- as.data.table(temp)
	# Fix Province for NCR
	temp[grepl(x=province, pattern="^NCR"),province:="Metropolitan Manila"]
	temp[,provMun:=lguMergePrep2(paste0(municipality,province))]
	temp <- temp[provMun!="nana"]
			
	temp <- temp[!which(provMun=="lapasleyte" & munCode!=3728)]
	temp <- temp[!which(provMun=="almeriabiliran" & munCode!=7801)]
	temp <- temp[!which(provMun=="bocauebulacan" & munCode!=1404)]
	temp <- temp[!which(provMun=="polilloqueson" & munCode!=5636)]
	# islandgardensamaldavaodelnorte
	temp[provMun=="islandgardensamaldavaodelnorte", provMun:="samaldavaodelnorte"]
	
	# Fix Encoding
	temp[,Encoding(municipality) <- "UTF-8"]




# Shapefiles 

	# Read the geojson as SP object
	urlMuni <- "https://raw.githubusercontent.com/opengovt/openroads-boundaries/master/polygons/or_Municipality.json"

	muni <- geojson_read(urlMuni, method = "local", what="sp")

	# Fix Encoding
	muni@data$NAME <- as.character(muni@data$NAME)
	Encoding(muni@data$NAME) <- "UTF-8"
	muni@data$NAME_2 <- as.character(muni@data$NAME_2)
	Encoding(muni@data$NAME_2) <- "UTF-8"

	# Fix North Cotabato to Cotabato
	muni@data$NAME_2[muni@data$NAME_2=="North Cotabato"] <- "Cotabato"
	# Kaloocan to Caloocan
	muni@data$NAME[muni@data$NAME=="Kalookan City"] <- "Caloocan City"
	# Davao Occidental
	muni@data[muni@data$NAME_2=="Davao del Sur" & muni@data$NAME %in% c("Don Marcelino","Malita","Sarangani","Jose Abad Santos","Santa Maria"),]$NAME_2 <- "Davao Occidental"

	# Create Common Merge Var
	muni@data$provMun <- paste0(muni@data[,"NAME"], muni@data[,"NAME_2"])
	muni@data$provMun <- lguMergePrep2(muni@data$provMun)

	# Join sp object with electoral data table
	muni@data <- join(x=muni@data, y=temp, by="provMun", type="left", match="first")

# Inspect Merge Failures
	muni@data[is.na(muni@data$winner) & muni@data$ENGTYPE_2 %in% c("Municipality","City"),]

	# Remove non-political features (e.g. lakes)
	muni <- muni[muni$ENGTYPE_2 %in% c("Municipality","City"),]
	
	# Format for presentation
	simpleCap <- function(x) {
	  s <- strsplit(x, " ")[[1]]
	  paste(toupper(substring(s, 1,1)), substring(s, 2),
	        sep="", collapse=" ")
	}

	muni$province <- simpleCap(tolower(muni$province))
	# muni$municipality <- tolower(muni$municipality)
	

# Prep Bar Plot
	add.alpha <- function(col, alpha=1){
	  if(missing(col))
	    stop("Please provide a vector of colours.")
	  apply(sapply(col, col2rgb)/255, 2, 
	        function(x) 
	          rgb(x[1], x[2], x[3], alpha=alpha))  
	}
	
	
	
	tot <- pres[ , sum(votes, na.rm=TRUE), by=name]
	  tot[,per:=round(V1/sum(V1)*100, 2)]
	  tot[,name:=unlist(lapply(str_split(string=tot$name, pattern="\\s"), function(x){x[1]}))]
	  tot[,name:=gsub("\\,", x=name, replacement="")]
	  tot <- tot[ 1:5,]
	  tot[,namePer:=paste0(name, " ", per,"%")]
	
	  png(filename="~/Dropbox/2016PhilippinesMaps/temp/barplot.png", height=550, width=500)
	  bp <- barplot(height=rev(tot[,per]), names.arg=rev(tot[,name]), col=rev(add.alpha(c("orange","red","dodgerblue4","cyan","yellow"), alpha=.75)), xaxt='n', horiz=TRUE, xlim=c(0,45))
	  text(y=5+.5, x=tot[1,per]+2.5, paste0(tot[1,per],"%"), col="black")
	  text(y=4+.3, x=tot[2,per]+2.5, paste0(tot[2,per],"%"), col="black")
	  text(y=3.1, x=tot[3,per]+2.5, paste0(tot[3,per],"%"), col="black")
	  text(y=1.9, x=tot[4,per]+2.5, paste0(tot[4,per],"%"), col="black")
	  text(y=.7, x=tot[5,per]+2.5, paste0(tot[5,per],"%"), col="black")
	  dev.off()
	
	
	
	bp <- ggplot(data=tot, aes(x=factor(name), y=V1, fill=c("orange","red","dodgerblue4","cyan","yellow"))) + 
	  geom_bar(stat="identity") +
	  theme(axis.text.y=element_blank(), axis.title.y=element_blank(), axis.title.x=element_blank(), legend.position="none") 
	
	+ scale_fill_manual(values=c("orange","red","dodgerblue4","cyan","yellow","white"))
	
	

# -------------------- #
# --- Make the Map --- #
# -------------------- #
# Polygon Aesthetics
p <- c("orange","red","dodgerblue4","cyan","yellow")
d <- levels(muni$winner)
factpal2 <- colorFactor(palette=p, domain=d)

# Plot it
m <- leaflet(data=muni, width=800, height=600) %>% addTiles %>%
	  addPolygons(stroke=FALSE, fillOpacity = muni$winPer, smoothFactor= 0.5, color = ~factpal2(winner), popup = paste0(as.character(muni$NAME), ", ", as.character(muni$NAME_2), "<br>", muni$winner, ": ", round(muni$winPer*100,2), "%")) %>%
    addLegend("topright", colors=c("orange","red","blue","cyan","yellow"), labels=tot[,namePer]) %>%
    setView(lng=122.5, lat=13.0, zoom=6, options = list()) 
m

library(htmlwidgets)
saveWidget(widget=m, file="~/Dropbox/2016PhilippinesMaps/output/Phils_Pres.html", selfcontained=TRUE)







