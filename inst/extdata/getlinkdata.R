data("nuts2006")
twin <- read.csv("~/Bureau/twin.csv", stringsAsFactors=FALSE)
a <- data.frame(id = unique(twin$NUTS2_A), stringsAsFactors = FALSE)
b <- data.frame(id = a[nchar(a$id)==4,])
c <- merge(b, nuts2.df, by = "id", all.x = TRUE)
n2 <- c[!is.na(c$pop2008),"id"]
twin <- twin[twin$NUTS2_A %in% n2 &  twin$NUTS2_B %in% n2, c(1,3)]
twin$nb <- 1
twincities <- aggregate(twin[,3], by = list(twin$NUTS2_A, twin$NUTS2_B), FUN = sum)
names(twincities) <- c("i", "j", "fij")
twincities  <- twincities[substr(twincities$i,1,2) != substr(twincities$j,1,2),]
rm(a, b, c,  twin, n2)


save(list = ls(),file = "data/nuts2006.RData", compress = "xz") 

