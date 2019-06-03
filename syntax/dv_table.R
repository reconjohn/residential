# names(regrs)
fhist <- regrs[, c(1,2,4,5,7,11,12,13,19)]
par(mfrow=c(3,3))
for(i in 1:length(fhist)){
  hist(fhist[[i]], main= paste(names(fhist)[i]),
       xlab= names(fhist)[i], col="gold")
  abline(v = median(fhist[[i]]), col="red", lwd=4)
  text(median(fhist[[i]]), 7, round(median(fhist[[i]]),2), col = "blue")
}
