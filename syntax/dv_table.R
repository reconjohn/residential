fhist <- regrs[, -c(1, 3, 7:10, 13:14, 16,17)]
par(mfrow=c(3,3))
for(i in 1:length(fhist)){
  hist(fhist[[i]], main= paste("Histogram of\n", names(fhist)[i]),
       xlab= names(fhist)[i], col="gold")
  abline(v = median(fhist[[i]]), col="red", lwd=4)
  text(median(fhist[[i]]), 7, round(median(fhist[[i]]),2), col = "blue")
}
