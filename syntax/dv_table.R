fhist <- regrs[, c(-1, -11)]
par(mfrow=c(3,4))
g_tab_ts <- for(i in 1:length(fhist)){
  hist(fhist[[i]], main= paste("Histogram of\n", names(fhist)[i]),
       xlab= names(fhist)[i], col="gold")
  abline(v = median(fhist[[i]]), col="red", lwd=4)
  text(median(fhist[[i]]), 0, round(median(fhist[[i]]),2), col = "blue")
}