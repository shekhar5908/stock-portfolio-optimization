findminmax <- function(data, minimise = TRUE) {
  minmax <- NA
  if (minimise) minmax <- min(data[, 2])
  else minmax <- max(data[, 2])
  
  rownum <- which(data[, 2] == minmax)
  if (length(rownum) > 1) rownum <- rownum[1]
  
  if (minimise) return(minmax - data[rownum, 3])
  else return(minmax + data[rownum, 3])
}

plotbars <- function(data1, data2, data3, cap1 = "pop_size50", cap2 = "pop_size100", cap3 = "pop_size200") {
  hues <- c("black", "blue", "green")
  
  min1 <- findminmax(data1)
  min2 <- findminmax(data2)
  min3 <- findminmax(data3)
  
  max1 <- findminmax(data1, FALSE)
  max2 <- findminmax(data2, FALSE)
  max3 <- findminmax(data3, FALSE)
  
  minn <- min(min1, min2, min3)
  maxx <- max(max1, max2, max3)
  
  df1 <- data.frame(x = data1[, 1], y = data1[, 2], dy = data1[, 3])
  df2 <- data.frame(x = data2[, 1], y = data2[, 2], dy = data2[, 3])
  df3 <- data.frame(x = data3[, 1], y = data3[, 2], dy = data3[, 3])
  
  plot(df1$x, df1$y, type = "l", col = hues[1], ylim = c(minn, maxx),
       main = "Best Fitness Values Blend Crossover", xlab = "Generations", ylab = "Fitness")
  segments(df1$x, df1$y - df1$dy, df1$x, df1$y + df1$dy, col = hues[1])
  
  lines(df2$x, df2$y, col = hues[2])
  segments(df2$x, df2$y - df2$dy, df2$x, df2$y + df2$dy, col = hues[2])
  
  lines(df3$x, df3$y, col = hues[3])
  segments(df3$x, df3$y - df3$dy, df3$x, df3$y + df3$dy, col = hues[3])
  
  legend("topleft", legend = c(cap1, cap2, cap3), col = hues, lwd = 1,
         cex = 0.5)
}

plotbars(parsed_result1, parsed_result2, parsed_result3, cap1 = "pop_size50", cap2 = "pop_size100", cap3 = "pop_size200")

