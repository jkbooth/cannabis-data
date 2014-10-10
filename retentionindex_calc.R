##This script requires dplyr. Enter the retention times of your unknown, and the script will output the retention index

library(plyr)
suppressMessages(library(dplyr))


#As a numeric vector, the retention times of your alkanes
rettimes <- c(4.787, 6.269, 7.515, 8.599, 9.586, 
              10.495, 11.344, 12.151, 12.909, 13.632,
              14.318, 14.968, 15.595, 16.191, 16.769,
              17.316, 17.852, 18.358, 18.894, 19.508)

#currently set up for C9 to C28. If your data is different, enter the alkanes you have data for

alkanes <- 9:28

retention <- function(unknown) {
  retdf <- cbind.data.frame(alkanes, rettimes)
  sub1 <- retdf %>%
    filter(rettimes < unknown)
  N <- as.numeric(max(sub1$alkanes))
  sub2 <- retdf %>%
    filter(rettimes > unknown)
  N1 <- as.numeric(min(sub2$alkanes))
  
  p1 <- as.numeric(grep(N, retdf$alkanes))
  p2 <- as.numeric(grep(N1, retdf$alkanes))
  
  t1 <- as.numeric(retdf$rettimes[p1])
  t2 <- as.numeric(retdf$rettimes[p2])
  return(print(100 * (N + (N1 - N) * ((unknown - t1) / (t2 - t1)))))
}

#the function retention takes as an argument the retention time of your unknown, and returns the retention index

#enter the retention times of your unknowns, if you have multiple unknowns
times <- c(11.488, 11.561, 11.687)

lapply(X = times, FUN = retention)

alloaromadendrene <- retention(11.157)
Bcaryophyllene <- retention(10.796)
