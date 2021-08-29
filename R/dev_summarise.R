#wr.data.table::mkdt()
#
# dt1 <- dt |> wr_summarise(mumpg=mean(mpg))
# dt1
#
# dt2 <- dt |> wr.data.table::wr_groupby(gear, am) |> wr_summarise(mumpg=mean(mpg),sdmpg=sd(mpg), muhp=mean(hp), sdhp=sd(hp))
# dt2
#
# dt[,.N,by=.(gear,am)]
#
# dt[,.(mumpg=mean(mpg),sdmpg=sd(mpg), muhp=mean(hp), sdhp=sd(hp)), by=list(gear,am)]
