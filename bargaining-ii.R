data<-read.csv("R_Merged_All_Grads_102915_Compensation.csv", header=T)

total.appts = as.integer(length(data$ID))
require(plyr)
# How many GAs in each Department
Dept.raw <- ddply(data, .(Descr),
                  summarise,
                  n=length(ID),
                  Proportion=(length(ID)/total.appts)
)

# How many appointments are at each FTE
## Raw
FTE.raw <- ddply(data, .(FTE), 
                    summarise, 
                    n=length(ID),
                    Proportion=(length(ID)/total.appts)
)

## By Degree Level (PhD or Masters)
FTExDeg <- ddply(data, .(FTE, Degree.Level.Coded),
                 summarise,
                 n=length(ID),
                 Proportion=(length(ID)/total.appts)
)

## By Department
FTExDept <- ddply(data, .(FTE, Descr),
                  summarise,
                  n=length(ID),
                  Proportion=(length(ID)/total.appts)
)

## By Degree Level x Department
FTExDegxDescr <- ddply(data, .(FTE, Degree.Level.Coded, Descr),
                       summarise,
                       n=length(ID),
                       Proportion=(length(ID)/total.appts)
)

# Stipends
## How many appointments at minimum yearly stipend?
total.appts.min.stipend = length(subset(data, Zero.If.LE.1.Percent.Prop=="0")$Zero.If.LE.1.Percent.Prop)

Min.stipend <- ddply(data, .(Zero.If.LE.1.Percent.Prop),
                     summarise,
                     n=length(ID),
                     Proportion=length(ID/total.appts)
)

## Median stipends across Degree Level
Mdn.stipend <- ddply(data, .(Degree.Level.Coded),
                        summarise,
                        median=median(Yearly.Rate)
)


# require(lattice)
# dplot <- densityplot(~Yearly.Rate,
#                      main="Income frequencies over all departments at PhD level")
# 
# attach(PhD.clean)
# # Histogram of incomes paneled by department
# histo <- histogram(~Yearly.Rate|Descr,
#                    main="Income frequencies by Department at PhD level",
#                    xlab="Yearly income rate (9-month appointment)",
#                    layout=c(5,10)
# )
# 
# # Box and whisker plots paneled by department
# whisk <- bwplot(~Yearly.Rate|Descr,
#                 main="Incomes by Department at PhD level",
#                 xlab="Yearly income rate (9-month appointment)",
#                 layout=c(5,10)
# )
# 
# # Median incomes by FTE, paneled by department
# require(ggplot2)
# dept.medians <- ggplot(deptxFTE.stats, aes(x=FTE, y=median)) +
#   geom_bar(stat="identity", position="dodge", fill="red") +
#   facet_wrap(~Descr, ncol=10, nrow=5) +
#   ggtitle("Median incomes by department and FTE at PhD level") +
#   ylab("Median Income")
# 
# # Mean incomes by FTE, paneled by department
# dept.means <- ggplot(deptxFTE.stats, aes(x=FTE, y=mean)) +
#   geom_bar(stat="identity", position="dodge", fill="green") +
#   facet_wrap(~Descr, ncol=10, nrow=5) +
#   ggtitle("Mean incomes by department and FTE at PhD level") +
#   ylab("Mean Income")

# Print the graphs and save them as images
png("dplot.png", width=900, height=600); plot(dplot); dev.off()

ggsave("dept.medians.png", plot=dept.medians, width=26, height=13)

# Export the summary stat tables
write.csv(FTE.raw, file="FTE-raw.csv")