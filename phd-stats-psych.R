data<-read.csv("R_Merged_All_Grads_051215.csv", header=T)

names(data)

# Get only the Doctor of Philosophy cases
PhD<-subset(data, Degree.Level=="Doctor of Philosophy")

attach(PhD)

# Quick histogram and summary stats of overall annual income distribution across departments
hist(Yearly.Rate)
summary(Yearly.Rate)

detach(PhD)

# Identify departments (Descr) with 4 or more appointments, keep those cases. Otherwise, drop them (3 or fewer appointments).

require(plyr)
PhD.clean<-ddply(PhD, "Descr", function(d) {if(nrow(d)>3) d else NULL}) # Now we have a cleaned dataset

# Do stuff by department with the cleaned dataset
attach(PhD.clean)

length(unique(Descr)) # How many departments are represented in the cleaned dataset?

# Summary statistics by Department (Descr)
dept.stats <- ddply(PhD.clean, .(Descr), 
                  summarise, 
                  n=length(Yearly.Rate),
                  mean=mean(Yearly.Rate), 
                  median=median(Yearly.Rate),
                  sd=sd(Yearly.Rate),
                  max=max(Yearly.Rate),
                  min=min(Yearly.Rate)
)

deptxFTE.stats <- ddply(PhD.clean, .(Descr, FTE), 
                    summarise, 
                    n=length(Yearly.Rate),
                    mean=mean(Yearly.Rate), 
                    median=median(Yearly.Rate),
                    sd=sd(Yearly.Rate),
                    max=max(Yearly.Rate),
                    min=min(Yearly.Rate)
)

require(lattice)
dplot <- densityplot(~Yearly.Rate,
            main="Income frequencies over all departments at PhD level")

attach(PhD.clean)
# Histogram of incomes paneled by department
histo <- histogram(~Yearly.Rate|Descr,
          main="Income frequencies by Department at PhD level",
          xlab="Yearly income rate (9-month appointment)",
          layout=c(5,10)
          )

# Box and whisker plots paneled by department
whisk <- bwplot(~Yearly.Rate|Descr,
       main="Incomes by Department at PhD level",
       xlab="Yearly income rate (9-month appointment)",
        layout=c(5,10)
       )

# Median incomes by FTE, paneled by department
require(ggplot2)
dept.medians <- ggplot(deptxFTE.stats, aes(x=FTE, y=median)) +
  geom_bar(stat="identity", position="dodge", fill="red") +
  facet_wrap(~Descr, ncol=10, nrow=5) +
  ggtitle("Median incomes by department and FTE at PhD level") +
  ylab("Median Income")
  
# Mean incomes by FTE, paneled by department
dept.means <- ggplot(deptxFTE.stats, aes(x=FTE, y=mean)) +
  geom_bar(stat="identity", position="dodge", fill="green") +
  facet_wrap(~Descr, ncol=10, nrow=5) +
  ggtitle("Mean incomes by department and FTE at PhD level") +
  ylab("Mean Income")

# Print the graphs and save them as images
png("dplot.png", width=900, height=600); plot(dplot); dev.off()

png("histo.png", width=900, height=600); plot(histo); dev.off()

png("whisk.png", width=900, height=600); plot(whisk); dev.off()

ggsave("dept.medians.png", plot=dept.medians, width=26, height=13)
ggsave("dept.means.png", plot=dept.means, width=26, height=13)

# Export the summary stat tables
write.csv(deptxFTE.stats, file="deptxFTE-stats.csv")
