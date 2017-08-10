library(tidyverse)

set.seed(42)  #Life, the universe, and everything

distribution0 <- function(scores) {
	mean(as.numeric(sample(x = c(0, 1, 2, 3, 4),
			       size=scores,
			       replace=TRUE,
			       prob=c(0.50, 0.40, 0.09, 0.01, 0.00))))  #Beginning
}
distribution1 <- function(scores) {
	mean(as.numeric(sample(x = c(0, 1, 2, 3, 4),
			       size=scores,
			       replace=TRUE,
			       prob=c(0.10, 0.55, 0.30, 0.05, 0.00))))  #Beginning
}

distribution2 <- function(scores) {
	mean(as.numeric(sample(x = c(0, 1, 2, 3, 4),
			       size=scores,
			       replace=TRUE,
			       prob=c(0.00, 0.26, 0.49, 0.24, 0.01))))  #Progressing
}

distribution3 <- function(scores) {
	mean(as.numeric(sample(x = c(0, 1, 2, 3, 4),
			       size=scores,
			       replace=TRUE,
			       prob=c(0.00, 0.01, 0.39, 0.46, 0.14))))  #Proficient
}

distribution4 <- function(scores) {
	mean(as.numeric(sample(x = c(0, 1, 2, 3, 4),
			       size=scores,
			       replace=TRUE,
			       prob=c(0.00, 0.00, 0.10, 0.27, 0.63))))  #Advanced
}

myScores0 <- ""
myScores1 <- ""
myScores2 <- ""
myScores3 <- ""
myScores4 <- ""

# Create 1000 classes of 16 students each from each distribution
for (i in 1:1000) {
	myScores0[i] <- distribution0(16) #faculty-student ratio is 16:1 at FSU
	myScores1[i] <- distribution1(16)
	myScores2[i] <- distribution2(16)
	myScores3[i] <- distribution3(16)
	myScores4[i] <- distribution4(16)
}

# Create each histogram
plot0 <- hist(as.numeric(myScores0))
plot1 <- hist(as.numeric(myScores1))
plot2 <- hist(as.numeric(myScores2))
plot3 <- hist(as.numeric(myScores3))
plot4 <- hist(as.numeric(myScores4))

# Make one nice plot with all distributions
# Standard colors for the levels with 40% transparency (60% opacity) to visualize any overlap
plot(plot0, xlim = c(0,4), xlab = "Mean Class Score", ylim = c(0,300), las = 1, main = "Predicted Score Distribution", col = "#29335c99")
plot(plot1, xlim = c(0,4), ylim = c(0,300), col = "#e4572e99", add=T)
plot(plot2, xlim = c(0,4), ylim = c(0,300), col = "#f3a71299", add=T)
plot(plot3, xlim = c(0,4), ylim = c(0,300), col = "#a8c68699", add=T)
plot(plot4, xlim = c(0,4), ylim = c(0,300), col = "#669bbc99", add=T)
# Add threshold lines
abline(v=1.0, lty=2)
abline(v=1.8, lty=2)
abline(v=2.6, lty=2)
abline(v=3.4, lty=2)
# Label regions
text(0.5, 300, "Deficient\n   1%")
text(1.4, 300, "Beginning\n   5%")
text(2.2, 300, "Progressing\n 25%")
text(3.0, 300, "Proficient\n  60%")
text(3.7, 300, "Advanced\n   90%")

# Gather some summary statistics on the distributions
mySummary <- tibble("Level"=c("Deficient","Beginning","Progressing","Proficient","Advanced"),
		    "Min"=c(min(myScores0),min(myScores1),min(myScores2),min(myScores3),min(myScores4)),
		    "Max"=c(max(myScores0),max(myScores1),max(myScores2),max(myScores3),max(myScores4)),
		    "Mean"=c(mean(as.numeric(myScores0)),mean(as.numeric(myScores1)),
		    	 mean(as.numeric(myScores2)),mean(as.numeric(myScores3)),mean(as.numeric(myScores4))),
		    "Std_Dev"=c(sd(as.numeric(myScores0)),sd(as.numeric(myScores1)),sd(as.numeric(myScores2)),sd(as.numeric(myScores3)),sd(as.numeric(myScores4))),
		    "CI"=c(2.96*sd(as.numeric(myScores0))/sqrt(as.numeric(length(myScores0))),2.96*sd(as.numeric(myScores1))/sqrt(as.numeric(length(myScores1))),2.96*sd(as.numeric(myScores2))/sqrt(as.numeric(length(myScores2))),2.96*sd(as.numeric(myScores3))/sqrt(as.numeric(length(myScores3))),2.96*sd(as.numeric(myScores4))/sqrt(as.numeric(length(myScores4)))),
		    "Median"=c(median(as.numeric(myScores0)),median(as.numeric(myScores1)),
		    	   median(as.numeric(myScores2)),median(as.numeric(myScores3)),median(as.numeric(myScores4))))
mySummary
#fracDef <- sum(myScores < 1.0)/length(myScores)
#fracBeg <- sum(myScores < 1.8)/length(myScores) - fracDef
#fracPrg <- sum(myScores < 2.6)/length(myScores) - fracDef - fracBeg
#fracPrf <- sum(myScores < 3.4)/length(myScores) - fracDef - fracBeg - fracPrg
#fracAdv <- sum(myScores >=3.4)/length(myScores)

#myLevels <- c(fracDef,fracBeg,fracPrg,fracPrf,fracAdv)

#myResult <- tibble("Level"=c("Deficient","Beginning","Progressing","Proficient","Advanced"),
#		   "Fraction"=myLevels)

#myResult
#paste("Fraction Deficient: ",   fracDef)
#paste("Fraction Beginning: ",   fracBeg)
#paste("Fraction Progressing: ", fracPrg)
#paste("Fraction Proficient: ",  fracPrf)
#paste("Fraction Advanced: ",    fracAdv)

#sample1 <- tibble("Competency"=c(rep("Natural Sciences",20)),"SCI1"=c(rep(3,20)))
#sample2 <- tibble("Competency"=c(rep("Culture",20)),"CUL1"=c(rep(2,20)))
#joined <- full_join(sample1, sample2, by="Competency")
