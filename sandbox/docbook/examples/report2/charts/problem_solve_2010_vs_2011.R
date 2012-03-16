rates <- read.csv("../tables/all.csv")
title = "Problem Solving,  2010, 2011"
y1 <- rates$hours.problem.solve.2010 
y2 <- rates$hours.problem.solve.2011
total.hours.2010 <- rates$hours.outbound.2010 + rates$hours.inbound.2010
total.hours.2011 <- rates$hours.outbound.2010 + rates$hours.inbound.2011

y1 <- y1/1000
y2 <- y2/1000
x <- rates$week
png(file="problem_solve_2010_2011.png",
width=5, height=3.5, units="in", res=res)
xlab.text="Week in Year"
xlab = xlab.text
ylab = "hours in thousands"
if (make.title == T ) top.margin = 3
if (make.title != T ) title="" 
if (months.grid == T) bottom.margin = 6
if (months.grid == T) xlab = ''
margins=c(bottom.margin, 4, top.margin, 0.5) 
par(mar=margins) 
par(mar=margins) 
par('family'='serif')
y_range = range(y1,y2, na.rm=T)
plot(x, type='n', xlab=xlab, ylab=ylab, las=1, main=title, 
        ylim=c(y_range), axes=F, xlim=c(range(x)))
axis(1, col=ax_col)
axis(2, col=ax_col, las=1, at=axTicks(2))
box(col=box_col)
rect(38, -10, 39, 300, col=rec_col, border=NA, xpd = F)
abline( h=axTicks(2), lty=1, lwd=lwd_grid, col=ax_col)

lines(x,y1,  col=line_colors[1], type="l", lwd=lwd, lty=line_types[1])
lines(x,y2,  col=line_colors[2], type="l", lwd=lwd, lty=line_types[2])
if(months.grid == T) mtext(side = 1, xlab.text, line=2)
source("make_x_axis.R")
legend("topleft", c("2010","2011"), cex=legend_font_size, 
 col=line_colors,  lty=line_types,  bty='y', bg="white", box.lwd=box.lwd)
dev.off()

title = "Problem Solving,  2010, 2011"
y1 <- rates$hours.problem.solve.2010 
y2 <- rates$hours.problem.solve.2011
total.hours.2010 <- rates$hours.outbound.2010 + rates$hours.inbound.2010
total.hours.2011 <- rates$hours.outbound.2011 + rates$hours.inbound.2011
y1 <- y1/total.hours.2010
y2 <- y2/total.hours.2011
y1 <- y1 * 100
y2 <- y2 * 100
#png(file="charts/problem_solve2_2010_2011.png",
#width=5, height=3.5, units="in", res=res)
#xlab.text="Week in Year"
#xlab = xlab.text
#ylab = "percentage to total"
#if (make.title == T ) top.margin = 3
#if (make.title != T ) title="" 
#if (months.grid == T) bottom.margin = 6
#if (months.grid == T) xlab = ''
#margins=c(bottom.margin, 4, top.margin, 0.5) 
#par(mar=margins) 
#par(mar=margins) 
#par('family'='serif')
#y_range = range(y1,y2, na.rm=T)
#plot(x, type='n', xlab=xlab, ylab=ylab, las=1, main=title, 
#        ylim=c(y_range), axes=F, xlim=c(range(x)))
#axis(1, col=ax_col)
#axis(2, col=ax_col, las=1, at=axTicks(2))
#box(col=box_col)
#rect(38, -10, 39, 300, col=rec_col, border=NA, xpd = F)
#abline( h=axTicks(2), lty=1, lwd=lwd_grid, col=ax_col)
#
#lines(x,y1,  col=line_colors[1], type="l", lwd=lwd, lty=line_types[1])
#lines(x,y2,  col=line_colors[2], type="l", lwd=lwd, lty=line_types[2])
#if(months.grid == T) mtext(side = 1, xlab.text, line=2)
#if(months.grid == T) axis(1, at=at, labels=months, line=4,
#cex.axis=months.font, lab=NULL)  
#legend("topleft", c("2010","2011"), cex=legend_font_size, 
# col=line_colors,  lty=line_types,  bty='y', bg="white", box.lwd=box.lwd)
#dev.off()
#t1 <- y1[38:52]
#t2 <- y2[38:52]
#mean.2010 <- mean(y1)
#cat("mean for 2010 is", mean.2010, "\n")
#cat("mean for pre cutover 2011 is", mean(y2[1:37]), "\n")
#cat("mean for post cutover 2011 is", mean(y2[38:52]), "\n")
#mean(t2) - mean(t1)
#t.test(t2,t1)
#t1 <- y2[1:37]
#t2 <- y2[38:52]
## t.test(t2,t1)
#s = sum(total.hours.2011)
#h = .015 * sum(total.hours.2011)
#t1 <- y1[1:38]
#t2 <- y2[1:38]
## t.test(t1,t2)
#t1 <- y1[42:52]
#t2 <- y2[42:52]
#cat("difference of means between weeks 42-52, 2010, 2011", mean(t2) - mean(t1), "(t test below)\n")
#t.test(t2,t1)
#h = .0096 * s
#cat("The hours extra are", .015 * s, "\n")
#c = 16.10 * h
#s * .015 * 16.1
