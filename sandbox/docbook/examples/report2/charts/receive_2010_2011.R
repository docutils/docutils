rates <- read.csv("../tables/all.csv")
x = rates$week
title=''
y1 = rates$rate.receive.2010
y2 = rates$rate.receive.2011
png(file="receive_rate_2010_2011.png", width=width, height=3.5, units="in", res=res)
xlab.text="week in year"
xlab = xlab.text
ylab = "units/man hour"
if (make.title == T ) top.margin = 3
if (make.title != T ) title="" 
if (months.grid == T) bottom.margin = 6
if (months.grid == T) xlab = ''
margins=c(bottom.margin, 4, top.margin, 0.5) 
par(mar=margins) 
par('family'='serif')
plot(x, type='n', xlab=xlab, ylab=ylab, las=1, main=title, ylim=c(range(y1,y2,na.rm=T)), axes=F)
axis(1, col=ax_col)
axis(2, col=ax_col, las=1, at=axTicks(2))
box(col=box_col)
abline( h=axTicks(2), lty=1, lwd=lwd_grid, col=ax_col)
lines(x,y1,  col=line_colors[1], type="l", lwd=lwd,lty=line_types[1])
lines(x,y2, col=line_colors[2],  lwd=lwd, lty=line_types[2])
legend("bottomleft", c("2010","2011"), cex=legend_font_size, 
 col=line_colors,  lty=line_types, bty='y', box.lwd=box.lwd)
source("make_x_axis.R")
dev.off()
t.results <- t.test(y1,y2)
m.diff <- mean(y1) - mean(y2)
conf.int <- abs(m.diff - t.results$conf.int[1])
x1 <- mean(y1)
x2 <- mean(y2)

percent.diff <- abs((x1 - x2)/((x1 + x2)/2) * 100)
# cat("diff in mean is", m.diff, "with +-", conf.int, "and percent change is", percent.diff, "\n")

percent.change <- (x2 - x1)/x1 * 100
percent.change.low <- ((x2 + conf.int) - x1)/x1 * 100
percent.change.high <- ((x2 - conf.int) - x1)/x1 * 100

percent.conf.int <- abs((percent.change.high - percent.change.low)/2)

#cat("percent change is", percent.change, "+-", percent.conf.int,  "\n")

y1 <- rates$rate.receive.2011[1:36]
y2 <- rates$rate.receive.2011[41:52]
t.results <- t.test(y1,y2)
m.diff <- mean(y1) - mean(y2)
conf.int <- abs(m.diff - t.results$conf.int[1])
x1 <- mean(y1)
x2 <- mean(y2)

percent.diff <- abs((x1 - x2)/((x1 + x2)/2) * 100)
# cat("diff in mean is", m.diff, "with +-", conf.int, "and percent change is", percent.diff, "\n")

percent.change <- (x2 - x1)/x1 * 100
percent.change.low <- ((x2 + conf.int) - x1)/x1 * 100
percent.change.high <- ((x2 - conf.int) - x1)/x1 * 100

percent.conf.int <- abs((percent.change.high - percent.change.low)/2)

#cat("percent change is", percent.change, "+-", percent.conf.int,  "\n")

y1 <- rates$indirect.hours.receive.2011
y2 <- rates$direct.hours.receive.2011
t1 <- rates$indirect.hours.receive.2011[1:38]/rates$hours.receive.2011[1:38]
t2 <- rates$indirect.hours.receive.2011[42:52]/rates$hours.receive.2011[42:52]


t1 <- t1 * 100
t2 <- t2 * 100


# mean(t2) - mean(t1)
t1 <- rates$indirect.hours.receive.2011[1:38]/rates$hours.receive.2011[1:38]
t2 <- rates$indirect.hours.receive.2010[1:38]/rates$hours.receive.2010[1:38]
# cat("Normal rate for early 2011 is", mean(t1), "Normal rate for early 2010
# is", mean(t2), "\n")
t1 <- rates$indirect.hours.receive.2010[42:52]/rates$hours.receive.2010[42:52]
t2 <- rates$indirect.hours.receive.2011[42:52]/rates$hours.receive.2011[42:52]
# cat("rate for post peak 2010:", mean(t1), "rate for post peak 2011",
# mean(t2), "\n")
s1 <- sum(rates$direct.hours.receive.2011[42:52])
s2 <- sum(rates$indirect.hours.receive.2011[42:52])
#cat("total hours direct post peak 2011", s1, "\n")
#cat("total hours indirect post peak 2011", s2, "\n")

#y1 <- rates$units.receive.2010/rates$direct.hours.receive.2010
#y2 <- rates$units.receive.2011/rates$direct.hours.receive.2011
#png(file="charts/receive_rate_direct_2010_2011.png", width=width, height=3.5, units="in", res=res)
#xlab.text="week in year"
#xlab = xlab.text
#ylab = "units/man hour"
#if (make.title == T ) top.margin = 3
#if (make.title != T ) title="" 
#if (months.grid == T) bottom.margin = 6
#if (months.grid == T) xlab = ''
#margins=c(bottom.margin, 4, top.margin, 0.5) 
#par(mar=margins) 
#par('family'='serif')
#plot(x, type='n', xlab=xlab, ylab=ylab, las=1, main=title, ylim=c(range(y1,y2,na.rm=T)), axes=F)
#axis(1, col=ax_col)
#axis(2, col=ax_col, las=1, at=axTicks(2))
#box(col=box_col)
#abline( h=axTicks(2), lty=1, lwd=lwd_grid, col=ax_col)
#lines(x,y1,  col=line_colors[1], type="l", lwd=lwd,lty=line_types[1])
#lines(x,y2, col=line_colors[2],  lwd=lwd, lty=line_types[2])
#legend("bottomleft", c("2010","2011"), cex=legend_font_size, 
# col=line_colors,  lty=line_types, bty='y', box.lwd=box.lwd)
#source("make_x_axis.R")
#dev.off()

title = "Perentage of indirct labor to total, 2010, 2011"
y1 <- rates$indirect.hours.receive.2010/rates$hours.receive.2010
y2 <- rates$indirect.hours.receive.2011/rates$hours.receive.2011
y1 <- y1 * 100
y2 <- y2 * 100
png(file="receive_rate_indirect_total_2010_2011.png", width=width, height=3.5, units="in", res=res)
xlab.text="week in year"
xlab = xlab.text
ylab = "percentage to total"
if (make.title == T ) top.margin = 3
if (make.title != T ) title="" 
if (months.grid == T) bottom.margin = 6
if (months.grid == T) xlab = ''
margins=c(bottom.margin, 4, top.margin, 0.5) 
par(mar=margins) 
par('family'='serif')
plot(x, type='n', xlab=xlab, ylab=ylab, las=1, main=title, ylim=c(range(y1,y2,na.rm=T)), axes=F)
axis(1, col=ax_col)
axis(2, col=ax_col, las=1, at=axTicks(2))
box(col=box_col)
abline( h=axTicks(2), lty=1, lwd=lwd_grid, col=ax_col)
lines(x,y1,  col=line_colors[1], type="l", lwd=lwd,lty=line_types[1])
lines(x,y2, col=line_colors[2],  lwd=lwd, lty=line_types[2])
error_bars = predict(lm(y2[1:36]~x[1:36]), interval='confidence')
#segments(1,error_bars[1], 36, error_bars[36], col=line_colors[2])
#rect(38, -10, 39, 300, col="lightgray", border=NA, xpd = F)
legend("topleft", c("2010","2011"), cex=legend_font_size, 
 col=line_colors,  lty=line_types, bty='y', box.lwd=box.lwd)
source("make_x_axis.R")
dev.off()



adj.start <- 39
adj.ratio <- .297
#adj.ratio <- .32 # with a confidence interval
adj.ratio <- .235 # last year's post cutover percent
y1 <- rates$rate.receive.2011
hours.direct.2011 <- rates$direct.hours.receive.2011
hours.indirect.2011 <- rates$indirect.hours.receive.2011
hours.direct.late <- hours.direct.2011[adj.start:52]
adj.interval <- adj.ratio * hours.direct.late
hours.indirect.adj.2011 <- c(hours.indirect.2011[1:(adj.start - 1)], adj.interval)
rate.adjusted <- rates$units.receive.2011/(hours.direct.2011 + hours.indirect.adj.2011)
y2 <- rate.adjusted
y3 <- rates$rate.receive.2010
diff.hours <- hours.indirect.2011 - hours.indirect.adj.2011
total.diff.hours <- sum(diff.hours)
#cat("total extra indirect hours are", round(total.diff.hours), "\n")

#png(file="receive_rate_2011_adjusted.png", width=width, height=3.5, units="in", res=res)
#y_range = range(y1,y2,y3)
#xlab.text="week in year"
#xlab = xlab.text
#ylab = "units/man hour"
#if (make.title == T ) top.margin = 3
#if (make.title != T ) title="" 
#if (months.grid == T) bottom.margin = 6
#if (months.grid == T) xlab = ''
#margins=c(bottom.margin, 4, top.margin, 0.5) 
#par(mar=margins) 
#par('family'='serif')
#plot(x, type='n', xlab=xlab, ylab=ylab, las=1, main=title,
#ylim=y_range, axes=F)
#axis(1, col=ax_col)
#axis(2, col=ax_col, las=1, at=axTicks(2))
#box(col=box_col)
#abline( h=axTicks(2), lty=1, lwd=lwd_grid, col=ax_col)
#lines(x,y2, col=line_colors[1],  lwd=lwd, lty=line_types[1])
#lines(x,y1,  col=line_colors[2], type="l", lwd=lwd,lty=line_types[2])
#x.poly = c(x, rev(x))
#y.poly = c(y1, rev(y2))
#polygon(x.poly, y.poly, col="lightgray", border=NA)
##lines(x,y3, col=line_colors[3],  lwd=lwd, lty=line_types[3])
#legend("bottomleft", c("real","adjusted"), cex=legend_font_size, 
# col=line_colors,  lty=line_types, bty='y', box.lwd=box.lwd)
#source("make_x_axis.R")
#dev.off()

title='Indirect hours Receive'
y1 = rates$indirect.hours.receive.2010
y2 = rates$indirect.hours.receive.2011
y3 = rates$direct.hours.receive.2011
#png(file="receive_indirect_hours_2010_2011.png",
#width=width, height=7, units="in", res=res)
#y_range <- range(y1,y2)
#xlab.text="week in year"
#xlab = xlab.text
#ylab = "hours"
#if (make.title == T ) top.margin = 3
#if (make.title != T ) title="" 
#if (months.grid == T) bottom.margin = 6
#if (months.grid == T) xlab = ''
#margins=c(bottom.margin, 4, top.margin, 0.5) 
#par(mar=margins) 
#par('family'='serif')
#plot(x, type='n', xlab=xlab, ylab=ylab, las=1, main=title,
#ylim=y_range, axes=F)
#axis(1, col=ax_col)
#axis(2, col=ax_col, las=1, at=axTicks(2))
#box(col=box_col)
#abline( h=axTicks(2), lty=1, lwd=lwd_grid, col=ax_col)
#lines(x,y1,  col=line_colors[1], type="l", lwd=lwd,lty=line_types[1])
#lines(x,y2, col=line_colors[2],  lwd=lwd, lty=line_types[2])
##lines(x,y3, col=line_colors[3],  lwd=lwd, lty=line_types[3])
#legend("topleft", c("2010","2011"), cex=legend_font_size, 
# col=line_colors,  lty=line_types, bty='y', box.lwd=box.lwd)
#source("make_x_axis.R")
#dev.off()

title='Units Receive'
y1 = rates$units.receive.2010
y2 = rates$units.receive.2011
#png(file="receive_units_2010_2011.png",
#width=width, height=7, units="in", res=res)
#y_range <- range(y1,y2)
#xlab.text="week in year"
#xlab = xlab.text
#ylab = "hours"
#if (make.title == T ) top.margin = 3
#if (make.title != T ) title="" 
#if (months.grid == T) bottom.margin = 6
#if (months.grid == T) xlab = ''
#margins=c(bottom.margin, 4, top.margin, 0.5) 
#par(mar=margins) 
#par('family'='serif')
#plot(x, type='n', xlab=xlab, ylab=ylab, las=1, main=title,
#ylim=y_range, axes=F)
#axis(1, col=ax_col)
#axis(2, col=ax_col, las=1, at=axTicks(2))
#box(col=box_col)
#abline( h=axTicks(2), lty=1, lwd=lwd_grid, col=ax_col)
#lines(x,y1,  col=line_colors[1], type="l", lwd=lwd,lty=line_types[1])
#lines(x,y2, col=line_colors[2],  lwd=lwd, lty=line_types[2])
#legend("topleft", c("2010","2011"), cex=legend_font_size, 
# col=line_colors,  lty=line_types, bty='y', box.lwd=box.lwd)
#source("make_x_axis.R")
#dev.off()
