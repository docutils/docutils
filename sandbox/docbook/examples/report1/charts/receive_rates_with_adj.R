path = 'receive_rate_adjusted_draft.png'
rates <- read.csv("../tables/all.csv")
x = rates$week
direct.hours <- rates$direct.hours.receive.2011
real.hours <- rates$hours.receive.2011
start <- 38
late <- direct.hours[start:52]
title='Rate Receive Adjust'
adj1 <- .455 # from earlier this year
adj2 <- .365 # from last year
indirect.late.adj1 <- late * adj1
indirect.late.adj2 <- late * adj2
late.adj1 <- indirect.late.adj1 + late
late.adj2 <- indirect.late.adj2 + late
adj1.line <- c(real.hours[1:(start - 1)], late.adj1) 
adj2.line <- c(real.hours[1:(start - 1)], late.adj2) 
y1 <- real.hours
y2 <- adj1.line
y3 <- adj2.line
png(file=path, width=width,
height=3.5, units="in", res=res)
y_range <- range(y1, y2,y3, na.rm=T)
xlab.text="week in year"
xlab = xlab.text
ylab = "units/man hour"
if (make.title == T ) top.margin = 3
if (make.title != T ) title="" 
if (months.grid == T) bottom.margin = 6
if (months.grid == T) xlab = ''
units <- rates$units.receive.2011
y1 <- units/y1
y2 <- units/y2
y3 <- units/y3
y_range <- range(y1,y2,y3)
margins=c(bottom.margin, 4, top.margin, 0.5) 
par(mar=margins) 
par('family'='serif')
plot(x, type='n', xlab=xlab, ylab=ylab, las=1, main=title, ylim=c(y_range), axes=F)
axis(1, col=ax_col)
axis(2, col=ax_col, las=1, at=axTicks(2))
box(col=box_col)
abline( h=axTicks(2), lty=1, lwd=lwd_grid, col=ax_col)
lines(x,y3, type="l", col=line_colors[1], lwd=lwd, lty=line_types[1])
#lines(x,y2, type="l", col=line_colors[2], lwd=lwd, lty=line_types[2])
lines(x,y1,  col=line_colors[2], type="l", lwd=lwd, lty=line_types[2])
x.poly = c(x, rev(x))
y.poly = c(y1, rev(y3))
polygon(x.poly, y.poly, col="lightgray", border=NA)
arrows(x0=10, x1=7, y0=150, y1=150, length=.1)
text(x=10, y=150,pos=4, "migration")
arrows(x0=30, x1=35, y0=80, y1=80, length=.1)
text(x=30, y=80,pos=2, "cutover")
legend("bottomleft", c("adjusted","real"), cex=legend_font_size, 
 col=line_colors,  lty=line_types, bty='y', bg="white", box.lwd=box.lwd)
source("make_x_axis.R")
dev.off()
