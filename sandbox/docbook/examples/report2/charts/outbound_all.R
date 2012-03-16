rates <- read.csv("../tables/all.csv")
x = rates$week
title=''
xlab.text="week in year"
xlab = xlab.text
ylab = "units/man hour"
if (make.title == T ) top.margin = 3
if (make.title != T ) title="" 
if (months.grid == T) bottom.margin = 6
if (months.grid == T) xlab = ''
y_picking = rates$rate.picking.2011
y_singulation = rates$rate.singulate.2011
y_singles = rates$rate.singles.2011
y_multis = rates$rate.multis.2011
png(file="outbound_rate_2011_all.png", width=width, height=4.5, units="in", res=res )
y_range=range(y_singulation, y_singles, y_multis, y_picking, na.rm=T)
margins=c(bottom.margin, 4, top.margin, 0.5) 
par(mar=margins) 
par('family'='serif')
plot(x, type='n', xlab=xlab, ylab=ylab, las=1,
main=title, ylim=c(y_range), axes=F)
axis(1, col=ax_col)
axis(2, col=ax_col, las=1, at=axTicks(2))
box(col=box_col)
rect(38, -10, 39, 1000, col=rec_col, border=NA, xpd=F)
abline( h=axTicks(2), lty=1, lwd=lwd_grid, col=ax_col)
lines(x,y_picking, col=line_colors[1],  lwd=lwd, lty=line_types[1])
lines(x,y_singulation, col=line_colors[2],  lwd=lwd, lty=line_types[2])
lines(x,y_singles, col=line_colors[3],  lwd=lwd, lty=line_types[3])
lines(x,y_multis, col=line_colors[4],  lwd=lwd, lty=line_types[4])
# legend(x = 0,y=-50, ncol=4, c("Picking","Singulation", "Singles", "Multis"), cex=legend_font_size, 
#  col=line_colors,  lty=line_types, bty='y', bg="white", box.lwd=box.lwd, xpd = T)
 legend("topleft", c("Picking","Singulation", "Singles", "Multis"), cex=legend_font_size, 
  col=line_colors,  lty=line_types, bty='y', bg="white", box.lwd=box.lwd, xpd = T)
source("make_x_axis.R")
dev.off()

