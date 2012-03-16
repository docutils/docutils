data = read.csv('../tables/projected_summary_costs.csv')
attach(data)
div.factor = 1000000
Zappos = Zappos/div.factor
FCSW = FCSW/div.factor
my.table = matrix(c(Zappos, FCSW), nrow=2, byrow=T)
my.ticks=seq(from = 0, to= 24, by=2)
tick_multiplier= .5
png(file="total_costs.png", width=width, height=3.5, units="in", res=res )
# pdf(file="../xml/charts/total_costs_r.pdf", height=5 )
par(mar=margins_bar) 
barplot(my.table, axes=F, col="white", border=NA, beside=T, ylim=c(0,26))
abline( h=my.ticks, lty=1, lwd=lwd_grid, col=ax_col)
axis(2, col=ax_col, las=1, at=my.ticks )
par(new=T)
barplot(my.table, beside=T, names.arg=c("return", "receive", "static pick", "excep vol.", "total"),
    col=bar_color, las=1,ylab="millions of dollars", space=c(0,.5), axis.lty=1, 
    axes=F,  cex.names=bar_label_size, ylim=c(0,26),
        )
box(col=box_col)
legend("topleft", legend=c("Pre-Cutover", "Post-Cutover"), fill=bar_color, bty="n")    
dev.off()


