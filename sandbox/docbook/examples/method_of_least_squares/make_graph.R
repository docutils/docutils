library('getopt')
spec = matrix(c(
  'draft' , 'm', 0, "logical"
), ncol=4, byrow=T)
opt = getopt(spec)
if (length(opt$draft) > 0 && opt$draft == T){
	res = 100
	path = 'force_displacment_draft.png'
}else{
	res = 500
	path = 'force_displacment.png'
}
x <- seq(from= 4, to=25, by=.2)
y <- 5 * x
y <- rnorm(length(y), mean=y, sd=4)
y_range <- range(y)
png(file=path, width=5.5, height=4.0, units="in", res=res)
par(bty='l', cex.axis=.75, tck=0.02, mar=c(2,2,1,1))
plot(x,y, ylim=y_range,  type='n', las=1, xlab='', ylab='', axes=F)
at <- seq(from=4, to=25, by= 2)
axis(1, )
axis(2, las=1)
points(x,y,cex=.3, pch=19 )
box()
dev.off()
