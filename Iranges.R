library(IRanges)
ir1 <- IRanges(start=1:10, width=10:1)
ir2 <- IRanges(start=1:10, end=11)
ir3 <- IRanges(end=11, width=10:1)
identical(ir1, ir2) && identical(ir1, ir3)
ir <- IRanges(c(1, 8, 14, 15, 19, 34, 40),width=c(12, 6, 6, 15, 6, 2, 7))
start(ir)
ir1 <- IRanges(start=1:10, width=10:1)
ir1
ir2 <- IRanges(start=1:10, end=11)
ir3 <- IRanges(end=11, width=10:1)
identical(ir1, ir2) && identical(ir1, ir3)
ir <- IRanges(c(1, 8, 14, 15, 19, 34, 40),
        width=c(12, 6, 6, 15, 6, 2, 7))
ir
start(ir)
end(ir)
width(ir)
ir[1:4]
ir[start(ir) <= 15]
plotRanges <- function(x, xlim=x, main=deparse(substitute(x)),
                       col="black", sep=0.5, ...)
{
  height <- 1
  if (is(xlim, "IntegerRanges"))
    xlim <- c(min(start(xlim)), max(end(xlim)))
  bins <- disjointBins(IRanges(start(x), end(x) + 1))
  plot.new()
  plot.window(xlim, c(0, max(bins)*(height + sep)))
  ybottom <- bins * (sep + height) - height
  rect(start(x)-0.5, ybottom, end(x)+0.5, ybottom + height, col=col, ...)
  title(main)
  axis(1)
}
plotRanges(ir)
reduce(ir)
plotRanges(reduce(ir))
rl <- IRangesList(ir, rev(ir))
start(rl)
set.seed(0)
lambda <- c(rep(0.001, 4500), seq(0.001, 10, length=500),seq(10, 0.001, length=500))
xVector <- rpois(1e7, lambda)
yVector <- rpois(1e7, lambda[c(251:length(lambda), 1:250)])
xRle <- Rle(xVector)
yRle <- Rle(yVector)
irextract <- IRanges(start=c(4501, 4901) , width=100)
xRle[irextract]
ol <- findOverlaps(ir, reduce(ir))
as.matrix(ol)
cov <- coverage(ir)
plotRanges(ir)
cov <- as.vector(cov)
mat <- cbind(seq_along(cov)-0.5, cov)
d <- diff(cov) != 0
mat <- rbind(cbind(mat[d,1]+1, mat[d,2]), mat)
mat <- mat[order(mat[,1]),]
lines(mat, col="red", lwd=4)
axis(2)
shift(ir, 10)
narrow(ir, start=1:5, width=2)
restrict(ir, start=2, end=3)
threebands(ir, start=1:5, width=2)
ir + seq_len(length(ir))
ir * -2 # double the width
disjoin(ir)
plotRanges(disjoin(ir))
disjointBins(ir)
reflect(ir, IRanges(start(ir), width=width(ir)*2))
flank(ir, width=seq_len(length(ir)))
gaps(ir, start=1, end=50)
plotRanges(gaps(ir, start=1, end=50), c(1,50))
xViews <- Views(xRle, xRle >= 1)
xViews <- slice(xRle, 1)
xRleList <- RleList(xRle, 2L * rev(xRle))
xViewsList <- slice(xRleList, 1)
head(viewSums(xViews))
viewSums(xViewsList)
head(viewMaxs(xViews))
viewMaxs(xViewsList)
showClass("RleList")
args(IntegerList)
cIntList1 <- IntegerList(x=xVector, y=yVector)
cIntList1
sIntList2 <- IntegerList(x=xVector, y=yVector, compress=FALSE)
sIntList2
## sparse integer list
xExploded <- lapply(xVector[1:5000], function(x) seq_len(x))
cIntList2 <- IntegerList(xExploded)
sIntList2 <- IntegerList(xExploded, compress=FALSE)
object.size(cIntList2)
object.size(sIntList2)
length(cIntList2)
Rle(lengths(cIntList2))
system.time(sapply(xExploded, mean))
system.time(sapply(sIntList2, mean))
system.time(sapply(cIntList2, mean))
identical(sapply(xExploded, mean), sapply(sIntList2, mean))
identical(sapply(xExploded, mean), sapply(cIntList2, mean))
xRleList > 0
yRleList <- RleList(yRle, 2L * rev(yRle))
xRleList + yRleList
sum(xRleList  >0 | yRleList  >0)
safe.max <- function(x) { if(length(x)) max(x) else integer(0) }
endoapply(sIntList2, safe.max)

endoapply(cIntList2, safe.max)

endoapply(sIntList2, safe.max)[[1]]
