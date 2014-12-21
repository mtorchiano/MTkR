#----------------------------------------#
# Bullet Graph
#
# Bullet graphs have been devised by Stephen Few
# A complete specification of bullet graphs can be retrieved at:
# http://www.perceptualedge.com/articles/misc/Bullet_Graph_Design_Spec.pdf
#
#   Author: Marco Torchiano
#    		email: marco.torchiano@polito.it
#  	       twitter: @mtorchiano
#
#    Version: 1.1 (20 December 2014)
#
#    Copyright (c) 2014 by Marco Torchiano <marco.torchiano@polito.it>
#
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
#----------------------------------------#
#
# usage:
#       x: the value of the indicator to be plotted as the main bar
#     ref: the reference value to be plotted as a thick line
#  limits: the boundaries of the limits to be plotted as backgound
#		  a four element vector providing: base, 1st limit, 2nd limit, upper limit.
#		  The base and limit are used for defining the scale
#   width: the defaul width of the main indicator as a portion of the graph width
#     col: the color of the indicato bar
# palette: the colors for the background limits
#

bulletgraph.palette <- function(n,colored){
	if(colored){ # this colors are up to my personal taste ;-)
		if(n==2) cols = c("firebrick","dodgerblue")
		if(n==3) cols = c("firebrick","orange","olivedrab1")
		if(n==4) cols = c("firebrick","orange","powderblue","olivedrab1")
		if(n==5) cols = c("firebrick","orange","powderblue","plum","olivedrab1")
		if(n>5) cols = hsv((n:1 + n*0.3)/(n*1.3),.9,.8+(1:n/(n*5)))
	}else{ # these are the gray levels recommended by Stephen Few
		if(n==2) cols = hsv(0,0,c(.65,.9))
		if(n==3) cols = hsv(0,0,c(.6,.75,.9))
		if(n==4) cols = hsv(0,0,c(.5,.65,.8,.9))
		if(n==5) cols = hsv(0,0,c(.5,.65,.8,.9,.97))
		if(n>5) cols = hsv(1,0,seq(.4,.97,length.out=n))
	}
	return(cols)
}

bulletgraph <- function(x,ref,limits, name=NULL, subname="", width=0.4, col=NULL, 
						palette=NULL,colored=T){
	if(length(limits)<3){
		stop("limits must be a vector with at least three elements")
	}
	if(length(x)!=1){
		stop(paste("x must be a scalar",name))
	}
	limits=sort(limits)
	if(x<limits[1] | x>limits[4]){
		stop("x must be within outer limits")
	}
	if(ref<limits[1] | ref>limits[4]){
		stop("x must be within outer limits")
	}
	if(width<.01 | width>1){
		stop("width must be in the range [0 .. 1]")
	}
	if(is.null(name)) name = sys.call()[[2]]
	if(is.null(palette)){
		palette = bulletgraph.palette(length(limits)-1,colored)
	}
	if(is.null(col)){
		if(colored) col="steelblue3"
		else col = "black"
	}
	n = length(limits)
	ranges = matrix(tail(limits,-1)-c(0,head(tail(limits,-1),n-2)),n-1)
	barplot(ranges,col=palette,border=NA,horiz=T,
				xlim=c(min(limits),max(limits)),xpd=F)
	segments(ref,.3,ref,1.1,lwd=3)
	barplot(x[1],width= width,names.arg=name,cex.names=1,
			space=((1-width)/2+0.2)/width,
			add=T,horiz=T,border=NA,col=col,las=1,xpd=F)
	mtext(subname,side=2,line=1,at=0.4,cex=0.6,adj=1,padj=1,las=2)
  	if(limits[1]!=0){
  		warning("Bars should be drawn on a zero-based scale: using a jagged base to remark such non conformance.")
   		jit = (limits[4] - limits[1])/100
		x = c(rep(c(limits[1],limits[1]+jit),6),0)
		y = c(2:13/10,1.2)
		polygon(x,y,col="white",border="white")
	}
}

## test:

bulletgraph.demo <- function(){
	par(mfrow=c(3,1), mar=c(2,12,.1,1))	
	par(mar=c(2,9,.1,1))
	# reproducing Stephen Few example in the specification document
	bulletgraph(x=270,ref=260,limits=c(0,200,250,300),
            name= "Revenue 2005 YTD",subname="(U.S. $ in thousands)",colored=F)

	# colored version of the above
	bulletgraph(x=270,ref=260,limits=c(0,200,250,300),
            name= "Revenue 2005 YTD",subname="(U.S. $ in thousands)")

	# an example with non-zero basis (no recommended)
	x = 0.75
	ref=0.83
	limits=c(0.4,.6,.9,1)
	bulletgraph(x,ref,limits,width=0.6)
}
