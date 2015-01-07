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
#     ref: the reference value(s) to be plotted as a thick line
#  limits: the boundaries of the limits to be plotted as backgound
#		  a four element vector providing: base, 1st limit, 2nd limit, upper limit.
#		  The base and limit are used for defining the scale
#   width: the defaul width of the main indicator as a portion of the graph width
#     col: the color of the indicato bar
# palette: the colors for the background limits
#

bulletgraph.palette <- function(n,shades){
  if(is.null(shades) ){ ## S.Few recommended gray levels
    if(n==2) cols = hsv(0,0,c(.65,.9))
    if(n==3) cols = hsv(0,0,c(.6,.75,.9))
    if(n==4) cols = hsv(0,0,c(.5,.65,.8,.9))
    if(n==5) cols = hsv(0,0,c(.5,.65,.8,.9,.97))
    if(n>5) cols = hsv(1,0,seq(.4,.97,length.out=n))  
  }else{
    if(length(shades)==1){
      cols = sapply(n:1*1/(n+1),adjustcolor,col=shades)
    }else{
      cols = shades
    }
  }
	return(cols)
}

bulletgraph <- function(x,ref,limits, projection=NULL, name=NULL, subname="", width=0.4, 
                        col=par("fg"), shades=NULL, reverse=F, 
                        perc=all(abs(c(x,ref,limits))<=1),...){
	if(length(limits)<3){
		stop("limits must be a vector with at least three elements")
	}
#	if(length(x)!=1){
#		stop(paste("x must be a scalar",name))
#	}
  nx = length(x)
  if(is.vector(limits)){
    limits = matrix(limits,nrow=1)
  }
  if(nx!=1 & dim(limits)[1]!=nx){
    stop("when 'limits' is a matrix it must have as many rows a the length of 'x'")
  }
  #limits=sort(limits)  
  limits = t(apply(limits,1,sort))
  if(any(x<limits[,1] | x>limits[,dim(limits)[2]])){
    stop("'x' must be within outer 'limits'")
  }
  if(any(ref<limits[,1] | ref>limits[,dim(limits)[2]])){
    stop("'ref' must be within outer 'limits'")
  }
	if(width<.01 | width>=1){
		stop("'width' must be in the range [0 .. 1]")
	}
	if(is.null(name)) name = paste(sys.call()[[2]],1:nx,sep=".")
	shades = bulletgraph.palette(dim(limits)[2]-1,shades)
  if(reverse){
    shades = shades[seq(length(shades),1)]
  }
  if(is.vector(ref)){
    ref = matrix(ref,nrow=1)
  }
  ns=dim(ref)[2]
  d = dim(limits)
  if(d[1]==1){
    ranges = matrix(limits[,2:d[2]] - c(min(0,limits[,1]),limits[,2:(d[2]-1)]),nrow=1)
    if(min(limits)<0){
      ranges <- cbind(min(limits[,1],0),ranges)
      shades <- c(NA,shades)
    }
  }else{
    ranges = limits[,2:d[2]] - cbind(sapply(limits[,1],0),limits[,2:(d[2]-1)])  
    if(min(limits)<0){
      ranges <- cbind(sapply(limits[,1],0),ranges) 
      shades <- c(NA,shades)  
    }
  }
  
  barplot(t(ranges),col=shades,border=NA,horiz=T,
        xlim=c(min(limits),max(limits)),xpd=F,xaxt=if(perc) "n"  else "s",...)
  if(perc){
    axis(1, at=pretty(limits), lab=paste0(pretty(limits) * 100,"%"))
  }
  if(min(limits)<0){
    segments(0,1:nx*1.2,0,1:nx*1.2-1,col="gray1")
  }
  pars = list(...)

  dist = .1
  segments(as.numeric(ref),rep(0:(nx-1)*1.2+.2+dist,ns),
           as.numeric(ref),rep(0:(nx-1)*1.2+1.2-dist,ns),
         lwd=rep(3*(ns:1/ns),each=nx),
         col=rep(sapply(ns:1/ns,adjustcolor,col=col),each=nx))
  if(limits[1]>0){
    if(is.null(pars$cex.names)){c=1}else{c=pars$cex.names}
    mtext(name,side=2,line=1,at=1:nx*1.2-.5,cex=c,las=2,padj=1)
    points(x,1:nx*1.2-.5,pch=4,cex=2*c)
  }else{
    if(!is.null(projection)){
      barplot(projection,width= width,beside=F,
              space=(if(nx==2) c((.2 + (1-width)),(.2 + (1-width)/2))
                     else c((.2 + (1-width)/2),rep((.2 + (1-width)),nx-1)))/width,
              add=T,horiz=T,border=NA,col=adjustcolor(col,.5),las=1,xpd=F,xaxt="n",...)
      
    }
    ## barplot bug workaround for spacing of 2 bars
	  barplot(x,width= width,names.arg=name,beside=F,
	      space=(if(nx==2) c((.2 + (1-width)),(.2 + (1-width)/2))
	          else c((.2 + (1-width)/2),rep((.2 + (1-width)),nx-1)))/width,
			  add=T,horiz=T,border=NA,col=col,las=1,xpd=F,xaxt="n",...)
  }
	mtext(subname,side=2,line=1,at=0.4,cex=0.6,adj=1,padj=1,las=2)
  if(limits[1]>0){
  #		warning("Bars should be drawn on a zero-based scale: using a jagged base to remark such non conformance.")
   		jit = (max(limits) - min(limits))/100
		x = c(rep(c(min(limits),min(limits)+jit),6),0)
		y = c(2:13/10,1.2)

    x = rep(c(0,rep(c(min(limits),min(limits)+jit),6),0),nx)
		y = c(.2,seq(.2,1.3,.1),1.3) + rep(0:(nx-1)*1.2,each=14)
		
		polygon(x,y,col="white",border="white")
	}
}

## test:

bulletgraph.demo <- function(){
	par(mfrow=c(4,1), mar=c(2,12,.1,1))	
	par(mar=c(2,9,.1,1))
	# reproducing Stephen Few example in the specification document
	bulletgraph(x=270,ref=260,limits=c(0,200,250,300),
            name= "Revenue 2005 YTD",subname="(U.S. $ in thousands)")

	# colored version of the above
	bulletgraph(x=270,ref=260,limits=c(0,200,250,300),
            col="steelblue4",shades="dodgerblue",
            name= "Revenue 2005 YTD",subname="(U.S. $ in thousands)")

	# an example with non-zero basis (not recommended, a warning is issued)
	x = 0.75
	ref=0.83
	limits=c(0.4,.6,.9,1)
	bulletgraph(x,ref,limits,width=0.6)
	
  # as above + two reference lines
	ref=c(0.83,0.8)
	bulletgraph(x,ref,limits,width=0.6,shades="firebrick",col="steelblue4")
	
  N=4
  par(mfrow=c(1,1),mar=c(2,6,.1,1))
	x = runif(N,.5,1)
  limits=matrix(rep(c(.6,.7,.9,1),each=N)+runif(N,-.1,.1),nrow=N)
  limits[,1] = .4
  limits[,4] = 1
  ref =matrix(rep(c(.85,.9,.5),each=N)*runif(N,.95,1.1),nrow=N)
  bulletgraph(x=x,ref=ref,limits=limits,name=paste("Indicator",1:N))
	bulletgraph(x=x,ref=ref,limits=limits,name=paste("Indicator",1:N),shades="olivedrab")
	
  
	bulletgraph(-20,200,c(-50,150,200,250),"Profit","(1,000s)")


	bulletgraph(59,75,c(0,40,80,120),"Expenses ($1,000s)",reverse=T)
	
}
