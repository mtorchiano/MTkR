\name{bulletgraph}
\alias{bulletgraph}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
Creates a bullet graph with orizontal bars.
}
\usage{
bulletgraph(x, ref, limits, 
            name=NULL, subname="", width=0.4, 
            col=NULL, shades=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
a numerical or a vector representing the value(s) of the indicator to be plotted as the main bar.
}
  \item{ref}{
a vector or a matrix containing the value(s) to be plotted as reference thick vertical lines.
  }
  
  \item{limits}{
a vector or a matrix representing the boundaries of the limits to be plotted as backgounds, the length (if vector), or number of columns must be at least 3. The extreme values (\code{min} and \code{max}) define the scale limits. Both the indicator value(s) and the reference(s) must be within the scale.
}
  \item{projection}{
    a numerical or a vector representing the projected value of the indicator, e.g. at the end of a standard period of time
  }
  \item{name}{ a caracter value or vector with the name(s) of the indicators.}
  \item{subname}{ a caracter value or vector with the subtitles(s) of the indicators.}
  \item{width}{
  the width of the main indicator bar as a portion of the background limits width
  }
  \item{col}{ 
  a optional character of the color for the indicator bar, by default \code{par("fg")}
  }
  \item{shades}{
   the colors for the background limits, by default shades of gray are used.
  }
  \item{reverse}{
  boolean, indicates whether the background shades should be reversed.
  }
  \item{perc}{
  indicates whether the axis should be printed as percentage, if all the values are below 1
  then it defaults to \code{TRUE}, otherwise to \code{FALSE}.
  }
}
\details{
This function generates a bullet graph as specified by S.Few.
Bullet graphs have been devised by Stephen Few as a mean to represent indicators.

It is recommended to have zero-based scales, because the bars lengths can be best compared perceptually when the lengths are proportional to the actual value i.e. when using a zero-based scale. When this is not true, a lie-factor (see Tufte) is introduced. In this case a warning is issued and a jagged margin is used to emphasize it.

}
\value{
No value is returned
}
\references{
The complete specification of bullet graphs:
\url{http://www.perceptualedge.com/articles/misc/Bullet_Graph_Design_Spec.pdf}}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ bullet }
\keyword{ graph }% __ONLY ONE__ keyword per line
