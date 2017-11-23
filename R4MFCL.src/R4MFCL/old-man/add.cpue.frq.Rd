\name{add.cpue.frq}
\alias{add.cpue.frq}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
add.cpue.frq.Rd
}
\description{
Replaces the nominal effort in the original .FRQ file with stanadrdised effort based on the CPUE index
Flexible to handle either sort of frq file and you have the choice to include the cv.  
Puts in -1 for effort first to make sure we account for missing values of CPUE 
}
\usage{
add.cpue.frq(CPUE.file = "P:/yft/2009/Data Preparation/CPUE/indices/yft_JPstd_R1.txt", data = out.data, fishery = 1, add.cv = "T")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{CPUE.file}{
%%     ~~Describe \code{CPUE.file} here~~
}
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{fishery}{
%%     ~~Describe \code{fishery} here~~
}
  \item{add.cv}{
%%     ~~Describe \code{add.cv} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
Shelton Harley
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
