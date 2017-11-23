\name{add.catch.frq}
\alias{add.catch.frq}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
add.catch.frq.Rd
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Used in sensitivity analyses, this function replaces the catch column relating to the fisheries specified - while accounting for the possibility that the number 
of rows or order differ between the sensitivity runs. 
The script assumes that the sensitivity *.frq will contain either: only those rows which are to be modified; or all rows to be modified. 
}
\usage{
add.catch.frq(frq, filepath, fshries)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{frq}{
%%     ~~Describe \code{frq} here~~
The frq file object
}
  \item{filepath}{
%%     ~~Describe \code{filepath} here~~
The path and filename of the frq file with the replacement fishery data. 
}
  \item{fshries}{
%%     ~~Describe \code{fshries} here~~
The id numbers of the fisheries to be edited 
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
Nick Davies
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
