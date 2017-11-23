\name{get.flags}
\alias{get.flags}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
get.flags.Rd
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
MFCL flag descriptions
}
\usage{
getflags(flags)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{flagsj}{
  a character vector of flag arguments (including the n.flags arg at the beginning of the string).
%%     ~~Describe \code{frq.obj} here~~
}
}
\details{
The flags argument takes the full list of numeric flag settings as passed to MFCL, including the first number which 
specifies the number of flags in the list.

}
\value{
Returns a data.frame of flags and flag descriptions
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

Robert Scott
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

get.flags("6 1 1 1 2 190 0 2 191 0 2 148 4 2 155 0 -999 55 1")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
