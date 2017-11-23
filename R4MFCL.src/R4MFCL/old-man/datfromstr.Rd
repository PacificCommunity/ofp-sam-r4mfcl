\name{datfromstr}
\alias{datfromstr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
datfromstr.Rd
}
\description{
converts a character string of numbers to a numeric vector
}
\usage{
datfromstr(datstring)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{datstring}{
A character string of numbers seperated by whitespace
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
Shelton Harley

}
\note{
First character of string must be a space - see examples below
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

class(datfromstr("2 4 5 66 786 2 3 1"))

el(datfromstr("2 4 5 66 786 2 3 1"),1)
el(datfromstr(" 2 4 5 66 786 2 3 1"),1)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
