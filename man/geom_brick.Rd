\name{geom_brick}
\alias{geom_brick}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
geom_brick
}
\description{
In geom_brick, the height of a rectangle corresponds to the bin width and the width of a rectangle corresponds to the counting the number of observations in each bins.}
\usage{
geom_brick(mapping = NULL, data = NULL, position = "identity", ...,
           bins = NULL, binwidth = NULL, binaxis = "y", binpositions = "all",
           stackdir = "up", stackratio = 1, dotsize = 1, stack_scale = 0.85,
           stackgroups = TRUE, width = 0.9, drop = FALSE, na.rm = FALSE,
           show.legend = NA, inherit.aes = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mapping}{
Set of aesthetic mappings created by aes() or aes_(). If specified and inherit.aes = TRUE (the default), it is combined with the default mapping at the top level of the plot. You must supply mapping if there is no plot mapping.}
  \item{data}{
The data to be displayed in this layer.}
  \item{position}{
Position adjustment, either as a string, or the result of a call to a position adjustment function
}
  \item{\dots}{
Other arguments passed on to layer(). These are often aesthetics, used to set an aesthetic to a fixed value, like colour = "red" or size = 3. They may also be parameters to the paired geom/stat.
}
  \item{bins}{
Number of bins. Overridden by binwidth. Defaults to 30
}
  \item{binwidth}{
This specifies maximum bin width. When method is "histodot", this specifies bin width. Defaults to 1/30 of the range of the data}
  \item{binaxis}{
The axis to bin along, "y" (default) or "x"
}
  \item{binpositions}{
"all" (default) determines positions of the bins with all the data taken together; this is used for aligning dot stacks across multiple group. "bygroup" determines positions of the bins for each group separately. }
  \item{stackdir}{
which direction to stack the dots. "up" (default), "down", "center", "centerwhole" (centered, but with dots aligned)
}
\item{stackratio}{
how close to stack the dots. Default is 1, where dots just just touch. Use smaller values for closer, overlapping dots.
}
  \item{dotsize}{
The diameter of the rectangles relative to binwidth, default 1.}
  \item{stack_scale}{
A scaling factor to scale the width of the rectangles relative to the spacing between them (default 0.85).}
  \item{stackgroups}{
should dots be stacked across groups? This has the effect that position = "stack" should have, but can't (because this geom has some odd properties).}
  \item{width}{
When binaxis is "y", the spacing of the rectangle stacks for dodging.
}
  \item{drop}{
If TRUE, remove all bins with zero counts
}
  \item{na.rm}{
If FALSE, the default, missing values are removed with a warning. If TRUE, missing values are silently removed.
}
  \item{show.legend}{
logical. Should this layer be included in the legends? NA, the default, includes if any aesthetics are mapped. FALSE never includes, and TRUE always includes. It can also be a named logical vector to finely select the aesthetics to display.}
  \item{inherit.aes}{
If FALSE, overrides the default aesthetics, rather than combining with them. This is most useful for helper functions that define both data and aesthetics and shouldn't inherit behaviour from the default plot specification, e.g. borders().}
}
\author{
ko ABE
}

%% ~Make other sections like Warning with \section{Warning }{....} ~
\examples{
library(ggplot2)
library(ggbrick)
ggplot(data = iris) +
  geom_brick(aes(y = Sepal.Length, x=Species), binwidth = 0.1)

ggplot(data = iris) +
  geom_brick(aes(y = Sepal.Length, x=Species), binwidth = 0.5)

ggplot(data = iris) +
  geom_brick(aes(y = Sepal.Length, x=Species), binwidth = 0.5, fill = "black")

ggplot(data = mpg,aes(y = cty, x=factor(year), fill=factor(cyl))) +
  geom_brick(binwidth = 1)

ggplot(data = mpg,aes(y = cty, x=factor(year), fill=factor(cyl))) +
  geom_brick(binwidth = 1, stackgroups = FALSE, alpha = 0.5)

ggplot(data = mpg,aes(y = cty, x=factor(year), fill=factor(cyl))) +
  geom_brick(binwidth = 1, stackgroups = FALSE, alpha = 0.5,
            stackdir = "centerwhole", position = position_dodge(0.5))

ggplot(data = diamonds, aes(x = color, y=carat, colour=cut)) +
  geom_brick(binwidth=0.2) +
  coord_flip()

ggplot(data = iris,aes(y = Sepal.Length, x=Species)) +
  geom_brick(binwidth = 0.1, stackdir = "centerwhole")+
  stat_summary(fun.y = median, fun.ymin = median, fun.ymax = median,
               geom = "crossbar")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
