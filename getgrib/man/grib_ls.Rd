\name{grib_ls}
\alias{grib_ls}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Mimiking the GRIB_API \code{grib_ls} Command}
\description{
   R interface to \code{grib_ls}. Currently only working on lnx systems.
   Returns \code{grib_ls} output as a \code{data.frame} which can
   be used to screen the content of grib files.
}
\usage{
   ## Basic usage. Using a default 'parameter' setup which
   ## actually differs from the grib_ls default command.
   grib_ls(file)
   ## Setting a different set of parameters (getting different meta data).
   ## This is the '-p' setting of the original grib_ls executable.
   grib_ls(file, parameters)
   ## Selective 'where' statement. This is the '-w' option of the 
   ## grib_ls executable.
   grib_ls(file, where)
}
\arguments{
   \item{file}{Character string. File name of the grib file to be concidered.}
   \item{parameters}{Optional, either a \code{character} string or a 
      \code{character} array to specify which meta information should be
      extracted. See details for more information.}
   \item{where}{Optional. Either a \code{character} string or a list.
      See details for more information.}
}
\details{
   Mimiking the \code{grib_ls} executable from the ECWMF GRIB_API.
   If \code{parameters} and \code{where} is not given a default set
   will be returned including \code{centre}, \code{dataDate}, \code{dataTime},
   \code{perturbationNumber}, and \code{step} from the grib headers.

   The outcome is a simple \code{data.frame} containing the corresponding
   information. For more specific or different outcomes the \code{-p} and
   \code{-w} option of the \code{grib_api} interface have been made available.
   Please see \code{grib_ls} (on your console) for more information.

   The \code{parameter} option can either be a simple \code{character} string
   or a \code{character} array. For example: you are interested in the
   grid dimension \code{Ni} and \code{Nj} plus the \code{shortName}:
   - Either use: \code{grib_ls(file,parameters='Ni,Nj,step')} 
   - Or: \code{grib_ls(file,parameters=c('Ni','Nj','step'))}

   The \code{where} is to select a subset of messages. Can be either a
   \code{character} string or a \code{list}. Note: if a list is given
   each pair of key/value is interpreted as \code{key==value}. If you
   need other operators (not equal to) use the \code{character} string
   option. See \code{grib_ls} executable help for more information.
   For example:
   - Use: \code{grib_ls(file,where='step=12,shortName=!2t')}
   - Or: \code{grib_ls(file,where=list('step'=12,'shortName'='2t'))}
}
\examples{
   # Find package installation path including the demo grib file
   file <- paste(path.package("getgrib"),"data/ECEPS_12.grib",sep="/")

   # Examples
   grib_ls(file,parameters='Ni,Nj,step') 
   grib_ls(file,parameters=c('Ni','Nj','step'))
   grib_ls(file,where='step=12,shortName!=2t')
   grib_ls(file,where=list('step'=12,'shortName'='2t'))

   ## See "getdata" manual to see why I've did this.
}
\seealso{
   \code{\link{getdata}}
}
%- maybe also 'usage' for other objects documented here.
\author{Reto Stauffer}
