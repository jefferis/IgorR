#IgorR

[![Build Status](https://travis-ci.org/jefferis/IgorR.png)](https://travis-ci.org/jefferis/IgorR)

Introduction
============
The IgorR package for R provides routines to read binary files generated by [Igor Pro](http://www.wavemetrics.com).

This includes both the standalone .ibw Igor Binary Wave format (v2 and the latest v5) and the .pxp Packed Experiment Format including both waves and variables.  In addition the package provides functions to read files generated by the [Neuromatic](http://www.neuromatic.thinkrandom.com) analysis suite written by Jason Rothman for Igor Pro.

Installation
============
Standard Install
----------------
Install the release version from [CRAN](http://cran.r-project.org/)

    install.packages('IgorR')

Github Install
--------------
To install the latest development version from github

    install.packages('devtools') # install hadley's devtools
    library(devtools)
    install_github('IgorR','jefferis','pub')

Development Install
-------------------
To checkout a version that can be used for development (e.g. with StatET for Eclipse)

    cd /some/suitable/dir
    git clone https://github.com/jefferis/IgorR.git
    # git clone jgit:IgorR # or local repository 

In R

    install.packages('devtools') # install hadley's devtools
    library(devtools)
    load_all('/some/suitable/dir/IgorR')
    test()
    #hack
    load_all()
    
    # ready for release
    check()
    build_win() # test for Windows
    release()

Details
=======

Details of the ibw file format were derived from the [Igor Technical Note 003](ftp://anonymous@ftp.wavemetrics.net/IgorPro/Technical_Notes/TN003.zip). 

For those interested in the source code, ReadIgorBinary.R provides code for standard pxp and ibw files, while ReadNclamp.R provides routines to read packed experiment files generated by the Nclamp data acquisition package.