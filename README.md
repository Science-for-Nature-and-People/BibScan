# BibScan

An R package to batch download PDFs from a .bib file

## Installing

To install this package from GitHub you first need to install and run the ```devtools``` package.

```
install.pacakges("devtools")
library(devtools)
```

To install the package, you can then execute

```
install_github("swood-ecology/BibScan")
```

## Downloading files

To download files you need to be on a server that has a license to download from journal websites. The success rate of this package depends on the institutional access of the institution whose server you are on.

First, download a .bib file from a Web of Science search. Make sure that your .bib file includes a DOI.

You can then run

```
article_pdf_download(indir, outdir)
```

This will download PDFs from the .bib files in the director indir and save those PDFs in the director outdir.

## License

This package is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License, version 3, as published by the Free Software Foundation.

This program is distributed in the hope that it will be useful, but without any warranty; without even the implied warranty of merchantability or fitness for a particular purpose. See the GNU General Public License for more details.

A copy of the GNU General Public License, version 3, is available at https://www.r-project.org/Licenses/GPL-3

## Authors

* **Stephen Wood** - *Creator, Author* - [swood-ecology](https://github.com/swood-ecology)
* **Julien Brun** - *Author* - [brunj7](https://github.com/brunj7)
* **Timothy Nguyen** - *Author* - [timothydnguyen](https://github.com/timothydnguyen)

