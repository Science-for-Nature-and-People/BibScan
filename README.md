# Project Title

BibScan: an R package to batch download PDFs from a .bib file

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

## Authors

* **Stephen Wood** - *Initial work* - [swood-ecology](https://github.com/swood-ecology)
* **Timothy Nguyen** - *Initial work* - [timothydnguyen](https://github.com/timothydnguyen)

## Acknowledgments

* Thanks to Julien Brun for providing input on code and technical support
