comicsia-downloader
===================

The tool to download comics from site http://comicsia.ru

Usage: 

comicsdl URL

where URL is any url of comics in comicsia ru to start download from:

comicsdl http://comicsia.ru/collections/ru_comicstrip/i2670.html

comicsdl now supports limiting arguments, which allow to specify first and
last comics to download. If url, specified to the tool points to comics not
in range of download, tool will follow links until it finds comics in range.

do a

comicsdl --help

to see options.
