Installation
============

1. Download and install the latest [Haskell-platform](https://www.haskell.org/platform/).

2. In a terminal, type `cabal update`.

3. And then `cabal install BCMtools`.

You will have the `bcmtools` executable in you cabal library directory. In Linux, this is typically under `$HOME/.cabal/bin/`. You can type `bcmtools --help` to get help information, and type `bcmtools COMMAND --help` to get more specific information.

Input format
============

BCMtools accept two types of input.

1. 3-column tsv. Example:

```
chr1  \t chr2  \t count
5000  \t 10000 \t  3.0
10000 \t 20000 \t  4.0
 .     .   .   .    .
 .     .   .   .    .
 .     .   .   .    .
```

In this format, the first line contains chromsome information. Every entries in the same column would then belong to the same chromsome.

2. 5-column tsv. Example:

```
chr1 \t 5000 \t chr2 \t 20000 \t 4.0
chr1 \t 5000 \t chr1 \t 30000 \t 2.0
```

File conversion
===============

To use BCMtools, we will need to convert text file to bcm files. bcm files are binary files which are designed to store huge matrix. This step will take 3-column/5-column tsv as input. Example:

``bcmtools convert -g hg19 -s 100K -r chr1 -c chr1 -i input --symmetric -o 100K.bcm``

Visualization
=============

``bcmtools view 100K.bcm --range 0-150 -o ouput.png``

![100K](example/GM12878_100K.png)

One key feature of bcmtools is that it uses constant memory. This is achieved by storing and processing matrix on hard drive. This is could be very useful when the matrix is large and memory is scarce. However, if you want to drain out full speed, you can turn on the `--memory` flag, which would read whole matrix in memory. This would typical gives 3-7X performance increase depends on the rotation speed of your hard drive.