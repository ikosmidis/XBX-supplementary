# Supplementary material code for “Extended-support beta regression for \[0, 1\] responses”

Ioannis Kosmidis, Achim Zeileis

September 10, 2024

# Directory structure

The directory `code/` contains the scripts

<table style="width:51%;">
<colgroup>
<col style="width: 51%" />
</colgroup>
<thead>
<tr class="header">
<th>script</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td>beta-vs-4-par-beta.R</td>
</tr>
<tr class="even">
<td>beta01.R</td>
</tr>
<tr class="odd">
<td>lossaversion.R</td>
</tr>
<tr class="even">
<td>reading-skills-interaction-stats.R</td>
</tr>
<tr class="odd">
<td>xbx-vs-beta.R</td>
</tr>
<tr class="even">
<td>xbx-vs-cn-crps.R</td>
</tr>
<tr class="odd">
<td>xbx-vs-cn.R</td>
</tr>
</tbody>
</table>

that reproduce all the numerical results and figures in the manuscript

> Kosmidis, I. and Zeileis, A. (2024). Extended-support beta regression
> for \[0, 1\] responses.

and the supplementary material document [`xbx-supp.pdf`](xbx-supp.pdf).

The directory `results/` is populated by R image files that store the
numerical results the scripts produce. The directory `figures/` is
populated by graphics that the scripts produce.

# R version and contributed packages

All results are reproducible using R version 4.4.1 (2024-06-14) and the
contributed packages

<table style="width:38%;">
<colgroup>
<col style="width: 23%" />
<col style="width: 13%" />
</colgroup>
<thead>
<tr class="header">
<th>package</th>
<th>version</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td>betareg</td>
<td>3.2-0</td>
</tr>
<tr class="even">
<td>crch</td>
<td>1.2-0</td>
</tr>
<tr class="odd">
<td>distributions3</td>
<td>0.2.1</td>
</tr>
<tr class="even">
<td>dplyr</td>
<td>1.1.4</td>
</tr>
<tr class="odd">
<td>ggplot2</td>
<td>3.5.1</td>
</tr>
<tr class="even">
<td>lmtest</td>
<td>0.9-40</td>
</tr>
<tr class="odd">
<td>topmodels</td>
<td>0.3-0</td>
</tr>
<tr class="even">
<td>VGAM</td>
<td>1.1-11</td>
</tr>
</tbody>
</table>

At the time of writing the `topmodels` are package is not on CRAN.
Please install it by doing

``` r
install.packages("topmodels", repos = "https://zeileis.r-universe.dev")
```

# Reproducing the results

The results can be reproduced by using an interactive R session and
going through the following scripts line-by-line:

`1.`
[`code/reading-skills-interaction-stats.R`](code/reading-skills-interaction-stats.R)
reproduces

-   Figure 1 in the main text;

`2.` [`code/beta-vs-4-par-beta.R`](code/beta-vs-4-par-beta.R) reproduces

-   Figure 2 in the main text;

`3.` [`code/xbx-vs-beta.R`](code/xbx-vs-beta.R) reproduces

-   Figure 3 in the main text;

`4.` [`code/xbx-vs-cn.R`](code/xbx-vs-cn.R) reproduces

-   Figures S1-S2 in Section S1 of the supplementary material document;

`5.` [`code/lossaversion.R`](code/lossaversion.R) reproduces

-   the numerical figures in Tables 1-3 of the main text,
-   Figures 4-5 of the main text,
-   the Wald and likelihood ratio statistics reported in Section 4.5 of
    the main text,
-   Figure S3 in Section S2 of the supplementary material document,
-   the numerical figures in Tables S1-S3 in Section S2 of the
    supplementary material document;

`6.` [`code/xbx-vs-cn-crps.R`](code/xbx-vs-cn-crps.R) carries out the
numerical experiment in Section 5, and reproduces

-   Figure 6 of the main text,
-   Figures S4-S8 in Section S3 of the supplementary material document.

Computation here relies on parallel computing, which is implemented
through the `parallel` R package. The script will not work on Windows
unless `n_cores <- 1` (which will lead in long compute times and is not
recommended) or it is modified to use a different parallel back-end. All
results should be exactly reproducible in Unix-based systems (e.g. macOS
and Linux).
