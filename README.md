# Supplementary material code for “Extended-support beta regression for \[0, 1\] responses”
Ioannis Kosmidis, Achim Zeileis
January 10, 2025

# Directory structure

The directory `code/` contains the scripts

<table style="width:51%;">
<colgroup>
<col style="width: 51%" />
</colgroup>
<thead>
<tr>
<th>script</th>
</tr>
</thead>
<tbody>
<tr>
<td>beta-vs-4-par-beta.R</td>
</tr>
<tr>
<td>beta01.R</td>
</tr>
<tr>
<td>lossaversion.R</td>
</tr>
<tr>
<td>reading-skills-interaction-stats.R</td>
</tr>
<tr>
<td>xbx-vs-beta-crps.R</td>
</tr>
<tr>
<td>xbx-vs-beta.R</td>
</tr>
<tr>
<td>xbx-vs-cn-crps.R</td>
</tr>
<tr>
<td>xbx-vs-cn.R</td>
</tr>
<tr>
<td>xbx-vs-ols-crps.R</td>
</tr>
</tbody>
</table>

that reproduce all the numerical results and figures in the manuscript

> Kosmidis, I. and Zeileis, A. (2024). Extended-support beta regression
> for \[0, 1\] responses. https://arxiv.org/abs/2409.07233

and the supplementary material document [`xbx-supp.pdf`](xbx-supp.pdf).

The directory `results/` is populated by R image files that store the
numerical results the scripts produce. The directory `figures/` is
populated by graphics that the scripts produce.

# R version and contributed packages

All results are reproducible using R version 4.4.2 (2024-10-31) and the
contributed packages

<table style="width:38%;">
<colgroup>
<col style="width: 23%" />
<col style="width: 13%" />
</colgroup>
<thead>
<tr>
<th>package</th>
<th>version</th>
</tr>
</thead>
<tbody>
<tr>
<td>betareg</td>
<td>3.2-1</td>
</tr>
<tr>
<td>crch</td>
<td>1.2-1</td>
</tr>
<tr>
<td>distributions3</td>
<td>0.2.2</td>
</tr>
<tr>
<td>dplyr</td>
<td>1.1.4</td>
</tr>
<tr>
<td>ggplot2</td>
<td>3.5.1</td>
</tr>
<tr>
<td>lmtest</td>
<td>0.9-40</td>
</tr>
<tr>
<td>topmodels</td>
<td>0.3-0</td>
</tr>
<tr>
<td>VGAM</td>
<td>1.1-12</td>
</tr>
</tbody>
</table>

At the time of writing the `topmodels` package is not on CRAN. Please
install it by doing

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

`7.` [`code/xbx-vs-cn-beta.R`](code/xbx-vs-beta-crps.R) and
[`code/xbx-vs-cn-ols.R`](code/xbx-vs-ols-crps.R) carry out the numerical
experiments in Section S5 of the supplementary material document, and
reproduce

-   Figure S9 of the supplementary material document
-   Figure S10 of the supplementary material document
