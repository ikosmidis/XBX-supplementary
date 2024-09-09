# Supplementary material code for “Extended-support beta regression for \[0, 1\] responses”

Ioannis Kosmidis, Achim Zeileis

September 7, 2024

The current directory provides the scripts

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
<td>xbx-vs-cn.R</td>
</tr>
<tr class="odd">
<td>xbx-vs-htobit-crss.R</td>
</tr>
</tbody>
</table>

that reproduce all the numerical results and figures in the manuscript

> Kosmidis, I. and Zeileis, A. (2024). Extended-support beta regression
> for \[0, 1\] responses.

and the supplementary material document \[ADD ARXIV ID AND LINKS TO
GITHUB\]

All results are exactly reproducible using R version 4.4.1 (2024-06-14)
and the contributed packages

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

and may remain reproducible with future versions.

At the time of writing the `topmodels` are package is not on CRAN.
Please install it by doing

``` r
install.packages("topmodels", repos = "https://zeileis.r-universe.dev")
```

The results are reproduced as follows:

-   `reading-skills-interaction-stats.R` reproduces Figure 1 in the main
    text
-   `beta-vs-4-par-beta.R` reproduces Figure 2 in the main text
-   `xbx-vs-beta.R` reproduces Figure 3 in the main text
-   `xbx-vs-cn.R` reproduces Figure S1 and Figure S2 in Section S1 of
    the supplementary material document
-   `lossaversion.R` reproduces:
    -   the numerical figures in Table 1, Table 2, and Table 3 of the
        main text
    -   Figure 4 and Figure 5 of the main text
    -   the Wald and likelihood ratio statistics reported in Section 4.5
        of the main text
    -   Figure S3 of the supplementary material document
    -   the numerical figures in Table S1, Table S2 and Table S3 of the
        supplementary material document
-   `beta01.R` provides support functions and methods for the analyses
    in `lossaversion.R`
-   `xbx-vs-htobit-crss.R` carries out the numerical experiment in
    Section 5.1. The experiment uses parallel computing via the
    `mclapply()` function of the parallel R package, hence, it is
    exactly reproducible in macOS and Linux systems. For Windows
    systems, replace the `mclapply()` call with `lapply()` or modify the
    script to use a different parallel computing backend. The script
    also reproduces Table 4 and Figure 6 of the main text, and Figures
    S4-S8 in the supplementary material document.
