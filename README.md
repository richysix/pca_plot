# PCA Plot

A shiny app for visualising results from a PCA.

## Instructions

The app is designed to produce 2D scatterplots of the results of a
Principal Component Analysis.

### Loading data

Data can be loaded using the buttons at the top of the side bar control
panel. Users need to supply a PCA data file along with a sample file.

#### PCA file

The first column is expected to be sample ids and must match the first
column of the samples file. The column names for the principal component
data are expected to be of the form PC*number*

e.g.
<table class="table table-light">
<thead>
<tr>
<th style="text-align:left;">
sample name
</th>
<th style="text-align:right;">
PC1
</th>
<th style="text-align:right;">
PC2
</th>
<th style="text-align:right;">
PC3
</th>
<th style="text-align:right;">
PC4
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
sample-8
</td>
<td style="text-align:right;">
-23.69999
</td>
<td style="text-align:right;">
28.76602
</td>
<td style="text-align:right;">
34.2002153
</td>
<td style="text-align:right;">
-35.60371
</td>
</tr>
<tr>
<td style="text-align:left;">
sample-15
</td>
<td style="text-align:right;">
30.15120
</td>
<td style="text-align:right;">
34.22453
</td>
<td style="text-align:right;">
0.4203505
</td>
<td style="text-align:right;">
-35.85475
</td>
</tr>
<tr>
<td style="text-align:left;">
sample-1
</td>
<td style="text-align:right;">
-26.72959
</td>
<td style="text-align:right;">
25.49802
</td>
<td style="text-align:right;">
-30.4204765
</td>
<td style="text-align:right;">
-26.95519
</td>
</tr>
<tr>
<td style="text-align:left;">
sample-5
</td>
<td style="text-align:right;">
-32.12815
</td>
<td style="text-align:right;">
26.56765
</td>
<td style="text-align:right;">
10.9217774
</td>
<td style="text-align:right;">
-27.95586
</td>
</tr>
<tr>
<td style="text-align:left;">
sample-11
</td>
<td style="text-align:right;">
26.06050
</td>
<td style="text-align:right;">
26.18924
</td>
<td style="text-align:right;">
-27.9793109
</td>
<td style="text-align:right;">
-27.12734
</td>
</tr>
<tr>
<td style="text-align:left;">
sample-2
</td>
<td style="text-align:right;">
-34.77699
</td>
<td style="text-align:right;">
30.90710
</td>
<td style="text-align:right;">
-22.9081639
</td>
<td style="text-align:right;">
-19.89748
</td>
</tr>
</tbody>
</table>

#### Sample File

The sample file must contain samples ids in the first column that match
the first column of the PCA data file. All other columns are optional.

<table class="table table-light">
<thead>
<tr>
<th style="text-align:left;">
sample name
</th>
<th style="text-align:left;">
clutch
</th>
<th style="text-align:left;">
genotype
</th>
<th style="text-align:right;">
somiteNumber
</th>
<th style="text-align:left;">
stage
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
sample-8
</td>
<td style="text-align:left;">
clutch1
</td>
<td style="text-align:left;">
hom
</td>
<td style="text-align:right;">
17
</td>
<td style="text-align:left;">
17-somites
</td>
</tr>
<tr>
<td style="text-align:left;">
sample-15
</td>
<td style="text-align:left;">
clutch2
</td>
<td style="text-align:left;">
het
</td>
<td style="text-align:right;">
17
</td>
<td style="text-align:left;">
17-somites
</td>
</tr>
<tr>
<td style="text-align:left;">
sample-1
</td>
<td style="text-align:left;">
clutch1
</td>
<td style="text-align:left;">
wt
</td>
<td style="text-align:right;">
18
</td>
<td style="text-align:left;">
18-somites
</td>
</tr>
<tr>
<td style="text-align:left;">
sample-5
</td>
<td style="text-align:left;">
clutch1
</td>
<td style="text-align:left;">
het
</td>
<td style="text-align:right;">
18
</td>
<td style="text-align:left;">
18-somites
</td>
</tr>
<tr>
<td style="text-align:left;">
sample-11
</td>
<td style="text-align:left;">
clutch2
</td>
<td style="text-align:left;">
wt
</td>
<td style="text-align:right;">
18
</td>
<td style="text-align:left;">
18-somites
</td>
</tr>
<tr>
<td style="text-align:left;">
sample-2
</td>
<td style="text-align:left;">
clutch1
</td>
<td style="text-align:left;">
wt
</td>
<td style="text-align:right;">
19
</td>
<td style="text-align:left;">
19-somites
</td>
</tr>
</tbody>
</table>

#### Test Data

There is also an option to load some pre-prepared test data.

### PCA plot

The plot defaults are to plot PC1 against PC2 with the first categorical
variable being used as the fill colour for the points.

#### Options

##### Sample Names

Sample ids are plotted next to each point by default. This can be
altered using this checkbox.

##### Axes

The *X axis component* and *Y axis component* radio buttons can be used
to select which components are plotted against each other.

##### Fill Colour

You can change both the type of variable (Categorical or Continuous) and
which variable is mapped to the colour of the points. For Categorical
variables, a colour-blind friendly palette is used if there are 8 or
less levels. Otherwise, it attempts to select colours that are as
different as possible. The actual levels displayed can be altered using
the “Fill levels” checkbox group.

For Continuous variables, the
[viridis](https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html)
palette is used unless the scale has both positive and negative values,
in which case a diverging red-white-blue palette is used.

##### Shape

Only Categorical variables can be mapped to shape and only if there are
7 different levels or less. The default is not to map shape to any
variable. All points will appear as circles. The actual levels displayed
can be altered using the “Shape levels” checkbox group.

##### Point Size

This slider can be used to alter the size of the points.

##### Set Limits

The limits of the X and Y scale can be set manually. The ‘Apply’ button
needs to be clicked before any changes take effect. The ‘Reset’ button
resets the limits so that all the points are displayed.

#### Downloads

The **Download plot** button downloads the current plot as either a pdf,
png, eps or svg.

The **Download all (pdf)** button downloads a pdf containing each
component plotted against the next one.

The **Download rda file of plot** button downloads an rda file of the
current plot to allow further customisation in R. The plot is saved as a
ggplot2 object called ‘pca\_plot’.
