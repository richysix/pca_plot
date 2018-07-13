# PCA Plot

A shiny app for visualising results from a PCA.

## Instructions

The app is designed to produce 2D scatterplots of the results of a Principal Component Analysis.

### Loading data

Data can be loaded using the buttons at the top of the side bar control panel. Users need to supply a PCA data file along with a sample file.

#### PCA file

The first column is expected to be sample ids and must match the first column of the samples file.
The column names for the principal component data are expected to be of the form PC*number*

e.g.
<table class="table-bordered-centered">
  <tr>
    <th>Sample_name</th>
    <th>PC1</th>
    <th>PC2</th>
    <th>PC3</th>
    <th>PC4</th>
  </tr>
  <tr>
    <td>Gene1_wt1</td>
    <td>-20</td>
    <td>-20</td>
    <td>-20</td>
    <td>35</td>
  </tr>
  <tr>
    <td>Gene1_wt2</td>
    <td>-35</td>
    <td>-25</td>
    <td>-24</td>
    <td>34</td>
  </tr>
  <tr>
    <td>Gene1_wt3</td>
    <td>-23</td>
    <td>-13</td>
    <td>-31</td>
    <td>29</td>
  </tr>
  <tr>
    <td>Gene1_het1</td>
    <td>-5</td>
    <td>-5</td>
    <td>-14</td>
    <td>-24</td>
  </tr>
  <tr>
    <td>Gene1_het2</td>
    <td>4</td>
    <td>5</td>
    <td>-21</td>
    <td>-22</td>
  </tr>
  <tr>
    <td>Gene1_het3</td>
    <td>3</td>
    <td>4</td>
    <td>-18</td>
    <td>-21</td>
  </tr>
  <tr>
    <td>Gene1_hom1</td>
    <td>28</td>
    <td>25</td>
    <td>-14</td>
    <td>-24</td>
  </tr>
  <tr>
    <td>Gene1_hom2</td>
    <td>31</td>
    <td>26</td>
    <td>-21</td>
    <td>-22</td>
  </tr>
  <tr>
    <td>Gene1_hom3</td>
    <td>27</td>
    <td>32</td>
    <td>-18</td>
    <td>-21</td>
  </tr>
</table>

#### Sample File

The sample file must contain samples ids in the first column that match the first column of the PCA data file. All other columns are optional.

<table class="table-bordered-centered">
  <tr>
    <th>Sample_name</th>
    <th>Gene</th>
    <th>Genotype</th>
    <th>Somite_number</th>
  </tr>
  <tr>
    <td>Gene1_wt1</td>
    <td>Gene1</td>
    <td>wt</td>
    <td>25</td>
  </tr>
  <tr>
    <td>Gene1_wt2</td>
    <td>Gene1</td>
    <td>wt</td>
    <td>24</td>
  </tr>
  <tr>
    <td>Gene1_wt3</td>
    <td>Gene1</td>
    <td>wt</td>
    <td>27</td>
  </tr>
  <tr>
    <td>Gene1_het1</td>
    <td>Gene1</td>
    <td>het</td>
    <td>24</td>
  </tr>
  <tr>
    <td>Gene1_het2</td>
    <td>Gene1</td>
    <td>het</td>
    <td>26</td>
  </tr>
  <tr>
    <td>Gene1_het3</td>
    <td>Gene1</td>
    <td>het</td>
    <td>25</td>
  </tr>
  <tr>
    <td>Gene1_hom1</td>
    <td>Gene1</td>
    <td>hom</td>
    <td>16</td>
  </tr>
  <tr>
    <td>Gene1_hom2</td>
    <td>Gene1</td>
    <td>hom</td>
    <td>17</td>
  </tr>
  <tr>
    <td>Gene1_hom3</td>
    <td>Gene1</td>
    <td>hom</td>
    <td>17</td>
  </tr>
</table>

#### Test Data

There is also an option to load some pre-prepared test data.

### PCA plot

The plot defaults are to plot PC1 against PC2 with the first categorical variable being used as the fill colour for the points.

#### Options

##### Sample Names

Sample ids are plotted next to each point by default. This can be altered using this checkbox.

##### Axes

The *X axis component* and *Y axis component* radio buttons can be used to select which components are plotted against each other.

##### Fill Colour

You can change both the type of variable (Categorical or Continuous) and which variable is mapped to the colour of the points.
For Categorical variables, a colour-blind friendly palette is used if there are 8 or less levels.
Otherwise, it attempts to select colours that are as different as possible.
The actual levels displayed can be altered using the "Fill levels" checkbox group.

For Continuous variables, the [viridis](https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html) palette
is used unless the scale has both positive and negative values, in which case a diverging red-white-blue palette is used.

##### Shape

Only Categorical variables can be mapped to shape and only if there are 7 different levels or less.
The default is not to map shape to any variable. All points will appear as circles.
The actual levels displayed can be altered using the "Shape levels" checkbox group.

##### Point Size

This slider can be used to alter the size of the points.

##### Set Limits

The limits of the X and Y scale can be set manually. The 'Apply' button needs to be clicked before any changes take effect.
The 'Reset' button resets the limits so that all the points are displayed.

#### Downloads

The **Download plot** button downloads the current plot as either a pdf, png, eps or svg.

The **Download all (pdf)** button downloads a pdf containing each component plotted against the next one.

The **Download rda file of plot** button downloads an rda file of the current plot to allow further customisation in R.
The plot is saved as a ggplot2 object called 'pca_plot'.

