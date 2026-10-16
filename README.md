# HMLET

High-resolution Machine Learning Eye-Tracking Toolbox (version 2.0).

HMLET is an R package that turns long-format gaze samples into time-resolved condition comparisons and tests those differences with cluster-based permutation tests. An optional MATLAB viewer can display a CSV exported from R. The analysis itself runs in R on a normal desktop computer. It does not require an eye tracker, a GPU, or any other non-standard hardware.

Read more: Kazemi, A., Geng, J., & Ghetti, S. (2024, July 2). HMLET – High-resolution Machine Learning Eye-tracking Toolbox. https://doi.org/10.31234/osf.io/f3xey

## 1. System requirements

### Operating system

Windows, macOS, or Linux. The R package is plain R and does not ship operating-system-specific binaries. A C++ toolchain is required only while installing `rray` from source (see below).

### R

- **R 4.1 or later** is the practical requirement. `DESCRIPTION` does not set `Depends: R (>= ...)`.
- **dplyr >= 1.1.0**, because data checks call `dplyr::reframe()`.
- **tidyr >= 1.2.0**, because sampling repair calls `tidyr::fill(..., .direction = "downup")`.

Base packages `stats` and `utils` are part of R.

### R packages

Install these before HMLET. Versions below are minimums required by the code or by `DESCRIPTION`. Packages without a minimum accept the current CRAN release.

| Package | Source | Version |
| --- | --- | --- |
| combinat | CRAN | current |
| dplyr | CRAN | >= 1.1.0 |
| ggplot2 | CRAN | current |
| miceadds | CRAN | current |
| purrr | CRAN | current |
| reshape2 | CRAN | current |
| tidyr | CRAN | >= 1.2.0 |
| vdiffr | CRAN | current (imported; used by the test snapshots) |
| zoo | CRAN | current |
| remotes or devtools | CRAN | current (install helper) |
| Xtensor.R | GitHub `xtensor-stack/Xtensor.R` | current `main` |
| rray | GitHub `r-lib/rray` | current `main` (`rray` is not on CRAN) |

Suggested, for the included tests only:

| Package | Source | Version |
| --- | --- | --- |
| testthat | CRAN | >= 3.0.0 (edition 3) |

`vdiffr` pulls in further CRAN packages such as `testthat` and `diffobj` when it is installed.

### Compiler toolchain

`rray` and `Xtensor.R` compile from source:

- **Windows:** [Rtools](https://cran.r-project.org/bin/windows/Rtools/) for your installed R version.
- **macOS:** Xcode Command Line Tools (`xcode-select --install`).
- **Linux:** `g++` and `make` (Debian/Ubuntu: `build-essential`).

### Optional MATLAB viewer

The statistics do not need MATLAB. `MATLAB GUI/+funcs/` is a viewer for `HMLET_DataforGUI.csv`. It uses `readtable`, `uiimport`, and package classdefs. The repository does not pin a MATLAB release. A current MATLAB install that can read CSV files is enough (the class files are marked HMLET 2.0, January 2024).

### Versions the software has been tested on

`DESCRIPTION` records **HMLET 2.0** and documentation generated with **roxygen2 7.3.2**. The regression suite is **testthat edition 3**. Reference demo output in `Demo/Archived/` was produced with `set.seed(5)` and `samples = 500` and is the expected result of the demo below.

This repository does not include a continuous-integration log or an `sessionInfo()` record, so a specific operating-system and R-version matrix is not stored here. After you install, the demo is the check: with that seed and sample count, `res$clusterStat` should match `Demo/Archived/Output_ClusterInfos.csv`.

### Hardware

None beyond a normal desktop or laptop. Gaze data must already be exported to a table. Runtime memory grows with the number of rows in that table. The demo file is about 19,200 rows and is small.

## 2. Installation guide

Install from a clone of this repository. From the repository root in R:

```r
install.packages(c(
  "combinat", "dplyr", "ggplot2", "miceadds", "purrr",
  "reshape2", "tidyr", "vdiffr", "zoo", "remotes"
))

remotes::install_github("xtensor-stack/Xtensor.R")
remotes::install_github("r-lib/rray")
remotes::install_local("RPackage/HMLET")

library(HMLET)
```

The same package can be installed from GitHub without a local clone:

```r
remotes::install_github("xtensor-stack/Xtensor.R")
remotes::install_github("r-lib/rray")
remotes::install_github("Alireza-Kazemi/HMLET/RPackage/HMLET")
```

If installation stops with `dependency 'rray' is not available`, install `Xtensor.R` and `rray` from GitHub first, then install HMLET again.

Confirm the install:

```r
packageVersion("HMLET")
```

That should report `2.0`.

### Typical install time

On a normal desktop, with R and the C++ toolchain already installed and a working internet connection, the CRAN packages plus compiling `Xtensor.R` and `rray` typically take **about 10 minutes** (often 5–15 minutes). Installing Rtools or Xcode for the first time adds extra time before that. Loading `library(HMLET)` after a successful install takes a few seconds.

Developer setup in RStudio (Build, Check, Clean and Install) is described in `RPackage/HMLET/R/README.md`.

## 3. Demo

The demo uses the shipped table `Demo/Archived/HMLET_Testing_Data.csv` (columns `ID`, `trial`, `timePoint`, `condition`, `AOI`). `AOI` is already numeric (0 or 1) for conditions `C1` and `C2`. The script `Demo/Archived/HMLET_TestingRoutine.R` is the same analysis with hard-coded Windows paths; use the script below so the path follows your clone.

Set the working directory to the repository root, then run:

```r
library(HMLET)

dat <- read.csv(file.path("Demo", "Archived", "HMLET_Testing_Data.csv"))

d_timeBin <- CreateTimeBinData_HMLET(
  data = dat,
  timePoint = "timePoint",
  groupingColumns = c("ID", "trial", "condition"),
  FixatedOn = "AOI"
)

datP <- PermutationTestDataPrep_HMLET(
  data = dat,
  ID = "ID",
  trial = "trial",
  timePoint = "timePoint",
  condition = "condition",
  conditionLevels = c("C1", "C2"),
  gazeMeasure = "AOI",
  targetAOI = 1
)

plot(PlotTimeSeries_HMLET(datP))

set.seed(5)
res <- PermutationTest_HMLET(
  datP,
  samples = 500,
  paired = TRUE,
  permuteTrialsWithinSubject = FALSE
)

plot(PlotNullDistribution_HMLET(res))
plot(PlotTimeSeries_HMLET(res))
print(res$clusterStat)
```

`CreateTimeBinData_HMLET` uses its defaults here: bins of 250 time units, samples kept through time 3000, aggregation by mean.

### Expected output

`d_timeBin` has columns `ID`, `trial`, `condition`, `timeBin`, `timeBinIndex`, `AOI_0`, and `AOI_1`. The first row of the reference file `Demo/Archived/Output_CreateTimeBinData.csv` is participant 1, trial 1, condition `C1`, bin `0->250`, with `AOI_0 = 0.24` and `AOI_1 = 0.76`.

`datP` has columns `testName`, `ID`, `trial`, `timePoint`, `condition`, and `AOI`. `testName` is `PermutationTest0` when you do not pass one. Because `targetAOI = 1`, the gaze measure stays 1 when `AOI` is 1 and 0 otherwise.

`res` is a list:

| Element | Contents |
| --- | --- |
| `clusterStat` | One row per cluster: `testName`, `Direction`, `index`, `tStatistic`, `timeStart`, `timeEnd`, `pValue`, `significant` |
| `statNULL` | Null distribution of the cluster statistic |
| `stastatValues` | Observed t at each time point, with positive and negative cluster indexes |
| `samples` | `500` |
| `filteredData` | Rows kept for the paired test |

With `set.seed(5)` and `samples = 500`, `res$clusterStat` matches `Demo/Archived/Output_ClusterInfos.csv`:

| Direction | timeStart | timeEnd | tStatistic | pValue | significant |
| --- | --- | --- | --- | --- | --- |
| Positive | 42 | 51 | 28.70 | 0.004 | `*` |
| Negative | 10 | 11 | -5.15 | 0.512 | |
| Negative | 57 | 59 | -9.92 | 0.218 | |
| Negative | 81 | 81 | -2.40 | 0.872 | |

`*` means p < 0.05. The positive cluster from time index 42 through 51 is the significant result.

The console prints `Running permutation test for: PermutationTest0` and a progress bar while the null distribution is built. `PlotTimeSeries_HMLET` draws gaze proportion over time for `C1` and `C2`, with the significant cluster shaded. `PlotNullDistribution_HMLET` draws the null distribution of the cluster statistic. The same figures are stored as vdiffr snapshots under `RPackage/HMLET/tests/testthat/_snaps/PermutationTest_HMLET/`.

Matching CSV copies of these objects are in `Demo/Archived/`: `Output_CreateTimeBinData.csv`, `Output_PermutationTestDataPrep.csv`, `Output_ClusterInfos.csv`, `NullDistribution.csv`, and `Output_tValueDistribution.csv`.

### Expected run time for the demo

On a normal desktop, this demo (**500** permutations, about **19,200** rows) typically finishes in **about 2–5 minutes**. Binning, data prep, and the two plots are seconds. Almost all of the time is the permutation loop, and it scales roughly with `samples`. The function default is 2000 permutations, which takes about four times as long as this demo.

## 4. Instructions for use

Export the eye tracker to a **long** table: one row per sample, with a participant id, a trial id, a time stamp, and gaze coordinates. Column names are arguments, so they do not have to match the names below. Time stamps within each participant and trial must be unique. Sampling intervals should be constant; if they are not, the fix-sampling step inserts gaps and downsamples to the largest interval.

Replace the column-name strings with yours. `samplingInterval` is in the same units as `timeStamp` (often milliseconds).

```r
library(HMLET)

dat <- read.csv("your_gaze.csv")

# Missing-data report. Does not modify dat.
# Columns: ID, nTrials, nTarialsMoreThanCap, averageMissRate.
miss <- Preprocess_CheckData_HMLET(
  data = dat,
  ID = "ID",
  trial = "trial",
  timePoint = "timeStamp",
  GazeX = "GazeX",
  GazeY = "GazeY",
  missRateCap = 50
)

dat <- Preprocess_FixSampling_HMLET(
  data = dat,
  ID = "ID",
  trial = "trial",
  timePoint = "timeStamp",
  GazeX = "GazeX",
  GazeY = "GazeY"
)

# New columns: time, sampleIdx, interval.
# Interpolated columns: GazeX_Interpolated, GazeY_Interpolated, Interpolated.
dat <- Preprocess_Interpolate_HMLET(
  data = dat,
  ID = "ID",
  trial = "trial",
  timePoint = "time",
  GazeX = "GazeX",
  GazeY = "GazeY",
  maxNAToFill = Inf
)
```

`Preprocess_CheckData_HMLET` stops if time stamps are duplicated, and it warns when the sampling interval is inconsistent or a trial has fewer than `shortTrialsThreshold` samples (default 2). Trials with a missing-gaze rate at or above `missRateCap` (default 50 percent) are counted in `nTarialsMoreThanCap`.

### Area-of-interest labels

If the table does not already name the looked-at region, label each sample from AOI centers and sizes. Width and height are full widths for rectangles, and horizontal and vertical diameters for ellipses. Each AOI entry may be one number for every row, or the name of a column that stores that number. Gaze outside every AOI is labeled `Content`. The new column is `AOIs`.

```r
dat <- Preprocess_AOIExtraction_HMLET(
  data = dat,
  GazeX = "GazeX_Interpolated",
  GazeY = "GazeY_Interpolated",
  AOI_Names = c("Target", "Lure"),
  AOIs_Center_X = c(400, 800),
  AOIs_Center_Y = c(500, 500),
  AOIs_Widths = c(200, 200),
  AOIs_Heights = c(200, 200),
  ContentLabel = "Content",
  EllipticAOI = FALSE
)
```

A worked description of the geometry is in `Demo/AOIExtraction/readme.md`.

The current function body looks up `AOIs_Radius_X` and `AOIs_Radius_Y`, which are not arguments of `Preprocess_AOIExtraction_HMLET` (the arguments are `AOIs_Widths` and `AOIs_Heights`). Calling it as documented above stops with an object-not-found error. Skip this step when the looked-at region is already a column, which is how the demo table is set up (`AOI`).

### Time bins and the gaze measure

Collapse samples into bins, then build a **numeric** gaze measure. A common measure is the proportion of looks to a target AOI versus a comparison AOI. `CreateTimeBinData_HMLET` adds one column per AOI name, prefixed with `AOI_`.

```r
binned <- CreateTimeBinData_HMLET(
  data = dat,
  groupingColumns = c("ID", "trial", "condition"),
  timeBinWidth = 250,
  timeMax = 3000,
  FixatedOn = "AOIs",
  timePoint = "time"
)

binned$gazeProp <- binned$AOI_Target /
  (binned$AOI_Target + binned$AOI_Lure)

prepared <- PermutationTestDataPrep_HMLET(
  data = binned,
  ID = "ID",
  trial = "trial",
  timePoint = "timeBinIndex",
  timeBinName = "timeBin",
  condition = "condition",
  conditionLevels = c("Cond1", "Cond2"),
  gazeMeasure = "gazeProp",
  testName = "testName"
)
```

`gazeMeasure` must be numeric. If you pass `targetAOI`, values equal to that label become 1 and every other value becomes 0. Pass `conditionLevels` in the order you want the conditions compared.

For a single contrast, `testName` can be a constant column. To run several contrasts in one call, stack them and give each block its own `testName`. The permutation routine loops over `testName`.

### Permutation test

```r
set.seed(1)
res <- PermutationTest_HMLET(
  data = prepared,
  samples = 2000,
  paired = TRUE,
  permuteTrialsWithinSubject = FALSE
)

res$clusterStat
plot(PlotTimeSeries_HMLET(res))
plot(PlotNullDistribution_HMLET(res))
```

What the arguments do:

- `samples`: size of the null distribution. The default is 2000. The demo uses 500 so it finishes faster. Cluster p-values are the share of null statistics at least as large as the observed cluster sum of t values.
- `paired = TRUE`: paired t-test across conditions, and time points that are missing a condition are dropped. Use `paired = FALSE` for a two-sample t-test. A two-sample test requires one condition per participant.
- `permuteTrialsWithinSubject = FALSE`: shuffle condition labels across participants (subject-level null). `TRUE` shuffles trials within each participant.
- The cluster-forming threshold is the two-sided critical t at alpha = 0.05, with degrees of freedom equal to the number of participants minus 1, unless you pass `threshold_t`.

`significant` is `*` when p < 0.05 and `.` when p < 0.1. `PlotTimeSeries_HMLET(res)` shades significant clusters. Pass `onlySignificantClusters = FALSE` to shade every cluster. `ClusterStats_HMLET(prepared, paired = TRUE)` returns the observed clusters without a permutation p-value, which is useful as a quick look before the full test.

Related entry points:

- `PermutationTestSingleParticipant_HMLET` runs the trial-level test for one participant.
- `SubjectLevelPermutationTestWithin_HMLET` and `TrialLevelPermutationTestWithin_HMLET` return only the null distribution.
- `AddClusterInfotoData_HMLET` joins cluster rows back onto the time series.
- `RelativeViewingProportionLabels_HMLET` labels trials whose looking time is skewed toward one of two AOIs.
- `GenerateRandomData_HMLET` simulates a within-participant effect for package checks. It is not a model of real gaze data.
- `ExportDataForGUI_HMLET` writes `HMLET_DataforGUI.csv` for the MATLAB viewer.

Help for any exported function:

```r
?PermutationTest_HMLET
```
# R Scripts used in the manuscript
 Behavioral Analysis.R is used for generating behavioral descriptives and statistical tests reported in the manuscript and supplementary information.
 MultilevelModels.R is used for performing longitudinal tests using multilevel models and generating graphs reported in the manuscript and supplementary information.