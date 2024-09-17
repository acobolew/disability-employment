---
editor_options: 
  markdown: 
    wrap: 72
---

This repository holds files for downloading, important, and analyzing
data for the disability employment manuscript. The analysis was
conducted in R.

".Rprofile" loads required R packages

"disability-employment.R" downloads census data (including American
Community Survey data), imports and cleans Department of Labor data from
xlsx sheets, conducts analysis, and produces figures

URLs.xlsx lists the URLs where all Department of Labor data were
downloaded. This includes data downloaded directly from the Department
of Labor Wage and Hour Division website (which hosted data in tableau)
and data downloaded from archive.org crawl of the Department of Labor
website (which was reorganized more than once in the past).

"whd_data" folder holds all data from Department of Labor (Wage and Hour
Division). There are four such files (three from archive.org crawls and
one directly from Department of Labor web site):

1.  "whd_data/from archive.org/CRP data on num certs only.xlsx": Data
    downloaded from archive.org crawl of Department of Labor site, with
    data files including only Community Rehabilitation Programs and
    omitting number of people paid subminimum wages. Covers dates
    2010-01-05 through 2015-04-01.

2.  "whd_data/from archive.org/CRP.xlsx": Data downloaded from
    archive.org crawl of Department of Labor site, with data files
    including only Community Rehabilitation Programs. Covers dates
    2015-10-01 through 2019-07-01

3.  "whd_data/from archive.org/Both.xlsx": Data downloaded from
    archive.org crawl of Department of Labor site, with data files
    including all certificate types. Covers dates 2020-01-01 through
    2021-10-01.

4.  "whd_data/14(c) Certificate Holder Archived List_data.xlsx": Data
    downloaded directly from Department of Labor tableau site. Covers
    dates 2021-10-01 through 2024-10-01. Data S5. (“URLs.xlsx”) URLs
    from which Department of Labor data were downloaded.
