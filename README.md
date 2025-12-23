# BioDataManifest

Tools for generating Biomedical Data Manifest HTML pages from survey results or the web.

## Overview

The Biomedical Data Manifest was designed to simplify the process and burden of documentation for biomedical data generators while emphasizing important goals such as transparency, accuracy and compliance for the dissemination process. This package is meant to be used with a Microsoft Forms (or similar) [survey](https://forms.office.com/Pages/ShareFormPage.aspx?id=V3lz4rj6fk2U9pvWr59xWH1ybZHaUkFGm97Ts1oa_d5UOVM2V1g0M0NOWkoxSDRTWEY5SzNaTExWQS4u&sharetoken=DisvTFgeMAcgV2zYQ6ue) which contains the fields to be filled out by the data generator. An alternate Microsoft Word version of the survey is also provided [here](inst/extdata/Biomedical%20Data%20Manifest_Survey.docx).

See Bottomly et al *In Preparation* for more details.

## Installation

```{r}

#If Bioconductor is not already available
install.packages("BiocManager")

BiocManager::install("GenomicDataCommons")

devtools::install_github("biodev/BioDataManifest")

```

## Usage

### 1) In conjunction with Microsoft Forms Excel output

Example: [Biomedical_Data_Manifest_Template_Example.xlsx](inst/extdata/Biomedical_Data_Manifest_Template_Example.xlsx)

There are two available *personas* which control the fields rendered:

a)  Data Managers/Computationalists: (DM/Comp)
b)  Bench or clinical researchers: (Bench/Clinical)

```{r}

library(BioDataManifest)

manifest_from_excel(
    system.file(
        file.path("extdata", "Biomedical_Data_Manifest_Template_Example.xlsx"), 
        package="BioDataManifest"), 
    persona = "DM/Comp", 
    output.file = "template_example.html"
)

```

The resulting [template_example.html](inst/extdata/template_example.html) HTML file can then be opened in any browser.

### 2) Retrieve information from Genomic Data Commons and dbGaP for specific projects.

Available projects are those from the [Genomic Data Commons](https://portal.gdc.cancer.gov/analysis_page?app=Projects).

NOTE: This functionality is experimental and fairly fragile due to a lot of information not being available from dbGaP or GDC's APIs and instead having to rely on more error-prone parsing of webpages.

```{r}

library(BioDataManifest)

manifest_from_gdc("TARGET-AML")
```

The resulting [TARGET-AML-biomedical_data_manifest.html](inst/extdata/TARGET-AML-biomedical_data_manifest.html) is fairly complete.

Other examples to try:

```{r}

manifest_from_gdc("TCGA-LAML")

```

[TCGA-LAML-biomedical_data_manifest.html](inst/extdata/TCGA-LAML-biomedical_data_manifest.html)

```{r}

manifest_from_gdc("BEATAML1.0-COHORT")

```

[BEATAML1.0-COHORT-biomedical_data_manifest.html](inst/extdata/BEATAML1.0-COHORT-biomedical_data_manifest.html)

## Getting Help

For bugs, new features or questions please open an [Issue](https://github.com/biodev/BioDataManifest/issues).
