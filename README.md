![alt text](https://github.com/clay-carey/DimPals/blob/main/repo_images/badge.png)
# DimPals

DimPals is a tool for interactively creating pleasing and informative color palettes for single-cell UMAP plots generated with the Seurat package.

## Requirements

Currently DimPals is only compatible with Seurat Objects that have a pre-computed UMAP.

The object must have a metadata column called "seurat_clusters"

DimPals supports applying palettes to subsets of clusters. These groups should be defined with a grouping column in the metadata. 


## Installation
```R

devtools::install_github("clay-carey/DimPals")

```

## Usage 
```R

library(DimPals)

DimPals(seurat_object)

```

## Using the application



