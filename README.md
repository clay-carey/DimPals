![alt text](https://github.com/clay-carey/DimPals/blob/main/repo_images/badge.png)
# DimPals

DimPals is a tool for interactively creating pleasing and informative color palettes for single-cell UMAP plots generated with the Seurat package.

## Requirements

DimPals is compatible with Seurat Objects that have:
- A pre-computed UMAP reduction
- At least one metadata column for grouping/coloring cells

## Installation
```R
devtools::install_github("clay-carey/DimPals")
```

## Usage
```R
library(DimPals)

DimPals(seurat_object)
```

## Features

### 1. Flexible Metadata Coloring
Color your UMAP by **any metadata column**, not just seurat_clusters! Select from any categorical variable in your Seurat object's metadata.

### 2. Individual Group Coloring
Manually assign custom colors to individual groups using an interactive color picker.

### 3. Apply Palettes to Groups
Apply professional color palettes to subsets of your data:
- Select a grouping column (e.g., cell type, sample, condition)
- Choose which group to color
- Pick from hundreds of palettes (Discrete, Continuous, or Dynamic)
- Apply coordinated color schemes to related groups

### 4. Save Palettes to R Environment
Save your perfected color palette directly to your R environment with a custom variable name for easy reuse in your analysis scripts.

### 5. Export Ready Code
Copy-paste ready R code is generated automatically for use in Seurat's `cols` argument.

## Using the Application

1. **Select Color By Column**: Choose which metadata column to use for coloring the UMAP
2. **Customize Colors**: Either manually color individual groups OR apply palettes to groups
3. **Preview**: Refresh the plot to see your changes
4. **Save**: Save the palette to your R environment or copy the generated code



