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

### 2. Live Palette Preview
See your current color palette displayed as visual swatches with group labels. No more guessing what colors you've assigned!

### 3. Shuffle Colors
Instantly randomize the order of colors in your palette with a single click. Great for quickly exploring different color assignments.

### 4. Individual Group Coloring
Manually assign custom colors to individual groups using an interactive color picker.

### 5. Apply Palettes to Groups
Apply professional color palettes to subsets of your data:
- Select a grouping column (e.g., cell type, sample, condition)
- Choose which group to color
- Pick from hundreds of palettes (Discrete, Continuous, or Dynamic)
- Apply coordinated color schemes to related groups

### 6. Customizable Plot Display
Full control over plot appearance:
- Toggle labels on/off
- Toggle legend visibility (on by default)
- Toggle axes visibility (off by default)

### 7. Export High-Resolution Plots
Download publication-ready plots directly from the app:
- PNG format with customizable dimensions
- Adjustable width, height, and DPI
- Plots exported with your current color palette and display settings

### 8. Save Palettes to R Environment
Save your perfected color palette directly to your R environment with a custom variable name for easy reuse in your analysis scripts.

### 9. Export Ready Code
Copy-paste ready R code is generated automatically for use in Seurat's `cols` argument.

## Using the Application

1. **Select Color By Column**: Choose which metadata column to use for coloring the UMAP
2. **Preview Your Palette**: View color swatches showing current assignments
3. **Customize Colors**:
   - Manually color individual groups with the color picker
   - Apply palettes to groups of related items
   - Shuffle colors for quick experimentation
4. **Adjust Display**: Toggle labels, legend, and axes to your preference
5. **Export**: Download high-resolution plots or save the palette to your R environment



