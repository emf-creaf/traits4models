# Plant trait definitions

A data frame of accepted plant trait definitions. This information
guides the format and content evaluation of harmonized trait datasets.

## Format

A data frame with parameters in rows and columns:

- `Definition`: Trait definition

- `Notation`: Trait notation required for harmonization.

- `Type`: Either 'Numeric' (for quantitative traits), 'Integer' (for
  counts) or 'String' (for qualitative traits).

- `Units`: Required units for quantitative traits.

- `EquivalentUnits`: Alternative, but equivalent, unit notation for
  quantitative traits.

- `AcceptedMethods`: Set of comma-separated accepted methods.

- `DefaultMethod`: Default method to be expected when the information is
  missing.

- `AcceptedValues`: Set of comma-separated accepted values.

- `MinimumValue`: Minimum value for quantitative traits.

- `MaximumValue`: Maximum value for quantitative traits.

## Examples

``` r
data(HarmonizedTraitDefinition)
```
