# Plant trait definitions

A data frame of accepted plant trait definitions.

## Format

A data frame with parameters in rows and columns:

- `Definition`: Trait definition

- `Notation`: Trait notation required for harmonization.

- `Type`: Either 'Numeric' (for quantitative traits), 'Integer' (for
  counts) or 'String' (for qualitative traits).

- `Units`: Required units for quantitative traits.

- `EquivalentUnits`: Alternative, but equivalent, unit notation for
  quantitative traits.

- `AcceptedValues`: Set of comma-separated accepted values.

- `MinimumValue`: Minimum value for quantitative traits.

- `MaximumValue`: Maximum value for quantitative traits.

## Examples

``` r
data(HarmonizedTraitDefinition)
```
