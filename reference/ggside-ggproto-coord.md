# Coord Compatible with ggside

S3 class that converts old Coord into one that is compatible with
ggside. Can also update ggside on the object. Typically, the new ggproto
will inherit from the object being replaced.

## Usage

``` r
ggside_coord(coord)

# Default S3 method
ggside_coord(coord)

# S3 method for class 'CoordCartesian'
ggside_coord(coord)

# S3 method for class 'CoordSide'
ggside_coord(coord)

# S3 method for class 'CoordTrans'
ggside_coord(coord)

# S3 method for class 'CoordFixed'
ggside_coord(coord)
```

## Arguments

- coord:

  coord ggproto Object to replace
