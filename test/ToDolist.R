#To do list

# X/Yside* geoms - ideally create support for x/yfill or x/ycolour.
#     Remember these return structure(list(layer = layer()), class = "ggside")
# Create a sub_facet function that will handle technicalitlies such
#     as side panel placement and specifying additional geoms to be
#     placed on the sides.
# Ensure regardless of panel placement panels are numbered correctly and possess correct scales. +++
# Create S3 class for sideFacets when it inherits from NULL, FacetWrap, or FacetGrid.
#     S3 should exist for compute_layout, map_data, and draw_panels
#
# Stop make_sideFacets from making empty panels... A bit unintuative.
# REDUCE THE FUNCTIONS IMPORTED.
# A lot of what is taken is just to get ggplot2 running correctly. Not
# everything is needed. I should spend a day and take things out till only the bare bones are present.

# Create a Collapse argument that collapse side Panels per row or column to one side.

#if x is top, then x is odd rows and m is even rows.
#if y is right, then y is even columns and m is odd columns
#if x is bottom, then x is even rows and m is odd rows
#if y is left, then y is odd columns and m is even columns
#   1 2 3
#1  x
#2  m y
#3

#   1 2 3
#1  m y
#2  x
#3

#   1 2 3
#1  y m
#2    x
#3

#   1 2 3
#1    x
#2  y m
#3
