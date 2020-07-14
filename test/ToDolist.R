#To do list

# X/Yside* geoms - ideally create support for x/yfill or x/ycolour.
#     Remember these return structure(list(layer = layer()), class = "ggside")
# Create a sub_facet function that will handle technicalitlies such
#     as side panel placement and specifying additional geoms to be
#     placed on the sides.
# Ensure regardless of panel placement panels are numbered correctly and possess correct scales.
# Create S3 class for sideFacets when it inherits from NULL, FacetWrap, or FacetGrid.
#     S3 should exist for compute_layout, map_data, and draw_panels
#
# REDUCE THE FUNCTIONS IMPORTED.
# A lot of what is taken is just to get ggplot2 running correctly. Not
# everything is needed. I should spend a day and take things out till only the bare bones are present.
