#To do list

# X/Yside* geoms - ideally create support for x/yfill or x/ycolour.
#     Remember these return structure(list(layer = layer()), class = "ggside")
# Create a ggside function that will handle technicalitlies such
#     as side panel placement and specifying additional geoms to be
#     placed on the sides.
#
# insure coordflip works correctly... should it change placement of xsidepanels and ysidepanels????
#   This may be the next big step
#   Coord_cartesian should only affect main panel and assocated side panel's shared scales. e.i
#   setting ylim on a plot with a geom_xsidebar should only set limits on main's y scale (and y bar's y scale because
#   they are shared), but not on xbar's y scale.
# maybe Coord* will need to be inhereted much like how Facet* was inhereted withing ggplot_build.ggside

# REDUCE THE FUNCTIONS IMPORTED.
# A lot of what is taken is just to get ggplot2 running correctly. Not
# everything is needed. I should spend a day and take things out till only the bare bones are present.
