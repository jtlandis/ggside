suppressMessages({
  library(dplyr, quietly = T)
  library(tidyr, quietly = T)
  library(vdiffr)
  library(ggdendro, quietly = T)
})

df0 <- mutate(diamonds,
              colclar = interaction(color, clarity,
                                    sep = "_", drop = T))
df1 <- df0 %>%
  group_by(color, clarity, colclar, cut) %>%
  summarise(m_price = mean(price))
df <- df1 %>%
  pivot_wider(id_cols = colclar,
              names_from = cut,
              values_from = m_price,
              values_fill = 0L)

mat <- as.matrix(df[,2:6])
rownames(mat) <- df[["colclar"]]
dst <- dist(mat)
hc_x <- hclust(dst)
lvls <- rownames(mat)[hc_x$order]

df1[["colclar"]] <- factor(df1[["colclar"]], levels = lvls)
dst <- dist(t(mat))
hc_y <- hclust(dst)
lvls <- colnames(mat)[hc_y$order]
df1[["cut"]] <- factor(df1[["cut"]], levels = lvls)
dendrox <- dendro_data(hc_x)
dendroy <- dendro_data(hc_y)

p <- ggplot(df1, aes(x = colclar, cut)) +
  geom_tile(aes(fill = m_price)) +
  viridis::scale_fill_viridis(option = "magma") +
  theme(axis.text.x = element_text(angle = 90, vjust = .5))

test_that("Testing Heatmap Base is consistent",{
  expect_doppelganger("Heatmap Base",p)
})

# test_that("geom_sidesegment",{
#     p0 <- p +
#     geom_xsidetile(aes(y = "Color", xfill = color)) +
#     geom_xsidetile(aes(y = "Clarity", xfill = clarity)) +
#     geom_xsidesegment(data = dendrox$segments,
#                       aes(x = x, y = y, xend = xend, yend = yend),
#                       position = position_yrescale(midpoint = 4.5, range = 4, location = "top")) +
#     geom_ysidesegment(data = dendroy$segments,
#                       aes(y = x, yend = xend, x = y, xend = yend)) +
#     theme_minimal() +
#     theme(axis.text.x=element_blank(), panel.grid = element_blank()) +
#     scale_y_discrete(expand = expansion()) +
#     scale_xsidey_discrete(expand = expansion()) +
#     guides(xfill = "none")
#   expect_doppelganger(title = "xySideSegments & xSideTile", p0)
# })

test_that("geom_sideboxplot", {
  p0 <- p +
    geom_xsideboxplot(data = df0, aes(y = price)) +
    geom_ysideboxplot(data = df0, aes(x = price)) +
    theme(
      ggside.panel.scale = .2
    )
  expect_doppelganger(title = "xySideBoxplots", p0)
})

