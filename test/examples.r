library(stringr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggsidebar)

i2 <- iris %>%
  mutate(Species2 = rep(c("A","B"), 75))
p <- ggplot(i2, aes(Sepal.Width, Sepal.Length, color = Species)) +
  geom_point()

mtcars1 <- as_tibble(mtcars, rownames = "Cars") %>%
  gather(key = "qualities", value = "value", -Cars) %>%
  group_by(qualities) %>%
  mutate(scaledValue = scale(value)) %>%
  ungroup() %>%
  mutate(carCase = case_when(str_detect(Cars, "^[A-M]") ~ "A-M",
                             TRUE ~ "N-Z"),
         qualityCase = case_when(str_detect(qualities, "^[a-m]") ~ "a-m",
                                 TRUE ~ "n-z"),
         endCase = case_when(str_detect(Cars, regex("[a-m]$", ignore_case = T)) ~ "[a-m]$",
                             str_detect(Cars, regex("[n-z]$", ignore_case = T)) ~ "[n-z]$",
                             TRUE ~ "[0-9]$")) %>%
  group_by(Cars) %>%
  mutate(mean_scale=mean(scaledValue)) %>% ungroup() %>%
  group_by(qualities) %>%
  mutate(mean_qual=mean(value)) %>% ungroup()

mtcars1$testPositions <- rep(c("left","right"),176)
#Test1 mainplot tile fill - continuous on x, discrete on y
ggplot(mtcars1, aes(Cars, qualities)) +
  geom_tile(aes(fill = scaledValue)) +
  geom_line(aes(y = scaledValue, color = qualities, group = qualities),
            position = "rescale") +
  geom_point(aes(y = scaledValue, color = qualities, group = qualities),
             position = position_rescale(instance = 1)) +
  geom_boxplot(aes(x = scaledValue, color = qualities),
               position = position_rescale(rescale = "x")) +
  facet_grid(rows = vars(qualityCase), cols = vars(carCase), scales = "free",space = "free") +
  theme(axis.text.x = element_text(angle = 90))

ggplot(mtcars1, aes(Cars, qualities)) +
  geom_tile(aes(fill = scaledValue)) +
  geom_boxplot(aes(y = scaledValue, color = Cars), position = "rescale") +
  geom_boxplot(aes(x = scaledValue, color = qualities),
               position = position_rescale(rescale = "x")) +
  facet_grid(rows = vars(qualityCase), cols = vars(carCase), scales = "free",space = "free") +
  theme(axis.text.x = element_text(angle = 90)) +
  guides(color = FALSE)



#Can apply xfill and yfill - each with its own legend. discrete scale is buggy



#Test2 continuous gradient on x and y axis
ggplot(mtcars1, aes(Cars, qualities)) +
  geom_tile(aes(fill = scaledValue )) +
  geom_ysidebar(aes(yfill = mean_qual)) +
  geom_xsidebar(aes(xfill = mean_scale, height = mean_scale)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5)) +
  scale_yfill_gradient(low = "red", high = "gold")
#Each xfill and yfill get's its own legend.

#test3 - discrete on x and y
ggplot(mtcars1, aes(Cars, qualities)) +
  geom_tile(aes(fill = scaledValue)) +
  geom_ysidebar(aes(yfill = qualityCase), location = "right", alpha = .5) +
  geom_xsidebar(aes(xfill = carCase), location = "bottom") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))

ggplot(mtcars1, aes(Cars, qualities)) +
  geom_tile(aes(fill = scaledValue)) +
  geom_xsidebar(aes(xfill = mean_scale)) +
  scale_xfill_continuous(low = "#FF0000", high = "#0000FF")

ggplot(mtcars1, aes(Cars, qualities)) +
  geom_tile(aes(fill = scaledValue)) +
  geom_xsidebar(aes(xfill = stat(summarise),
                    domain = scaledValue, group = Cars), stat = "summarise", alpha = .7) +
  scale_xfill_continuous(low = "#FF0000", high = "#0000FF")

#Test4 adding additional scales on same xsidebar
ggplot(mtcars1, aes(Cars, scaledValue)) +
  geom_xsidebar(aes(xfill = mean_scale), location = "bottom") +
  geom_point(aes(color = qualities)) +
  #geom_ysidebar(aes(yfill = qualityCase),location="right", alpha = .5) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))

ggplot(mtcars1, aes(Cars, scaledValue)) +
  geom_xsidebar(aes(xfill = carCase, height = .5), alpha = .3, stat = "identity") +
  geom_point(aes(color = qualities)) +
  scale_y_continuous(expand = expansion(c(0,.05)))
  #geom_ysidebar(aes(yfill = qualityCase),location="right", alpha = .5) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))


ggplot(mtcars1, aes(Cars)) +
  #geom_tile(aes(fill = scaledValue)) +
  geom_xsidebar(aes(xfill = carCase)) +
  geom_xsidebar(aes(xfill = endCase))

scale_xfill_gradient <- function(...) scale_color_gradient(..., aesthetics = "xfill")

mtcars2 <- gather(mtcars1, key = "metaKey", value = "metaValue", carCase, endCase)
ggplot(mtcars1, aes(Cars, qualities)) +
  geom_tile(aes(fill = scaledValue)) +
  geom_xsidebar(aes(xfill = carCase)) +
  geom_xsidebar(aes(xfill = endCase))


p <- ggplot(iris, aes(Sepal.Width, Sepal.Length)) +
  geom_point(aes(color = Species))

test <- function(){
  browser()
  ggplot_build(g)
}
test()
