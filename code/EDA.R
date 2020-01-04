# encoding : UTF-8
install.packages("DataExplorer")
library(DataExplorer)

plot_histogram()



plot_boxplot()


library(plotly)

p <- plot_ly(
  x = c("giraffes", "orangutans", "monkeys"),
  y = c(20, 14, 23),
  name = "SF Zoo",
  type = "bar"
)

p


















