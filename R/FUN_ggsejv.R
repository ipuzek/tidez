ggsejv <- function(..., AA) {
    switch(AA,
           A4.l = ggsave(..., width = 11.69, height = 8.27, units = "in"),
           A4.p = ggsave(..., width = 8.27, height = 11.69, units = "in"),
           A5.l = ggsave(..., width = 8.27, height = 5.83, units = "in"),
           A5.p = ggsave(..., width = 5.83, height = 8.27, units = "in"),
           A6.l = ggsave(..., width = 5.83, height = 4.13, units = "in"),
           A6.p = ggsave(..., width = 4.13, height = 5.83, units = "in"))
}

# EXMAPLE
# ggsejv("meh.svg", AA = "A4.l")

