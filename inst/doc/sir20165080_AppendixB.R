## ----echo=FALSE----------------------------------------------------------
f <- "Rd2.tex"
p <- ifelse(file.exists(f), f, system.file("misc", f, package="wrv"))
n <- match("\\Rdcontents{\\R{} topics documented:}", readLines(p))
rm.idxs <- -seq_len(n)

