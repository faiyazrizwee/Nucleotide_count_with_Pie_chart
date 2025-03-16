atgc_count <- function(seq) {
  seq <- toupper(seq)
  countA <- sum(strsplit(seq, "")[[1]] == 'A')
  countT <- sum(strsplit(seq, "")[[1]] == 'T')
  countG <- sum(strsplit(seq, "")[[1]] == 'G')
  countC <- sum(strsplit(seq, "")[[1]] == 'C')
  
  v <- c(countA, countT, countG, countC)
  cat("Count A = ",countA, "\n")
  cat("Count T = ", countT, "\n")
  cat("Count G = ", countG, "\n")
  cat("Count C = ", countC, "\n")
  total <- nchar(seq)
  cat("Total = ", total, "\n")
  cat("GC percent = ", ((countG + countC) / total) * 100, "%" )
  labels <- c("A", "T", "G", "C")
  
  per <- round(v / sum(v) * 100)
  
  labels_per <- paste(labels, per, "%")
  
  pie(v, labels = labels_per, col = c("red", "green", "blue", "yellow"))
}

# seq <- readline(prompt = "Enter a sequence : ")
atgc_count('atgccgatgatcgctaggagctcgctaggctgacgcgctag')