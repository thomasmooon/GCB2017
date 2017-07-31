# get ENTREZ2KEGG matching table
columns(org.Hs.eg.db)
keys        <- keys(org.Hs.eg.db, keytype = "ENTREZID")
entrez2path <- as_tibble(AnnotationDbi::select(org.Hs.eg.db, keys = keys, keytype="ENTREZID" ,columns=c("ENTREZID","PATH")))
entrez2path <- entrez2kegg[complete.cases(entrez2kegg),] %>% distinct()


# keggHyperGTest
genes <- DRD.W3.3 %>% filter(TTDDRUGID == "DAP000725") %>% dplyr::select(ENTREZID) %>% distinct() %>% unlist() %>% as.character()

KeggEnrichment <- function(genes, entrez2path, .pvalueCutoff = 0.05, .fdrValueCutoff = 0.05) {
pathsInGenset <- entrez2path %>% filter(ENTREZID %in% genes) %>% dplyr::select(PATH) %>% distinct()
genesPerPath  <- entrez2path %>% filter(PATH %in% pathsInGenset$PATH) %>% dplyr::select(PATH, ENTREZID) %>% distinct() %>% group_by(PATH) %>% tally() 
N             <- as.numeric(n_distinct(entrez2path$ENTREZID)) # background for hyperG-Test

result <- c()
for(path in unique(pathsInGenset$PATH)) {
  white.all     <- entrez2path %>% filter(PATH == path) %>% dplyr::select(ENTREZID) %>% distinct() %>% unlist() %>%  as.character()
  size   <- length(white.all)
  count  <- sum(genes %in% white.all)
  pval   <- phyper(count - 1e-3, size, N-size, length(genes), lower.tail = FALSE)
  result <- rbind(result, data.frame(path, count, size, pval))
}
fdr    <- fdrtool::fdrtool(x = result$pval, statistic = "pvalue", plot = FALSE, verbose = FALSE)$qval
result <- cbind(result,fdr) %>% filter(pval <= .pvalueCutoff, fdr <= .fdrValueCutoff)
return(result)
}

KeggEnrichment(genes,entrez2path)
