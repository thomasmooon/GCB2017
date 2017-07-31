source("https://bioconductor.org/biocLite.R")

biocLite("GO.db")
# biocLite("KEGG.db")
biocLite("KEGGREST")

library(GO.db)
# library(KEGG.db)
library(reactome.db)

GO.db:::GO_dbInfo()
KEGGREST::listDatabases()
KEGGREST::keggList("organism")
KEGGREST::keggInfo("pathway")
KEGGREST::keggList("pathway")

