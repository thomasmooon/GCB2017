#### DISEASE2SYMPTOMS ####
##########################


# http://www.ncbi.nlm.nih.gov/pmc/articles/PMC2850671/pdf/procascamc00002-0740.pdf
# https://www.nlm.nih.gov/research/umls/knowledge_sources/metathesaurus/index.html
# http://davidrothman.net/2006/11/21/mapping-icd-9-to-medlineplus-or-mesh/
# http://www.ncbi.nlm.nih.gov/mesh
# https://www.nlm.nih.gov/pubs/factsheets/mesh.html

# http://www.ontobee.org/
# http://www.disease-ontology.org
# https://github.com/DiseaseOntology/HumanDiseaseOntology/tree/master/src/ontology

DISEASE2SYMPTOMS <- tbl(src, "REFERENCE.DISEASE2SYMPTOMS")
DISEASE2SYMPTOMS
    # !
    # Problem: Viele Elemente aus DISEASE2SYMPTOMS$MESH_DISEASE_TERM werden nicht
    # in der disease ontology https://sourceforge.net/p/diseaseontology/code/HEAD/tree/trunk/HumanDO.obo
    # gefunden! therefore, mapping will have many gaps.


head(dplyr::select(ICD9GROUPING,ICD9_STRING,PHEWAS_STRING) )

filter(DISEASE2SYMPTOMS,MESH_DISEASE_TERM=="Li-Fraumeni Syndrome ")
