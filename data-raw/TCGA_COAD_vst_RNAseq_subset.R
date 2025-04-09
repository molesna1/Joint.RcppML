## Code to prepare a very small subset of RNA-seq values from TCGA-COAD Pan-Cancer Atlas as an example dataset
library(DESeq2)
row.names(tcga_rna_seq)<- NULL
tcga_rna_seq<- tcga_rna_seq %>% distinct(gene_id, .keep_all = TRUE)%>%column_to_rownames(var="gene_id")

tcga_rna_seq <- round(tcga_rna_seq,0)
tcga_rna_seq<- tcga_rna_seq[ , colSums(is.na(tcga_rna_seq)) == 0]

# Align columns and rownames from input and metadata
tcga_rna_metadata<- tcga_metadata%>%filter(SampleID %in% colnames(tcga_rna_seq))

tcga_rna_seq<- tcga_rna_seq%>%select(any_of(tcga_rna_metadata$SampleID))

des_input <- tcga_rna_seq[, as.vector(tcga_rna_metadata$SampleID)]

dds <- DESeqDataSetFromMatrix(countData = des_input,
                              colData = tcga_rna_metadata,
                              design = ~ SampleType + Location)
dds$SampleType <- relevel(dds$SampleType, ref = "normal")

keep <- rowSums(counts(dds) >= 10) >= 14
dds <- dds[keep,]

# Run DESeq2
dds <- DESeq(dds)

vsd <- assay(vst(dds, blind=FALSE))

vst<- as.data.frame(vsd)

TCGA_COAD_vst_RNAseq<- vst

# Get a very small subset
TCGA_COAD_vst_RNAseq<- TCGA_COAD_vst_RNAseq[{set.seed(15); sample(500)}, {set.seed(15); sample(75)}]

usethis::use_data(TCGA_COAD_vst_RNAseq_subset, overwrite = TRUE)
