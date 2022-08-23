require(visNetwork)
library(ggplot2)
require(cowplot)
signif_results <- read.delim("data/signif_results_m.txt",header = TRUE,sep=",")
signif_results[is.na(signif_results)] = 0
cell_order <- read.delim("data/cell_order.txt",header = TRUE,sep=",")
pheno = "OMIM:131950"
type ="3"
table_cell_do = function(pheno,type =1){
  if(pheno != ""){
  if(type == "1"){
    DF =  signif_results[signif_results$Phenotype == pheno , ]
  return(DF)}
   else if(type == "2"){
    DF =  signif_results[signif_results$DO.Disease == pheno , ]
    return(DF)} else{
    DF =  signif_results[signif_results$OMIM.ID == pheno , ]
    return(DF)
    }
}}
network_cell_do = function(pheno,type=1){
  if(pheno != ""){
    if(type == "1"){
  adjacency = data.frame()
  results = signif_results[signif_results$Phenotype == pheno , ]
  for( c in unique(results[, "CellType"])){
    adjacency = rbind(adjacency,data.frame("from"=pheno,"to"=c))
  }

for( d in unique(results[, "DO.Disease"])){
  adjacency = rbind(adjacency,data.frame("from"=d,"to"=pheno))
}
  
Npheno = length(pheno)
Ncell = length(unique(results[, "CellType"]))
Ndo = length(unique(results[, "DO.Disease"]))

nodes <- data.frame( id=c(pheno,unique(results[, "CellType"]),
                          unique(results[, "DO.Disease"])),
                     label=c(pheno,unique(results[, "CellType"]),
                             unique(results[, "DO.Disease"])),
                     shape=c(rep("box",Npheno),rep("ellipse",Ncell),rep("box",Ndo)),
                     color=c(rep("lightblue",Npheno),rep("orange",Ncell),rep("red",Ndo)))

edges <- data.frame(from = c(adjacency$from),to = c(adjacency$to),arrows = c("to"))
visNetwork(nodes, edges)
    } else{
      if(type == "2"){
      results = signif_results[signif_results$DO.Disease == pheno , ]}
      else{results = signif_results[signif_results$OMIM.ID == pheno , ]}
      adjacency = results[,c("Phenotype","CellType")]
      adjacency = adjacency[!duplicated(adjacency),]
      
      phenotype = unique(results$Phenotype)
      Npheno = length(phenotype)
      cell = unique(results$CellType)
      Ncell = length(cell)
      
      nodes <- data.frame( id=c(phenotype,cell),
                           label=c(phenotype,cell),
                           shape=c(rep("box",Npheno),rep("ellipse",Ncell)),
                           color=c(rep("lightblue",Npheno),rep("orange",Ncell)))
      
      edges <- data.frame(from = c(adjacency$Phenotype),to = c(adjacency$CellType),arrows = c("to"))
      visNetwork(nodes, edges)
    }
  }}

plot_pheno_cell = function(pheno,type=1){
  if(pheno != ""){
    if(type != "1"){
      if(type == "2"){
        results = signif_results[signif_results$DO.Disease == pheno , ]}
      else{results = signif_results[signif_results$OMIM.ID == pheno , ]}
      do_signif_counts = data.frame()
      for (c in unique(results$CellType)) {
        n_signif = length(results[results$CellType == c, ]$q)
        do_signif_counts = rbind(do_signif_counts,
                                 data.frame("Do"=pheno,
                                            "CellType"=c,
                                            "n_signif"=n_signif))
      }

      do_signif_counts$CellType = factor(do_signif_counts$CellType,levels = cell_order$x)
      
      do_plt <- ggplot(do_signif_counts, aes(x=CellType,y=n_signif)) +
        geom_col( fill = "#69b3a2", color = "#e9ecef") +
        geom_text(mapping= aes(label = n_signif, y = n_signif + 1))+
        theme_cowplot()+
        ylab("N phenotypes") +
        scale_y_continuous(expand=c(0,0), limits= c(0,max(do_signif_counts$n_signif)+2))+
        theme(axis.text.x = element_text(angle=90,hjust=1,vjust = 0.3),axis.title.x = element_blank(),legend.position="none",
              plot.title = element_text(hjust = 0.5)) +
        #coord_flip() +
        ggtitle(pheno)
      return (do_plt)
    }
}}

