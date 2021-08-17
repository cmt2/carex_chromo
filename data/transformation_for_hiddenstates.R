### Transformation of the data to expand to test hidden states model for chromosome number

carex_data<-read.table("data/carex_chromosome_counts_withoutSiderosticta.tsv")
maximum<-62+10 # maximum number of chromosomes is 62 and we go with 10 more (Mayrose et al. 2010 recommendation)
second_column<-as.numeric(carex_data[,2]) 
expand_data<- second_column+maximum+1 # Expansion (leave room for '0' in hidden state )

#Creating the double entry in each datum
new_column<- paste0("(", second_column, " ",expand_data,")")
names(new_column)<-carex_data[,1]
carex_new_data<-data.frame(new_column)


### Writing the new table
write.table(carex_new_data, 
            file="data/carex_chromosome_counts_expanded_withoutSiderosticta.tsv", 
            sep="\t",
            col.names = FALSE)
