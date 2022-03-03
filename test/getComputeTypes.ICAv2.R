url = "https://illumina.gitbook.io/ica/project/p-flow/f-pipelines#compute-types"
#library(XML)
#doc <- htmlParse(url)

library(rvest)
html = read_html(url)
html_div_nodes = html %>% html_elements("div")
nodes_that_have_table_data = html %>% html_elements("div") %>% html_attr("data-rnw-int-class")

#html_attrs(html_div_nodes[!is.na(nodes_that_have_table_data)][1])

nodes_to_check = (1:length(html_div_nodes))[nodes_that_have_table_data == "table-row____"]
nodes_to_check = nodes_to_check[!is.na(nodes_to_check)]

computeTypes = FALSE
lines_to_keep = c()
for(i in 1:length(nodes_to_check)){
  text_of_interest = html_div_nodes[nodes_to_check[i]] %>% html_text2()
  cpuReference = FALSE
  if(grepl("Compute Type",text_of_interest)){
    computeTypes = TRUE
    
  } else if(grepl("cpu",text_of_interest) || grepl("small",text_of_interest) || grepl("medium",text_of_interest) || grepl("large",text_of_interest)){
    cpuReference = TRUE
  } else{
    computeTypes = FALSE
  }
  
  if(computeTypes || cpuReference){
    #print(html_attrs(html_div_nodes[nodes_to_check[i]]))
    #print(html_div_nodes[nodes_to_check[i]] %>% html_text2())
    lines_to_keep = c(lines_to_keep,text_of_interest)
  }
}

header_line = c()
content_lines = c()
for(line in 1:length(lines_to_keep)){
  if(line == 1){
    header_line = strsplit(lines_to_keep[line],"\n")[[1]]
  } else{
    content_lines = rbind(content_lines,strsplit(lines_to_keep[line],"\n")[[1]])
  }
}
colnames(content_lines) = header_line
rownames(content_lines) = NULL
print(content_lines)