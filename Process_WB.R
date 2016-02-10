# Function to extract CE-COICOP concordance from WB .do files
# Function requires file path and country ISO3 code for given input data

processWBscript = function(file, iso3='IND') {
  
  d = readLines(file)
  id = ifelse(iso3=='BRA', 'recode COD_ITEM', 'recode src_cod')
  t1 = which(str_detect(d, fixed(id)))
  t2 = which(str_detect(d, fixed('(icp_seq)')))
  
  recode_lines = lapply(1:length(t1), function(i) {
    x = d[t1[i]:t2[i]]
    x = gsub('\t', '', x, fixed=T)
    x = str_c(x, collapse='')
    return(x)
  })
  
  result = lapply(1:length(recode_lines), function(i) {
    
    x = recode_lines[[i]]
    x = gsub(id, '', x, fixed=T)
    x = str_split(x, ',')[[1]][1]
    x = gsub('[ ]{2,}', ' ', x)
    re <- "\\(([^()]+)\\)"
    x = gsub(re, "\\1", str_extract_all(x, re)[[1]])
    x = str_split(x, '=', n=2)
    
    out = lapply(x, function(z) {
      z = gsub('.', NA, z, fixed=T)
      z = gsub('/', ':', z, fixed=T)
      z = gsub(' ', ',', z, fixed=T)
      code = eval(parse(text=paste0('c(',z[1],')')))
      if (iso3=='BRA') {
        temp = which(str_detect(d, fixed('keep if')))
        grupo_ind = max(temp[temp<t1[i]])
        grupo = str_sub(d[grupo_ind], -2, -1)
        code = as.numeric(paste0(grupo, str_pad(code, width=5, pad=0)))
      }
      icp_seq = as.numeric(z[2])
      data.frame(code, icp_seq)
    })
    
    bind_rows(out)
    
  })
  
  result = bind_rows(result) %>% arrange(code)
  names(result) <- c("CODE", "ICP_SEQ")
  
  if(iso3!='BRA') {
    id1 = ifelse(iso3=='IND', 'label def src_cod', 'la def src_cod')
    t3 = which(str_detect(d, fixed(id1)))+1
    t4 = which(str_detect(d, fixed('la val src_cod src_cod')))-1
    x <- d[t3:t4]
    x <- x[str_detect(x, "[0-9]+ \"")]
    code <- str_extract(x, "[0-9]+ \"")
    code <- gsub(' \"', '', code, fixed=T)
    desc <- str_extract(x, "\".+\"")
    desc <- gsub('\"', '', desc, fixed=T)
    codebook <- data.frame(code, desc) 
    names(codebook) <- c("CODE", "Surv_Heading")  
    result <- unique(merge(result, codebook, by="CODE"))
  }

  return(result)
}

#-------------------

# Process each country...
# Output is data frame providing concordance between survey line item ('code')
# and WB GCD COICOP aggregate category code ('icp_seq')
wb_path = 'H:/MyDocuments/IO work/Bridging/CES-COICOP/Worldbank/'
IDN_WB = processWBscript(paste(wb_path, 'INDONESIA2010.txt', sep=''), iso3='IDN')
BRA_WB = processWBscript(paste(wb_path, 'BRAZIL2008.txt', sep=''), iso3='BRA')
ZAF_WB = processWBscript(paste(wb_path, 'SOUTH-AFRICA2010.txt', sep=''), iso3='ZAF')
IND_WB = processWBscript(paste(wb_path, 'INDIA2011.txt', sep=''), iso3='IND')



