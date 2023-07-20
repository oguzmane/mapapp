
cat_indexFUN <- function(cat) {
  
  if (cat=="Ethnicity (Raw)") {
    
    return("ethR")
    
  } else if (cat=="Ethnicity (Broad)") {
    
    return("ethB")
    
  } else if (cat=="Gender") {
    
    return("gen")
    
  } else if (cat=="Sex") {
    
    return("sex")
    
  }
  
}

area_indexFUN <- function(area) {
  
  if (area=="County") {
    
    return("county")
    
  } else if (area=="District") {
    
    return("district")
    
  }
  
}

df_readFUN <- function(year,cat,area) {
  
  cat1 <- cat_indexFUN(cat)
  area1 <- area_indexFUN(area)
  
  read_rds(paste0("inst/pop/cen",year,"/",cat1,"/",area1,"/",
                  "cen",year,"_",cat1,"_",area1,".rds")) %>% 
    rename(cat=all_of(cat1),
           area=all_of(area1))
  
}

tableFUN <- function(year,cat,area,loc) {
  
  df_readFUN(year,cat,area) %>% 
    filter(area%in%loc) %>% 
    reactable(.,
              height=400,
              groupBy="area",
              columns=list(
                cat=colDef(name=cat),
                area=colDef(name=area),
                pop=colDef(name="Population Size")
              ))
  
}

writeFUN <- function(year,cat,area,loc) {
  
  df_readFUN(year,cat,area) %>% 
    filter(area%in%loc) 
  
}


