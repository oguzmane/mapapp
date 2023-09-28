
cat_indexFUN <- function(cat) {
  
  if (cat=="Age") {
    
    return("age")
    
  } else if (cat=="Ethnicity") {
    
    return("ethnicity")
    
  } else if (cat=="Gender") {
    
    return("gender")
    
  } else if (cat=="Sex") {
    
    return("sex")
    
  } 
  
}

df_readFUN <- function(year,cat,area) {
  
  cat1 <- cat_indexFUN(cat)
  
  read_rds(paste0("inst/pop/cen",year,"/",
                  "cen",year,"_",cat1,
                  ".rds")) %>% 
    rename(cat=all_of(cat1),
           area=tolower(area)) %>% 
    dplyr::select(area,cat,pop) %>% 
    group_by(area,cat) %>% 
    mutate(pop=sum(pop)) %>% 
    ungroup() %>% 
    distinct()
    
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
    filter(area%in%loc) %>% 
    rename_with(.cols=area, ~area) %>% 
    rename_with(.cols=cat, ~cat) %>% 
    rename_with(.cols=pop, ~"Population Size")
  
}


