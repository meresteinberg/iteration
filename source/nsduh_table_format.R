drug_import= function(html, table, drug_name) {
  
  drug_table=
    html |> 
    html_table() |> 
    nth(table) |> 
    slice(-1) |> 
    mutate(drug= drug_name) |> 
    select(-contains("P Value"))
  
  return(drug_table)
}