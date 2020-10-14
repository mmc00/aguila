# data0 
# data plot
data_ <- fit2 %>% 
  broom::tidy() %>% 
  select(-uniqueness)

data_plot0 <- data_ %>% 
  pivot_longer(cols = starts_with("fl"), names_to = "factores",
               values_to = "values")

data_plot1 <- data_plot0 %>% 
  group_by(variable) %>% 
  summarise(val_abs = max(abs(values), na.rm = T), .groups = "drop")

data_plot0 <- data_plot0 %>% 
  mutate(val_abs = abs(values))

data_plot_f <- data_plot1 %>% 
  left_join(data_plot0, by = c("val_abs", "variable")) %>% 
  select(-val_abs) %>% 
  mutate(factores = as.character(factores)) %>% 
  mutate(ido = as.integer(gsub("fl", "", factores)))

# data
data_plot_f

# factores
factores_n <- data_plot_f %>% 
  select(factores) %>% 
  distinct() %>% 
  unlist() %>% 
  unname() %>% 
  length()

# data_mmd 
data_mmd <- data_plot_f %>% 
  mutate(var_fix = gsub("\\,.*","", variable)) %>% 
  mutate(variable = gsub("\\(.*","", var_fix)) %>% 
  mutate(id = as.numeric(gsub("fl", "", factores)))%>%  
  mutate(id2 = (row_number() + factores_n)) %>% 
  mutate_at(.vars = c("id", "id2"), .funs = ~LETTERS[.]) %>% 
  mutate(values = round(values, 2)) %>% 
  mutate(mmd = paste0(id, "[", factores, "] ", "--> ",
                      "|", values, "| ",
                      id2, "(", variable, ")")) %>% 
  select(mmd) %>% 
  add_row(mmd = "graph LR", .before = 1)

write.table(data_mmd, "diagr.mmd",
            col.names = F,
            row.names = F,
            quote = F)

DiagrammeR::mermaid("diagr.mmd")           
