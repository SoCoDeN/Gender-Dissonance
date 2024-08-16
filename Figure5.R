print(Table_for_figure_bargraphs[Table_for_figure_bargraphs$eventname=="3 year follow up",] %>%
  group_by(eventname, kbi_y_trans_id) %>%
  ggplot(aes(x = eventname,fill = kbi_y_trans_id, y = Gender_Dissonance)) + 
  scale_fill_manual(values = c("#00BFC4","#7CAE00","#C77CFF","#F8766D"))+
  geom_violin(trim=FALSE))