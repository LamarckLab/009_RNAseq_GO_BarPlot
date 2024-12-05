# 加载必要的包
library(ggplot2)

# 读取编辑后的CSV文件
ego_result_BP <- read.csv("C:/Users/Lamarck/Desktop/ego_result_BP.csv", header = TRUE)
ego_result_CC <- read.csv("C:/Users/Lamarck/Desktop/ego_result_CC.csv", header = TRUE)
ego_result_MF <- read.csv("C:/Users/Lamarck/Desktop/ego_result_MF.csv", header = TRUE)

# 合并数据框
go_enrich_df <- data.frame(
  ID = c(ego_result_BP$ID, ego_result_CC$ID, ego_result_MF$ID),
  Description = c(ego_result_BP$Description, ego_result_CC$Description, ego_result_MF$Description),
  GeneNumber = c(ego_result_BP$Count, ego_result_CC$Count, ego_result_MF$Count),
  type = factor(
    c(
      rep("biological process", nrow(ego_result_BP)),
      rep("cellular component", nrow(ego_result_CC)),
      rep("molecular function", nrow(ego_result_MF))
    ),
    levels = c("biological process", "cellular component", "molecular function")
  )
)

# 简化GO描述
for (i in 1:nrow(go_enrich_df)) {
  description_splite <- strsplit(go_enrich_df$Description[i], split = " ")
  description_collapse <- paste(description_splite[[1]][1:5], collapse = " ")
  go_enrich_df$Description[i] <- description_collapse
}
go_enrich_df$Description <- gsub(pattern = "NA", "", go_enrich_df$Description)

# 绘制柱状图
go_enrich_df$type_order <- factor(rev(as.integer(rownames(go_enrich_df))), labels = rev(go_enrich_df$Description))
COLS <- c("#66C3A5", "#8DA1CB", "#FD8D62")

# 横向柱状图
ggplot(data = go_enrich_df, aes(x = type_order, y = GeneNumber, fill = type)) +
  geom_bar(stat = "identity", width = 0.8) +
  scale_fill_manual(values = COLS) +
  coord_flip() +
  xlab("GO term") +
  ylab("Gene Number") +
  labs(title = "The Most Enriched GO Terms") +
  theme_bw()

###竖着的柱状图 
go_enrich_df$type_order=factor(rev(as.integer(rownames(go_enrich_df))),labels=rev(go_enrich_df$Description))
COLS <- c("#66C3A5", "#8DA1CB", "#FD8D62")
ggplot(data=go_enrich_df, aes(x=type_order,y=GeneNumber, fill=type)) + 
  geom_bar(stat="identity", width=0.8) + 
  scale_fill_manual(values = COLS) + 
  theme_bw() + 
  xlab("GO term") + 
  ylab("Num of Genes") + 
  labs(title = "The Most Enriched GO Terms")+ 
  theme(axis.text.x=element_text(face = "bold", color="gray50",angle = 70,vjust = 1, hjust = 1 ))#angle是坐标轴字体倾斜的角度，可以自己设置
