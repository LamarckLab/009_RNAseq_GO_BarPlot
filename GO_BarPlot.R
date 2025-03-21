# 加载必要的包
library(ggplot2)
library(dplyr)

# 读取 CSV 文件
ego_result_BP <- read.csv("C:/Users/Lamarck/Desktop/ego_result_BP.csv", header = TRUE)
ego_result_CC <- read.csv("C:/Users/Lamarck/Desktop/ego_result_CC.csv", header = TRUE)
ego_result_MF <- read.csv("C:/Users/Lamarck/Desktop/ego_result_MF.csv", header = TRUE)

# 添加分类信息
ego_result_BP$type <- "biological process"
ego_result_CC$type <- "cellular component"
ego_result_MF$type <- "molecular function"

# 合并三个数据框
go_enrich_df <- bind_rows(ego_result_BP, ego_result_CC, ego_result_MF)

# 提取需要列，并计算 -log10(pvalue)
go_enrich_df <- go_enrich_df %>%
  dplyr::select(Description, pvalue, type) %>%
  mutate(logP = -log10(pvalue))

# 每类选取前10个p值最小的GO条目
go_enrich_df <- go_enrich_df %>%
  group_by(type) %>%
  arrange(pvalue) %>%
  slice_head(n = 10) %>%
  ungroup()

# 按照 type + pvalue 排序，设置 GO term 顺序
go_enrich_df <- go_enrich_df %>%
  arrange(factor(type, levels = c("biological process", "cellular component", "molecular function")), pvalue)

go_enrich_df$Description <- factor(go_enrich_df$Description, levels = rev(go_enrich_df$Description))

# 设置颜色
COLS <- c("biological process" = "#66C3A5", 
          "cellular component" = "#8DA1CB", 
          "molecular function" = "#FD8D62")

# 设置输出路径（Windows 桌面）
output_path <- "C:/Users/Lamarck/Desktop/GO_BarPlot.pdf"

# 保存为 PDF，设定图像大小（单位为英寸，可根据需要调整）
pdf(file = output_path, width = 10, height = 8)

# 绘图
ggplot(go_enrich_df, aes(x = Description, y = logP, fill = type)) +
  geom_bar(stat = "identity", width = 0.8) +
  scale_fill_manual(values = COLS) +
  coord_flip() +
  xlab("GO term") +
  ylab("-log10(P value)") +
  labs(title = "Top Enriched GO Terms by Category") +
  theme_bw() +
  theme(
    axis.text.y = element_text(size = 12, color = "black"),
    axis.text.x = element_text(size = 12, color = "black"),
    axis.title = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    legend.position = "right"  # 可改为 "bottom", "top", "left"，或 c(0.8, 0.2) 自定义坐标
  )

# 关闭设备，保存文件
dev.off()
