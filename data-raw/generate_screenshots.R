#!/usr/bin/env Rscript
# Generate all README screenshots
# Run from package root: Rscript data-raw/generate_screenshots.R

devtools::load_all(reset = TRUE)
data(jtbd_sample)
library(ggplot2)
library(dplyr)
library(tidyr)
library(gt)

scores <- get_jtbd_scores(jtbd_sample)
comparison <- get_jtbd_scores.comparison(jtbd_sample, "segment")

clean_obj <- function(x) {
  x <- gsub("minimize_time_to_", "", x)
  x <- gsub("minimize_likelihood_of_", "Avoid ", x)
  x <- gsub("_", " ", x)
  tools::toTitleCase(x)
}

# ============================================================
# 1. Opportunity matrix with zones
# ============================================================
p <- plot_opportunity_matrix(scores,
  title = "Opportunity Score Matrix",
  subtitle = "Sample JTBD Survey (N=200)",
  show_zones = TRUE)
ggsave("man/figures/readme-opportunity-matrix-zones.png", p,
       width = 10, height = 7, dpi = 150, bg = "white")
cat("1. Opportunity matrix saved\n")

# ============================================================
# 2. Importance vs Satisfaction Gap Chart
# ============================================================
gap_df <- scores %>%
  mutate(label = clean_obj(as.character(objective))) %>%
  arrange(desc(opp.all)) %>%
  mutate(label = factor(label, levels = rev(label)))

p_gap <- ggplot(gap_df) +
  geom_segment(aes(x = sat.all, xend = imp.all, y = label, yend = label),
               color = "#BDC3C7", linewidth = 2) +
  geom_point(aes(x = imp.all, y = label), color = "#2C3E50", size = 4) +
  geom_point(aes(x = sat.all, y = label), color = "#E74C3C", size = 4) +
  geom_text(aes(x = imp.all, y = label, label = round(imp.all, 1)),
            nudge_x = 0.35, size = 3, color = "#2C3E50") +
  geom_text(aes(x = sat.all, y = label, label = round(sat.all, 1)),
            nudge_x = -0.35, size = 3, color = "#E74C3C") +
  annotate("text", x = 9.5, y = 12.5, label = "Importance", color = "#2C3E50",
           size = 3.5, fontface = "bold") +
  annotate("text", x = 1.5, y = 12.5, label = "Satisfaction", color = "#E74C3C",
           size = 3.5, fontface = "bold") +
  scale_x_continuous(limits = c(0, 11), breaks = seq(0, 10, 2)) +
  labs(title = "The Opportunity Gap",
       subtitle = "Wider gap = bigger opportunity. Dark = importance, Red = satisfaction",
       x = "Score (0-10)", y = "") +
  theme_jtbd() +
  theme(panel.grid.major.y = element_blank(),
        axis.line.y = element_blank())

ggsave("man/figures/readme-gap-chart.png", p_gap, width = 10, height = 6, dpi = 150, bg = "white")
cat("2. Gap chart saved\n")

# ============================================================
# 3. Heatmap of all scores across segments
# ============================================================
heat_df <- comparison %>%
  select(job_step, objective, opp.all, opp.casual, opp.new_user, opp.power_user) %>%
  mutate(label = clean_obj(as.character(objective)),
         step = tools::toTitleCase(gsub("_", " ", job_step))) %>%
  arrange(desc(opp.all)) %>%
  mutate(label = factor(label, levels = rev(label))) %>%
  select(label, step, opp.all, opp.casual, opp.new_user, opp.power_user) %>%
  pivot_longer(cols = starts_with("opp."), names_to = "segment", values_to = "opp") %>%
  mutate(segment = case_when(
    segment == "opp.all" ~ "All",
    segment == "opp.casual" ~ "Casual",
    segment == "opp.new_user" ~ "New User",
    segment == "opp.power_user" ~ "Power User"
  ),
  segment = factor(segment, levels = c("All", "Casual", "New User", "Power User")))

p_heat <- ggplot(heat_df, aes(x = segment, y = label, fill = opp)) +
  geom_tile(color = "white", linewidth = 1.5) +
  geom_text(aes(label = round(opp, 1),
                color = opp > 13), size = 3.5, fontface = "bold", show.legend = FALSE) +
  scale_color_manual(values = c("TRUE" = "white", "FALSE" = "#2C3E50")) +
  scale_fill_gradient2(low = "#F7F7F7", mid = "#FDEBD0", high = "#C0392B",
                       midpoint = 10, limits = c(2, 19),
                       name = "Opportunity\nScore") +
  labs(title = "Opportunity Heatmap by Segment",
       subtitle = "Darker = bigger unmet need. Compare across columns to find segment-specific pain.",
       x = "", y = "") +
  theme_jtbd() +
  theme(panel.grid = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(face = "bold", size = 11),
        legend.position = "right")

ggsave("man/figures/readme-heatmap.png", p_heat, width = 9, height = 7, dpi = 150, bg = "white")
cat("3. Heatmap saved\n")

# ============================================================
# 4. Cleveland dot plot comparing two segments
# ============================================================
pair_df <- comparison %>%
  select(job_step, objective, opp.casual, opp.power_user) %>%
  mutate(label = clean_obj(as.character(objective)),
         gap = opp.power_user - opp.casual) %>%
  arrange(gap) %>%
  mutate(label = factor(label, levels = label))

p_cleveland <- ggplot(pair_df) +
  geom_segment(aes(x = opp.casual, xend = opp.power_user, y = label, yend = label),
               color = "#BDC3C7", linewidth = 1.5) +
  geom_point(aes(x = opp.casual, y = label), color = "#3498DB", size = 4) +
  geom_point(aes(x = opp.power_user, y = label), color = "#2ECC71", size = 4) +
  geom_text(aes(x = opp.casual, y = label, label = round(opp.casual, 1)),
            nudge_x = -0.6, size = 3, color = "#3498DB") +
  geom_text(aes(x = opp.power_user, y = label, label = round(opp.power_user, 1)),
            nudge_x = 0.6, size = 3, color = "#2ECC71") +
  annotate("text", x = 3, y = 12.5, label = "Casual", color = "#3498DB",
           size = 4, fontface = "bold") +
  annotate("text", x = 19, y = 12.5, label = "Power User", color = "#2ECC71",
           size = 4, fontface = "bold") +
  scale_x_continuous(limits = c(0, 21)) +
  labs(title = "Casual vs Power User: Where Do They Diverge?",
       subtitle = "Connected dots show how the same objective scores differently across segments",
       x = "Opportunity Score", y = "") +
  theme_jtbd() +
  theme(panel.grid.major.y = element_blank(),
        axis.line.y = element_blank())

ggsave("man/figures/readme-cleveland.png", p_cleveland, width = 10, height = 6, dpi = 150, bg = "white")
cat("4. Cleveland saved\n")

# ============================================================
# 5. Priority ranking with zones
# ============================================================
water_df <- scores %>%
  mutate(label = clean_obj(as.character(objective)),
         step = tools::toTitleCase(gsub("_", " ", job_step))) %>%
  arrange(desc(opp.all)) %>%
  mutate(label = factor(label, levels = rev(label)),
         zone = case_when(
           opp.all >= 12 ~ "Critical",
           opp.all >= 10 ~ "High",
           opp.all >= 8 ~ "Moderate",
           TRUE ~ "Low"
         ),
         zone = factor(zone, levels = c("Critical", "High", "Moderate", "Low")))

p_ranked <- ggplot(water_df, aes(x = label, y = opp.all, fill = zone)) +
  geom_col(width = 0.7) +
  geom_hline(yintercept = 10, linetype = "dashed", color = "#7F8C8D", linewidth = 0.5) +
  annotate("text", x = 0.5, y = 10.4, label = "High Opportunity Threshold",
           hjust = 0, size = 3, color = "#7F8C8D", fontface = "italic") +
  geom_text(aes(label = round(opp.all, 1)), hjust = -0.2, size = 3.2) +
  coord_flip() +
  scale_fill_manual(values = c("Critical" = "#C0392B", "High" = "#E74C3C",
                                "Moderate" = "#F39C12", "Low" = "#95A5A6")) +
  scale_y_continuous(limits = c(0, 17), expand = c(0, 0)) +
  labs(title = "Opportunity Score Ranking",
       subtitle = "Color-coded priority: critical > high > moderate > low",
       x = "", y = "Opportunity Score", fill = "Priority") +
  theme_jtbd() +
  theme(axis.line.y = element_blank(),
        legend.position = c(0.85, 0.3))

ggsave("man/figures/readme-ranked.png", p_ranked, width = 10, height = 6, dpi = 150, bg = "white")
cat("5. Ranked saved\n")

# ============================================================
# 6. Strategic Quadrant
# ============================================================
quad_df <- scores %>%
  mutate(
    label = clean_obj(as.character(objective)),
    imp_med = median(imp.all),
    sat_med = median(sat.all),
    quadrant = case_when(
      imp.all >= imp_med & sat.all <= sat_med ~ "Act Now",
      imp.all >= imp_med & sat.all > sat_med ~ "Monitor",
      imp.all < imp_med & sat.all <= sat_med ~ "Investigate",
      TRUE ~ "Deprioritize"
    ),
    quadrant = factor(quadrant, levels = c("Act Now", "Monitor", "Investigate", "Deprioritize"))
  )

imp_med <- median(scores$imp.all)
sat_med <- median(scores$sat.all)

p_quad <- ggplot(quad_df, aes(x = imp.all, y = sat.all)) +
  annotate("rect", xmin = imp_med, xmax = 10, ymin = 0, ymax = sat_med,
           fill = "#FADBD8", alpha = 0.5) +
  annotate("rect", xmin = imp_med, xmax = 10, ymin = sat_med, ymax = 10,
           fill = "#FDEBD0", alpha = 0.5) +
  annotate("rect", xmin = 0, xmax = imp_med, ymin = 0, ymax = sat_med,
           fill = "#D5F5E3", alpha = 0.5) +
  annotate("rect", xmin = 0, xmax = imp_med, ymin = sat_med, ymax = 10,
           fill = "#EBF5FB", alpha = 0.5) +
  annotate("text", x = 9, y = 0.8, label = "ACT NOW", fontface = "bold",
           color = "#C0392B", size = 4.5, alpha = 0.7) +
  annotate("text", x = 9, y = 9, label = "MONITOR", fontface = "bold",
           color = "#E67E22", size = 4.5, alpha = 0.7) +
  annotate("text", x = 1.5, y = 0.8, label = "INVESTIGATE", fontface = "bold",
           color = "#27AE60", size = 4.5, alpha = 0.7) +
  annotate("text", x = 1.5, y = 9, label = "DEPRIORITIZE", fontface = "bold",
           color = "#3498DB", size = 4.5, alpha = 0.7) +
  geom_point(aes(color = quadrant), size = 5) +
  ggrepel::geom_text_repel(aes(label = label), size = 3.2, max.overlaps = 15,
                           seed = 42, box.padding = 0.5) +
  geom_hline(yintercept = sat_med, linetype = "dashed", color = "#7F8C8D") +
  geom_vline(xintercept = imp_med, linetype = "dashed", color = "#7F8C8D") +
  scale_color_manual(values = c("Act Now" = "#C0392B", "Monitor" = "#E67E22",
                                 "Investigate" = "#27AE60", "Deprioritize" = "#3498DB")) +
  coord_cartesian(xlim = c(0, 10), ylim = c(0, 10)) +
  labs(title = "Strategic Priority Quadrant",
       subtitle = "Where to focus product investment based on importance x satisfaction",
       x = "Importance", y = "Satisfaction", color = "Action") +
  theme_jtbd() +
  theme(legend.position = "none")

ggsave("man/figures/readme-quadrant.png", p_quad, width = 10, height = 8, dpi = 150, bg = "white")
cat("6. Quadrant saved\n")

# ============================================================
# 7. Segment divergence
# ============================================================
div_df <- comparison %>%
  select(job_step, objective, opp.all, opp.casual, opp.new_user, opp.power_user) %>%
  pivot_longer(cols = c(opp.casual, opp.new_user, opp.power_user),
               names_to = "segment", values_to = "opp") %>%
  mutate(
    segment = gsub("opp\\.", "", segment),
    segment = gsub("_", " ", segment),
    segment = tools::toTitleCase(segment),
    delta = opp - opp.all,
    label = clean_obj(as.character(objective))
  )

p_div <- ggplot(div_df, aes(x = delta, y = reorder(label, abs(delta)), fill = segment)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6, alpha = 0.85) +
  geom_vline(xintercept = 0, linewidth = 0.5, color = "#2C3E50") +
  scale_fill_manual(values = c("Casual" = "#3498DB", "New User" = "#E74C3C", "Power User" = "#2ECC71")) +
  labs(title = "Segment Divergence from Overall",
       subtitle = "How each segment's opportunity scores differ from the population average",
       x = "Difference in Opportunity Score", y = "", fill = "Segment") +
  theme_jtbd() +
  theme(panel.grid.major.y = element_blank(),
        axis.line.y = element_blank(),
        legend.position = "top")

ggsave("man/figures/readme-segment-divergence.png", p_div, width = 10, height = 7, dpi = 150, bg = "white")
cat("7. Segment divergence saved\n")

# ============================================================
# 8. Segment comparison gt table (regenerate with latest data)
# ============================================================
tbl_data <- comparison %>%
  select(job_step, objective, opp.all, opp.casual, opp.new_user, opp.power_user) %>%
  mutate(label = clean_obj(as.character(objective)),
         step = tools::toTitleCase(gsub("_", " ", job_step))) %>%
  arrange(desc(opp.all)) %>%
  select(step, label, opp.all, opp.casual, opp.new_user, opp.power_user)

tbl <- tbl_data %>%
  gt(groupname_col = "step") %>%
  tab_header(title = "Opportunity Scores by Segment",
             subtitle = "Sample JTBD Survey (N=200)") %>%
  cols_label(label = "Objective", opp.all = "All", opp.casual = "Casual",
             opp.new_user = "New User", opp.power_user = "Power User") %>%
  fmt_number(columns = c(opp.all, opp.casual, opp.new_user, opp.power_user), decimals = 1) %>%
  gt_theme_jtbd() %>%
  cols_width(label ~ px(280), everything() ~ px(90))

gtsave(tbl, filename = "readme-segment-table.png", path = "man/figures/", vwidth = 800, vheight = 800)
cat("8. Segment table saved\n")

cat("\nAll 8 visualizations generated!\n")
