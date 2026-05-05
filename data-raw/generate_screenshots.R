#!/usr/bin/env Rscript
# Generate all README screenshots
# Run from package root: Rscript data-raw/generate_screenshots.R

devtools::load_all(reset = TRUE)
data(jtbd_sample)
library(ggplot2)
library(dplyr)
library(tidyr)
library(gt)

# ----- Study + sample-size constants used in every footer -----
STUDY <- "JTBD Sample Survey"
N_TOTAL <- get_sample_size(jtbd_sample)               # 200
SEG_N <- as.integer(table(jtbd_sample$segment))
names(SEG_N) <- levels(droplevels(jtbd_sample$segment))
N_BY_SEG <- c(all = N_TOTAL, SEG_N)                    # for per-segment CIs

scores <- get_jtbd_scores(jtbd_sample)
scores_ci <- add_score_cis(scores, n = N_TOTAL)
comparison <- get_jtbd_scores.comparison(jtbd_sample, "segment")
comparison_ci <- add_score_cis(comparison, n = N_BY_SEG)

clean_obj <- function(x) {
  x <- gsub("minimize_time_to_", "", x)
  x <- gsub("minimize_likelihood_of_", "Avoid ", x)
  x <- gsub("_", " ", x)
  tools::toTitleCase(x)
}

# ============================================================
# 1. Opportunity matrix with zones (auto Wilson CI crossbars)
# ============================================================
p <- plot_opportunity_matrix(scores,
  title = "Opportunity Score Matrix",
  subtitle = "Importance x Satisfaction with under/over-served zones",
  show_zones = TRUE,
  n = N_TOTAL, study = STUDY)
ggsave("man/figures/readme-opportunity-matrix-zones.png", p,
       width = 10, height = 7, dpi = 150, bg = "white")
cat("1. Opportunity matrix saved\n")

# ============================================================
# 2. Importance vs Satisfaction Gap Chart
# ============================================================
gap_df <- scores_ci %>%
  mutate(label = clean_obj(as.character(objective))) %>%
  arrange(desc(opp.all)) %>%
  mutate(label = factor(label, levels = rev(label)))

p_gap <- ggplot(gap_df) +
  geom_segment(aes(x = sat.all, xend = imp.all, y = label, yend = label),
               color = "#BDC3C7", linewidth = 2) +
  geom_errorbarh(aes(y = label, xmin = imp_lo.all, xmax = imp_hi.all),
                 color = "#2C3E50", height = 0.25, alpha = 0.5, linewidth = 0.4) +
  geom_errorbarh(aes(y = label, xmin = sat_lo.all, xmax = sat_hi.all),
                 color = "#E74C3C", height = 0.25, alpha = 0.5, linewidth = 0.4) +
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
       x = "Score (0-10)", y = "",
       caption = jtbd_footer(n = N_TOTAL, study = STUDY, extra = "Error bars: 95% Wilson CI")) +
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
       x = "", y = "",
       caption = jtbd_footer(n = N_BY_SEG[c("all","casual","new_user","power_user")], study = STUDY)) +
  theme_jtbd() +
  theme(panel.grid = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(face = "bold", size = 11),
        legend.position = "right")

ggsave("man/figures/readme-heatmap.png", p_heat, width = 9, height = 7, dpi = 150, bg = "white")
cat("3. Heatmap saved\n")

# ============================================================
# 4. Cleveland dot plot comparing two segments (with Wilson CIs)
# ============================================================
pair_df <- comparison_ci %>%
  select(job_step, objective,
         opp.casual, opp.power_user,
         opp_lo.casual, opp_hi.casual,
         opp_lo.power_user, opp_hi.power_user) %>%
  mutate(label = clean_obj(as.character(objective)),
         gap = opp.power_user - opp.casual,
         x_left = pmin(opp.casual, opp.power_user),
         x_right = pmax(opp.casual, opp.power_user),
         left_color = ifelse(opp.casual < opp.power_user, "#3498DB", "#2ECC71"),
         right_color = ifelse(opp.casual < opp.power_user, "#2ECC71", "#3498DB"),
         left_label = ifelse(opp.casual < opp.power_user,
                             round(opp.casual, 1), round(opp.power_user, 1)),
         right_label = ifelse(opp.casual < opp.power_user,
                              round(opp.power_user, 1), round(opp.casual, 1))) %>%
  arrange(gap) %>%
  mutate(label = factor(label, levels = label))

p_cleveland <- ggplot(pair_df) +
  geom_segment(aes(x = opp.casual, xend = opp.power_user, y = label, yend = label),
               color = "#BDC3C7", linewidth = 1.5) +
  geom_errorbarh(aes(y = label, xmin = opp_lo.casual, xmax = opp_hi.casual),
                 color = "#3498DB", height = 0.25, alpha = 0.5, linewidth = 0.4) +
  geom_errorbarh(aes(y = label, xmin = opp_lo.power_user, xmax = opp_hi.power_user),
                 color = "#2ECC71", height = 0.25, alpha = 0.5, linewidth = 0.4) +
  geom_point(aes(x = opp.casual, y = label), color = "#3498DB", size = 4) +
  geom_point(aes(x = opp.power_user, y = label), color = "#2ECC71", size = 4) +
  geom_text(aes(x = x_left, y = label, label = left_label),
            nudge_x = -0.7, size = 3, color = pair_df$left_color) +
  geom_text(aes(x = x_right, y = label, label = right_label),
            nudge_x = 0.7, size = 3, color = pair_df$right_color) +
  annotate("text", x = 1, y = 12.5, label = "Casual", color = "#3498DB",
           size = 4, fontface = "bold", hjust = 0) +
  annotate("text", x = 20, y = 12.5, label = "Power User", color = "#2ECC71",
           size = 4, fontface = "bold", hjust = 1) +
  scale_x_continuous(limits = c(-1, 21)) +
  labs(title = "Casual vs Power User: Where Do They Diverge?",
       subtitle = "Connected dots show how the same objective scores differently across segments",
       x = "Opportunity Score", y = "",
       caption = jtbd_footer(n = N_BY_SEG[c("casual","power_user")], study = STUDY,
                             extra = "Error bars: 95% Wilson CI")) +
  theme_jtbd() +
  theme(panel.grid.major.y = element_blank(),
        axis.line.y = element_blank())

ggsave("man/figures/readme-cleveland.png", p_cleveland, width = 10, height = 6, dpi = 150, bg = "white")
cat("4. Cleveland saved\n")

# ============================================================
# 5. Priority ranking with zones (with Wilson CI bars on opp scores)
# ============================================================
water_df <- scores_ci %>%
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
  geom_errorbar(aes(ymin = opp_lo.all, ymax = opp_hi.all),
                width = 0.25, color = "#2C3E50", alpha = 0.6, linewidth = 0.4) +
  geom_hline(yintercept = 10, linetype = "dashed", color = "#7F8C8D", linewidth = 0.5) +
  annotate("text", x = 0.5, y = 10.4, label = "High Opportunity Threshold",
           hjust = 0, size = 3, color = "#7F8C8D", fontface = "italic") +
  geom_text(aes(label = round(opp.all, 1)), hjust = -0.2, size = 3.2,
            position = position_nudge(y = 0)) +
  coord_flip() +
  scale_fill_manual(values = c("Critical" = "#C0392B", "High" = "#E74C3C",
                                "Moderate" = "#F39C12", "Low" = "#95A5A6")) +
  scale_y_continuous(limits = c(0, 17), expand = c(0, 0)) +
  labs(title = "Opportunity Score Ranking",
       subtitle = "Color-coded priority: critical > high > moderate > low",
       x = "", y = "Opportunity Score", fill = "Priority",
       caption = jtbd_footer(n = N_TOTAL, study = STUDY, extra = "Error bars: 95% Wilson CI")) +
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
       x = "Importance", y = "Satisfaction", color = "Action",
       caption = jtbd_footer(n = N_TOTAL, study = STUDY)) +
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
       x = "Difference in Opportunity Score", y = "", fill = "Segment",
       caption = jtbd_footer(n = N_BY_SEG[c("all","casual","new_user","power_user")], study = STUDY)) +
  theme_jtbd() +
  theme(panel.grid.major.y = element_blank(),
        axis.line.y = element_blank(),
        legend.position = "top")

ggsave("man/figures/readme-segment-divergence.png", p_div, width = 10, height = 7, dpi = 150, bg = "white")
cat("7. Segment divergence saved\n")

# ============================================================
# 8. Segment comparison gt table — Importance/Satisfaction with ± SE
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
             subtitle = "Sample JTBD Survey") %>%
  cols_label(label = "Objective", opp.all = "All", opp.casual = "Casual",
             opp.new_user = "New User", opp.power_user = "Power User") %>%
  fmt_number(columns = c(opp.all, opp.casual, opp.new_user, opp.power_user), decimals = 1) %>%
  gt_theme_jtbd(n = N_BY_SEG[c("all","casual","new_user","power_user")], study = STUDY) %>%
  cols_width(label ~ px(280), everything() ~ px(90))

gtsave(tbl, filename = "readme-segment-table.png", path = "man/figures/", vwidth = 800, vheight = 800)
cat("8. Segment table saved\n")

# ============================================================
# 9. Top opportunities (compact bar of just the top 6)
# ============================================================
top_df <- scores_ci %>%
  arrange(desc(opp.all)) %>%
  slice_head(n = 6) %>%
  mutate(label = clean_obj(as.character(objective)),
         label = factor(label, levels = rev(label)))

p_top <- ggplot(top_df, aes(x = label, y = opp.all)) +
  geom_col(fill = "#E74C3C", width = 0.65) +
  geom_errorbar(aes(ymin = opp_lo.all, ymax = opp_hi.all),
                width = 0.25, color = "#2C3E50", alpha = 0.7, linewidth = 0.4) +
  geom_text(aes(label = round(opp.all, 1)), hjust = -0.25, size = 3.5) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 17), expand = c(0, 0)) +
  labs(title = "Top 6 Opportunities",
       subtitle = "Where users are most underserved",
       x = "", y = "Opportunity Score",
       caption = jtbd_footer(n = N_TOTAL, study = STUDY, extra = "Error bars: 95% Wilson CI")) +
  theme_jtbd() +
  theme(axis.line.y = element_blank())

ggsave("man/figures/readme-top-opportunities.png", p_top, width = 9, height = 5, dpi = 150, bg = "white")
cat("9. Top opportunities saved\n")

# ============================================================
# 10. Bare opportunity matrix (no zones, with auto CI crossbars)
# ============================================================
p_om <- plot_opportunity_matrix(scores,
  title = "Opportunity Score Matrix",
  subtitle = "Importance vs Satisfaction (high-opp items in red)",
  show_zones = FALSE,
  n = N_TOTAL, study = STUDY)
ggsave("man/figures/readme-opportunity-matrix.png", p_om,
       width = 10, height = 7, dpi = 150, bg = "white")
cat("10. Opportunity matrix (no zones) saved\n")

# ============================================================
# Outcome-based segmentation pipeline (PCA + K-Means + profiles)
# ============================================================
set.seed(42)
cl <- jtbd_cluster(jtbd_sample, n_clusters = 3)
prof <- jtbd_profile_segments(cl$data)
opp_profile <- jtbd_cluster_profile(jtbd_sample, cl, test_sig = FALSE)

cluster_n <- as.integer(table(cl$data$jtbd_cluster))
names(cluster_n) <- levels(cl$data$jtbd_cluster)

# 11. PCA scree
ggsave("man/figures/readme-pca-scree.png",
       plot_pca_scree(cl$pca, n = N_TOTAL, study = STUDY),
       width = 9, height = 5.5, dpi = 150, bg = "white")
cat("11. PCA scree saved\n")

# 12. PCA loadings
ggsave("man/figures/readme-pca-loadings.png",
       plot_pca_loadings(cl$pca, component = 1, n = N_TOTAL, study = STUDY),
       width = 9, height = 6, dpi = 150, bg = "white")
cat("12. PCA loadings saved\n")

# 13. Elbow + silhouette
k_eval <- jtbd_find_k(jtbd_sample, max_k = 6)
ggsave("man/figures/readme-elbow.png",
       plot_elbow(k_eval, n = N_TOTAL, study = STUDY),
       width = 9, height = 5.5, dpi = 150, bg = "white")
cat("13. Elbow saved\n")

# 14. PCA biplot with clusters
ggsave("man/figures/readme-pca-biplot.png",
       plot_pca_biplot(cl$pca, cl$cluster, n = N_TOTAL, study = STUDY),
       width = 8, height = 6, dpi = 150, bg = "white")
cat("14. PCA biplot saved\n")

# 15. Cluster opportunity heatmap
ggsave("man/figures/readme-cluster-heatmap.png",
       plot_cluster_heatmap(opp_profile, n = cluster_n, study = STUDY),
       width = 9, height = 7, dpi = 150, bg = "white")
cat("15. Cluster heatmap saved\n")

# 16. Segment divergence (profile)
ggsave("man/figures/readme-segment-profiles.png",
       plot_segment_profiles(prof, n = cluster_n, study = STUDY),
       width = 10, height = 7, dpi = 150, bg = "white")
cat("16. Segment profiles saved\n")

# 17. Segment index heatmap
ggsave("man/figures/readme-segment-index.png",
       plot_segment_index(prof, n = cluster_n, study = STUDY),
       width = 9, height = 6, dpi = 150, bg = "white")
cat("17. Segment index saved\n")

# 18. Segment fingerprint (parallel coordinates)
ggsave("man/figures/readme-parallel.png",
       plot_segment_fingerprint(prof, n = cluster_n, study = STUDY),
       width = 11, height = 6, dpi = 150, bg = "white")
cat("18. Segment fingerprint saved\n")

# 19. Segment DNA (faceted lollipops)
ggsave("man/figures/readme-lollipop-facet.png",
       plot_segment_dna(prof, n = cluster_n, study = STUDY),
       width = 11, height = 6, dpi = 150, bg = "white")
cat("19. Segment DNA saved\n")

# 20–21. Radar charts (overlaid + faceted)
ggsave("man/figures/readme-radar.png",
       plot_segment_radar(prof, n = cluster_n, study = STUDY),
       width = 9, height = 7, dpi = 150, bg = "white")
cat("20. Segment radar saved\n")

ggsave("man/figures/readme-radar-facet.png",
       plot_segment_radar_facet(prof, n = cluster_n, study = STUDY),
       width = 11, height = 6.5, dpi = 150, bg = "white")
cat("21. Segment radar facet saved\n")

# 22. Persona table
gtsave(create_persona_table(prof, opp_profile, n = cluster_n, study = STUDY),
       filename = "readme-persona-table.png", path = "man/figures/",
       vwidth = 1100, vheight = 600)
cat("22. Persona table saved\n")

cat("\nAll 22 README visualizations regenerated.\n")
