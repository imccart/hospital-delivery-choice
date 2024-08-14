# Estimate Choice Model ---------------------------------------------------

choice.reg <- choice.dat %>% mutate(year=as.numeric(year)) %>%
    left_join(facility.dat %>% 
              select(facility=facility_d, psi_90, perilevel01, perilevel2, perilevel34,
                     teach_major, teach_minor, c_section_elect,year), by=c("facility","year")) %>%
    left_join(patient.dat %>% 
              select(patid=alias_to_mother_longid, age=mother_age_in_years,
              ci_score, ci_scorent,
              hispanic, nhblack, nhwhite, nhother, nulliparous, primiparous,
              ins_priv, ins_mcaid, ins_mcare, ins_self, ins_other, date_delivery), by=c("patid","date_delivery")) %>%
    mutate(ci_score_dist=ci_score*diff_dist,
           ci_score_peri2=ci_score*perilevel2, 
           ci_score_peri34=ci_score*perilevel34,
           ci_score_teach=ci_score*teach_major,
           ci_score_csect=ci_score*c_section_elect,
           age_dist=age*diff_dist,
           age_peri2=age*perilevel2,
           age_peri34=age*perilevel34,
           age_teach=age*teach_major,
           age_csect=age*c_section_elect,
           nhwite_dist=nhwhite*diff_dist,
           nhwite_peri2=nhwhite*perilevel2,
           nhwite_peri34=nhwhite*perilevel34,
           nhwite_teach=nhwhite*teach_major,
           nhwite_csect=nhwhite*c_section_elect)

choice.reg.m <- choice.reg %>% filter(mkt==2)
logit.m <- dchoice.est(data=choice.reg.m, 
                       vars=c("diff_dist","perilevel2","perilevel34","teach_major",
                              "c_section_elect", 
                              "ci_score*diff_dist", "ci_score*perilevel2", "ci_score*perilevel34", "ci_score*teach_major", "ci_score*c_section_elect",
                              "age*diff_dist", "age*perilevel2", "age*perilevel34", "age*teach_major", "age*c_section_elect",
                              "nhwhite*diff_dist", "nhwhite*perilevel2", "nhwhite*perilevel34", "nhwhite*teach_major", "nhwhite*c_section_elect"),
                       mvars=c("diff_dist","perilevel2","perilevel34","teach_major",
                              "c_section_elect", 
                              "ci_score","age","nhwhite"))
pred.m <- logit.m$pred
coef.m <- logit.m$coef
mfx.m <- logit.m$mfx
mod.m <- logit.m$mod

# plot_predictions(logit.m$mod, condition="ci_score", type="response")
# avg_predictions(logit.m$mod, by="diff_dist")

plot.age <- plot_slopes(mod.m,variables="diff_dist",condition="age") + theme_bw()
ggsave("results/figures/distance-age.png", plot.age)

plot.age <- plot_slopes(mod.m,variables="c_section_elect",condition="age") + theme_bw()
ggsave("results/figures/csection-age.png", plot.age)


plot.ci <- plot_slopes(mod.m,variables="diff_dist",condition="ci_score") + theme_bw()
ggsave("results/figures/distance-ci.png", plot.ci)

plot.ci <- plot_slopes(mod.m,variables="c_section_elect",condition="ci_score") + theme_bw()
ggsave("results/figures/csection-ci.png", plot.ci)


plot.white <- plot_slopes(mod.m,variables="diff_dist",condition="nhwhite") + theme_bw()
ggsave("results/figures/distance-white.png", plot.white)

plot.white <- plot_slopes(mod.m,variables="c_section_elect",condition="nhwhite") + theme_bw()
ggsave("results/figures/csection-white.png", plot.white)
