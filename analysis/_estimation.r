# Estimate Choice Model ---------------------------------------------------

## update with ci_scorent and ndi_scale
## noncore and micropolitan count as rural now

choice.reg <- choice.dat %>% mutate(year=as.numeric(year)) %>%
    left_join(facility.dat %>% 
              select(facility=facility_d, psi_90, perilevel01, perilevel2, perilevel34,
                     teach_major, teach_minor, c_section_elect,year), by=c("facility","year")) %>%
    left_join(patient.dat %>% 
              select(patid=alias_to_mother_longid, age=mother_age_in_years,
              ci_score, ci_scorent,
              hispanic, nhblack, nhwhite, nhother, nulliparous, primiparous,
              ins_priv, ins_mcaid, ins_mcare, ins_self, ins_other, date_delivery), by=c("patid","date_delivery")) %>%
    mutate(any_teach=(teach_major==1 | teach_minor==1))

choice.reg.m <- choice.reg %>% filter(mkt!=9)
logit.m <- dchoice.est(data=choice.reg.m, 
                       vars=c("diff_dist","perilevel2","perilevel34","teach_major",
                              "c_section_elect", "psi_90"),
                       mvars=c("diff_dist","perilevel2","perilevel34","teach_major","c_section_elect"))
pred.m <- logit.m$pred
coef.m <- logit.m$coef
mfx.m <- logit.m$mfx
mod.m <- logit.m$mod

# plot_predictions(logit.m$mod, condition="ci_score", type="response")
# avg_predictions(logit.m$mod, by="diff_dist")
avg_slopes(mod.m, variables=c("diff_dist","perilevel2","perilevel34","teach_major","c_section_elect","psi_90"), type="response", esp=1)


logit.m <- dchoice.est(data=choice.reg.m, 
                       vars=c("diff_dist","perilevel2","perilevel34","teach_major",
                              "c_section_elect", "psi_90",
                              "ci_score*diff_dist", "ci_score*perilevel2", "ci_score*perilevel34", "ci_score*teach_major", "ci_score*c_section_elect", "ci_score*psi_90",
                              "age*diff_dist", "age*perilevel2", "age*perilevel34", "age*teach_major", "age*c_section_elect", "age*psi_90",
                              "nhwhite*diff_dist", "nhwhite*perilevel2", "nhwhite*perilevel34", "nhwhite*teach_major", "nhwhite*c_section_elect", "nhwhite*psi_90",
                              "ins_mcaid*diff_dist", "ins_mcaid*perilevel2", "ins_mcaid*perilevel34", "ins_mcaid*teach_major", "ins_mcaid*c_section_elect", "ins_mcaid*psi_90"),
                       mvars=c("diff_dist","perilevel2","perilevel34","teach_major","c_section_elect"))

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



plot.mcaid <- plot_slopes(mod.m,variables="diff_dist",condition="ins_mcaid") + theme_bw()
ggsave("results/figures/distance-mcaid.png", plot.mcaid)

plot.mcaid <- plot_slopes(mod.m,variables="c_section_elect",condition="ins_mcaid") + theme_bw()
ggsave("results/figures/csection-mcaid.png", plot.mcaid)



plot.priv <- plot_slopes(mod.m,variables="diff_dist",condition="ins_priv") + theme_bw()
ggsave("results/figures/distance-priv.png", plot.priv)

plot.priv <- plot_slopes(mod.m,variables="c_section_elect",condition="ins_priv") + theme_bw()
ggsave("results/figures/csection-priv.png", plot.priv)
