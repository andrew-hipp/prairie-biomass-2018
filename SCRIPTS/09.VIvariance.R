
t.test(prairie$vNDVI.CI ~ prairie$Plot.category)
# variance in GDVI2 is significantly higher in monoculture plots p = 0.006
plot(x = prairie$Plot.category, y = prairie$vGDVI2)


summary(aov(prairie$vNDVI ~ prairie$trait.div))
## phy div
# vNDVI p = 0.08
# vGNDVI p = 0.02
# vGDVI2 p = 0.05

plot(x = prairie$phy.div, y = prairie$vGNDVI)
# higher phy div has higher variance, opposite trend from mono/tmt

summary(aov(prairie$vGDVI2 ~ prairie$trait.div))
## trait div
# vNDVI p = 0.04
# vGNDVI p = 0.12
# vGDVI2 p = 0.15

plot(x = prairie$trait.div, y = prairie$vNDVI)
# low trait div has higher variance, same trend as mono/tmt
