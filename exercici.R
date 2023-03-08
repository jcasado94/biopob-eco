library("readxl")

pinyes <- data.frame(read_excel("Dades_Exercici_No_Presencial.xlsx", sheet = "PINYES"))
colnames(pinyes)[1] = "parcela"
colnames(pinyes)[2] = "arbre"
colnames(pinyes)[3] = "especie"
colnames(pinyes)[6] = "dap"
colnames(pinyes)[7] = "H"
colnames(pinyes)[8] = "estatus"
colnames(pinyes)[9] = "tipus_mort"
colnames(pinyes)[10] = "pinyes_totals"

#exercici 1

arbres_vius <- pinyes[pinyes$estatus != "Mort",]
dominants <- arbres_vius[arbres_vius$estatus == "Dominant",]
codominants <- arbres_vius[arbres_vius$estatus == "Codominant",]
intermitjos <- arbres_vius[arbres_vius$estatus == "Intermig",]
suprimits <- arbres_vius[arbres_vius$estatus == "Suprimit",]


pinyesVsDap(dominants)
pinyesVsDap(codominants)
pinyesVsDap(intermitjos)
pinyesVsDap(suprimits)


pinyesVsDap <- function(data) {
  pinyes_vs_dap <- lm(pinyes_totals ~ dap, data = data)
  summary <- summary(pinyes_vs_dap)
  summary
  rvalue <- summary$adj.r.squared
  pvalue <- summary$coefficients[2,4]
  plot(dominants$dap, dominants$pinyes_totals, xlab = "DAP (cm)", ylab = "Pinyes totals")
  abline(pinyes_vs_dap_dominants)
  rp = vector('expression',2)
  rp[1] = substitute(expression(italic(R)^2 == MYVALUE), 
                     list(MYVALUE = format(rvalue,dig=3)))[2]
  rp[2] = substitute(expression(italic(p) == MYOTHERVALUE), 
                     list(MYOTHERVALUE = format(pvalue, digits = 2)))[2]
  legend('topright', legend = rp, bty = 'n')
}
