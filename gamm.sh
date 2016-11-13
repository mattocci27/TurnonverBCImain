#gamm


R --vanilla --slave < ~/Dropbox/MS/LES_MS/LMApsModel/model/PA_conv_AR10_same.r > ~/Dropbox/LES/PA_conv_AR10_same.log

nohup R --vanilla --slave < ~/Dropbox/MS/TurnoverBCI/TurnoverBCImain/feeley_sim2.R > ~/Desktop/gamm.log &
