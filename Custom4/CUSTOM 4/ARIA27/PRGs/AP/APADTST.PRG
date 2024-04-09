lnTimeOut = .0009
 
if !gfSetup()
 return
endif


*E300683,1 Call *.SPR from screens directory
* DO APADTST.SPR 
DO (gcScrDir + gcWinAppl + '\APADTST.SPR')
*E300683,1 end          