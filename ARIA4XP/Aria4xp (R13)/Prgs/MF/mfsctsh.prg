*E303898,1 MMT 12/12/2017 Modify Aria4XP screens to work in view and add modes from Aria5[T20171126.0003][Start]
LPARAMETERS lcItmMajor,lcCostSht_id
*E303898,1 MMT 12/12/2017 Modify Aria4XP screens to work in view and add modes from Aria5[T20171126.0003][End]
*E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[T20100922.0014]
*B124299,1 AMH [Start]
*=oAriaApplication.DoProgram('AWRMFCSTSH','"M","0001"',.F.,'MF')
*N000587,1 HBG 02/22/2007 Add parameter to indecate if this is cost sheet or cost sheet template [Begin]
*DO FORM (oAriaApplication.ScreenHome+"MFCSTSH.SCX") WITH "M","0001"
*E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[START]
*DO FORM (oAriaApplication.ScreenHome+"MFCSTSH.SCX") WITH "M","0001","S"
*E303898,1 MMT 12/12/2017 Modify Aria4XP screens to work in view and add modes from Aria5[T20171126.0003][Start]
*=gfCallForm('MFCSTSH',.F.,'"M","0001","S"')
IF TYPE('lcItmMajor')='C'
  =gfCallForm('MFCSTSH',.F.,'"M","0001","S",.F.,.F.,"'+lcItmMajor+'","'+IIF(TYPE('lcCostSht_id')='C',lcCostSht_id,'')+'"')
ELSE
  =gfCallForm('MFCSTSH',.F.,'"M","0001","S"')
ENDIF  
*E303898,1 MMT 12/12/2017 Modify Aria4XP screens to work in view and add modes from Aria5[T20171126.0003][End]
*E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[END]
*N000587,1 HBG 02/22/2007 [End]
*B124299,1 AMH [End]
