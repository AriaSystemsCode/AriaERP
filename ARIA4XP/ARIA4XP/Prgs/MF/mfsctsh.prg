*E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[T20100922.0014]
*B124299,1 AMH [Start]
*=oAriaApplication.DoProgram('AWRMFCSTSH','"M","0001"',.F.,'MF')
*N000587,1 HBG 02/22/2007 Add parameter to indecate if this is cost sheet or cost sheet template [Begin]
*DO FORM (oAriaApplication.ScreenHome+"MFCSTSH.SCX") WITH "M","0001"
*E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[START]
*DO FORM (oAriaApplication.ScreenHome+"MFCSTSH.SCX") WITH "M","0001","S"
=gfCallForm('MFCSTSH',.F.,'"M","0001","S"')
*E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[END]
*N000587,1 HBG 02/22/2007 [End]
*B124299,1 AMH [End]
