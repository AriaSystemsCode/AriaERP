*E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[T20100922.0014]
*B124299,1 AMH [Start]
*=oAriaApplication.DoProgram('AWRMFCSTSH','"I","0001"',.F.,'PO')
*N000587,1 HBG 02/22/2007 Add parameter to indecate if this is cost sheet or cost sheet template [Begin]
*DO FORM (oAriaApplication.ScreenHome+"MFCSTSH.SCX") WITH "I","0001"
*E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[END]
*DO FORM (oAriaApplication.ScreenHome+"MFCSTSH.SCX") WITH "I","0001","S"
=gfCallForm('MFCSTSH',.F.,'"I","0001","S"')
*E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[Start]
*N000587,1 HBG 02/22/2007 [End]
*B124299,1 AMH [End]
