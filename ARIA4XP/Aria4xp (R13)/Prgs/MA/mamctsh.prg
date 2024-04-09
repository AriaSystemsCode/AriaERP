*E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[T20100922.0014]
*B124299,1 AMH [Start]
*=oAriaApplication.DoProgram('AWRMFCSTSH','"T","0002"',.F.,'MA')
*E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[Start]
*DO FORM (oAriaApplication.ScreenHome+"MFCSTSH.SCX") WITH "T","0002"
=gfCallForm('MFCSTSH',.F.,'"T","0002"')
*E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[END]
*B124299,1 AMH [End]
