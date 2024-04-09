*E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[T20100922.0014]
*-- Call program as sales rep. credit adjustment with parameter "C"
*oAriaApplication.DoProgram('AWRSRTRN',"'C'",.F.,"SR")
*E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[Start]
*DO FORM (oAriaApplication.ScreenHome+"SR\SRTRN.SCX") WITH "C"
=gfCallForm('SRTRN','SR','"C"')
*E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[END]