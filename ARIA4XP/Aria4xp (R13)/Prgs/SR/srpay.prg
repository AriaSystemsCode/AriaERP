*E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[T20100922.0014]
*-- Call program as sales rep. payment with parameter "P"
*oAriaApplication.DoProgram('AWRSRTRN',"'P'",.F.,"SR")
*E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[Start]
*DO FORM (oAriaApplication.ScreenHome+"SR\SRTRN.SCX") WITH "P"
=gfCallForm('SRTRN','SR','"P"')
*E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[END]
