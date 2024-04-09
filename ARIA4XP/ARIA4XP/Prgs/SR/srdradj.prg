*E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[T20100922.0014]
*-- Call program as sales rep. debit adjustment with parameter "D"
*oAriaApplication.DoProgram('AWRSRTRN',"'D'",.F.,"SR")
*E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[START]
*DO FORM (oAriaApplication.ScreenHome+"SR\SRTRN.SCX") WITH "D"
=gfCallForm('SRTRN','SR','"D"')
*E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[END]