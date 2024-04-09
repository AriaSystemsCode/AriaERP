*-- Call the POSHP screen with the parametrs for Style Inter Location Shipmnet
*DO (gcAppHome+'\POSHP') WITH .F.,'M'
*=oAriaApplication.DoProgram('AWRPOSHP',['','N','N'],'','PO')
DO FORM (oAriaApplication.ScreenHome+"POSHP.SCX") WITH .F.,'N','N'
