*-- Call the POSHP screen with the parametrs for Materail PO Shipmnet
*DO (gcAppHome+'\POSHP') WITH .F.,'M'
*=oAriaApplication.DoProgram('AWRPOSHP',['','P','M'],'','MA')
DO FORM (oAriaApplication.ScreenHome+"POSHP.SCX") WITH .F.,'P','M'
