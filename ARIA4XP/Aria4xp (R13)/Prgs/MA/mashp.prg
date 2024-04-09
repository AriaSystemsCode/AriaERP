*E303898,1 MMT 12/12/2017 Modify Aria4XP screens to work in view and add modes from Aria5[T20171126.0003][Start]
LPARAMETERS lcShpMntNo
*E303898,1 MMT 12/12/2017 Modify Aria4XP screens to work in view and add modes from Aria5[T20171126.0003][End]

*-- Call the POSHP screen with the parametrs for Materail PO Shipmnet
*DO (gcAppHome+'\POSHP') WITH .F.,'M'
*=oAriaApplication.DoProgram('AWRPOSHP',['','P','M'],'','MA')
*E303898,1 MMT 12/12/2017 Modify Aria4XP screens to work in view and add modes from Aria5[T20171126.0003][Start]
*DO FORM (oAriaApplication.ScreenHome+"POSHP.SCX") WITH .F.,'P','M'
DO FORM (oAriaApplication.ScreenHome+"POSHP.SCX") WITH lcShpMntNo,'P','M'
*E303898,1 MMT 12/12/2017 Modify Aria4XP screens to work in view and add modes from Aria5[T20171126.0003][End]