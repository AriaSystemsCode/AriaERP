*!*****************************************************************************************
*!*	Descrption    : Loan Tracker Program
*!*	Developer     : Mariam Mazhar {MMT}
*!*	Date          : 06/10/2015
*!*	Entry #       : C201684 [T20150409.0006]
*!*****************************************************************************************
PUBLIC m.loann, m.entered, m.loaned, m.dueback, m.returned, m.billto, m.billton, m.shipto, m.cperson, m.cemail, m.billable, m.scharge
PUBLIC m.comm1, m.comm2, m.comm3, mtgqty, mtgdlrs, m.shipadd1, m.shipadd2, m.shipadd3, m.shipadd4, m.shipadd5, mreting
PUBLIC m.entered, m.loaned, m.dueback, m.returned, mfrtchg, mrclick, m.rentfee, m.loantype, msavexl
STORE 0 TO m.loann, mtgqty, mtgdlrs, mfrtchg, mtrqty, m.rentfee
STORE DATE() TO m.loaned, m.entered
m.returned = {}
m.dueback = m.loaned + 30
mreting = .F.
m.loantype = 1
STORE "" TO m.billto, m.billton, m.shipto, m.cperson, m.cemail, m.comm1, m.comm2, m.comm3, m.shipadd1, m.shipadd2, m.shipadd3,  ;
      m.shipadd4, m.shipadd5
STORE "" TO m.billto1, m.billto2, m.billto3, m.billto4, m.billto5
STORE .T. TO m.billable, m.scharge
DO FORM (oAriaApplication.ScreenHome+"AR\loantrack.SCX")  WITH  oAriaApplication.DefaultPath 
