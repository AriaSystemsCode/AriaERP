*!**************************************************************************
*! Name      : REVMAIN.PRG
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 11/13/2001
*! Purpose   : Revue Custom Process Program .
*!**************************************************************************
*! Parameters: lcEvntFun -> Process event function name without 'lf..'  .
*!             lcFunPars -> Process function parameters, sent as a string.
*!**************************************************************************
*! Returns   : Logical value.       C101796,1
*!**************************************************************************
*
PARAMETER lcEvntFun,lcFunPars

lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLTRIM(lcEvntFun)+'('+lcFunPars+')'

*-- Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue
*-- End of Program.

*!**************************************************************************
*! Name      : lfwEdit
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 11/13/2001
*! Purpose   : Check if field is editable or not.
*!**************************************************************************
*! Called From : When function for Cartons,Weight,Freight in browse screen.
*!**************************************************************************
*! Example   : gfDoTriger('ARIINV',PADR('WEDIT',10))
*!**************************************************************************
*C102458,1
FUNCTION lfwEdit
PRIVATE llEditVal
llEditVal = .T.
IF &lcInvHdr..Consol = "Y"
  llEditVal = .F.
ENDIF
RETURN llEditVal
*-- End of lfwEdit.

*!**************************************************************************
*! Name      : lfAssgCrt
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 11/13/2001
*! Purpose   : Assign cartons from browse screen.
*!**************************************************************************
*! Called From : Valid function for Cartons.
*!**************************************************************************
*! Example   : gfDoTriger('ARIINV',PADR('ASSGCRT',10))
*!**************************************************************************
*C102458,1
FUNCTION lfAssgCrt
m.Cartons = &lcInvHdr..Cartons
REPLACE &lcInvHdr..Cartons WITH 0
=lfvCartons()
=lfCusIndex()
*-- End of lfAssgCrt.

*!**************************************************************************
*! Name      : lfAssgWgh
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 11/13/2001
*! Purpose   : Assign weight from browse screen.
*!**************************************************************************
*! Called From : Valid function for weight.
*!**************************************************************************
*! Example   : gfDoTriger('ARIINV',PADR('ASSGWGH',10))
*!**************************************************************************
*C102458,1
FUNCTION lfAssgWgh
m.Weight = &lcInvHdr..Weight
REPLACE &lcInvHdr..Weight WITH 0
=lfvWeight()
=lfCusIndex()
*-- End of lfAssgWgh.

*!**************************************************************************
*! Name      : lfAssgFrg
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 11/13/2001
*! Purpose   : Assign Freight from browse screen.
*!**************************************************************************
*! Called From : Valid function for freight.
*!**************************************************************************
*! Example   : gfDoTriger('ARIINV',PADR('ASSGFRG',10))
*!**************************************************************************
*C102458,1
FUNCTION lfAssgFrg
m.Freight = &lcInvHdr..Freight
REPLACE &lcInvHdr..Freight WITH 0
=lfvInvChrg()
=lfCusIndex()
*-- End of lfAssgFrg.

*!**************************************************************************
*! Name      : lfCusIndex
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 11/13/2001
*! Purpose   : Custom Index on lcInvHdr file.
*!**************************************************************************
*! Example   : gfDoTriger('ARIINV',PADR('CUSINDEX',10))
*!**************************************************************************
*C102458,1
FUNCTION lfCusIndex
DO CASE
  CASE lnSortBy = 1
    SET ORDER TO TAG (lcInvHdr) IN (lcInvHdr)  
  CASE lnSortBy = 2
    SET ORDER TO TAG CusDate IN (lcInvHdr)  
  CASE lnSortBy = 3      
    SET ORDER TO TAG CusShip IN (lcInvHdr)  
ENDCASE
IF llConsolid
  SET ORDER TO TAG (lcInvHdr) IN (lcInvHdr)
ENDIF
GO TOP IN (lcInvHdr)
*-- End of lfCusIndex.

*!**************************************************************************
*! Name      : lfAddField
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 11/13/2001
*! Purpose   : Add fields and index on lcInvHdr file.
*!**************************************************************************
*! Example   : gfDoTriger('ARIINV',PADR('ADDFIELD',10))
*!**************************************************************************
*C102458,1

*B607335,1 KHM 06/15/2003 (Begin) Changing the name of the function as its exist
*B607335,1                for other customers 
*FUNCTION lfAddField
FUNCTION lfAdInvFld
*B607335,1 KHM 06/15/2003 (End)
DECLARE laFileStru[ALEN(laFileStru,1)+2,4]
laFileStru[ALEN(laFileStru,1)-1,1] = 'SHIPDESC'
laFileStru[ALEN(laFileStru,1)-1,2] = 'C'
laFileStru[ALEN(laFileStru,1)-1,3] = 30
laFileStru[ALEN(laFileStru,1)-1,4] = 0
laFileStru[ALEN(laFileStru,1),1] = 'COMPLETE'
laFileStru[ALEN(laFileStru,1),2] = 'D'
laFileStru[ALEN(laFileStru,1),3] = 8
laFileStru[ALEN(laFileStru,1),4] = 0
DECLARE laIndex[5,2]
laIndex[4,1] = 'DTOS(Complete)+Account+Order+Store+PikTkt+CDIVISION'
laIndex[4,2] = 'CusDate'
laIndex[5,1] = 'ShipDesc+Account+Order+Store+PikTkt+CDIVISION'
laIndex[5,2] = 'CusShip'
*-- End of lfAddField.  

*!**************************************************************************
*! Name      : lfRevDumy
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 11/13/2001
*! Purpose   : Check if this customer is Revue or not.
*!**************************************************************************
*! Called From : Before gfSetup in ARIINV.
*!**************************************************************************
*! Example   : gfDoTriger('ARIINV',PADR('REVUE',10))
*!**************************************************************************
*C102458,1
FUNCTION lfRevDumy
RETURN .T.
*-- End of lfRevDumy
