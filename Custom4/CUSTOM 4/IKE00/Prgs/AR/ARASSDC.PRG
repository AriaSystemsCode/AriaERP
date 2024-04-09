*!*************************************************************
*! Name      : ARASSDC.PRG
*! Developer : Mariam Mazhar[MMT]
*! Date      : 08/04/2016
*! Purpose   : Assign stores to DC
*! Entry#    : C201853 [P20160323.0002]
*!*************************************************************
= gfOpGrid('ARSOTRDC' , .T.)  && Run selection grid.
*!*************************************************************
*! Name      : lfvAccount
*! Developer : Mariam Mazhar[MMT]
*! Date      : 08/04/2016
*! Purpose   : Validate Account
*!*************************************************************
FUNCTION lfvAccount
PRIVATE lcObjVal
PRIVATE lnAlsNo,lcCustOrd,lcObjName
lnAlsNo = SELECT(0)
IF !USED('CUSTOMER')
  =gfOpenTable('Customer','Customer')
ENDIF
SELECT CUSTOMER
lcCustOrd = ORDER()
gfSetOrder('CUSTOMER')
*IF The user want to Browse or if the Account he entered is not in the file
IF '?' $ lcRpAcct .OR. (!EMPTY(lcRpAcct) .AND. !gfSEEK('M' +lcRpAcct, 'CUSTOMER'))
  llObjRet = CusBrowM(@lcRpAcct, '' , 'M')
  lcRpAcct= IIF(llObjRet , lcRpAcct, '')
ENDIF

SELECT CUSTOMER
gfSETORDER(lcCustOrd)
SELECT(lnAlsNo)
CLEARREAD()
FUNCTION lfwOGWhen

*!*************************************************************
*! Name      : lfvDC
*! Developer : Mariam Mazhar[MMT]
*! Date      : 08/04/2016
*! Purpose   : Validate DC
*!*************************************************************
FUNCTION lfvDC
IF EMPTY(lcRpAcct)
  =gfModalGen('TRM00000B00000',.F.,.F.,.F.,'Please select Account.')
  lcRpDc = ''
  RETURN
ENDIF

PRIVATE lcObjVal
PRIVATE lnAlsNo,lcCustOrd,lcObjName
lnAlsNo = SELECT(0)
IF !USED('CUSTOMER')
  =gfOpenTable('Customer','Customer')
ENDIF
SELECT CUSTOMER
lcCustOrd = ORDER()
gfSetOrder('CUSTOMER')
*IF The user want to Browse or if the Account he entered is not in the file
IF '?' $ lcRpDc .OR. (!EMPTY(lcRpDc) AND !EMPTY(lcRpAcct) .AND. !gfSEEK('S' +lcRpAcct+PADR(lcRpDc,8) , 'CUSTOMER'))
  llObjRet = CusBrowM(@lcRpAcct, '' , 'S')
  lcRpDc = IIF(llObjRet , Customer.Store, '')
ENDIF
SELECT CUSTOMER
gfSETORDER(lcCustOrd)
SELECT(lnAlsNo)
CLEARREAD()
*!*************************************************************
*! Name      : lfCreatExp
*! Developer : Mariam Mazhar[MMT]
*! Date      : 08/04/2016
*! Purpose   : Create Expression
*!*************************************************************
FUNCTION lfCreatExp
IF EMPTY(lcRpAcct) 
  =gfModalGen('TRM00000B00000',.F.,.F.,.F.,'There is no selected Account#. Cannot proceed.')
  RETURN .F.
ENDIF
 
IF EMPTY(lcRpDc)
  =gfModalGen('TRM00000B00000',.F.,.F.,.F.,'There is no selected DC#. Cannot proceed.') 
  RETURN .F.  
ENDIF
llSelectStore = .F.
lnPosStore = ASCAN(loOgScroll.laOgFXFlt,"CUSTOMER.STORE")
IF lnPosStore > 0
  lnPosStore = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosStore,1)
  lcCursorStore= loOgScroll.laOgFxFlt[lnPosStore,6]
  IF !EMPTY(lcCursorStore)
    SELECT(lcCursorStore)
    LOCATE
    IF !EOF()
      llSelectStore = .T.
    ENDIF  
  ENDIF  
ENDIF
IF !llSelectStore 
  IF gfModalgen("QRM00000B00006","DIALOG",.F.,.F.,'No Stores selected. Do you want to assign all Account#:'+ALLTRIM(lcRpAcct)+' stores to DC#:'+ALLTRIM(lcRpDc)+'?') = 2
    RETURN .F.
  ENDIF
ELSE
  IF gfModalgen("QRM00000B00006","DIALOG",.F.,.F.,'Do you want to assign the selected stores of Account#:'+ALLTRIM(lcRpAcct)+' to DC#:'+ALLTRIM(lcRpDc)+'?') = 2
    RETURN .F.
  ENDIF
ENDIF

IF llSelectStore 
  SELECT (lcCursorStore)
  LOCATE
  SCAN FOR &lcCursorStore..KEYEXP <> lcRpDc 
    WAIT WINDOW  "Assigning store # "+ALLt(&lcCursorStore..KEYEXP)+" to DC # "+ALLTRIM(lcRpDc) NOWAIT 
    IF gfSeek('S'+lcRpAcct+&lcCursorStore..KEYEXP,'CUSTOMER','CUSTOMER')
      SELECT CUSTOMER
      =gfReplace('DIST_CTR with "'+lcRpDc+'"')
    ENDIF
  ENDSCAN
ELSE
  SELECT CUSTOMER
  =gfSetOrder("CUSTOMER")
  =gfSeek('S'+lcRpAcct)
  SCAN REST WHILE TYPE+ACCOUNT+STORE= 'S'+lcRpAcct FOR STORE <> lcRpDc 
    WAIT WINDOW  "Assigning store # "+ALLt(CUSTOMER.STORE)+" to DC # "+ALLTRIM(lcRpDc) NOWAIT
    =gfReplace('DIST_CTR with "'+lcRpDc+'"')     
  ENDSCAN
ENDIF
SELECT CUSTOMER
=gfTableUpdate()
=gfModalGen('INM00000B00000',.F.,.F.,.F.,'Selected stores assigned to DC# '+ALLTRIM(lcRpDc)+' successfully.')