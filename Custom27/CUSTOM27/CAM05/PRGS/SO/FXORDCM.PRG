**:***************************************************************************
*  PROGRAM       : FxOrdCmp.PRG
*  APPLICATION   : ARIA27
*  MODULE        : SO2
*  PORPOS        : Fix orders for the customer "Campio" , no data in the order header except the order No , 
*                  the order line data is complete
*  DEVELOPER     : TMI - TAREK MOHAMED IBRAHIM
*  ENTRY #       : B# 606576 Open Issue 115140
**:***************************************************************************

*--Open needed files
=gfOpenFile(gcDataDir+'ORDHDR','ORDACCT','SH')
=gfOpenFile(gcDataDir+'ORDLINE','ORDLINE','SH')
=gfOpenFile(gcDataDir+'STYLE','STYLE','SH')
=gfOpenFile(gcDataDir+'CODES','CCODE_NO','SH')

*--Set needed relations
SELECT ORDHDR
SET RELATION TO 'O'+ORDER INTO ORDLINE
SELECT ORDLINE
SET RELATION TO STYLE INTO STYLE

SELECT ORDHDR
LOCATE FOR ACCOUNT+CORDTYPE+ORDER = '     '
IF !FOUND()
  =gfModalGen('QRM00000B00000','DIALOG',.F.,.F.,'There are no orders to fix.')
  RETURN
ENDIF

IF gfModalGen('QRM00000B00006','DIALOG',.F.,.F.,;
      'This program will fix orders. Do you want to continue?') = 2
  RETURN
ENDIF   

*--Loop throgh all orders with no account in the account field
SELECT ORDHDR
SET ORDER TO 
GO TOP
*SCAN REST WHILE ACCOUNT+CORDTYPE+ORDER = '     '
SCAN FOR ORDHDR.ACCOUNT = '     '

  WAIT WINDOW NOWAIT 'Updating order# : '+ORDHDR.ORDER
  SCATTER MEMVAR BLANK
  
  M.CORDTYPE    = ORDHDR.CORDTYPE
  M.ORDER       = ORDHDR.ORDER       

  M.CDIVISION = STYLE.CDIVISION 

  DIMENSION laSeas[1],laStore[1],laCustPo[1]
  SELECT ORDLINE
  SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = M.CORDTYPE+M.ORDER      

    M.OPEN      = M.OPEN    + ORDLINE.TOTQTY
    M.OPENAMT   = M.OPENAMT + ORDLINE.TOTQTY*ORDLINE.PRICE
    M.BOOK      = M.BOOK    + ORDLINE.TOTBOOK    
    M.BOOKAMT   = M.BOOKAMT + ORDLINE.TOTBOOK*ORDLINE.PRICE

    M.LASTLINE  = ORDLINE.LINENO    
    
    M.START     = IIF(M.ENTERED <ORDLINE.START,ORDLINE.START,M.ENTERED )
    M.ENTERED   = M.START      
    M.COMPLETE  = IIF(M.COMPLETE <ORDLINE.COMPLETE,ORDLINE.COMPLETE,M.COMPLETE )

    =lfPopArr('laSeas'  ,ORDLINE.SEASON)
    =lfPopArr('laStore' ,ORDLINE.STORE)
    =lfPopArr('laCustPo',ORDLINE.CUSTPO)
    
  ENDSCAN  
  =SEEK(M.CORDTYPE+M.ORDER,'ORDLINE')   && Reposition the pointer in the ordline file
  
  M.DISC        = 0
  M.STATUS      = 'H'
  M.ACCOUNT     = ORDLINE.ACCOUNT
  
  M.SEASON      = IIF(ALEN(laSeas)  > 1 ,'*     ',laSeas[1])
  M.STORE       = IIF(ALEN(laStore) > 1 ,'        ',laStore[1])
  M.MULTI       = IIF(ALEN(laStore) > 1 , 'Y' , 'N' )
  M.BULK        = 'N'
      
  M.DEPT        = ''
  
  M.MULTIPO     = IIF(M.MULTI = 'N' , .F. , ALEN(laCustPo)>1 )
  M.CUSTPO      = IIF(ALEN(laCustPo) > 1 , '' , laCustPo[1] )
  
  M.PRIORITY    = '5'
  
  M.NOTE1       = ''
  M.NOTE2       = ''
  
  M.CORDERCAT   = ''
  M.CREORDER    = 'N'
  M.CONSOL      = ''

  M.CTERMCODE   = lfDefcode('CTERMCODE')  
  M.SHIPVIA     = lfDefcode('SHIPVIA')
  M.SPCINST     = lfDefcode('SPCINST')
  
  M.CINSUR      = 'Y'
  M.BUYER       = ''
  M.CFACCODE    = ''
  M.FACTACCT    = ''
  M.APPROVAL    = ''
  M.APPRAMT     = 0
  M.REP1        = ''
  M.COMM1       = 0
  M.REP2        = ''
  M.COMM2       = 0
  
  M.DECL_DATE   = {}
  M.DECL_CODE   = ''

  M.CANCELLED   = {}
  M.CANCEL      = 0
  M.CANCELAMT   = 0
  M.SHIP        = 0
  M.SHIPAMT     = 0

  M.TOTCUT      = 0 
  M.FLAG        = ''

  M.LINK_CODE   = 'DEFDEF'
  M.GL_SALES    = 'DEF   '
  M.INT_VEND    = ''
  M.EVENT_COD   = ''
  M.BILLNO      = ''
  M.MERC_TYPE   = ''
  M.BLANK_ORD   = ''
  M.DISTRB_NO   = ''
  M.CCLASS      = ''
  M.MON_FLG     = ''
  M.LABELS      = 0
  
  M.CWARECODE   = ORDLINE.CWARECODE
  
  M.CCANCRESON  = ''

  M.ALT_SHPTO   = .F.
  M.STNAME      = ''
  M.CADDRESS1   = ''
  M.CADDRESS2   = ''
  M.CADDRESS3   = ''
  M.CADDRESS4   = ''
  M.CADDRESS5   = ''
  M.PHONE       = ''

  M.CCURRCODE   = 'USD'
  M.NEXRATE     = 1.0000
  M.NCURRUNIT   = 1
  
  M.CFROMORDER  = ''
  M.DIRECT_INV  = ''
  M.LHASNOTES   = .F.

  M.CADD_USER   = gcUser_ID
  M.CLOK_TIME   = ''
  M.LEDIORDER   = .F.
  M.COWNER      = ''
  M.CADD_TIME   = TIME()
  M.DADD_DATE   = gdSysdate
  M.LLOK_STAT   = .F.
  M.CLOK_USER   = ''
  M.DLOK_DATE   = {}
  M.LFROMWEB    = .F.
  M.CEDIT_USER  = gcUser_ID
  M.DEDIT_DATE  = gdSysdate
  M.CEDIT_TIME  = TIME()
  
  SELECT ORDHDR
  GATHER MEMVAR
    
ENDSCAN
WAIT CLEAR  
=gfModalGen('QRM00000B00000','DIALOG',.F.,.F.,;
      'Orders updated successfully.')


*:**************************************************************************
*:* Name        : lfPopArr
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 10/28/2002
*:* Purpose     : Populate passed array with a value
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : lcArr   : Passed array name
*:*            : lcValue : Value to be inserted in the array
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfPopArr()
*:***************************************************************************
FUNCTION lfPopArr
PARAMETERS lcArr,lcValue

IF ASCAN(&lcArr,lcValue) = 0
  IF EMPTY(&lcArr.[1])
    &lcArr.[1] = lcValue
  ELSE
    DIME &lcArr.[ALEN(&lcArr.)+1] 
    &lcArr.[1] = lcValue
  ENDIF
ENDIF    
*-- end of lfPopArr.


*:**************************************************************************
*:* Name        : lfDefcode
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 10/28/2002
*:* Purpose     : Get the default code from the codes file
*:***************************************************************************
*:* Parameters : lcCodeName  : Code name 
*:***************************************************************************
*:* Return      : Default code
*:***************************************************************************
*:* Example     :  = lfDefcode('SEASON')
*:***************************************************************************
FUNCTION lfDefcode
PARAMETERS lcCodeName
PRIVATE lcRet
lcCodeName = ALLT(UPPER(lcCodeName))
lcRet = ''
IF SEEK('D'+lcCodeName,'CODES')
  lcRet = CODES.CCODE_NO
ENDIF
RETURN lcRet
*-- end of lfDefcode.