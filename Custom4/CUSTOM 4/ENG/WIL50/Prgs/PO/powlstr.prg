*:************************************************************************
*: Program file  : ICOTSEX.PRG
*: Program desc. : Update the Wilson PO Tracking file 
*:         System: ARIA4XP
*:         Module: PO
*:      Developer: TMI - TAREK MOHAMMED IBRAHIM
*:     Tracking #: C200853,1 , ticket #T20070720.0008
*:     Date      : 09/13/2007
*:************************************************************************
* Modifications
*C200853,4 TMI 10/21/2007 Adding the CVENSTY field to the output sql file
*C200853,5 11/21/2007 Change qotation formulas to meet data with qotations at Wilson imports , like " Kir's Mode " as 
*                          a style description
*                      replace the "'" with '[' and ']' for places that seems to use qotation as part of the item description
*:************************************************************************

IF gfModalGen('INM00000B34012','','','',"Update the PO Tracking file.") <> 1
  RETURN
ENDIF

**- get the color position and length
STORE 0 TO lnClrPos,lnClrLen
=lfUpdCLrDt()

*- Open needed files to use in the program
=lfOpenFiles()

*- set relations
SELECT STYINVJL
SET RELATION TO STYLE INTO STYLE, ;
                CWARECODE INTO WAREHOUS
SELECT STYLE
SET RELATION TO 'S'+SCALE INTO SCALE

SELECT STYINVJL
LOCATE

llAddLine = .F.

WAIT WINDOW NOWAIT STYINVJL.ctrtype+STYINVJL.ctrcode

SELECT POWLSTR
=gfSeek('')

*- Loop to update the cursors
SELECT STYINVJL
SCAN FOR !LWLSPO
  llAddLine = .T.
  
  WAIT WINDOW NOWAIT STYINVJL.ctrtype+STYINVJL.ctrcode
  STORE '' TO M.CPONO,M.CSONO
  M.SPRICE = 0
  
  DO CASE
  
  CASE STYINVJL.CTRTYPE = '6'
  
    SELECT POWLSTR
    M.CPONO = STYINVJL.ctrcode

    SELECT CUTPICK
    =gfSetOrder('CUTPICK')
    =gfSeek('2'+M.CPONO)
    
    m.CSONO = CUTPICK.ORDER
    M.SPRICE = STYLE.PRICEA
    
  CASE STYINVJL.CTRTYPE = '3'

    SELECT INVHDR
    =gfSeek(STYINVJL.CTRCODE)
    m.CSONO = INVHDR.ORDER
    
    SELECT CUTPICK
    =gfSetOrder('CUTORD')
    =gfSeek('2'+INVHDR.ORDER)    
    M.CPONO = CUTPICK.CTKTNO
    
    SELECT INVLINE
    =gfSeek(STYINVJL.CTRCODE+STR(STYINVJL.LINENO,6))
    M.SPRICE = INVLINE.PRICE
    
  CASE STYINVJL.CTRTYPE = '7'
    
    SELECT RETHDR
    =gfSeek(STYINVJL.CTRCODE)
    
    SELECT POWLSTR
    =Seek('3'+RETHDR.INVOICE+STYINVJL.STYLE)
    M.CPONO = POWLSTR.CPONO
    M.CSONO = RETHDR.ORDER  
    SELECT RETLINE
    =gfSeek(STYINVJL.CTRCODE+STYINVJL.STYLE+STR(STYINVJL.LINENO,6))
    M.SPRICE = RETLINE.PRICE
    
  CASE STYINVJL.CTRTYPE = '1'
    
    SELECT POSHDR
    *T20070905.0020 TMI [Start] 
    *IF !EMPTY(STYINVJL.CADJREF) AND gfSeek('PP'+STYINVJL.CADJREF)
    *  M.CPONO = STYINVJL.CADJREF
    IF !EMPTY(STYINVJL.CADJREF)
      M.CPONO = PADL(ALLTRIM(STYINVJL.CADJREF),6,'0')
      *T20070905.0020 TMI [End  ] 
      SELECT CUTPICK
      =gfSetOrder('CUTPKORD')
      =gfSEEK('2'+M.CPONO)
      M.CSONO = CUTPICK.ORDER
      M.SPRICE = STYLE.PRICEA
    ELSE
      M.CPONO = STYINVJL.CSESSION
    ENDIF
    
  ENDCASE
  
  *- Build the update string expression
  DO CASE
  CASE STYINVJL.CTRTYPE $ '1367'
    *T20070905.0020 TMI [Start] 
    SELECT ORDHDR
    =gfSeek('O'+m.CSONO)    
    *T20070905.0020 TMI [End  ] 

    *C200853,5 TMI [Start] replace the "'" with '[' and ']' for places that seems to use qotation as part of the item description
    *lcAddStr =  [CPONO     WITH  ']+M.CPONO+[' ] +;
                [CPOLN     WITH  ']+STR(STYINVJL.LINENO,6)+[' ] +;
                [CSONO     WITH  ']+M.CSONO+[' ] +;
                [SPRICE    WITH   ]+STR(M.SPRICE,7,2) + [ ]+;
                [CTRTYPE   WITH  ']+STYINVJL.CTRTYPE       +[' ]+;
                [CTRANNO   WITH  ']+STYINVJL.CTRCODE       +[' ]+;
                [STYLE     WITH  ']+STYINVJL.STYLE         +[' ]+;
                [CWARECODE WITH  ']+STYINVJL.CWARECODE     +[' ]+;
                [CDESC     WITH  ']+WAREHOUS.CDESC         +[' ]+;
                [CSESSION  WITH  ']+STYINVJL.CSESSION      +[' ]+;
                [CUSTPO    WITH  ']+ORDHDR.CUSTPO          +[' ]+;
                [CTRDATE   WITH  {^]+STR(YEAR(STYINVJL.DTRDATE),4)+'-'+STR(MONTH(STYINVJL.DTRDATE),2)+'-'+STR(DAY(STYINVJL.DTRDATE)) +[} ]

    *lcAddStr = lcAddStr + ;
                [QTY1      WITH   QTY1+]+STR(STYINVJL.NSTK1,7)  +[ ]+;
                [QTY2      WITH   QTY2+]+STR(STYINVJL.NSTK2,7)  +[ ]+;
                [QTY3      WITH   QTY3+]+STR(STYINVJL.NSTK3,7)  +[ ]+;
                [QTY4      WITH   QTY4+]+STR(STYINVJL.NSTK4,7)  +[ ]+;
                [QTY5      WITH   QTY5+]+STR(STYINVJL.NSTK5,7)  +[ ]+;
                [QTY6      WITH   QTY6+]+STR(STYINVJL.NSTK6,7)  +[ ]+;
                [QTY7      WITH   QTY7+]+STR(STYINVJL.NSTK7,7)  +[ ]+;
                [QTY8      WITH   QTY8+]+STR(STYINVJL.NSTK8,7)  +[ ]+;
                [TOTQTY    WITH   TOTQTY+]+STR(STYINVJL.NTOTSTK,7)+[ ]+;
                [NCOST      WITH   ]+STR(STYINVJL.NCOST,9,2) +[ ]+;
                [CDIVISION  WITH  ']+STYLE.CDIVISION    +[' ] 

    *lcAddStr = lcAddStr + ;
                [Colourdesc WITH ']+gfCodDes(SUBSTR(STYINVJL.STYLE,lnClrPos,lnClrLen),'COLOR') + [' ] +;
                [DESC       WITH  ']+STYLE.DESC1        +[' ]+;
                [SDESC1     WITH  ']+SCALE.SZ1          +[' ]+;
                [SDESC2     WITH  ']+SCALE.SZ2          +[' ]+;
                [SDESC3     WITH  ']+SCALE.SZ3          +[' ]+;
                [SDESC4     WITH  ']+SCALE.SZ4          +[' ]+;
                [SDESC5     WITH  ']+SCALE.SZ5          +[' ]+;
                [SDESC6     WITH  ']+SCALE.SZ6          +[' ]+;
                [SDESC7     WITH  ']+SCALE.SZ7          +[' ]+;
                [SDESC8     WITH  ']+SCALE.SZ8          +[' ]

    *C200853,4 TMI [Start] Add the cvensty to the output file
    *lcAddStr = lcAddStr + ;
                [CVENSTY   WITH  ']+STYLE.CVENSTY        +[' ]
    *C200853,4 TMI [END  ]

    lcAddStr =  [CPONO     WITH  ]+'['+M.CPONO+'] '+;
                [CPOLN     WITH  ']+STR(STYINVJL.LINENO,6)+[' ] +;
                [CSONO     WITH  ']+M.CSONO+[' ] +;
                [SPRICE    WITH   ]+STR(M.SPRICE,7,2)      +[ ]+;
                [CTRTYPE   WITH  ']+STYINVJL.CTRTYPE       +[' ]+;
                [CTRANNO   WITH  ']+STYINVJL.CTRCODE       +[' ]+;
                [STYLE     WITH  ]+'['+STYINVJL.STYLE      +'] '+;
                [CWARECODE WITH  ]+'['+STYINVJL.CWARECODE  +'] '+;
                [CDESC     WITH  ]+'['+WAREHOUS.CDESC      +'] '+;
                [CSESSION  WITH  ']+STYINVJL.CSESSION      +[' ]+;
                [CUSTPO    WITH  ]+'['+ORDHDR.CUSTPO       +'] '+;
                [CTRDATE   WITH  {^]+STR(YEAR(STYINVJL.DTRDATE),4)+'-'+STR(MONTH(STYINVJL.DTRDATE),2)+'-'+STR(DAY(STYINVJL.DTRDATE)) +[} ]

    lcAddStr = lcAddStr + ;
                [QTY1      WITH   QTY1+]+STR(STYINVJL.NSTK1,7)  +[ ]+;
                [QTY2      WITH   QTY2+]+STR(STYINVJL.NSTK2,7)  +[ ]+;
                [QTY3      WITH   QTY3+]+STR(STYINVJL.NSTK3,7)  +[ ]+;
                [QTY4      WITH   QTY4+]+STR(STYINVJL.NSTK4,7)  +[ ]+;
                [QTY5      WITH   QTY5+]+STR(STYINVJL.NSTK5,7)  +[ ]+;
                [QTY6      WITH   QTY6+]+STR(STYINVJL.NSTK6,7)  +[ ]+;
                [QTY7      WITH   QTY7+]+STR(STYINVJL.NSTK7,7)  +[ ]+;
                [QTY8      WITH   QTY8+]+STR(STYINVJL.NSTK8,7)  +[ ]+;
                [TOTQTY    WITH   TOTQTY+]+STR(STYINVJL.NTOTSTK,7)+[ ]+;
                [NCOST      WITH   ]+STR(STYINVJL.NCOST,9,2) +[ ]+;
                [CDIVISION  WITH  ]+'['+STYLE.CDIVISION    +'] ' 
    lcAddStr = lcAddStr + ;
                [Colourdesc WITH ]+'['+gfCodDes(SUBSTR(STYINVJL.STYLE,lnClrPos,lnClrLen),'COLOR') + '] ' +;
                [DESC       WITH  ]+'['+STYLE.DESC1        +'] '+;
                [SDESC1     WITH  ]+'['+SCALE.SZ1          +'] '+;
                [SDESC2     WITH  ]+'['+SCALE.SZ2          +'] '+;
                [SDESC3     WITH  ]+'['+SCALE.SZ3          +'] '+;
                [SDESC4     WITH  ]+'['+SCALE.SZ4          +'] '+;
                [SDESC5     WITH  ]+'['+SCALE.SZ5          +'] '+;
                [SDESC6     WITH  ]+'['+SCALE.SZ6          +'] '+;
                [SDESC7     WITH  ]+'['+SCALE.SZ7          +'] '+;
                [SDESC8     WITH  ]+'['+SCALE.SZ8          +'] '

    lcAddStr = lcAddStr + ;
                [CVENSTY   WITH  ]+'['+STYLE.CVENSTY        +'] '
    *C200853,5 TMI [End  ] 


    SELECT POWLSTR
    && Key :: CTRTYPE+CTRANNO+STYLE+CPOLN
    *T20070905.0020 TMI [Start] 
    *IF !SEEK(STYINVJL.CTRTYPE+STYINVJL.CTRCODE+STYINVJL.STYLE+M.CPONO+STR(STYINVJL.LINENO,6))
    IF !SEEK(STYINVJL.CTRTYPE+STYINVJL.CTRCODE+STYINVJL.STYLE+M.CPONO+STR(STYINVJL.LINENO,6)+STYINVJL.CSESSION)
      *T20070905.0020 TMI [End  ] 
      =gfAppend()
    ENDIF
    =gfReplace(lcAddStr)   

  CASE STYINVJL.CTRTYPE $ '48'
  
      lcTyp = STR(VAL(STYINVJL.CTRTYPE)-1,1)
      SELECT POWLSTR
      =Seek(lcTyp+STYINVJL.CTRCODE)
      SCAN REST WHILE CTRTYPE+CTRANNO+STYLE+CPOLN = lcTyp+STYINVJL.CTRCODE      
        =gfDelete()
      ENDSCAN

  ENDCASE

  SELECT STYINVJL
  REPLACE LWLSPO WITH .T.

ENDSCAN

IF llAddLine
  
  SELECT POWLSTR

  lcTranCode = oAriaApplication.RemoteCompanyData.BeginTran(oAriaApplication.ActiveCompanyConStr,3,'')
  IF TYPE('lcTranCode') = 'N'
    =oAriaApplication.RemoteCompanyData.CheckRetResult("BeginTran",lcTranCode,.T.)
    RETURN .F.
  ENDIF
  
  llUpdate = gfTableUpdate(lcTranCode,'POWLSTR')
  llUpdate = IIF(llUpdate,gfTableUpdate(lcTranCode,'STYINVJL'),.F.)
  
  IF llUpdate
    =oAriaApplication.RemoteCompanyData.CommitTran(lcTranCode)
    SELECT STYINVJL
    *=TABLEUPDATE(.T.)
    =gfModalGen('INM00000B00000','','','',"Data updated.")
  ELSE
    =oAriaApplication.RemoteCompanyData.RollBackTran(lcTranCode)
    SELECT STYINVJL
    =TABLEREVERT(.T.)
  ENDIF

ELSE
  
  =gfModalGen('INM00000B00000','','','',"No lines added.")
  
ENDIF

*:**************************************************************************
*:* Name        : lfUpdCLrDt
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 09/17/2007
*:* Purpose     : Update color data
*:***************************************************************************
FUNCTION lfUpdCLrDt
PRIVATE laItemStru,lnCount
DIMENSION laItemStru[1]
=gfItemMask(@laItemStru)
FOR lnCount = 1 TO ALEN(laItemStru,1)
  IF laItemStru[lnCount,1]='C'
    lnClrPos = laItemStru[lnCount,4]
    lnClrLen = LEN(laItemStru[lnCount,3])
    EXIT
  ENDIF
ENDFOR

*-- end of lfUpdCLrDt.

*:**************************************************************************
*:* Name        : lfOpenFiles
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 09/17/2007
*:* Purpose     : open needed files
*:***************************************************************************
FUNCTION lfOpenFiles

WAIT WINDOW NOWAIT 'Opening files ...'

=gfOpenTable(oAriaApplication.DataDir+'STYINVJL','MFGOPR','SH')
=gfOpenTable(oAriaApplication.DataDir+'STYLE','STYLE','SH')
=gfOpenTable(oAriaApplication.DataDir+'POWLSTR','POWLSTR','SH')
=gfOpenTable(oAriaApplication.DataDir+'SCALE','SCALE','SH')
=gfOpenTable(oAriaApplication.DataDir+'WAREHOUS','WAREHOUS','SH')
=gfOpenTable(oAriaApplication.DataDir+'CUTPICK','CUTPICK','SH')
=gfOpenTable(oAriaApplication.DataDir+'ORDLINE','ORDLINST','SH')
=gfOpenTable(oAriaApplication.DataDir+'INVHDR','INVHDR','SH')
=gfOpenTable(oAriaApplication.DataDir+'INVLINE','INVLINE','SH')
=gfOpenTable(oAriaApplication.DataDir+'RETHDR','RETHDR','SH')
=gfOpenTable(oAriaApplication.DataDir+'POSHDR','POSHDR','SH')
=gfOpenTable(oAriaApplication.DataDir+'RETLINE','RETLINE','SH')
=gfOpenTable(oAriaApplication.DataDir+'ORDHDR','ORDHDR','SH')

WAIT CLEAR
*-- end of lfOpenFiles.