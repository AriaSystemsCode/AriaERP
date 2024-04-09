*:***************************************************************************
*: Program file  : SOANLSIS
*: Program desc. : Custom Sales Order Analysis Report for Alison Hayes
*: For Report    : (SOANLSIS.FRX)
*: System        : Aria Advantage Series.
*: Module        : Sales Order (SO)
*: Developer     : RANIA ABDEL RAZIK (RAE)
*:***************************************************************************
*: Calls : 
*:    Functions  : gfDispRe()
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example : DO SOANLSIS
*:***************************************************************************
*: C#200409
*:***************************************************************************
*: Modifications ..
*:***************************************************************************

STORE ''  TO lcActAlias
STORE {}  TO ldStrDat   , ldEndDat
STORE .F. TO llSelByAcc
DIMENSION laOldValue[1]

*-- llFrstTim to indicate the first time of running the Option Grid.
llFrstTim = IIF(TYPE('llFrstTim') = "U" , .T. , llFrstTim)

=lfUpdFltVar()
=lfvCrTemp()
=lfCollData()

SELECT (lcTempFile)
SET FILTER TO nTotQty != 0
LOCATE
IF RECCOUNT() = 0 .OR. EOF()
  *---Text : 'No Record Selected for the report..!'
  =gfModalGen('TRM00052B00000','DIALOG')
  SET DEVICE TO SCREEN
  RETURN
ELSE
  LOCATE
  DO gfDispRe WITH EVAL('lcRpForm')
ENDIF

*!*************************************************************
*! Name      : lfCustBrw
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 09/12/2002
*! Purpose   : To set relation on or off when running the in 
*!             range function in the option grid.
*!*************************************************************
*! Called from : OPTION GRID
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : lcParm
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfCustBrw()
*!*************************************************************

FUNCTION lfCustBrw

PARAMETERS lcParm
IF lcParm = 'S'  && Set code
  SELECT CUSTOMER
  LOCATE
ENDIF

*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 09/12/2002
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : OPTION GRID
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************

FUNCTION lfwRepWhen

IF llFrstTim
  DECLARE laRpSource[3]
  DECLARE laRpTarget[1]  
  STORE 'Open'      TO laRpSource[1]
  STORE 'Complete'  TO laRpSource[2]
  *200409,3
  *STORE 'Canceled' TO laRpSource[3]
  STORE 'Cancelled' TO laRpSource[3]
  *200409,3
ELSE
  DECLARE laRpSource[3]
  DECLARE laRpTarget[ALEN(laRpTarget,1)]
  FOR lnI = 1 TO ALEN(laRpTarget,1)
    laRpTarget[lnI] = laOldValue[lnI]
  ENDFOR
ENDIF

DECLARE laCurrVal[1,1]
IF !USED('SYCCURR')
  =gfOpenFile(gcSysHome+'SYCCURR',gcSysHome+'Ccurrcode','SH')
ENDIF
SELECT DISTINCT CCURRCODE FROM SYCCURR ORDER BY CCURRCODE INTO ARRAY laCurrVal
DIMENSION laCurrDesc[ALEN(laCurrVal,1),1],laCurrSmbl[ALEN(laCurrVal,1),1]
SELECT SYCCURR
SET ORDER TO CCURRCODE  && To VALIDATE currency code.
FOR lnI = 1 TO ALEN(laCurrVal,1)
  = SEEK(ALLTRIM(laCurrVal[lnI,1]))
  laCurrVal[lnI,1]  = PADR(laCurrVal[lnI,1],3)
  laCurrSmbl[lnI,1] = ALLTRIM(PADR(CCURRSMBL,3))
  laCurrDesc[lnI,1] = CCURRCODE + ' - ' + ALLTRIM(CCURRDESC)
ENDFOR


*!**************************************************************************
*! Name      : lfVarPos
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 09/12/2002
*! Purpose   : To get the position of the Variable in OG.
*!**************************************************************************
*! Called from : OPTION GRID
*!**************************************************************************
*! Example     : = lfVarPos()
*!**************************************************************************

FUNCTION lfVarPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos
lnItmPos = ASCAN(laOGObjType,lcItmInFlt)
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(laOGObjType,lnItmPos,1)
ENDIF
RETURN lnItmPos

*!*************************************************************
*! Name      : lfvCurDisp
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 09/12/2002
*! Purpose   : Valid function to display the Currency.
*!*************************************************************
*! Called from  : OPTION GRID
*!*************************************************************
*! Example      : =lfvCurDisp()
*!*************************************************************

FUNCTION lfvCurDisp

llRpProced = gfRepCur(.T., @lcRpCurr,@ldRpExDate,lcRpTmpNam)

*!*************************************************************
*! Name      : lfvOStatus
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 09/12/2002
*! Purpose   : Evaluate Status expression.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Called from : OPTION GRID
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvOStatus()
*!*************************************************************

FUNCTION lfvOStatus
PRIVATE lcOldStat,lcCurrChr

lcOldStat = lcRpStatus  && Save old status value.

= gfMover(@laRpSource,@laRpTarget,'Select Order Status',.T.,'')  && call mover function.

lcRpStatus = ' '
*-- Loop to make Status expression.
IF !EMPTY(laRpTarget[1])
  FOR lnI = 1 TO ALEN(laRpTarget,1)
    lcRpStatus = lcRpStatus + IIF(laRpTarget[lnI] = 'Open','O',;
                              IIF(laRpTarget[lnI] = 'Complete','C',;                                                            
                              IIF(laRpTarget[lnI] = 'Cancelled','X','')))
  ENDFOR
ENDIF

*-- if empty of status is like select all available values.
lcRpStatus = IIF(EMPTY(lcRpStatus),'OCX',ALLTRIM(lcRpStatus))

DIMENSION laOldValue[ALEN(laRpTarget,1)]
FOR lnI = 1 TO ALEN(laRpTarget,1)
  laOldValue[lnI] = laRpTarget[lnI]
ENDFOR  

*!*************************************************************
*! Name      : lfvCrTemp
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 09/12/2002
*! Purpose   : Create Temp File.
*!*************************************************************
*! Calls     : NONE
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example            : =lfvCrTemp()
*!*************************************************************

FUNCTION lfvCrTemp

*-- Create temporary file to collect the data.

CREATE TABLE (gcWorkDir+lcTempFile) (cOrder C(6)    , cAccount C(5) , cStyNo C(19)  ,;
                                     cLineNo C(19)  , dDueDate D(8) , nTotQty N(7)  ,;
                                     nPrice N(12,2) , nAmount N(12,2))
                                     
SELECT (lcTempFile)
*-- If sort by Order #
IF lcRpSortBy = 'O'
  *INDEX ON cOrder + cAccount TAG (lcTempFile)
  INDEX ON cAccount + cOrder TAG (lcTempFile)
ELSE
  *-- If sort by Complete Date
  INDEX ON cAccount + DTOS(dDueDate) TAG (lcTempFile)
ENDIF

*!*************************************************************
*! Name      : lfUpdFltVar
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 09/12/2002
*! Purpose   : Get the values of filter Exp. variables.
*!*************************************************************
*! Calls     : NONE
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example            : =lfUpdFltVar()
*!*************************************************************

FUNCTION lfUpdFltVar

FOR lnInd = 1 TO ALEN(laOgFxFlt,1)
  DO CASE
    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'ORDHDR.ACCOUNT' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      *--- Account Code Range
      lcActAlias = laOgFxFlt[lnInd,6]
      llSelByAcc = (USED(lcActAlias) .AND. RECCOUNT(lcActAlias) <> 0)

    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'ORDLINE.COMPLETE' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      *--- Due Date Range
      ldStrDat = CTOD(ALLTRIM(SUBSTR(laOgFxFlt[lnInd,6],1,10)))
      ldEndDat = CTOD(ALLTRIM(SUBSTR(laOgFxFlt[lnInd,6],12,20)))
    *200409,3
    *CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'ORDHDR.CCURRCODE' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
    *  *--- Currency Code
    *  lcCurrCode = laOgFxFlt[lnInd,6]
    *200409,3
  ENDCASE
ENDFOR

*!*************************************************************
*! Name      : lfCollData
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 09/12/2002
*! Purpose   : Collect the data.
*!*************************************************************
*! Calls     : NONE
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example            : =lfCollData()
*!*************************************************************

FUNCTION lfCollData

PRIVATE llCallGfAm
*200409,3
*llCallGfam = EMPTY(lcCurrCode) .OR. (!EMPTY(lcCurrCode) .AND. lcRpCurr <> 'F')
llCallGfam = lcRpCurr <> 'F'
*200409,3
*-- If Account is selected
IF llSelByAcc
  SELECT (lcActAlias)
  SCAN
    m.cAccount = ACCOUNT
    SELECT ORDHDR
    SCAN FOR account+status+bulk+cordtype+order = m.cAccount .AND. ;
             IIF(!EMPTY(lcRpStatus),Status $ lcRpStatus,!(Status $ "BH"))
      m.cOrder = ORDHDR.ORDER
      SELECT ORDLINE
      SCAN FOR cordtype+order+STR(lineno,6) = 'O' + m.cOrder .AND. ;
               BETWEEN(DTOS(ORDLINE.COMPLETE),ALLTRIM(DTOS(ldStrDat)),ALLTRIM(DTOS(ldEndDat)))
        WAIT WINDOW "Collecting data for Order# : " + m.cOrder NOWAIT
        m.dDueDate = ORDLINE.COMPLETE
        m.cStyNo   = ORDLINE.STYLE
        m.nPrice   = ORDLINE.GROS_PRICE
        m.nTotQty  = IIF(lcRpOrdQty = 'O',ORDLINE.TOTQTY,ORDLINE.TOTBOOK)
        SELECT ORDHDR
        m.nAmount = m.nTotQty * IIF(llCallGfam,gfAmntDisp(m.nPrice,lcRpCurr,ldRpExDate,lcRpTmpNam),m.nPrice)
        SELECT ORDLINE
        m.nPrice  = IIF(m.nTotQty = 0,ORDLINE.GROS_PRICE,m.nAmount / m.nTotQty)
        =SEEK(m.cStyNo,'STYLE')
        m.cLineNo = STYLE.CVENSTY
        INSERT INTO (lcTempFile) FROM MEMVAR
      ENDSCAN
    ENDSCAN
  ENDSCAN
ELSE
  SELECT ORDHDR
  SCAN FOR cordtype+order = 'O' .AND. ;
           IIF(!EMPTY(lcRpStatus),Status $lcRpStatus,!(Status $ "BH"))

    m.cOrder = ORDHDR.ORDER
    SELECT ORDLINE
      SCAN FOR cordtype+order+STR(lineno,6) = 'O' + m.cOrder .AND. ;
               BETWEEN(DTOS(ORDLINE.COMPLETE),ALLTRIM(DTOS(ldStrDat)),ALLTRIM(DTOS(ldEndDat)))
        WAIT WINDOW "Collecting data for Order# : " + m.cOrder NOWAIT
        m.cAccount = ORDLINE.ACCOUNT
        m.dDueDate = ORDLINE.COMPLETE
        m.cStyNo   = ORDLINE.STYLE
        m.nPrice   = ORDLINE.GROS_PRICE
        m.nTotQty  = IIF(lcRpOrdQty = 'O' ,ORDLINE.TOTQTY,ORDLINE.TOTBOOK)
        SELECT ORDHDR
        m.nAmount = m.nTotQty * IIF(llCallGfam,gfAmntDisp(m.nPrice,lcRpCurr,ldRpExDate,lcRpTmpNam),m.nPrice)
        SELECT ORDLINE
        m.nPrice  = IIF(m.nTotQty = 0 ,ORDLINE.GROS_PRICE, m.nAmount / m.nTotQty)
        =SEEK(m.cStyNo,'STYLE')
        m.cLineNo = STYLE.CVENSTY
        INSERT INTO (lcTempFile) FROM MEMVAR
      ENDSCAN
  ENDSCAN
ENDIF
