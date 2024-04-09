*:***************************************************************************
*: Program file : ICSPRUN2.PRG
*: SYSTEM       : ARIA27 ADVANTAGE SERIES
*: MODULE       : IC (Inventory Control module)
*: Developer    : TMI - Tarek Mohamed Ibrahim
*: Porpous      : Running the Crystal Report for Inventory availability by style reports for GMA
*: Date         : 07/01/2001
*: Note         :This program runs the form named frmIcsprun that calls the Crystal Report.
*:***************************************************************************
PARAMETERS lcMemFile

*--Hide the VFP screen
_SCREEN.VISIBLE = .F.

*C102224,4 TMI [Start] Call the error handler
lcErrHand = ON('ERROR')
*ON ERROR DO lfErrHandler
*C102224,4 TMI [End  ]
IF VARTYPE(lcMemFile)#'C'
  RETURN
ENDIF
RESTORE FROM (lcMemFile) ADDI

lnRecCount = 0
DO FORM FRMICSPRUN.SCX
*--Suppress if output is to screen only
IF lnOutput = 0 AND lnRecCount > 0
  READ EVENTS
ENDIF

ON ERROR &lcErrHand


*:**************************************************************************
*:* Name        : lfErrHandler
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 07/24/01
*:* Purpose     : Local error handler for the project
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfErrHandler()
*:***************************************************************************
FUNCTION lfErrHandler
LOCAL cOK,cSTOP,laERRS
lnErr = ERROR()
cOK   = 0
cOkCancel = 1
cSTOP =16
*--Define and initialize error array
DIME laERRS[1]
laERRS = ''
AERROR(laERRS)

DO CASE
  *-- if this is an OLE error
  CASE INLIST(lnErr ,1427,1429)                 
    lnResponse = MESSAGEBOX(laERRS[2]+CHR(13)+laERRS[3],cOkCancel+cSTOP,'Aria Advantage Series')
    *-- if there is en OLE error don't continue , go to solve this error
    IF lnResponse = 2 && Cancel button
      CLEAR EVENTS
      QUIT
    ENDIF     
  OTHERWISE
    MESSAGEBOX(STR(lnErr)+CHR(13)+;
    laERRS[2],cOK+cSTOP,'Aria Advantage Series')
ENDCASE
*-- end of lfErrHandler.

*:**************************************************************************
*:* Name        : lfCrit
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 07/22/01
*:* Purpose     : Build the Criteria to filter SQL Server data.
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfCrit()
*:***************************************************************************
FUNCTION lfCrit
PRIVATE lcRet
lcRpExp = STRTRAN(lcRpExp,["],['])

lcRet = "ROWTYPE<>'2'"
lcRet = lcRet + lfFxFlt('STYLE.STYLE','STYLECOLOR',12)
lcRet = lcRet + lfFxFlt('STYLE.CSTYGROUP','CSTYGROUP',16)
lcRet = lcRet + lfUpdCrit('laRpTDep'  ,'DEPT'      ,6)
*C102224,1 TMI [Start] Error in fields names 
*lcRet = lcRet + lfUpdCrit('laRpTCatg' ,'CSTYCATG'  ,6)
*lcRet = lcRet + lfUpdCrit('laRpTClass','CSTYCLASS' ,6)
*lcRet = lcRet + lfUpdCrit('laRpTSbCls','CSTYSBCLAS',6)
lcRet = lcRet + lfUpdCrit('laRpTCatg' ,'CCATG'     ,6)
lcRet = lcRet + lfUpdCrit('laRpTClass','CCLASS'    ,6)
lcRet = lcRet + lfUpdCrit('laRpTSbCls','CSUB_CLASS',6)
*C102224,1 TMI [End  ]

IF !EMPTY(lcRet)
  lcRet = ' WHERE ' + lcRet
ENDIF  

lcRet = lcRet + ' ORDER BY DEPT,CSTYGROUP,CCATG,CCLASS,CSUB_CLASS,STYLE,COLOR,ROWTYPE'
RETURN lcRet
*-- end of lfCrit.

*:**************************************************************************
*:* Name        : lfUpdCrit
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 07/24/01
*:* Purpose     : Update Criteria from passed arrays
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfUpdCrit()
*:***************************************************************************
FUNCTION lfUpdCrit
PARAMETERS lcArrName,lcFld,lnWidth
LOCAL lcExpr,lnCnt,lcRetUpd
STORE '' TO lcExpr,lcRetUpd
IF !EMPTY(&lcArrName)
  lcExpr = ''
  FOR lnCnt = 1 TO ALEN(&lcArrName,1)
    lcExpr = lcExpr + "'" + PADR(&lcArrName.[lnCnt],lnWidth) + "',"
  NEXT
  lcRetUpd = IIF(!EMPTY(lcRet),' AND ','') + ;
             lcFld + ' IN (' + LEFT(lcExpr,LEN(lcExpr)-1) + ')'
ENDIF
RETURN lcRetUpd
*-- end of lfUpdCrit.

*:**************************************************************************
*:* Name        : lfFxFlt
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 07/24/01
*:* Purpose     : Update the sql-Where expr from fixed filters on OG
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfFxFlt()
*:***************************************************************************
FUNCTION lfFxFlt
PARAMETERS lcSrhFld,lcFld,lnLen
LOCAL lcExpr,lcRetFx
STORE '' TO lcExpr,lcRetFx
IF lcSrhFld $ lcRpExp
  lcExpr = SUBSTR(lcRpExp,AT(lcSrhFld,lcRpExp)+lnLen)
  lcExpr = lcFld + ' IN (' + LEFT(lcExpr,AT(')',lcExpr ))
  lcRetFx = IIF(!EMPTY(lcRet),' AND ','') + lcExpr 
ENDIF
RETURN lcRetFx
*-- end of lfFxFlt.