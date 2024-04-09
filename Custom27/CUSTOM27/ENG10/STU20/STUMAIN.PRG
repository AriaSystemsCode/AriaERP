*!**************************************************************************
*! Name      : STUMAIN.PRG
*! Developer : Ahmed Maher (AMH)
*! Date      : 07/11/2001
*! Purpose   : STU Custom Process Program .
*!**************************************************************************
*! Parameters: lcEvntFun -> Process event function name without 'lf..'  .
*!             lcFunPars -> Process function parameters, sent as a string.
*!**************************************************************************
*! Returns   : Logical value.
*!**************************************************************************
*!Modifications:
*!C200339,1 TMI Check if the sales rep assigned on the divisoin before generate
*!C200339,1     saes order
*!B606120,1 NAD 06/13/2002 Fix bug of getting Rep code beside commision from DIVSLSRP file.
*!SSH....,4     SSH 06/13/2002 Fix bug of getting Rep code beside commision from DIVSLSRP file.
*!ASH    ,5     RM
*!B606196,1 TMI 07/02/2002 Sales rep by division to be based on stores
*!B606206,1 TMI 07/04/2002 Fix for 200339
*!**************************************************************************
PARAMETER lcEvntFun,lcFunPars

lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLTRIM(lcEvntFun)+'('+lcFunPars+')'

*-- Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue
*-- End of Program.

*!**************************************************************************
*! Name      : lfStuAdBar
*! Developer : Ahmed Maher (AMH)
*! Date      : 07/11/2001
*! Purpose   : Add a new custom Bar to option's pad in Style Purchase Order screen.
*! Reference : C200208,1
*!**************************************************************************
*! Example   : lfStuAdBar()
*!**************************************************************************
*
FUNCTION lfStuAdBar
IF !llImpCost
  DEFINE BAR 11 OF _OPTIONPOP PROMPT '\<Labelling Instructions' SKIP FOR laScrMode[1]
ELSE
  IF llDispPric
    DEFINE BAR 14 OF _OPTIONPOP PROMPT '\<Labelling Instructions' SKIP FOR laScrMode[1]
  ELSE
    DEFINE BAR 13 OF _OPTIONPOP PROMPT '\<Labelling Instructions' SKIP FOR laScrMode[1]
  ENDIF
ENDIF

IF !USED(lcTmpLblIn)
  PRIVATE lnAlias
  lnAlias = SELECT(0)
  SELECT LBLINST
  =AFIELDS(laLblStru)
  =gfCrtTmp(lcTmpLblIn,@laLblStru,'cStyType+Po',lcTmpLblIn)
  SELECT (lcTmpLblIn)
  APPEND BLANK
  SELECT (lnAlias)
ENDIF
*--end of lfStuAdBar.

*!**************************************************************************
*! Name      : lfRunLblIn
*! Developer : Ahmed Maher (AMH)
*! Date      : 07/11/2001
*! Purpose   : Run custom Labelling instructions screen from Option's pad
*!             in Style Purchase Order screen.
*! Reference : C200208,1
*!**************************************************************************
*! Example   : lfRunLblIn()
*!**************************************************************************
*
FUNCTION lfRunLblIn
PRIVATE lcPoAlias

lcPoAlias = ALIAS()
  
*-- Clear the trapped keys.
PUSH KEY
ON KEY

IF (BAR() = 10 .AND. llImpCost) .OR. (BAR() = 14 .AND. llDispPric) .OR. (BAR() = 13 .AND. !llDispPric)
  PUSH MENU _MSYSMENU
  RELEASE PAD _OPTIONPOP OF _MSYSMENU
  DO (gcAppHome+gcWinAppl+'\POLBLINS.FXP')
  POP MENU _MSYSMENU
ENDIF  
  
*-- Restore the trapped keys.
POP KEY
SELECT (lcPoAlias)
*-- End of lfRunLblIn.

*!**************************************************************************
*! Name      : lfSavLblIn
*! Developer : Ahmed Maher (AMH)
*! Date      : 07/11/2001
*! Purpose   : Save data to LBLINST file.
*! Reference : C200208,1
*!**************************************************************************
*! Example   : lfSavLblIn()
*!**************************************************************************
*
FUNCTION lfSavLblIn

PRIVATE lnAlias
lnAlias = SELECT(0)
SELECT (lcTmpLblIn)
SCATTER MEMO MEMVAR
SELECT LBLINST
IF SEEK(ALLTRIM(lcAType)+laData[1])
  GATHER MEMO MEMVAR
ELSE
  APPEND BLANK
  GATHER MEMO MEMVAR
  REPLACE CSTYTYPE WITH ALLTRIM(lcAType),;
          PO       WITH laData[1]
ENDIF
SELECT (lnAlias)
*-- End of lfSavLblIn.

*!**************************************************************************
*! Name      : lfGetLblIn
*! Developer : Ahmed Maher (AMH)
*! Date      : 07/11/2001
*! Purpose   : Get data From LBLINST file.
*! Reference : C200208,1
*!**************************************************************************
*! Example   : lfGetLblIn()
*!**************************************************************************
*
FUNCTION lfGetLblIn

PRIVATE lnAlias
lnAlias = SELECT(0)
SELECT LBLINST
IF laScrMode[2]
  SEEK ALLTRIM(lcAType)+laData[1]
  SCATTER MEMO MEMVAR
ELSE
  SCATTER MEMO MEMVAR BLANK
ENDIF
SELECT (lcTmpLblIn)
GATHER MEMO MEMVAR
SELECT (lnAlias)
*-- End of lfGetLblIn.

*:**************************************************************************
*:* Name        : lfSalesRep
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 05/08/2002
*:* Purpose     : Open Division/sales rep screen
*:***************************************************************************
*:* Called from : Arcust.prg 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfSalesRep()
*:* Refer To    : C200339,1 
*:***************************************************************************
FUNCTION lfSalesRep
PRIVATE lcAlias,lcWinCh0,lcWinCh1,lcWinCh2,lnI

lcAlias = ALIAS()
lcWinCh0    = gftempname()     &&Temp name for Windows.
lcWinCh1    = gftempname()     &&Temp name for Windows.
lcWinCh2    = gftempname()     &&Temp name for Windows.

PUSH KEY
 
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcDet_Ttl)
ON KEY LABEL TAB   DO lptab 

=gfwCodePop(@laCodes,'CDIVISION','L')

lcObjStat  = IIF(laScrMode[1] .OR. laScrMode[2],"DISABLE","ENABLE")

lcDet_Ttl = "Sales Representatives."
lnDiv = lnDivision-1
*lcDivSlsRp

=lfOpnFls()

DIMENSION laDivs[ALEN(laDivision,1)-1,2]
FOR lnI=1 TO ALEN(laDivision,1)-1
  laDivs[lnI,1] = laDivision[lnI+1,1]
  laDivs[lnI,2] = laDivision[lnI+1,2]
ENDFOR
IF !USED('DIVSLSRP')
  =gfOpenFile(gcDataDir+'DIVSLSRP','DIVSLSRP','SH')
ENDIF

SELECT IIF(laScrMode[2],'DIVSLSRP',(lcDivSlsRp))
*B606196,1 TMI [Start] Add store to the filter Key 
*SET FILTER TO ACCOUNT+STORE+CDIVISION+REPCODE = laData[2]+laDivs[lnDiv,2] &&*tmi
SET FILTER TO ACCOUNT+STORE+CDIVISION+REPCODE = laData[2]+PADR(laData[3],8)+laDivs[lnDiv,2]
*B606196,1 TMI [End  ] 
SET RELATION TO REPCODE INTO SALESREP
LOCATE
lnDMarker = 1
DO (gcScrDir+"SO\SODIVSR.SPX")

POP KEY
SELECT (lcAlias)
*-- end of lfSalesRep.

*:**************************************************************************
*:* Name        : lfvAddSlRp
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 05/13/02
*:* Purpose     : Add Sales rep
*:***************************************************************************
*:* Called from : SODIVSLSRP.SCX
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = vAddSlRp()
*:* Refer To    : C200339,1 
*:***************************************************************************
FUNCTION lfvAddSlRp
PRIVATE lcSales

GO TOP IN 'SalesRep'
lcSales = SPACE(3)
=REPCHK(@lcSales)
*B606196,1 TMI [Start]  Add store to the seek Key 
*IF !EMPTY(lcSales) AND !SEEK(laData[2]+laDivs[lnDiv,2]+lcSales,(lcDivSlsRp))  &&*tmi
*  INSERT INTO (lcDivSlsRp) (ACCOUNT,CDIVISION,REPCODE) ;
                    VALUES (laData[2],laDivs[lnDiv,2],lcSales)    &&*tmi
IF !EMPTY(lcSales) AND !SEEK(laData[2]+PADR(laData[3],8)+laDivs[lnDiv,2]+lcSales,(lcDivSlsRp))
  INSERT INTO (lcDivSlsRp) (ACCOUNT,STORE,CDIVISION,REPCODE) ;
                    VALUES (laData[2],laData[3],laDivs[lnDiv,2],lcSales)  
  *B606196,1 TMI [End  ] 
  IF !USED('REP_DIV')
    =gfOpenFile(gcDataDir+'REP_DIV','REP_DIV','SH')
  ENDIF
  IF gfGetMemVar('M_REP_COMM') = 'D'
    IF SEEK(lcSales+laDivs[lnDiv,2],'REP_DIV')
      REPLACE &lcDivSlsRp..COMM_RATE WITH REP_DIV.COMM_RATE
    ENDIF
  ELSE
    REPLACE &lcDivSlsRp..COMM_RATE WITH SALESREP.COMM 
  ENDIF
  =lfBrowLine(RECNO(lcDivSlsRp))
  
ENDIF
*-- end of lfvAddSlRp.

*:**************************************************************************
*:* Name        : lfvRmvSlRp
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 05/13/02
*:* Purpose     : Remove Sales Rep
*:***************************************************************************
*:* Called from : SODIVSLSRP.SCX
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = vRmvSlRp()
*:* Refer To    : C200339,1 
*:***************************************************************************
FUNCTION lfvRmvSlRp
PRIVATE lnRecno,llBOf

SELECT (lcDivSlsRp)
*-- Are you sure you want to delete this line
lnRecno = 0
IF gfModalGen('QRM42164B32000') = 1
  IF !BOF()
    SKIP -1
  ENDIF
  IF BOF()
    lnRecno = 0
  ELSE
    lnRecno = RECNO(lcDivSlsRp)    
    IF !EOF()
      SKIP  
    ENDIF
  ENDIF
  DELETE
  =lfBrowLine(lnRecno)
ENDIF
*-- end of lfvRmvSlRp.

*:**************************************************************************
*:* Name        : lpTab
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 05/13/02
*:* Purpose     : Trap Key Fn
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lpTab()
*:* Refer To    : C200339,1 
*:***************************************************************************
PROCEDURE lpTab
IF WONTOP() = lcDet_Ttl
  ACTIVATE WINDOW (lcWinCh1)
*  _CUROBJ=OBJNUM(ibTabs)
  _CUROBJ=OBJNUM(pbExit)
ELSE
  _CUROBJ=_CUROBJ+1
ENDIF

*:**************************************************************************
*:* Name        : lfBrowLine
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 05/13/02
*:* Purpose     : Browse fn
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfBrowLine()
*:* Refer To    : C200339,1 
*:***************************************************************************
FUNCTION lfBrowLine
PARAMETERS lnRecNo
lnAlias = SELECT()
IF !USED('DIVSLSRP')
  =gfOpenFile(gcDataDir+'DIVSLSRP','DIVSLSRP','SH')
ENDIF

SELECT IIF(laScrMode[2],'DIVSLSRP',(lcDivSlsRp))
GO TOP
IF laScrMode[3] 
  lcRemStat = IIF(EOF(),'DISABLE','ENABLE')   && if no lines , disabel remove button
  SHOW GET pblRemov &lcRemStat
ENDIF
lcRW = IIF(laScrMode[2],':R','')
lnDMarker  = RECNO()
lcBrowFlds = [cMarker =IIF(RECNO()=lnDMarker ,'>',' '):H=' ':R:1:W=.F.,]+;
             [CDIVISION          :R :H='Division'       :W=.F.:10,]+; 
             [REPCODE            :R :H='Rep Code'       :W=.F.:15,]+;
             [SALESREP.NAME      :R :H='Sales Rep Name' :W=.F.:30,]+;
             [COMM_RATE       &lcRW :H='Default Comm'   :18]

**           [SALESREP.DISCNT1   :R :H='Discnt1'        :10,]+;
             [SALESREP.REDCOM1   :R :H='Reduce'         :10,]+;
             [SALESREP.DISCNT2   :R :H='Discnt2'        :10,]+;
             [SALESREP.REDCOM2   :R :H='Reduce'         :10,]+;
             [SALESREP.DISCNT3   :R :H='Discnt3'        :10,]+;   
             [SALESREP.REDCOM3   :R :H='Reduce'         :10,]+;   
             [SALESREP.DISCNT4   :R :H='Discnt4'        :10,]+;   
             [SALESREP.REDCOM4   :R :H='Reduce'         :10 ]

BROWSE FIELDS &lcBrowFlds;
       NOAPPEND ;
       NOCLEAR  ;
       NODELETE ;
       NOMENU   ;
       NOWAIT   ;
       SAVE     ;
       VALID :F lfvBrows();       
       WHEN lfwBrowUp();
       TITLE lcDet_Ttl ;
       WINDOW (lcWinCh2) IN WINDOW (lcWinCh0)

IF !EMPTY(lnRecNo)
  IF BETWEEN(lnRecNo,1,RECCOUNT())
    GOTO (lnRecNo)
  ENDIF
ELSE
  GO TOP
ENDIF
lnDMarker = RECNO()

=lfRefresh(lcWinCh2)
=lfRefresh(lcDet_Ttl)

SELECT(lnAlias)

*:**************************************************************************
*:* Name        : lfvBrows
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 05/13/02
*:* Purpose     : 
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvBrows()
*:* Refer To    : C200339,1 
*:***************************************************************************
FUNCTION lfvBrows
IF WONTOP() # (lcDet_Ttl)
  = gfStopBrow()
ENDIF

*:**************************************************************************
*:* Name        : lfwBrowUp
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 05/13/02
*:* Purpose     : BrowUp
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfwBrowUp()
*:* Refer To    : C200339,1 
*:***************************************************************************
FUNCTION lfwBrowUp
lnDMarker = IIF(laScrMode[2],RECNO('DIVSLSRP'),RECNO(lcDivSlsRp))
SHOW WINDOW (lcDet_Ttl) REFRESH

*:**************************************************************************
*:* Name        : lfvDiv
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 05/13/02
*:* Purpose     : Show different Sales reps for when changing division
*:***************************************************************************
*:* Called from : divslsrp.spx
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvDiv()
*:* Refer To    : C200339,1 
*:***************************************************************************
FUNCTION lfvDiv
PRIVATE lcAlias,lcInsStat
lcAlias = ALIAS()
IF !USED('DIVSLSRP')
  =gfOpenFile(gcDataDir+'DIVSLSRP','DIVSLSRP','SH')
ENDIF

SELECT IIF(laScrMode[2],'DIVSLSRP',(lcDivSlsRp))
*SET FILTER TO ACCOUNT+STORE+CDIVISION+REPCODE = laData[2]+laDivs[lnDiv,2]  */tmi
SET FILTER TO ACCOUNT+STORE+CDIVISION+REPCODE = laData[2]+PADR(laData[3],8)+laDivs[lnDiv,2]
GO TOP
=lfBrowLine()
SELECT (lcAlias)
*-- end of lfvDiv.

*:**************************************************************************
*:* Name        : lfUpdate
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 05/13/02
*:* Purpose     : Update data in file DIVSLSRP
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfUpdate()
*:* Refer To    : C200339,1 
*:***************************************************************************
FUNCTION lfUpdate
PRIVATE lcAlias,lcDelset

IF !USED(lcDivSlsRp)
  SELECT CUSTOMER
  REPLACE lLok_stat WITH .F.,;
          CLOK_TIME WITH '' ,; 
          CLOK_USER WITH '' ,;        
          DLOK_DATE WITH {}
  RETURN
ENDIF

lcAlias = ALIAS()
lcDelset = SET('DELETED')
SET DELETED OFF
SELECT (lcDivSlsRp)
SET FILTER TO 
GO TOP
REPLACE ALL LMARK WITH DELETED()
GO TOP
RECALL ALL 
SET DELETED &lcDelset

LOCATE
SCAN
*B606196,1 TMI [Start]  Add store to the seek Key 
*  IF !SEEK(&lcDivSlsRp..Account+&lcDivSlsRp..CDIVISION+&lcDivSlsRp..REPCODE,'DIVSLSRP')  &&*tmi
*    INSERT INTO DIVSLSRP (ACCOUNT,CDIVISION,REPCODE,COMM_RATE) ;
                VALUES   (laData[2],&lcDivSlsRp..CDIVISION,&lcDivSlsRp..REPCODE,&lcDivSlsRp..COMM_RATE)  &&*tmi
  IF !SEEK(&lcDivSlsRp..Account+&lcDivSlsRp..STORE+&lcDivSlsRp..CDIVISION+&lcDivSlsRp..REPCODE,'DIVSLSRP')
    INSERT INTO DIVSLSRP (ACCOUNT,STORE,CDIVISION,REPCODE,COMM_RATE) ;
                VALUES   (laData[2],laData[3],&lcDivSlsRp..CDIVISION,&lcDivSlsRp..REPCODE,&lcDivSlsRp..COMM_RATE)
  *B606196,1 TMI [End  ]                 
  ELSE
    IF LMARK
      SELECT DIVSLSRP 
      DELETE
    ELSE
      IF DIVSLSRP.COMM_RATE # &lcDivSlsRp..COMM_RATE
        REPLACE DIVSLSRP.COMM_RATE WITH &lcDivSlsRp..COMM_RATE
      ENDIF
    ENDIF
  ENDIF               
ENDSCAN

IF USED(lcDivSlsRp) 
  USE IN (lcDivSlsRp) 
ENDIF

SELECT CUSTOMER
REPLACE lLok_stat WITH .F.,;
        CLOK_TIME WITH '',; 
        CLOK_USER WITH '',;        
        DLOK_DATE WITH {}

SELECT (lcAlias)
*-- end of lfUpdate.
*:**************************************************************************
*:* Name        : lfLoadSR
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 05/14/2002
*:* Purpose     : Load Sales rep data
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfLoadSR()
*:* Refer To    : C200339,1 
*:***************************************************************************
FUNCTION lfLoadSR
PRIVATE lcAlias,lcDiv,lcVar,lnRec
lcAlias = ALIAS()

laData[27] = ""
laData[28] = 0
laData[29] = ""
laData[30] = 0
IF !USED('DIVSLSRP')
  =gfOpenFile(gcDataDir+'DIVSLSRP','DIVSLSRP','SH')
ENDIF
SELECT DIVSLSRP
lcDiv = IIF(EMPTY(laData[15]),PADR(lcODefDiv,6),laData[15])
*B606196,1 TMI [Start]  Add store to the seek Key 
*IF !SEEK(laData[2],'DIVSLSRP') &&*tmi
IF !SEEK(laData[2]+IIF(!EMPTY(laData[3]),PADR(laData[3],8),''),'DIVSLSRP')
  *B606196,1 TMI [End  ] 
  =gfModalGen('INM00000B00000','DIALOG','','', ; 
  'No division assigned to the customer/store ' + laData[2]+IIF(!EMPTY(laData[3]),'/'+laData[3],'') +;
                                                   ', please contact your system administrator.')
  *B606196,1 TMI [Start]
  *laData[2] = ''      &&*tmi
  lcVar = IIF(EMPTY(laData[3]),'laData[2]','laData[3]')
  &lcVar = lcOldValue
  SHOW GET laData[2]
  SHOW GET laData[3]
  *B606196,1 TMI [End  ]
  RETURN .F.
ENDIF
llContino = .T.
*B606196,1 TMI [Start]  Add store to the seek Key 
*IF !SEEK(laData[2]+lcDiv,'DIVSLSRP')  &&*tmi
IF !SEEK(laData[2]+PADR(laData[3],8)+lcDiv,'DIVSLSRP')
  *B606196,1 TMI [End  ] 
  *=gfModalGen('INM00000B00000','DIALOG','','', ;  
  'The Sales Rep ' + laData[lnSlsRep] + ' is not assigned to the division ' + lcDiv + ;
  ' for the selected customer - please contact your system administrator.')  
  
  =gfModalGen('INM00000B00000','DIALOG','','', ;
  'No Sales Rep assigned to the division ' + lcDiv + ;
  ' for the selected customer/store - please contact your system administrator.')

  *B606206,1 TMI [Start]   *--If there is lines in lcOrdLine , exit
  lnRec = RECNO(lcOrdLine)
  GO TOP IN (lcOrdLine)
  IF !EOF(lcOrdLine)
    laData[3] = lcOldValue
    SELECT (lcOrdLine)
    GO TOP
    REPLACE ALL COMM1 WITH 0,COMM2 WITH 0
    IF BETWEEN(lnRec,1,RECCOUNT(lcOrdLine))
      GOTO (lnRec)
    ENDIF    
    llContino = .F.
    SELECT (lcAlias)
    RETURN .F.
  ENDIF
  IF BETWEEN(lnRec,1,RECCOUNT(lcOrdLine))
    GOTO (lnRec)
  ENDIF
  *B606206,1 TMI [End  ] 

  laData[lnSlsRep  ] = '   '
  laData[lnSlsRep+1] = 0
  laData[27] = ""
  laData[28] = 0
  laData[29] = ""
  laData[30] = 0
  *B606196,1 TMI [Start]  Add store to the seek Key 
  *=SEEK(laData[2],'DIVSLSRP') &&*tmi
  =SEEK(laData[2]+IIF(!EMPTY(laData[3]),PADR(laData[3],8),''),'DIVSLSRP')
  *B606196,1 TMI [End  ] 
  lcDiv = DIVSLSRP.cdivision
  laData[15] = DIVSLSRP.cdivision
  IF ASCAN('laDivision',laData[15]) > 0
    lnDivision = ASUBSCRIPT['laDivision',ASCAN('laDivision',laData[15]),1]  
  ELSE
    = gfwCodePop(@laCodes , "CDIVISION" , "V,"+laData[15])
  ENDIF
  SHOW GET lnDivision  &&TMI
  SHOW GET laData[15]  &&TMI
ENDIF

lnCount = 0
*B606196,1 TMI [Start]  Add store to the seek Key 
*SCAN REST WHILE account+cdivision+repcode = laData[2]+lcDiv .AND. lnCount < 2   &&*tmi
SCAN REST WHILE account+store+cdivision+repcode = laData[2]+PADR(laData[3],8)+lcDiv .AND. lnCount < 2
  *B606196,1 TMI [End  ] 
  lnCount = lnCount +1
  IF lnCount = 1
    laData[27] = DIVSLSRP.RepCode
    laData[28] = DIVSLSRP.COMM_RATE
  ELSE  
    laData[29] = DIVSLSRP.RepCode
    laData[30] = DIVSLSRP.COMM_RATE
  ENDIF
ENDSCAN

SHOW GET laData[27]
SHOW GET laData[28]
SHOW GET laData[29]
SHOW GET laData[30]
SHOW GET laData[15]
SELECT (lcAlias)
*-- end of lfLoadSR.

*:**************************************************************************
*:* Name        : lfChkRep1
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 05/15/2002
*:* Purpose     : Check Rep1
*:***************************************************************************
*:* Called from : soord ( lfvDept )
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfChkRep1()
*:* Refer To    : C200339,1 
*:***************************************************************************
FUNCTION lfChkRep1
PRIVATE lcRep

IF !EMPTY(laData[27])
  IF !USED('DIVSLSRP')
    =gfOpenFile(gcDataDir+'DIVSLSRP','DIVSLSRP','SH')
  ENDIF
  *B606196,1 TMI [Start]  Add store to the seek Key 
*  IF !SEEK(laData[2]+PADR(laData[15],6)+laData[27],'DIVSLSRP') &&*tmi
  IF !SEEK(laData[2]+PADR(laData[3],8)+PADR(laData[15],6)+laData[27],'DIVSLSRP')
  *B606196,1 TMI [End  ] 
    = gfModalGen('QRM00000B00000','','','', ;
        'The Sales Rep ' + laData[27] + ' is not assigned to the division ' + laData[15] +;
        ' for the selected customer/store - please contact your system administrator')  
  ENDIF
  
  *B606196,1 TMI [Start]  Add store to the seek Key 
*  IF SEEK(laData[2]+PADR(laData[15],6)+laData[27],'DIVSLSRP')  &&*tmi
  IF SEEK(laData[2]+PADR(laData[3],8)+PADR(laData[15],6)+laData[27],'DIVSLSRP')
  *B606196,1 TMI [End  ] 
    laData[27] = DIVSLSRP.RepCode
    laData[28] = DIVSLSRP.COMM_RATE
  ELSE
    *B606196,1 TMI [Start]  Add store to the seek Key 
*    IF SEEK(laData[2]+PADR(laData[15],6),'DIVSLSRP')  &&*tmi
    IF SEEK(laData[2]+PADR(laData[3],8)+PADR(laData[15],6),'DIVSLSRP')
    *B606196,1 TMI [End  ] 
      laData[27] = DIVSLSRP.RepCode
      laData[28] = DIVSLSRP.COMM_RATE
    ELSE
      laData[27] = ""
      laData[28] = 0
    ENDIF
  ENDIF
  SHOW GET laData[27]
  SHOW GET laData[28]
  =lfRefresh(lcWinCh2)
ENDIF
*-- end of lfChkRep1.

*:**************************************************************************
*:* Name        : lfChkRep2
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 05/15/2002
*:* Purpose     : Check Rep2
*:***************************************************************************
*:* Called from : soord ( lfvDept )
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfChkRep2()
*:* Refer To    : C200339,1 
*:***************************************************************************
FUNCTION lfChkRep2
PRIVATE lcRep

IF !EMPTY(laData[29])
  IF !USED('DIVSLSRP')
    =gfOpenFile(gcDataDir+'DIVSLSRP','DIVSLSRP','SH')
  ENDIF
  *B606196,1 TMI [Start]  Add store to the seek Key 
*  IF !SEEK(laData[2]+PADR(laData[15],6)+laData[29],'DIVSLSRP')  &&*tmi
  IF !SEEK(laData[2]+PADR(laData[3],8)+PADR(laData[15],6)+laData[29],'DIVSLSRP')
  *B606196,1 TMI [End  ]   
    = gfModalGen('QRM00000B00000','','','', ;
        'The Sales Rep ' + laData[29] + ' is not assigned to the division ' + laData[15] +;
        ' for the selected customer/store - please contact your system administrator')  
  ENDIF
  *B606196,1 TMI [Start]  Add store to the seek Key 
*  IF SEEK(laData[2]+PADR(laData[15],6)+laData[29],'DIVSLSRP')  &&*tmi
  IF SEEK(laData[2]+PADR(laData[3],8)+PADR(laData[15],6)+laData[29],'DIVSLSRP')
  *B606196,1 TMI [End  ] 
    laData[29] = DIVSLSRP.RepCode
    laData[30] = DIVSLSRP.COMM_RATE
  ELSE
  *B606196,1 TMI [Start]  Add store to the seek Key 
*    IF SEEK(laData[2]+PADR(laData[15],6),'DIVSLSRP')  &&*tmi
    IF SEEK(laData[2]+PADR(laData[3],8)+PADR(laData[15],6),'DIVSLSRP')
  *B606196,1 TMI [End  ] 
      laData[29] = DIVSLSRP.RepCode
      laData[30] = DIVSLSRP.COMM_RATE
    ELSE
      laData[29] = ""
      laData[30] = 0
    ENDIF
  ENDIF
  SHOW GET laData[29]
  SHOW GET laData[30]
  =lfRefresh(lcWinCh2)
ENDIF
*-- end of lfChkRep2.

*:**************************************************************************
*:* Name        : lfCstDvCmm
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 05/14/2002
*:* Purpose     : Customer/Division Commesion
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfCstDvCmm()
*:* Refer To    : C200339,1 
*:***************************************************************************
FUNCTION lfCstDvCmm
PRIVATE lcAlias,lnI
lcAlias = ALIAS()
IF !USED('DIVSLSRP')
  =gfOpenFile(gcDataDir+'DIVSLSRP','DIVSLSRP','SH')
ENDIF

SELECT DIVSLSRP

FOR lnI = 27 TO 29 STEP 2
  *--get the commession from the divslsrp file.
  IF !EMPTY(laData[lnI]) 
    *B606196,1 TMI [Start]  Add store to the seek Key 
*    IF SEEK(laData[2]+PADR(laData[15],6)+laData[lnI],'DIVSLSRP')  &&*tmi
    IF SEEK(laData[2]+PADR(laData[3],8)+PADR(laData[15],6)+laData[lnI],'DIVSLSRP')
    *B606196,1 TMI [End  ] 
      laData[lnI+1] = DIVSLSRP.COMM_RATE
    ENDIF  
  ELSE
    laData[lnI+1] = 0
  ENDIF
ENDFOR
SELECT (lcAlias )
*-- end of lfCstDvCmm.

*:**************************************************************************
*:* Name        : lfEmptyRep
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 05/15/2002
*:* Purpose     : Empty sales reps when changing division
*:***************************************************************************
*:* Called from : ARCUST.PRG
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfEmptyRep()
*:* Refer To    : C200339,1 
*:*************************************************************************** not used
FUNCTION lfEmptyRep
PRIVATE lnI,lcAlias
lcAlias = ALIAS()

IF EMPTY(laData[35])
  RETURN
ENDIF

=lfOpnFls()

SELECT IIF(laScrMode[2],'DIVSLSRP',(lcDivSlsRp))
FOR lnI = 19 TO 21 STEP 2
  IF !EMPTY(laData[lnI])   
    *B606196,1 TMI [Start]  Add store to the seek Key 
*    IF !SEEK(laData[2]+PADR(laData[35],6)+laData[lnI])  &&*tmi
    IF !SEEK(laData[2]+PADR(laData[3],8)+PADR(laData[35],6)+laData[lnI])
    *B606196,1 TMI [End  ] 
      = gfModalGen('QRM00000B00000','','','', ;
            'The Sales Rep ' + laData[lnI] + ' is not assigned to the division ' + laData[35] +;
            ' for the selected customer/store - please contact your system administrator')  
      laData[lnI] = '   '
    ENDIF
    laData[lnI+1] = 0
  ENDIF
ENDFOR
=lfRefresh(lcWinCh1)
SELECT (lcAlias)
*-- end of lfEmptyRep.

*:**************************************************************************
*:* Name        : lfRepNotfy
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 05/15/2002
*:* Purpose     : Notify when rep is changed
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfRepNotfy()
*:* Refer To    : C200339,1 
*:*************************************************************************** not used
FUNCTION lfRepNotfy
PRIVATE lcRep,laAlias
laAlias = ALIAS()
lcRep = SYS(18)

IF EMPTY(laData[lnSlsRep]) OR EMPTY(laData[35]) OR laData[lnSlsRep] == lcOldValue
  RETURN
ENDIF

=lfOpnFls()

*B606196,1 TMI [Start]  Add store to the seek Key 
*IF !SEEK(laData[2]+PADR(laData[35],6)+laData[lnSlsRep],(lcDivSlsRp))  &&*tmi
IF !SEEK(laData[2]+PADR(laData[3],8)+PADR(laData[35],6)+laData[lnSlsRep],(lcDivSlsRp))
  *B606196,1 TMI [End  ] 
  =gfModalGen('INM00000B00000','DIALOG','','', ;
  'The Sales Rep ' + laData[lnSlsRep] + ' is not assigned to the division ' + laData[35] + ;
  ' for the selected customer/store - please contact your system administrator.')  
*  laData[lnSlsRep] = '   '
ENDIF
*laData[lnSlsRep+1] = 0

SHOW GET laData[lnSlsRep] 
SHOW GET laData[lnSlsRep+1] 
SELECT (laAlias)
*-- end of RepNotfy.

*:**************************************************************************
*:* Name        : lfOpnFls
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 05/15/2002
*:* Purpose     : Open Files
*:***************************************************************************
*:* Called from : lfSalesRep(),lfEmptyRep(),RepNotfy()
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfOpnFls()
*:* Refer To    : C200339,1 
*:***************************************************************************
FUNCTION lfOpnFls

IF !USED('DIVSLSRP')
  =gfOpenFile(gcDataDir+'DIVSLSRP','DIVSLSRP','SH')
ENDIF
IF !USED('SALESREP')  
  =gfOpenFile(gcDataDir+'SALESREP','SALESREP','SH')
ENDIF  
IF !laScrMode[2] AND !USED(lcDivSlsRp)
  CREATE CURSOR (lcDivSlsRp) (TYPE C(1),ACCOUNT C(5),STORE C(8),CDIVISION C(6),REPCODE C(3),COMM_RATE N(5,2),LMARK L)
  INDEX ON ACCOUNT+STORE+CDIVISION+REPCODE TAG (lcDivSlsRp)
  *B606196,1 TMI [Start]  Add store to the seek Key 
*  APPEND FROM (gcDataDir+'DIVSLSRP') FOR ACCOUNT=laData[2]  &&*tmi
  APPEND FROM (gcDataDir+'DIVSLSRP') FOR ACCOUNT+STORE=laData[2]+PADR(laData[3],8)
  *B606196,1 TMI [End  ] 
ENDIF  
*-- end of lfOpnFls.
*:**************************************************************************
*:* Name        : lfRSlsRep1
*:* Developer   : Abdou El-Elgendy
*:* Date        : 05/24/2002
*:* Purpose     : Update the sales rep1 & Comm. up on the Division.
*:***************************************************************************
*:* Called from : RMCRMEM.PRG
*:***************************************************************************
*:* Parameters  : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lf...()
*:***************************************************************************
*:* Refer To    : C200341,1 
*:***************************************************************************
*:
FUNCTION lfRSLSREP1
PRIVATE lcSlsRep,laAlias
laAlias = ALIAS()
lcSlsRep = SYS(18)

*-- > laData[2]  == Account.
*-- > laData[14] == cDivision.
*-- > laData[16] == Sales Rep 1
*-- > laData[19] == Sales Rep 2
*-- check if the file at the data directry or no,
   
IF !USED('DIVSLSRP')
  =gfOpenFile(gcDataDir+'DIVSLSRP','DIVSLSRP','SH')
ENDIF
*B606196,1 TMI [Start]  Add store to the seek Key 
*IF !SEEK(laData[2]+PADR(laData[14],6)+laData[16],'DIVSLSRP')  &&*tmi
IF !EMPTY(laData[16]) &&*TMI
  *B606196,1 TMI [End  ] 
  IF !SEEK(laData[2]+laData[4]+PADR(laData[14],6)+laData[16],'DIVSLSRP')
    = gfModalGen('QRM00000B00000','','','', ;
        'The Sales Rep ' + laData[16] + ' is not assigned to the division ' + laData[14] +;
        ' for the selected customer/store - please contact your system administrator')  
  ENDIF
  *B606196,1 TMI [Start]  Add store to the seek Key 
  *IF !SEEK(laData[2]+PADR(laData[14],6)+laData[16],'DIVSLSRP') .AND. SEEK(laData[2]+PADR(laData[14],6),'DIVSLSRP')  &&*tmi
  IF !SEEK(laData[2]+laData[4]+PADR(laData[14],6)+laData[16],'DIVSLSRP') .AND. SEEK(laData[2]+laData[4]+PADR(laData[14],6),'DIVSLSRP')
  *B606196,1 TMI [End  ] 
    laData[16] = DIVSLSRP.RepCode
    laData[17] = DIVSLSRP.COMM_RATE
  ELSE  
    *B606196,1 TMI [Start]  Add store to the seek Key 
  *  IF !SEEK(laData[2]+PADR(laData[14],6),'DIVSLSRP')  &&*tmi
    IF !SEEK(laData[2]+laData[4]+PADR(laData[14],6),'DIVSLSRP')
    *B606196,1 TMI [End  ] 
      laData[16] = ""
      laData[17] = 0
      SHOW GET laData[17] DISABLE  &&*tmi B606196,1
    ENDIF
  ENDIF
ENDIF  &&*tmi
SHOW GET laData[16]
SHOW GET laData[17]

*-- End Of RSLSREP1
*:**************************************************************************
*:* Name        : lfRSlsRep2
*:* Developer   : Abdou El-Elgendy
*:* Date        : 05/24/2002
*:* Purpose     : Update the sales rep2 & Comm. up on the Division.
*:***************************************************************************
*:* Called from : RMCRMEM.PRG
*:***************************************************************************
*:* Parameters  : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lf...()
*:***************************************************************************
*:* Refer To    : C200341,1 
*:***************************************************************************
*:
FUNCTION lfRSLSREP2
PRIVATE lcRep
laAlias = ALIAS()
lcSlsRep = SYS(18)

*-- > laData[2]  == Account.
*-- > laData[14] == cDivision.
*-- > laData[16] == Sales Rep 1
*-- > laData[19] == Sales Rep 2


IF !USED('DIVSLSRP')
  =gfOpenFile(gcDataDir+'DIVSLSRP','DIVSLSRP','SH')
ENDIF
*B606196,1 TMI [Start]  Add store to the seek Key 
*IF !SEEK(laData[2]+PADR(laData[14],6)+laData[19],'DIVSLSRP')  &&*tmi
IF !EMPTY(laData[19])  &&*tmi
  *B606196,1 TMI [End  ] 
  IF !SEEK(laData[2]+laData[4]+PADR(laData[14],6)+laData[19],'DIVSLSRP')
    = gfModalGen('QRM00000B00000','','','', ;
        'The Sales Rep ' + laData[19] + ' is not assigned to the division ' + laData[14] +;
        ' for the selected customer/store - please contact your system administrator')  
  ENDIF
  *B606196,1 TMI [Start]  Add store to the seek Key 
  *IF !SEEK(laData[2]+PADR(laData[14],6)+laData[19],'DIVSLSRP') .AND. SEEK(laData[2]+PADR(laData[14],6),'DIVSLSRP')  &&*tmi
  IF !SEEK(laData[2]+laData[4]+PADR(laData[14],6)+laData[19],'DIVSLSRP') .AND. SEEK(laData[2]+laData[4]+PADR(laData[14],6),'DIVSLSRP')
  *B606196,1 TMI [End  ] 
    laData[19] = DIVSLSRP.RepCode
    laData[20] = DIVSLSRP.COMM_RATE
  ELSE
  *B606196,1 TMI [Start]  Add store to the seek Key 
  *  IF !SEEK(laData[2]+PADR(laData[14],6),'DIVSLSRP')  &&*tmi
    IF !SEEK(laData[2]+laData[4]+PADR(laData[14],6),'DIVSLSRP')
    *B606196,1 TMI [End  ] 
      laData[19] = ""
      laData[20] = 0
      SHOW GET laData[20] DISABLE  &&*tmi  B606196,1
    ENDIF
  ENDIF
ENDIF &&*tmi
SHOW GET laData[19]
SHOW GET laData[20]
*-- End Of RSLSREP2

*:**************************************************************************
*:* Name        : lfASLSREP1
*:* Developer   : Abdou El-Elgendy
*:* Date        : 05/26/2002
*:* Purpose     : Update the sales rep1 & Comm. up on the Division.
*:***************************************************************************
*:* Called from : ARDINV.PRG
*:***************************************************************************
*:* Parameters  : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     : = lf....()
*:***************************************************************************
*:* Refer To    : C200340,1 
*:***************************************************************************
*:
FUNCTION lfASLSREP1
PRIVATE lcSlsRep,laAlias
laAlias = ALIAS()

lcSlsRep = SYS(18)

*-- > laData[2]  == Account.
*-- > laData[13] == cDivision.
*-- > laData[17] == Sales Rep 1
*-- > laData[19] == Sales Rep 2
*B606120,1 NAD
* laData[17] =SPACE(03)
*B606120,1 NAD

*-- check if the file at the data directry or no,
IF FILE(gcDataDir+'DIVSLSRP.DBF') 
  llDIVSLSRP = .F.
  IF !USED('DIVSLSRP')
    llDIVSLSRP = gfOpenFile(gcDataDir+'DIVSLSRP',gcDataDir+'DIVSLSRP','SH')
  ENDIF
   
  *-- Cheak for first sales rep.
  
  *B606120,1 NAD
  
  IF (!EMPTY(laData[2]) .AND. !EMPTY(laData[13]))
    *B606196,1 TMI [Start]  Add store to the seek Key 
*    IF SEEK(laData[2]+PADR(laData[13],6)+ IIF(EMPTY(laData[17]),'',laData[17]),'DIVSLSRP')  &&*tmi
    IF SEEK(laData[2]+laData[5]+PADR(laData[13],6)+ IIF(EMPTY(laData[17]),'',laData[17]),'DIVSLSRP')
    *B606196,1 TMI [End  ] 
      STORE DIVSLSRP.RepCode TO laData[17]
      STORE DIVSLSRP.Comm_Rate TO laData[18] , lnSoucomm1
     
    ELSE    
    
      IF EMPTY(ladata[17])
        =gfModalGen('INM00000B00000','DIALOG','','', ;  
         'No Sales Rep assigned to the division ' + laData[13] + ;
         ' for the selected customer/store - please contact your system administrator.')  
        
      
      ELSE
        = gfModalGen('INM00000B00000','DIALOG','','', ;
        'The Sales Rep ' + ladata[17] + ' is not assigned to the division ' + laData[13] + ;
        ' for the selected customer/store - please contact your system administrator.')  
        *B606196,1 TMI [Start]  Add store to the seek Key 
*        IF SEEK (laData[2]+PADR(laData[13],6),'DIVSLSRP')  &&*tmi
        IF SEEK (laData[2]+laData[5]+ PADR(laData[13],6),'DIVSLSRP')
        *B606196,1 TMI [End  ]  
          laData[17] = DIVSLSRP.RepCode
          laData[18] = DIVSLSRP.Comm_Rate
          lnSoucomm1 =DIVSLSRP.Comm_Rate
          laData[19] = ''
          laData[20] = 0
          lnSoucomm2 =0 
        ElSE
          *B606196,1 TMI [Start]  Add store to the seek Key 
*          IF SEEK (laData[2],'DIVSLSRP')  &&*tmi
          IF SEEK (laData[2]+laData[5],'DIVSLSRP')
          *B606196,1 TMI [End  ] 
            laData[13] = DIVSLSRP.cdivision
            lnDivision  = ASUBSCRIPT('laDivision',IIF(ASCAN('laDivision',Alltrim(laData[13]))=0,1,ASCAN('laDivision',ALLTRIM(laData[13]))),1)      
            laData[17] = DIVSLSRP.RepCode
            laData[18] = DIVSLSRP.Comm_Rate
            lnSoucomm1 =DIVSLSRP.Comm_Rate
            laData[19] = ''
            laData[20] = 0
            lnSoucomm2 =0
          ENDIF           
       ENDIF
        
      ENDIF  
      
      
      
    ENDIF
    SHOW GET laData[17]
    SHOW GET laData[18]
  ENDIF

  IF EMPTY(laData[17])
    laData[18] = 0.0
  ENDIF
  
  IF llDIVSLSRP
    = gfCloseFile('DIVSLSRP')
  ENDIF
ENDIF

SELECT (laAlias)
*-- End Of lfASLSREP1
*:**************************************************************************
*:* Name        : lfASLSREP2
*:* Developer   : Abdou El-Elgendy
*:* Date        : 05/26/2002
*:* Purpose     : Update the sales rep1 & Comm. up on the Division.
*:***************************************************************************
*:* Called from : ARDINV.PRG
*:***************************************************************************
*:* Parameters  : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     : = lf....()
*:***************************************************************************
*:* Refer To    : C200340,1 
*:***************************************************************************
*:
FUNCTION lfASLSREP2
PRIVATE lcSlsRep,laAlias
laAlias = ALIAS()
lcSlsRep = SYS(18)

*-- > laData[2]  == Account.
*-- > laData[13] == cDivision.
*-- > laData[17] == Sales Rep 1
*-- > laData[19] == Sales Rep 2


*-- check if the file at the data directry or no,
IF FILE(gcDataDir+'DIVSLSRP.DBF') 
  llDIVSLSRP = .F.
  IF !USED('DIVSLSRP')
    llDIVSLSRP = gfOpenFile(gcDataDir+'DIVSLSRP',gcDataDir+'DIVSLSRP','SH')
  ENDIF
  *-- Cheak for secand sales rep.

  *B606120,1 NAD
  IF (!EMPTY(laData[2]) .AND. !EMPTY(laData[13]) .AND. !EMPTY(laData[19]))  
    *B606196,1 TMI [Start]  Add store to the seek Key 
*    IF SEEK(laData[2]+PADR(laData[13],6)+laData[19],'DIVSLSRP')      &&*tmi
    IF SEEK(laData[2]+laData[5]+PADR(laData[13],6)+laData[19],'DIVSLSRP')      
       *B606196,1 TMI [End  ] 
        STORE DIVSLSRP.RepCode TO laData[19] 
        STORE DIVSLSRP.Comm_Rate TO laData[20] , lnSouComm2
    ELSE
    
     IF !EMPTY(ladata[19])
        = gfModalGen('INM00000B00000','DIALOG','','', ;
        'The Sales Rep ' + ladata[19] + ' is not assigned to the division ' + laData[13] + ;
        ' for the selected customer/store - please contact your system administrator.')  
          *B606196,1 TMI [Start]  Add store to the seek Key 
*          IF SEEK (laData[2]+laData[13],'DIVSLSRP')   &&*tmi
          IF SEEK (laData[2]+laData[5]+laData[13],'DIVSLSRP')
          *B606196,1 TMI [End  ] 
           laData[19] = DIVSLSRP.RepCode
            laData[20] = DIVSLSRP.Comm_Rate
            lnSoucomm2 =DIVSLSRP.Comm_Rate
          ELSE         
            laData[19] = SPACE(03)
            laData[20] = 0
            lnSouComm2 =0
          ENDIF  
      ENDIF
    ENDIF 
    SHOW GET laData[19]
    SHOW GET laData[20]
  ENDIF
   
   IF EMPTY(laData[19])
     laData[20] = 0.0
   ENDIF

  IF llDIVSLSRP
    = gfCloseFile('DIVSLSRP')
  ENDIF
ENDIF

SELECT (laAlias)
*-- End Of lfASLSREP2
*:***************************************************************************


*:**************************************************************************
*:* Name        : lfLoadRMSR
*:* Developer   : ASH 
*:* Date        : 06/18/2002
*:* Purpose     : Load Sales rep data for Sales rep
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfLoadRMSR()
*:* Refer To    : C200341,1 
*:***************************************************************************

FUNCTION lfLoadRMSR
PRIVATE lcAlias,lcDiv,lcVar,lnRec
lcAlias = ALIAS()

laData[16] = ""
laData[17] = 0
laData[19] = ""
laData[20] = 0
IF !USED('DIVSLSRP')
  =gfOpenFile(gcDataDir+'DIVSLSRP','DIVSLSRP','SH')
ENDIF
SELECT DIVSLSRP
lcDiv = laData[14]
*B606196,1 TMI [Start]  Add store to the seek Key 
*IF !SEEK(laData[2],'DIVSLSRP')  &&*tmi
IF !SEEK(laData[2]+IIF(!EMPTY(laData[4]),laData[4],''),'DIVSLSRP')
  *B606196,1 TMI [End  ] 
  =gfModalGen('INM00000B00000','DIALOG','','', ; 
  'No division assigned to the customer/store ' + laData[2] +IIF(!EMPTY(laData[4]),'/'+laData[4],'')+;
                         ', please contact your system administrator.') 
  *laData[2] = ""  &&*tmi  B606196,1
  lcVar = IIF(!EMPTY(laData[4]),'laData[4]','laData[2]')
  &lcVar = ''
  SHOW GET laData[2]
  SHOW GET laData[4]
  SHOW GET laData[16]
  SHOW GET laData[17]
  SHOW GET laData[19]
  SHOW GET laData[20]
  RETURN .F.
ENDIF
llContino = .T.
*B606196,1 TMI [Start]  Add store to the seek Key 
*IF !SEEK(laData[2]+lcDiv,'DIVSLSRP')  &&*tmi
IF !SEEK(laData[2]+PADR(laData[4],8)+lcDiv,'DIVSLSRP')
  *B606196,1 TMI [End  ] 
  *=gfModalGen('INM00000B00000','DIALOG','','', ;  
  'The Sales Rep ' + laData[lnSlsRep] + ' is not assigned to the division ' + lcDiv + ;
  ' for the selected customer/store - please contact your system administrator.')  
  
  =gfModalGen('INM00000B00000','DIALOG','','', ;  
  'No Sales Rep assigned to the division ' + lcDiv + ;
  ' for the selected customer/store - please contact your system administrator.')  

  *B606206,1 TMI [Start]  *--If there is lines in lcCrMemLin , exit
  lnRec = RECNO(lcCrMemLin)
  GO TOP IN (lcCrMemLin)
  IF !EOF(lcCrMemLin)
    laData[4] = lcOldValue
    IF BETWEEN(lnRec,1,RECCOUNT(lcCrMemLin))
      GOTO (lnRec)
    ENDIF
    llContino = .F.
    RETURN .F.
  ENDIF
  IF BETWEEN(lnRec,1,RECCOUNT(lcCrMemLin))
    GOTO (lnRec)
  ENDIF
  *B606206,1 TMI [End  ] 

  laData[16] = ""
  laData[17] = 0
  laData[19] = ""
  laData[20] = 0
  *B606196,1 TMI [Start]  Add store to the seek Key 
*  =SEEK(laData[2],'DIVSLSRP')  &&*tmi
  =SEEK(laData[2]+IIF(!EMPTY(laData[4]),laData[4],''),'DIVSLSRP')
  *B606196,1 TMI [End  ] 
  lcDiv = DIVSLSRP.cdivision
  laData[14] = DIVSLSRP.cdivision
  IF ASCAN('laDiv',laData[14]) > 0
    lnDiv = ASUBSCRIPT['laDiv',ASCAN('laDiv',laData[14]),1]
  ELSE
    = gfwCodePop(@laCodInfo, "CDIVISION" , "V,"+laData[14])
  ENDIF
  
  SHOW GET lnDiv
  SHOW GET laData[14]
  
ENDIF

lnCount = 0
*B606196,1 TMI [Start]  Add store to the scan Key 
*SCAN REST WHILE account+cdivision+repcode = laData[2]+lcDiv .AND. lnCount < 2  &&*tmi
SCAN REST WHILE account+store+cdivision+repcode = laData[2]+IIF(!EMPTY(laData[4]),laData[4],SPACE(8))+lcDiv .AND. lnCount < 2
  *B606196,1 TMI [End  ] 
  lnCount = lnCount +1
  IF lnCount = 1
    laData[16] = DIVSLSRP.RepCode
    laData[17] = DIVSLSRP.COMM_RATE
  ELSE  
    laData[19] = DIVSLSRP.RepCode
    laData[20] = DIVSLSRP.COMM_RATE
  ENDIF
ENDSCAN

SHOW GET laData[14]
SHOW GET laData[16]
SHOW GET laData[17]
SHOW GET laData[19]
SHOW GET laData[20]
SELECT (lcAlias)
*-- end of lfRMLoadSR.


*:**************************************************************************
*:* Name        : lfEMPT17
*:* Developer   : 
*:* Date        : 
*:* Purpose     : 
*:***************************************************************************
*:* Called from : ardinv.prg
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfEMPT17()
*:***************************************************************************
FUNCTION  lfEMPT17
  
  laData(17)=''
  laData(18)=0  &&tmi
*:**************************************************************************
*:* Name        : lfEMPT19
*:* Developer   : 
*:* Date        : 
*:* Purpose     : 
*:***************************************************************************
*:* Called from : ardinv.prg
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfEMPT19()
*:***************************************************************************
FUNCTION lfEMPT19
  laData(19)=''
  laData(20)=0  &&tmi


*:**************************************************************************
*:* Name        : lfVAcc
*:* Developer   : Nader Metry
*:* Date        : 
*:* Purpose     : 
*:***************************************************************************
*:* Called from : ardinv.prg
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfVAcc()
*:***************************************************************************
FUNCTION lfVAcc

PRIVATE llDIVSL,lcAlias
llDIVSL= .F.
lcAlias = SELECT(0)

IF !USED('DIVSLSRP')
  llDIVSL = gfOpenFile(gcDataDir+'DIVSLSRP',gcDataDir+'DIVSLSRP','SH')
ENDIF
*B606196,1 TMI [Start]  Add store to the seek Key 
*IF SEEK (laData[2]+lcidefdiv,'DIVSLSRP')
lcDiv = IIF(EMPTY(laData[13]),lciDefDiv,laData[13])
IF SEEK (laData[2]+IIF(!EMPTY(laData[5]),PADR(laData[5],8),''),'DIVSLSRP')
  *B606196,1 TMI [End  ] 
  *RETURN .F.  &&Commented by tmi B606206,1
ELSE
    *B606206,1 TMI [Start] If no sales rep is assigned to this store , release it and exit
    *laData[2]=''    
    *=gfModalGen('INM00000B00000','DIALOG','','', ;  
    'No Sales Rep assigned to the division ' + lcidefdiv + ;
    ' for the selected customer/store - please contact your system administrator.')
    =gfModalGen('INM00000B00000','DIALOG','','', ; 
    'No division assigned to the customer/store ' + laData[2] + IIF(!EMPTY(laData[5]),'/'+laData[5],'')+;
                                               ', please contact your system administrator.') 
    lcVar = IIF(EMPTY(laData[5]),'laData[2]','laData[5]')
    &lcVar = IIF('2'$lcVar,SPACE(5),SPACE(8))
    SHOW GET laData[2]
    SHOW GET laData[5]
    SHOW GET laData[17]
    SHOW GET laData[18]
    SHOW GET laData[19]
    SHOW GET laData[20]
    *B606206,1 TMI [End  ] If no sales rep is assigned to this store , release it and exit 
    RETURN .T.
ENDIF

*B606206,1 TMI [Start] Continue Loading sales rep data for the selected customer/store
IF !SEEK(laData[2]+PADR(laData[5],8)+lcDiv,'DIVSLSRP')
  =gfModalGen('INM00000B00000','DIALOG','','', ;
  'No Sales Rep assigned to the division ' + lcDiv + ;
  ' for the selected customer/store - please contact your system administrator.')

  *B606206,1 TMI [Start]  *--If there is lines in lcInvLine , exit
  lnRec = RECNO(lcInvLine)
  GO TOP IN (lcInvLine)
  IF !EOF(lcInvLine)
    laData[5] = SPACE(8)  &&lcOldStore
    SELECT (lcInvHdr)
    REPLACE STORE WITH laData[5]    
    SELECT (lcInvLine)
    REPLACE ALL COMM1 WITH 0 , COMM2 WITH 0
    IF BETWEEN(lnRec,1,RECCOUNT(lcInvLine))
      GOTO (lnRec)
    ENDIF
    SELECT (lcAlias)
    RETURN .T.
  ENDIF
  IF BETWEEN(lnRec,1,RECCOUNT(lcInvLine))
    GOTO (lnRec)
  ENDIF
  *B606206,1 TMI [End  ] 

  laData[17] = ""
  laData[18] = 0
  laData[19] = ""
  laData[20] = 0
  =SEEK(laData[2]+IIF(!EMPTY(laData[5]),PADR(laData[5],8),''),'DIVSLSRP')
  lcDiv = DIVSLSRP.cdivision
  laData[13] = DIVSLSRP.cdivision
  IF ASCAN('laDivision',laData[13]) > 0
    lnDivision = ASUBSCRIPT['laDivision',ASCAN('laDivision',laData[13]),1]
  ELSE
    = gfwCodePop(@laCodes , "CDIVISION" , "V,"+laData[13])
  ENDIF
  
  SHOW GET lnDivision  &&TMI
  SHOW GET laData[13]  &&TMI
ENDIF

lnCount = 0
SELECT DIVSLSRP
SCAN REST WHILE account+store+cdivision+repcode = laData[2]+PADR(laData[5],8)+lcDiv .AND. lnCount < 2
  lnCount = lnCount +1
  IF lnCount = 1
    laData[17] = DIVSLSRP.RepCode
    laData[18] = DIVSLSRP.COMM_RATE
  ELSE  
    laData[19] = DIVSLSRP.RepCode
    laData[20] = DIVSLSRP.COMM_RATE
  ENDIF
ENDSCAN

SELECT (lcInvLine)
lnRec = RECNO(lcInvLine)
GO TOP
REPLACE ALL COMM1 WITH laData[18],;
            COMM2 WITH laData[20]
IF BETWEEN(lnRec,1,RECCOUNT(lcInvLine) )
  GOTO (lnRec)
ENDIF
SHOW GET laData[13]
SHOW GET laData[17]
SHOW GET laData[18]
SHOW GET laData[19]
SHOW GET laData[20]

SELECT (lcInvLine)
lnRec = RECNO(lcInvLine)
GO TOP IN (lcInvLine)
REPLACE ALL COMM1 WITH laData[18],COMM2 WITH laData[20]
IF BETWEEN(lnRec,1,RECCOUNT(lcInvLine))
  GOTO (lnRec)
ENDIF
*B606206,1 TMI [End  ] Continue Loading sales rep data for the selected customer/store
IF llDIVSL
  =gfCloseFile('DIVSLSRP')
ENDIF

*B606206,1 TMI [Start] Add return by .f. for the calling fun. to continue running
SELECT (lcAlias)
RETURN .F.
*B606206,1 TMI [End  ] Add return by .f. for the calling fun. to continue running

*-- end of lfVAcc.

*:**************************************************************************
*:* Name        : lfSOSAVEOK    
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 07/07/2002
*:* Purpose     : Deny save  process if No sales reps entred
*:***************************************************************************
*:* Called from : SOORD.PRG
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfSAVEOK    ()
*:***************************************************************************
FUNCTION lfSOSAVEOK
IF EMPTY(laData[27]) AND EMPTY(laData[29])
  =gfModalGen('INM42072B00000','ALERT','Sales rep(s)')
  llCSave = .F.
ENDIF
RETURN llCSave
*-- end of lfSAVEOK.

*:**************************************************************************
*:* Name        : lfARSAVEOK
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 07/07/2002
*:* Purpose     : Deny save  process if No sales reps entred
*:***************************************************************************
*:* Called from : ARDINV.PRG
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfARSAVEOK()
*:***************************************************************************
FUNCTION lfARSAVEOK

IF EMPTY(laData[17]) AND EMPTY(laData[19])
  =gfModalGen('INM42072B00000','ALERT','Sales rep(s)')
  llCSave = .F.
ENDIF
RETURN llCSave
*-- end of lfSAVEOK.

*:**************************************************************************
*:* Name        : lfRMSAVEOK
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 07/07/2002
*:* Purpose     : Deny save  process if No sales reps entred
*:***************************************************************************
*:* Called from : RMCRMEM.PRG
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfRMSAVEOK()
*:***************************************************************************
FUNCTION lfRMSAVEOK
IF EMPTY(laData[16]) AND EMPTY(laData[19])
  =gfModalGen('INM42072B00000','ALERT','Sales rep(s)')
  llCSave = .F.
ENDIF
RETURN llCSave
*-- end of lfSAVEOK.

*:**************************************************************************
*:* Name        : lfADDSR                                         *B606206,2
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 07/28/2002
*:* Purpose     : No lines are added if no sales rep is selected
*:***************************************************************************
*:* Called from : SOORD
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfADDSR()
*:***************************************************************************
FUNCTION lfADDSR
IF EMPTY(laData[27]) AND EMPTY(laData[29])
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'No Sales Rep. is selected!')
  RETURN .F.
ENDIF
*-- end of lfADDSR.
************************************** End of file *********************************************