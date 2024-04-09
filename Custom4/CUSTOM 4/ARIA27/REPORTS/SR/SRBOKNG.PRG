*:************************************************************************
*: Program file  : SRBOKNG.Prg          (E301259)
*: Program desc. : SALESREP  Boking
*: System        : Aria Advantage Series VER. 2.7
*: Module        : SR
*: Developer     : WAB- WALID A. WAHAB
*: Date          : 06/14/1999
*:************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : 
*:************************************************************************
*: Passed Parameters  : None
*:************************************************************************
*: Example : DO SRBOKNG
*:************************************************************************
*: Modifications:
*:B802738,1 RAMY 11/02/1999 When filter on certain salesrep and selecting one season only show 
*:B802738,1                 orders belong to only the selected season
*:E301364,1 SSE 01/25/1999 We need to suppress Decimals to be able to display 
*:E301364,1                larger figures (modifications in DOS FRX) 
*:B803017,1 SSE 02/02/2000 Fixed the bug Missing ')'
*:B603173,1 RAMY 03/07/2000 Fix the last point 
*:B603173,1                 Note : Fixed in the .FRX file only
*:B802816,1 RAMY 04/07/2000 Fixed the wrong filter in case select a sales rep
*:B603547,1 ABD  04/12/2000 Report support multi currency format.(Header,variables and this code)
*:B603590,1 WMA  04/23/2000 Adjust the Cust PO field width in Cust PO InList()
*:B803305,1 ABD  05/08/2000 1) The report should include the  orders with zero comm.%
*:B803305,1 ABD             2) The report shouldn't list the orders that are booked by 
*:B803305,1 ABD                two sales rep unless he select them.
*:B605089,1 ADEL 07/10/02  When Sales Rep based on Division, Don't ignore
*:B605089,1                OrdHdr.Rep2 when claculating Booked.
*:B608129,1 NNA 06/15/2007 (T20060928.0002) Fix bug that if there are some different accounts for the same Rep , report
*:B608129,1 NNA            does not count them correctly , it just count records not accounts
*:************************************************************************
lcTime = TIME()
lcNewExp = ' '

IF "REP1" $ lcRpExp
  =lfAddFxOr("ORDHDR.REP1","ORDHDR.REP2")
ELSE
  lcNewExp = lcRpExp
ENDIF
select OrdHdr
LOCATE FOR &lcNewExp
IF EOF()
  = gfModalGen("TRM00052B00000","DIALOG",'')  && 'There are no records to display' 
  RETURN
ENDIF

*------ Collecting data  [BEGIN] --------
lcOrdhTmp  = gfTEMPNAME()
lcOrdhTmp1 = gfTEMPNAME()  && TEMP FILE TO HOLD RECORDS WITH SPLIT COMM
lcCustTmp  = gfTEMPNAME()  && TEMP FILE TO HOLD ACCOUNTS TO GET A COUNT 


COPY REST TO (gcWorkDir+lcOrdhTmp) FOR &lcNewExp
SELECT CUSTOMER
COPY STRUCTURE TO (gcWorkDir+lcCustTmp) FIELDS SalesRep,Account

*B605089,1 (Begin) Is salesrep Commession based on Division?
llRep_div = ALLTRIM(gfGetMemVar('M_REP_COMM' , gcAct_Comp)) = 'D'
IF llRep_div
  = gfOpenFile(gcDataDir+'REP_DIV',gcDataDir+'REP_DIV','SH')
ENDIF
*B605089,1 (End)

=gfOpenFile(gcWorkDir+lcCustTmp,'','SH')
INDEX ON  SalesRep+Account TAG &lcCustTmp

=gfOpenFile(gcWorkDir+lcOrdhTmp,'','SH')

*:B803305,1 ABD  Remark the next lines and call collect function [Begin]
*-- Function to Collect the correct data.
= lfCollect()
*COPY ALL TO (gcWorkDir+lcOrdhTmp1) FOR .NOT. EMPTY(Rep2) .AND. .NOT. EMPTY(Comm2)
*=gfOpenFile(gcWorkDir+lcOrdhTmp1,'','SH')
*REPL ALL FLAG WITH '2'
*SELECT &lcOrdhTmp
*APPE FROM (gcWorkDir+lcOrdhTmp1)
*:B803305,1 ABD [End]
SELECT &lcOrdhTmp

*B803305,1 ABD The report should include the  orders with zero comm.% [Begin]
IF !llRpPrntCo
  SET FILTER TO IIF(Flag<>"2",Comm1 <> 0,.T.)
ENDIF
*B803305,1 ABD  [End]
GOTO TOP
IF EOF()
  = gfModalGen("TRM00052B00000","DIALOG",'')  && 'There are no records to display' 
  RETURN
ENDIF

*B603547,1 ABD  Define Variables .[Begin]
lcGrpCurr = cCurrCode
llSameGrp = .T.
*B603547,1 ABD [End]


*B603547,1 ABD Create Index on Currency.  [BEGIN]
*INDEX ON IIF(FLAG='2',Rep2+Order , Rep1+Order ) TAG &lcOrdhTmp
IF llMultCurr
  INDEX ON IIF(FLAG='2',Rep2+cCurrCode+Order, Rep1+cCurrCode+Order) TAG &lcOrdhTmp
  
  *B608129,1 NNA 06/15/2007 (Begin)
  INDEX ON IIF(FLAG='2',Rep2+cCurrCode+Account, Rep1+cCurrCode+Account) TAG RepAcc
  *B608129,1 NNA (End)
ELSE
  INDEX ON IIF(FLAG='2',Rep2+Order , Rep1+Order ) TAG &lcOrdhTmp

  *B608129,1 NNA 06/15/2007 (Begin)
  INDEX ON IIF(FLAG='2',Rep2+Account, Rep1+Account) TAG RepAcc
  *B608129,1 NNA (End)
ENDIF
*B603547,1 ABD [End]


SET RELATION TO IIF(STORE=SPACE(8),'M'+Account,'S'+Account+STORE) INTO CUSTOMER
GOTO TOP

*B608129,1 NNA 06/15/2007 (Begin) The following code to get the correct Numbers of accounts per Sales Rep.
lcTotAccs  = gfTEMPNAME() && TEMP FILE TO HOLD TOTAL NUMBER OF ACCOUNTS PER REP
CREATE TABLE (gcWorkDir+lcTotAccs )(REP C(3),ACC_COUNT N(5))
INDEX ON (Rep) TAG &lcTotAccs

lcCountAcc = gfTEMPNAME() && TEMP FILE TO HOLD NUMBER OF ACCOUNTS PER REP+CURRANCY
CREATE TABLE (gcWorkDir+lcCountAcc)(REP C(3),CCURRCODE C(3),ACC_COUNT N(5))
IF llMultCurr
  INDEX ON (Rep+cCurrCode) TAG &lcCountAcc
ELSE
  INDEX ON (Rep) TAG &lcCountAcc
ENDIF
STORE '' TO lcOldAcc,lcOldRep,lcOldCurr
SELECT &lcOrdhTmp
GOTO TOP
SCAN REST WHILE !EOF()
  IF NOT(lcOldRep==IIF(FLAG='2',Rep2,Rep1))
     lcOldRep  = IIF(FLAG='2',Rep2,Rep1)
     lcOldAcc  = EVAL(lcOrdhTmp+'.Account')
     lcOldCurr = EVAL(lcOrdhTmp+'.cCurrCode')     
     INSERT INTO &lcCountAcc VALUES(lcOldRep,lcOldCurr,1)
     INSERT INTO &lcTotAccs  VALUES(lcOldRep,1)
  ELSE
    IF NOT (lcOldCurr==EVAL(lcOrdhTmp+'.cCurrCode'))
      lcOldCurr=EVAL(lcOrdhTmp+'.cCurrCode')
      INSERT INTO &lcCountAcc VALUES(lcOldRep,lcOldCurr,1)
      IF NOT (lcOldAcc==EVAL(lcOrdhTmp+'.Account'))
        SELECT &lcTotAccs
        SEEK(IIF(EVAL(lcOrdhTmp+'.FLAG')='2',EVAL(lcOrdhTmp+'.Rep2'),EVAL(lcOrdhTmp+'.Rep1')))
        lcOldAcc=EVAL(lcOrdhTmp+'.Account')
        REPLACE ACC_COUNT WITH EVAL(lcTotAccs+'.ACC_COUNT')+1
      ENDIF
    ELSE          
      IF NOT (lcOldAcc==EVAL(lcOrdhTmp+'.Account'))
        SELECT &lcCountAcc
        IF SEEK(IIF(llMultCurr,;
                IIF(EVAL(lcOrdhTmp+'.FLAG')='2',;
                EVAL(lcOrdhTmp+'.Rep2')+EVAL(lcOrdhTmp+'.cCurrCode'),EVAL(lcOrdhTmp+'.Rep1')+EVAL(lcOrdhTmp+'.cCurrCode')),;
                IIF(EVAL(lcOrdhTmp+'.FLAG')='2',;
                EVAL(lcOrdhTmp+'.Rep2'),EVAL(lcOrdhTmp+'.Rep1'))))
          REPLACE ACC_COUNT WITH EVAL(lcCountAcc+'.ACC_COUNT')+1
          lcOldAcc=EVAL(lcOrdhTmp+'.Account')
          lcOldCurr=EVAL(lcOrdhTmp+'.cCurrCode')
        ENDIF
        SELECT &lcTotAccs
        SEEK(IIF(EVAL(lcOrdhTmp+'.FLAG')='2',EVAL(lcOrdhTmp+'.Rep2'),EVAL(lcOrdhTmp+'.Rep1')))
        lcOldAcc=EVAL(lcOrdhTmp+'.Account')
        REPLACE ACC_COUNT WITH EVAL(lcTotAccs+'.ACC_COUNT')+1
      ENDIF
    ENDIF
  ENDIF   
ENDSCAN
*--Generate relation between Master temp file and my temp files that hold number of accounts per REP+Currency or Rep only
SELECT &lcOrdhTmp
SET ORDER TO TAG &lcOrdhTmp
IF llMultCurr
  SET RELATION TO IIF(FLAG='2',REP2+cCurrCode,REP1+cCurrCode) INTO &lcCountAcc ADDITIVE
ELSE
  SET RELATION TO IIF(FLAG='2',REP2,REP1) INTO &lcCountAcc ADDITIVE
ENDIF
SET RELATION TO IIF(FLAG='2',REP2,REP1) INTO &lcTotAccs ADDITIVE
*B608129,1 NNA (End)



*------ Collecting data  [End ] --------
DO gfDispRe WITH EVAL('lcRpForm')


IF USED(lcOrdhTmp)
  USE IN (lcOrdhTmp)
ENDIF
IF USED(lcOrdhTmp1)
  USE IN (lcOrdhTmp1)
ENDIF
IF USED(lcCustTmp)
  USE IN (lcCustTmp)
ENDIF

*B608129,1 NNA 06/15/2007 (Begin) Close and erase the Temp files that I created before
IF USED(lcCountAcc)
  USE IN (lcCountAcc)
  ERASE(gcWorkDir+lcCountAcc+'.DBF')
  ERASE(gcWorkDir+lcCountAcc+'.CDX')
ENDIF
IF USED(lcTotAccs)
  USE IN (lcTotAccs)
  ERASE(gcWorkDir+lcTotAccs+'.DBF')
  ERASE(gcWorkDir+lcTotAccs+'.CDX')
ENDIF
*B608129,1 NNA (End)

ERASE(gcWorkDir+lcOrdhTmp+'.DBF')
ERASE(gcWorkDir+lcOrdhTmp+'.CDX')
ERASE(gcWorkDir+lcOrdhTmp1+'.DBF')
ERASE(gcWorkDir+lcOrdhTmp1+'.CDX')
ERASE(gcWorkDir+lcCustTmp+'.DBF')

*!**************************************************************************
*! Name      : lfSeTOrdr
*! Developer : WAB - WALID A. WAHAB
*! Date      : 06/14/1999
*! Purpose   : Set Relation,Reset Relation, in range browse screen.
*!**************************************************************************
*! Calls     : 
*!**************************************************************************
*! Called from : Option Grid && Report DYE DEMOND REPORT
*!**************************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example   : =fSeTOrdr()
*!**************************************************************************
*! Note      : symbol is [S,Set- R,ReSet]
*!**************************************************************************
FUNCTION lfSeTOrdr 
PARAMETERS OpGrdParm
DO CASE
  CASE OpGrdParm = 'S'
    SELECT ORDHDR
    lcRelation = [IIF(EMPTY(Store) , 'M' + Account,'S' + Account + Store)]
    SET ORDER TO Customer IN Customer
    SET RELATION TO &lcRelation INTO CUSTOMER && To customer file.
    GO TOP
  CASE OpGrdParm = 'R'
    SELECT ORDHDR
    SET RELATION OFF INTO CUSTOMER && To customer file.
ENDCASE
*!**************************************************************************
*! Name      : lfSetAcc
*! Developer : WAB - WALID A. WAHAB
*! Date      : 06/14/1999
*! Purpose   : Set Relation,Reset Relation, in range browse screen.
*!**************************************************************************
*! Calls     : 
*!**************************************************************************
*! Called from : Option Grid  && Report DYE DEMOND REPORT
*!**************************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example   : =lfSetAcc()
*!**************************************************************************
*! Note      :  symbol is [S,Set- R,ReSet]
*!**************************************************************************
FUNCTION lfSetAcc
PARAMETERS OpGrdParm
DO CASE
  CASE OpGrdParm = 'S'  && Set code
    SELECT CUSTOMER
    SET ORDER TO TAG CUSTOMER
    GO TOP IN CUSTOMER
  CASE OpGrdParm = 'R'  && Reset code
    SELECT CUSTOMER
    SET ORDER TO 
ENDCASE
*!**************************************************************************
*! Name      : lfSeTSRep 
*! Developer : WAB - WALID A. WAHAB
*! Date      : 06/14/1999
*! Purpose   : Go top in the style IN RANGE
*!**************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example   : =lfSeTSRep()
*!**************************************************************************
FUNCTION lfSeTSRep 
PARAMETERS OpGrdParm
DO CASE
  CASE OpGrdParm = 'S'
   SELECT SALESREP
   SET ORDER TO TAG  SALESREP
   GO TOP
  CASE OpGrdParm = 'R'
    SELECT SALESREP 
    SET ORDER TO 
ENDCASE
*!**************************************************************************
*! Name      : lfDydWhen 
*! Developer : WAB - WALID A. WAHAB
*! Date      : 06/10/1999
*! Purpose   : set Last Order Start Ship Date = today()+dye 
*!             visibility window
*!**************************************************************************
*! Calls     : gfGetmemVar()
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example   : =lfDydWhen()
*!**************************************************************************
FUNCTION lfDydWhen
PRIVATE lnarrPos
lnarrpos = INT(ASCAN(laOGFxFlt,'ORDHDR.START') / 7 + 0.9)
IF EMPTY(laOGFxFlt[lnarrpos,6])
  laOGFxFlt[lnarrpos,6] = date()+gfGetmemVar('M_DOVW','gcAct_Comp')
ENDIF
*!**************************************************************************
*! Name      : lfGetPos  
*! Developer : WAB - WALID A. WAHAB
*! Date      : 06/10/1999
*! Purpose   : Get Starting Position & lenth Of Color
*!**************************************************************************
*! Calls     : ()
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example   : =lfGetPos()
*!**************************************************************************
FUNCTION lfGetPos
PRIVATE lnClrPos
*-- laMajSeg array holds the style code segments data
*-- laMajSeg[x,1] Holds segment type
*-- laMajSeg[x,2] Holds segment title
*-- laMajSeg[x,3] Holds segment picture
*-- laMajSeg[x,4] Holds segment Starting position
*-- laMajSeg[x,5] Holds segment description
*-- laMajSeg[x,6] Holds segment separator
*-- laMajSeg[x,7] Holds (.T./.F.) segment end major. 
 DIMENSION laMajSeg[1,1]
 = gfItemMask(@laMajSeg)
 lnClrPos = int(ASCAN(laMajSeg,'C')/7+0.9)
 lnStrtPos = laMajSeg[lnClrPos,4] 
 lnFldLngth= LEN(laMajSeg[lnClrPos,3])
*!**************************************************************************
*! Name      : lfAddFxOr
*! Developer : WAB - WALID A. WAHAB
*! Date      : 06/10/1999
*! Purpose   : Create expretion Filter  
*!           : Devide laOgFlt into 2 array and add (.or.) inside 2nd array
*!**************************************************************************
*! Calls     : gfGenFlt()
*!**************************************************************************
*! Called from : srBokng.prg
*!**************************************************************************
*! Passed Parameters  : Exist Field / New Field
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example   : =lfAddFxOr()
*!**************************************************************************
FUNCTION lfAddFxOr
PARAMETER lcExstFld,lcNewFld
PRIVATE lnFldPos,lnElmntNo,lnBrsPos,llNewRow,lnBrRow,lnBrCol

*:B802816,1 RAMY Comment the old lines
*=ACOPY(laOGFxFlt,laOgFxOLd)    && copy Fixed Filter Array to another array
*=ACOPY(laBrFldFlt,laBrFldOLd)  && copy Borwse Filed Filter Arry to another array
*lnFldPos = ASCAN(laOGFxFlt,lcExstFld)       && get postion for exist filed at 
*lnFldPos = ASUBSCRIPT(laOGFxFlt,lnFldPos,1) && Fixed Filter Array
*lnElmntNo = ALEN(laOGFxFlt,1)     			&& get lenth of Fixed Filter Array
*lnBrRow = ALEN(laBrFldFlt,1)				&& get lenth of Fixed Filter Array
*
**-new array for hold the 2 filed and ".or." operator
*DECLARE laNewFlt[3,7]
*
**-increase the Row of browse filed array with one Row
*DECLARE laBrFldFlt[lnBrRow + 1 ,5]
*
*llNewRow = .F.
**- fill then new array and replace laBrFldFld[X,5] with then new array name
**- and position
*FOR lnRow = 1 TO 3
*  IF !llNewRow 
*    lnBrsPos = ASCAN(laBrFldFlt,"laOGFxFlt:"+ALLTRIM(STR(lnFldPos)))
*    lnBrsPos = ASUBSCRIPT(laBrFldFlt,lnBrsPos,1)
*    laBrFldFlt[lnBrsPos,5] = "laNewFlt:"+ALLTRIM(STR(lnRow))
*    llNewRow = .T.
*  ENDIF
*  FOR lnCol = 1 TO 7
*    IF lnROW = 2 
*      laNewFlt[lnRow,lnCol] = " "
*      laNewFlt[lnRow,1]     = ".OR."
*      laNewFlt[lnRow,4]     = ".F."
*    ELSE  
*      laNewFlt[lnRow,lnCol] = laOGFxFlt[lnFldPos,lnCol]
*    ENDIF
*  NEXT
*NEXT
*For lnBrCol = 1 TO 4
*  laBrFldFlt[lnBrRow + 1,lnBrCol] = laBrFldFlt[lnBrRow,lnBrCol]
*NEXT
*laBrFldFlt[lnBrRow + 1,5] = "laNewFlt:"+ALLTRIM(STR(lnRow-1))
*laNewFlt[3,1] = lcNewFld
*=ADEL(laOGFxFlt,lnFldPos)   	&& delete the row from Fixed Filter Array
*DECLARE laOGFxFlt[lnElmntNo - 1 ,7]  && dencrease the array elements
*llOGFilter = .T.
*lcOGHDFlt=gfGenFlt('laOGHDFlt' , llOGFilter , .T.)  && generate exp
*lcOgFxFlt=gfGenFlt('laOGFxFlt' , llOGFilter , .T.)  && generate exp
*lcNewFlt =gfGenFlt('laNewFlt'  , llOGFilter , .T.)  && generate exp
*lcOGVrFlt=gfGenFlt('laOGVrFlt' , llOGFilter , .T.)  && generate exp
*
*=lfInitValue('laOGVrFlt','C')
*=lfInitValue('laOGFxFlt','C')
*
**- generate the Final Filter Experation
*lcOGTmpExp=IIF(!EMPTY(lcOGHDFlt),lcOGHDFlt+'|Ÿ',lcOGHDFlt)+;
*           IIF(!EMPTY(lcOgFxFlt) AND !EMPTY(lcNewFlt),lcOgFxFlt+'|Ÿ',lcOgFxFlt);
*           +IIF(!EMPTY(lcNewFlt) AND !EMPTY(lcOGVRFlt),lcNewFlt+'|Ÿ',lcNewFlt)+;
*           lcOGVrFlt +IIF(!EMPTY(lcOGHDFlt),'Ÿ','')+IIF(!EMPTY(lcOgFxFlt)AND ;
*           !EMPTY(lcNewFlt),'Ÿ','')+IIF(!EMPTY(lcNewFlt)AND !EMPTY(lcOGVRFlt),'Ÿ','')
**- add '() '
*
**B802738,1 RAMY 11/02/99 [start] Add paranthes to separte the OR operateor to evaluate the sales rep code first
**lcNewExp=STRTRAN(STRTRAN(lcOGTmpExp,IIF(OCCURS('|Ÿ|Ÿ',lcOGTmpExp)=0,'|Ÿ','|Ÿ|Ÿ');
*                       ,' AND('),IIF(OCCURS('|Ÿ|Ÿ',lcOGTmpExp)=0,'Ÿ','ŸŸ'),')')
*
*
**B803017,1 02/02/2000 SSE (Begin) Fixed the bug missing ')'
**lcNewExp='('+STRTRAN(STRTRAN(lcOGTmpExp,IIF(OCCURS('|Ÿ|Ÿ',lcOGTmpExp)=0,'|Ÿ','|Ÿ|Ÿ');
*                      ,')'+' AND ' ),IIF(OCCURS('|Ÿ|Ÿ',lcOGTmpExp)=0,'Ÿ','ŸŸ'),'')
*
*lcNewExp = STRTRAN(STRTRAN(lcOGTmpExp,IIF(OCCURS('|Ÿ|Ÿ',lcOGTmpExp)=0,'|Ÿ','|Ÿ|Ÿ');
*                      ,')'+' AND ' ),IIF(OCCURS('|Ÿ|Ÿ',lcOGTmpExp)=0,'Ÿ','ŸŸ'),'')
**B803017,1 02/02/2000 SSE (End)
*
**B802738,1 RAMY 11/02/99 [End]
*
*DECLARE laBrFldFlt[lnBrRow,5]  		&& redelcare the array as above
*DECLARE laOGFxFlt[lnElmntNo,7]		&& redelcare the array as above
*=ACOPY(laOgFxOLd,laOGFxFlt)         && Return the values as the same
*=ACOPY(laBrFldOLd,laBrFldFlt)       && Return the values as the same
*RETURN

*Add new lines
*-- We will extract the filter part concerning with REP1 and add REP2 to the filter expression
IF RAT('AND' , LEFT(lcRpExp,AT('ORDHDR.REP1',lcRpExp) )) # 0
  *-- Get the expression part before the rep1 expression
  lcTempV = SUBSTR(lcRpExp,RAT('AND' , LEFT(lcRpExp,AT('ORDHDR.REP1',lcRpExp) ))+4   )
ELSE
  lcTempV = lcRpExp
ENDIF
IF ATC(')AND',lcTempV) # 0 .OR. ATC(') AND',lcTempV) # 0
  *-- Get the exact REP1 expression
  lcTempV = LEFT(lcTempV,ATC('AND',lcTempV)-1 )
ENDIF
*-- Add an expression pretty much like REP1 but of REP2
lcExpr  = '(' + lcTempV + '.OR.' + STRTRAN(lcTempV,'ORDHDR.REP1','ORDHDR.REP2') + ')'
*-- Form the new expression
lcNewExp = STRTRAN(lcRpExp,lcTempV,lcExpr)

RETURN
*:B802816,1 RAMY (end)
*!*************************************************************
*! Name      : lfFillVars
*: Developer : ABDOU ELGENDI - (ABD)
*! Date      : 04/12/2000
*! Purpose   : Fill most of report memory variables.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfFillVars()
*!*************************************************************
*B603547,1
FUNCTION lfFillVars

IF !USED('SYCCOMP')
  USE &gcSysHome.SYCCOMP ORDER TAG cComp_ID IN 0
  llOpenComp = .T.
ENDIF  
IF llMultCurr
  *-- Open international file.
  IF !USED("SYCINT")
    USE (gcSysHome+"SYCINT.DBF") IN 0 
    llOpenInt = .T.
  ENDIF

  *-- Open exchange rates file.
  IF !USED("SYCEXCH")
    USE (gcSysHome+"SYCEXCH.DBF") IN 0 ORDER TAG Currency
    llOpenExch = .T.
  ENDIF  

  *-- Fill Currency arrays [Begin]
  DIMENSION laCurrVal[1,1]
  *-- Open Currency file.
  IF !USED('SYCCURR')
    llOpenCurr = gfOpenFile(gcSysHome+'SYCCURR',gcSysHome+'Ccurrcode','SH')
  ELSE
    SELECT SYCCURR
    SET ORDER TO CCURRCODE  && To VALIDATE currency code.
  ENDIF

  SELECT DISTINCT CCURRCODE FROM SYCCURR ORDER BY CCURRCODE INTO ARRAY laCurrVal
  DIMENSION laCurrDesc[ALEN(laCurrVal,1),1]

  FOR lnI = 1 TO ALEN(laCurrVal,1)
    = SEEK(ALLTRIM(laCurrVal[lnI,1]))
    laCurrVal[lnI,1]  = PADR(laCurrVal[lnI,1],3)
    laCurrDesc[lnI,1] = CCURRCODE + ' - ' + ALLTRIM(CCURRDESC)
  ENDFOR
  *-- Fill Currency arrays [End  ]
ENDIF

*-- End Of lfFillVars.
*!*************************************************************
*! Name      : lfClearRep
*: Developer : ABDOU ELGENDI - (ABD)
*! Date      : 04/12/2000
*! Purpose   : Function that we call when Close the option grid.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfClearRep()
*!*************************************************************
*B603547,1
FUNCTION lfClearRep
IF llOpenComp AND USED('SYCCOMP')
  USE IN SYCCOMP
ENDIF

IF llMultCurr
  SET CURRENCY TO lcCurrSymb
  SET CURRENCY &lcCurAlign

  IF llOpenInt AND USED("SYCINT")
    USE IN SYCINT 
  ENDIF

  IF llOpenCurr AND USED("SYCCURR")
    USE IN SYCCURR
  ENDIF

  IF llOpenExch AND USED("SYCEXCH")
    USE IN SYCEXCH
  ENDIF  
ENDIF
*-- End Of lfClearRep.
*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 04/12/2000
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : lfObjState,lfSelcObjs,gfGetMemVar
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************
*B603547,1
FUNCTION lfwRepWhen

**B603547,1 Evaluate Currency filter postion. [Begin]
IF llMultCurr
  lnCurrPos  = lfItmPos('ORDHDR.CCURRCODE')
ELSE
  lcRpCurr = "O"
ENDIF  
*B603547,1 Evaluate Currency filter postion. [End  ]

*:B803305,1 ABD  variable that get the position of RepCode.[begin]
lnOrdPos = lfItmPos('ORDHDR.REP1')
*:B803305,1 ABD  [End]


*-- end of lfwRepWhen.
*!*************************************************************
*! Name      : lfItmPos
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 04/12/2000
*! Purpose   : Evaluate fixed filter position within array.
*!*************************************************************
*! Passed Parameters  : ...
*!*************************************************************
*! Returns            : Position
*!*************************************************************
*! Example   : = lfItmPos()
*!*************************************************************
*!B603547,1 
FUNCTION lfItmPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos

lnItmPos = ASCAN(laOGFxFlt,lcItmInFlt)
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(laOGFxFlt,lnItmPos,1)
ENDIF
RETURN lnItmPos

*-- End Of lfItmPos.

*!*************************************************************
*! Name      : lfSetCurr
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 04/12/2000
*! Purpose   : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Called from : From the FRX.
*!*************************************************************
*! Example   : =lfSetCurr()
*!*************************************************************
*B603547,1
Function lfSetCurr
lcGrpCurr = cCurrCode
llSameGrp = .T.
Return ""
*-- End Of lfSetCurr.
*!*************************************************************
*! Name      : lfSameCurr
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 04/12/2000
*! Purpose   : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Called from : From the FRX.
*!*************************************************************
*! Example     : =lfSameCurr()
*!*************************************************************
*B603547,1
Function lfSameCurr
IF (lcRpCurr= "F") AND llSameGrp
  llSameGrp = (lcGrpCurr = cCurrCode)
ENDIF

Return ""

*-- End Of lfSameCurr.
*!*************************************************************
*! Name      : lfCollect
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 04/12/2000
*! Purpose   : function to collect the correct data.
*!*************************************************************
*! Called from : None.
*!*************************************************************
*! Calls       : None.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfCollect()
*!*************************************************************
*B803305,1
FUNCTION lfCollect

lcTOrdFile = laOGFxFlt[lnOrdPos,6]
llWorkDeal = !EMPTY(lcTOrdFile) AND USED(lcTOrdFile) AND RECCOUNT(lcTOrdFile) > 0
IF llWorkDeal
  SELECT (lcOrdhTmp)  
  COPY STRUCTURE TO (gcWorkDir+lcOrdhTmp1)
  =gfOpenFile(gcWorkDir+lcOrdhTmp1,'','SH')
  SELECT (lcOrdhTmp)  
  SCAN
    *B605089,1 (Begin) Consider if salesrep Commession based on Division.
    *--If NOT based on Division or Based but Neither Rep1 nor Rep2 was assigned any dividion,
    *--go normally
    IF !llRep_div OR (!SEEK(Rep1,'REP_DIV') AND !SEEK(Rep2,'REP_DIV'))
    *B605089,1 (End)
      DO CASE 
        *-- REP1 is the required
        CASE !EMPTY(REP1) .AND. EMPTY(REP2)
          REPLACE FLAG WITH '1'
        *-- Both rep and rep2 are required
        CASE !EMPTY(REP1) .AND.  SEEK(REP1,lcTOrdFile)
          REPLACE FLAG WITH '1'
          IF !EMPTY(REP2) .AND. SEEK(REP2,lcTOrdFile)
            SCATTER MEMVAR MEMO
            M.fLAG = '2'
            INSERT INTO (lcOrdhTmp1) From MEMVAR
          ENDIF
        *-- Both rep and rep2 are required
        CASE !EMPTY(REP1) .AND.  !SEEK(REP1,lcTOrdFile) .AND. SEEK(REP2,lcTOrdFile)
          REPLACE FLAG WITH '2'
        *-- Rep2 is the required
        CASE EMPTY(REP1) .AND. SEEK(REP2,lcTOrdFile)
          REPLACE FLAG WITH '2'
      ENDCASE
    *B605089,1 (Begin) Consider if salesrep Commession based on Division.
    ELSE
      *--If this order has Rep2 and this order division is the division assigned to
      *--Rep2 in REP_DIV file, then this order belongs to Rep2 not Rep1
      IF !EMPTY(Rep2) AND SEEK(Rep2+cDivision,'REP_DIV') AND cDivision = REP_DIV.cDivision
        *-- Print it only when the user wants to print it.     
        IF SEEK(REP2,lcTOrdFile)
          REPLACE Flag WITH '2'
        ELSE
          *--This record belongs to Rep2 But the user doesn't want to print it.
          BLANK
          DELETE
        ENDIF
      ELSE
        *--Otherwise it belongs to Rep1, check if he wants to print it.
        IF !SEEK(REP1,lcTOrdFile)
          *--This record belongs to Rep1 But the use doesn't want to print it.
          BLANK
          DELETE
        ENDIF  
      ENDIF
    ENDIF
    *B605089,1 (End)
  ENDSCAN
  APPEND FROM (gcWorkDir+lcOrdhTmp1)
ELSE
  =gfOpenFile(gcWorkDir+lcOrdhTmp,'','SH')
  *B605089,1 (Begin) Consider if salesrep Commession based on Division.
  *!*COPY ALL TO (gcWorkDir+lcOrdhTmp1) FOR .NOT. EMPTY(Rep2) .AND. .NOT. EMPTY(Comm2)
  *!*=gfOpenFile(gcWorkDir+lcOrdhTmp1,'','SH')
  *!*REPLACE ALL FLAG WITH '2'
  *!*SELECT &lcOrdhTmp
  *!*APPEND FROM (gcWorkDir+lcOrdhTmp1)
  *--If Sales rep based on division
  IF llRep_div
    COPY STRUCTURE TO (gcWorkDir+lcOrdhTmp1)
    =gfOpenFile(gcWorkDir+lcOrdhTmp1,'','SH')
    SELECT (lcOrdhTmp)
    SCAN
      IF SEEK(Rep1,'REP_DIV') OR SEEK(Rep2,'REP_DIV')
        *--If this order has Rep2 and this order division is the division assigned to
        *--Rep2 in REP_DIV file, then this order belongs to Rep2 not Rep1
        *--Otherwise do nothing as the current record already belongs to Rep1
        IF !EMPTY(Rep2) AND SEEK(Rep2+cDivision,'REP_DIV') AND cDivision = REP_DIV.cDivision
          REPLACE Flag WITH '2'
        ENDIF
      ELSE
        *--If neither Rep1 nor Rep2 was assigned a Division, Work Normally.
        SCATTER MEMVAR MEMO
        M.fLAG = '2'
        INSERT INTO (lcOrdhTmp1) From MEMVAR
      ENDIF
    ENDSCAN
    SELECT &lcOrdhTmp
    APPEND FROM (gcWorkDir+lcOrdhTmp1)
  ELSE
    *--If Sales rep based on Company, go normally.
    COPY ALL TO (gcWorkDir+lcOrdhTmp1) FOR .NOT. EMPTY(Rep2) .AND. .NOT. EMPTY(Comm2)
    =gfOpenFile(gcWorkDir+lcOrdhTmp1,'','SH')
    REPLACE ALL FLAG WITH '2'
    SELECT &lcOrdhTmp
    APPEND FROM (gcWorkDir+lcOrdhTmp1)
  ENDIF
  *B605089,1 (End)
ENDIF

*!*************************************************************