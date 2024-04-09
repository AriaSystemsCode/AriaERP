*!**************************************************************************
*! Name      : SOGENCN.PRG
*! Developer : Mariam Mazhar (MMT)
*! Date      : 06/27/2022
*! Purpose   : Generate Sales Order from Contract
*! Tracking Entry: E612573
*!**************************************************************************
*B612634,1 MMT 10/23/2022 Adjust the item description on Generate SO from contract to reduce the number of characters used for period description[T20221019.0001]
*!**************************************************************************
#INCLUDE R:\Aria4xp\Prgs\SO\sogencn.h
PUBLIC lcFrequency  ,lnDataSessPre,llCallOption,laScopExpr,llSaveCriteria

DECLARE laScopExpr[1]

STORE "" TO laScopExpr
lnPrintStatV = 1
llCallOption = .F.
lndataSessPre = ""
llSaveCriteria = .F.


DO FORM (oAriaApplication.ScreenHome+"\SO\SOGENCN.SCX")

*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Mariam Mazhar (MMT)
*! Date      : 06/27/2022
*! Purpose   : OG When function
*!*************************************************************
FUNCTION lfwRepWhen

*!*************************************************************
*! Name      : lfvScope
*! Developer : Mariam Mazhar (MMT)
*! Date      : 06/27/2022
*! Purpose   : Scope function
*!*************************************************************
FUNCTION lfvScope
LPARAMETERS loFormSet
 lndataSessPre = SET("Datasession" )
 IF loFormSet.llCalledFromOp
   llCallOption = loFormSet.llCalledFromOp
 *  =ACOPY(loFormSet.laSeleCritria,laScopExpr)
 ENDIF
 *N037426,1 MMT 08/11/2005,make option grid opened after screen opened [START]
 loFormSet.llCallScop = .F.             && Screen Already Initialized
 *N037426,1 MMT 08/11/2005,make option grid opened after screen opened [END]
 lcDataSessI = SET("Datasession" )&&THIS.loFormSet.DatasessionID

 lcExpr = gfOpGrid('SOGENCN' , .T.)&&,.F.,.F.,.T.,.T.)
 SET DATASESSION TO lcDataSessI

 IF lcExpr <> ".F."
   lfCrTmpSO(loFormSet)
   =ACOPY(laScopExpr,loFormSet.laSeleCritria)
   SELECT (loFormSet.oFormEnvironment.lcOrdHdr)
   LOCATE
   loFormSet.lnSelRec   =  RECCOUNT()
   loFormSet.lnDelRec   = 0
   *loFormSet.loFormSet.llenablerel = .F.
   loFormSet.lnUnSelRec = 0
   IF !EOF()
     loFormSet.llEnableInvert = .T.
     loFormSet.llEnableSelect = .T.
     loFormSet.llEnableSelectall = .T.
     loFormSet.llEnableSelectnone = .F.
   ELSE    && Else
     loFormSet.llEnableInvert = .F.
     loFormSet.llEnableSelect = .F.
     loFormSet.llEnableSelectAll = .F.
     loFormSet.llEnableSelectNone = .F.
   ENDIF    && End of IF
 ELSE
   loFormSet.llEnableInvert = .F.
   loFormSet.llEnableSelect = .F.
   loFormSet.llEnableSelectAll = .F.
   loFormSet.llEnableSelectNone = .F.
   RETURN
 ENDIF
 
*!*************************************************************
*! Name      : lfCrTmpSO
*! Developer : Mariam Mazhar (MMT)
*! Date      : 06/27/2022
*! Purpose   : Collect data based on OG Criteria
*!*************************************************************
FUNCTION lfCrTmpSO
LPARAMETERS loFormSet
=gfOpenTABLe(oAriaApplication.DataDir+'OrdLine',oAriaApplication.DataDir+'ORDLINE','SH')
=gfOpenTABLe(oAriaApplication.DataDir+'Scale',oAriaApplication.DataDir+'Scale','SH')
=gfOpenTABLe(oAriaApplication.DataDir+'Style',oAriaApplication.DataDir+'Style','SH')
=gfOpenTABLe(oAriaApplication.DataDir+'Customer',oAriaApplication.DataDir+'Customer','SH')
=gfOpenTABLe(OARIAAPPLICATION.DATADIR+'SALESREP',OARIAAPPLICATION.DATADIR+'SALESREP','SH')
=gfOpenTABLe(OARIAAPPLICATION.SYSPATH+'SYCFACT',OARIAAPPLICATION.SYSPATH+'CFACCODE','SH')
=IIF(loFormSet.OFORMENVIRONMENT.LASETUPS[4,2]='Y',gfOpenTABLe(OARIAAPPLICATION.DATADIR+'GL_LINK',OARIAAPPLICATION.DATADIR+'GL_LINK1','SH'),.T.)
=IIF(loFormSet.OFORMENVIRONMENT.LASETUPS[5,2]='Y',gfOpenTABLe(OARIAAPPLICATION.DATADIR+'WAREHOUS',OARIAAPPLICATION.DATADIR+'WAREHOUS','SH'),.T.)
=IIF(loFormSet.OFORMENVIRONMENT.LASETUPS[1,2]='Y',gfOpenTABLe(OARIAAPPLICATION.DATADIR+'SPCK_LIN',OARIAAPPLICATION.DATADIR+'SPCK_LIN','SH'),.T.)
*B612634,1 MMT 10/23/2022 Adjust the item description on Generate SO from contract to reduce the number of characters used for period description[T20221019.0001][Start]
IF (loFormSet.OFORMENVIRONMENT.LASETUPS[5,2]='Y')
*B612634,1 MMT 10/23/2022 Adjust the item description on Generate SO from contract to reduce the number of characters used for period description[T20221019.0001][End]
  SELECT WAREHOUS
  SET FILTER TO LSTYINV
*B612634,1 MMT 10/23/2022 Adjust the item description on Generate SO from contract to reduce the number of characters used for period description[T20221019.0001][Start]
ENDIF
*B612634,1 MMT 10/23/2022 Adjust the item description on Generate SO from contract to reduce the number of characters used for period description[T20221019.0001][End]
=gfOpenTABLe(OARIAAPPLICATION.DATADIR + 'PROFVALU', 'PROFILE', 'SH')

=gfOpenFile(oAriaApplication.DataDir+'ORDCANLN',oAriaApplication.DataDir+'ORDCANLN','SH')
=AFIELDS(laFileStru)
=gfCrtTmp(loFormSet.lcOrdCanLn,@laFileStru,[CORDTYPE+ORDER+STR(LINENO,6)],loFormSet.lcOrdCanLn)

*-- Create Temporary Order lines files
SELECT OrdLine
SET STEP ON 
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
IF loFormSet.OFORMENVIRONMENT.laSetups[24,2] 
  DIMENSION laFileStru[lnFileStru+5,18]
ELSE
  DIMENSION laFileStru[lnFileStru+4,18]
ENDIF
WITH loFormSet.AriaForm1.Ariapageframe1.Page2
  .grdEditLines.RecordSource =''
  WITH .AriaEditRegion1.keyStyle
    laFileStru[lnFileStru+1,1] = 'lContract'
    laFileStru[lnFileStru+1,2] = 'L'
    laFileStru[lnFileStru+1,3] = 1
    laFileStru[lnFileStru+1,4] = 0
    laFileStru[lnFileStru+2,1] = 'cMajor'
    laFileStru[lnFileStru+2,2] = 'C'
    laFileStru[lnFileStru+2,3] = IIF(.lnLastPart >=1,LEN(.lcItemMjrPict),1)
    laFileStru[lnFileStru+2,4] = 0
    laFileStru[lnFileStru+3,1] = 'cNonMajor'
    laFileStru[lnFileStru+3,2] = 'C'
    laFileStru[lnFileStru+3,3] = IIF(.lnLastPart >=2,LEN(.lcItemNMjPict),1)
    laFileStru[lnFileStru+3,4] = 0
    laFileStru[lnFileStru+4,1] = 'cMjrScale'
    laFileStru[lnFileStru+4,2] = 'C'
    laFileStru[lnFileStru+4,3] =  IIF(.lnLastPart >=3,LEN(.lcScalePict) ,1)
    laFileStru[lnFileStru+4,4] = 0
  ENDWITH
ENDWITH

IF loFormSet.OFORMENVIRONMENT.laSetups[24,2] 
  laFileStru[lnFileStru+5,1] = 'TRD_price'
  laFileStru[lnFileStru+5,2] = 'N'
  laFileStru[lnFileStru+5,3] =  12
  laFileStru[lnFileStru+5,4] = 2
ENDIF


lnFileStru1 = ALEN(laFileStru,1)
 DIMENSION laFileStru[lnFileStru1+1,18]
  laFileStru[lnFileStru1+1,1] = 'cvensty'
    laFileStru[lnFileStru1+1,2] = 'C'
    laFileStru[lnFileStru1+1,3] = 19
    laFileStru[lnFileStru1+1,4] = 0
FOR lnCount = 1 TO IIF(loFormSet.OFORMENVIRONMENT.laSetups[24,2],6,5)
  STORE '' TO laFileStru[lnFileStru+lnCount,7],laFileStru[lnFileStru+lnCount,8],laFileStru[lnFileStru+lnCount,9],;
              laFileStru[lnFileStru+lnCount,10],laFileStru[lnFileStru+lnCount,11],laFileStru[lnFileStru+lnCount,12],;
              laFileStru[lnFileStru+lnCount,13],laFileStru[lnFileStru+lnCount,14],laFileStru[lnFileStru+lnCount,15],;
              laFileStru[lnFileStru+lnCount,16]
  STORE 0 TO  laFileStru[lnFileStru+lnCount,17],laFileStru[lnFileStru+lnCount,18]
ENDFOR


DECLARE laIndex[4,2]

laIndex[1,1] = 'cOrdType+ORDER+STORE+STYLE+DYELOT+STR(LINENO,6)'
laIndex[1,2] = 'ORDLINST'
laIndex[2,1] = 'cOrdType+ORDER+STYLE+STORE+STR(LINENO,6)'
laIndex[2,2] = 'ORDLINES'
laIndex[3,1] = 'cOrdType+ORDER+STR(LINENO,6)'
laIndex[3,2] = 'ORDLINE'
laIndex[4,1] = 'Order+Store+STYLE+Dyelot+STR(LineNo,6)'
laIndex[4,2] = 'CONFIGLIN'

=gfCrtTmp(loFormSet.oFormEnvironment.lcOrdLine,@laFileStru,@laIndex)
SET ORDER TO TAG 'ORDLINE' IN (loFormSet.oFormEnvironment.lcOrdLine)
*-- Create Template Order lines file
DECLARE laIndex[3,2]
laIndex[1,1] = 'cOrdType+ORDER+STORE+STYLE+STR(LINENO,6)'
laIndex[1,2] = 'ORDLINST'
laIndex[2,1] = 'cOrdType+ORDER+STYLE+STORE+STR(LINENO,6)'
laIndex[2,2] = 'ORDLINES'
laIndex[3,1] = 'cOrdType+ORDER+STR(LINENO,6)'
laIndex[3,2] = 'ORDLINE'
=gfCrtTmp(loFormSet.oFormEnvironment.lcTempline,@laFileStru,@laIndex)

*-- Use Variant Cost Sheet
IF loFormSet.oFormenvironment.laSetups[19,2] 
  =gfOpenFile(oAriaApplication.DataDir+'BomVar',oAriaApplication.DataDir+'BomVar','SH')
  =gfOpenFile(oAriaApplication.DataDir+'ORDDSGN',oAriaApplication.DataDir+'ORDLINE','SH')
  =gfOpenFile(oAriaApplication.DataDir+'ARTWRKDS',oAriaApplication.DataDir+'ARTWRKDS','SH')
  IF .F.
    IF USED(This.lcT_BomVar)
      USE IN (This.lcT_BomVar)
    ENDIF
    IF ASCAN(loFormSet.laEvntTrig,PADR('COLLDATA',10),1,ALEN(loFormSet.laEvntTrig,1),1) > 0
      =loFormSet.mDoTrigger(PADR('COLLDATA',10)) 
    ELSE
      lcOrder = OrdHdr.Order
      *E302303,1 WAM 09/01/2006 changes in design screen
      *SELECT * , "S" AS cStatus, RECNO() AS nRecNo FROM BomVar ;
      WHERE  cIdType+cCost_Id+STR(LineNo,6) = "SO" + lcOrder ;
      INTO DBF (This.lcT_BomVar)
      *INDEX ON cIdType+cCost_Id+STR(LineNo,6) TAG (This.lcT_BomVar)

      SELECT * , "S" AS cStatus, RECNO() AS nRecNo FROM ORDDSGN ;
      WHERE  Order+STR(LineNo,6) = lcOrder ;
      INTO DBF (This.lcT_BomVar)
      INDEX ON order+cordline+STR(lineno,6) TAG ORDLINE
      *E302303,1 WAM 09/01/2006 (End)

    ENDIF
  ELSE
    SELECT BomVar
    =AFIELDS(laFileStru)
    lnFileStru = ALEN(laFileStru,1)

    *B607706,1 MMT 09/15/2005  Fix of error when insering new line a new so [Start]
    *DIMENSION laFileStru[lnFileStru+2,16]
    DIMENSION laFileStru[lnFileStru+2,18]
    *B607706,1 MMT 09/15/2005  Fix of error when insering new line a new so [End]    

    laFileStru[lnFileStru+1,1] = 'nRecno'
    laFileStru[lnFileStru+1,2] = 'N'
    laFileStru[lnFileStru+1,3] = 10
    laFileStru[lnFileStru+1,4] = 0
    laFileStru[lnFileStru+2,1] = 'cStatus'
    laFileStru[lnFileStru+2,2] = 'C'
    laFileStru[lnFileStru+2,3] = 1
    laFileStru[lnFileStru+2,4] = 0

    FOR lnLoop = 1 to  2
       STORE ' ' TO  laFileStru[lnFileStru+lnLoop,7],laFileStru[lnFileStru+lnLoop,8],;
                laFileStru[lnFileStru+lnLoop,9],laFileStru[lnFileStru+lnLoop,10],;
                laFileStru[lnFileStru+lnLoop,11],laFileStru[lnFileStru+lnLoop,12],;
                laFileStru[lnFileStru+lnLoop,13],laFileStru[lnFileStru+lnLoop,14],;
                laFileStru[lnFileStru+lnLoop,15],laFileStru[lnFileStru+lnLoop,16]
      STORE 0 TO    laFileStru[lnFileStru+lnLoop,17] ,laFileStru[lnFileStru+lnLoop,18]
    ENDFOR


*!*	    IF ASCAN(Thisformset.laEvntTrig,PADR('CRBOMVAR',10),1,ALEN(Thisformset.laEvntTrig,1),1) > 0
*!*	      =This.mDoTrigger(PADR('CRBOMVAR',10)) 
*!*	    ELSE 
      IF USED(loFormSet.lcT_BomVar)
        ZAP IN (loFormSet.lcT_BomVar)
      ELSE
        SELECT ORDDSGN
        =AFIELDS(laFileStru)
        =gfCrtTmp(loFormSet.lcT_BomVar,@laFileStru,[order+cordline+STR(lineno,6)],'ORDLINE')
      ENDIF
*!*	    ENDIF
  ENDIF
ENDIF

SELECT ORDHDR
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+2,18]
laFileStru[lnFileStru+1,1] = 'llSel'
laFileStru[lnFileStru+1,2] = 'L'
laFileStru[lnFileStru+1,3] = 1
laFileStru[lnFileStru+1,4] = 0
laFileStru[lnFileStru+2,1] = 'NewOrder'
laFileStru[lnFileStru+2,2] = 'C'
laFileStru[lnFileStru+2,3] = 6
laFileStru[lnFileStru+2,4] = 0

FOR lnLoop = 1 to  2
   STORE ' ' TO  laFileStru[lnFileStru+lnLoop,7],laFileStru[lnFileStru+lnLoop,8],;
            laFileStru[lnFileStru+lnLoop,9],laFileStru[lnFileStru+lnLoop,10],;
            laFileStru[lnFileStru+lnLoop,11],laFileStru[lnFileStru+lnLoop,12],;
            laFileStru[lnFileStru+lnLoop,13],laFileStru[lnFileStru+lnLoop,14],;
            laFileStru[lnFileStru+lnLoop,15],laFileStru[lnFileStru+lnLoop,16]
  STORE 0 TO    laFileStru[lnFileStru+lnLoop,17] ,laFileStru[lnFileStru+lnLoop,18]
ENDFOR

DECLARE laIndex[1,2]
laIndex[1,1] = 'cordtype+order'
laIndex[1,2] = loFormSet.oFormEnvironment.lcOrdHdr
=gfCrtTmp(loFormSet.oFormEnvironment.lcOrdHdr,@laFileStru,@laIndex)

*loFormSet.llUpdate = .T.

*check if user has selected a piktkt or not
llSeleCust = .F. && flag to indicate if there is any piktkt selected
lnPosCust = ASCAN(laScopExpr,"CUSTOMER.ACCOUNT")
IF lnPosCust > 0
  lnPosCust = ASUBSCRIPT(laScopExpr,lnPosCust ,1)
  lcFileCust =IIF(!EMPTY(laScopExpr[lnPosCust ,6]),laScopExpr[lnPosCust ,6],'')
  IF !EMPTY(lcFileCust) AND USED(lcFileCust) AND RECCOUNT(lcFileCust)> 0
    llSeleCust = .T.
  ENDIF
ENDIF

llSeleOrder = .F. && flag to indicate if there is any piktkt selected
lnPosOrder = ASCAN(laScopExpr,"ORDHDR.ORDER")
IF lnPosOrder> 0
  lnPosOrder= ASUBSCRIPT(laScopExpr,lnPosOrder,1)
  lcFileOrder =IIF(!EMPTY(laScopExpr[lnPosOrder,6]),laScopExpr[lnPosOrder,6],'')
  IF !EMPTY(lcFileOrder) AND USED(lcFileOrder) AND RECCOUNT(lcFileOrder)> 0
    llSeleOrder= .T.
  ENDIF
ENDIF

ldEnteredEnd = {}
ldEnteredStart ={}
llEnteredDateSel = .F. && flag to indicate if user Selected date range or not
lcDate = ""
lnPosEnteredDate = ASCAN(laScopExpr,"ORDHDR.ENTERED")
IF lnPosEnteredDate > 0
  lnPosEnteredDate = ASUBSCRIPT(laScopExpr,lnPosEnteredDate ,1)
  lcDateEntered =IIF(!EMPTY(laScopExpr[lnPosEnteredDate ,6]),laScopExpr[lnPosEnteredDate ,6],'')
  IF !EMPTY(lcDateEntered)
    llEnteredDateSel = .T.
    ldEnteredEnd =CTOD(SUBSTR(lcDateEntered ,ATC('|',lcDateEntered )+1))
    ldEnteredStart = CTOD(SUBSTR(lcDateEntered ,1,ATC('|',lcDateEntered )-1))
  ENDIF
ENDIF


ldCompleteEnd = {}
ldCompleteStart ={}
llCompleteDateSel = .F. && flag to indicate if user Selected date range or not
lnPosCompleteDate = ASCAN(laScopExpr,"ORDHDR.COMPLETE")
IF lnPosCompleteDate > 0
  lnPosCompleteDate = ASUBSCRIPT(laScopExpr,lnPosCompleteDate ,1)
  lcDateComp =IIF(!EMPTY(laScopExpr[lnPosCompleteDate ,6]),laScopExpr[lnPosCompleteDate ,6],'')
  IF !EMPTY(lcDateComp)
    llCompleteDateSel = .T.
    ldCompleteEnd =CTOD(SUBSTR(lcDateComp,ATC('|',lcDateComp)+1))
    ldCompleteStart = CTOD(SUBSTR(lcDateComp,1,ATC('|',lcDateComp)-1))
  ENDIF
ENDIF

ldStartEnd = {}
ldStartStart ={}
llStartDateSel = .F. && flag to indicate if user Selected date range or not
lnPosStartDate = ASCAN(laScopExpr,"ORDHDR.START")
IF lnPosStartDate > 0
  lnPosStartDate= ASUBSCRIPT(laScopExpr,lnPosStartDate,1)
  lcDateStart =IIF(!EMPTY(laScopExpr[lnPosStartDate,6]),laScopExpr[lnPosStartDate,6],'')
  IF !EMPTY(lcDateStart)
    llStartDateSel = .T.
    ldStartEnd =CTOD(SUBSTR(lcDateStart,ATC('|',lcDateStart)+1))
    ldStartStart = CTOD(SUBSTR(lcDateStart,1,ATC('|',lcDateStart)-1))
  ENDIF
ENDIF

ldLastGenEnd = {}
ldLastGenStart ={}
llLastGenDateSel = .F. && flag to indicate if user Selected date range or not
lnPosLastGenDate = ASCAN(laScopExpr,"ORDHDR.COMPLETED")
IF lnPosLastGenDate > 0
  lnPosLastGenDate= ASUBSCRIPT(laScopExpr,lnPosLastGenDate ,1)
  lcDateLastGen =IIF(!EMPTY(laScopExpr[lnPosLastGenDate ,6]),laScopExpr[lnPosLastGenDate ,6],'')
  IF !EMPTY(lcDateLastGen)
    llLastGenDateSel = .T.
    ldLastGenEnd =CTOD(SUBSTR(lcDateLastGen ,ATC('|',lcDateLastGen )+1))
    ldLastGenStart = CTOD(SUBSTR(lcDateLastGen ,1,ATC('|',lcDateLastGen )-1))
  ENDIF
ENDIF

lcTermFile =''
llTermCode  = .F.
lnTermCodePos = ASCAN(laScopExpr,"ORDHDR.CTERMCODE")
IF lnTermCodePos > 0
  lnTermCodePos = ASUBSCRIPT(laScopExpr,lnTermCodePos,1)
  lcTermCodeSel =IIF(!EMPTY(laScopExpr[lnTermCodePos ,6]),laScopExpr[lnTermCodePos ,6],'')
  IF !EMPTY(lcTermCodeSel)
    lcTermFile = gfTempName()
    llTermCode  = IIF(LEN(lcTermCodeSel)>0,.T.,.F.) AND lfConvertToCursor(lcTermCodeSel,'CTERMCODE',lcTermFile)
  ENDIF
ENDIF

IF !USED('CONTRACTINF')
  =gfOpenTable('CONTRACTINF','CONTRACTIN')
ENDIF

lnMonCnt = 1
DO CASE
  CASE lcFrequency  ='M'
    lnMonCnt = 1
  CASE lcFrequency  ='Q'
    lnMonCnt = 3
  CASE lcFrequency  ='S'
    lnMonCnt = 6
  CASE lcFrequency  ='A'
    lnMonCnt = 12
ENDCASE 



DO CASE 
  CASE llSeleOrder 
    SELECT ORDHDR
    =gfSetOrder('ORDHDR')
    SELECT (lcFileOrder)
    LOCATE
    SCAN
      SELECT ORDHDR
      =SEEK('C'+&lcFileOrder..ORDER)
      SCAN REST WHILE CORDTYPE+ORDER = 'C'+&lcFileOrder..ORDER FOR IIF(llSeleCust,SEEK(ORDHDR.Account,lcFileCust),.T.)  AND ;
        IIF(llTermCode,SEEK(ORDHDR.CTERMCODE,lcTermFile),.T.) AND;
        IIF(llStartDateSel ,BETWEEN(ORDHDR.START,ldStartStart,ldStartEnd),.T.) AND ;
        IIF(llCompleteDateSel ,BETWEEN(ORDHDR.COMPLETE,ldCompleteStart ,ldCompleteEnd ),.T.) AND ;
        IIF(llEnteredDateSel ,BETWEEN(ORDHDR.ENTERED,ldEnteredStart ,ldEnteredEnd ),.T.) AND;
        !INLIST(Status,'X','B') &&.AND. BETWEEN(oAriaApplication.SystemDate,START,COMPLETE)
        
        IF !gfSeek(ORDHDR.ORDER,'CONTRACTINF','CONTRACTIN')
          LOOP 
        ELSE
          IF !CONTRACTINF.LRECURRING  OR IIF(lcFrequency ='L',.F. ,ALLTRIM(CONTRACTINF.CFREQUENCY) != lcFrequency ) 
            SELECT ORDHDR                     
            LOOP  
          ENDIF
          if llLastGenDateSel 
            IF !BETWEEN(CONTRACTINF.DLASTGEN ,ldLastGenStart ,ldLastGenEnd )
              SELECT ORDHDR
              LOOP
            ENDIF
          ENDIF
        ENDIF
        *XXX
        ldLastPeriodEndDate =IIF(EMPTY(CONTRACTINF.DLASTGEN) or ISNULL(CONTRACTINF.DLASTGEN),ORDHDR.Start-1,CONTRACTINF.DLASTGEN)
        lcFreq = CONTRACTINF.CFREQUENCY
        DO CASE
          CASE lcFreq ='M'
            lnMonCnt = 1
            *lnMonCnt = 30
          CASE lcFreq ='Q'
            lnMonCnt = 3
            *lnMonCnt = 90
          CASE lcFreq ='S'
            lnMonCnt = 6
            *lnMonCnt = 180
          CASE lcFreq ='A'
            lnMonCnt = 12
            *lnMonCnt = 365
        ENDCASE 
        *XXX
        
        SELECT ORDHDR
        SCATTER MEMO MEMVAR 
        m.Entered = oAriaApplication.SystemDate
        m.Start = ldLastPeriodEndDate +1&&DATE(YEAR(GOMONTH(oAriaApplication.SystemDate,1)),MONTH(GOMONTH(oAriaApplication.SystemDate,1)),1)
       * m.Complete = DATE(YEAR(GOMONTH(m.Start,lnMonCnt+1)),MONTH(GOMONTH(m.Start,lnMonCnt+1)),1)-1
        *m.Complete = m.Start + lnMonCnt 
        m.Complete = GOMONTH(m.Start,lnMonCnt)-1
        IF  m.Complete > ORDHDR.Complete
          =gfModalGen('INM32151B00000','DIALOG',Ordhdr.Order+ '|' + DTOC(m.Complete)+'|' + DTOC(Ordhdr.Complete) )
          SELECT ORDHDR
          LOOP 
        ENDIF
        m.Note1 = ALLTRIM(m.Note1)+IIF(oAriaApplication.oActivelang.cLang_ID = "EN" OR TYPE('LOFORMSET')<>'O',LANG_GEN_FROM_CONTRACT,LOFORMSET.GetHeaderText("LANG_GEN_FROM_CONTRACT",LOFORMSET.HeaderAlias)) +ORDHDR.ORDER
        m.cOrdType ='O'
        m.llSel= .T.
        m.cadd_user = ''
        m.cadd_time = ''
        INSERT INTO (loFormSet.oFormEnvironment.lcOrdHdr) FROM MEMVAR
        =gfAdd_Info(loFormSet.oFormEnvironment.lcOrdHdr)
        SELECT ORDLINE
        =SEEK(ORDHDR.CORDTYPE+ORDHDR.ORDER)
        SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6) =  ORDHDR.CORDTYPE+ORDHDR.ORDER
          SCATTER MEMO MEMVAR 
          m.cOrdType ='O'
*!*            m.Start = DATE(YEAR(GOMONTH(oAriaApplication.SystemDate,1)),MONTH(GOMONTH(oAriaApplication.SystemDate,1)),1)
*!*            m.Complete = DATE(YEAR(GOMONTH(oAriaApplication.SystemDate,lnMonCnt+1)),MONTH(GOMONTH(oAriaApplication.SystemDate,lnMonCnt+1)),1)-1
          m.Start = ldLastPeriodEndDate +1&&DATE(YEAR(GOMONTH(oAriaApplication.SystemDate,1)),MONTH(GOMONTH(oAriaApplication.SystemDate,1)),1)
          *m.Complete = DATE(YEAR(GOMONTH(m.Start,lnMonCnt+1)),MONTH(GOMONTH(m.Start,lnMonCnt+1)),1)-1
          *m.Complete = m.Start + lnMonCnt 
          m.Complete = GOMONTH(m.Start,lnMonCnt)-1
          *m.Desc1 = ALLTRIM(m.Desc1)+" "+ ALLTRIM(CMONTH(m.Complete))+" "+ALLTRIM(STR(YEAR(m.Complete)))
          *m.Desc1 = ALLTRIM(m.Desc1)+" "+ "Period: From:"+DTOC(m.Start)+" To:"+DTOC(m.Complete)
          *B612634,1 MMT 10/23/2022 Adjust the item description on Generate SO from contract to reduce the number of characters used for period description[T20221019.0001][Start]
*!*            IF LEN(ALLTRIM(m.Desc1))>22
*!*              m.Desc1 = SUBSTR(ALLTRIM(m.Desc1),1,22)
*!*            ENDIF
*!*            m.Desc1 = ALLTRIM(m.Desc1)+" "+;
*!*            IIF(oAriaApplication.oActivelang.cLang_ID = "EN" OR TYPE('LOFORMSET')<>'O',LANG_PERIOD_FROM,LOFORMSET.GetHeaderText("LANG_PERIOD_FROM",LOFORMSET.HeaderAlias))+;
*!*             DTOC(m.Start)+ ;
*!*             IIF(oAriaApplication.oActivelang.cLang_ID = "EN" OR TYPE('LOFORMSET')<>'O',LANG_TO,LOFORMSET.GetHeaderText("LANG_TO",LOFORMSET.HeaderAlias)) +;
*!*             DTOC(m.Complete)
          IF LEN(ALLTRIM(m.Desc1))>42
            m.Desc1 = SUBSTR(ALLTRIM(m.Desc1),1,42)
          ENDIF
          lcCentSet = SET("Century")
          SET CENTURY OFF 
          m.Desc1 = ALLTRIM(m.Desc1)+" "+;
                    DTOC(m.Start)+"-"+DTOC(m.Complete)
          SET CENTURY &lcCentSet. 
          *B612634,1 MMT 10/23/2022 Adjust the item description on Generate SO from contract to reduce the number of characters used for period description[T20221019.0001][End]           
          m.cadd_user = ''
          m.cadd_time = ''
          m.Flag = 'N'&&Iif(This.MultiStores,'','N')
          INSERT INTO (loFormSet.oFormEnvironment.lcOrdLine) FROM MEMVAR
          =gfAdd_Info(loFormSet.oFormEnvironment.lcOrdLine)
        ENDSCAN 
      ENDSCAN
    ENDSCAN   
  CASE llSeleCust
    SELECT ORDHDR
    =gfSetOrder('ORDACCT')
    SELECT (lcFileCust)
    LOCATE
    SCAN
      SELECT ORDHDR
      =SEEK(&lcFileCust..Account+'C')
      SCAN REST WHILE ACCOUNT+CORDTYPE+ORDER = &lcFileCust..Account+'C' FOR IIF(llTermCode,SEEK(ORDHDR.CTERMCODE,lcTermFile),.T.) AND;
        IIF(llStartDateSel ,BETWEEN(ORDHDR.START,ldStartStart,ldStartEnd),.T.) AND ;
        IIF(llCompleteDateSel ,BETWEEN(ORDHDR.COMPLETE,ldCompleteStart ,ldCompleteEnd ),.T.) AND ;
        IIF(llEnteredDateSel ,BETWEEN(ORDHDR.ENTERED,ldEnteredStart ,ldEnteredEnd ),.T.)  AND;
        !INLIST(Status,'X','B')&& .AND. BETWEEN(oAriaApplication.SystemDate,START,COMPLETE)
        
        IF !gfSeek(ORDHDR.ORDER,'CONTRACTINF','CONTRACTIN')
          LOOP 
        ELSE
          IF !CONTRACTINF.LRECURRING OR IIF(lcFrequency ='L',.F. ,ALLTRIM(CONTRACTINF.CFREQUENCY) != lcFrequency )
            SELECT ORDHDR                     
            LOOP  
          ENDIF
          if llLastGenDateSel 
            IF !BETWEEN(CONTRACTINF.DLASTGEN ,ldLastGenStart ,ldLastGenEnd )
              SELECT ORDHDR
              LOOP
            ENDIF
          ENDIF
        ENDIF
        
        *XXX
        ldLastPeriodEndDate =IIF(EMPTY(CONTRACTINF.DLASTGEN) or ISNULL(CONTRACTINF.DLASTGEN),ORDHDR.Start-1,CONTRACTINF.DLASTGEN)
        lcFreq = CONTRACTINF.CFREQUENCY
        DO CASE
          CASE lcFreq ='M'
            lnMonCnt = 1
            *lnMonCnt = 30
          CASE lcFreq ='Q'
            lnMonCnt = 3
            *lnMonCnt = 90
          CASE lcFreq ='S'
            lnMonCnt = 6
            *lnMonCnt = 180
          CASE lcFreq ='A'
            lnMonCnt = 12
            *lnMonCnt = 365
        ENDCASE 
        *XXX
        
        SELECT ORDHDR
        SCATTER MEMO MEMVAR 
*!*            m.Start = DATE(YEAR(GOMONTH(oAriaApplication.SystemDate,1)),MONTH(GOMONTH(oAriaApplication.SystemDate,1)),1)
*!*            m.Complete = DATE(YEAR(GOMONTH(oAriaApplication.SystemDate,lnMonCnt+1)),MONTH(GOMONTH(oAriaApplication.SystemDate,lnMonCnt+1)),1)-1
        m.Start = ldLastPeriodEndDate +1&&DATE(YEAR(GOMONTH(oAriaApplication.SystemDate,1)),MONTH(GOMONTH(oAriaApplication.SystemDate,1)),1)
        *m.Complete = DATE(YEAR(GOMONTH(m.Start,lnMonCnt+1)),MONTH(GOMONTH(m.Start,lnMonCnt+1)),1)-1
        *m.Complete = m.Start + lnMonCnt 
        m.Complete = GOMONTH(m.Start,lnMonCnt)-1
        *XXX
        IF  m.Complete > ORDHDR.Complete
          =gfModalGen('INM32151B00000','DIALOG',Ordhdr.Order + '|' + DTOC(m.Complete)+'|' + DTOC(Ordhdr.Complete))
          SELECT ORDHDR
          LOOP 
        ENDIF
        *XXX
        m.Note1 = ALLTRIM(m.Note1)+IIF(oAriaApplication.oActivelang.cLang_ID = "EN" OR TYPE('LOFORMSET')<>'O',LANG_GEN_FROM_CONTRACT,LOFORMSET.GetHeaderText("LANG_GEN_FROM_CONTRACT",LOFORMSET.HeaderAlias)) +ORDHDR.ORDER
        m.cOrdType ='O'
        m.llSel= .T.
       * m.Order = ''
        m.cadd_user = ''
        m.cadd_time = ''
		    m.Entered = oAriaApplication.SystemDate
        INSERT INTO (loFormSet.oFormEnvironment.lcOrdHdr) FROM MEMVAR
        =gfAdd_Info(loFormSet.oFormEnvironment.lcOrdHdr)
        SELECT ORDLINE
        =SEEK(ORDHDR.CORDTYPE+ORDHDR.ORDER)
        SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6) =  ORDHDR.CORDTYPE+ORDHDR.ORDER
          SCATTER MEMO MEMVAR 
          m.cOrdType ='O'
          m.Flag = 'N'&&Iif(This.MultiStores,'','N')
          
          m.Start = ldLastPeriodEndDate +1&&DATE(YEAR(GOMONTH(oAriaApplication.SystemDate,1)),MONTH(GOMONTH(oAriaApplication.SystemDate,1)),1)
          *m.Complete = DATE(YEAR(GOMONTH(m.Start,lnMonCnt+1)),MONTH(GOMONTH(m.Start,lnMonCnt+1)),1)-1
          *m.Complete = m.Start + lnMonCnt 
          m.Complete = GOMONTH(m.Start,lnMonCnt)-1
*!*              m.Start = DATE(YEAR(GOMONTH(oAriaApplication.SystemDate,1)),MONTH(GOMONTH(oAriaApplication.SystemDate,1)),1)
*!*            m.Complete = DATE(YEAR(GOMONTH(oAriaApplication.SystemDate,lnMonCnt+1)),MONTH(GOMONTH(oAriaApplication.SystemDate,lnMonCnt+1)),1)-1
          *m.Desc1 = ALLTRIM(m.Desc1)+" "+ ALLTRIM(CMONTH(m.Complete))+" "+ALLTRIM(STR(YEAR(m.Complete)))
          *B612634,1 MMT 10/23/2022 Adjust the item description on Generate SO from contract to reduce the number of characters used for period description[T20221019.0001][Start]           
*!*            IF LEN(ALLTRIM(m.Desc1))>22
*!*              m.Desc1 = SUBSTR(ALLTRIM(m.Desc1),1,22)
*!*            ENDIF
*!*            m.Desc1 = ALLTRIM(m.Desc1)+" "+;
*!*            IIF(oAriaApplication.oActivelang.cLang_ID = "EN" OR TYPE('LOFORMSET')<>'O',LANG_PERIOD_FROM,LOFORMSET.GetHeaderText("LANG_PERIOD_FROM",LOFORMSET.HeaderAlias))+;
*!*             DTOC(m.Start)+ ;
*!*             IIF(oAriaApplication.oActivelang.cLang_ID = "EN" OR TYPE('LOFORMSET')<>'O',LANG_TO,LOFORMSET.GetHeaderText("LANG_TO",LOFORMSET.HeaderAlias)) +;
*!*             DTOC(m.Complete)
          IF LEN(ALLTRIM(m.Desc1))>42
            m.Desc1 = SUBSTR(ALLTRIM(m.Desc1),1,42)
          ENDIF
          lcCentSet = SET("Century")
          SET CENTURY OFF 
          m.Desc1 = ALLTRIM(m.Desc1)+" "+;
                    DTOC(m.Start)+"-"+DTOC(m.Complete)
          SET CENTURY &lcCentSet. 
          *B612634,1 MMT 10/23/2022 Adjust the item description on Generate SO from contract to reduce the number of characters used for period description[T20221019.0001][End]           
          
          m.cadd_user = ''
          m.cadd_time = ''
          INSERT INTO (loFormSet.oFormEnvironment.lcOrdLine) FROM MEMVAR
          =gfAdd_Info(loFormSet.oFormEnvironment.lcOrdLine)
        ENDSCAN 
        
      ENDSCAN
    ENDSCAN
  
  OTHERWISE 
    SELECT ORDHDR
    =gfSetOrder('ORDHDR')
    =SEEK('C')
    SCAN REST WHILE CORDTYPE+ORDER = 'C' FOR IIF(llTermCode,SEEK(ORDHDR.CTERMCODE,lcTermFile),.T.) AND;
        IIF(llStartDateSel ,BETWEEN(ORDHDR.START,ldStartStart,ldStartEnd),.T.) AND ;
        IIF(llCompleteDateSel ,BETWEEN(ORDHDR.COMPLETE,ldCompleteStart ,ldCompleteEnd ),.T.) AND ;
        IIF(llEnteredDateSel ,BETWEEN(ORDHDR.ENTERED,ldEnteredStart ,ldEnteredEnd ),.T.)  AND;
        !INLIST(Status,'X','B') &&.AND. BETWEEN(oAriaApplication.SystemDate,START,COMPLETE)
        
        IF !gfSeek(ORDHDR.ORDER,'CONTRACTINF','CONTRACTIN')
          LOOP 
        ELSE
          IF !CONTRACTINF.LRECURRING OR IIF(lcFrequency ='L',.F. ,ALLTRIM(CONTRACTINF.CFREQUENCY) != lcFrequency )
            SELECT ORDHDR                     
            LOOP  
          ENDIF
          if llLastGenDateSel 
            IF !BETWEEN(CONTRACTINF.DLASTGEN ,ldLastGenStart ,ldLastGenEnd )
              SELECT ORDHDR
              LOOP
            ENDIF
          ENDIF
        ENDIF
        
        *XXX
        ldLastPeriodEndDate =IIF(EMPTY(CONTRACTINF.DLASTGEN) or ISNULL(CONTRACTINF.DLASTGEN),ORDHDR.Start-1,CONTRACTINF.DLASTGEN)
        lcFreq = CONTRACTINF.CFREQUENCY
        DO CASE
          CASE lcFreq ='M'
            lnMonCnt = 1
            *lnMonCnt = 30
          CASE lcFreq ='Q'
            lnMonCnt = 3
            *lnMonCnt = 90
          CASE lcFreq ='S'
            lnMonCnt = 6
            *lnMonCnt = 180
          CASE lcFreq ='A'
            lnMonCnt = 12
            *lnMonCnt = 365
        ENDCASE 
        *XXX
        
        SELECT ORDHDR
        
        SCATTER MEMO MEMVAR 
        m.Start = ldLastPeriodEndDate +1&&DATE(YEAR(GOMONTH(oAriaApplication.SystemDate,1)),MONTH(GOMONTH(oAriaApplication.SystemDate,1)),1)
        m.Complete = GOMONTH(m.Start,lnMonCnt)-1 && m.Start + lnMonCnt &&DATE(YEAR(GOMONTH(m.Start,lnMonCnt+1)),MONTH(GOMONTH(m.Start,lnMonCnt+1)),1)-1
        
        IF  m.Complete > ORDHDR.Complete
          =gfModalGen('INM32151B00000','DIALOG',Ordhdr.Order + '|' + DTOC(m.Complete)+'|' + DTOC(Ordhdr.Complete))
          SELECT ORDHDR
          LOOP 
        ENDIF
        m.Note1 = ALLTRIM(m.Note1)+IIF(oAriaApplication.oActivelang.cLang_ID = "EN" OR TYPE('LOFORMSET')<>'O',LANG_GEN_FROM_CONTRACT,LOFORMSET.GetHeaderText("LANG_GEN_FROM_CONTRACT",LOFORMSET.HeaderAlias)) +ORDHDR.ORDER
*!*          m.Start = DATE(YEAR(GOMONTH(oAriaApplication.SystemDate,1)),MONTH(GOMONTH(oAriaApplication.SystemDate,1)),1)
*!*          m.Complete = DATE(YEAR(GOMONTH(oAriaApplication.SystemDate,lnMonCnt+1)),MONTH(GOMONTH(oAriaApplication.SystemDate,lnMonCnt+1)),1)-1
        m.cOrdType ='O'
        m.llSel= .T.
        m.cadd_user = ''
        m.cadd_time = ''
		    m.Entered = oAriaApplication.SystemDate
       * m.Order = ''
        INSERT INTO (loFormSet.oFormEnvironment.lcOrdHdr) FROM MEMVAR
        =gfAdd_Info(loFormSet.oFormEnvironment.lcOrdHdr)
        SELECT ORDLINE
        =SEEK(ORDHDR.CORDTYPE+ORDHDR.ORDER)
        SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6) =  ORDHDR.CORDTYPE+ORDHDR.ORDER
          SCATTER MEMO MEMVAR 
          m.cOrdType ='O'
          m.Start = ldLastPeriodEndDate +1&&DATE(YEAR(GOMONTH(oAriaApplication.SystemDate,1)),MONTH(GOMONTH(oAriaApplication.SystemDate,1)),1)
          *m.Complete = DATE(YEAR(GOMONTH(m.Start,lnMonCnt+1)),MONTH(GOMONTH(m.Start,lnMonCnt+1)),1)-1
          *m.Complete = m.Start + lnMonCnt 
          m.Complete = GOMONTH(m.Start,lnMonCnt)-1
*!*            m.Start = DATE(YEAR(GOMONTH(oAriaApplication.SystemDate,1)),MONTH(GOMONTH(oAriaApplication.SystemDate,1)),1)
*!*            m.Complete = DATE(YEAR(GOMONTH(oAriaApplication.SystemDate,lnMonCnt+1)),MONTH(GOMONTH(oAriaApplication.SystemDate,lnMonCnt+1)),1)-1
          *m.Desc1 = ALLTRIM(m.Desc1)+" "+ "Period: From:"+DTOC(m.Start)+" To:"+DTOC(m.Complete)

          *B612634,1 MMT 10/23/2022 Adjust the item description on Generate SO from contract to reduce the number of characters used for period description[T20221019.0001][Start]           
*!*            IF LEN(ALLTRIM(m.Desc1))>22
*!*              m.Desc1 = SUBSTR(ALLTRIM(m.Desc1),1,22)
*!*            ENDIF
*!*            m.Desc1 = ALLTRIM(m.Desc1)+" "+;
*!*            IIF(oAriaApplication.oActivelang.cLang_ID = "EN" OR TYPE('LOFORMSET')<>'O',LANG_PERIOD_FROM,LOFORMSET.GetHeaderText("LANG_PERIOD_FROM",LOFORMSET.HeaderAlias))+;
*!*             DTOC(m.Start)+ ;
*!*             IIF(oAriaApplication.oActivelang.cLang_ID = "EN" OR TYPE('LOFORMSET')<>'O',LANG_TO,LOFORMSET.GetHeaderText("LANG_TO",LOFORMSET.HeaderAlias)) +;
*!*             DTOC(m.Complete)
          IF LEN(ALLTRIM(m.Desc1))>42
            m.Desc1 = SUBSTR(ALLTRIM(m.Desc1),1,42)
          ENDIF
          lcCentSet = SET("Century")
          SET CENTURY OFF 
          m.Desc1 = ALLTRIM(m.Desc1)+" "+;
                    DTOC(m.Start)+"-"+DTOC(m.Complete)
          SET CENTURY &lcCentSet. 
          *B612634,1 MMT 10/23/2022 Adjust the item description on Generate SO from contract to reduce the number of characters used for period description[T20221019.0001][End]           


          m.cadd_user = ''
          m.cadd_time = ''
          m.Flag = 'N'&&Iif(This.MultiStores,'','N')
          INSERT INTO (loFormSet.oFormEnvironment.lcOrdLine) FROM MEMVAR
          =gfAdd_Info(loFormSet.oFormEnvironment.lcOrdLine)
        ENDSCAN 
      ENDSCAN
ENDCASE

loFormSet.lnSelRec   = RECCOUNT(loFormSet.oFormEnvironment.lcOrdHdr) 
IF loFormSet.lnSelRec > 0 
  loFormSet.ChangeMode('A')
  loFormSet.oFormEnvironment.ActiveMode ='A'
   SELECT STYLE
   SET RELATION TO 'S'+SCALE INTO SCALE
ELSE
  =gfModalGen('TRM40128B00000','ALERT')  
ENDIF 
loFormSet.lnDelRec   = 0
loFormSet.llenablerel = .T.
loFormSet.lnUnSelRec = 0

lfAddControlSource(loFormSet)

*!*************************************************************
*! Name      : lfAddControlSource
*! Developer : Mariam Mazhar (MMT)
*! Date      : 06/27/2022
*! Purpose   : Add the Grids control Source
*!*************************************************************
FUNCTION lfAddControlSource
LPARAMETERS loFormSet
  WITH loFormSet.AriaForm1.Ariapageframe1.pgOrdhdr.grdOrders.grdMultiSelectionGrid
    .RecordSource = ''
    .RecordSource = loFormSet.oFormEnvironment.lcOrdHdr
    .Column1.ControlSource = 'Thisformset.mgetValueLogic()'
    .Column1.Header1.Caption = ""
    .Column1.CurrentControl = "AriaCheckBox1"
    .Column2.ControlSource = loFormSet.oFormEnvironment.lcOrdHdr+'.NewOrder'
    .Column2.Visible = .F.
    .Column3.ControlSource = loFormSet.oFormEnvironment.lcOrdHdr+'.Order'
    .Column4.ControlSource = loFormSet.oFormEnvironment.lcOrdHdr+'.Account'
    .Column5.ControlSource = loFormSet.oFormEnvironment.lcOrdHdr+'.Start'
    .Column6.ControlSource = loFormSet.oFormEnvironment.lcOrdHdr+'.Complete'
    .Column7.ControlSource = loFormSet.oFormEnvironment.lcOrdHdr+'.Open'
    .Column8.ControlSource = loFormSet.oFormEnvironment.lcOrdHdr+'.OpenAmt'
    .Column1.Enabled = .T.
    .SETALL('ReadOnly',.T.,'COLUMN')
    .Column1.readonly = .F.
    .Enabled = .T.
    .Column1.AriaCheckBox1.Enabled = .T.
    .refresh()
  ENDWITH 
  WITH loFormSet.AriaForm1.Ariapageframe1.Page2.grdEditLines
   .RecordSource = ''
   .RecordSource = loFormSet.oFormEnvironment.lcOrdLine
   .Column1.ControlSource = loFormSet.oFormEnvironment.lcOrdLine+'.LineNo'
   .Column2.ControlSource = loFormSet.oFormEnvironment.lcOrdLine+'.Store'
   .Column3.ControlSource = loFormSet.oFormEnvironment.lcOrdLine+'.Style'
   .Column4.ControlSource = loFormSet.oFormEnvironment.lcOrdLine+'.Desc1'
   .Column5.ControlSource = loFormSet.oFormEnvironment.lcOrdLine+'.TOTQTY'
   .Column6.ControlSource = loFormSet.oFormEnvironment.lcOrdLine+'.Price'
   .Column7.ControlSource = loFormSet.oFormEnvironment.lcOrdLine+'.TOTQTY *'+ loFormSet.oFormEnvironment.lcOrdLine+'.Price'
   .SETALL('ReadOnly',.T.,'COLUMN')
   .Enabled =.T.
 ENDWITH
 WITH loFormSet.AriaForm1.Ariapageframe1.Page2.AriaEditRegion1
   .LCT_BOMVAR = loFormSet.LCT_BOMVAR
   .Detailfile = loFormSet.oFormEnvironment.lcOrdLine
   .HEADERFILE= loFormSet.oFormEnvironment.lcOrdhdr
   .mSetControlSource()
 ENDWITH 

*!*************************************************************
*! Name      : lfCreatExpCN
*! Developer : Mariam Mazhar (MMT)
*! Date      : 06/27/2022
*! Purpose   : Copy OG Arrays
*!*************************************************************
FUNCTION lfCreatExpCN
=ACOPY(loOGScroll.laOGFxFlt , laScopExpr)
lcFrequency  = lcRPFreq
llSaveCriteria = .T.

*!*************************************************************
*! Name      : lfConvertToCursor
*: Developer : MAriam Mazhar (MMT)
*: Date      : 06/27/2022
*! Purpose   : Convert a list of values into a cusrsor
*!*************************************************************
*!B608130
FUNCTION lfConvertToCursor
PARAMETERS lcStrToConv,lcFieldName ,lcNewFile
lcCursorTemp = lcNewFile &&Cursor Hold Selected values
DIMENSION laTempacstru[1,4]
laTempacstru[1,1] = lcFieldName

DO CASE

CASE   ALLTRIM(lcFieldName) = 'CTERMCODE'
  laTempacstru[1,2]='C'
  laTempacstru[1,3]= 6
  laTempacstru[1,4]= 0

ENDCASE
 = gfCrtTmp(lcCursorTemp ,@laTempacstru,lcFieldName ,lcCursorTemp ,.T.)
lcValuesToConvert = lcStrToConv
IF !EMPTY(lcValuesToConvert)
  lnStart=1
  lnEnd=AT('|',lcValuesToConvert )
  DO WHILE lnEnd <> 0
    SELECT(lcCursorTemp )
    APPEND BLANK
    REPLACE &lcFieldName  WITH SUBSTR(lcValuesToConvert,lnStart,lnEnd-1)
    lcValuesToConvert = STUFF(lcValuesToConvert ,lnStart,lnEnd,"")
    lnEnd=AT('|',lcValuesToConvert )
  ENDDO
  IF lnEnd = 0
    SELECT(lcCursorTemp )
    APPEND BLANK
    REPLACE &lcFieldName  WITH lcValuesToConvert
  ENDIF
ENDIF
RETURN .T.

*!*************************************************************
*! Name      : lfSaveOrders
*! Developer : Mariam Mazhar (MMT)
*! Date      : 06/27/2022
*! Purpose   : Save Sales Orders
*!*************************************************************
FUNCTION lfSaveOrders
PARAMETERS loFormSet
SET STEP ON 
DIMENSION laOrders[1,2]
lnOrdCount = 0
SELECT * FROM profvalu WHERE .F. INTO CURSOR 'TmpProfile' READWRITE 
SELECT DISTINCT ORDER FROM (loFormSet.oFormEnvironment.lcOrdHdr) WHERE llSel INTO CURSOR 'ContractList'

oprogress = CREATEOBJECT('ariaprogressbar') 
oprogress.lblFirstLabel.Caption = IIF(oAriaApplication.oActivelang.cLang_ID = "EN" OR TYPE('LOFORMSET')<>'O',LANG_SAVING_ORDER,LOFORMSET.GetHeaderText("LANG_SAVING_ORDER",LOFORMSET.HeaderAlias))
oprogress.TotalProgress = RECCOUNT('ContractList')
oprogress.AutoCenter    = .T.
oprogress.Show()

lnCntOrder = 0
SELECT 'ContractList'
SCAN 
  lnCntOrder = lnCntOrder + 1
  oprogress.CurrentProgress(lnCntOrder)
  WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN" OR TYPE('LOFORMSET')<>'O',LANG_SAVING_ORDER,LOFORMSET.GetHeaderText("LANG_SAVING_ORDER",LOFORMSET.HeaderAlias)) NOWAIT 
  SELECT (loFormSet.oFormEnvironment.lcOrdHdr)
  SET FILTER TO ORDER = ContractList.ORDER
  LOCATE 
  SELECT (loFormSet.oFormEnvironment.lcOrdLine)
  SET FILTER TO ORDER = ContractList.ORDER
  LOCATE 

  DO lfSavScr IN (oAriaApplication.ApplicationHome + 'SO\SOUPDATE.FXP') ;
     WITH .F., 'A', loFormSet.oFormEnvironment.lcOrdHdr,loFormSet.oFormEnvironment.lcOrdLine,.F.,.f.,loFormSet.lcT_BomVar,loFormSet
     
  SELECT (loFormSet.oFormEnvironment.lcOrdHdr)
  SET FILTER TO 
  LOCATE 
  SELECT (loFormSet.oFormEnvironment.lcOrdLine)
  SET FILTER TO
  LOCATE
  SET ORDER TO TAG 'ORDLINE' IN (loFormSet.oFormEnvironment.lcOrdLine)
  lnOrdCount = lnOrdCount + 1
  DIMENSION laOrders[lnOrdCount,2]
  laOrders[lnOrdCount,1] = ORDHDR.ORDER
  laOrders[lnOrdCount,2] = ContractList.ORDER
  
  
	lcProfileKey ="SO"+"O"+ContractList.ORDER
	lcOrdType = "O" 
	IF SEEK(lcProfileKey,'profvalu')
	  SELECT profvalu
    SCAN REST WHILE  cpro_type+ckey+cpro_code = lcProfileKey
      SCATTER MEMO MEMVAR 
      INSERT INTO 'TmpProfile' FROM MEMVAR
    ENDSCAN
	ENDIF
ENDSCAN  
SELECT profvalu
TABLEREVERT(.T.)

FOR lnX =1 TO ALEN(laOrders,1)
  lcProfileKey ="SO"+"O"+ laOrders[lnX ,2]
  SELECT TmpProfile
  SCAN FOR  cpro_type+ckey+cpro_code = lcProfileKey
    SCATTER MEMO MEMVAR 
    m.CKEY = STRTRAN(m.CKEY,laOrders[lnX ,2],laOrders[lnX ,1])
    INSERT INTO profvalu FROM MEMVAR
  ENDSCAN
ENDFOR
lfSavefiles()
FOR lnOrdCnt = 1 TO ALEN(laOrders,1)
  =gfSeek(laOrders[lnOrdCnt ,2],'CONTRACTINF','CONTRACTIN')
  =gfSeek('O'+laOrders[lnOrdCnt ,1],'ORDHDR','ORDHDR')
  SELECT CONTRACTINF
  REPLACE DLASTGEN WITH ORDHDR.COMPLETE,;
          CLASTGENSO WITH laOrders[lnOrdCnt ,1]
  =gfReplace('')
ENDFOR 
SELECT CONTRACTINF
=gfTableUpdate()
oprogress.Hide()
DO CASE 
  CASE lnOrdCount=1
    *=gfModalGen('INM00000B00000',.F.,.F.,.F.,'Order is saved as ' + laOrders[1,1])
    =gfModalGen('INM32045B00000','DIALOG',IIF(oAriaApplication.oActivelang.cLang_ID = "EN" OR TYPE('LOFORMSET')<>'O',LANG_LabelOrder,LOFORMSET.GetHeaderText("LANG_LabelOrder",LOFORMSET.HeaderAlias))+'|'+laOrders[1,1])
  CASE lnOrdCount > 1
  
    *=gfModalGen('INM00000B00000',.F.,.F.,.F.,'Orders from ' + laOrders[1,1] + ' to ' + laOrders[lnOrdCount,1]+ ' have been generated successfully')
    =gfModalGen('INM32150B00000','DIALOG',IIF(oAriaApplication.oActivelang.cLang_ID = "EN" OR TYPE('LOFORMSET')<>'O',LANG_LabelOrders,LOFORMSET.GetHeaderText("LANG_LabelOrders",LOFORMSET.HeaderAlias))+'|' + laOrders[1,1] + '|' + laOrders[lnOrdCount,1])
  CASE lnOrdCount = 0
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,IIF(oAriaApplication.oActivelang.cLang_ID = "EN" OR TYPE('LOFORMSET')<>'O',LANG_NO_ORDER_SAVED,LOFORMSET.GetHeaderText("LANG_NO_ORDER_SAVED",LOFORMSET.HeaderAlias)) )
ENDCASE
loFormSet.llCallScop = .F.
*loFormSet.ChangeMode('S')
*!*************************************************************
*! Name      : lfSavefiles
*: Developer : Mariam Mazhar- [MMT]
*: Date      : 06/2027/2022
*! Purpose   : Function to save 
*!*************************************************************
FUNCTION lfSavefiles

lcTranCode = oAriaApplication.RemoteCompanyData.BeginTran(oAriaApplication.ActiveCompanyConStr,3,'')
IF TYPE('lcTranCode') = 'N'
  =oAriaApplication.RemoteCompanyData.CheckRetResult("BeginTran",lcTranCode,.T.)
   RETURN .F.
ENDIF
lnUpdated = 0
lnAryLen = ALEN(oAriaApplication.laRemoteTable)
FOR lnCounter=1 TO lnAryLen
  IF oAriaApplication.laRemoteTable[lnCounter].lnDataSession == loFormSet.DataSessionId
    IF !oAriaApplication.laRemoteTable[lnCounter].TableUpdate(lcTranCode)
      lnUpdated=lnCounter
      exit
    ENDIF
  ENDIF
NEXT
IF lnUpdated>0
  oAriaApplication.RemoteCompanyData.RollBackTran(lcTranCode)
  MESSAGEBOX('Saving Process is Rolled Back')
  ThisFormSet.Undo()
  RETURN
ELSE
  oAriaApplication.RemoteCompanyData.CommitTran(lcTranCode)
ENDIF
*!*************************************************************
*! Name      : lfcustmsg
*! Developer : Mariam Mazhar (MMT)
*! Date      : 06/27/2022
*! Purpose   : Dummy function to stop displaying SO# from Order saving function
*!*************************************************************
FUNCTION lfcustmsg