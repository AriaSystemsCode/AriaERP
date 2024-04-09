*:************************************************************************
*: Program file  : ICOLS15.PRG (Converted from 26 to 27 for OLSEN)
*:          Date : 08/15/1999.
*: Program desc. : Inventory reconciliation report for Olsen.
*:        System : ARIA APPAREL SERIES.
*:     Developer : Adel Mohammed El Gazzar (ADEL).
*: Refer to      : C(101588)
*:************************************************************************
*: Calls         : gfGetMemVar()
*:                 gfModalGen()
*:                 gfTEMPNAME()
*:                 gfDispRe()
*:                 lfAdjTra()
*:*************************************************************************

*--Restore needed variables
llAvg_Cost  = (gfGetMemVar('M_COST_MET')  = 'A')

*-- Check the date range.
lnDatePos = ASCAN(laOGFxFlt,"STYINVJL.DTRDATE")
IF lnDatePos > 0
  lnDatePos = ASUBSCRIPT(laOGFxFlt,lnDatePos,1)
  lnPPos    = AT("|",laOGFxFlt[lnDatePos,6])
  lcFDate   = IIF(lnPPos <>1,CTOD(SUBSTR(laOGFxFlt[lnDatePos,6],1,10)),{})
  lcTDate   = CTOD(SUBSTR(laOGFxFlt[lnDatePos,6],lnPPos+1,10))  
ENDIF
IF lnDatePos = 0 OR lcFDate = {}
  *-- Message : MUST ENTER DATE RANGE!
  =gfModalGen('TRM42193B40011','ALERT')
  RETURN
ENDIF

IF llOgFltCh
  *-- Get the entered seasons.
  lnSeaPos = ASCAN(laOGVrFlt,"STYLE.SEASON")
  IF lnSeaPos > 0
    *---If the user selected seasons.
    *--Get the row.
    lnSeaPos = ASUBSCRIPT(laOGVrFlt,lnSeaPos,1)
    *--Get the selected seasons.
    lcSeaStr = laOGVrFlt[lnSeaPos,6]
    lcSeaStr = STRTRAN(lcSeaStr,'|',' ')
  ENDIF
  *-- Get the entered styles.
  STORE SPACE(LEN(lcMajPic)) TO lcFSty,lcTSty
  lnStyPos = ASCAN(laOGFxFlt,"SUBSTR(STYINVJL.STYLE,1,LEN(lcMajPic))")
  IF lnStyPos > 0
    lnStyPos = ASUBSCRIPT(laOGFxFlt,lnStyPos ,1)
    lnPPos   = AT("|",laOGFxFlt[lnStyPos ,6])
    lcFSty   = IIF(lnPPos <>1,SUBSTR(laOGFxFlt[lnStyPos ,6],1,LEN(lcMajPic)),lcFSty)
    lcTSty   = SUBSTR(laOGFxFlt[lnStyPos ,6],lnPPos+1,LEN(lcMajPic))
  ENDIF
  lcFDate1=DTOS(lcFDate)
  lcTDate1=DTOS(lcTDate)
  *--Close the cursor and recreate it.
  SELECT (lcCursName)
  IF RECCOUNT() >0
    USE IN (lcCursName)
    =lfCreatCur()
  ENDIF
  SELECT StyINVJL
  GO TOP
  SET FILTER TO
  lcFilter = " "
  lcFilter1= " "
  lcFilter = "DTOS(DTrDate)>=lcFDate1.AND.DTOS(DTrDate)<=lcTDate1"
  lcFilter1= "DTOS(DtrDate)<lcFDate1"
  IF !EMPTY(lcRpWareCd)
    lcFilter = lcFilter+ ".AND. cWareCode=lcRpWareCd"
    lcFilter1= lcFilter1+".AND. cWareCode=lcRpWareCd"
    lcWFilt  = "cWareCode=lcRpWareCd"
  ELSE
    lcWFilt  = "1=1"
  ENDIF
  IF lnSeaPos > 0 AND !EMPTY(lcSeaStr)
    lcFilter = lcFilter + ".AND. Style.Season $ lcSeaStr"
    lcFilter1= lcFilter1+ ".AND. Style.Season $ lcSeaStr"
    lcSFilt  = "Style.Season $ lcSeaStr"
  ELSE
    lcSFilt  = "1=1"
  ENDIF
  lcFilter=ALLTRIM(lcFilter)
  lcFilter1=ALLTRIM(lcFilter1)
  DO CASE
    CASE EMPTY(lcFSty) AND EMPTY(lcTSty)
      lcWCond='!EOF()'
    CASE EMPTY(lcFSty) AND !EMPTY(lcTSty)
      lcWCond='SUBSTR(STYLE,1,lnMajorLen)<=lcTSty'
    CASE !EMPTY(lcFSty) AND !EMPTY(lcTSty)
      lcWCond='SUBSTR(STYLE,1,LEN(lcMajPic))>=lcFSty AND SUBSTR(STYLE,1,LEN(lcMajPic))<=lcTSty'
  ENDCASE
  IF !EMPTY(lcFSty) 
    lcSFSty=lcFSty
    DO WHILE .T.
      SELECT STYINVJL
      IF SEEK(lcSFSty)
        EXIT
      ELSE
        SELECT STYLE
        =SEEK(lcSFSty)
        IF !EOF()
          SKIP
          lcSFSty=Style
          LOOP
        ELSE
          EXIT
        ENDIF
      ENDIF
    ENDDO
  ELSE
    SELECT STYINVJL
  ENDIF
  DO WHILE &lcWCond  	  
    IF !(&lcFilter1)
      IF lcFDate1<=DTOS(DtrDate) AND &lcWFilt AND &lcSFilt
        lcOpenInv=0
      ELSE
        IF !EOF()
          SKIP
          LOOP
        ELSE
          EXIT
        ENDIF
      ENDIF
    ENDIF
    lcWareCode=cWareCode
    lcStyle   =Style
    lcCost    =0
    xSeason   =SPACE(6)
    IF SEEK(lcStyle,'Style')
      lcCost = IIF(llAvg_Cost,Style.AVE_COST,Style.TotCost)
      xSeason=Style.Season
    ELSE
      IF !EOF()
        SKIP
        LOOP
      ENDIF
    ENDIF
    lcOpenInv =0
    lcCloseInv=0
    lcSales   =0
    lcPurch   =0
    lcReturn  =0
    lcTransf  =0
    lcAdjus   =0
    llTrans   =.F.
    llAdjTra  =.T.
    WAIT WINDOW 'Collecting data => Style/Warehouse = '+lcStyle+lcWareCode NOWAIT
    SELECT StyInvJL
    SCAN WHILE style+cwarecode+csession+DTOS(dtrdate)+ctrcode = lcStyle+lcWareCode FOR &lcFilter1 
      IF cTrType = '2' OR cTrType = '9'
        lcOpenInv = 	IIF(cIRType = "I",lcOpenInv ,NTotStk)
      ELSE
        lcOpenInv=lcOpenInv+NTotStk
      ENDIF
    ENDSCAN
    lcCloseInv=lcOpenInv
    IF !EMPTY(lcWareCode) AND SEEK(lcStyle+lcWareCode)
      SCAN WHILE style+cwarecode+csession+DTOS(dtrdate)+ctrcode = lcStyle+lcWareCode FOR &lcFilter
        IF cTrType = '2' OR cTrType = '9'
          lcCloseInv = lcCloseInv+NTotStk
        ELSE
          lcCloseInv=lcCloseInv+NTotStk
        ENDIF
        DO CASE
          CASE cTrType = '1'
            IF llAdjTra
              =lfAdjTra()
            ENDIF
          CASE cTrType = '2' OR cTrType = '9'
            lcAdjus=lcAdjus +nTotStk
          CASE cTrType = '3' OR cTrType = '4'
            lcSales = lcSales + NTotStk
          CASE cTrType = '5' OR CtrType = '6'
            lcPurch = lcPurch + NTotStk
          CASE cTrType = '7' OR cTrType = '8'
            lcReturn= lcReturn+ NTotStk
        ENDCASE
      ENDSCAN
      SELECT (lcCursName)
      APPEND BLANK
      REPLACE WareHous WITH lcWareCode,Style WITH lcStyle,Season WITH xSeason,;
              OpenInv WITH lcOpenInv,CloseInv WITH lcCloseInv,Sales WITH lcSales,;
              Purch WITH lcPurch,Adjus WITH lcAdjus,Transf WITH lcTransf,Return WITH lcReturn,;
              OpenV WITH lcOpenInv*lcCost,PurchV WITH lcPurch*lcCost,SalesV WITH lcSales*lcCost,;
              ReturnV WITH lcReturn*lcCost,TransfV WITH lcTransf*lcCost,;
              AdjusV WITH lcAdjus*lcCost,CloseV WITH lcCloseInv*lcCost
    ENDIF
    SELECT STYINVJL
  ENDDO  
  WAIT CLEAR
ENDIF  
R_WIDTH='N'
SELECT (lcCursName)
GO TOP
IF EOF()
  *-- Message : There are no records to display...!
  =gfModalGen('TRM00052B40011','ALERT')
  RETURN
ENDIF
DO gfDispRe WITH gcRepHome+gcAct_Appl+'\ICOLS15.frx'
SET DEVICE TO SCREEN

*!*************************************************************
*! Name      : lfAdjTra
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 08/15/1999
*! Purpose   : Defrentiate between Transfer and Adjustment in StyInvJL file.
*!*************************************************************
*! Example            :  =lfAdjTra()
*!*************************************************************
FUNCTION lfAdjTra

ll=.F.
llAdjTra = .F.
SELECT INVTADJ
IF SEEK(lcStyle)
  SCAN WHILE (Style=lcStyle) FOR ((cFromWare=lcWareCode OR cToWare=lcWareCode) AND BETWEEN(Date,lcFDate,lcTDate))
   IF TYPE = 'T'  AND !llTrans
     ll=.T.
     IF cFromWare=lcWareCode
       lcTransf=lcTransf-TotAdj
     ENDIF
     IF cToWare=lcWareCode
       lcTransf=lcTransf+TotAdj
     ENDIF
   ENDIF
   IF TYPE = 'A'
     lcAdjus = lcAdjus+TotAdj
   ENDIF
 ENDSCAN
 IF ll
   llTrans=.T.
 ENDIF
ENDIF 

****************************************************************************
* FUNC: lfwOldVal
* DESC: To get the old value.
* AUTH: Adel Mohammed El Gazzar (ADEL)
* DATE: 08/15/1999
****************************************************************************
FUNCTION lfwOldVal

lcOldVal = EVALUATE(SYS(18))

*!*************************************************************
*! Name      : lfvWareCod
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 08/15/1999
*! Purpose   : Validation function for the Warehouse Code field
*!*************************************************************
*! Called from : Warehouse Code field [Option Grid]
*!*************************************************************
*! Calls       : gfBrowWare()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvWare

PRIVATE lcObjName , lcObjVal

lcObjName = SYS(18)      && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = EVALUATE(SYS(18))      && Varible to hold  the value of the current GET field

*IF The user want to Browse or if the Warehouse he entered is not in the file
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK(lcObjVal , 'WAREHOUS'))
  lcObjVal = gfBrowWare(.T.)
  lcObjVal = IIF(EMPTY(lcObjVal) , lcOldVal , lcObjVal)
  &lcObjName = lcObjVal
ENDIF    && End of IF


****************************************************************************
* FUNC: lfwRepWhen
* DESC: To valid the OG WHEN function.
* AUTH: Adel Mohammed El Gazzar (ADEL)
* DATE: 08/15/1999
****************************************************************************
FUNCTION lfwRepWhen

*-- Initilize the date range with system date.
lnDatePos = ASCAN(laOGFxFlt,"STYINVJL.DTRDATE")
IF lnDatePos > 0
  lnDatePos = ASUBSCRIPT(laOGFxFlt,lnDatePos,1)
  IF EMPTY(laOGFxFlt[lnDatePos,6])
    laOGFxFlt[lnDatePos,6] = DTOC(gdSysDate) + '|' + DTOC(gdSysDate)
  ENDIF  
ENDIF
*--Create the cursor
=lfCreatCur()

****************************************************************************
* FUNC: lfvStyle
* DESC: To valid the style.
* AUTH: Adel Mohammed El Gazzar (ADEL)
* DATE: 08/15/1999
****************************************************************************
FUNCTION lfvStyle

PRIVATE lnCurSelct,lcStyOrder
lnCurSelct = SELECT(0)
SELECT STYLE
lcStyOrder = ORDER()
SET ORDER TO cStyle 
*-- Varible to hold  the name of the memory variable used to create the current GET field
lcObjName = SYS(18)
*-- Varible to hold  the value of the current GET field
lcObjVal = EVALUATE(SYS(18)) 
*--IF The user want to Browse or if the Style he entered is not in the file
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK(lcObjVal , 'STYLE'))
  lcObjVal = gfStyBrw('M',"","",.F.)  &&Browse style major only.
  lcObjVal = IIF(!EMPTY(lcObjVal) , lcObjVal , lcOldVal)
  &lcObjName = lcObjVal
ENDIF 
SELECT STYLE
SET ORDER TO &lcStyOrder
SELECT (lnCurSelct)

****************************************************************************
* FUNC: lfCreatCur
* DESC: To create the cursor.
* AUTH: Adel Mohammed El Gazzar (ADEL)
* DATE: 08/15/1999
****************************************************************************
FUNCTION lfCreatCur

CREATE CURSOR (lcCursName) (Warehous C(6),Style C(19),Season C(6),;
                            OpenInv N(7),Purch N(7),Sales N(7),Return N(7),;
                            Transf N(7),Adjus N(7),CloseInv N(7),;
                            OpenV N(10,2),PurchV N(10,2),SalesV N(10,2),ReturnV N(10,2),;
                            TransfV N(10,2),AdjusV N(10,2),CloseV N(10,2))
ZAP                            
INDEX ON Warehous+Season TAG (lcCursName) OF (gcWorkDir+lcCursName)

****************************************************************************
* FUNC: lfClearRep
* DESC: To Close the cursor.
* AUTH: Adel Mohammed El Gazzar (ADEL)
* DATE: 08/15/1999
****************************************************************************
FUNCTION lfClearRep

llOgFltCh = .T.
IF USED(lcCursName)
  USE IN (lcCursName)
ENDIF  

