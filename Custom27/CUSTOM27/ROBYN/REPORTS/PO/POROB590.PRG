*:************************************************************************
*: Program file  : POROB590.PRG
*: Program desc. : FACTORY CAPACITY REPORT FOR (ROBYN MERDITH).
*:                 The program is a conversion from 2.6 ==> 2.7
*: System        : Aria Apparel Series. Ver 2.7
*: Module        : PO
*: Date          : 07/18/2000
*: Developer     : Wael Mohammed Anwar (WMA)
*: Refer to      : C#101335
*:************************************************************************
*: Calls : 
*:         Functions  : lfGetData()
*:                      lfPrnRep()
*:                      lfPrnHdr()
*:                      lfPrnTotal()
*:                      lfOpnFiles()
*:                      lfAddTo()
*:                      lfItemFilt()
*:                      lfItmPos()
*:         Procedures : 
*:************************************************************************
*:Passed Parameters : None
*:************************************************************************
*:Example           : Do POROB590
*:************************************************************************
*:Modifications     :
*:************************************************************************

*-- Initializing the necessary varibales.
STORE SPACE(03)  TO lcStatus,lcPoStatus
STORE SPACE(0)   TO lcRpExpCt,lcRpExpPO,lcSelComp
STORE 0 TO lnWeek1,lnWeek2,lnWeek3,lnWeek4,lnWeek5,lnWeek6,lnWeek7,;
            lnWeek8,lnWeek9,lnWeek10,lnWeek11,lnWeek12,lnWeek13,lnWeek14,;
            lnWeek15,lnWeek16,lnWeek17,lnWeek18,lnWeek19lnWeek20,lnWeek21
STORE {} TO ldWeek1,ldWeek2,ldWeek3,ldWeek4,ldWeek5,ldWeek6,ldWeek7,;
            ldWeek8,ldWeek9,ldWeek10,ldWeek11,ldWeek12,ldWeek13,ldWeek14,;
            ldWeek15,ldWeek16,ldWeek17,ldWeek18,ldWeek19,ldWeek20,ldWeek21

llAllVen   = .T.
llRecToprn = .F.
lcVenTemp  = gfTempName()
TmpPoCt    = gfTempName()
TmpFile    = gfTempName()
PageNo     = 0
ROW        = 99
R_WIDTH    = 'XW'
R_TITLE  = 'Factory Capacity Report'

lnProfile2 = lfItmPos('CUTTKTH.CUTTKT')
lnProfile3 = lfItmPos('POSHDR.PO')
lnProfile4 = lfItmPos('CUTTKTH.COMPLETE')
lnProfile5 = lfItmPos('STYLE.CDIVISION')
lnProfile6 = lfItmPos('STYLE.SEASON')


*-- Check if user select company.
DO WHILE .T.
  IF EMPTY(laRpComp)
    =gfModalGen(.f.,.f.,.f.,.f.,'You should choose at least one company.')  
    =lfvOComp()
  ELSE
    EXIT
  ENDIF
ENDDO

*-- Create temp. file.
CREATE CURSOR (TmpFile);
              (cVendor C(8),nWeek1 N(6),nWeek2 N(6),nWeek3 N(6),nWeek4 N(6),;
              nWeek5 N(6),nWeek6 N(6),nWeek7 N(6),nWeek8 N(6),nWeek9 N(6),;
              nWeek10 N(6),nWeek11 N(6),nWeek12 N(6),nWeek13 N(6),nWeek14 N(6),;
              nWeek15 N(6),nWeek16 N(6),nWeek17 N(6),nWeek18 N(6),;
              nWeek19 N(6),nWeek20 N(6),nWeek21 N(6),nTotWip N(6),;
              cCompany C(2),cType C(1))
INDEX ON cVendor+cType+cCompany TAG(TmpFile) OF (TmpFile)
             
*-- collecting the data
IF lfGetData()
  *-- Printing the report
  =lfPrnRep()  
ENDIF
=lfOpnCCFil()
*-- End of POROB590.PRG.


*-- Begin of Functions --*
*!*************************************************************
*! Name      : lfGetData
*! Developer : Wael Mohammed Anwar
*! Date      : 07/18/2000
*! Purpose   : To get the C/T and Po
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Called from :  
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            :  lfGetData()
*!*************************************************************
*! Note      : 
*!*************************************************************
FUNCTION lfGetData

PRIVATE lcProfExp , laBrTmpFlt
STORE '' TO lcProfExp , laBrTmpFlt

DIMENSION laBrTmpFlt[ALEN(laBrFldFlt,1),ALEN(laBrFldFlt,2)]

=ACOPY(laBrFldFlt,laBrTmpFlt)

FOR lnI = 1 TO ALEN(laBrFldFlt,1)
  IF "LAOGFXFLT" $ UPPER(laBrFldFlt[lnI,5])
    laBrFldFlt[lnI,5]  = "laTempExpr:1"
  ENDIF
ENDFOR

*-- the Cutting tickets filter.
IF lnProfile2 > 0
  lcProfExp = lfItemFilt(lnProFile2)
  =lfAddTo(@lcRpExpCT,lcProfExp)
ENDIF

IF lnProfile4 > 0
  lcProfExp = lfItemFilt(lnProFile4)
  =lfAddTo(@lcRpExpCT,lcProfExp)
ENDIF

IF lnProfile5 > 0
  lcProfExp = lfItemFilt(lnProFile5)
  =lfAddTo(@lcRpExpCT,lcProfExp)
ENDIF

IF lnProfile6 > 0
  lcProfExp = lfItemFilt(lnProFile6)
  =lfAddTo(@lcRpExpCT,lcProfExp)
ENDIF


*-- the Purchase orders filter.
IF lnProfile3 > 0
  lcProfExp = lfItemFilt(lnProFile3)
  =lfAddTo(@lcRpExpPO,lcProfExp)
ENDIF

IF lnProfile4 > 0
  lcProfExp = lfItemFilt(lnProFile4)
  =lfAddTo(@lcRpExpPO,lcProfExp)
ENDIF

IF lnProfile5 > 0
  lcProfExp = lfItemFilt(lnProFile5)
  =lfAddTo(@lcRpExpPO,lcProfExp)
ENDIF


=ACOPY(laBrTmpFlt,laBrFldFlt)

*-- Build the Cutting tickets filter Exp.
lcRpExpCT = STRTRAN(lcRpExpCT,"STYLE.","CUTTKTH.")

IF EMPTY(lcRpExpCT)
  lcRpExpCT = ".T."
ENDIF

*-- Build the Purchase orders filter Exp.
lcRpExpPO = STRTRAN(lcRpExpPO,"STYLE.","POSHDR.")
lcRpExpPO = STRTRAN(lcRpExpPO,"CUTTKTH.","POSHDR.")

IF EMPTY(lcRpExpPO)
  lcRpExpPO = ".T."
ENDIF


IF !EMPTY(lcRpStatus)
  lcStatus   = ALLTRIM(lcRpStatus)
  lcPoStatus = STRTRAN(lcRpStatus,"A","")
  lcRpExpCt = lcRpExpCt + "  .AND. CUTTKTH.STATUS $ lcStatus"
  lcRpExpPO = lcRpExpPO + "  .AND. POSHDR.STATUS $ lcPoStatus"
ENDIF

*-- Specifying the weeks

ldWeek1 = gdSysDate+IIF(UPPER(CDOW(gdSysDate))="SATURDAY",6,;
                      IIF(UPPER(CDOW(gdSysDate))="SUNDAY",5,;
                      IIF(UPPER(CDOW(gdSysDate))="MONDAY",4,;
                      IIF(UPPER(CDOW(gdSysDate))="TUESDAY",3,;
                      IIF(UPPER(CDOW(gdSysDate))="WEDNESDAY",2,;
                      IIF(UPPER(CDOW(gdSysDate))="THURSDAY",1,0))))))

ldStorWeek = ldWeek1
FOR lnCounter = 2 TO 21
  lcCounter  = ALLTRIM(STR(lnCounter))
  ldStorWeek = ldStorWeek + 7
  ldWeek&lcCounter = ldStorWeek
ENDFOR

CREATE CURSOR (TmpPoCt) ;
              (Vendor C(8), CutTktPo C(6),ldEntere D,ldComplete D,;
               cMIType C(1), cCompany C(2))
INDEX ON Vendor+CutTktPo+cMIType+cCompany TAG(TmpPoCt) OF (TmpPoCt)

*-- Vendor temp. file (in fixed filter)
lcVeFile = ''
lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'APVENDOR.CVENDCODE'),1)
  IF lnPosition > 0
    lcVeFile = laOGFxFlt[lnPosition,6]
    IF !EMPTY(lcVeFile) AND USED(lcVeFile)
      SELECT (lcVeFile)
      GOTO TOP
      IF !EOF(lcVeFile) 
        llAllVen = .F.
      ENDIF 
    ENDIF  
  ELSE
    llAllVen = .T.
  ENDIF

*-- Style groups (in fixed filter)
lcGroups = ''
lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'STYLE.CSTYGROUP'),1)
IF lnPosition > 0
  lcGroups = laOGFxFlt[lnPosition,6]
ENDIF

*-- Style Colors (in fixed filter)
lcColors = ''
lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'SUBSTR(STYLE.STYLE,lnStartPos,lnNonMajLn)'),1)
IF lnPosition > 0
  lcColors = laOGFxFlt[lnPosition,6]
ENDIF

*-- Fabric temp. file (in fixed filter)
lcFabFile = ''
llFabFile = .F.
lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'STYLE.FABRIC'),1)
IF lnPosition > 0
  lcFabFile = laOGFxFlt[lnPosition,6]
  IF !EMPTY(lcFabFile) AND USED(lcFabFile)
    SELECT (lcFabFile)
    GOTO TOP
    llFabFile = .T.
  ENDIF  
ENDIF

*-- Style temp. file (in fixed filter)
lcStyFile = ''
llStyFile = .F.
lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'STYLE.CSTYMAJOR'),1)
IF lnPosition > 0
  lcStyFile = laOGFxFlt[lnPosition,6]
  IF !EMPTY(lcStyFile) AND USED(lcStyFile)
    SELECT (lcStyFile)
    GOTO TOP
    llStyFile = .T.
  ENDIF
ENDIF


lnI = 1
DO WHILE !EMPTY(laRpComp[lnI])
  SELECT SycComp
  = SEEK(laRpComp[lnI])
  lnI = lnI + 1
  *-- To open the necessary files of the selected company.
  =lfOpnFiles()
  *-- Reinitializing the weeks.
  STORE 0 TO lnWeek1,lnWeek2,lnWeek3,lnWeek4,lnWeek5,lnWeek6,lnWeek7,;
             lnWeek8,lnWeek9,lnWeek10,lnWeek11,lnWeek12,lnWeek13,lnWeek14,;
             lnWeek15,lnWeek16,lnWeek17,lnWeek18,lnWeek19,lnWeek20,lnWeek21
  *-- To get all the open and hold Cutting Tickets.
  WAIT WINDOW "Selecting data from company " + SycComp.cComp_id +" ." NOWAIT
  
  SELECT CutTktH  
  SCAN FOR &lcRpExpCt
    WAIT WINDOW "Selecting cutting tickte # : " + CutTkt NOWAIT

    IF !EMPTY(lcStyFile) .AND. llStyFile
      IF !SEEK(STYLE.CSTYMAJOR,lcStyFile)
        LOOP
      ENDIF
    ENDIF      

    lcOprCode = IIF(!EMPTY(lcRpOper),lcRpOper,'')

    SELECT MFGOPRHD
    IF !SEEK("M"+CutTktH.CUTTKT+lcOprCode,'MFGOPRHD')
      LOOP
    ENDIF

    llOper = .F.
    SCAN REST WHILE cimtyp+ctktno+coprcode = "M"+CutTktH.CUTTKT+lcOprCode
      IF !llAllVen
        IF !SEEK(MFGOPRHD.CCONTCODE,lcVeFile)
          LOOP
        ENDIF
      ELSE
       IF EMPTY(MFGOPRHD.CCONTCODE)
         LOOP
       ENDIF 
      ENDIF      
      llOper = .T.
      EXIT
    ENDSCAN
      
    SELECT CutTktH  
    IF !llOper
      LOOP
    ENDIF


    SELECT MFGOPRHD
    =SEEK("M"+CutTktH.CUTTKT+lcOprCode)
    SCAN REST WHILE cimtyp+ctktno+coprcode = "M"+CutTktH.CUTTKT+lcOprCode
      SELECT (TmpPoCt)
      IF !SEEK(Mfgoprhd.cContcode+CutTktH.CutTkt+'M'+Syccomp.cComp_id)
        APPEND BLANK
        REPLACE Vendor     WITH Mfgoprhd.cContcode      ,;
                CutTktPo   WITH CutTktH.CutTkt          ,;
                ldEntere   WITH CutTktH.Entered         ,;
                ldComplete WITH CutTktH.Complete        ,;
                cMIType    WITH "M"                     ,; 
                cCompany   WITH Syccomp.cComp_id
      ENDIF
    ENDSCAN 
  ENDSCAN

  *-- To get all the open and hold Purchase orders.
  SELECT PoSHdr
  SCAN FOR &lcRpExpPO
    WAIT WINDOW "Selecting Purchase Order # : " + Po NOWAIT
    lcOprCode = IIF(!EMPTY(lcRpOper),lcRpOper,'')

    SELECT MFGOPRHD
    IF !SEEK("I"+POSHDR.PO+lcOprCode,'MFGOPRHD')
      LOOP
    ENDIF

    llOper = .F.
    SCAN REST WHILE cimtyp+ctktno+coprcode = "I"+POSHDR.PO+lcOprCode
      IF !llAllVen
        IF !SEEK(MFGOPRHD.CCONTCODE,lcVeFile)
          LOOP
        ENDIF
      ELSE
       IF EMPTY(MFGOPRHD.CCONTCODE)
         LOOP
       ENDIF 
      ENDIF      
      llOper = .T.
      EXIT
    ENDSCAN
     
    SELECT PoSHdr
    IF !llOper
      LOOP
    ENDIF

    SELECT MFGOPRHD
    =SEEK("I"+POSHDR.PO+lcOprCode)
    SCAN REST WHILE cimtyp+ctktno+coprcode = "I"+POSHDR.PO+lcOprCode
      SELECT (TmpPoCt)
      IF !SEEK(Mfgoprhd.cContcode+PoSHdr.Po+'I'+Syccomp.cComp_id)
        APPEND BLANK
        REPLACE Vendor     WITH Mfgoprhd.cContcode      ,;
                CutTktPo   WITH PoSHdr.Po               ,;
                ldEntere   WITH PoSHdr.Entered          ,;
                ldComplete WITH PoSHdr.Complete         ,;
                cMIType    WITH "I"                     ,; 
                cCompany   WITH Syccomp.cComp_id
      ENDIF
    ENDSCAN 

  ENDSCAN
  WAIT CLEAR
  SELECT (TmpPoCt)
  GOTO TOP
  llRecToprn = .F.
  IF EOF()
    SELECT SYCCOMP
    LOOP
  ELSE  
    llRecToprn = .T.
  ENDIF  
  lcVendCode = " "


  DO WHILE !EOF()
    IF Vendor <> lcVendCode
      *-- Reinitializing the weeks.
       STORE 0 TO lnWeek1,lnWeek2,lnWeek3,lnWeek4,lnWeek5,lnWeek6,lnWeek7,;
                  lnWeek8,lnWeek9,lnWeek10,lnWeek11,lnWeek12,lnWeek13,lnWeek14,;
                  lnWeek15,lnWeek16,lnWeek17,lnWeek18,lnWeek19,lnWeek20,lnWeek21
      lcVendCode = Vendor
    ENDIF
    SCAN REST WHILE Vendor+CutTktPo = lcVendCode
      lcScanFile = IIF(cMIType = "M","CutTktL","PoSLn")
      lcCond     = IIF(cMIType = "M",;
                   "CUTTKT+STYLE+DYELOT+TRANCD",;
                   "CSTYTYPE+PO+STYLE+STR(LINENO,6)+TRANCD")
      SELECT (lcScanFile)
      =SEEK(&TmpPoCt..CutTktPo)
      lnOpnQty = 0
      SCAN REST WHILE &lcCond = &TmpPoCt..CutTktPo
        WAIT WINDOW + " " + lcStyTit + " :" + Style NOWAIT

        IF &TmpPoCt..cMIType = "I"             
          IF !EMPTY(lcStyFile) .AND. llStyFile
            IF !SEEK(STYLE.CSTYMAJOR,lcStyFile)
              LOOP
            ENDIF
          ENDIF      
        ENDIF

        IF !EMPTY(lcFabFile) .AND. llFabFile
          IF !SEEK(Style.Fabric,lcFabFile)
            LOOP
          ENDIF
        ENDIF
        IF !EMPTY(lcGroups) AND !(Style.Cstygroup $ lcGroups)
          LOOP
        ENDIF  
        IF !EMPTY(lcColors) AND !(SUBSTR(STYLE.STYLE,lnStartPos,lnNonMajLn) $ lcColors)
          LOOP
        ENDIF

        lnOpnQty = lnOpnQty + IIF(TranCd = "1",TotQty,;
                              IIF(&TmpPoCt..cMIType = "M",;
                              IIF(TranCd$"234",-TotQty,0),;
                              IIF(TranCd$"245",-TotQty,0)))
      ENDSCAN

      DO CASE
        CASE &TmpPoCt..ldComplete <= ldWeek1
          lnWeek1 = lnWeek1 + lnOpnQty
        CASE &TmpPoCt..ldComplete >= ldWeek21
          lnWeek21 = lnWeek21 + lnOpnQty
        OTHERWISE
          FOR lnCounter = 2 TO 20
            lcCounter = ALLTRIM(STR(lnCounter))
            ln1stDate = ldWeek&lcCounter - 6
            IF BETWEEN(&TmpPoCt..ldComplete,ln1stDate,ldWeek&lcCounter)
              lnWeek&lcCounter = lnWeek&lcCounter + lnOpnQty
            ENDIF
          ENDFOR
      ENDCASE    
      SELECT(TmpFile)
      =SEEK(lcVendCode,'APVendor')
      IF lnOpnQty > 0 AND !SEEK(lcVendCode+APVendor.ccategtype+Syccomp.cComp_id)
        APPEND BLANK
      ENDIF
      IF lnOpnQty > 0
        REPLACE cVendor  WITH lcVendCode,;
                cCompany WITH Syccomp.cComp_id,;
                nTotWip  WITH 0
        FOR lnCounter = 1 TO 21
          lcCounter = ALLTRIM(STR(lnCounter))
          REPLACE nWeek&lcCounter WITH  lnWeek&lcCounter,;      
                  nTotWip         WITh nTotWip + nWeek&lcCounter
        ENDFOR
        REPLACE cType WITH APVendor.ccategtype
      ENDIF  
    ENDSCAN
    WAIT CLEAR
    SELECT (TmpPoCt)
  ENDDO  
  SELECT (TmpPoCt)
  ZAP
ENDDO

IF !llRecToprn
  ***There are no records to display***
  =gfModalGen('TRM00052B00000','DIALOG')
  RETURN .F.
ENDIF  
*-- End of lfGetData.


*!*************************************************************
*! Name      : lfPrnRep
*! Developer : Wael Mohammed Anwar
*! Date      : 07/18/2000
*! Purpose   : To print the report.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            :  lfPrnRep()
*!*************************************************************
*! Note      : 
*!*************************************************************
FUNCTION lfPrnRep
PRIVATE llPrint,lnGrandTot,lnDomestic,lnOffShore,lnNotAssig

llPrint    = .F.
STORE 0 TO lnGrandTot,lnDomestic,lnOffShore,lnNotAssig,lnCapacity
*-- variable to hold the totals of the total WIP and total WeekValues
STORE 0 TO lnTotW1,lnTotW2,lnTotW3,lnTotW4,lnTotW5,lnTotW6,lnTotW7,;
           lnTotW8,lnTotW9,lnTotW10,lnTotW11,lnTotW12,lnTotW13,lnTotW14,;
           lnTotW15,lnTotW16,lnTotW17,lnTotW18,lnTotW19,lnTotW20,lnTotW21
STORE 0 TO lnWeekV1,lnWeekV2,lnWeekV3,lnWeekV4,lnWeekV5,lnWeekV6,lnWeekV7,;
           lnWeekV8,lnWeekV9,lnWeekV10,lnWeekV11,lnWeekV12,lnWeekV13,lnWeekV14,;
           lnWeekV15,lnWeekV16,lnWeekV17,lnWeekV18,lnWeekV19,lnWeekV20,lnWeekV21

CLEAR TYPEAHEAD
SET DEVICE TO PRINT

SELECT (TmpFile)
GOTO TOP

DO WHILE !EOF()
  WAIT WINDOW "Vendor "+ALLTRIM(cVendor) + " in company "+cCompany NOWAIT
  STORE 0 TO lnWeek1,lnWeek2,lnWeek3,lnWeek4,lnWeek5,lnWeek6,lnWeek7,;
             lnWeek8,lnWeek9,lnWeek10,lnWeek11,lnWeek12,lnWeek13,lnWeek14,;
             lnWeek15,lnWeek16,lnWeek17,lnWeek18,lnWeek19,lnWeek20,lnWeek21,;
             lnTotWip,lnCapacity  
  lcVen = cVendor
  lcCompany = SPACE(2)
  SCAN REST WHILE cVendor = lcVen
    =SEEK(cCompany,'SycComp')
    IF cCompany <> lcCompany
      IF USED('Apvendor')
        SELECT Apvendor
        USE
        lcDir=ALLTRIM(SycComp.cCom_ddir)
        =gfOpenFile(lcDir+'Apvendor',lcDir+'VenCode','SH')       
      ENDIF  
      =SEEK(lcVen,'Apvendor')              
      lnCapacity = lnCapacity + Apvendor.NCapacity
      lcCompany = &TmpFile..cCompany    
    ENDIF
    SELECT (TmpFile)
    IF SEEK(lcVen,'Apvendor')
      FOR lnCounter = 1 TO 21
        lcCounter = ALLTRIM(STR(lnCounter))
        lnWeek&lcCounter = lnWeek&lcCounter + nWeek&lcCounter        
        *-- Calculate the totals of WIP/Week
        lnTotW&lcCounter = lnTotW&lcCounter + nWeek&lcCounter
      ENDFOR
      lnTotWip   = lnTotWip + nTotWip
      lnGrandTot = lnGrandTot + nTotWip
      DO CASE  
        CASE  Apvendor.cCategType = "D"
          lnDomestic = lnDomestic + nTotWip
        CASE Apvendor.cCategType = "O"
          lnOffShore = lnOffShore + nTotWip
        CASE Apvendor.cCategType = "N"
          lnNotAssig = lnNotAssig + nTotWip
      ENDCASE  
    ENDIF
  ENDSCAN
  SELECT (TmpFile)
  =IIF (Row >= 43,lfPrnHdr(),.F.)
  IF SEEK(lcVen,'ApVendor')  
    llPrint = .T.
    @ Row,00 SAY lcVen
    @ Row,09 SAY LEFT(ApVendor.cVencomp,15)
    @ Row,25 SAY lnCapacity PICTURE "999999"
    lnCol = 32
    FOR lnCounter = 1 TO 21
      lcCounter = ALLTRIM(STR(lnCounter))
      @ Row,lnCol SAY lnWeek&lcCounter PICTURE "@Z 999999"
      lnCol = lnCol + 7
    ENDFOR
    @ Row,179 SAY lnTotWip PICTURE "@Z 999999"
    Row = Row + 1
    =IIF (Row >= 43,lfPrnHdr(),.F.)
    lnNegVal   = 0
    lnWeekValu = 0
    lnCol = 32
    FOR lnCounter = 1 TO 21
      lcCounter = ALLTRIM(STR(lnCounter))
      IF lnWeek&lcCounter > 0
        lnWeekValu = lnCapacity +IIF(lnNegVal<0,lnNegVal,0)- lnWeek&lcCounter
        lnNegVal   = 0
        IF lnWeekValu > 0
          @ Row,lnCol SAY lnWeekValu PICTURE "@Z 999999"
        ELSE
          lnNegVal = lnWeekValu
          @ Row,lnCol SAY IIF(lnNegVal<0,PADL("<"+ALLTRIM(STR(ABS(lnWeekValu)))+">",6),"")
        ENDIF          
        *-- Calculate the total for the WeekValue/Week
        lnWeekV&lcCounter = lnWeekV&lcCounter + lnWeekValu
      ELSE
        IF lnNegVal < 0
          @ Row,lnCol SAY IIF(lnCapacity+lnWeekValu<0,;
                          PADL("<"+ALLTRIM(STR(ABS(lnCapacity+lnWeekValu)))+">",6),;
                          lnCapacity+lnWeekValu) PICTURE "@Z 999999"
         lnNegVal = 0
        ENDIF
      ENDIF    
      lnCol = lnCol + 7
    ENDFOR
    Row = Row + 1  
  ENDIF
ENDDO
WAIT CLEAR
IF !llPrint
  =IIF (Row >= 43,lfPrnHdr(),.F.)
  @ Row,52 SAY "No records have been printed"
ELSE
 =lfPrnTotal()
ENDIF
DO ENDREPORT
SET DEVICE TO SCREEN
*-- End of lfPrnRep.


*!*************************************************************
*! Name      : lfRptHdr
*! Developer : Wael Mohammed Anwar
*! Date      : 07/18/2000
*! Purpose   : To print the first report header
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            :  lfRptHdr()
*!*************************************************************
*! Note      : 
*!*************************************************************
PROCEDURE lfRptHdr
PRIVATE lnCol1,lnCol2,lnCol3

lnCol1 = ((185 - (LEN(TRIM(gccom_name))))/2)
lnCol2 = ((185 - (LEN(R_TITLE)))/2)
lnCol3 = ((185 - (LEN(TRIM(lcRpOpTlt))))/2)
@ 01,000    SAY "POROB590"
@ 01,lnCol1 SAY gccom_name
@ 01,175    SAY gdSysDate
@ 01,184    SAY '~'
@ 02,000    SAY TIME()
@ 02,lnCol2 SAY R_TITLE 
@ 02,175    SAY 'PAGE#'
@ 02,181    SAY STR(PAGENO,4)
@ 03,lnCol3 SAY lcRpOpTlt
@ 04,00 SAY REPLICATE('*',185)
*-- End of lfRptHdr.


*!*************************************************************
*! Name      : lfPrnHdr
*! Developer : Wael Mohammed Anwar
*! Date      : 07/18/2000
*! Purpose   : To print the report's header.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            :  lfPrnHdr()
*!*************************************************************
*! Note      : 
*!*************************************************************
FUNCTION lfPrnHdr

PageNo = PageNo + 1
=lfRptHdr()
Row = 5
*0----*----1----*----2----*----3----*----4----*----5----*----6----*----7----*----8----*----9----*----0----*----1----*----2----*----3----*----4----*----5----*----6----*----7----*----8----*
*Vendor   Name            W.Cap.  XX/XX  XX/XX  XX/XX  XX/XX  XX/XX  XX/XX  XX/XX  XX/XX  XX/XX  XX/XX  XX/XX  XX/XX  XX/XX  XX/XX  XX/XX  XX/XX  XX/XX  XX/XX  XX/XX  XX/XX  XX/XX 9999999
@ Row,00 SAY "Vendor   Name            W.Cap."
@ Row,33 SAY SUBSTR(DTOC(ldWeek1),1,5)

lnCol = 40
FOR lnCounter = 2 TO 21
  lcCounter = ALLTRIM(STR(lnCounter))
  @ Row,lnCol SAY SUBSTR(DTOC(ldWeek&lcCounter),1,5)
  lnCol = lnCol + 7
ENDFOR
@ Row,179 SAY "TotWIP"
Row = Row + 1
@ Row,00 SAY REPLICATE("-",185)
Row = Row + 1
*-- End of lfPrnHdr.


*!*************************************************************
*! Name      : lfPrnTotal
*! Developer : Wael Mohammed Anwar
*! Date      : 07/18/2000
*! Purpose   : To print the totals.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            : lfPrnTotal()
*!*************************************************************
*! Note      : 
*!*************************************************************
FUNCTION lfPrnTotal

Row = Row + 1
=IIF (Row >= 43,lfPrnHdr(),.F.)
@ Row,00 SAY REPLICATE("-",185)
Row = Row + 1
=IIF (Row >= 43,lfPrnHdr(),.F.)

@ Row,00 SAY "Grand Total  : "
@ Row,19 SAY lnGrandTot PICTURE "@Z 999999"
lnCol = 32
FOR lnCounter = 1 TO 21
  lcCounter = ALLTRIM(STR(lnCounter))
  @ Row,lnCol SAY lnTotW&lcCounter PICTURE "@Z 999999"
  lnCol = lnCol + 7
ENDFOR  
Row = Row + 1
=IIF (Row >= 43,lfPrnHdr(),.F.)
@ Row,00 SAY "Domestic     %"
@ Row,16 SAY (lnDomestic/lnGrandTot) * 100 PICTURE "@Z 999999.99"
lnCol = 32
FOR lnCounter = 1 TO 21
  lcCounter = ALLTRIM(STR(lnCounter))
  @ Row,lnCol SAY lnWeekV&lcCounter PICTURE "@Z 999999"
  lnCol = lnCol + 7
ENDFOR  
Row = Row + 1
=IIF (Row >= 43,lfPrnHdr(),.F.)
@ Row,00 SAY "Off Shore    %"
@ Row,16 SAY (lnOffShore/lnGrandTot) * 100 PICTURE "@Z 999999.99"
Row = Row + 1
=IIF (Row >= 43,lfPrnHdr(),.F.)
@ Row,00 SAY "Not assigned %"
@ Row,16 SAY (lnNotAssig/lnGrandTot) * 100 PICTURE "@Z 999999.99"
Row = Row + 1
=IIF (Row >= 43,lfPrnHdr(),.F.)
@ Row,00 SAY REPLICATE("-",185)
*-- End of lfPrnTotal.


*!*************************************************************
*! Name      : lfOpnFiles
*! Developer : Wael Mohammed Anwar
*! Date      : 07/18/2000
*! Purpose   : To open the files of the selected company
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            : lfOpnFiles()
*!*************************************************************
*! Note      : 
*!*************************************************************
FUNCTION lfOpnFiles

IF USED('CutTktH')
  SELECT CutTktH
  USE
ENDIF
IF USED('CutTktL')
  SELECT CutTktL
  USE
ENDIF
IF USED('PoSHdr')
  SELECT PoSHdr
  USE
ENDIF
IF USED('PoSLn')
  SELECT PoSLn
  USE
ENDIF
IF USED('Style')
  SELECT Style
  USE
ENDIF
IF USED('Mfgoprhd')
  SELECT Mfgoprhd
  USE
ENDIF
IF USED('Fabric')
  SELECT Fabric
  USE
ENDIF
IF USED('ApVendor')
  SELECT ApVendor
  USE
ENDIF
 
lcDir=ALLTRIM(SycComp.cCom_ddir)
=gfOpenFile(lcDir+'Fabric',lcDir+'Fabric','SH')
=gfOpenFile(lcDir+'Style',lcDir+'Style','SH')
=gfOpenFile(lcDir+'Mfgoprhd',lcDir+'Mfgoprhd','SH')
=gfOpenFile(lcDir+'PoSLn',lcDir+'PoSLn','SH')
SET RELATION TO Style INTO Style ADDI
=gfOpenFile(lcDir+'PoSHdr',lcDir+'PoSHdr','SH')
SET RELATION TO PO INTO Mfgoprhd
=gfOpenFile(lcDir+'CutTktH',lcDir+'CutTktH','SH')
SET RELATION TO Cuttkth.style INTO Style ADDITIVE
=gfOpenFile(lcDir+'CutTktL',lcDir+'CutTktL','SH')
=gfOpenFile(lcDir+'ApVendor',lcDir+'VenCode','SH')
*-- End of lfOpnFiles.


*!*************************************************************
*! Name      : lfOpnCCFil
*! Developer : Wael Mohammed Anwar
*! Date      : 07/18/2000
*! Purpose   : To open the files of the current company
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            : lfOpnCCFil()
*!*************************************************************
*! Note      : 
*!*************************************************************
FUNCTION lfOpnCCFil

IF USED('CutTktH')
  SELECT CutTktH
  USE
ENDIF
IF USED('CutTktL')
  SELECT CutTktL
  USE
ENDIF
IF USED('PoSHdr')
  SELECT PoSHdr
  USE
ENDIF
IF USED('PoSLn')
  SELECT PoSLn
  USE
ENDIF
IF USED('Style')
  SELECT Style
  USE
ENDIF
IF USED('Mfgoprhd')
  SELECT Mfgoprhd
  USE
ENDIF
IF USED('Fabric')
  SELECT Fabric
  USE
ENDIF
IF USED('ApVendor')
  SELECT ApVendor
  USE
ENDIF
 
lcDir = gcDataDir
=gfOpenFile(lcDir+'Fabric',lcDir+'Fabric','SH')
=gfOpenFile(lcDir+'Style',lcDir+'Style','SH')
=gfOpenFile(lcDir+'Mfgoprhd',lcDir+'Mfgoprhd','SH')
=gfOpenFile(lcDir+'PoSLn',lcDir+'PoSLn','SH')
SET RELATION TO Style INTO Style ADDI
=gfOpenFile(lcDir+'PoSHdr',lcDir+'PoSHdr','SH')
SET RELATION TO PO INTO Mfgoprhd
=gfOpenFile(lcDir+'CutTktH',lcDir+'CutTktH','SH')
SET RELATION TO Cuttkth.style INTO Style ADDITIVE
=gfOpenFile(lcDir+'CutTktL',lcDir+'CutTktL','SH')
=gfOpenFile(lcDir+'ApVendor',lcDir+'VenCode','SH')
*-- End of lfOpnCCFil.


*!*************************************************************
*! Name      : lfSRVFab
*! Developer : Wael Mohammed Anwar
*! Date      : 07/18/2000
*! Purpose   : control browsing primary fabric and validate 
*!           : selecting it in inlist function.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfSRVFab()
*!*************************************************************
*! Note      : SRV symbol is [S,Set--R,Reset--V,Valid]
*!*************************************************************
FUNCTION lfSRVFab
PARAMETERS lcParm
PRIVATE lcAlias,llHaveSty

DO CASE
  CASE lcParm = 'S'  && Set code
    USE (gcDataDir+'Fabric') AGAIN ALIAS FABRIC_X ORDER TAG FABRIC IN 0
    SELECT FABRIC
    SET ORDER TO TAG cFabric
    SET RELATION TO FABRIC.FABRIC INTO FABRIC_X
    GO TOP IN FABRIC
    llChFabric = .T.
  CASE lcParm = 'R'  && Reset code
    USE IN FABRIC_X
    SELECT FABRIC
    SET ORDER TO TAG FABRIC
  OTHERWISE      && Valid code
    lcAlias = ALIAS()
    SELECT STYLE
    LOCATE FOR Fabric = Fabric.Fabric AND ;
             !MAKE AND lDetCost AND STATUS = 'A'
    llHaveSty = FOUND()
    IF !llHaveSty
      = gfModalGen("INM34122B36000","Dialog",ALLTRIM(Fabric.Fabric))
    ENDIF
    SELECT (lcAlias)
    RETURN llHaveSty    && Record selected only if fabric found in style file.
ENDCASE
*-- End Of lfSRVFab.


*!*************************************************************
*! Name      : lfsrvSty
*! Developer : Wael Mohammed Anwar
*! Date      : 07/18/2000
*! Purpose   : Rise change style flag, in range browse screen.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfsrvSty()
*!*************************************************************
*! Note      : SRV symbol is [S,Set -- R,Reset -- V,Valid]
*!*************************************************************
FUNCTION lfSRVSty
PARAMETERS lcParm

DO CASE
  CASE lcParm = 'S'  && Set code
    USE (gcDataDir+'Style') AGAIN ALIAS STYLE_X ORDER TAG Style IN 0
    SELECT STYLE
    SET ORDER TO TAG Cstyle
    SET RELATION TO STYLE.STYLE INTO STYLE_X
    GO TOP IN STYLE
    llChStyle = .T.
  CASE lcParm = 'R'  && Reset code
    USE IN STYLE_X
    SELECT STYLE
    SET ORDER TO TAG STYLE
ENDCASE
*-- End Of lfsrvSty.


*!*************************************************************
*! Name      : lfStySum
*! Developer : Wael Mohammed Anwar
*! Date      : 07/18/2000
*! Purpose   : sum a specific field for the current style in style file
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Called from : Option Grid,style browse calculated fields.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : Calculated field value.
*!*************************************************************
*! Example   : =lfStySum()
*!*************************************************************
FUNCTION lfStySum
PARAMETERS lcSty,lccomp,lnAddToVar
PRIVATE lnStyRec

lnStyRec = IIF(RECNO('STYLE') <= RECCOUNT('STYLE'),RECNO('STYLE'),1)
lnTotcomp = 0
SELECT Style_X
SUM &lcCOMP TO lnTotcomp WHILE Style = ALLTRIM(lcSty)
SELECT Style
GO lnStyRec
DO CASE
  CASE lnAddToVar = 1
  	lnO_T_S = lnTotcomp
  CASE lnAddToVar = 2
  	lnO_T_S = lnO_T_S + lnTotcomp
  CASE lnAddToVar = 3
  	lnO_T_S = lnO_T_S - lnTotcomp
ENDCASE
RETURN INT(lnTotcomp)
*-- End Of lfStySum.


*!*************************************************************
*! Name      : lfEvalSeg
*! Developer : Wael Mohammed Anwar
*! Date      : 07/18/2000
*! Purpose   : To evaluate the non-major segment.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfEvalSeg()
*!*************************************************************
FUNCTION lfEvalSeg
PRIVATE lnClrPos

DIMENSION laMajSeg[1,1]

= gfItemMask(@laMajSeg)
lnClrPos   = INT(ASCAN(laMajSeg,'C')/7+0.9)
lnStartPos = laMajSeg[lnClrPos,4]
lnNonMajLn = LEN(laMajSeg[lnClrPos,3])
*-- End Of lfEvalSeg.


*!*************************************************************
*! Name      : lfsrvVen
*! Developer : Wael Mohammed Anwar
*! Date      : 07/18/2000
*! Purpose   : Rise change Vendor flag, in range browse screen.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfsrvVen()
*!*************************************************************
*! Note      : SRV symbol is [S,Set -- R,Reset -- V,Valid]
*!*************************************************************
FUNCTION lfSRVVen
PARAMETERS lcParm

IF lcParm = 'S'  && Set code
  GO TOP IN APVENDOR
ENDIF
*-- End Of lfSRVVen.


*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Wael Mohammed Anwar
*! Date      : 07/18/2000
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************
FUNCTION lfwRepWhen
PRIVATE lnI,lnCurAlias,lcCurTag

DIMENSION laCompDesc[1,1]
DECLARE laRpSource[3],laRpTarget[1],laRpTComp[1]
STORE SPACE(0) TO laCompDesc[1,1],laRpTComp[1]

lnStatus = lcRpStatus
STORE 'Open'    TO laRpSource[1] 
STORE ''          TO laRpTarget[1]
STORE 'Hold'      TO laRpSource[2] &&,laRpTarget[2]
STORE 'Actualize' TO laRpSource[3]
*lcRpStatus = 'OH'

lnCurAlias = SELECT(0)
SELECT SycComp
lcCurTag = ORDER()
SET ORDER TO Ccomp_id
PRIVATE lnI
lnI = 1
SCAN
  lnI = ALEN(laCompDesc,1) + IIF(EMPTY(laCompDesc[lnI]),0,1)
  DIMENSION laCompDesc[lnI,1]
  laCompDesc[lnI,1] = SycComp.cComp_ID+"-"+SycComp.cCom_Name 
ENDSCAN

SET ORDER TO &lcCurTag

SELECT (lnCurAlias)
*-- End of lfwRepWhen.


*!*************************************************************
*! Name      : lfvOComp
*! Developer : Wael Mohammed Anwar
*! Date      : 07/18/2000
*! Purpose   : Call Mover to Select Company
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvOComp()
*!*************************************************************
FUNCTION lfvOComp

DIMENSION laRpComp[5]
STORE '' TO laRpComp

DO WHILE .T.
  = gfMover(@laCompDesc,@laRpTComp,'Select Company',.T.,'')  && call mover function.
  IF ALEN(laRpTComp,1) > 5
    =gfModalGen(.f.,.f.,.f.,.f.,'You can not choose more than 5 companies.')  
  ELSE
    EXIT
  ENDIF
ENDDO

IF !EMPTY(laRpTComp)
  FOR lnI = 1 TO ALEN(laRpTComp,1)
    laRpComp[lnI] = LEFT(laRpTComp[1,lnI],2)
  ENDFOR
ENDIF
*-- End of lfvOComp.


*!*************************************************************
*! Name      : lfvOStatus
*! Developer : Wael Mohammed Anwar
*! Date      : 07/18/2000
*! Purpose   : Call Mover to Select Status
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvOStatus()
*!*************************************************************
FUNCTION lfvOStatus
PRIVATE lcOldStat,lcCurrChr

lcOldStat = lcRpStatus  && Save old status value.

= gfMover(@laRpSource,@laRpTarget,'Select C/T And PO Status',.T.,'')  && call mover function.

lcRpStatus = ' '
*-- Loop to make Status expression.
FOR lnI = 1 TO ALEN(laRpTarget,1)
  lcRpStatus = lcRpStatus + IIF(laRpTarget[lnI] = 'Open','O',;
                            IIF(laRpTarget[lnI] = 'Hold','H',;
                            IIF(laRpTarget[lnI] = 'Actualize','A','')))

ENDFOR  && end Loop to make Status expression.

lcRpStatus = IIF(EMPTY(lcRpStatus),lcRpStatus,ALLTRIM(lcRpStatus))


*!*************************************************************
*! Name      : lfItmPos
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/31/1999
*! Purpose   : Postion in fixed filter array.
*!*************************************************************
*
FUNCTION lfItmPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos

lnItmPos = ASCAN(laOGFxFlt,lcItmInFlt)
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(laOGFxFlt,lnItmPos,1)
ENDIF
RETURN lnItmPos
*-- end of lfItmPos.


*!*************************************************************
*! Name      : lfItemFilt
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/31/1999
*! Purpose   : Item Filter.
*!*************************************************************
*
FUNCTION lfItemFilt
PARAMETERS lnCurrFilt
PRIVATE lnCurrFilt , lnNoOfCols , laTempExpr , lcFilter
lnNoOfCols = ALEN(laOGFxFlt,2)
DIMENSION laTempExpr[1,lnNoOfCols]
lnCurrFilt = (lnCurrFilt - 1) * lnNoOfCols + 1
=ACOPY(laOGFxFlt,laTempExpr,lnCurrFilt,lnNoOfCols,1)
lcFilter = gfGenFlt('laTempExpr',.T.,.T.)
RETURN lcFilter
*-- end of lfItemFilt.


*!*************************************************************
*! Name      : lfAddTo
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/31/1999
*! Purpose   : Create Expression.
*!*************************************************************
*
FUNCTION lfAddTo
PARAMETERS lcExpr , lcAddedExp
PRIVATE lcExpr , lcAddedExp
IF !EMPTY(lcExpr) AND !EMPTY(lcAddedExp)
  lcExpr = lcExpr + [ AND ]
ENDIF  
lcExpr = lcExpr + lcAddedExp
*-- end of lfAddTo.


*-- End of Functions --*