*!********************************************************************
*: Program file  : ICEIL100.PRG (REFERE TO C#200083)
*: Program desc. : UPC PRICE TICKETS FOR EILEEN FISHER
*: For screen    : None.
*:         System: ARIA APPAREL SERIES
*:         Module: TEXT FILE
*:      Developer: Ahmed Salah Shalaby -(SSH)
*!********************************************************************
*: Calls         : FUNCTIONS  : lfPrepFils,lfSelData,lfPrnText,lfWOpGrid
*:                              lfvVendor,lfSubExp.
*!********************************************************************
*: Passed Parameters  : NONE
*!********************************************************************
*C102007,1 HBG 11/20/2000 Add option Select by C/T or PO 
*!********************************************************************
llDyelot = (gfGetMemVar('M_DYELOT',gcAct_Comp)='Y')
*-- Declaring the necessary variables
lcBillName = ''
lcShipName = ''
IF !EMPTY(lcRPBillTo)
  lcBillName = IIF(SEEK(lcRPBillTo,'APVENDOR'),ApVendor.cVenComp,'')
ENDIF
IF !EMPTY(lcRPShipTo)
  lcShipName = IIF(SEEK(lcRPShipTo,'APVENDOR'),ApVendor.cVenComp,'')
ENDIF
lcMajPic = "@! " + gfItemMask("PM")
lnMajPic = LEN(gfItemMask("PM"))
lcPoNumber = lcRpPoNo  
lcTerms    = lcRpTerm
ldDelDate  = ldRpDlDate
lnPercToPc = lnRpPer
lcSelSize  = lcRpSelSiz
lnFileHndl = 0
FOR lnInd  = 1 TO ALEN(laOgFxFlt,1)
   *C102007,1 HBG 11/20/2000 If Select by CutTkt [Begin]
   IF lcRpSelBy = 'C'
   *C102007,1 [End]
     IF ALLTRIM(laOgFxFlt[lnInd,1]) = 'CUTTKTH.CUTTKT'
       TmpCutFile = laOgFxFlt[lnInd,6]
     ENDIF
   *C102007,1 HBG 11/20/2000 If Select by PO Get the master File which is "CUTTKTH" [Begin]
   ELSE
     IF ALLTRIM(laOgFxFlt[lnInd,1]) = 'POSHDR.PO'
       TmpPOFile = laOgFxFlt[lnInd,6]
     ENDIF
   ENDIF
   *C102007,1 [End]
ENDFOR

*C102007,1 HBG 11/20/2000 If Select by CutTkt [Begin]
IF lcRpSelBy = 'C'
*C102007,1 [End] 
  IF !USED(TmpCutFile)
    =gfModalGen('TRM00052B00000','DIALOG')
    RETURN
    SET DEVICE TO SCREEN
  ELSE
    SELECT (TmpCutFile)
    GOTO TOP
    IF EOF()
      =gfModalGen('TRM00052B00000','DIALOG')
      RETURN
      SET DEVICE TO SCREEN
    ENDIF
  ENDIF
  IF USED(TmpCutFile)
    SELECT CutTktH
    SET ORDER TO CutTktH
    SELECT(TmpCutFile)
    SET RELATION TO CUTTKT INTO CutTktH ADDIT
    IF !EMPTY(lcRpProdCod)
      DELETE ALL FOR PADR(CutTktH.Style,LEN(ALLTRIM(lcRpProdCod))) <> ALLTRIM(lcRpProdCod)
    ENDIF
    SELECT (TmpCutFile)
    SET RELATION TO
    SELECT CutTktH
    SET ORDER TO TAG CUTTKTHS
  ENDIF
*C102007,1 HBG 11/20/2000 If Select by PO check if there is Data to Desplay [Begin]
ELSE
  IF !USED(TmpPOFile)
    =gfModalGen('TRM00052B00000','DIALOG')
    RETURN
    SET DEVICE TO SCREEN
  ELSE
    SELECT (TmpPOFile)
    GOTO TOP
    IF EOF()
      =gfModalGen('TRM00052B00000','DIALOG')
      RETURN
      SET DEVICE TO SCREEN
    ENDIF
  ENDIF
ENDIF
*C102007,1 [End] 

TmpCutLine = gfTempName()


*-- These functions are used to open the necessary files and draw the grid
IF lfPrepFils()
 *-- lfSelData function is used to select the cutkts for printing
 IF lfSelData()
   *-- To print the selected cuttkts
   =lfPrnText()
 ENDIF
ENDIF
IF USED(TmpCutLine)
  USE IN &TmpCutLine
  ERASE &gcWorkDir.&TmpCutLine+'.DBF'
  ERASE &gcWorkDir.&TmpCutLine+'.CDX'
ENDIF
WAIT CLEAR
SET DEVICE TO SCREEN

*!*************************************************************
*! Name      : lfPrepFils   (REFERE TO C#200083)
*! Developer : Ahmed Salah Shalaby -(SSH)
*! Date      : 22/06/99
*! Purpose   : Open the necessary files
*!*************************************************************
*! Calls     :
*!*************************************************************
*! Passed Parameters  : 
*!*************************************************************
*! Returns            : 
*!*************************************************************
*! Example            :  lfPrepFils()
*!*************************************************************
FUNCTION lfPrepFils

*-- opening the vendor file according to the A/P setup.
SELECT STYLE
SET RELATION TO 'S'+SCALE INTO SCALE

*C102007,1 HBG 11/20/2000 IF Select by C/T , SELECT C/t line 'CUTTKTL' [Begin]
IF lcRpSelBy = 'C'
  SELECT CUTTKTL
ELSE  && SELECT PO line 'POSLN'
  SELECT POSLN
ENDIF
*C102007,1 [END]

=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+2,4]
laFileStru[lnFileStru+1,1] = 'nPrice'
laFileStru[lnFileStru+1,2] = 'N'
laFileStru[lnFileStru+1,3] = 7
laFileStru[lnFileStru+1,4] = 2
laFileStru[lnFileStru+2,1] = 'cStyDesc'
laFileStru[lnFileStru+2,2] = 'C'
laFileStru[lnFileStru+2,3] = 20
laFileStru[lnFileStru+2,4] = 0
CREATE TABLE (gcWorkDir+TmpCutLine) FROM ARRAY laFileStru

*C102007,1 HBG 11/20/2000 IF Select by C/T , INDEX the C/t Temp line [Begin]
IF lcRpSelBy = 'C'
  INDEX ON STR(nPrice,7,2)+CutTkt+Style+Color TAG (TmpCutLine)
ELSE  &&  INDEX the PO Temp line
  INDEX ON STR(nPrice,7,2)+PO+Style TAG (TmpCutLine)
ENDIF
*C102007,1 [End]

*!*************************************************************
*! Name      : lfSelData    (REFERE TO C#200083)
*! Developer : Ahmed Salah Shalaby -(SSH)
*! Date      : 22/06/99
*! Purpose   : To select the data for printing
*!*************************************************************
*! Calls     :
*!*************************************************************
*! Passed Parameters  : 
*!*************************************************************
*! Returns            : 
*!*************************************************************
*! Example            :  lfSelData()
*!*************************************************************
FUNCTION lfSelData

*C102007,1 HBG 11/20/2000 If Select By CutTkt select Temp CutTkt master file and order it [Begin]
IF lcRpSelBy = 'C'
*C102007,1 [End]
  SELECT (TmpCutFile)
*C102007,1 HBG 11/20/2000 If Select By PO select Temp PO master file and order it [Begin]  
ELSE
 SELECT (TmpPOFile)
ENDIF 
*C102007,1 [End]

GOTO TOP
IF !FILE(gcWorkDir+"PRICETKT.TXT")
  lnFileHndl = FCREATE(gcWorkDir+"PRICETKT.TXT",0)
ELSE
  *--- 'PRICETKT.TXT is already exist!','\!\<Append;\<Overwrite;\<Cancel')
  lnChoice = gfModalGen('QRM42182B42016','DIALOG')
  DO CASE
    CASE lnChoice = 1
     lnFileHndl = FOPEN(gcWorkDir+"PRICETKT.TXT",12)
     = FSEEK(lnFileHndl,0,2)
    CASE lnChoice = 2
      lnFileHndl = FCREATE(gcWorkDir+"PRICETKT.TXT",0)
    CASE lnChoice = 3
      RETURN(.F.)
  ENDCASE    
ENDIF
IF lnFileHndl < 0 
  *--- 'Can not create the output UPCTKT.TXT file.'
   =gfModalGen('TRM42183B00000','DIALOG')
  RETURN(.F.)
ENDIF
*C102007,1 HBG 11/20/2000 If Select By CutTkt scan Temp CutTkt master file to get the data[Begin]
IF lcRpSelBy = 'C'
*C102007,1 [End]
  SELECT CutTktL
  SET ORDER TO CutTktL
  SELECT (TmpCutFile)
  SCAN
    IF SEEK (&TmpCutFile..CutTkt,'CutTktL')
      SELECT CutTktL
      SCAN REST WHILE CutTkt = &TmpCutFile..CutTkt FOR TRANCD ='1'
        SCATTER MEMVAR MEMO
        SELECT (TmpCutLine)
        APPEND BLANK
        GATHER MEMVAR MEMO
        =SEEK(Style,'Style')    
        REPLACE nPrice   WITH IIF(Style.nSugRetPri>0,Style.nSugRetPri,;
                                                   Style.PriceA * 2 + 2),;
                cStyDesc WITH Style.Desc
      ENDSCAN
    ENDIF
  ENDSCAN
*C102007,1 HBG 11/20/2000 If Select By PO scan Temp PO master file to get the data[Begin]
ELSE
  SELECT POSLN
  SET ORDER TO POSLN
  lcForExp = IIF(!EMPTY(lcRpProdCod),;
             "PADR(Style,LEN(ALLTRIM(lcRpProdCod)))=ALLTRIM(lcRpProdCod) AND TRANCD ='1'" ,"TRANCD ='1'")
  SELECT (TmpPOFile)
  SCAN
    IF SEEK ('P' + &TmpPOFile..PO,'POSLN')
      SELECT POSLN
      SCAN REST WHILE cStyType+Po+Style+STR(LineNo,6)+TranCd = 'P'+&TmpPOFile..PO;
                FOR &lcForExp 
        SCATTER MEMVAR MEMO
        SELECT (TmpCutLine)
        APPEND BLANK
        GATHER MEMVAR MEMO
        =SEEK(Style,'Style')    
        REPLACE nPrice   WITH IIF(Style.nSugRetPri>0,Style.nSugRetPri,;
                                                   Style.PriceA * 2 + 2),;
                cStyDesc WITH Style.Desc
      ENDSCAN
    ENDIF
  ENDSCAN
ENDIF
*C102007,1 [End]
*!*************************************************************
*! Name      : lfPrnText  (REFERE TO C#200083)
*! Developer : Ahmed Salah Shalaby -(SSH)
*! Date      : 22/06/99
*! Purpose   : To print the selected cuttkts
*!*************************************************************
*! Calls     :
*!*************************************************************
*! Passed Parameters  : 
*!*************************************************************
*! Returns            : 
*!*************************************************************
*! Example            :  lfPrnText()
*!*************************************************************
FUNCTION lfPrnText

*-- Printing the header
SELECT (TmpCutLine)
GOTO TOP
lnPrice = 0
SCAN WHILE INKEY() <> 32
  *C102007,1 HBG 11/20/2000 print 'CutTkt' OR 'PO' according to the option select by [Begin]
  *WAIT WINDOW 'Printing Cuttkt No. ' + CutTkt NOWAIT
  WAIT WINDOW 'Printing Cuttkt No. ' + IIF(lcRpSelBy = 'C',CutTkt,PO) NOWAIT
  *C102007,1 [End]
  IF lnPrice <> nPrice
    *-- Printing the header
    =FPUTS(lnFileHndl,'ISA')
    lcSecondLn = ALLTRIM(lcBillName) +','
    lcSecondLn = lcSecondLn + ALLTRIM(lcShipName) + ','
    lcSecondLn = lcSecondLn + ALLTRIM(lcPoNumber) + ','
    lcSecondLn = lcSecondLn + ALLTRIM(lcRpProdCod) + ','
    lcSecondLn = lcSecondLn + ALLTRIM(lcTerms)    + ','
    lcSecondLn = lcSecondLn + IIF(ldDelDate <> {},DTOC(ldDelDate),'')
    =FPUTS(lnFileHndl,lcSecondLn)
    =FPUTS(lnFileHndl,'IEA')
    lnPrice = nPrice
  ENDIF  
  lcColrCod = SUBSTR(Style,lnMajPic+2)
  lcClrDesc = ALLTRIM(gfCodDes(lcColrCod,'COLOR     '))
  =SEEK(STYLE,'STYLE')
  IF lcSelSize = 'A'
    *-- All sizes
    lnFrstScl = 1
    lnLastScl = Scale.Cnt
  ELSE
    IF VAL(lcSelSize) > Scale.CNT
      LOOP
    ENDIF 
    STORE VAL(lcSelSize) TO lnFrstScl, lnLastScl
  ENDIF
  FOR lnCount = lnFrstScl TO lnLastScl
    lcSizeDesc = ' '
    lcStyUpce  = ' '
    lcDyelot   = ' '
    lcPrnLine  = ' '
    lcCount    = STR(lnCount,1)
    lcSizeDesc = ALLTRIM(Scale.SZ&lcCount)
    IF SEEK(Style+lcCount,'StyleUpc')
      lcStyUpce = ALLT(StyleUpc.cUpcNum1)+ALLT(StyleUpc.cUpcNum2)+ALLTRIM(StyleUpc.cUpcNum3)
    ENDIF  
    IF llDyelot AND Style.cDye_flg = 'Y'
      lcDyelot = Dyelot
    ENDIF
    lcTotQty = ALLTRIM(STR(INT(ROUND(((lnPercToPc/100) * Qty&lcCount),0))))
    IF VAL(lcTotQty) > 0
      *C102007,1 HBG 11/20/2000 print 'CutTkt' OR 'PO' according to the option select by [Begin]
      *lcPrnLine = CutTkt + ','
      lcPrnLine = IIF(lcRpSelBy = 'C',CutTkt,PO) + ','
      *C102007,1 [END]
      lcPrnLine = lcPrnLine + ALLTRIM(lcStyUpce) + ','
      lcPrnLine = lcPrnLine + ALLTRIM(SUBSTR(Style,1,lnMajPic)) + ','
      lcPrnLine = lcPrnLine + lcClrDesc          + ','
      lcPrnLine = lcPrnLine + lcSizeDesc         + ','
      lcPrnLine = lcPrnLine + ALLTRIM(lcDyelot)  + ','
      lcPrnLine = lcPrnLine + lcTotQty           + ','
      lcPrnLine = lcPrnLine + ALLTRIM(STR(nPrice,7,IIF(MOD(nPrice,1)=0,0,2))) + ','
      lcPrnLine = lcPrnLine + ALLTRIM(cStyDesc)
      =FPUTS(lnFileHndl,lcPrnLine)
    ENDIF
  ENDFOR
ENDSCAN
USE
=FCLOSE(lnFileHndl)
*C102007,1 HBG 11/20/2000 Inform the user that the Procces completed succesfuly [Begin]
lcMessg = "File "+gcWorkDir+"PRICETKT.TXT" + " has been successfully generated."
=gfModalGen('TRM00000B00000','DIALOG',.F.,.F.,lcMessg)
*C102007,1 [End]

*!*************************************************************
*! Name      : lfWOpGrid (REFERE TO C#200083)
*! Developer : Ahmed Salah Shalaby -(SSH)
*! Date      : 22/06/99
*! Purpose   : Option grid when function.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfWOpGrid()
*!*************************************************************
FUNCTION lfWOpGrid

SHOW GET pbRun DISABLE
ldRpDlDate = gdSysDate+10


*!*************************************************************
*! Name      : lfvVendor   (REFERE TO C#200083)
*! Developer : Ahmed Salah Shalaby -(SSH)
*! Date      : 22/06/99
*! Purpose   : Vaildate vendor
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvVendor()
*!*************************************************************
FUNCTION lfvVendor
PARAMETER lcVend

SELECT APVENDOR
SET ORDER TO TAG VenCode 
IF !EMPTY(&lcVend) .AND. ;
   ('?' $ &lcVend .OR. !SEEK(&lcVend , 'APVENDOR'))
  =gfApVnBrow(@&lcVend)
ENDIF

*!*************************************************************
*! Name      : lfSubExp  (REFERE TO C#200083)
*! Developer : Ahmed Salah Shalaby -(SSH)
*! Date      : 22/06/99
*! Purpose   : Suppress expretion function.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfSubExp()
*!*************************************************************
FUNCTION lfSubExp

*C102007,1 HBG 11/20/2000 Seek by Product code in the correct file according to
*C102007,1                     select by what [Begin]
IF lcRpSelBy = 'C'
*C102007,1 [End]
  SELECT CutTktH
  SET ORDER TO TAG CUTTKTHS
  IF !SEEK(ALLTRIM(lcRpProdCod))
    *---'No cutting tickets found for the entered product code.')
    =gfModalGen('TRM42184B00000','DIALOG')
  ELSE
    SELECT CutTktH
    SET FILTER TO
    SET RELATION TO
    SET ORDER TO TAG CUTTKTH
  ENDIF
*C102007,1 HBG 11/20/2000 Seek by Product code in the 'POSLN' file [Begin]
ELSE 
  SELECT POSLN
  SET ORDER TO TAG Poslns
  IF !SEEK(ALLTRIM(lcRpProdCod))
    *---'No PO found for the entered product code.')
    =gfModalGen('TRM00000B00000','DIALOG',.F.,.F.,'No PO found for the entered product code.')
  ELSE
    SELECT POSLN
    SET FILTER TO
    SET RELATION TO
    SET ORDER TO TAG POSLN
  ENDIF
ENDIF
*C102007,1 [End]

*!*************************************************************
*! Name      : lfClrRead
*! Developer : HEND GHANEM
*! Date      : 14/09/2000
*! Purpose   : Refresh the filters in the option grid 
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfClrRead ()
*!*************************************************************
*!*C102007,1
FUNCTION lfClrRead

*-- Refresh the filters in the option grid.
CLEAR READ

*-- End of lfClrRead.

*!*************************************************************
*! Name      : lfSRPO   
*! Developer : Hend Ghanem
*! Date      : 14/09/2000
*! Purpose   : Function to Go top in PO header file needed
*!             for the PO # field [For the In range]
*!*************************************************************
*! Called from : Picking ticket field [Option Grid]
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : 1) 'S' To set the relations
*!                     2) 'R' To release the relations
*!*************************************************************
*! Return      : None
*!*************************************************************
*!C102007,1
*!*************************************************************
FUNCTION lfSRPO   
PARAMETERS lcParm

DO CASE
  CASE lcParm = "S"
	SELECT POSHDR
    LOCATE
    SET RELATION TO 'P'+ PO INTO POSLN ADDITIVE
  CASE lcParm = "R"
   	SELECT POSHDR
    SET RELATION TO
ENDCASE