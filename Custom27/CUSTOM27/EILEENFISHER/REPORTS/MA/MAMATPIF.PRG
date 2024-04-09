*:***************************************************************************
*: Program file  : MAMATPIF.PRG
*: Program desc. : CUSTOMIZED Material PO Form For Eileen Fisher.
*: Date          : 10/07/2000
*: System        : Aria Advantage Series.
*: Module        : MATERIAL (MA)
*: Developer     : NADER (NAD)
*: Tracking Job Number: C101964
*: 
*:***************************************************************************
*: Calls : 						
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO MAMATPIF
*:***************************************************************************
*B804133,1 NAD 04/18/2001 All the modification done in the FRX (MAMATPIF.FRX)
*B804203,1 NAD 06/03/2001 Field for YARN WARP should be inserted between the
*        DYESTUFF field and the YARN FILL field.done in the FRX (MAMATPIF.FRX)

PRIVATE lnx
SELECT POFHdr

=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+1,4]
laFileStru[lnFileStru+1,1] = 'Fab'
laFileStru[lnFileStru+1,2] = 'C'
laFileStru[lnFileStru+1,3] = 7
laFileStru[lnFileStru+1,4] = 0

CREATE CURSOR (lcTmpFile) FROM ARRAY laFileStru
INDEX ON pomat+cmattype TAG (lcTmpFile)


SELECT POFLn 
SET FILTER TO Trancd = '1'
SET RELATION OFF INTO (lcNoteLns)
SELECT POFHdr
SET SKIP TO

SCAN FOR &lcRpExp 
  SCATTER MEMVAR MEMO
  SELECT (lcTmpFile)  
  APPEND BLANK  
  GATHER MEMVAR MEMO
  lcFab=POFln.Fabric
  FOR lnx=1 to 2
    APPEND BLANK
    REPLACE cMATTYPE  WITH IIF (lnx=1,'Y','Z'),;
            FAB       WITH lcFab              ,;
            POMAT     WITH m.POMAT            ,;
            STATUS    WITH m.Status             
  ENDFOR
  
ENDSCAN

SET RELATION TO 
*lcRpexp=[POFln.Trancd = '1']
lcRpexp='.T.'
SELECT (lcTmpFile)
SET RELATION TO vendor INTO Apvendor
SET RELATION TO cWareCode INTO WareHous ADDITIVE
SET RELATION TO cMatType + POMat INTO POFLN ADDITIVE 
SET RELATION TO 'M'+ CMATTYPE + POMAT INTO NOTEPAD_A ADDITIVE
SET SKIP TO  [POFLN]
=lfEndGroup()

*!*************************************************************
*! Name      : lfLstPg
*! Developer : Nader Anis Mitry (NAD)
*! Date      : 10/15/2000
*! Purpose   : Get the pages count
*!*************************************************************
*! Called from : Page header band.
*!*************************************************************
*! Calls     : 
*!             Procedures : lcDummy,lcDummy2
*!             Functions  :
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfLstPg()
*!*************************************************************


FUNCTION lfLstPg
PARAMETER lcDummy,lcDummy2

PRIVATE lcAlias,lnPage,lcNotPad,lnRecNo

DECLARE lnPage[1]
lcAlias = ALIAS()
STORE 0 TO lnLines
STORE SPACE(0) TO lcNotPad
Private lcMExp
IF cMatType='P'
  m.POMat=POMat
  SELECT MAX(lineNo) FROM POFLn INTO ARRAY lnPage;
         WHERE cmattype+pomat+fabric+color+trancd='P'+m.POMat AND TranCD='1'
  lnlines=lnPage[1]
  lnRecNo=RECNO('NotePad')

  IF llRpPrtPn AND SEEK ('MP'+&lcTmpFile..POMat,'NOTEPAD') 
    IF !EMPTY(ALLTRIM(NOTEPAD_A.MNOTES))
      lcNotPad = ALLTRIM(NOTEPAD_A.MNOTES)
    ELSE
      lcNotPad=""
    ENDIF 
  ENDIF

  IF BETWEEN(lnRecNo,1,RECCOUNT('NotePad'))
    GOTO lnRecNo IN NOTEPAD
  ENDIF 
  lnNoPg= CEIL((lnLines + (MEMLINES(lcNotPad)/3))/9) +2
ENDIF


*!*************************************************************
*! Name      : lfHeadVar
*! Developer : Nader Anis Mitry (NAD)
*! Date      : 10/15/2000
*! Purpose   : Get data to be printed on page header
*!*************************************************************
*! Called from : Page header band.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfRltFld,gfCodDes,gfGetAdr,lfAdrShift
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfHeadVar()
*!*************************************************************
FUNCTION lfHeadVar
PARAMETER lcDummy1,lcDummy2
PRIVATE lcPriceSmb , lcDutySmb

*-- Get Currency symbol for Price, and Duty [begin]
lcPriceSmb  = PADL(ALLTRIM(IIF(llRpFCurr,IIF(SEEK(&lcTmpfile..cPriceCur,'SYCCURR'),SycCurr.cCurrSmbl,''),IIF(SEEK(gcBaseCurr,'SYCCURR'),SycCurr.cCurrSmbl,''))),3)
lcDutySmb   = PADL(ALLTRIM(IIF(llRpFCurr,IIF(SEEK(&lcTmpfile..cDutyCur ,'SYCCURR'),SycCurr.cCurrSmbl,''),IIF(SEEK(gcBaseCurr,'SYCCURR'),SycCurr.cCurrSmbl,''))),3)
*-- Get Currency symbol for Price, and Duty [end]

lcMerch     = lcPriceSmb + ;
              ALLTRIM(STR(IIF(llRpFCurr,&lcTmpfile..nCost1,&lcTmpfile..neCost1),13,2))
lcFreight   = lcDutySmb  + ;
              ALLTRIM(STR(IIF(llRpFCurr,&lcTmpfile..nCost2,&lcTmpfile..neCost2),13,2))
lcTax       = lcDutySmb  + ;
              ALLTRIM(STR(IIF(llRpFCurr,&lcTmpfile..nCost3,&lcTmpfile..neCost3),13,2))
lcQuota     = lcDutySmb  + ;
              ALLTRIM(STR(IIF(llRpFCurr,&lcTmpfile..nCost4,&lcTmpfile..neCost4),13,2))

= gfRltFld(&lcTmpfile..CDivision , @laDivLName , 'CDIVISION')  && Get the division long name.

lcDivDesc   = gfCodDes(&lcTmpfile..CDIVISION, 'CDIVISION')     && Division description.
lcShipVia   = gfCodDes(&lcTmpfile..ShipVia , 'SHIPVIA')        && Ship Via description.
lcTerms     = gfCodDes(&lcTmpfile..CTermCode   , 'CTERMCODE')  && Terms description.

*-- Fill Vendor array with its data [Begin]
laVendor[1] = APVENDOR.CVenComp
laVendor[2] = gfGetAdr('APVENDOR' , '' , '' , '' , 1)
laVendor[3] = gfGetAdr('APVENDOR' , '' , '' , '' , 2)
laVendor[4] = gfGetAdr('APVENDOR' , '' , '' , '' , 3)
laVendor[5] = gfGetAdr('APVENDOR' , '' , '' , '' , 4)
laVendor[6] = gfGetAdr('APVENDOR' , '' , '' , '' , 5)

*-- Get the vendor addresses
= lfAdrShift('laVendor')
*-- Fill Vendor array with its data [End]

*-- Fill Ship to array with its data. [begin]
laShipTo[1] = WareHous.cDesc
laShipTo[2] = &lcTmpfile..COutAddr1
laShipTo[3] = &lcTmpfile..COutAddr2
laShipTo[4] = &lcTmpfile..COutAddr3
laShipTo[5] = &lcTmpfile..COutAddr4
laShipTo[6] = &lcTmpfile..COutAddr5
= lfAdrShift('laShipTo')
*-- Fill Ship to array with its data. [begin]

GO RECNO('POFLN') IN POFLN    && Refresh relation with PO Fabric Line file.
RETURN ''
*-- end of lfHeadVar.



*!*************************************************************
*! Name      : lfGetNotes
*! Developer : Nader Anis Mitry  (NAD)
*! Date      : 10/15/2000
*! Purpose   : Fill variables for Notepad printing with its 
*!           : corressponding data. 
*!*************************************************************
*! Called from : Detail band.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfGetNotes()
*!*************************************************************
FUNCTION lfGetNotes
PARAMETERS  lcDummy1,lcDummy2

STORE '' TO lcTitle,lcNotes
lcTitle = IIF(CMATTYPE = 'P','Comments :','')          
lcNotes =IIF(CMATTYPE='P',ALLTRIM(NOTEPAD_A.MNOTES),'')
RETURN ''
*-- end of lfGetNotes











