*:***************************************************************************
*: Program file  : POSTYPMU.PRG
*: Program desc. : CUSTOMIZED PURCHASE ORDER FOR MULLBREY.
*: Date          : 06/29/2000
*: System        : Aria Advantage Series.
*: Module        : STYLE PURCHASE ORDER (PO)
*: Developer     : BASSEM RAFAAT (BWA)
*: Tracking Job Number: C101845
*: 
*:***************************************************************************
*: Calls : 						
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO POSTYPMU
*:***************************************************************************
*: Modifications :
*: B603999,1 BWA 11/12/2000 Fix the bug of the printing notepad and the showing
*: B603999,1                of the header and the show of the field of the sub
*: B603999,1                total for the quantity and the amount after every 
*: B603999,1                style color,and comment the old code.
*: B604053,1 BWA 12/04/2000 Fix the bug of the printing the header after the 10th page[FRX]
*: B604053,1                Fix the bug of not printing the notepad of the last style.
*: B604257,1 KHM 02/22/2001 Fix the bug of not printing the notepad of the last style.
*:***************************************************************************

*B603999,1 BWA 11/12/2000[START]
*--Section break the relation and skip to.
SELECT PosHdr
SET SKIP TO

*-- Creating the temporary file and its index.
SELECT PosLn
=AFIELDS(laFStru)
DIMENSION laFStru[ALEN(laFStru,1)+1,4]
laFStru[ALEN(laFStru,1),1] = 'cNotePad'
laFStru[ALEN(laFStru,1),2] = 'M'
laFStru[ALEN(laFStru,1),3] = 10
laFStru[ALEN(laFStru,1),4] = 0

DIMENSION laFStru[ALEN(laFStru,1)+1,4]
laFStru[ALEN(laFStru,1),1] = 'Sort'
laFStru[ALEN(laFStru,1),2] = 'C'
laFStru[ALEN(laFStru,1),3] = 2
laFStru[ALEN(laFStru,1),4] = 0

DIMENSION laFStru[ALEN(laFStru,1)+1,4]
laFStru[ALEN(laFStru,1),1] = 'llPrint'
laFStru[ALEN(laFStru,1),2] = 'L'
laFStru[ALEN(laFStru,1),3] = 1
laFStru[ALEN(laFStru,1),4] = 0

=gfCrtTmp(lcTmpPolns,@laFStru,'cstytype+PO+cWareCode+account+store+LEFT(STYLE,lnMajSize)',lcTmpPolns)

*--Section initialze the variables.
STORE " " TO lcStyle, lcLastSty , lcPo , lcAcount , lcWare , lcScale

*--Section of collecting the data.
SELECT PosHdr
SCAN FOR &lcRpExp
  lcStyle = LEFT(Posln.style,lnMajSize)
  IF SEEK(cStyType+Po,'PosLn')
    SELECT PosLn
    SCAN REST WHILE cstytype+PO+POSLN.cWareCode+ Posln.account+ Posln.store+LEFT(Posln.style,lnMajSize) = ;
                    PosHdr.cStyType+PosHdr.PO FOR TranCd = '1' AND TotQty <>0

      *-- Variable to get the style notepad inside the loop or outside it.
      llStyflg = .T.

      SCATTER MEMVAR MEMO
      INSERT INTO (lcTmpPolns) FROM MEMVAR

      *--Case for the style notepad.
      IF LEFT(Posln.style,lnMajSize) <> lcStyle AND ;
         llRpPrtSn AND SEEK('F'+lcStyle,'Notepad') 
         SELECT (lcTmpPolns)
         INSERT INTO (lcTmpPolns) (cstytype,PO,cWareCode,Account,Store,Style,Scale);
                   values (lcRpForm,lcPo,lcWare,lcAcount,lcStore,lcLastSty,lcScale)
         REPLACE TotQty   WITH 0             ;
                 Sort     WITH 'S'           ;
                 STYLE    WITH lcLastSty     ;
                 cNotePad WITH NotePad.mNotes

         lcStyle = LEFT(Posln.style,lnMajSize)
         llStyflg = .F.
      ENDIF
      lcPo      = PosHdr.Po
      lcWare    = POSLN.cWareCode
      lcScale   = Posln.Scale
      lcAcount  = PosHdr.Account
      lcStore   = Posln.Store
      lcLastSty = Posln.Style
    ENDSCAN

    *-- Case the po finished and get the style notepad for the po.
    *B604053,1 BWA 12/04/2000 Fix the bug of not printing the notepad of the last style.[START]
    *IF LEFT(Posln.style,lnMajSize) <> lcStyle AND ;
    *   llRpPrtSn AND SEEK('F'+lcStyle,'Notepad') AND llStyflg
    
    *B604257,1 KHM 02/22/2001 (Begin) Fix the bug of not printing the 
    *B604257,1                notepad of the last style.
    *IF LEFT(Posln.style,lnMajSize) <> lcStyle AND ;
       llRpPrtSn AND SEEK('F'+lcStyle,'Notepad')

    IF (LEFT(Posln.style,lnMajSize) <> lcStyle OR Posln.PO <> lcPO) AND ;
       llRpPrtSn AND SEEK('F'+lcStyle,'Notepad')

    *B604257,1 KHM 02/22/2001 (End)
    *B604053,1 [END]

       SELECT (lcTmpPolns)
       INSERT INTO (lcTmpPolns) (cstytype,PO,cWareCode,account,Store,style,Scale) values (lcRpForm,lcPo,lcWare,lcAcount,lcStore,lcLastSty,lcScale)
       REPLACE TotQty   WITH 0             ;
               Sort     WITH 'S'           ;
               STYLE    WITH lcLastSty     ;
               cNotePad WITH NotePad.mNotes

       lcStyle = LEFT(Posln.style,lnMajSize)
    ENDIF
   
    *-- Case the po finished and get the po notepad.
    IF llrpPrtPn AND SEEK('P' + lcPo , 'NOTEPAD')
      SELECT (lcTmpPolns)
      INSERT INTO (lcTmpPolns) (cstytype,PO,cWareCode,account,Store,style,Scale) values (lcRpForm,lcPo,lcWare,lcAcount,lcStore,lcLastSty,lcScale)
      lcText = IIF(lcRpForm ='P','Purchase Order Notepad',IIF(lcRpForm='C' ,'Contract Notepad',IIF(lcRpForm='P' ,'Return Purchase Order Notepad',IIF(lcRpForm='N' ,'Inter-Location P/O Notepad','Dye Order Notepad'))))
      
      REPLACE TotQty    WITH 0              ;
              Sort      WITH 'W'            ;
              STYLE     WITH lcLastSty      ;
              cNotePad  WITH NotePad.mNotes ;
              Reference WITH lcText         ;
              llPrint   WITH .T.
    ENDIF
  ENDIF
ENDSCAN

SELECT PosLn
SET RELATION TO
SELECT PosHdr
SET RELATION TO

SELECT (lcTmpPolns)
SET RELATION TO cStyType+PO INTO PosHdr ADDITIVE
SET RELATION TO STYLE INTO STYLE ADDITIVE
SET RELATION TO 'S' + Scale INTO SCALE ADDITIVE

SELECT objlink
SET RELATION off INTO Objects

SELECT &TMPObjlink
SET RELATION TO

SELECT PosHdr
SET RELATION TO Poshdr.vendor INTO Apvendor ADDITIVE

SELECT (lcTmpPolns)
GOTO TOP
lcRpExp = '.T.'

*!**************************************************************************
*! Name      : lfBringSN
*! Developer : Wael Mohammed Anwar (WMA)
*! Date      : 06/29/2000
*! Purpose   : Function to Get Style Note Pad
*!**************************************************************************
*! Called from : Report
*!**************************************************************************
*! Calls       : lfBringSN()
*!**************************************************************************
*FUNCTION lfBringSN
*PARAMETER lcDummy
  
*IF SEEK("F"+LEFT(Posln.style,lnMajSize),'NOTEPAD')
*  lcSTitle = 'Style Notepad'
*  lnCurAlias = SELECT(0)
*  SELECT NOTEPAD
*  lcSNotes = ALLTRIM(NotePad.mNotes)
*  SELECT (lnCurAlias)
*ELSE
*  lcSTitle = ''
*  lcSNotes = ''
*ENDIF

*RETURN ""
*-- end of lfBringSN
    
*!**************************************************************************
*! Name      : lfBringPN
*! Developer : Wael Mohammed Anwar (WMA)
*! Date      : 06/29/2000
*! Purpose   : Function to Get Po Note Pad
*!**************************************************************************
*! Called from : Report
*!**************************************************************************
*! Calls       : lfBringPN()
*!**************************************************************************
*FUNCTION lfBringPN
*PARAMETER lcDummy

*IF SEEK("P"+POSHDR.PO,'NOTEPAD')
*  IF POSHDR.cStyType='P'
*    lcPOTitle = 'Purchase Order Notepad'  
*  ELSE
*    IF POSHDR.cStyType='C'
*      lcPOTitle = 'Contract Notepad'
*    ELSE
*      IF POSHDR.cStyType='R'
*        lcPOTitle = 'Return Purchase Order Notepad'
*      ELSE
*        IF POSHDR.cStyType='N' 
*          lcPOTitle = 'Inter-Location P/O Notepad'
*        ELSE
*          lcPOTitle = 'Dye Order Notepad'
*        ENDIF
*      ENDIF
*    ENDIF
*  ENDIF
*  lnCurAlias = SELECT(0)
*  SELECT NOTEPAD
*  lcPONotes = ALLTRIM(NotePad.mNotes)
*  SELECT (lnCurAlias)
*ELSE
*  lcPOTitle = ''
*  lcPONotes = ''
*ENDIF

*RETURN ""
*-- end of lfBringPN
*!**************************************************************************
*! Name      : lfStyleCont
*! Developer : Wael Mohammed Anwar (WMA)
*! Date      : 06/29/2000
*! Purpose   : Function to Get Style Contents
*!**************************************************************************
*! Called from : Report
*!**************************************************************************
*! Calls       : lfStyleCont()
*!**************************************************************************
*FUNCTION lfStyleCont
*PARAMETER lcDummy

*lnCurAlias = SELECT(0)
*lnCurOrder = ORDER('STYLE')
*SELECT Style
*SET ORDER TO STYLE

*IF SEEK(LEFT(Posln.style,lnMajSize),'Style')
*  lcStyleCont = Style.Content1
*ELSE
*  lcStyleCont = ''
*ENDIF

*SET ORDER TO lnCurOrder
*SELECT (lnCurAlias)

*RETURN ""
*-- end of lfStyleCont

*B603999,1 BWA 11/12/2000[END]