*:****************************************************************
*: Program file  : ALIMPLW
*: Program desc. : Import M&S File.
*: System        : Aria Apparel System - Version 2.7.
*: Module        : Sales Order Allocation [AL]
*: Developer     : ABDOU ELGENDI -  [ABD]
*: Date          : 08/28/2003
*: Tracking Job Number : C200572,1
*:****************************************************************
*: Calls         : G.FUNCTIONS: gfSetup 
*:               : FUNCTIONS  : lfProceed , lfGetFile , lfvMs3File
*:               :            : lfUpPikQty, lfOpenFls , lfClosFls
*:               :            : lfErrLog  , lfvPrint  , lfUpdtPick
*:               :            : lfUpdtPck2, lfgetStyle,lfGenPiktk
*:               :            : lfEscap.
*:               -----------------------------------------------
*:               : PROCEDURE  : lpSaveOrd.
*:****************************************************************
*: Passed Parameters  : None.
*:****************************************************************
*:C200572,1
*:****************************************************************
*:Modifications  :
*:****************************************************************
*:
*-- lcWindTitle :- Window Titel.
*-- lcTempMs3   :- Temp File hold the MS3 Data File.
*-- laOpenFile  :- Array Hold the Opened Files.
*-- lnLineNo    :- Variable to hold the printed line number at error file.
*-- lcOrdline   :- Variable to hold the order line file.
*-- lcTmpError  :- Variable to hold the all the errors occures while import the data.

DIMENSION laOpenFile[07,3]
STORE ''  TO lcTempMs3 , M.style , M.Order
STORE .F. TO llOpenRep , llOpenFils
STORE 0   TO lnFilHandl , lnLineNo , M.LineNo
lcTempMs3 = gfTempName()
lcOrdline = gfTempName()
lcTmpError= gfTempName()


IF !gfSetup()
  RETURN
ENDIF

*-- Function to open needed files.
= lfOpenFls ()

PUSH KEY
lcWindTitle = 'Import MS3 File'
*--Call screen.
ON KEY LABEL ESC DO lfEscap
DO (gcScrDir+gcWinAppl+"\ALIMPLW.SPX")
ON KEY
POP KEY

= lfClosFls()
glQuitting = .T.  && This is modal screen, this rise quitting flag.

RETURN
*-- END of Code.
*:*************************************************************
*: Name      : lfProceed
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 08/28/2003
*: Purpose   : Funtion to collect the data From text file.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfProceed ()
*:*************************************************************
*:
FUNCTION lfProceed
PRIVATE llLoop , lcLine , lcConvLine , lcSku_No , lcLastpldN

*-- Give User message in case the ms3 file variable is empty.
IF EMPTY(lcTxtFile)
  =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,;
              'You must select an MS3 file.')  
  RETURN
ENDIF
SET MEMOWIDTH TO 600

*-- Create a Table
CREATE TABLE ('&gcWorkDir.'+lcTempMs3) (MEMO M(10))
APPEN BLANK
APPEND MEMO MEMO FROM (lcTxtFile) OVERWRITE
llLoop = .F.
FOR lnI = 1 TO MEMLINES(MEMO)
  lcLine     = MLINE(MEMO,lnI)
  lcConvLine = PADL(lcLine,3)

  *-- Check for the first 5 recored not needed while convert ms3 file.
  IF INLIST(lcConvLine,'Y50','TYP','SDT','CDT','FIL','PIC','PFT','DIN')
    LOOP
  ENDIF
  
  IF lcConvLine = 'ORD' 
    IF SUBSTR(lcLine,5,4) =  '0000'
      *-- if ord set to 0000 then loop until the next ord.
      llLoop = .T.
      Loop
    ELSE
      llLoop = .F.
    ENDIF
  ENDIF
  
  *-- Get The Sku# 
  IF !llLoop .AND. lcConvLine = 'PLD'
    lcSku_No   = SUBSTR(lcLine,10,8)
    lcLastpldN = SUBSTR(lcLine,25,3)
    WAIT WINDOW 'Please wait ... data collecting for Sku# : '+lcSku_No NOWAI
  ENDIF
  
  *-- Get the Qty.
  IF !llLoop .AND. lcConvLine = 'PTR'
    lnQty = Eval(SUBSTR(lcLine,23,10))
    = lfUpPikQty (lnQty,lcSku_No,lcLastpldN)
  ENDIF
  
ENDFOR

WAIT CLEAR

IF llOpenRep
  llOpenRep = .F.
  *-- Must close the error file before append this erro into memo field.
  = FCLOSE(lnFilHandl)
  CREATE TABLE ('&gcWorkDir.'+lcTmpError) (MEMO M(10))
  APPEN BLANK
  APPEND MEMO MEMO FROM ('&gcWorkdir.ErrLog.txt') OVERWRITE
  PUSH KEY
  *--Call Error screen.
  DO (gcScrDir+gcWinAppl+"\ALDISPER.SPX")
  ON KEY
  POP KEY
  
ELSE
  *-- Function to generate and save the order and pick ticket.
  DO lpSaveOrd
  
  *- Message Text   :- Import SBT files complete successfully.
  *- Message No.    :- 000000.
  *- Buttom Message :- Ok
  *- Buttom Number  :- 00000.
  lcMessage = 'Data was imported successfully from the MS3 file to Aria 2.7 system.'
  = gfModalGen('INM00000B00000','F','ALERT',' ',lcMessage)
ENDIF
RETURN


*-- End of lfProceed
*:*************************************************************
*: Name      : lfGetFile
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 08/28/2003
*: Purpose   : Funtion to get the MS3 File.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfGetFile ()
*:*************************************************************
*:
FUNCTION lfGetFile

lcTxtFile =GetFile('MS3','Select the MS3 file.')

=lfvMs3File()
*-- End of lfGetFile
*:*************************************************************
*: Name      : lfvMs3File
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 08/28/2003
*: Purpose   : Funtion to Vaild the MS3 File.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfvMs3File()
*:*************************************************************
*:
FUNCTION lfvMs3File

IF !EMPTY(lcTxtFile)
  lcTxtFile = UPPER(lcTxtFile)
  IF ATC('.MS3',lcTxtFile) = 0 
    IF ATC('.',lcTxtFile) <> 0 
      =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,'You have to select an MS3 file.')
      lcTxtFile = ""
      SHOW GET lcTxtFile
      RETURN
    ELSE
      lcTxtFile = ALLTRIM(lcTxtFile) + '.MS3'
    ENDIF
  ENDIF
  IF !FILE(lcTxtFile)
    =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,'This file does not exist.') 
    lcTxtFile = ""
  ENDIF
  SHOW GET lcTxtFile
ENDIF

*-- End Of lfvMs3File.
*:*************************************************************
*: Name      : lfUpPikQty
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 08/28/2003
*: Purpose   : Funtion to update the piktkt Qty.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : lnQty   :- Variable hold the Piked Qty.
*:                    : lcSku_No:- Variable hold the Sku No.
*:                    : lcLastpldN:- Hold 3 Charcter from the MS3 file.
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfUpPikQty()
*:*************************************************************
*:
FUNCTION lfUpPikQty
Parameters lnSndPkQty, lcSndSkuNo, lclast_Pld
PRIVATE lnPrvAlias , lcStyle , lcSize
STORE '' TO lcStyle , lcSize
lnPrvAlias = SELECT (0)
IF TYPE('lnSndPkQty')= 'U' .OR. TYPE('lcSndSkuNo') = 'U' .OR. ;
   TYPE('lclast_Pld')= 'U' 
  RETURN
ENDIF

lnLen = LEN(lclast_Pld)
IF lnLen < 3
  FOR I = lnLen TO 3
    lnChart = lnChart + '0'
  ENDFOR
  lclast_Pld = lnChart + lclast_Pld 
ENDIF


*-- First Check for the Sku #.
IF !EMPTY(lcSndSkuNo) 
  *-- Function to Check if the SKU # is exist or not if found return
  *-- the Style and the size.
  IF lfgetStyle(lcSndSkuNo)
    *-- Check if this Style is exist at the order line file.
    IF SEEK(lcStyle,'ORDLINE')
      SELECT ORDLINE
      LOCATE REST WHILE style+DTOS(complete)+cordtype+order+store+STR(lineno,6) = ;
        lcStyle For EMPTY(Piktkt) .AND. Ordhdr.Status = 'O'
      IF FOUND()
        *-- Check if this record was picked before that.
        = lfUpdtPick ()
      ELSE
        *-- Send Erro to Erro File in case no ordr open for cuuernt style.
        lcError = 'There is no order open for style# / Sku# :'+ lcStyle +' / '+ lcSndSkuNo +' .'
        = lfErrLog (lcError)
      ENDIF
    ELSE
      *-- Send Erro to Erro File in case not found the style into order line file.
      lcError = 'Style# / Sku# :'+ lcStyle +' / '+ lcSndSkuNo +' not found into order line file.'
      = lfErrLog (lcError)
    ENDIF
  ELSE
    *-- Send Erro to Erro File in case not found the Sku.
    lcError = 'Sku # :' + lcSndSkuNo +' not found into the database.'
    = lfErrLog (lcError)
  ENDIF
ENDIF

SELECT (lnPrvAlias)
*-- End OF lfUpPikQty
*:*************************************************************
*: Name      : lfOpenFls
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 08/28/2003
*: Purpose   : Function to open needed files.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfOpenFls ()
*:*************************************************************
*:
FUNCTION lfOpenFls
PRIVATE lnPrvAlias 

lnPrvAlias = SELECT(0)

*--- Array laOpenFile column 1 :- Hold the name of the file.
*--- Array laOpenFile column 2 :- Hold the name of the index file.
*--- Array laOpenFile column 3 :- Hold true in case open the file .
*-- Aria 27 Tables.
llOpenFils = .T.
laOpenFile[1,1] = 'spck_lin'
laOpenFile[1,2] = 'Spck_lin'

laOpenFile[2,1] = 'STYLE'
laOpenFile[2,2] = 'STYLE'

laOpenFile[3,1] = 'STYDYE'
laOpenFile[3,2] = 'STYDYE'

laOpenFile[4,1] = 'ordline'
laOpenFile[4,2] = 'Ordlines'

laOpenFile[5,1] = 'ordhdr'
laOpenFile[5,2] = 'ordhdr'

laOpenFile[6,1] = 'spck_hdr'
laOpenFile[6,2] = 'spck_hdr'

laOpenFile[7,1] = 'PIKTKT'
laOpenFile[7,2] = 'PIKTKT'

FOR I = 1 To ALEN(laOpenFile,1)
  IF !USED(laOpenFile[I,1])
    laOpenFile[I,3] =gfOpenFile(gcDataDir+laOpenFile[I,1],laOpenFile[I,2], "SH")
  ENDIF
ENDFOR


SELECT ORDLINE
SET ORDER TO Ordlines DESC
SET RELATION TO cordtype+order INTO ORDHDR

lnFileStru = AFIELDS(laFileStru)

DIMENSION laFileStru[lnFileStru+1,4]
laFileStru[lnFileStru+1,1] = 'cLastpldNo'
laFileStru[lnFileStru+1,2] = 'C'
laFileStru[lnFileStru+1,3] = 6
laFileStru[lnFileStru+1,4] = 0

DECLARE laIndex[2,2]
laIndex[1,1] = 'Style+Order+STR(Lineno,6)'
laIndex[1,2] = 'Ordline'
laIndex[2,1] = 'Order + Store + cWareCode '
laIndex[2,2] = 'Ordlins'

=gfCrtTmp(lcOrdline,@laFileStru,@laIndex)
SELECT(lnPrvAlias)

*-- End of lfOpenFls.
*:*************************************************************
*: Name      : lfClosFls
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 08/28/2003
*: Purpose   : Function to close opened files.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Called from : Prog.
*:*************************************************************
*: Example   : = lfClosFls ()
*:*************************************************************
*:
FUNCTION lfClosFls

IF llOpenFils
  FOR I = 1 To ALEN(laOpenFile,1)
    IF USED(laOpenFile[I,1]) .AND.  laOpenFile[I,3]
      = gfCloseFile(laOpenFile[I,1])
    ENDIF
  ENDFOR
ENDIF

IF USED(lcTempMs3)
  SELECT (lcTempMs3)
  USE
  ERASE (gcWorkDir+'lcTempMs3'+".DBF")
  ERASE (gcWorkDir+'lcTempMs3'+".FPT")
ENDIF


IF USED(lcOrdline)
  SELECT (lcOrdline)
  USE
  ERASE (gcWorkDir+'lcOrdline'+".DBF")
  ERASE (gcWorkDir+'lcOrdline'+".FPT")
ENDIF

IF USED(lcTmpError)
  SELECT (lcTmpError)
  USE
  ERASE (gcWorkDir+'lcTmpError'+".DBF")
  ERASE (gcWorkDir+'lcTmpError'+".FPT")
ENDIF

*-- End of lfClosFls
*:*************************************************************
*: Name      : lfErrLog
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 08/28/2003
*: Purpose   : Function to write the error into log file.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Called from : Prog.
*:*************************************************************
*: Example   : = lfErrLog ()
*:*************************************************************
*:
FUNCTION lfErrLog
PARAMETER lcErr_Mesage

PRIVATE lnAlias
lnAlias = SELECT(0)
lnLineNo = lnLineNo + 1
IF !llOpenRep
  llOpenRep = .T.
  lnFilHandl = FCREAT('&gcWorkdir.ErrLog.txt')
  =FPUTS(lnFilHandl,REPLICATE('*',65))
  =FPUTS(lnFilHandl,"*     Errors occurred while importing the MS3 file to Aria27    *")
  =FPUTS(lnFilHandl,REPLICATE('*',65))
  =FPUTS(lnFilHandl,' ')
  =FPUTS(lnFilHandl,' ')
ENDIF

=FPUTS(lnFilHandl, ALLTRIM(STR(lnLineNo)) + ' - '+ lcErr_Mesage)

SELECT(lnAlias)
RETURN

*-- END OF lfErrLog
*:*************************************************************
*: Name      : lfvPrint
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 08/28/2003
*: Purpose   : Function to print error file.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Called from : Prog.
*:*************************************************************
*: Example   : = lfvPrint()
*:*************************************************************
*:
FUNCTION lfvPrint

IF PSETUP(.T.) 
  SET DEVICE TO PRINT 
  SET CONSOLE OFF
  TYPE  gcWorkdir+'ErrLog.txt' TO PRINTER
  SET CONSOLE ON
  lcOGPlatForm='DOS'
  DO GFENDPRN
  SET DEVICE TO SCREEN
ENDIF
RETURN

*-- End OF lfvPrint
*:*************************************************************
*: Name      : lfUpdtPick
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 08/28/2003
*: Purpose   : Function to print error file.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Called from : Prog.
*:*************************************************************
*: Example   : = lfUpdtPick()
*:*************************************************************
*:
FUNCTION lfUpdtPick
PRIVATE lnAlias
lnAlias = SELECT(0)

SELECT ORDLINE
*-- Check if this line was picked before
IF SEEK(Ordline.style+ordline.Order+STR(Ordline.LineNo,6),lcOrdline)
  IF &lcOrdline..PIK&lcSize < &lcOrdline..Qty&lcSize
    SELECT (lcOrdline)
    *ash on 01/25/04 if the style appears more than once in the order line
    *REPLACE PIK&lcSize WITH MAX(MIN(lnSndPkQty,Qty&lcSize - PIK&lcSize),0),;
            TotPik     WITH Pik1+Pik2+Pik3+Pik4+Pik5+Pik6+Pik7+Pik8       ,;
            cLastpldNo WITH lclast_Pld
    REPLACE PIK&lcSize WITH PIK&lcSize + MAX(MIN(lnSndPkQty,Qty&lcSize - PIK&lcSize),0),;
            TotPik     WITH Pik1+Pik2+Pik3+Pik4+Pik5+Pik6+Pik7+Pik8                    ,;
            cLastpldNo WITH lclast_Pld            
          lnSndPkQty = lnSndPkQty - PIK&lcSize
  ELSE
   *-- Call function to update the pick Qty.
    = lfUpdtPck2 ()
  ENDIF 
ELSE
  *-- Check if the Open Qty is Cover the picked Qty.
  IF Qty&lcSize >= lnSndPkQty && if the Open Qty Cover.
    SELECT ORDLINE
    SCATTER MEMVAR MEMO
    SELECT (lcOrdline)
    APPEND BLANK
    GATHER MEMVAR MEMO
  
    REPLACE PIK&lcSize WITH MAX(MIN(lnSndPkQty,Qty&lcSize),0)      ,;
            TotPik     WITH Pik1+Pik2+Pik3+Pik4+Pik5+Pik6+Pik7+Pik8,;
            cLastpldNo WITH lclast_Pld

    lnSndPkQty = lnSndPkQty - PIK&lcSize
    
  ELSE && IF the Qty not Cover the Picked Qty.
    
    *-- Call function to update the pick Qty.
    = lfUpdtPck2 ()
  ENDIF 
    
ENDIF

*-- Check if I pick all the Qty or not
IF lnSndPkQty > 0
  SELECT (lcOrdline)
  *ash on 11/11/2003 Update the right style not the first one.
  *=SEEK(M.style+M.Order+STR(M.LineNo,6),lcOrdline)
  =SEEK(OrdLine.style+OrdLine.Order+STR(OrdLine.LineNo,6),lcOrdline)
  REPLACE PIK&lcSize   WITH PIK&lcSize + lnSndPkQty                ,;
            TotPik     WITH Pik1+Pik2+Pik3+Pik4+Pik5+Pik6+Pik7+Pik8
  lnSndPkQty = 0
ENDIF

SELECT(lnAlias)

*-- End OF lfUpdtPick
*:*************************************************************
*: Name      : lfUpdtPck2
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 08/28/2003
*: Purpose   : Function to Update the Pick Qty .
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Called from : Prog.
*:*************************************************************
*: Example   : = lfUpdtPck2()
*:*************************************************************
*:
FUNCTION lfUpdtPck2
PRIVATE lnAlias
lnAlias = SELECT(0)
SELECT ORDLINE
*ash on 11/11/2003 save the current record.
lnOldRec=RECNO()

SCAN REST WHILE style+DTOS(complete)+cordtype+order+store+STR(lineno,6) = ;
  lcStyle For EMPTY(Piktkt) .AND. Ordhdr.Status = 'O'

  IF SEEK(Ordline.style+ordline.Order+STR(Ordline.LineNo,6),lcOrdline)
    SELECT (lcOrdline)
    *- Check if this order still have avilable pick Qty.
    IF PIK&lcSize < Qty&lcSize
      *ash on 01/25/04 if the style appears more than once in the order line
      *REPLACE PIK&lcSize WITH MAX(MIN(lnSndPkQty,Qty&lcSize - PIK&lcSize),0),;
              TotPik     WITH Pik1+Pik2+Pik3+Pik4+Pik5+Pik6+Pik7+Pik8         ,;
              cLastpldNo WITH lclast_Pld

      REPLACE PIK&lcSize WITH PIK&lcSize + MAX(MIN(lnSndPkQty,Qty&lcSize - PIK&lcSize),0),;
              TotPik     WITH Pik1+Pik2+Pik3+Pik4+Pik5+Pik6+Pik7+Pik8                         ,;
              cLastpldNo WITH lclast_Pld              
      lnSndPkQty = lnSndPkQty - PIK&lcSize
      SCATTER MEMVAR MEMO      
    ELSE
      LOOP
    ENDIF
  ELSE
    SCATTER MEMVAR MEMO
    SELECT (lcOrdline)
    APPEND BLANK
    GATHER MEMVAR MEMO

    REPLACE PIK&lcSize WITH MAX(MIN(lnSndPkQty,Qty&lcSize),0)       ,;
            TotPik     WITH Pik1+Pik2+Pik3+Pik4+Pik5+Pik6+Pik7+Pik8 ,;
            cLastpldNo WITH lclast_Pld

    lnSndPkQty = lnSndPkQty - PIK&lcSize
    SELECT ORDLINE
  ENDIF
ENDSCAN
*ash1
GOTO lnOldRec
SELECT(lnAlias)

*-- End of lfUpdtPck2
*:*************************************************************
*: Name      : lfgetStyle
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 08/28/2003
*: Purpose   : Function to get the style from the spck_lin file.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Called from : Prog.
*:*************************************************************
*: Example   : = lfgetStyle()
*:*************************************************************
*:
FUNCTION lfgetStyle
Parameters lcSndPckId
PRIVATE lnAlias , llgetStyle
llgetStyle = .F.

IF TYPE('lcSndPckId') = 'U' 
  RETURN
ENDIF
lnAlias = SELECT(0)

SELECT spck_lin
IF SEEK('S')
  SCAN REST WHILE type+account+pack_id = 'S' FOR lcSndPckId $ ALLTRIM(PACK_id)
    lcStyle = spck_lin.Style
    FOR I = 1 To 8
      *-- Hold the size scale.
      lcSize = STR(I,1)
      IF spck_lin.Qty&lcSize = 1
        EXIT
      ENDIF
    ENDFOR
    llgetStyle = .T.
    EXIT
  ENDSCAN
ENDIF

SELECT(lnAlias)
RETURN llgetStyle

*-- End Of lfgetStyle
*:*************************************************************
*: Name      : lpSaveOrd
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 08/28/2003
*: Purpose   : Function to get the style from the spck_lin file.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Called from : Prog.
*:*************************************************************
*: Example   : DO lpSaveOrd
*:*************************************************************
*:
FUNCTION lpSaveOrd
PRIVATE lnAlias , laOverPik ,  lUpdOrdQty , lnStart , lcStart,;
        lnDiffernt , lnTotQty
DIMENSION laOverPik [8]
STORE .F. To  lUpdOrdQty
STORE ''  TO lcStart
STORE 0 TO lnStart , laOverPik , lnDiffernt, lnTotQty
lnAlias = SELECT (0)

*-- Function to Generate piktkt for the order lines.
= lfGenPiktk ()

SELECT ORDLINE
SET RELATION TO 
SET ORDER TO TAG Ordlines  Ascending

SELECT (lcOrdLine)
SCAN
  SCATTER MEMVAR MEMO
  *-- Index key into Order line file.
  *--style+DTOS(complete)+cordtype+order+store+STR(lineno,6)
  IF SEEK(M.Style+ DTOS(M.Complete) + M.cOrdtype+ M.order+M.store+STR(M.lineno,6),'ORDLINE')
    SELECT ORDLINE
    =RLOCK()
    *-- Update the Picked fields.
    REPLACE Pik1    WITH M.Pik1   ,;
            Pik2    WITH M.Pik2   ,;
            Pik3    WITH M.Pik3   ,;
            Pik4    WITH M.Pik4   ,;
            Pik5    WITH M.Pik5   ,;
            Pik6    WITH M.Pik6   ,;
            Pik7    WITH M.Pik7   ,;
            Pik8    WITH M.Pik8   ,;
            TotPik  WITH M.TotPik ,;
            Piktkt  WITH M.Piktkt ,;
            PikDate WITH M.PikDate,;
            Picked  WITH .T.
    
    UNLOCK
    
    *-- Get the Over allocated Qty.
    laOverPik[1] = MAX(Pik1 - Qty1,0)
    laOverPik[2] = MAX(Pik2 - Qty2,0)
    laOverPik[3] = MAX(Pik3 - Qty3,0)
    laOverPik[4] = MAX(Pik4 - Qty4,0)
    laOverPik[5] = MAX(Pik5 - Qty5,0)
    laOverPik[6] = MAX(Pik6 - Qty6,0)
    laOverPik[7] = MAX(Pik7 - Qty7,0)
    laOverPik[8] = MAX(Pik8 - Qty8,0)
    
    *-- Check if we have any size over allocated.
    lUpdOrdQty = .F.
    FOR lnStart = 1 TO 8
      lcStart = STR(lnStart,1)
      *XXXX
      *IF laOverPik[1] > Qty&lcStart
      IF Pik&lcStart > Qty&lcStart
      *XXXX
        lUpdOrdQty = .T.
        EXIT
      ENDIF
    
    ENDFOR
    *-- Update the Open And Book Qty in case over allocate.
    IF lUpdOrdQty
      lnOvrAlQty = TotPik - TotQty
      lnTotQty   = QTy1 + QTy2 + QTy3 + QTy4 + QTy5 + QTy6 + QTy7 + QTy8 
      =RLOCK()
      *-- Update the Open Qty fields.
      REPLACE Qty1    WITH MAX(M.Pik1,Qty1)   ,;
              Qty2    WITH MAX(M.Pik2,Qty2)   ,;
              Qty3    WITH MAX(M.Pik3,Qty3)   ,;
              Qty4    WITH MAX(M.Pik4,Qty4)   ,;
              Qty5    WITH MAX(M.Pik5,Qty5)   ,;
              Qty6    WITH MAX(M.Pik6,Qty6)   ,;
              Qty7    WITH MAX(M.Pik7,Qty7)   ,;
              Qty8    WITH MAX(M.Pik8,Qty8)   ,;
              TotQty  WITH MAX(M.TotPik,Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8),;
              Prepak  WITH '' ,;
              PpQty   WITH 0
      UNLOCK

      =RLOCK()
      *-- Update the Book Qty fields.
      REPLACE Book1    WITH MAX(M.Pik1,Book1) ,;
              Book2    WITH MAX(M.Pik2,Book2) ,;
              Book3    WITH MAX(M.Pik3,Book3) ,;
              Book4    WITH MAX(M.Pik4,Book4) ,;
              Book5    WITH MAX(M.Pik5,Book5) ,;
              Book6    WITH MAX(M.Pik6,Book6) ,;
              Book7    WITH MAX(M.Pik7,Book7) ,;
              Book8    WITH MAX(M.Pik8,Book8) ,;
              TotBook  WITH MAX(M.TotPik,Book1+Book2+Book3+Book4+Book5+Book6+Book7+Book8)
      UNLOCK
      
      *-- Update the order header file.
      lnDiffernt = MAX(TotQty - lnTotQty,0 )
      SELECT Ordhdr
      IF SEEK(M.cOrdtype+ M.Order)
        =RLOCK()
        REPLACE Book    WITH Book    + lnDiffernt            ,;
                Open    WITH Open    + lnDiffernt            ,;        
                BookAmt WITH BookAmt + (lnDiffernt * M.Price),;
                OpenAmt WITH OpenAmt + (lnDiffernt * M.Price)
      ENDIF
      UNLOCK
    ENDIF
  ENDIF
  
  *-- Update the Style File.
  IF SEEK(M.Style,'STYLE')
    SELECT STYLE 
    =RLOCK()
    REPLACE Alo1       WITH Alo1   + M.Pik1      ,;
            Alo2       WITH Alo2   + M.Pik2      ,;
            Alo3       WITH Alo3   + M.Pik3      ,;
            Alo4       WITH Alo4   + M.Pik4      ,;
            Alo5       WITH Alo5   + M.Pik5      ,;
            Alo6       WITH Alo6   + M.Pik6      ,;
            Alo7       WITH Alo7   + M.Pik7      ,;
            Alo8       WITH Alo8   + M.Pik8      ,;
            TotAlo     WITH TotAlo + M.TotPik    ,;
            cLastpldNo WITH M.cLastpldNo         ,;
            Ord1       WITH Ord1   + laOverPik[1],;
            Ord2       WITH Ord2   + laOverPik[2],;
            Ord3       WITH Ord3   + laOverPik[3],;
            Ord4       WITH Ord4   + laOverPik[4],;
            Ord5       WITH Ord5   + laOverPik[5],;
            Ord6       WITH Ord6   + laOverPik[6],;
            Ord7       WITH Ord7   + laOverPik[7],;
            Ord8       WITH Ord8   + laOverPik[8],;
            TotOrd     WITH Ord1 + Ord2 + Ord3 + Ord4 + Ord5 + Ord6 + Ord7 + Ord8 
            
    UNLOCK
  ENDIF

  *-- Update the Stydye file.
  IF SEEK(M.Style+M.cWareCode,'STYDYE')
    SELECT STYDYE
    =RLOCK()
    REPLACE Alo1       WITH Alo1   + M.Pik1      ,;
            Alo2       WITH Alo2   + M.Pik2      ,;
            Alo3       WITH Alo3   + M.Pik3      ,;
            Alo4       WITH Alo4   + M.Pik4      ,;
            Alo5       WITH Alo5   + M.Pik5      ,;
            Alo6       WITH Alo6   + M.Pik6      ,;
            Alo7       WITH Alo7   + M.Pik7      ,;
            Alo8       WITH Alo8   + M.Pik8      ,;
            TotAlo     WITH TotAlo + M.TotPik    ,;
            Ord1       WITH Ord1   + laOverPik[1],;
            Ord2       WITH Ord2   + laOverPik[2],;
            Ord3       WITH Ord3   + laOverPik[3],;
            Ord4       WITH Ord4   + laOverPik[4],;
            Ord5       WITH Ord5   + laOverPik[5],;
            Ord6       WITH Ord6   + laOverPik[6],;
            Ord7       WITH Ord7   + laOverPik[7],;
            Ord8       WITH Ord8   + laOverPik[8],;
            TotOrd     WITH Ord1 + Ord2 + Ord3 + Ord4 + Ord5 + Ord6 + Ord7 + Ord8 
            
    UNLOCK
  ENDIF

ENDSCAN


SELECT ORDLINE
SET ORDER TO Ordlines DESC
SET RELATION TO cordtype+order INTO ORDHDR

SELECT (lnAlias)
*-- End OF lpSaveOrd
*:*************************************************************
*: Name      : lfGenPiktk
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 08/28/2003
*: Purpose   : Function to get the style from the spck_lin file.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Called from : Prog.
*:*************************************************************
*: Example   :  = lfGenPiktk ()
*:*************************************************************
*:
FUNCTION lfGenPiktk
PRIVATE lnAlias , lcChgPkTKt , lcPiktkNo
STORE '' TO lcPiktkNo , lcChgPkTKt
lnAlias = SELECT (0)
SELECT (lcOrdline)
SET ORDER TO Ordlins
LOCATE

SCAN
  SCATTER MEMVAR MEMO
  IF lcChgPkTKt <> m.Order + m.Store + m.cWareCode 
    lcChgPkTKt = m.Order + m.Store + m.cWareCode 
    lcPikTktNo = lfGetPkTkt(m.Order, ORDHDR.cDivision, m.Store, m.cWareCode)
    SELECT PIKTKT
  
    APPEN BLANK
    REPLACE Piktkt    WITH lcPikTktNo ,;
            Account   WITH M.Account  ,;
            Store     WITH M.Store    ,;
            Order     WITH M.Order    ,;
            Date      WITH gdSysDate  ,;
            cWareCode WITH M.cWareCode,;
            Status    WITH 'O'        ,;
            cAdd_User WITH gcUser_ID  ,;
            dAdd_Date WITH DATE()     ,;
            cAdd_Time WITH gfGetTime(),;
            CADD_VER  WITH "A27"
  ENDIF
  
  SELECT (lcOrdline)
  REPLACE Piktkt  WITH lcPikTktNo ,;
          PikDate WITH gdSysDate

ENDSCAN

SELECT(lnAlias)
*-- End OF lfGenPiktk
*! Purpose   : Traping of Esacp
*:*************************************************************
*: Name      : lfEscap
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 08/28/2003
*: Purpose   : Function to Traping of Esacp
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Called from : Prog.
*:*************************************************************
*: Example   :  = lfEscap()
*:*************************************************************
*:
FUNCTION lfEscap

CLEAR READ

*-- End of lfEscap
*:*************************************************************