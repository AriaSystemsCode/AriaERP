*:***************************************************************************
*: Program file  : POSTYPEE.PRG
*: Program desc. : CUSTOMIZED PURCHASE ORDER FOR EAGLE'S EYE.
*: Date          : 10/22/2000
*: System        : Aria Advantage Series.
*: Module        : PURCHASE ORDER (PO)
*: Developer     : BASSEM RAFAAT (BWA)
*: Tracking Job Number: C101958
*: 
*:***************************************************************************
*: Calls : 						
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO POSTYPEE
*:***************************************************************************
*:Modifications  :
*:B803856,1 BWA 11/27/2000 1)Increase the size of the Fabric Group field in FRX.
*:B803856,1                2)Add thousands comma on the contract total field in FRX.
*:B803856,1                3)Change the layout of the FRX to match with the priprented form
*:B803904,1 BWA 12/13/2000 1)The consignee address does not print the last chr on all lines.
*:B804052,1 BWA 03/14/2001 Fix the bug of Wrong calculation for the contract price in the Header part.[FIX IN FRX]
*:***************************************************************************
*-- There are fields in the right of the FRX the aim of it in case the Po lines 
*-- less than 11, I add lines to be 11 and suppress the empty fields there for I
*-- put this fields to book the place not to suppress the fields.

*-llFlgNote   >> Variable used to check if the notepade is finished or not.
*-lnContPO    >> Hold the last lineno of the invoice.
STORE 1  TO lnCounter
STORE 0  TO lnPoIndex , lnContPO
llFlgNote = .T.
llhrdTxt  = .F.

*-- Set order to the SYDFIELD file for the custom fields.
SELECT SYDFIELD
SET ORDER TO Cfld_name

*--THE Temporary Index for the posln file.
lcTmpIndx = gfTempName()
SELECT POSLN
INDEX ON cstytype+po+STR(lineno,6)+trancd TAG Trancd OF (gcWorkDir + lcTmpIndx+ '.CDX')
SET ORDER TO TAG Trancd OF (gcWorkDir +lcTmpIndx  + '.CDX')

*-- Open the POSLN.DBF again to get the last lineno for the PO.
IF !USED(lcPosln_A)
  =gfOpenFile(gcDataDir+"POSLN","Trancd",'SH', @lcPosln_A, .T.)
ENDIF

*-- 
DIMENSION laArray[1,2]
laArray[1,1] = 'CNRFCODE'
laArray[1,2] = 'lcCode'

SELECT POSHDR
SET SKIP TO

SELECT POSHDR
SCAN FOR &lcRpExp AND POSHDR.cStyType = lcRpForm
  *--lnNoLinNot >> Variable used to check if the lines of the notepad finished or not.
  *--lnContEePo >> Variable used to count the line # of the po and if the note in the back more than 1 page increment the count.
  *--lnContNtpd >> Variable used to count the line # of the po.
  *--lnLineUpc  >> Variable used to count records in the Upc part and if < 30 add the lines to the end of the page.
  *--lnUpcBkNot >> Variable used to count the records for the back note in case the upc.
  *-lcPoee      >> Variable hold the Po No.
  STORE 0  TO lnNoLinNot , lnContEePo , lnContNtpd , lnLineUpc , lnUpcBkNot , LcPageOf , lnBoth , lnupc , lnCount30
  STORE 1  TO LcPageNo , lnUpcPagNo
  STORE SPACE(0) TO lcSelct

  lcPoee = POSLN.PO
  *-- The function which we get the last recno for the po.
  =lfRecNo()

  *-- Get the page counters.
  IF lnContPO # 0
    IF llRpPrtPn AND SEEK('P' + POSHDR.PO , 'NOTEPAD')
      *B803856,1 Change the layout of the FRX to match with the priprented form.[START]
      *lnBoth = lnContPO + CEILING(MEMLINES(NOTEPAD.MNOTES)/ 3.36)
      lnBoth = lnContPO + CEILING(MEMLINES(NOTEPAD.MNOTES)/ 3)
      *B803856,1 [END]
    ELSE
     lnBoth = lnContPO
    ENDIF
  ENDIF
  *-- 1 for the header
  LcPageOf = 1 + CEILING(lnBoth/11)

  *-- Variable to give the temp file the index of the selected po.
  lnPoIndex = lnPoIndex + 1

  *--Prepare the Temp file record to print the first report (The PO Header)
  WAIT WINDOW "Collecting data for PO#: " + PO NOWAIT
  SCATTER MEMVAR MEMO
  SELECT (lcTmpPoHd)

  *-- Case the first line for the header. Type = 'H'
  INSERT INTO (lcTmpPoHd) FROM MEMVAR
  REPLACE TYPE       WITH 'H'              ;
          PageOf     WITH LcPageOf         ;
          PageNo     WITH LcPageNo         ;
          STYLE      WITH POSLN.STYLE      ;
          Gros_price WITH POSLN.Gros_price ;
          PoIndex    WITH lnPoIndex        ;
          cQuota   WITH gfCodDes(POSHDR.cQuota, 'CQUOTA');
          cconttyp WITH gfCodDes(POSHDR.cconttyp, 'CCONTTYP')

  *-- The function which we get descreption of the custom fields.
  =lfvConDsc()

  *--Case for the notepad in the back.
  INSERT INTO (lcTmpPoHd) (TYPE , cStyType , PO , llprnNots , PoIndex , PageOf) values ('I' , lcRpForm , POSHDR.PO , .T. , lnPoIndex , LcPageOf)
  =lfbacknot()
  
  SELECT POSLN
  SCAN REST WHILE cstytype+po+STR(lineno,6)+trancd = lcRpForm + POSHDR.PO ;
       FOR POSLN.Trancd = '1'

    lnContEePo = lnContEePo + 1
    lnContNtpd = lnContNtpd + 1

    IF MOD(lnconteepo,12) # 0
      =lfInsert()
      *--Case the po lines finished and there is notepad,I must put record for the back
      IF POSLN.LineNo = lnContPO AND MOD(lnconteepo,11) = 0
        IF llRpPrtPn AND SEEK('P' + POSHDR.PO , 'NOTEPAD')
          REPLACE llhrdTxt WITH .F.
        ENDIF
        lnContEePo = lnContEePo + 1
        INSERT INTO (lcTmpPoHd) (TYPE , cStyType , PO , PoIndex , PageOf , llprnNots , LineNo) values ('L' , lcRpForm , POSHDR.PO , lnPoIndex , LcPageOf , .T. , lnContEePo)
        =lfbacknot()
        *-- To return to the origenal number.
        lnContEePo = lnContEePo - 1
      ENDIF
    ELSE
      *-- Case the message user greater than one page in the back.
      INSERT INTO (lcTmpPoHd) (TYPE , cStyType , PO , LineNo , llprnNots , PoIndex , PageOf) values ('L' , lcRpForm , POSHDR.PO , lnContEePo , .T. , lnPoIndex , LcPageOf)
      lnContEePo = lnContEePo + 1
      =lfbacknot()
      =lfInsert()
      *-- To return to the origenal number.
      lnContEePo = lnContEePo - 1
    ENDIF
  ENDSCAN

  *-- Case for the notepad.
  IF llRpPrtPn AND SEEK('P' + POSHDR.PO , 'NOTEPAD')
    SELECT (lcTmpPoHd)
    INSERT INTO (lcTmpPoHd) FROM MEMVAR
    REPLACE TYPE    WITH 'L'       ;
            LineNo  WITH 222222    ;
            PoIndex WITH lnPoIndex ;
            PageOf  WITH LcPageOf

    =lfNotePad()
  ENDIF

  *-- In case the lines < 11 and print notpad = NO to create records till the end of the page.
  IF MOD(lnconteepo,11) # 0 AND !llRpPrtPn
    lnValue = MOD(lnconteepo,11)
    FOR lnComplt = lnValue TO 10
      lnContEePo = lnContEePo + 1
      INSERT INTO (lcTmpPoHd) (TYPE , cStyType , PO        , LineNo     , PoIndex                 , PageOf   , PageNo  );
                       values ('L'  , lcRpForm , POSHDR.PO , lnContEePo , lnPoIndex , LcPageOf , LcPageNo)
    ENDFOR

    *-- FOR THE HARD TEXT IN THE FRX
    IF MOD(lnconteepo,11) = 0
      lcSelct = SELECT(0)
      SELECT (lcTmpPoHd)
      REPLACE llhrdTxt WITH .T.
      SELECT(lcSelct)
    ENDIF

    *-- The back.
    lnContEePo = lnContEePo + 1
    INSERT INTO (lcTmpPoHd) (TYPE , cStyType , PO , LineNo , llprnNots , COLORUPC , PoIndex , PageOf) values ('L' , lcRpForm , POSHDR.PO , lnContEePo , .T. , 'DELETE' , lnPoIndex , LcPageOf)
    =lfbacknot()
  ENDIF

  *-- In case the lines < 11 and print notpad = YES and there is no notepad in the notepad file ,
  *-- get in to create records till the end of the page.
  IF MOD(lnconteepo,11) # 0 AND llRpPrtPn AND !SEEK('P' + POSHDR.PO , 'NOTEPAD')
    lnValue = MOD(lnconteepo,11)
    FOR lnComplt = lnValue TO 10
      lnContEePo = lnContEePo + 1
      INSERT INTO (lcTmpPoHd) (TYPE , cStyType , PO        , LineNo     , PoIndex                 , PageOf   , PageNo  );
                       values ('L'  , lcRpForm , POSHDR.PO , lnContEePo , lnPoIndex , LcPageOf , LcPageNo)
    ENDFOR

    *-- FOR THE HARD TEXT IN THE FRX
    IF MOD(lnconteepo,11) = 0
      lcSelct = SELECT(0)
      SELECT (lcTmpPoHd)
      REPLACE llhrdTxt WITH .T.
      SELECT(lcSelct)
    ENDIF

    *-- The back.
    lnContEePo = lnContEePo + 1
    INSERT INTO (lcTmpPoHd) (TYPE , cStyType , PO , LineNo , llprnNots , COLORUPC , PoIndex , PageOf) values ('L' , lcRpForm , POSHDR.PO , lnContEePo , .T. , 'DELETE' , lnPoIndex , LcPageOf)
    =lfbacknot()
  ENDIF

  *-- Case the upc lines = 30 and increment the lnLineUpc
  *-- llhrdTxt >> flage to not print the line # 30
  IF lnLineUpc # 0 AND MOD(lnCount30,30) = 0 AND lnCount30 # 0
    SELECT (lcTmpPoHd)
    SCATTER MEMVAR

    FOR I = 1 TO 2
      INSERT INTO (lcTmpPoHd) FROM MEMVAR
      lnLineUpc = IIF(I = 1 , lnLineUpc + 1 , lnLineUpc)

      *--lnCount30 to count 30 recrds
      lnCount30 = IIF(I = 1 , lnCount30 + 1 , lnCount30)
      REPLACE TYPE      WITH 'S'                                      ;
              llprnNots WITH .F.                                      ;
              Linenoupc WITH lnLineUpc                                ;
              llprnUpc  WITH M.llprnUpc                               ;
              Qty       WITH EVALUATE(lcTmpPoHd+'.TotQty')            ;
              ColorUpc  WITH SUBSTR(POSLN.Style,lnMajSize+2,lnNonMaj) ;
              SortUpc   WITH IIF(I = 1 , 'A' , 'B')                   ;
              PoIndex   WITH lnPoIndex                                ;
              PageNo    WITH lnUpcPagNo                               ;
              UpcNum    WITH EVALUATE(lcStyleUpc+'.cupcnum1')+EVALUATE(lcStyleUpc+'.cupcnum2')+EVALUATE(lcStyleUpc+'.cupcnum3')

      IF MOD(lnCount30,30) = 0
        *--This variable used to know how many times we have 30 records.
        lnUpcBkNot = IIF(I = 1 , lnUpcBkNot + 1 , lnUpcBkNot)
        REPLACE UpcBkNot WITH lnUpcBkNot;
                PageNo   WITH lnUpcBkNot

        *--Case for the notepad in the back.
        INSERT INTO (lcTmpPoHd) FROM MEMVAR
        lnLineUpc = lnLineUpc + 1
        REPLACE TYPE      WITH 'S'            ;
                llprnNots WITH .T.            ;
                lcSeek    WITH 'BACKNOTE'     ;
                PageNo    WITH lnUpcBkNot     ;
                PoIndex   WITH lnPoIndex      ;
                Linenoupc WITH lnLineUpc      ;
                SortUpc   WITH IIF(I = 1 , 'A' , 'B')

        *-- Case the records more than 1 page
        lnUpcPagNo = IIF(I = 1 , lnUpcPagNo + 1 , lnUpcPagNo)

        *-- Case 'A' ( 30 +1 =31 ) musy be 30 to print the rigth # in case 'B'
        lnLineUpc = IIF(I = 1 , lnLineUpc -1 , lnLineUpc)

      ENDIF
    ENDFOR
  ENDIF

  IF MOD(lnCount30,30) = 0 AND lnCount30 # 0
    STORE 0 TO lnCount30
  ENDIF

  *-- Case to complete the page for the upc if the lines < 30
  *-- and in other case didn't get inside in case the lines = 30.
  IF lnLineUpc # 0 AND MOD(lnCount30,30) < 30 AND lnCount30 # 0
    SELECT (lcTmpPoHd)
    lnupcomplt = MOD(lnCount30,30)
    FOR lnCompUpc = lnupcomplt TO 29
      FOR I = 1 TO 2
        INSERT INTO (lcTmpPoHd) FROM MEMVAR
        lnLineUpc = IIF(I = 1 , lnLineUpc + 1 , lnLineUpc)

        *--lnCount30 to count 30 recrds
        lnCount30 = IIF(I = 1 , lnCount30 + 1 , lnCount30)

        REPLACE TYPE      WITH 'S'                     ;
                Linenoupc WITH lnLineUpc               ;
                Lineno    WITH 9999                    ;
                llprnUpc  WITH .F.                     ;
                SortUpc   WITH IIF(I = 1 , 'A' , 'B')  ;
                PoIndex   WITH lnPoIndex               ;
                llhrdTxt  WITH .T.                     ;
                PageNo    WITH lnUpcPagNo

        IF MOD(lnCount30,30) = 0
          REPLACE llhrdTxt WITH .T.

          *--This variable used to know how many times we have 30 records.
          lnUpcBkNot = IIF(I = 1 , lnUpcBkNot + 1 , lnUpcBkNot)
          REPLACE UpcBkNot WITH lnUpcBkNot;
                  PageNo   WITH lnUpcBkNot

          *--Case for the notepad in the back.
          INSERT INTO (lcTmpPoHd) FROM MEMVAR
          lnLineUpc = lnLineUpc + 1
          REPLACE TYPE      WITH 'S'            ;
                  PoIndex   WITH lnPoIndex      ;
                  llprnNots WITH .T.            ;
                  Lineno    WITH 9999           ;
                  lcSeek    WITH 'BACKNOTE'     ;
                  PageNo    WITH lnUpcBkNot     ;
                  Linenoupc WITH lnLineUpc      ;
                  SortUpc   WITH IIF(I = 1 , 'A' , 'B')

          *-- Case 'A' ( 30 +1 =31 ) musy be 30 to print the rigth # in case 'B'
          lnLineUpc = IIF(I = 1 , lnLineUpc -1 , lnLineUpc)

        ENDIF
      ENDFOR
    ENDFOR
  ENDIF

  *--Case for the message note to be print in case it is not finished in the back of the upc.
  IF lnUpcBkNot # 0
    SELECT (lcTmpPoHd)
    FOR lnLop = 1 TO lnUpcBkNot
      lnupc = (30 * lnLop) + (lnLop-1)
      FOR I = 1 TO 2
      =SEEK(STR(LnPoIndex,10) + 'S    ' + lcRpForm + PO + IIF(I = 1 , 'A' , 'B') + STR(lnupc,6) , lcTmpPoHd )
      IF lnLop = lnUpcBkNot
        *-- REPLACE THE FIELD WITH .T. TO PRINT THE HARD TEXT IN CASE WE HAVE MORE THAN 30 LINES
        *-- And didn't print the header in the 2nd page.
        REPLACE llhrdTxt WITH .T.  ;
                HardCode WITH 'END'
      ENDIF
      *-- Then after that skip to the 2nd record to print in it the backnote.
      SKIP
      IF I = 2
        lnCounter = lnCounter -1
        lnNoLinNot = lnNoLinNot - 54
      ENDIF
      =lfbacknot()
      ENDFOR
    ENDFOR
  ENDIF
  IF lnUpcBkNot # 0
    SELECT (lcPoUpc)
    APPEND BLANK
    REPLACE PO       WITH POSHDR.PO ;
            UpcRecNo WITH ALLTRIM(STR(CEILING(lnLineUpc/31)))
  ENDIF

  *-- Case the Po is change to another one,Initial variables used in notepad in the back.
  IF POSLN.PO # lcPoee
    STORE 1  TO lnCounter
  ENDIF
ENDSCAN

SELECT POSHDR
SET RELATION OFF INTO Apvendor
SET RELATION OFF INTO POSLN
SET RELATION off INTO &TMPObjlink

SELECT POSLN
SET RELATION OFF INTO Objlink

*-- Case the user select print transaction = No
IF llPrntBoth
  SET RELATION OFF INTO (lcNoteLns)
ENDIF

SELECT objlink
SET RELATION off INTO Objects

SELECT &TMPObjlink
SET RELATION TO

SELECT POSLN
SET ORDER TO POSLN

SELECT (lcTmpPoHd)
SET RELATION TO &lcTmpPoHd..vendor INTO Apvendor ADDITIVE
SET RELATION TO lcRpForm + PO + &lcTmpPoHd..STYLE INTO POSLN ADDITIVE
IF !('PO INTO ' $ SET('RELATION'))
  SET RELATION TO PO INTO (lcPoUpc) ADDITIVE
ENDIF

GOTO TOP
DO gfDispRe WITH EVAL('lcFormName')
WAIT CLEAR
llPoDisp = .F.                         && variable to indicate that we will print from the optional
                                       && program not from the main program.

*-- Check if the Temp. file that hold the lines is empty and 
*-- if it is not must delete all the records.
IF RECCOUNT(lcTmpPoHd) > 0
  SELECT (lcTmpPoHd)
  DELETE ALL
  ZAP
ENDIF

*-- Set the POSLN and break all the relation of it.
SELECT POSLN
SET RELATION TO

SELECT (lcTmpPoHd)
SET RELATION OFF INTO POSLN
SET RELATION OFF INTO Apvendor

SELECT POSLN
CLOSE INDEX
ERASE (gcWorkDir +lcTmpIndx  + '.CDX')

*!*************************************************************
*! Name      : lfGetCodes
*! Developer : Bassem Rafaat (BWA)
*! Date      : 10/22/2000
*! Purpose   : To get the codes descreption.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfGetCodes()
*!*************************************************************
FUNCTION lfGetCodes
PARAMETER lcDummy

PRIVATE lnAlias
lnAlias = SELECT(0)
=gfRltFld(&lcTmpPoHd..cDivision , @laDivLName , 'CDIVISION')

llEndGroup  = .F.
lcDivDesc   = gfCodDes(&lcTmpPoHd..cDIVISION, 'CDIVISION')
lcShipVia   = gfCodDes(&lcTmpPoHd..ShipVia , 'SHIPVIA')
lcTerms     = gfCodDes(&lcTmpPoHd..CTERMCODE, 'CTERMCODE')
lcFabGroup  = gfCodDes(&lcTmpPoHd..CPURCODE, 'CPURCODE')

IF &lcTmpPoHd..cStyType # 'N'
  lcVenName   = APVENDOR.CVenComp
  lcFax       = APVENDOR.CFAXNO
  laVenAdr[1] = gfGetAdr('APVENDOR' , '' , '' , '' , 1)
  laVenAdr[2] = gfGetAdr('APVENDOR' , '' , '' , '' , 2)
  laVenAdr[3] = gfGetAdr('APVENDOR' , '' , '' , '' , 3)
  laVenAdr[4] = gfGetAdr('APVENDOR' , '' , '' , '' , 4)
  laVenAdr[5] = gfGetAdr('APVENDOR' , '' , '' , '' , 5)
ELSE
  =SEEK(LEFT(&lcTmpPoHd..Vendor,6),'WAREHOUS')
  lcVenName   = WAREHOUS.cDesc
  lcFax       = WAREHOUS.cFAX
  laVenAdr[1] = gfGetAdr('WAREHOUS' , '' , '' , '' , 1)
  laVenAdr[2] = gfGetAdr('WAREHOUS' , '' , '' , '' , 2)
  laVenAdr[3] = gfGetAdr('WAREHOUS' , '' , '' , '' , 3)
  laVenAdr[4] = gfGetAdr('WAREHOUS' , '' , '' , '' , 4)
  laVenAdr[5] = gfGetAdr('WAREHOUS' , '' , '' , '' , 5)
ENDIF

SELECT (lnAlias)
RETURN ''

*-- End of lfGetCodes.
*!*************************************************************
*! Name      : lfGetNotes
*! Developer : Bassem Rafaat (BWA)
*! Date      : 10/22/2000
*! Purpose   : Print the PO notes.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfGetNotes()
*!*************************************************************
FUNCTION lfGetNotes
PRIVATE lnAlias

lcNotes = ''
lnAlias = SELECT(0)

*-- If we are to print both notes.
IF llRpPrtPn .AND. SEEK('P' + &lcTmpPoHd..PO , 'NOTEPAD') .AND. !EMPTY(ALLTRIM(NOTEPAD.mNotes))
      lcNotes = ALLTRIM(NotePad.mNotes)

ENDIF
SELECT (lnAlias)
RETURN lcNotes

*-- End of lfGetNotes.
*!*************************************************************
*! Name      : lfGetMemo
*! Developer : Bassem Rafaat (BWA)
*! Date      : 10/22/2000
*! Purpose   : Check if the memo file doesn't exsit show the screen.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfGetMemo()
*!*************************************************************
FUNCTION lfGetMemo
PARAMETER lcDummy

IF !FILE(gcWorkDir + 'PoTerms.MEM')
  DO (gcRepHome + 'POEE1.SPX')
ENDIF

*-- End of lfGetMemo.
*!*************************************************************
*! Name      : lfvMsgSave
*! Developer : Bassem Rafaat (BWA)
*! Date      : 10/22/2000
*! Purpose   : Valid function to the Save button on the notes screen.
*!*************************************************************
*! Called from : POEE1.SPX
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvMsgSave()
*!*************************************************************
FUNCTION lfVMsgSave
PARAMETER lcDummy

SAVE TO (gcWorkDir + 'PoTerms.MEM') ALL LIKE lcNotTerms
CLEAR READ

*-- End of lfvMsgSave.
*!*************************************************************
*! Name      : lfFirstTim
*! Developer : Bassem Rafaat (BWA)
*! Date      : 10/22/2000
*! Purpose   : Function to initial the variabls in the first time.
*!*************************************************************
*! Called from : SYREPUVR.DBF
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfFirstTim()
*!*************************************************************
FUNCTION lfFirstTim
PARAMETER lcReturn

DECLARE laFileStru[1,1]

CREATE CURSOR (lcLogBmp) (gPic G)
APPEND BLANK
APPEND GENERAL gPIC FROM ( gcBmpHome + 'EEPOLOG.BMP')

CREATE CURSOR (lcPoUpc) (Po C(6) , UpcRecNo C(10))
INDEX ON Po TAG (lcPoUpc) OF (gcWorkDir+lcPoUpc+".CDX")


SELECT POSHDR
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+35,4]

*-- Fields for the po line quantity.
FOR X = 1 TO 8
  laFileStru[lnFileStru + X , 1] = 'QTY'+ALLTRIM(STR(X))
  laFileStru[lnFileStru + X , 2] = 'N'
  laFileStru[lnFileStru + X , 3] = 6
  laFileStru[lnFileStru + X , 4] = 0
ENDFOR

*-- Field for the Total quantity.
laFileStru[lnFileStru + 9 , 1] = 'Totqty'
laFileStru[lnFileStru + 9 , 2] = 'N'
laFileStru[lnFileStru + 9 , 3] = 7
laFileStru[lnFileStru + 9 , 4] = 0

*-- Field for the type that will print.
laFileStru[lnFileStru + 10 , 1] = 'Type'
laFileStru[lnFileStru + 10 , 2] = 'C'
laFileStru[lnFileStru + 10 , 3] = 5
laFileStru[lnFileStru + 10 , 4] = 0

*-- Field for the LineNo.
laFileStru[lnFileStru + 11 , 1] = 'LineNo'
laFileStru[lnFileStru + 11 , 2] = 'N'
laFileStru[lnFileStru + 11 , 3] = 6
laFileStru[lnFileStru + 11 , 4] = 0

*-- Field for the Color.
laFileStru[lnFileStru + 12 , 1] = 'ColorUpc'
laFileStru[lnFileStru + 12 , 2] = 'C'
laFileStru[lnFileStru + 12 , 3] = 6
laFileStru[lnFileStru + 12 , 4] = 0

*-- Field for the Size descreption.
laFileStru[lnFileStru + 13 , 1] = 'SizeDesc'
laFileStru[lnFileStru + 13 , 2] = 'C'
laFileStru[lnFileStru + 13 , 3] = 5
laFileStru[lnFileStru + 13 , 4] = 0

*-- Field for the string posln quantity and the size.
laFileStru[lnFileStru + 14 , 1] = 'Qty'
laFileStru[lnFileStru + 14 , 2] = 'N'
laFileStru[lnFileStru + 14 , 3] = 6
laFileStru[lnFileStru + 14 , 4] = 0

*-- Field for the dublicate of the UPC lines and put it in the index.
laFileStru[lnFileStru + 15 , 1] = 'SortUpc'
laFileStru[lnFileStru + 15 , 2] = 'C'
laFileStru[lnFileStru + 15 , 3] = 1
laFileStru[lnFileStru + 15 , 4] = 0

*-- Field for the style.
laFileStru[lnFileStru + 16 , 1] = 'Style'
laFileStru[lnFileStru + 16 , 2] = 'C'
laFileStru[lnFileStru + 16 , 3] = 20
laFileStru[lnFileStru + 16 , 4] = 0

*-- Field for the MNOTES.
laFileStru[lnFileStru + 17 , 1] = 'mNotes'
laFileStru[lnFileStru + 17 , 2] = 'M'
laFileStru[lnFileStru + 17 , 3] = 10
laFileStru[lnFileStru + 17 , 4] = 0

*-- Field for the Gros_Price.
laFileStru[lnFileStru + 18 , 1] = 'Gros_Price'
laFileStru[lnFileStru + 18 , 2] = 'N'
laFileStru[lnFileStru + 18 , 3] = 12
laFileStru[lnFileStru + 18 , 4] = 2

*-- Field for the UpcNum.
laFileStru[lnFileStru + 19 , 1] = 'UpcNum'
laFileStru[lnFileStru + 19 , 2] = 'C'
laFileStru[lnFileStru + 19 , 3] = 13
laFileStru[lnFileStru + 19 , 4] = 0

*-- Field to check if print notes or not.
laFileStru[lnFileStru + 20 , 1] = 'llprnNots'
laFileStru[lnFileStru + 20 , 2] = 'L'
laFileStru[lnFileStru + 20 , 3] = 1
laFileStru[lnFileStru + 20 , 4] = 0

*-- Field for the real line in the posln file.
laFileStru[lnFileStru + 21 , 1] = 'RealLine'
laFileStru[lnFileStru + 21 , 2] = 'N'
laFileStru[lnFileStru + 21 , 3] = 6
laFileStru[lnFileStru + 21 , 4] = 0

*-- Field to print first line in upc only.
laFileStru[lnFileStru + 22 , 1] = 'llprnUpc'
laFileStru[lnFileStru + 22 , 2] = 'L'
laFileStru[lnFileStru + 22 , 3] = 1
laFileStru[lnFileStru + 22 , 4] = 0

*-- Field for the real line in the posln file.
laFileStru[lnFileStru + 23 , 1] = 'Linenoupc'
laFileStru[lnFileStru + 23 , 2] = 'N'
laFileStru[lnFileStru + 23 , 3] = 6
laFileStru[lnFileStru + 23 , 4] = 0

*-- Field for sorting the temp. file.
laFileStru[lnFileStru + 24 , 1] = 'PoIndex'
laFileStru[lnFileStru + 24 , 2] = 'N'
laFileStru[lnFileStru + 24 , 3] = 10
laFileStru[lnFileStru + 24 , 4] = 0

*-- Field for the hard coded text.
laFileStru[lnFileStru + 25 , 1] = 'HardCode'
laFileStru[lnFileStru + 25 , 2] = 'C'
laFileStru[lnFileStru + 25 , 3] = 3
laFileStru[lnFileStru + 25 , 4] = 0

*-- Field use it to print th hard text or not.
laFileStru[lnFileStru + 26 , 1] = 'llhrdTxt'
laFileStru[lnFileStru + 26 , 2] = 'L'
laFileStru[lnFileStru + 26 , 3] = 1
laFileStru[lnFileStru + 26 , 4] = 0

*-- Field used to seek with it for the records of the back in case upc.
laFileStru[lnFileStru + 27 , 1] = 'lcSeek'
laFileStru[lnFileStru + 27 , 2] = 'C'
laFileStru[lnFileStru + 27 , 3] = 8
laFileStru[lnFileStru + 27 , 4] = 0

*-- Field for sorting the backnote for the upc.
laFileStru[lnFileStru + 28 , 1] = 'UpcBkNot'
laFileStru[lnFileStru + 28 , 2] = 'N'
laFileStru[lnFileStru + 28 , 3] = 6
laFileStru[lnFileStru + 28 , 4] = 0

*-- Field for the first part of the page no.
laFileStru[lnFileStru + 29 , 1] = 'PageNo'
laFileStru[lnFileStru + 29 , 2] = 'N'
laFileStru[lnFileStru + 29 , 3] = 6
laFileStru[lnFileStru + 29 , 4] = 0

*-- Field for the 2nd part of the page no.
laFileStru[lnFileStru + 30 , 1] = 'PageOf'
laFileStru[lnFileStru + 30 , 2] = 'N'
laFileStru[lnFileStru + 30 , 3] = 6
laFileStru[lnFileStru + 30 , 4] = 0

*-- Fields for the custom addressess.
FOR X = 31 TO 35
  laFileStru[lnFileStru + X , 1] = 'cConsDisc'+ALLTRIM(STR(X-30))
  laFileStru[lnFileStru + X , 2] = 'C'
  laFileStru[lnFileStru + X , 3] = 35
  laFileStru[lnFileStru + X , 4] = 0
ENDFOR

CREATE TABLE (gcWorkDir+lcTmpPoHd) FROM ARRAY laFileStru
INDEX ON STR(PoIndex,10) + TYPE + cStyType + PO + SortUpc + STR(Linenoupc,6) + lcSeek + STR(UpcBkNot,6) + STR(LineNo,6) TAG (lcTmpPoHd) OF (gcWorkDir+lcTmpPoHd+".CDX")

*-- Open the STYLEUPC file.
IF !USED(lcStyleUpc)
  =gfOpenFile(gcDataDir+"STYLEUPC","STYLEUPC", 'SH', @lcStyleUpc, .T.)
ENDIF

*-- Restore from the memo file that holds the user notes to be printed 
*-- on the back of the first page.
CREATE CURSOR (lcNotsTbl) (mNote M(10))
APPEND BLANK

IF FILE(gcWorkDir + 'PoTerms.MEM')
  RESTORE FROM (gcWorkDir + 'POTerms.mem') ADDIT
  REPLACE mNote WITH lcNotTerms
ELSE
  REPLACE mNote WITH SPACE(0)
ENDIF

RETURN ''
*-- End of lfFirstTim.
*!*************************************************************
*! Name      : lfbacknot
*! Developer : Bassem Rafaat (BWA)
*! Date      : 10/22/2000
*! Purpose   : Function to Get the notepad wrote by the user in the back.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
*-- The page will hold 52 line for the notepad.
FUNCTION lfbacknot

STORE 1 TO lnNotLine , lnBegin , lnEnd
lcAlias = ALIAS()

lnOldMemW = SET("MEMOWIDTH")
SET MEMOWIDTH TO 75
SELECT (lcTmpPoHd)

lnBegin = lnNotLine+(52*(lnCounter-1))
lnEnd = IIF(lnCounter > 1 , 52*lnCounter,52)

lnMemLins = MEMLINES(EVAL(lcNotsTbl+'.MNOTE'))
FOR lnNotes = lnBegin TO lnEnd
  REPLACE &lcTmpPoHd..mNotes WITH IIF(!EMPTY(MLINE(EVAL(lcNotsTbl+'.MNOTE'),lnNotes)) , MLINE(EVAL(lcNotsTbl+'.MNOTE'),lnNotes) , SPACE(5) ) + CHR(13) ADDITIVE
ENDFOR

lnNoLinNot = lnNoLinNot + 52
lnCounter = lnCounter + 1

SET MEMOWIDTH TO lnOldMemW
SELECT (lcAlias)

*-- End of lfbacknot.
*!*************************************************************
*! Name      : lfInsert
*! Developer : Bassem Rafaat (BWA)
*! Date      : 10/22/2000
*! Purpose   : Function to insert the po lines.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfInsert

*-- lcStylUpc >> Variable to check that the po style change.
*-- lnTotQty  >> Variable hold the total quantity for the quantity per (po style color).
lcStylUpc = SPACE(0)
STORE 0 TO lnTotQty

SCATTER MEMVAR MEMO
LcPageNo = 1 + CEILING(POSLN.LineNo/11)
SELECT (lcTmpPoHd)
INSERT INTO (lcTmpPoHd) FROM MEMVAR
REPLACE TYPE     WITH 'L'          ;
        LineNo   WITH lnContEePo   ;
        RealLine WITH POSLN.LineNo ;
        PageOf   WITH LcPageOf     ;
        PageNo   WITH LcPageNo     ;
        PoIndex  WITH lnPoIndex

*-- FOR THE HARD TEXT IN THE FRX
IF MOD(lnconteepo,11) = 0 AND (lnContPO = lnContEePo)
  REPLACE llhrdTxt WITH .T.
ENDIF

*-- Case for the StyleUpc.
IF SEEK(POSLN.STYLE,lcStyleUpc)
  FOR lnSiz = 1 TO SCALE.CNT
  
    *-- To seek with the style and the size #.
    =SEEK(POSLN.STYLE + ALLTRIM(STR(lnSiz)) ,lcStyleUpc)

    lnQty = 'QTY' + ALLTRIM(STR(lnSiz))
    IF POSLN.&lnQty > 0
      lnTotQty = lnTotQty + POSLN.&lnQty
      SELECT (lcStyleUpc)
      SCATTER FIELDS STYLE,SIZE,cupcnum1,cupcnum2,cupcnum3 MEMVAR
      SELECT (lcTmpPoHd)

      IF MOD(lnCount30,30) = 0 AND lnCount30 # 0
        STORE 0 TO lnCount30
      ENDIF

      FOR I = 1 TO 2
        INSERT INTO (lcTmpPoHd) FROM MEMVAR
        lnLineUpc = IIF(I = 1 , lnLineUpc + 1 , lnLineUpc)

        *--lnCount30 to count 30 recrds
        lnCount30 = IIF(I = 1 , lnCount30 + 1 , lnCount30)
        REPLACE TYPE      WITH 'S'                                                       ;
                Linenoupc WITH lnLineUpc                                                 ;
                llprnUpc  WITH IIF(lcStylUpc # POSLN.STYLE , .T. , .F.)                  ;
                ColorUpc  WITH SUBSTR(POSLN.Style,lnMajSize+2,lnNonMaj)                  ;
                SizeDesc  WITH EVALUATE('SCALE.SZ' + ALLTRIM(EVAL(lcStyleUpc+'.SIZE')))  ;
                Qty       WITH EVALUATE('POSLN.Qty' + ALLTRIM(EVAL(lcStyleUpc+'.SIZE'))) ;
                SortUpc   WITH IIF(I = 1 , 'A' , 'B')                                    ;
                PoIndex   WITH lnPoIndex                                                 ;
                llhrdTxt  WITH .T.                                                       ;
                PageNo    WITH lnUpcPagNo                                                ;
                UpcNum    WITH EVALUATE(lcStyleUpc+'.cupcnum1')+EVALUATE(lcStyleUpc+'.cupcnum2')+EVALUATE(lcStyleUpc+'.cupcnum3')

        IF MOD(lnCount30,30) = 0
          REPLACE llhrdTxt WITH .T.

          *--This variable used to know how many times we have 30 records.
          lnUpcBkNot = IIF(I = 1 , lnUpcBkNot + 1 , lnUpcBkNot)
          REPLACE UpcBkNot WITH lnUpcBkNot;
                  PageNo   WITH lnUpcBkNot

          *--Case for the notepad in the back.
          INSERT INTO (lcTmpPoHd) FROM MEMVAR
          lnLineUpc = lnLineUpc + 1
          REPLACE TYPE      WITH 'S'          ;
                  llprnNots WITH .T.          ;
                  lcSeek    WITH 'BACKNOTE'   ;
                  PageNo    WITH lnUpcBkNot   ;
                  Linenoupc WITH lnLineUpc    ;
                  SortUpc   WITH IIF(I = 1 , 'A' , 'B') ;
                  PoIndex   WITH lnPoIndex

          *-- Case the records more than 1 page
          lnUpcPagNo = IIF(I = 1 , lnUpcPagNo + 1 , lnUpcPagNo)

          *-- Case 'A' ( 30 +1 =31 ) musy be 30 to print the rigth # in case 'B'
          lnLineUpc = IIF(I = 1 , lnLineUpc -1 , lnLineUpc)

        ENDIF
      ENDFOR
    ENDIF
    lcStylUpc = POSLN.STYLE
  ENDFOR

  IF MOD(lnCount30,30) = 0 AND lnCount30 # 0
    STORE 0 TO lnCount30
  ENDIF

  *-- Case for the total quantity for the upc.
  FOR I = 1 TO 2
    INSERT INTO (lcTmpPoHd) FROM MEMVAR
    lnLineUpc = IIF(I = 1 , lnLineUpc + 1 , lnLineUpc)

    *--lnCount30 to count 30 recrds
    lnCount30 = IIF(I = 1 , lnCount30 + 1 , lnCount30)

    REPLACE TYPE      WITH 'S'                     ;
            llprnUpc  WITH .T.                     ;
            Linenoupc WITH lnLineUpc               ;
            LineNo    WITH POSLN.LineNo            ;
            Qty       WITH lnTotQty                ;
            PageNo    WITH lnUpcPagNo              ;
            STYLE     WITH 'Contract Line Totals'  ;
            SortUpc   WITH IIF(I = 1 , 'A' , 'B')  ;
            PoIndex   WITH lnPoIndex               ;
            llhrdTxt  WITH .T.

      IF MOD(lnCount30,30) = 0
        REPLACE llhrdTxt WITH .T.
          
        *--This variable used to know how many times we have 30 records.
        lnUpcBkNot = IIF(I = 1 , lnUpcBkNot + 1 , lnUpcBkNot)
        REPLACE UpcBkNot WITH lnUpcBkNot;
                PageNo   WITH lnUpcBkNot

        *--Case for the notepad in the back.
        INSERT INTO (lcTmpPoHd) FROM MEMVAR
        lnLineUpc = lnLineUpc + 1
        REPLACE TYPE      WITH 'S'          ;
                llprnNots WITH .T.          ;
                lcSeek    WITH 'BACKNOTE'   ;
                PageNo    WITH lnUpcBkNot   ;
                Linenoupc WITH lnLineUpc    ;
                SortUpc   WITH IIF(I = 1 , 'A' , 'B') ;
                PoIndex   WITH lnPoIndex

      *-- Case the records more than 1 page
      lnUpcPagNo = IIF(I = 1 , lnUpcPagNo + 1 , lnUpcPagNo)

      *-- Case 'A' ( 30 +1 =31 ) musy be 30 to print the rigth # in case 'B'
      lnLineUpc = IIF(I = 1 , lnLineUpc -1 , lnLineUpc)

    ENDIF
  ENDFOR
ENDIF 
IF MOD(lnCount30,30) = 0 AND lnCount30 # 0
  STORE 0 TO lnCount30
ENDIF

*-- End of lfInsert.
*!*************************************************************
*! Name      : lfNotePad
*! Developer : Bassem Rafaat (BWA)
*! Date      : 10/22/2000
*! Purpose   : Function to Get the notepad from the notepad file.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
*-- The page hold 34 line for the notepad in one page and 10 record for po.
*-- 30/10 >> 3


FUNCTION lfNotePad
*--lnPoLine  >> Hold the remaning lines in the page for the notepad.
*--lnContNot >> Hold the # of times the notepad take pages.

STORE 1 TO lnNotLine , lnFrom , lnTo , lnContNot
STORE 0 TO lnPoLine
llFinsh = .T.
lcAlias = ALIAS()
lnOldMemW = SET("MEMOWIDTH")
SET MEMOWIDTH TO 75

SELECT (lcTmpPoHd)
lnNotcont = IIF(llRpPrtPn,MEMLINES(NOTEPAD.MNOTES) , 0)

DO WHILE llFinsh
  lnFrom = lnNotLine
  *B803856,1 Change the layout of the FRX to match with the priprented form.[START]
  *lnPoLine = CEILING(MOD(lnContNtpd,11) * 3.36)
  lnPoLine = CEILING(MOD(lnContNtpd,11) * 3)
  *B803856,1 [END]

  *-- 30 in the 2nd page because the text 'NOTES' in the FRX take one place and must subtract from 31.
  *B803856,1 Change the layout of the FRX to match with the priprented form.[START]
  *lnTo = IIF(lnContNot > 1 ,  lnFrom + 36  , 37 - lnPoLine)
  lnTo = IIF(lnContNot > 1 ,  lnFrom + 30  , 31 - lnPoLine)
  *B803856,1 [END]

  FOR lnNot = lnFrom TO lnTo
    REPLACE &lcTmpPoHd..mNotes WITH IIF(!EMPTY(MLINE(NOTEPAD.MNOTES , lnNot)), MLINE(NOTEPAD.MNOTES , lnNot) , SPACE(5)) + CHR(13) ADDITIVE
  ENDFOR

  *-- To replace the pageno.
  REPLACE PageNo  WITH LcPageNo

  *-- To Print the text NoTES
  IF lnFrom = 1
    REPLACE QTY  WITH 1
  ENDIF
  
  *-- Case for the remain lines of the notepad.
  lnContNot = lnContNot + 1
  LcPageNo  = LcPageNo + 1

  *-- To check that this is the last part for the lines and the notepad for the po.  
  IF lnTo => lnNotcont
    REPLACE llhrdTxt WITH .T.
  ENDIF
  
  *-- LineNo give it increment value to put in the right place due to the index
  lnLineNo = LineNo + 1
  INSERT INTO (lcTmpPoHd) (TYPE , cStyType , PO , LineNo , llprnNots , COLORUPC , PoIndex , PageOf) values ('L' , lcRpForm , POSHDR.PO , lnLineNo , .T. , 'DELETE' , lnPoIndex , LcPageOf)
  =lfbacknot()
  
  lnLineNo = LineNo + 1
  IF lnTo < lnNotcont
    INSERT INTO (lcTmpPoHd) FROM MEMVAR
    REPLACE TYPE    WITH 'L'       ;
            LineNo  WITH lnLineNo  ;
            PoIndex WITH lnPoIndex ;
            PageOf  WITH LcPageOf

    lnNotLine = lnTo + 1
  ELSE
    llFinsh = .F.
  ENDIF
ENDDO

SET MEMOWIDTH TO lnOldMemW
SELECT (lcAlias)

*-- End of lfNotePad.
*!*************************************************************
*! Name        : lfRecNo
*! Developer   : Bassem Rafaat (BWA)
*! Date        : 10/09/2000
*! Purpose     : To get the last Record for the po to know how
*!             : many records in this po.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Refer to    : 
*!*************************************************************
*! Example     : lfRecNo()
*!*************************************************************
FUNCTION lfRecNo
PRIVATE lcAlias

lcAlias = ALIAS()
SELECT (lcPosln_A)
SET ORDER TO TAG Trancd DESCENDING
=SEEK(LCRPFORM + POSLN.PO,lcPosln_A)

lnContPO = IIF(EOF(lcPosln_A) , 0 , EVAL(lcPosln_A+'.LINENO'))
SET ORDER TO TAG Trancd ASCENDING

SELECT (lcAlias)
RETURN ''

*-- End of lfRecNo.
*!*************************************************************
*! Name      : LFGETNRF
*! Developer : Bassem Rafaat (BWA)
*! Date      : 10/22/2000
*! Purpose   : To get the NRF related descreption.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =LFGETNRF()
*!*************************************************************
FUNCTION LFGETNRF
PARAMETER lcDummy

PRIVATE lnAlias
lnAlias = SELECT(0)
=gfRltFld(SUBSTR(EVAL(lcTmpPoHd+'.Style'),lnMajSize+2,lnNonMaj) , @laArray , 'COLOR')

SELECT (lnAlias)
RETURN ''

*-- End of LFGETNRF.
*!*************************************************************
*! Name      : lfvConDsc
*! Developer : Bassem Rafaat (BWA)
*! Date      : 10/22/2000
*! Purpose   : To get the descreption of the custom fields.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfvConDsc()
*!*************************************************************
FUNCTION lfvConDsc

PRIVATE lnAlias
lnAlias = SELECT(0)
SELECT POSHDR

STORE SPACE(0) TO lcTmpFld , lcField , lcAll_Desc , lcAll_Code , lcPipe , lcFldDesc
STORE 0 TO lnAtCode , lnPipe , lnPipeDes , lnNxtPipe

FOR lnDes = 1 TO 5
  lcTmpFld   = 'cConsDisc'+ALLTRIM(STR(lnDes))
  lcField    = 'CCONSADD' + ALLTRIM(STR(lnDes))
  IF !EMPTY(POSHDR.&lcField)
    =SEEK(lcField,'SYDFIELD')
    lcAll_Desc = SUBSTR(SYDFIELD.Mventries,1,AT("~",SYDFIELD.Mventries)-1)
    lcAll_Code = SUBSTR(SYDFIELD.Mventries,AT("~",SYDFIELD.Mventries)+1)
    lnAtCode   = AT(ALLTRIM(POSHDR.&lcField),lcAll_Code)
    IF lnAtCode > 1
      lcPipe     = LEFT(lcAll_Code,lnAtCode-1)
      lnPipe     = OCCURS('|',lcPipe)
      lnPipeDes  = AT('|',lcall_desc,lnpipe)
    ELSE
      STORE SPACE(0) TO lcPipe
      STORE 0 TO lnPipe
      STORE 1 TO lnPipeDes
    ENDIF
    lnNxtPipe  = AT('|',lcall_desc,lnpipe+1)

    *B803904,1 BWA 12/13/2000 1)The consignee address does not print the last chr on all lines.[START]
    *lcFldDesc  = IIF(lnNxtPipe # 0 , IIF(lnAtCode = 1 ,SUBSTR(lcall_desc , lnPipeDes , (lnNxtPipe-1)-lnPipeDes),;
                                                       SUBSTR(lcall_desc , lnPipeDes+1 , (lnNxtPipe-1)-lnPipeDes)),;
                                     SUBSTR(lcall_desc , IIF(lnAtCode = 1 , lnPipeDes , lnPipeDes+1)))
    lcFldDesc  = IIF(lnNxtPipe # 0 , IIF(lnAtCode = 1 ,SUBSTR(lcall_desc , lnPipeDes , (lnNxtPipe)-lnPipeDes),;
                                                       SUBSTR(lcall_desc , lnPipeDes+1 , (lnNxtPipe-1)-lnPipeDes)),;
                                     SUBSTR(lcall_desc , IIF(lnAtCode = 1 , lnPipeDes , lnPipeDes+1)))
    *B803904,1 [END]

    REPLACE &lcTmpPoHd..&lcTmpFld WITH lcFldDesc
  ENDIF
ENDFOR

SELECT (lnAlias)
*-- End of lfvConDsc.