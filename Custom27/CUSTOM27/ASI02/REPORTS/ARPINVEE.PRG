*:***************************************************************************
*: Program file  : ARPINVEE.PRG
*: Program desc. : CUSTOMIZED INVOICE FOR EAGLE'S EYE.
*: Date          : 10/09/2000
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : BASSEM RAFAAT (BWA)
*: Tracking Job Number: C101955
*: 
*:***************************************************************************
*: Calls : 						
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ARPINVEE
*:***************************************************************************
*! Modifications :
*!B803793,1 BWA 11/07/2000 1) Fix the fonts of the FRX and remove the text of 'optional message'.
*!B803793,1                2) Don't show the 'optional message' filter in case the 
*!B803793,1                   user fill any line in it.[Syrepuvr>>(lnMsgEE)]
*!B803793,1                3) Fill the stup part of the invoice with the data.[FRX]
*!B803854,1 BWA 11/21/2000 1) Change the "OF" on the page counter to "of".[FRX]
*!B803854,1                2) Printing 13 style/color on the form creates problems,You
*!B803854,1                   will notice that the system printed 3 pages instead of 2 and
*!B803854,1                   half of the things that should print on the back of the form 
*!B803854,1                   will be shifted to the form face.You will also notice that 
*!B803854,1                   the last page will come out with the counter (0 of 2).
*!B803906,1 ASH 12/12/2000 1- Numbers under the "Total" column should be centered (FRX)
*!B803906,1                2- Fix the bug of printing only the sizes that have qty. (FRX)
*!B803906,1                3- Get the bmp from SYFRMCDD file.
*!B604138,1 BWA 01/22/2001 The invoice doesn't print the 2nd page.
*!B804051,1 BWA 03/18/2001 The invoice line amount doesn't print if it exceeds 4 digits.[Fix in FRX]
*:***************************************************************************

*-- Initial the vaiables.
*-laNote       >> The name of the array hold the lines of the notepad.
*-lcAliastmp   >> Variable hold the used file.
*-lnLincomp    >> Hold the last lineno of the invoice.
*-lnContLin    >> Hold the number of the invoice lineno in the invline
*-lnCounter    >> Variable used to get the next lines of the notepad.
*-lnAftercnt   >> Hold the value of the lnCounter for the next time this variable get its value.
*-llFlgNote    >> Variable used to check if the notepade is finished or not.

DECLARE laNote[44,1]     && the 44 for the numbers of the line in the back paper.
STORE "" TO lcAliastmp
STORE 0  TO lnLincomp , lnContLin , lnReplace
STORE 1  TO lnCounter , lnAftercnt
llFlgNote = .F.

*-- Open the INVLINE.DBF again to get the last lineno for the invoice.
IF !USED(lcInvLine_B)
  =gfOpenFile(gcDataDir+"InvLine","InvLine",'SH', @lcInvLine_B, .T.)
ENDIF

*-- Check if the Temp. file that hold the lines is empty and 
*-- if it is not must delete all the records.
IF RECCOUNT(lcTmpLin) > 0
  SELECT (lcTmpLin)
  DELETE ALL
  ZAP
ENDIF

*-- Check with the lcrpexp in the INVHDR.DBF if there is records for the critiria or not.
SELECT INVHDR
LOCATE FOR &lcRpExp

IF !FOUND()
  *---Text : 'No Record Selected for the report..!'
  =gfModalGen('TRM00052B00000','DIALOG')
  llNoRec = .T.
  *--Case there is no records,to not preview the message tow times.
  llarpinv = .F.
  SET DEVICE TO SCREEN
  RETURN
ENDIF
lcASExp = IIF(EMPTY(lcRpExp) , '.T.' , lcRpExp)
SET DEVICE TO PRINT
SET RELATION TO 'C' + INVHDR.INVOICE INTO NOTEPAD ADDITIVE

SCAN FOR &lcASExp
  *-lcInvcee    >> Variable hold the invoice No.
  *-llFlagPlus  >> Variable used in case the lineno > 14 to increase the new lineno after that +1
  *-lnNoLinNot  >> Variable used to check if the lines of the notepad finished or not.
  *-lnAcumlat   >> Variable used in the accumulation of the lineno in the tmp. file.
  *--lnRelLinNo >> Variable used to print the real no. of the records of the invoice lines not the value in the lineno field.

  *--In case the user want to print the invoices, must change the printflage of them.
  IF gcDevice <> 'SCREEN'
    INSERT INTO (lcInvPrtUp) (INVOICE) VALUES (INVHDR.INVOICE)
  ENDIF

  lcInvcee = INVHDR.INVOICE
  llFlagPlus = .F.
  llFlagEnd  = .F.

  *B604138,1 BWA 01/22/2001 The invoice doesn't print the 2nd page.[START]
  llFlgEndGr = .F.
  *B604138,1 [END]

  *-- Reinitial the value of the lcInvcomp , M.Type every time get new invoice.
  STORE "" TO lcInvcomp  , M.Type
  STORE 0  TO lnNoLinNot , M.PageOf , lnPageOf , lnOprater , lnRelLinNo , lcChckPg
  STORE 1  TO M.PageNO   , lnAcumlat

  *-- The function which we get the last recno for the invoice.
  =lfRecNo()

  *-- Get the page counters.
  IF (lnContLin > 9 AND lnContLin < 14) OR IIF(lnContLin > 14,MOD(lnContLin,14) > 9,.F.)
    *B604138,1 BWA 01/22/2001 The invoice doesn't print the 2nd page.[START]
    *M.PageOf = CEILING(lnContLin/ 14)
      M.PageOf = CEILING(lnContLin/ 14)+1
    *B604138,1 [END]
  ELSE
    M.PageOf = IIF( lnContLin < 7 , ROUND(MOD(lnContLin/14,14),0)+1 ,CEILING(lnContLin/14))
  ENDIF
  lnPageOf = IIF(llRpInvNot,IIF(MOD(MEMLINES(NOTEPAD.MNOTES)/34,34) < 1 OR MOD(MEMLINES(NOTEPAD.MNOTES)/34,34) > INT(MOD(MEMLINES(NOTEPAD.MNOTES)/34,34)) , INT(MOD(MEMLINES(NOTEPAD.MNOTES)/34,34))+1 , INT(MOD(MEMLINES(NOTEPAD.MNOTES)/34,34))),0)
  lcChckPg = M.PageOf
  M.PageOf = IIF(M.PageOf > lnPageOf , M.PageOf, lnPageOf)

  IF MOD(lnContLin,14) = 0 AND lcChckPg > lnPageOf   && Case if there is notepad.
    M.PageOf = M.PageOf + 1
  *B604138,1 BWA 01/22/2001 (Begin) Add the Else to fix the problem of wrong pageno.
  ELSE
    IF MOD(lnContLin,14) = 0                         && && Case if the lines of the invoice divided by 14 becuse the footer will print in the 2nd page.
      M.PageOf = M.PageOf + 1
    ENDIF
  *B604138,1 BWA 01/22/2001 (End)
  ENDIF
  
  SELECT INVLINE
  SCAN REST WHILE Invoice+STR(lineno,6) = lcInvcee
    WAIT WINDOW 'Selecting Records For The Report ...' + lcInvcee NOWAIT
    SCATTER MEMVAR MEMO
    lnRelLinNo = lnRelLinNo + 1
    IF llFlagPlus
      M.LINENO = M.LINENO + lnAcumlat
    ENDIF
    INSERT INTO (lcTmpLin) FROM MEMVAR
    
    lcAliastmp = SELECT(0)
    SELECT (lcTmpLin)
    REPLACE lnlines WITH lnRelLinNo
    SELECT(lcAliastmp)
    
    *-- Case lines of invoice < 5
    *-- Case lines of invoice 14 and in the last page < 5 . I add lines after the end of the invoice line
    *-- The check must be in the last record of the invoice line not every line.
    *-- .P. to make the field in the frx !empty to break the condintion of the suppres fo the empty lines.
    *-- .F. in case the invoice lines > 5 and < 14 I didn't want to add lines.
    IF INVLINE.LINENO = lnLincomp
      *-- Case the remainder of the lines of the invoice in the last page < 5
      *-- and in the same time check if the No. of the invoice line is < the lineno value due to 
      *-- the invoice creat from sales order and sone lines from the sales order not all of it.
      lnReplace = IIF(lnContLin < lnLincomp , lnContLin , lnLincomp)
      lnOprater = INT(lnReplace/14)
      lnExp = lnReplace - (lnOprater*14)
      IF lnExp < 5
        IF lnReplace < 5 OR ( IIF(lnReplace > 14 ,MOD(lnReplace - (14*ROUND(lnReplace/14,0)),5) # 0 , .F.) AND INVLINE.LINENO = lnLincomp)
          lnFirst = IIF(lnReplace < 5 , (lnReplace + 1) , MOD((lnReplace-14),5) + 1)
          FOR I = lnFirst TO 5
            IF I < 5
              INSERT INTO (lcTmpLin) (INVOICE,PAGEOF) VALUES (INVLINE.INVOICE,M.PageOf)
            ELSE
              INSERT INTO (lcTmpLin) (INVOICE,ACCOUNT,PAGEOF) VALUES (INVLINE.INVOICE,'P',M.PageOf)
            ENDIF
          ENDFOR
        ENDIF
      ENDIF
    ENDIF

    *-- Case lines of invoice < 14 and > 9 OR the lines > 14 and take over 9 lines in the 2nd page.
    IF INVLINE.LINENO = lnLincomp
      lnReplace = IIF(lnContLin < lnLincomp , lnContLin , lnLincomp)
      IF (lnReplace > 9 AND lnReplace < 14) OR IIF(lnReplace > 14,MOD(lnReplace,14) > 9,.F.)
        lnFirst = lnReplace + 1
        lnEndFor= IIF( lnReplace > 9 AND lnReplace < 14 , 14 , lnReplace+(14-MOD(lnReplace,14)))

        *B604138,1 BWA 01/22/2001 The invoice doesn't print the 2nd page.[START]
        llFlgEndGr = .T.
        *B604138,1 [END]

        FOR I = lnFirst TO lnEndFor
          IF I = lnEndFor
            INSERT INTO (lcTmpLin) (INVOICE,PAGEOF,PageNO,llprnLin) VALUES (INVLINE.INVOICE,M.PageOf,M.PageNO+1,.T.)
          ELSE
            INSERT INTO (lcTmpLin) (INVOICE,PAGEOF,PageNO) VALUES (INVLINE.INVOICE,M.PageOf,M.PageNO+1)
          ENDIF
        ENDFOR
      ENDIF
    ENDIF

    *-- (MOD(lnRelLinNo,14)) for the case if the user make sales invoice and take from the sales order part of the lines not all the lines.
    *B604138,1 BWA 01/22/2001 The invoice doesn't print the 2nd page.[START]
    *IF MOD(INVLINE.LINENO,14) = 0 AND MOD(lnRelLinNo,14) = 0 
     IF MOD(lnRelLinNo,14) = 0 OR llFlgEndGr
    *B604138,1 [END]

      *--Case if the invoice lines = 14 or any number / 14.
      lnReplace = IIF(lnContLin < lnLincomp , lnContLin , lnLincomp)
      IF INVLINE.LINENO = lnLincomp AND MOD(lnReplace,14) = 0 OR llFlgEndGr
        M.PageNO = M.PageNO + 1
        FOR I = 1 TO 5
          IF I < 5
           *-- use 'zzz' in the type field to get this records i insert after the 
           *-- last record for the notepad for the 14 lines of the invoice and 
           *-- befor the last record for the notepad for the last page which 
           *-- the footer must print in it.
           INSERT INTO (lcTmpLin) (INVOICE,PAGEOF,TYPE,PageNO) VALUES (INVLINE.INVOICE,M.PageOf,'ZZZ',M.PageNO)
          ELSE
           *-- In the last record print 'P' in the account field.
           INSERT INTO (lcTmpLin) (INVOICE,ACCOUNT,PAGEOF,TYPE,PageNO) VALUES (INVLINE.INVOICE,'P',M.PageOf,'ZZZ',M.PageNO)
         ENDIF
        ENDFOR
        INSERT INTO (lcTmpLin) (INVOICE,llprnNots,Type,PAGEOF) VALUES (lcInvcee,.T.,'W',M.PageOf)
        =lfNotePad(lcInvcee)
        llFlagEnd = .T.
        lcInvcomp = lcInvcee
      ELSE
        M.LINENO = M.LINENO + lnAcumlat
        INSERT INTO (lcTmpLin) (INVOICE,LINENO,llprnNots,PAGEOF) VALUES (INVLINE.INVOICE,M.LINENO,.T.,M.PageOf)
        =lfNotePad(INVOICE)
        lcInvcomp = INVHDR.INVOICE

        IF (INVLINE.LINENO/14) > lnAcumlat
          lnAcumlat = INT(INVLINE.LINENO/14)
        ENDIF  
        llFlagPlus = .T.
        M.PageNO = M.PageNO + 1
      ENDIF
    ENDIF

    *-- Case after the end of the lines of the invoice begin make the last page of the back.
    IF INVLINE.LINENO = lnLincomp AND !llFlagEnd
      IF !SEEK(INVLINE.INVOICE + 'W' , lcTmpLin ) AND !EMPTY(INVLINE.INVOICE)
        INSERT INTO (lcTmpLin) (INVOICE,llprnNots,Type,PAGEOF) VALUES (lcInvcee,.T.,'W',M.PageOf)
        =lfNotePad(lcInvcee)
        lcInvcomp = lcInvcee
      ENDIF
    ENDIF

    *-- Same case for the notepad to countiue it in the next page and in the same time
    *-- check if the notepad is (empty or finished) or not.

    *B803854,1 BWA 11/21/2000 [START]
    *IF INVLINE.LINENO = lnLincomp
    IF INVLINE.LINENO = lnLincomp AND llFlgNote
    *B803854,1 [END]

      lnReplace = IIF(lnContLin < lnLincomp , lnContLin , lnLincomp)
      IF lnReplace > 9 AND lnReplace < 14 OR IIF(lnReplace > 14,MOD(lnReplace,14) > 9,.F.)
          M.Type = 'W'
        FOR I = 1 TO 7
          IF I < 7
            INSERT INTO (lcTmpLin) (INVOICE,Type,PAGEOF,PageNO) VALUES (lcInvcee,M.Type,M.PageOf,M.PageNO+1)
          ELSE

            *B803854,1 BWA 11/21/2000 [START]
            *-- Add value to the TYPE field to get the tmp. file sorted rightly.
            *INSERT INTO (lcTmpLin) (INVOICE,ACCOUNT,PAGEOF,PageNO) VALUES (INVLINE.INVOICE,'P',M.PageOf,M.PageNO+1)
            INSERT INTO (lcTmpLin) (INVOICE,ACCOUNT,PAGEOF,PageNO,Type) VALUES (INVLINE.INVOICE,'P',M.PageOf,M.PageNO+1,M.Type)
            *B803854,1 BWA 11/21/2000 [END]

          ENDIF
        ENDFOR
        M.PageNO = M.PageNO + 1
        M.Type = ALLTRIM(M.Type) + '1'
        INSERT INTO (lcTmpLin) (INVOICE,llprnNots,Type,PAGEOF) VALUES (lcInvcee,.T.,M.Type,M.PageOf)
        =lfNotePad(lcInvcee)
       lcInvcomp = lcInvcee
      ENDIF

    *B803854,1 BWA 11/21/2000 [START]
    *-- Case the Invoice line less than the repercussion of the number 14 and must insert record for the back.
    ELSE
      IF INVLINE.LINENO = lnLincomp AND llFlagEnd
        *M.Type = 'ZZZZZZ1111'      && To be shure that the record get in the last postion.
        INSERT INTO (lcTmpLin) (INVOICE,llprnNots,Type,PAGEOF,PageNO) VALUES (lcInvcee,.T.,'ZZZZZZ1111',M.PageOf,M.PageNO+1)
        =lfNotePad(lcInvcee)
        lcInvcomp = lcInvcee
      ENDIF
    *B803854,1 BWA 11/21/2000 [END]

    ENDIF
  ENDSCAN

  *-- Case the notepad didn't finished and the line of the invoice finished.
  IF SEEK('C'+lcInvcee,'NOTEPAD')
    IF INVLINE.INVOICE # lcInvcee AND llFlgNote
      SCAN FOR llFlgNote
        lcInvcomp = lcInvcee
        M.PageNO  = M.PageNO + 1
        IF llFlagEnd
          M.Type = 'ZZZZ'
        ELSE
          M.Type = &lcTmpLin..TYPE
          FOR I = 1 TO 5
            IF I < 5
              INSERT INTO (lcTmpLin) (INVOICE,Type,PAGEOF,PageNO) VALUES (lcInvcee,M.Type,M.PageOf,M.PageNO)
            ELSE
              INSERT INTO (lcTmpLin) (INVOICE,ACCOUNT,Type,PAGEOF,PageNO) VALUES (lcInvcee,'P',M.Type,M.PageOf,M.PageNO)
            ENDIF
          ENDFOR
          M.Type = ALLTRIM(M.Type) + '1'
        ENDIF
        INSERT INTO (lcTmpLin) (INVOICE,llprnNots,Type,PAGEOF,PageNO) VALUES (lcInvcee,.T.,M.Type,M.PageOf,M.PageNO)
        =lfNotePad(lcInvcee)
       lcInvcomp = lcInvcee
       IF llFlagEnd AND llFlgNote
         M.Type = ALLTRIM(M.Type) + '1'
         FOR I = 1 TO 5
           IF I < 5
             INSERT INTO (lcTmpLin) (INVOICE,Type,PAGEOF,PageNO) VALUES (lcInvcee,M.Type,M.PageOf,M.PageNO)
           ELSE
             INSERT INTO (lcTmpLin) (INVOICE,ACCOUNT,Type,PAGEOF,PageNO) VALUES (lcInvcee,'P',M.Type,M.PageOf,M.PageNO)
           ENDIF
         ENDFOR
       ENDIF
      ENDSCAN
    ENDIF
  ENDIF

  *-- Case the invocie is change to another one,Initial variables used in notepad in the back.
  IF INVLINE.INVOICE # lcinvcee
    STORE 1  TO lnCounter , lnAftercnt
  ENDIF

ENDSCAN

               *-- End section of collecting the data --*

               *-- Section break the relatoins --*
SELECT InvHdr
SET RELATION OFF INTO (lcTmpDbt)
SET RELATION OFF INTO CUSTOMER
SET RELATION OFF INTO Ordhdr
SELECT (lcTmpDbt)
SET RELATION TO

SELECT InvLine
SET RELATION OFF INTO STYLE
SET RELATION OFF INTO SPCK_LIN

SELECT STYLE
SET RELATION OFF INTO SCALE
     *-- End section break the relatoins --*
     
     *-- Section set order to the files --* 
SELECT (lcTmpLin)
SET ORDER TO 1
     *-- End section set order to the files --* 

     *-- Section create the relatoins of the program --* 
SELECT (lcTmpLin)
IF !('IIF(.NOT.EMPTY(' $ SET('RELATION'))
  SET RELATION TO IIF(!EMPTY(&lcTmpLin..ALTSTYLE) , &lcTmpLin..ALTSTYLE ,&lcTmpLin..Style) INTO STYLE ADDITIVE
ENDIF  
IF !('INVOICE' $ SET('RELATION'))
  SET RELATION TO INVOICE INTO INVHDR ADDI
ENDIF

IF !('"S"+SCALE' $ SET('RELATION'))
  SET RELATION TO "S"+SCALE INTO SCALE ADDI
ENDIF

SELECT INVHDR
SET RELATION TO IIF(EMPTY(Store) OR Store = "********", 'M' + Account ,;
                    'S' + Account + Store) INTO CUSTOMER ADDITIVE
SET RELATION TO 'O' + INVHDR.ORDER INTO OrdHdr ADDITIVE
     *-- End section create the relatoins of the program --* 

SELECT (lcTmpLin)
GOTO TOP
DO gfDispRe WITH EVAL('lcFormName')
SET DEVICE TO SCREEN
llarpinv = .F.
WAIT CLEAR

*--Break the relation of the temp. file to make a new one if the user select preview many times.
SELECT (lcTmpLin)
SET RELATION OFF INTO STYLE
SET RELATION OFF INTO INVHDR
SET RELATION OFF INTO SCALE

*!*************************************************************
*! Name      : lfNotePad
*! Developer : Bassem Rafaat (BWA)
*! Date      : 10/09/2000
*! Purpose   : Function to Get the notepad and the optional message.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
*-- the page will hold 36 line for the notepad and 2 lines empty 
*-- and 6 lines for the optional message.

FUNCTION lfNotePad
PARAMETERS lcInvNote

*-lnFilCreat >> variable hold the No of the file of the notepad in the memory.
PRIVATE lcAlias , lnFilCreat , lnFileOpen
STORE "" TO laNote
STORE 1 TO lnNotLine , lnBegin , lnEnd
STORE 0 TO lnMemLins
lcAlias = ALIAS()

ERASE (gcWorkDir + 'NoteText.Txt')
lnFilCreat = FCREATE(gcWorkDir+'NoteText.Txt')
IF lnFilCreat > 0
  lnFileOpen = FOPEN(gcWorkDir+'NoteText.Txt',12)
ENDIF

IF lcInvcomp = lcInvNote
  lnCounter  = lnAftercnt + 1
  lnAftercnt = lnCounter
ENDIF

lnOldMemW = SET("MEMOWIDTH")
SET MEMOWIDTH TO 75
SELECT NOTEPAD
lnBegin = lnNotLine+(34*(lnCounter-1))
lnEnd = IIF(lnCounter > 1 , 34*lnCounter,34)

IF llRpInvNot
  lnMemLins = MEMLINES(NOTEPAD.MNOTES)
  DO WHILE lnBegin <= lnEnd
    laNote[IIF(lnCounter > 1,lnBegin-(34*(lnCounter-1)),lnBegin),1] = IIF(EMPTY(MLINE(NOTEPAD.MNOTES,lnBegin)),SPACE(75),MLINE(NOTEPAD.MNOTES,lnBegin))
    lnBegin = lnBegin + 1
  ENDDO

  lnNoLinNot = lnNoLinNot + 34
  IF lnNoLinNot < lnMemLins
    llFlgNote = .T.
  ELSE
    llFlgNote = .F.
  ENDIF
ELSE
  DO WHILE lnBegin <= lnEnd
    laNote[IIF(lnCounter > 1,lnBegin-(34*(lnCounter-1)),lnBegin),1] = REPLICATE(' ',75)
    lnBegin = lnBegin + 1
  ENDDO
  llFlgNote = .F.
ENDIF
SET MEMOWIDTH TO lnOldMemW
laNote[35,1] = REPLICATE(' ',75)
laNote[36,1] = REPLICATE(' ',75)

*B803793,1 BWA 11/07/2000 Remove the text of 'optional message'[START]
*laNote[37,1] = "Optional Message"
laNote[37,1] = REPLICATE(' ',75)
*B803793,1 [END]

laNote[38,1] = REPLICATE('.',20)

FOR I = 1 TO 6
  lcRpEe  = 'lcRpEe' + ALLTRIM(STR(I))
  laNote[38+I,1] = IIF(EMPTY(&lcRpEe),SPACE(75),&lcRpEe)
ENDFOR

FOR I = 1 TO 44
  =FPUTS(lnFilCreat,laNote[I,1])
ENDFOR
=FCLOSE(lnFilCreat)

SELECT (lcTmpLin)
APPEND MEMO MNOTES FROM (gcWorkDir+'NoteText.Txt')

*-- Case the invoice change and in the same time the notepad finished due to the flag [llFlgNote]
IF lcInvNote # INVLINE.INVOICE AND !llFlgNote
  STORE 1 TO   lnCounter , lnAftercnt
ENDIF

SELECT (lcAlias)
RETURN laNote
*-- End of lfNotePad.
*!*************************************************************
*! Name      : lfSolShp
*! Developer : Bassem Rafaat (BWA)
*! Date      : 10/09/2000
*! Purpose   : Function to Get the Sold to & Ship to Address
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfSolShp
PARAMETER lcSolShp

DECLARE laSoldTo[5,1] , laShipTo[5,1] 

laSoldTo = ''           && Array to hold the Sold To address
laShipTo = ''           && Array to hold the Ship To address

lcSolTName = CUSTOMER.BTName
laSoldTo[1] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 , '2')
laSoldTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 , '2')
laSoldTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 , '2')
laSoldTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4 , '2')
laSoldTo[5] = gfGetAdr('CUSTOMER' , '' , '' , '' , 5 , '2')

=lfAdrShift('laSoldTo')

IF OrdHdr.Alt_ShpTo
  lcShpTName  = OrdHdr.STName  
  laShipTo[1] = OrdHdr.cAddress1
  laShipTo[2] = OrdHdr.cAddress2
  laShipTo[3] = OrdHdr.cAddress3
  laShipTo[4] = OrdHdr.cAddress4
  laShipTo[5] = OrdHdr.cAddress5
ELSE 
  lcShpTName  = IIF( EMPTY(CUSTOMER.DBA) , CUSTOMER.STNAME , CUSTOMER.DBA)  
  laShipTo[1] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1)
  laShipTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2)
  laShipTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3)
  laShipTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4)
  laShipTo[5] = gfGetAdr('CUSTOMER' , '' , '' , '' , 5)
ENDIF 
=lfAdrShift('laShipTo')

RETURN ''
*-- End of lfSolShp.
*!*************************************************************
*! Name      : lfAdrShift
*! Developer : Bassem Rafaat (BWA)
*! Date      : 10/09/2000
*! Purpose   : Function to Shift the Address array if there is any
*!             empty lines in the address
*!*************************************************************
*! Called from : lfSolSpAdr()
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : Address Array name
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfAdrShift
PARAMETERS lcArrayNam

FOR lnCount = 1 TO 5
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") = "C" .AND.;
     EMPTY(&lcArrayNam.[lnCount])
    =ADEL(&lcArrayNam , lnCount)
    lnCount = lnCount - 1
  ENDIF
ENDFOR

FOR lnCount = 1 TO 5
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") <> "C"
    &lcArrayNam.[lnCount] = ''
  ENDIF
ENDFOR

*-- End of lfAdrShift.
*!*************************************************************
*! Name      : lfvMsgEE
*! Developer : Bassem Rafaat (BWA)
*! Date      : 10/09/2000
*! Purpose   : Function to get Optional Message from the User
*!             [Validation function for the Push button Optional Message]
*!*************************************************************
*! Called from : Option Grid    [Optional Message option]
*!*************************************************************
*! Calls       : gfOptMsg()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvMsgEE()
*!*************************************************************
FUNCTION lfvMsgEE
PARAMETER lcReturn
PRIVATE laMsgEe
DECLARE laMsgEe[6,2]       && Array to hold the name and length of the variables to be used in the Optional Message screen.

laMsgEe[1,1] = 'lcRpEe1'      && line Variable No 1
laMsgEe[1,2] = 60             && Line length
laMsgEe[2,1] = 'lcRpEe2'
laMsgEe[2,2] = 60
laMsgEe[3,1] = 'lcRpEe3'
laMsgEe[3,2] = 60
laMsgEe[4,1] = 'lcRpEe4'
laMsgEe[4,2] = 60
laMsgEe[5,1] = 'lcRpEe5'
laMsgEe[5,2] = 60
laMsgEe[6,1] = 'lcRpEe6'
laMsgEe[6,2] = 60

IF FILE(gcDataDir+lcMsgFile+'.MEM')
  RESTORE FROM gcDataDir+lcMsgFile+'.MEM' ADDITIVE
ENDIF

lcReturn= gfOptMsg('laMsgEe','Optional Message')    && Call Function to write Optional Message.
    
SET MEMOWIDTH TO 60
SAVE TO gcDataDir+lcMsgFile+'.MEM' ALL LIKE lcRpEe*

RETURN lcReturn
*-- End of lfvMsgEE.
*!*************************************************************
*! Name      : lfFirstTim
*! Developer : Bassem Rafaat (BWA)
*! Date      : 10/09/2000
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

STORE SPACE(0) TO lcInvoice
DECLARE laFileStru[1,1]

*-- lcMsgFile  && Variable that is hold the name of the Optional Message memo file
*-- lcMsgFile = 'OpsonMsg'
IF FILE(gcDataDir+lcMsgFile+'.MEM')
  RESTORE FROM gcDataDir+lcMsgFile+'.MEM' ADDITIVE
ENDIF

SELECT CODES
SET ORDER TO CODES IN CODES

*B803906,1 ASH 12/12/2000 (Begin) Comment out to get the bmp from syfrmcdd file.
*CREATE CURSOR (lcLogBmp) (gPic G)
*APPEND BLANK
*APPEND GENERAL gPIC FROM ( gcBmpHome + 'EELOGO.BMP')
*!B803906,1 ASH 12/12/2000 (End)

SELECT InvLine
=AFIELDS(laFileStru)

lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+7,4]

*-- Field for the notepad
laFileStru[lnFileStru+1,1] = 'mNotes'
laFileStru[lnFileStru+1,2] = 'M'
laFileStru[lnFileStru+1,3] = 10
laFileStru[lnFileStru+1,4] = 0

*-- Field to check if print notes or not.
laFileStru[lnFileStru+2,1] = 'llprnNots'
laFileStru[lnFileStru+2,2] = 'L'
laFileStru[lnFileStru+2,3] = 1
laFileStru[lnFileStru+2,4] = 0

*-- Field for the lineno of the real invoice.
laFileStru[lnFileStru+3,1] = 'lnlines'
laFileStru[lnFileStru+3,2] = 'N'
laFileStru[lnFileStru+3,3] = 6
laFileStru[lnFileStru+3,4] = 0

*-- Field for the note to use it in the index to get the note in the last record.
laFileStru[lnFileStru+4,1] = 'Type'
laFileStru[lnFileStru+4,2] = 'C'
laFileStru[lnFileStru+4,3] = 15
laFileStru[lnFileStru+4,4] = 0

*-- Field for the first part of the page no.
laFileStru[lnFileStru+5,1] = 'PageNo'
laFileStru[lnFileStru+5,2] = 'N'
laFileStru[lnFileStru+5,3] = 6
laFileStru[lnFileStru+5,4] = 0

*-- Field for the 2nd part of the page no.
laFileStru[lnFileStru+6,1] = 'PageOf'
laFileStru[lnFileStru+6,2] = 'N'
laFileStru[lnFileStru+6,3] = 6
laFileStru[lnFileStru+6,4] = 0

*-- Field to check if print notes or not.
laFileStru[lnFileStru+7,1] = 'llprnLin'
laFileStru[lnFileStru+7,2] = 'L'
laFileStru[lnFileStru+7,3] = 1
laFileStru[lnFileStru+7,4] = 0

CREATE TABLE (gcWorkDir+lcTmpLin) FROM ARRAY laFileStru
INDEX ON INVOICE + TYPE TAG (lcTmpLin)

RETURN ''
*-- End of lfFirstTim.
*!*************************************************************
*! Name      : gfOptMsg
*! Developer : Bassem Rafaat (BWA)
*! Date      : 10/09/2000
*! Purpose   : Function to view 6 lines in the optional screen.
*!*************************************************************
*! Called from : ARPINVEE.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = gfOptMsg()
*!*************************************************************
FUNCTION gfOptMsg
PARAMETERS lcArrayNam , lcTitle
PRIVATE laLine , lnRowNumb , lnLineWid , lnLineNo , lnMaxWid , lnLinePos , lnBoxWid

*IF The first parameter is not of type Character [The Array name] or
*The first element of the Array is not of type Character
IF TYPE('lcArrayNam') <> 'C' .OR. TYPE(lcArrayNam) <> 'C'
  RETURN
ENDIF

lcTitle = IIF(TYPE('lcTitle') <> 'C' .OR. EMPTY(lcTitle) ,'Optional message' , lcTitle)
lnLineWid = IIF(TYPE(lcArrayNam + '[1,2]') <> 'N' .OR.;
                EMPTY(&lcArrayNam.[1 , 2]) , 50 , &lcArrayNam.[1 , 2])    && Variable to hold the line length

lnMaxWid = lnLineWid          && Variable to hold the Max line length

*IF (lcArrayNam) is not an Array
IF TYPE(lcArrayNam + '[1,1]') = 'U'
  DECLARE laLine(1,2)
  laLine[1,1] = &lcArrayNam
  laLine[1,2] = REPLICATE('X' , lnLineWid)
ELSE

  *FOR Loop to scan the Array (lcArrayNam) [IF The Array is more than 6 rows
  * scan the First 6 only]
  FOR lnRowNumb = 1 TO MIN(6 , ALEN(&lcArrayNam , 1))
    
    *IF The First column of the current row is not of type Character
    * OR if it is empty OR its not holding the name of a Variable of type Character
    IF TYPE(lcArrayNam + '[' + STR(lnRowNumb , 1) + ',1]') <> 'C' .OR.;
       EMPTY(&lcArrayNam.[lnRowNumb , 1]) .OR.;
       TYPE(&lcArrayNam.[lnRowNumb , 1]) <> 'C'
      EXIT
    ELSE
      DIMENSION laLine(lnRowNumb , 2)
      laLine[lnRowNumb,1] = EVALUATE(&lcArrayNam.[lnRowNumb , 1])
      
      laLine[lnRowNumb,2] = IIF(TYPE(lcArrayNam + '[' + STR(lnRowNumb ,;
                                1) + ',3]') <> 'C' .OR.;
                                EMPTY(&lcArrayNam.[lnRowNumb , 3]) .OR.;
                                ALEN(&lcArrayNam , 2) < 3 ,;
                                REPLICATE('X' , lnLineWid) ,;
                                &lcArrayNam.[lnRowNumb , 3])
    
      lnMaxWid = MAX(lnMaxWid , IIF(TYPE(lcArrayNam + '[' + STR(lnRowNumb ,;
                     1) + ',2]') <> 'N' .OR. ALEN(&lcArrayNam , 2) < 2 ,;
                     0 , &lcArrayNam.[lnRowNumb , 2]))
    ENDIF
  ENDFOR
ENDIF

lnMaxWid = MIN(lnMaxWid , 58)
lnLineNo = ALEN(laLine , 1)
lnLineWid = lnMaxWid * 2
lnLinePos = ((615 - (lnLineWid * 5) - 3) / 8) / 2
lnBoxWid = (lnLineWid * 5 / 8) + 2
lnBoxHi = ((lnLineNo*14)+13-lnLineNo)/12

lcSOkBmp  = gcBmpHome + 'OK.BMP'           && Variable to hold the name and location of the OK BMP
lcSCanBmp = gcBmpHome + 'CAN.BMP'          && Variable to hold the name and location of the CANCEL BMP

DO (gcRepHome + gcAct_Appl + '\ARMSGEE.SPR')

*-- End of gfOptMsg.
*!*************************************************************
*! Name      : lfvOk
*! Developer : Bassem Rafaat (BWA)
*! Date      : 10/09/2000
*! Purpose   : Valid function of push button Ok.
*!*************************************************************
*! Called from : SCREEN ARMSGEE
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfvOk
PRIVATE lcVarName

IF TYPE(lcArrayNam + '[1,1]') = 'U'
  &lcArrayNam = laLine[1,1]
ELSE
  FOR lnRowNumb = 1 TO ALEN(laLine , 1)
    lcVarName = &lcArrayNam.[lnRowNumb , 1]
    &lcVarName = laLine[lnRowNumb , 1]
  ENDFOR
ENDIF

*-- End of lfvOk.
*!*************************************************************
*! Name        : lfRecNo
*! Developer   : Bassem Rafaat (BWA)
*! Date        : 10/09/2000
*! Purpose     : To get the Record count of the InvLine file.
*!             : To get the last No. of the invoice line.
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

*-- Case get the count of the invoice lines.
lnContLin = 0
lcAlias = ALIAS()
SELECT (lcInvLine_B)
=SEEK(InvHdr.Invoice,lcInvLine_B)
SCAN REST WHILE Invoice+STR(lineno,6) = InvHdr.Invoice
  lnContLin = lnContLin + 1
ENDSCAN

*-- Case get the last lineno of the invoice line.
SET ORDER TO TAG InvLine DESCENDING
=SEEK(InvHdr.Invoice,lcInvLine_B)
lnLincomp = IIF(EOF(lcInvLine_B) , 0 , EVAL(lcInvLine_B+'.LINENO'))
SET ORDER TO TAG InvLine ASCENDING

SELECT (lcAlias)
RETURN ''
*-- End of lfRecNo.

*!*************************************************************
*! Name        : lfGetBmp
*! Developer   : Ashraf Sherif (ASH)
*! Date        : 12/12/2000
*! Purpose     : Function to get bmp for the company
*!*************************************************************
*! Called from : syrepuvr
*!*************************************************************
*! Calls       : 
*!              Procedures : ....
*!              Functions  :
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return            : Null
*!*************************************************************
*! Example           : = lfGetBmp()
*!*************************************************************
FUNCTION lfGetBmp
PARAMETER lcDummy

=gfOpenFile(gcSysHome+'SYFRMCDD',gcSysHome+'FORMCDDT','SH')
=SEEK("ARPINV"+"EE")
RETURN ''
