FUNCTION gfCloseFile
   PARAMETER lcFileName
   FOR lnFilePos = 1 TO ALEN(laFileName,1)
      IF ALLTRIM(laFileName[lnFilePos,1]) == ALLTRIM(lcFileName)
         EXIT
      ENDIF
   ENDFOR
   IF lnFilePos > ALEN(laFileName,1)
      lnFilePos = 0
   ENDIF
   IF lnFilePos > 0
      FOR lnFileElm = 1 TO ALEN(gaMnu_Fl,1)
         IF ALLTRIM(gaMnu_Fl[lnFileElm,1]) == ALLTRIM(laFileName[lnFilePos ,1])
            EXIT
         ENDIF
      ENDFOR
      IF lnFileElm > ALEN(gaMnu_Fl,1)
         lnFileElm = 0
      ENDIF
      IF lnFileElm > 0
         lnRowNo = lnFileElm
         gaMnu_Fl[lnRowNo,4] = gaMnu_Fl[lnRowNo,4] - 1
         IF gaMnu_Fl[lnRowNo,5] ="P" .AND. USED(gaMnu_Fl[lnRowNo,1])
            USE IN (lcFileName)
         ENDIF
         IF gaMnu_Fl[lnRowNo,4]<=0 AND gaMnu_Fl[lnRowNo,5] ="P"
            =ADEL(gaMnu_Fl,lnRowNo)
            IF ALEN(gaMnu_Fl,1) > 1
               DECLARE gaMnu_Fl[ALEN(gaMnu_Fl,1) -1,ALEN(gaMnu_Fl,2)]
            ENDIF
         ENDIF
      ENDIF
      =gfAdel(@laFileName,lnFilePos)
   ENDIF

FUNCTION lfGetFile

   lcTxtFile =GETFILE('MS3','Select the MS3 file.')

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
   PARAMETERS lnSndPkQty, lcSndSkuNo, lclast_Pld
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
            LOCATE REST WHILE STYLE+DTOS(COMPLETE)+cordtype+ORDER+STORE+STR(LINENO,6) = ;
               lcStyle FOR EMPTY(Piktkt) .AND. Ordhdr.STATUS = 'O'
            IF FOUND()
               *-- Check if this record was picked before that.
               = lfUpdtPick ()
            ELSE
               *C127221,AAH,adjust new specs ,04/16/2006 [Begin]
               *-- Find the the earliest order
               =SEEK(lcStyle,'ORDLINE')
               SELECT ORDLINE
               llFound = .F.
               *SUSPEND 
               *SCAN REST WHILE STYLE+DTOS(COMPLETE)+cordtype+ORDER+STORE+STR(LINENO,6) = lcStyle
               SCAN REST WHILE STYLE = lcStyle
                  IF  PIK&lcSize < Qty&lcSize AND ORDHDR.STATUS # 'X'
                     llFound = .T.
                     EXIT
                  ENDIF
               ENDSCAN
               
               IF !llFound
                  SKIP -1
               ENDIF
               
               SCATTER MEMVAR MEMO
               SELECT (lcOrdline)
               APPEND BLANK
               GATHER MEMVAR MEMO
               REPLACE Qty&lcSize WITH Qty&lcSize - lnSndPkQty,Book&lcSize WITH Book&lcSize-lnSndPkQty,Piktkt  WITH 'XXXXXX'

               APPEND BLANK
               GATHER MEMVAR MEMO
               REPLACE Qty&lcSize WITH lnSndPkQty,Book&lcSize WITH lnSndPkQty,PIK&lcSize WITH lnSndPkQty ,Piktkt  WITH '',;
                  TotPik     WITH Pik1+Pik2+Pik3+Pik4+Pik5+Pik6+Pik7+Pik8 ,;
                  cLastpldNo WITH lclast_Pld,LINENO WITH Ordhdr.lastline+1

               *-- Send Erro to Erro File in case no ordr open for cuuernt style.
               *lcError = 'There is no order open for style# / Sku# :'+ lcStyle +' / '+ lcSndSkuNo +' .'
               *= lfErrLog (lcError)

               *C127221,AAH,adjust new specs ,04/16/2006 [End]


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

   FOR I = 1 TO ALEN(laOpenFile,1)
      IF !USED(laOpenFile[I,1])
         laOpenFile[I,3] =gfOpenFile(gcDataDir+laOpenFile[I,1],laOpenFile[I,2], "SH")
      ENDIF
   ENDFOR


   SELECT ORDLINE
   *C127221,AAH, AS PER SPACS AND AS PER PMEDI ,05/02/2006 [BEGIN]
   *SET ORDER TO Ordlines DESC
   SET ORDER TO Ordlines
   *C127221,AAH, AS PER SPACS AND AS PER PMEDI ,05/02/2006 [END]
   SET RELATION TO cordtype+ORDER INTO Ordhdr

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
      FOR I = 1 TO ALEN(laOpenFile,1)
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
      lnFilHandl = FCREAT(lcWorkPath+'ErrLog.txt')
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
      TYPE  gcWorkDir+'ErrLog.txt' TO PRINTER
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
   IF SEEK(ORDLINE.STYLE+ORDLINE.ORDER+STR(ORDLINE.LINENO,6),lcOrdline)
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
      =SEEK(ORDLINE.STYLE+ORDLINE.ORDER+STR(ORDLINE.LINENO,6),lcOrdline)
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

   SCAN REST WHILE STYLE+DTOS(COMPLETE)+cordtype+ORDER+STORE+STR(LINENO,6) = ;
         lcStyle FOR EMPTY(Piktkt) .AND. Ordhdr.STATUS = 'O'

      IF SEEK(ORDLINE.STYLE+ORDLINE.ORDER+STR(ORDLINE.LINENO,6),lcOrdline)
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
   PARAMETERS lcSndPckId
   PRIVATE lnAlias , llgetStyle
   llgetStyle = .F.

   IF TYPE('lcSndPckId') = 'U'
      RETURN
   ENDIF
   lnAlias = SELECT(0)

   SELECT spck_lin
   IF SEEK('S')
      SCAN REST WHILE TYPE+account+pack_id = 'S' FOR lcSndPckId $ ALLTRIM(pack_id)
         lcStyle = spck_lin.STYLE
         FOR I = 1 TO 8
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
   STORE .F. TO  lUpdOrdQty
   STORE ''  TO lcStart
   STORE 0 TO lnStart , laOverPik , lnDiffernt, lnTotQty
   lnAlias = SELECT (0)

   *-- Function to Generate piktkt for the order lines.
   = lfGenPiktk ()

   SELECT ORDLINE
   SET RELATION TO
   SET ORDER TO TAG Ordlines  ASCENDING

   SELECT (lcOrdline)
   GO TOP 
   *SUSPEND
   llopenstatus = .F.
   SCAN
      SCATTER MEMVAR MEMO
      

      *-- Index key into Order line file.
      *--style+DTOS(complete)+cordtype+order+store+STR(lineno,6)
      IF !SEEK(M.Style+ DTOS(M.Complete) + M.cordtype+ M.order+M.store+STR(M.lineno,6),'ORDLINE')
         SELECT ORDLINE
         APPEND BLANK
         GATHER MEMVAR MEMO
      ELSE
      ENDIF
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
         Piktkt  WITH IIF(EMPTY(Piktkt),M.Piktkt,Piktkt) ,;
         PikDate WITH M.PikDate,;
         Picked  WITH .T.
      *C127221,AAH,adjust new specs ,04/16/2006 [Begin]
      IF &lcOrdline..Piktkt = 'XXXXXX'
         REPLACE Qty1    WITH M.Qty1   ,;
            Qty2    WITH M.Qty2  ,;
            Qty3    WITH M.Qty3   ,;
            Qty4    WITH M.Qty4  ,;
            Qty5    WITH M.Qty5  ,;
            Qty6    WITH M.Qty6   ,;
            Qty7    WITH M.Qty7   ,;
            Qty8    WITH M.Qty8   ,;
            TotQty  WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
         UNLOCK
         llopenstatus = .T.
      ELSE
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
            IF PIK&lcStart > Qty&lcStart
               *XXXX
               lUpdOrdQty = .T.
               EXIT
            ENDIF

         ENDFOR
         *-- Update the Open And Book Qty in case over allocate.
         IF lUpdOrdQty
            lnOvrAlQty = TotPik - TotQty
            lnTotQty   = Qty1 + Qty2 + Qty3 + Qty4 + Qty5 + Qty6 + Qty7 + Qty8
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
         ENDIF
         *C127221,AAH,adjust new specs ,04/16/2006 [End]

         *-- Update the order header file.
         lnDiffernt = MAX(TotQty - lnTotQty,0 )
         SELECT Ordhdr
         IF SEEK(M.cordtype+ M.order)
            =RLOCK()
            REPLACE Book    WITH Book    + lnDiffernt            ,;
               OPEN    WITH OPEN    + lnDiffernt            ,;
               BookAmt WITH BookAmt + (lnDiffernt * M.Price),;
               OpenAmt WITH OpenAmt + (lnDiffernt * M.Price)
		
              IF  llopenstatus AND ordhdr.status = 'C' 
				REPLACE status WITH "O"                
              ENDIF 
 
         ENDIF
         UNLOCK
      ENDIF
      *ENDIF

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
   *C127221,AAH, AS PER SPACS AND AS PER PMEDI ,05/02/2006 [BEGIN]
   *SET ORDER TO Ordlines DESC
   SET ORDER TO Ordlines
   *C127221,AAH, AS PER SPACS AND AS PER PMEDI ,05/02/2006 [END]
   SET RELATION TO cordtype+ORDER INTO Ordhdr

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
      IF lcChgPkTKt <> m.order + m.store + m.cWareCode
         lcChgPkTKt = m.order + m.store + m.cWareCode
         lcPikTktNo = lfGetPkTkt(m.order, Ordhdr.cDivision, m.store, m.cWareCode)
         SELECT Piktkt

         APPEN BLANK
         REPLACE Piktkt    WITH lcPikTktNo ,;
            account   WITH M.account  ,;
            STORE     WITH M.store    ,;
            ORDER     WITH M.order    ,;
            DATE      WITH gdSysDate  ,;
            cWareCode WITH M.cWareCode,;
            STATUS    WITH 'O'        ,;
            cAdd_User WITH gcUser_ID  ,;
            dAdd_Date WITH DATE()     ,;
            cAdd_Time WITH gfGetTime(),;
            CADD_VER  WITH "A27"
      ENDIF

      SELECT (lcOrdline)
      *C127221,AAH,adjust new specs ,04/16/2006 [Begin]
      *!*REPLACE Piktkt  WITH lcPikTktNo ,;
      *!*   PikDate WITH gdSysDate
      REPLACE Piktkt  WITH IIF(Piktkt='XXXXXX',Piktkt,lcPikTktNo) ,;
         PikDate WITH gdSysDate
      *C127221,AAH,adjust new specs ,04/16/2006 [End]

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
FUNCTION lfGetPkTkt

   *E300676,1 Change this line to add 2 new parameters for the store and the
   *warecode [Begin]
   *PARAMETERS lcOrdNo , lcDivision
   *PARAMETERS lcOrdNo , lcDivision , lcStore , lcWareCode
   *E300676,1 Change this line to add 2 new parameters [End]

   *E300989,1 Change the following line adding new parameter for skip ask [begin]
   *E300989,1 user for generate new pick ticket.
   *PARAMETERS lcOrdNo , lcDivision , lcStore , lcWareCode
   PARAMETERS lcOrdNo , lcDivision , lcStore , lcWareCode , lnAskType
   *E300989,1 Change the following line adding new parameter for skip ask [end]

   *E300989,1 Assure that it's defaulted to ask avoiding problems by unpassed parameter.
   IF TYPE('lnAskType') $ 'UL'
      lnAskType = 0
   ENDIF
   *E300989,1 Assure that [end]

   PRIVATE lcOrdNo   , lcDivision , lcCurAlias , lcPikTkt , ;
      llOpnPkTk , lnOldTag   , lnRecPkTk  , laPikNo  , lcExact , llAskUser

   llAskUser = (lnAskType = 0)
   *-- Save the exact setting.
   lcExact = SET("EXACT")

   *-- Define array hold the piktkts no. , status , printed or not.
   DECLARE laPikNo[1]

   *-- Save the current alias.
   lnCurAlias = SELECT()

   *-- Initialize the needed variables.
   STORE ""  TO laPikNo , lcPikTkt
   STORE .F. TO llOpnPkTk
   STORE 0   TO lnOldTag   , lnRecPkTk

   *-- Open the needed files.
   *IF !USED('PikTkt')
   *-- Open the file if it was not open.
   *  =gfOpenFile(QDD+'PikTkt',QDD+'OrdPik','SH')
   *  llOpnPkTk = .T.
   *ELSE
   SELECT Piktkt
   lnOldTag = INT(VAL(SYS(21)))
   SET ORDER TO TAG OrdPik
   *-- Save the current record no. if the file was open.
   lnRecPkTk = RECNO("PikTkt")
   *ENDIF

   SELECT Piktkt
   *E300511,1 Reham On 07/23/97  *** Begin ***
   *IF SEEK(lcOrdNo , "PikTkt")
   *RENEE
   *IF SEEK(lcOrdNo , "PikTkt") .AND. ;
   !(PikTkt.Status $ "CX") .AND. ;
   Store = lcStore .AND. cWareCode = lcWareCode
   IF SEEK(lcOrdNo , "PikTkt")
      *B606788,1 Reham On 12/18/2002  *** Begin ***
      *B606788,1 Do not include the "******" piktkt in the piktkt list
      *LOCATE REST WHILE Order = lcOrdNo;
      *       FOR !(PikTkt.Status $ "CX") .AND. ;
      *       Store = lcStore .AND. cWareCode = lcWareCode
      LOCATE REST WHILE ORDER = lcOrdNo;
         FOR !(Piktkt.STATUS $ "CX") .AND. ;
         STORE = lcStore .AND. cWareCode = lcWareCode AND !("*" $ Piktkt)
      *B606788,1 Reham On 12/18/2002  *** End   ***
      IF FOUND()
         *RENEE end
         *E300511,1 Reham On 07/23/97  *** End   ***

         *-- If there was any piktkts created for any of the
         *-- current order lines.
         *E300511,1 Reham On 07/23/97  *** Begin ***
         *E300511,1 Comment the if condition to add it to the seek condition.
         *E300676,1 Add this IF statment to check the Store and the Warecode [Begin]
         *IF Store = lcStore .AND. cWareCode = lcWareCode
         *E300676,1 Add this IF statment to check the Store and the Warecode [End]
         *E300511,1 Reham On 07/23/97  *** END   ***
         laPikNo[ALEN(laPikNo,1)] = Piktkt.Piktkt + " " + ;
            IIF(Piktkt.prtflag="P","Yes","No ") + SPACE(5) + ;
            IIF(Piktkt.STATUS $ "O ","Open   ",IIF(Piktkt.STATUS="H" , "On hold" , SPACE(7))) + ;
            " " + DTOC(Piktkt.DATE)

         *E300511,1 Reham On 07/23/97  *** Begin ***
         *E300676,1 Add ENDIF for the IF statment to check the Store and the
         *Warecode [Begin]
         *ENDIF    && End of IF
         *E300676,1 Add ENDIF for the IF statment [End]
         *E300511,1 Reham On 07/23/97  *** Begin ***

         SKIP
         IF EOF()
            SKIP -1
         ENDIF

         *E300676,1 Change this SCAN loop to check the Store and the Warecode [Begin]
         *SCAN REST WHILE PikTkt.Order + PikTkt.PikTkt = lcOrdNo FOR !(PikTkt.Status $ "CX")
         *B606788,1 Reham On 12/18/2002  *** Begin ***
         *B606788,1 Do not include the "******" piktkt in the piktkt list
         *SCAN REST WHILE PikTkt.Order + PikTkt.PikTkt = lcOrdNo ;
         *            FOR !(PikTkt.Status $ "CX") .AND. Store = lcStore .AND. cWareCode = lcWareCode
         SCAN REST WHILE Piktkt.ORDER + Piktkt.Piktkt = lcOrdNo ;
               FOR !(Piktkt.STATUS $ "CX") .AND. STORE = lcStore .AND. cWareCode = lcWareCode AND !("*" $ Piktkt)
            *B606788,1 Reham On 12/18/2002  *** End   ***

            *E300676,1 Change this SCAN loop to check the Store and the Warecode [End]

            *-- Set excat to off.
            SET EXACT OFF
            IF ASCAN(laPikNo , Piktkt.Piktkt) = 0

               *E300676,1 Add this IF statment to check if the last element is not
               *empty
               IF !EMPTY(laPikNo[ALEN(laPikNo,1)])
                  *E300676,1 Add this IF statment [End]

                  DIMENSION laPikNo[ALEN(laPikNo,1)+1]

                  *E300676,1 Add ENDIF for the IF statment to check if the last element
                  *is not empty
               ENDIF    && End of IF
               *E300676,1 Add ENDIF for the IF statment [End]

               laPikNo[ALEN(laPikNo,1)] = Piktkt.Piktkt + " " + ;
                  IIF(Piktkt.prtflag="P","Yes","No ") + SPACE(5) + ;
                  IIF(Piktkt.STATUS $ "O ","Open   ",IIF(Piktkt.STATUS="H" , "On hold" , SPACE(7))) + ;
                  " " + DTOC(Piktkt.DATE)
            ENDIF
            *-- Restore the excat setting.
            SET EXACT &lcExact
         ENDSCAN

         IF ALEN(laPikNo,1) > 1
            lsPikLst = 1

            *E300989,1 if user want to generate new from passed parameter [begin]
            *E300989,1 we does not ask him and always generate new.
            *DO ALPIKLST.SPR
            *C127221,AAH,adjust new specs ,04/16/2006 [Begin]
            llAskUser = .F.
               *C127221,AAH,adjust new specs ,04/16/2006 [End]
            IF llAskUser
               DO (gcScrDir + 'AL' + '\ALPIKLST.SPX')
            ELSE && Add new pick tickt.
               lcPikTkt = gfSequence('PIKTKT', '', '', lcDivision)
            ENDIF
            *E300989,1 if user want to generate new from passed parameter [end]

         ELSE
            *Reham On 17/12/97  ** Begin **
            *IF SUBSTR(laPikNo[1],8,1) = "P"

            *E300676,1 Change this line to change gfDialog() to gfModalGen() [Begin]
            *IF gfDialog("?" , "There is a printed picking ticket","\<Add;\<New") = 1
            *** There is Picking ticket no. : {laPikNo[1]}.     ***
            *** Do you wish to assign a new picking ticket no.? ***
            *** < Add > - < New > ***
            *IF gfModalGen("TRM44011B44000","DIALOG") = 1

            *E300989,1 Add the following lines to skip ask  [begin]
            *E300989,1 if passed asktype is not zero, and change gfmod to be in variable.

            lnAskType = IIF(lnAskType=0,gfModalGen("TRM44011B44000","DIALOG" , SUBSTR(laPikNo[1],1,6)),lnAskType)

            *E300989,1 Change the following line to If lnAskType instead of gfModalGen.
            *IF gfModalGen("TRM44011B44000","DIALOG" , SUBSTR(laPikNo[1],1,6)) = 1
            IF lnAskType = 1
               *E300676,1 Change this line to change gfDialog() to gfModalGen() [End]
               *E300989,1 Add the following lines to skip ask  [end]

               lcPikTkt = SUBSTR(laPikNo[1],1,6)
            ELSE
               *lcPikTkT = Sequence('PikTkt',lcDivision)
               lcPikTkt = gfSequence('PIKTKT', '', '', lcDivision)
            ENDIF
            *ELSE
            *  lcPikTkT = SUBSTR(laPikNo[1],1,6)
            *ENDIF
            *Reham On 17/12/97  ** End   **
         ENDIF
         *RENEE endif
      ELSE
         *-- If the order no. not found in the PikTkt file, create new piktkt.
         *lcPikTkT = Sequence('PikTkt' , lcDivision)
         lcPikTkt = gfSequence('PIKTKT', '', '', lcDivision)
      ENDIF
      *RENEE endif end
   ELSE
      *-- If the order no. not found in the PikTkt file, create new piktkt.
      *lcPikTkT = Sequence('PikTkt' , lcDivision)
      lcPikTkt = gfSequence('PIKTKT', '', '', lcDivision)
   ENDIF

   *IF llOpnPkTk
   *-- If the file was opened in this session, close it.
   *  USE IN PikTkt
   *ELSE
   SELECT Piktkt
   SET ORDER TO lnOldTag
   *-- File was opened in previous session, Set the opinter to th right record.
   IF lnRecPkTk > 0 .AND. lnRecPkTk <= RECCOUNT("PikTkt")
      GOTO lnRecPkTk
   ENDIF
   *ENDIF

   *-- Restore the exact setting.
   SET EXACT &lcExact

   *-- Restore the old alias.
   SELECT (lnCurAlias)

   *-- Return with the selected picking ticket no.
   RETURN lcPikTkt
   *-- end of lfGetPkTkt. GFMOD
FUNCTION gfSequence

   *E300894,1 06/18/98 YMA Add an optional parameter to be used to
   *E300894,1              get the sequence propirities if the original
   *E300894,1              requested sequence has no associated field.
   *PARAMETERS lcSeqType,lcCompanyId,lcGroupId,lcDivision
   PARAMETERS lcSeqType,lcCompanyId,lcGroupId,lcDivision,lcField

   *E300894,1 06/18/98 YMA End.
   *B603586,1 SSH 29/02/00 Add new Private variables .
   *PRIVATE lnRetVal,lcSavAlias,lcDataDir
   PRIVATE lnRetVal,lcSavAlias,lcDataDir, lnOldGenNm,lcExtraStr,lcToFind,lcKeyExp
   *B603586,1 SSH 29/02/00 (End)
   *E300894,1 06/18/98 YMA Validate the optional passed parameter.
   lcField    = IIF(TYPE("lcField")="C", ALLTRIM(UPPER(lcField)), SPACE(0))
   *E300894,1 06/18/98 YMA End.

   lcSavAlias = SELECT(0)
   lcSeqType  = UPPER(lcSeqType)
   lcDataDir  = gcDataDir

   *E300888 06/04/98 YMA If the communication module is installed, then
   *E300888              get the unique site prefix for the active site
   *E300888              from the sites file.
   lcUnqPreFx = SPACE(0)
   IF "CM" $ gcComp_Mdl
      USE (gcSysHome+"SYCSITES") IN 0 AGAIN ALIAS Sites ORDER cSiteID
      lcUnqPreFx = IIF(SEEK(gcCurSite, "Sites"), Sites.cUniqStPre, lcUnqPreFx)
      USE IN Sites
   ENDIF
   *E300888 06/04/98 YMA End.

   IF TYPE('lcCompanyId')='C' AND !EMPTY(lcCompanyId) AND lcCompanyId <> gcAct_Comp
      IF USED("sycComp")
         SELECT sycComp
         luSycComp = .F.
         ltSycComp = VAL(SYS(21))
         leSycComp = RECNO()
         SET ORDER TO TAG cComp_Id IN sycComp
      ELSE
         luSycComp = .T.
         USE (gcSysHome+"syccomp") ORDER TAG cComp_Id IN 0
      ENDIF

      IF SEEK(lcCompanyId,'syccomp')
         *E301098,1 Hesham (Start)
         *lcDataDir = ALLTRIM(syccomp.cCom_dDir)
         *IF UPPER(SUBSTR(gcOrgPath,1,ATC('\',gcOrgPath,2))) = ;
         *   UPPER(SUBSTR(lcDataDir,1,ATC('\',lcDataDir,2))) AND ;
         *   UPPER(SUBSTR(gcOrgPath,1,ATC('\',gcOrgPath,2))) <>  ;
         *   UPPER(SUBSTR(gcSysHome,1,ATC('\',gcSysHome,2)))
         *  lcDataDir= SUBSTR(gcSysHome,1,ATC('\',gcSysHome,2))+;
         *             SUBSTR(lcDataDir,ATC('\',lcDataDir,2)+1)
         *ENDIF
         lcDataDir = gfGetDataDir(ALLTRIM(sycComp.cCom_dDir))
         *E301098,1 Hesham (End)
      ENDIF

      IF luSycComp
         USE IN sycComp
      ELSE
         SET ORDER TO TAG ltSycComp IN sycComp
         IF BETWEEN(leSycComp,1,RECCOUNT('syccomp'))
            GOTO leSycComp IN 'syccomp'
         ENDIF
      ENDIF
   ENDIF

   *E301046,4 Assure that lcGroupId is 3 Char. only
   *lcGroupId  = IIF(TYPE('lcGroupId') ='C', ALLTRIM(lcGroupId) , SPACE(2))
   lcGroupId  = IIF(TYPE('lcGroupId') ='C' , PADR(lcGroupId,3)  , SPACE(2))
   *E301046,4 end
   lcDivision = IIF(TYPE('lcDivision')='C',ALLTRIM(lcDivision),SPACE(10))
   lnRetVal   = 0

   *300632,1 Get division sequence group
   *B802982,1 [start] Don't GET the GroupID if the system is not set to
   *                  generate seq.# based on division
   llDivOnSeq = gfgetMemvar('M_DIV_SEQ' , lcCompanyId) = 'Y'
   *Change this line to check the llDivOnSeq
   *IF EMPTY(lcGroupId) .AND. !EMPTY(lcDivision)
   IF llDivOnSeq AND EMPTY(lcGroupId) .AND. !EMPTY(lcDivision)
      *B802982,1 [End]
      DECLARE laDivDlt[1,2]
      laDivDlt[1,1] = 'DIVGROUP'
      laDivDlt[1,2] = 'lcGroupId'
      =gfRltFld(PADR(lcDivision,6),@laDivDlt,'CDIVISION')
      *E301046,4 Change lcGroupId to be 3 Char. only
      *lcGroupId = SUBSTR(lcGroupId,1,10)
      lcGroupId = SUBSTR(lcGroupId,1,3)
      *E301046,4 end
   ENDIF
   *B802982,1 [start] make sure the group id is empty if the system
   *                  is not set to generate seq.# based on division
   *                  This case will BE FEASABLE ONLY
   *                  IF llDivOnSeq = .F.
   *                  AND !EMPTY(lcGroupId)
   lcGroupId = IIF(llDivOnSeq , SUBSTR(lcGroupId,1,3) , SPACE(3))
   *B802982,1 [End]
   IF !USED('SEQUENCE')
      luSequence = .T.
      USE &lcDataDir.SEQUENCE IN 0 ORDER TAG 'cSeq_Type'
   ELSE
      SELECT SEQUENCE
      luSequence = .F.
      ltSequence = VAL(SYS(21))
      leSequence = RECNO()
      SET ORDER TO TAG Cseq_type IN SEQUENCE
   ENDIF

   IF !SEEK(PADR(lcSeqType,10)+lcGroupId,'SEQUENCE')
      IF !USED('sydflfld')
         luSydflfld = .T.
         USE &gcSysHome.sydflfld ORDER TAG 'Cfld_name' IN 0 SHARED
      ELSE
         SELECT sydflfld
         luSydflfld = .F.
         ltSydflfld = VAL(SYS(21))
         leSydflfld = RECNO()
         SET ORDER TO TAG Cfld_name IN 'sydflfld'
      ENDIF
      IF !USED('sydfield')
         luSydfield = .T.
         USE &gcSysHome.sydfield ORDER TAG 'Cfld_name' IN 0 SHARED
      ELSE
         SELECT sydfield
         luSydfield = .F.
         ltSydfield = VAL(SYS(21))
         leSydfield  = RECNO()
         SET ORDER TO TAG Cfld_name IN 'sydfield'
      ENDIF

      *E300894,1 06/18/98 YMA Use the optional field to get the sequence
      *E300894,1              proprities instead of the sequence field
      *E300894,1              if any.
      lcPropFld = IIF(EMPTY(lcField), lcSeqType, lcField)
      = SEEK(PADR(lcPropFld,10),'sydfield')
      SELECT sydflfld
      = SEEK(PADR(lcPropFld,10))
      LOCATE REST WHILE Cfld_name=PADR(lcPropFld,10) FOR lEnumerate

      *=SEEK(PADR(lcSeqType,10),'sydfield')
      *SELECT sydflfld
      *=SEEK(PADR(lcSeqType,10))
      *LOCATE REST WHILE cFld_Name=PADR(lcSeqType,10) FOR lEnumerate
      *E300894,1 06/18/98 YMA End.

      lnDefSeq = sydflfld.nDef_Seq
      IF !EMPTY(lcGroupId) AND SEEK(PADR(lcSeqType,10),'SEQUENCE')
         SELECT SEQUENCE
         lnDefSeq = 0
         SCAN REST WHILE Cseq_type+cseq_group = PADR(lcSeqType,10)
            lnDefSeq = MAX(lnDefSeq,nSeq_No)
         ENDSCAN
         lnDefSeq = (INT(lnDefSeq/50000)+1)*50000
      ENDIF

      INSERT INTO SEQUENCE (Cseq_type,nSeq_No,cseq_group,cData_Typ,nFld_Wdth) ;
         VALUES (lcSeqType,lnDefSeq,lcGroupId,sydfield.cData_Typ,;
         sydfield.nFld_Wdth)
      IF sydflfld.lEnumerate
         IF !USED('sydfiles')
            luSydfiles = .T.
            USE &gcSysHome.sydfiles ORDER TAG 'Cfile_nam' IN 0 SHARED
         ELSE
            SELECT sydfiles
            luSydfiles = .F.
            ltSydfiles = VAL(SYS(21))
            leSydfiles = RECNO()
            SET ORDER TO TAG Cfile_nam IN 'sydfiles'
         ENDIF
         =SEEK(sydflfld.Cfile_nam,'sydfiles')
         SELECT SEQUENCE
         REPLACE Cfile_nam WITH sydfiles.Cfile_nam ,;
            cFile_Tag WITH sydfiles.cFile_Tag
         IF luSydfiles
            USE IN sydfiles
         ELSE
            SET ORDER TO TAG ltSydfiles IN sydfiles
            IF BETWEEN(leSydfiles,1,RECCOUNT('Sydfiles'))
               GOTO leSydfiles IN 'Sydfiles'
            ENDIF
         ENDIF
      ENDIF
      IF luSydflfld
         USE IN sydflfld
      ELSE
         SET ORDER TO TAG ltSydflfld IN sydflfld
         IF BETWEEN(leSydflfld,1,RECCOUNT('Sydflfld'))
            GOTO leSydflfld IN 'Sydflfld'
         ENDIF
      ENDIF
      IF luSydfield
         USE IN sydfield
      ELSE
         SET ORDER TO TAG ltSydfield IN sydfield
         IF BETWEEN(leSydfield,1,RECCOUNT('Sydfield'))
            GOTO leSydfield IN 'Sydfield'
         ENDIF
      ENDIF
   ENDIF
   *--MAN Added RLOCK Condition[Start]
   DO WHILE !RLOCK("SEQUENCE")
   ENDDO
   lnRetVal   = SEQUENCE.nSeq_No
   *--MAN Added RLOCK Condition[End]

   *E300888 06/04/98 YMA Compute the required code width assuming that
   *E300888              the minemum code field width = 6.
   lnRetLen = SEQUENCE.nFld_Wdth - LEN(lcUnqPreFx)
   *E300888 06/04/98 YMA End.
   *B603586,1 SSH 29/02/00  (Begin) Use the new field (cSeq_Chr) from sequence file.
   lnOldGenNm = SEQUENCE.nSeq_No
   lcExtraStr = ''
   IF !EMPTY(SEQUENCE.cSeq_Chr)
      lcExtraStr = SEQUENCE.cSeq_Chr
   ENDIF
   *B603586,1 SSH 29/02/00 (End)

   IF !EMPTY(SEQUENCE.Cfile_nam) .AND. !EMPTY(SEQUENCE.cFile_Tag)
      lcSeqFile = ALLTRIM(SEQUENCE.Cfile_nam)
      lcSeqTag  = ALLTRIM(SEQUENCE.cFile_Tag)
      IF !USED(lcSeqFile)
         luSeqFile = .T.
         *B601946,1 Use the file again to prevent 'File is in use' message
         *USE &gcDataDir.&lcSeqFile ORDER TAG (lcSeqTag) IN 0 SHARED
         USE &gcDataDir.&lcSeqFile AGAIN ORDER TAG (lcSeqTag) IN 0 SHARED
         *B601946,1 end
      ELSE
         SELECT (lcSeqFile)
         luSeqFile = .F.
         ltSeqFile = VAL(SYS(21))
         leSeqFile = RECNO()
         SET ORDER TO TAG (lcSeqTag) IN (lcSeqFile)
      ENDIF
      SELECT (lcSeqFile)
      lcKeyField = SUBSTR(KEY(),1,AT('+'+lcSeqType,KEY())-1)
      DECLARE laVldEnt[1]

      IF !EMPTY(lcKeyField) .AND. gfGetVld(lcKeyField,@laVldEnt) > 0
         FOR lnCount = 1 TO ALEN(laVldEnt,1)
            *E300888 06/04/98 YMA Search for the generated code prefixed with
            *E300888              the unique site prefix.
            *DO WHILE SEEK(laVldEnt[lnCount,2]+PADL(lnRetVal,SEQUENCE.nFld_Wdth,'0'),lcSeqFile)
            *B603586,1 SSH 29/02/00  (Begin) Use the new field (cSeq_Chr) from sequence file.
            lcKeyExp = IIF(EMPTY(lcExtraStr),PADL(lnRetVal,lnRetLen,"0"),;
               ALLTRIM(lcExtraStr)+PADL(lnRetVal,lnRetLen-1,"0"))
            *DO WHILE SEEK(laVldEnt[lnCount,2]+lcUnqPreFx+PADL(lnRetVal,lnRetLen,"0"),lcSeqFile)
            DO WHILE SEEK(laVldEnt[lnCount,2]+lcUnqPreFx+lcKeyExp,lcSeqFile)
               *B603586,1 SSH 29/02/00  (End)
               *E300888 06/04/98 YMA End.
               lnRetVal = lnRetVal + 1
               *B603586,1 SSH 29/02/00  (Begin) Use the new field (cSeq_Chr) from sequence file.
               IF !EMPTY(lcExtraStr) .AND. lnRetVal > 99999999
                  lcExtraStr = CHR(ASC(lcExtraStr)+1)
               ENDIF
               lcKeyExp = IIF(EMPTY(lcExtraStr),PADL(lnRetVal,lnRetLen,"0"),;
                  ALLTRIM(lcExtraStr)+PADL(lnRetVal,lnRetLen-1,"0"))
               *B603586,1 SSH 29/02/00  (End)
            ENDDO
         ENDFOR
      ELSE
         *E300888 06/04/98 YMA Search for the generated code prefixed with
         *E300888              the unique site prefix.
         *B603586,1 SSH 29/02/00  (Begin) Use the new field (cSeq_Chr) from sequence file.
         lcKeyExp = IIF(EMPTY(lcExtraStr),PADL(lnRetVal,lnRetLen,"0"),;
            ALLTRIM(lcExtraStr)+PADL(lnRetVal,lnRetLen-1,"0"))
         *DO WHILE SEEK(PADL(lnRetVal,lnRetLen,'0'),lcSeqFile)
         DO WHILE SEEK(lcKeyExp,lcSeqFile)
            *B603586,1 SSH 29/02/00  (End)
            *DO WHILE SEEK(PADL(lnRetVal,SEQUENCE.nFld_Wdth,'0'),lcSeqFile)
            *E300888 06/04/98 YMA End.
            lnRetVal = lnRetVal + 1
            *B603586,1 SSH 29/02/00  (Begin) Use the new field (cSeq_Chr) from sequence file.
            IF !EMPTY(lcExtraStr) .AND. lnRetVal > 99999999
               lcExtraStr = CHR(ASC(lcExtraStr)+1)
            ENDIF
            lcKeyExp = IIF(EMPTY(lcExtraStr),PADL(lnRetVal,lnRetLen,"0"),;
               ALLTRIM(lcExtraStr)+PADL(lnRetVal,lnRetLen-1,"0"))
            *B603586,1 SSH 29/02/00  (End)
         ENDDO
      ENDIF

      IF luSeqFile
         USE IN (lcSeqFile)
      ELSE
         SET ORDER TO TAG ltSeqFile IN (lcSeqFile)
         IF BETWEEN(leSeqFile,1,RECCOUNT(lcSeqFile))
            GOTO leSeqFile IN (lcSeqFile)
         ENDIF
      ENDIF
   ENDIF
   SELECT SEQUENCE
   *B603586,1 SSH 29/02/00  (Begin) Check if [lnRetVal+1] exceed 6 digit.
   *REPLACE nSeq_No WITH lnRetVal+1
   REPLACE nSeq_No WITH IIF(lnRetVal + 1 > 999999,0,lnRetVal + 1)
   *B603586,1 SSH 29/02/00 (End)
   =gfAdd_info('SEQUENCE')
   *--MAN Added RLOCK Condition[Start]

   *B603586,1 SSH 29/02/00  (Begin) Use the new field (cSeq_Chr) from sequence file.
   IF  nSeq_No = 0 .AND. lnOldGenNm <> 0
      REPLACE cSeq_Chr WITH IIF(EMPTY(cSeq_Chr),'A',CHR(ASC(cSeq_Chr)+1))
   ENDIF
   IF !EMPTY(lcExtraStr)
      lnRetVal = ALLTRIM(lcExtraStr) + PADL(lnRetVal,lnRetLen-1,"0")
   ENDIF
   *B603586,1 SSH 29/02/00 (End)

   UNLOCK
   *--MAN Added RLOCK Condition[End]
   *E300888 06/04/98 YMA Never return a numeric code, and return the code
   *E300888              prefixed with the active site unique prefix code
   *E300888              if any.
   *lnRetVal = IIF(SEQUENCE.cData_Typ='N',lnRetVal,;
   PADL(lnRetVal,SEQUENCE.nFld_Wdth,'0'))
   lnRetVal = lcUnqPreFx + PADL(lnRetVal, lnRetLen, "0")
   *E300888 06/04/98 YMA End.

   IF luSequence
      USE IN SEQUENCE
   ELSE
      SET ORDER TO TAG ltSequence IN SEQUENCE
      IF BETWEEN(leSequence,1,RECCOUNT('Sequence'))
         GOTO leSequence IN 'Sequence'
      ENDIF
   ENDIF
   SELECT (lcSavAlias)
   RETURN(lnRetVal)

FUNCTION gfgetMemvar
   PARAMETERS lcArray,lcCompID
   PRIVATE lnAliasNo,llCUsedBy,llArrayORvar,llTwoDimen,lcSetupTag,;
      lcConfgTag,lnRetCount,lcOnErr,llError,laVarArr,llUseSycC
   *B601818,1  Get company path
   *E301077,1 Add private variable llCompUsed
   *PRIVATE lcCompDir, lcSetPath, llReUsedBy, lnCurTag, lnCurRec
   PRIVATE lcCompDir, lcSetPath, llReUsedBy, lnCurTag, lnCurRec, llCompUsed
   *E301077,1 end
   *B601818,1  end

   lnAliasNo=SELECT()
   llUsedBy  = .F.
   llCUsedBy = .F.
   llSUsedBy = .F.
   llUseSycC = .F.
   *E301077,1 Check if SYCCOMP is used
   *E301077,78 Hesham (Start)
   *llCompUsed = USED('SYCCOMP')
   lcCompID  = IIF(TYPE('lcCompId')<>'C' OR EMPTY(lcCompID),gcAct_Comp,lcCompID)
   *B601818,1  Get company path
   llCompUse = gfOpenFile(gcSysHome+"SYCCOMP",'CCOMP_ID')
   *IF !llCompUsed
   *  USE (gcSysHome+'SYCCOMP') IN 0 ORDER TAG CCOMP_ID SHARED
   *ENDIF
   *E301077,78 Hesham (End)
   *E301077,1  end
   *E301098,1 Hesham (Start)
   *lcCompDir  = IIF(SEEK(lcCompID, 'SYCCOMP'), ALLTRIM(SYCCOMP.cCom_DDir), gcDataDir)
   lcCompDir  = IIF(SEEK(lcCompID, 'SYCCOMP'), gfGetDataDir(ALLTRIM(sycComp.cCom_dDir)), gcDataDir)
   *E301098,1 Hesham (End)
   *E301077,1 Close it if we have opened it
   *E301077,78 Hesham (Start)
   =IIF(llCompUse,gfSysClose("SYCCOMP"),'')
   *IF !llCompUsed
   *  USE IN SYCCOMP
   *ENDIF
   *E301077,78 Hesham (End)
   *E301077,1 end
   llReUsedBy = .F.
   *B601818,1  end
   llArrayORvar = TYPE('lcArray[1]')='C'
   llTwoDimen = IIF(llArrayORvar AND ALEN(lcArray,2)=2,IIF(TYPE('lcArray[1,2]')='L' OR TYPE(lcArray[1,1])='U','A','N'),'V' )
   IF !llArrayORvar AND ',' $ lcArray
      DIMENSION laVarArr[1]
      =gfSubStr(lcArray,@laVarArr)
      DIMENSION lcArray[ALEN(laVarArr)]
      =ACOPY(laVarArr,lcArray)
      llArrayORvar = .T.
      llTwoDimen = 'V'
      *ELSE
      *  lcArray = UPPER(PADR(LCARRAY,10))
   ENDIF
   IF !USED('SETUPS')
      SELECT 0
      *B601818,1 use SETUPS from the company directory
      *USE (gcDataDir+'SETUPS')
      USE (lcCompDir+'SETUPS') AGAIN
      *B601818,1 end
      llSUsedBy = .T.
   ELSE
      SELECT SETUPS
      *B601818,1 Check if the file is opened from the company path
      lcSetPath = SET('FULLPATH')
      SET FULLPATH ON
      lcSetupsDir = DBF()
      SET FULLPATH &lcSetPath
      IF !(lcCompDir) $ lcSetupsDir
         lnCurTag  = VAL(SYS(21))
         lnCurRec  = RECNO()
         USE (lcCompDir+'SETUPS') AGAIN
         llReUsedBy = .T.
      ENDIF
      *B601818,1 end
   ENDIF
   lcSetupTag =TAG()
   SET ORDER TO TAG VARNAME
   lcRetVal=''
   lnRetCount=0
   lcOnErr=ON('ERROR')
   ON ERROR llError = .T.
   IF !llArrayORvar
      IF SEEK(PADR(UPPER(lcArray),10))
         DO CASE
            CASE cDefa_Typ='V'
               lcRetVal = lfTrnsStr(STRTRAN(mData_Def,CHR(13)+CHR(10),''),cData_Typ)
            CASE cDefa_Typ='E'
               lcRetVal=EVAL(STRTRAN(mData_Def,CHR(13)+CHR(10),''))
         ENDCASE
      ELSE
         llUseSycC = .T.
         IF !USED('SYCCONFG')
            SELECT 0
            USE (gcSysHome+'SYCCONFG')
            llCUsedBy = .T.
         ELSE
            SELECT SYCCONFG
         ENDIF
         lcConfgTag=TAG()
         SET ORDER TO TAG VARNAME

         IF SEEK(PADR(UPPER(lcArray),10))
            DO CASE
               CASE cDefa_Typ='V'
                  lcRetVal=lfTrnsStr(STRTRAN(mData_Def,CHR(13)+CHR(10),''),cData_Typ)
               CASE cDefa_Typ='E'
                  lcRetVal=EVAL(STRTRAN(mData_Def,CHR(13)+CHR(10),''))
            ENDCASE
         ENDIF
      ENDIF
   ELSE
      llUseSycC = .T.
      IF !USED('SYCCONFG')
         SELECT 0
         USE (gcSysHome+'SYCCONFG')
         llCUsedBy = .T.
      ELSE
         SELECT SYCCONFG
      ENDIF
      lcConfgTag=TAG()
      SET ORDER TO TAG VARNAME

      FOR lnCount = 1 TO ALEN(lcArray,1)
         llError = .F.
         lcRetVal=''
         SELECT SETUPS
         IF SEEK(PADR(UPPER(IIF(llTwoDimen = 'V',lcArray[lnCount],lcArray[lnCount,1])),10))
            DO CASE
               CASE cDefa_Typ='V'
                  lcRetVal=lfTrnsStr(STRTRAN(mData_Def,CHR(13)+CHR(10),''),cData_Typ)
               CASE cDefa_Typ='E'
                  lcRetVal=EVAL(STRTRAN(mData_Def,CHR(13)+CHR(10),''))
            ENDCASE
         ELSE
            SELECT SYCCONFG
            IF SEEK(PADR(UPPER(IIF(llTwoDimen = 'V',lcArray[lnCount],lcArray[lnCount,1])),10))
               DO CASE
                  CASE cDefa_Typ='V'
                     lcRetVal=lfTrnsStr(STRTRAN(mData_Def,CHR(13)+CHR(10),''),cData_Typ)
                  CASE cDefa_Typ='E'
                     lcRetVal=EVAL(STRTRAN(mData_Def,CHR(13)+CHR(10),''))
               ENDCASE
            ENDIF
         ENDIF
         DO CASE
            CASE llTwoDimen = 'N'
               &lcArray[lnCount,2] = lcRetVal
            CASE llTwoDimen = 'V'
               &lcArray[lnCount] = lcRetVal
            CASE llTwoDimen = 'A'
               lcArray[lnCount,2] = lcRetVal
         ENDCASE
         lnRetCount=lnRetCount+IIF(!llError,1,0)
      ENDFOR
   ENDIF
   ON ERROR &lcOnErr
   IF llUseSycC
      SELECT SYCCONFG
      IF !EMPTY(lcConfgTag)
         SET ORDER TO TAG (lcConfgTag)
      ELSE
         SET ORDER TO
      ENDIF
      IF llCUsedBy
         USE IN SYCCONFG
      ENDIF
   ENDIF

   SELECT SETUPS
   IF !EMPTY(lcSetupTag)
      SET ORDER TO TAG (lcSetupTag)
   ELSE
      SET ORDER TO
   ENDIF
   IF llSUsedBy
      USE IN SETUPS
   ENDIF

   *B601818,1 ReUse SETUPS file
   IF llReUsedBy .AND. !EMPTY(lcSetupsDir)
      SELECT SETUPS
      USE (lcSetupsDir) ORDER lnCurTag
      IF BETWEEN(lnCurRec, 1, RECCOUNT())
         GO lnCurRec
      ELSE
         GO TOP
      ENDIF
   ENDIF
   *B601818,1 end

   SELECT (lnAliasNo)
   RETURN IIF(!llArrayORvar,lcRetVal,lnRetCount)
FUNCTION gfOpenFile

   *B602015,1 AHM 05/07/98
   *PARAMETERS NFILE,lcIndex,MODE
   PARAMETERS NFILE,lcIndex,MODE,lcAliasNam,llForceOp
   *MAN Added lcSetExact to the following statement
   PRIVATE MODE,lcFileName,lcPath,llReturnVal,lcMsg,lcSetExact

   *E301316,1 [Start]
   PRIVATE lcMacroSub
   lcMacroSub=""
   *E301316,1 [End..]

   *lcPath     = IIF(ATC('\',nfile)<>0,SUBSTR(NFILE,1,RAT('\',nfile)),'')
   lcFileName  = IIF(ATC('\',NFILE)<>0,SUBSTR(NFILE,RAT('\',NFILE)+1),NFILE)
   lcOpenMode  = IIF(TYPE('MODE')='C' AND MODE='EX', "EXCLUSIVE", "SHARED")
   lcOrderTag  = IIF(TYPE('lcIndex')='C',SUBSTR(lcIndex,IIF('\' $ lcIndex,ATC('\',lcIndex,OCCURS('\',lcIndex)),0) +1),'')



   *-- MAN Commented out the following block .... to the end of the function and changed the code
   *E301077,1 Hesham (Start)
   *E301077,1 IF the file we are trying to open is not assigned to the
   *E301077,1 program files then add it to the program files
   *E301077,1 so we can close it in the global cleanup
   *B602513,1 Hesham (Start)
   *B602513,1 if the file we are opening is a temproray file then do not
   *B602513,1  add it to the files array

   **MAN-- IF LEFT(ALLT(lcFileName),1)<>'X'
   PRIVATE llOpen
   llOpen = .F.
   *B602513,1 Hesham (End)
   **MAN--   lcErrOn = ON('ERROR')
   **MAN--   llError = .F.
   **MAN--   ON ERROR llError = .T.
   **MAN--   IF TYPE('laFileName')<>'U'
   **MAN--     lcSetExact = SET('Exact')
   **MAN--     SET EXACT ON
   **MAN--     lnFilePos = ASCAN(laFileName,lcFileName)
   **MAN--     IF lnFilePos = 0
   **MAN--       IF !EMPTY(laFileName[1,1])
   **MAN--         DIMEN laFileName[ALEN(laFileName,1)+1,ALEN(laFileName,2)]
   **MAN--       ENDIF
   **MAN--       laFileName[ALEN(laFileName,1),1] = lcFileName
   **MAN--       laFileName[ALEN(laFileName,1),2] = lcOrderTag
   **MAN--       lnFileElm = ASCAN(gaMnu_Fl,lcFileName)
   **MAN--       IF lnFileElm > 0
   **MAN--          lnRowNo = ASUBSCRIPT(gaMnu_Fl,lnFileElm,1)
   **MAN--          gaMnu_Fl[lnRowNo,4] = gaMnu_Fl[lnRowNo,4] + 1
   **MAN--       ELSE
   **MAN--          DECLARE gaMnu_Fl[ALEN(gaMnu_Fl,1)+1,ALEN(gaMnu_Fl,2)]
   **MAN--          =AINS(gaMnu_Fl,1)
   **MAN--          gaMnu_Fl[1,1] = lcFileName
   **MAN--          gaMnu_Fl[1,2] = lcOrderTag
   **MAN--          gaMnu_Fl[1,3] = SELECT(0)
   **MAN--          *gaMnu_Fl[1,4] = IIF(USED(lcFileName),2,1)
   **MAN--          *gaMnu_Fl[1,5] = "P"
   **MAN--          gaMnu_Fl[1,4] = 1
   **MAN--          gaMnu_Fl[1,5] = IIF(USED(lcFileName),'S','P')
   **MAN--          gaMnu_Fl[1,6] = " "
   **MAN--       ENDIF
   **MAN--     ENDIF
   **MAN--     SET EXACT &lcSetExact
   **MAN--   ENDIF
   **MAN--   ON ERROR &lcErrOn
   *B602513,1 Hesham (Start)
   **MAN-- ENDIF
   *B602513,1 Hesham (End)
   *E301077,1 Hesham (End)
   *-- MAN End
   *B602015,1 lcAliasNam parameter that holds the name of the alias that file
   *B602015,1                           will be used with.
   *B602015,1 llForceOp parameter that showes if the function used want to
   *B602015,1                          open the file with another alias name
   *B602015,1                          regardless it is rather opened or opened
   *B602015,1                          from the same directory or not opened
   IF TYPE('lcAliasNam') <> 'C'
      lcAliasNam = lcFileName
   ENDIF

   *E300247,1 YMA 06/13/95 Changed the displaying of the file
   *E300247,1 name to be in the status message.
   lcMsg = 'Opening '+NFILE+IIF(EMPTY(lcIndex),'', ' Index Tag '+lcOrderTag)+'....'
   lcMsg = PROPER(lcMsg)
   lnCurRow = ROW()
   lnCurCol = COL()
   IF 'SCREEN' $ SYS(101)
      DO CASE
         CASE _DOS OR _UNIX
            lcActWin = WONTOP()
            ACTIVATE SCREEN
            @ 24,00 SAY gfMsgStr(lcMsg) COLOR &gcMsgClr
            IF !EMPTY(lcActWin)
               ACTIVATE WINDOW (lcActWin)
            ENDIF
         CASE _WINDOWS OR _MAC
            SET MESSAGE TO lcMsg
      ENDCASE
   ENDIF
   *E300247,1 YMA 06/13/95 End.

   *B602015,1 AHM 05/07/98 using alias name to use the file by this alias

   *IF USED(lcFilename)
   *  SELECT (lcFilename)
   *  IF !EMPTY(lcIndex)
   *    SET ORDER TO TAG lcOrderTag
   *  ENDIF
   *ELSE
   *  SELECT 0
   *  IF EMPTY(lcIndex)
   *    USE (NFILE) &lcOpenMode
   *  ELSE
   *    USE (NFILE) ORDER TAG lcOrderTag &lcOpenMode
   *  ENDIF
   *  llReturnVal = .T.
   *ENDIF

   llReturnVal = .T.
   *B602015,1 saving the FULLPATH Status
   lcFPathSt   = SET('FULLPATH')
   SET FULLPATH ON
   IF USED(lcFileName)
      lcOpenMode = "SHARED"
      *-- if the file is used and it is from the same data directory
      IF DBF(lcFileName) == ALLTRIM(STRTRAN(UPPER(NFILE), ".DBF") + ".DBF")
         *-- if forced open is desired
         IF llForceOp
            lcAliasNam  = gfTempName()
            *E301316,1 [Start]
            *USE (NFILE) ALIAS (lcAliasNam) AGAIN IN 0 &lcOpenMode
            lcMacroSub="USE (NFILE) ALIAS (lcAliasNam) AGAIN IN 0 &lcOpenMode"
            &lcMacroSub
            *E301316,1 [End..]

            *--MAN
            llOpen = .T.
            IF !EMPTY(lcOrderTag)
               SET ORDER TO TAG lcOrderTag IN (lcAliasNam)
            ENDIF    &&IF !EMPTY(lcOrderTag)
         ELSE
            *-- if forced open is not desired
            llReturnVal = .F.
            lcAliasNam  = IIF(TYPE('lcAliasNam')#'C' OR EMPTY(lcAliasNam),ALLTRIM(STRTRAN(UPPER(lcFileName),".DBF")),lcAliasNam)
            *-- if there is no tag is desired to set order to
            IF EMPTY(lcOrderTag)
               SET ORDER TO 0 IN (lcAliasNam)
            ELSE
               SET ORDER TO TAG lcOrderTag IN (lcAliasNam)
            ENDIF   &&IF EMPTY(lcOrderTag)
         ENDIF     &&IF llForceOp
      ELSE
         *-- if the file is used but not from the same data directory
         lcAliasNam  = IIF(TYPE('lcAliasNam')#'C' OR EMPTY(lcAliasNam),gfTempName(),lcAliasNam)
         *E301316,1 [Start]
         *USE (NFILE) ALIAS (lcAliasNam) AGAIN IN 0 &lcOpenMode
         lcMacroSub="USE (NFILE) ALIAS (lcAliasNam) AGAIN IN 0 &lcOpenMode"
         &lcMacroSub
         *E301316,1 [End..]

         *-- MAN
         llOpen = .T.
         IF !EMPTY(lcOrderTag)
            SET ORDER TO TAG lcOrderTag IN (lcAliasNam)
         ENDIF  &&IF !EMPTY(lcOrderTag)
      ENDIF   &&IF DBF(lcFilename) == .......
   ELSE
      *-- if the file is not used
      lcAliasNam  = IIF(TYPE('lcAliasNam')#'C' OR EMPTY(lcAliasNam),ALLTRIM(STRTRAN(UPPER(lcFileName),".DBF")),lcAliasNam)
      *E301316,1 [Start]
      *USE (NFILE) ALIAS (lcAliasNam) AGAIN IN 0 &lcOpenMode
      lcMacroSub="USE (NFILE) ALIAS (lcAliasNam) AGAIN IN 0 &lcOpenMode"
      &lcMacroSub
      *E301316,1 [End..]

      *-- MAN
      llOpen = .T.
      IF !EMPTY(lcOrderTag)
         SET ORDER TO TAG lcOrderTag IN (lcAliasNam)
      ENDIF    &&IF !EMPTY(lcOrderTag)
   ENDIF    &&IF IF USED(lcFilename)
   SELECT (lcAliasNam)
   SET FULLPATH &lcFPathSt
   *B602015,1 AHM 05/07/98 End.

   *-- MAN Start
   *-- MAN Commented out the following block .... to the end of the function and changed the code
   *E301077,1 Hesham (Start)
   *E301077,1 IF the file we are trying to open is not assigned to the
   *E301077,1 program files then add it to the program files
   *E301077,1 so we can close it in the global cleanup
   *B602513,1 Hesham (Start)
   *B602513,1 if the file we are opening is a temproray file then do not
   *B602513,1  add it to the files array


   IF !UPPER(gcWorkDir) $ UPPER(NFILE)

      *B602513,1 Hesham (End)
      lcErrOn = ON('ERROR')
      llError = .F.
      ON ERROR llError = .T.
      IF TYPE('laFileName')<>'U'
         lcSetExact = SET('Exact')
         SET EXACT ON
         *-- YMA MAN
         *lnFilePos = ASCAN(laFileName,lcAliasNam)
         FOR lnFilePos = 1 TO ALEN(laFileName,1)
            IF ALLTRIM(laFileName[lnFilePos,1]) == ALLTRIM(lcAliasNam)
               EXIT
            ENDIF
         ENDFOR
         IF lnFilePos > ALEN(laFileName,1)
            lnFilePos = 0
         ENDIF
         *-- YMA MAN

         IF lnFilePos = 0
            IF !EMPTY(laFileName[1,1])
               DIMEN laFileName[ALEN(laFileName,1)+1,ALEN(laFileName,2)]
               *-- MAN Added ELSE Cond.
            ELSE
               *B802826,1 RAMY Add more columns [start]
               *DIME laFileName[1,1]
               DIME laFileName[1,4]
               *B802826,1 RAMY [end]
            ENDIF
            laFileName[ALEN(laFileName,1),1] = lcAliasNam
            laFileName[ALEN(laFileName,1),2] = lcOrderTag
            *E000001,1 Hesham (Start)
            laFileName[ALEN(laFileName,1),3] = NFILE
            laFileName[ALEN(laFileName,1),4] = lcFileName
            *E000001,1 Hesham (End)
            *-- YMA MAN
            *lnFileElm = ASCAN(gaMnu_Fl,lcAliasNam)
            FOR lnFileElm = 1 TO ALEN(gaMnu_Fl,1)
               IF ALLTRIM(gaMnu_Fl[lnFileElm,1]) == ALLTRIM(lcAliasNam)
                  EXIT
               ENDIF
            ENDFOR
            IF lnFileElm > ALEN(gaMnu_Fl,1)
               lnFileElm = 0
            ENDIF
            *-- YMA MAN
            IF lnFileElm > 0
               *-- YMA MAN
               *lnRowNo = ASUBSCRIPT(gaMnu_Fl,lnFileElm,1)
               lnRowNo = lnFileElm
               *-- YMA MAN
               gaMnu_Fl[lnRowNo,4] = gaMnu_Fl[lnRowNo,4] + 1
            ELSE
               DECLARE gaMnu_Fl[ALEN(gaMnu_Fl,1)+1,ALEN(gaMnu_Fl,2)]
               =AINS(gaMnu_Fl,1)
               gaMnu_Fl[1,1] = lcAliasNam
               gaMnu_Fl[1,2] = lcOrderTag
               gaMnu_Fl[1,3] = SELECT(0)
               *gaMnu_Fl[1,4] = IIF(USED(lcFileName),2,1)
               *gaMnu_Fl[1,5] = "P"
               gaMnu_Fl[1,4] = 1
               gaMnu_Fl[1,5] = IIF(!llOpen ,'S','P')
               gaMnu_Fl[1,6] = " "
            ENDIF
         ENDIF
         SET EXACT &lcSetExact
      ENDIF
      ON ERROR &lcErrOn
      *B602513,1 Hesham (Start)
   ENDIF
   *B602513,1 Hesham (End)
   *E301077,1 Hesham (End)

   *-- MAN END

   *E300247,1 YMA 06/13/95 Changed the displaying of the file
   *E300247,1 name to be in the status message.
   IF 'SCREEN' $ SYS(101)
      DO CASE
         CASE _DOS OR _UNIX
            lcActWin = WONTOP()
            ACTIVATE SCREEN
            @ 24,00 SAY gfMsgStr("") COLOR &gcMsgClr
            IF !EMPTY(lcActWin)
               ACTIVATE WINDOW (lcActWin)
            ENDIF
            @ lnCurRow,lnCurCol SAY ""
         CASE _WINDOWS OR _MAC
            SET MESSAGE TO ""
      ENDCASE
   ENDIF
   *E300247,1 YMA 06/13/95 End.

   RETURN llReturnVal

FUNCTION gfCloseFile
   PARAMETER lcFileName
   FOR lnFilePos = 1 TO ALEN(laFileName,1)
      IF ALLTRIM(laFileName[lnFilePos,1]) == ALLTRIM(lcFileName)
         EXIT
      ENDIF
   ENDFOR
   IF lnFilePos > ALEN(laFileName,1)
      lnFilePos = 0
   ENDIF
   IF lnFilePos > 0
      FOR lnFileElm = 1 TO ALEN(gaMnu_Fl,1)
         IF ALLTRIM(gaMnu_Fl[lnFileElm,1]) == ALLTRIM(laFileName[lnFilePos ,1])
            EXIT
         ENDIF
      ENDFOR
      IF lnFileElm > ALEN(gaMnu_Fl,1)
         lnFileElm = 0
      ENDIF
      IF lnFileElm > 0
         lnRowNo = lnFileElm
         gaMnu_Fl[lnRowNo,4] = gaMnu_Fl[lnRowNo,4] - 1
         IF gaMnu_Fl[lnRowNo,5] ="P" .AND. USED(gaMnu_Fl[lnRowNo,1])
            USE IN (lcFileName)
         ENDIF
         IF gaMnu_Fl[lnRowNo,4]<=0 AND gaMnu_Fl[lnRowNo,5] ="P"
            =ADEL(gaMnu_Fl,lnRowNo)
            IF ALEN(gaMnu_Fl,1) > 1
               DECLARE gaMnu_Fl[ALEN(gaMnu_Fl,1) -1,ALEN(gaMnu_Fl,2)]
            ENDIF
         ENDIF
      ENDIF
      =gfAdel(@laFileName,lnFilePos)
   ENDIF
FUNCTION gfSysClose
   PARAMETERS lcFile
   PRIVATE lnFilePos

   lnFilePos = 0
   IF (TYPE('laFileName[1,1]')='C' AND !EMPTY(laFileName[1,1]))
      FOR lnFilePos = 1 TO ALEN(laFileName,1)
         IF ALLTRIM(laFileName[lnFilePos,1]) == ALLTRIM(lcFile)
            EXIT
         ENDIF
      ENDFOR
      IF lnFilePos > ALEN(laFileName,1)
         lnFilePos = 0
      ENDIF
   ENDIF
   IF lnFilePos = 0 AND USED(lcFile)
      USE IN (lcFile)
   ENDIF

   *IF (TYPE('laFileName[1,1]')#'C' OR EMPTY(laFileName[1,1])) AND USED(lcFile)
   *  USE IN (lcFile)
   *ENDIF
FUNCTION lfTrnsStr
   PARAMETERS lcValueStr,lcDataType,lcDirection
   DO CASE
      CASE lcDataType $ 'CM'
         RETURN ALLT(lcValueStr)
      CASE lcDataType = 'N'
         RETURN VAL(lcValueStr)
      CASE lcDataType='D'
         RETURN CTOD(lcValueStr)
      CASE lcDataType = 'L'
         RETURN IIF(UPPER(ALLTRIM(lcValueStr))='.F.',.F.,.T.)
   ENDCASE
FUNCTION gfModalGen
   PARAMETER lcDlgID,lcDlgTyp,lcVarsStr,lcDlgValid,lcDlgMessg

   PRIVATE lcDlgID,lcDlgTyp,lcVarsStr,lcDlgValid,lcCurrDbf,lnButtons,lcDlgMessg,;
      lcDevice

   DECLARE laButtNo [1]         && Array to hold the buttons separetly
   DECLARE laVarsStr[1]         && Array to hold variabels to be replaced
   && in the messag
   lcDevice = SET('DEVICE')
   SET DEVICE TO SCREEN
   PUSH KEY
   ON KEY
   lcCurrDbf   = ALIAS()
   lcDlgID     = IIF(TYPE('lcDlgID'   ) $ "UL",'',lcDlgID   )
   lcDlgTyp    = IIF(TYPE('lcDlgTyp'  ) $ "UL",'D',lcDlgTyp )
   lcVarsStr   = IIF(TYPE('lcVarsStr' ) $ "UL",'',lcVarsStr )
   lcDlgValid  = IIF(TYPE('lcDlgValid') $ "UL",'',lcDlgValid)
   lcDlgMessg  = IIF(TYPE('lcDlgMessg') $ "UL",'',lcDlgMessg)
   lcMsgCatgry = SUBSTR(lcDlgID,1,2)

   lnColrSchm  = VAL(SUBSTR('57',AT(LEFT(lcDlgTyp,1),'DA'),1))
   lnDlgOpTion = 1              && Variable to get the result in
   lnButtSize  = 1
   lnOldRec    = 0
   *B603228,1 AMM Consider the icon space if any
   *lnTextStrt  = IIF(EMPTY(lcDlgMessg),7,2)
   *lnRigtShft  = IIF(EMPTY(lcDlgMessg),10,5)
   lnTextStrt  = IIF(!EMPTY(lcMsgCatgry),7,2)
   lnRigtShft  = IIF(!EMPTY(lcMsgCatgry),10,5)
   *B603228,1 AMM START, Get the buttons even with custom messages
   *IF EMPTY(lcDlgMessg)
   IF !EMPTY(lcDlgID)
      *B603228,1 AMM end
      *** Open dialog file if it is not opend
      *E301077,78 Hesham (Start)
      llDlUse = gfOpenFile(gcSysHome+"syddlobj",'cdlobjid')
      *IF !USED('SydDlObj')
      *  SELECT 0
      *  USE (gcSysHome+"SydDlObj")
      *ENDIF
      *SET ORDER TO TAG CDLOBJID IN ALIAS('SydDlObj')
      lnOldRec = IIF(llDlUse,0,RECNO('SydDlObj'))
      *E301077,78 Hesham (End)


      *** Get dialog button
      *B603228,1 AMM If the button sent, get it, else display by OK button
      *IF SEEK(SUBSTR(lcDlgID,9),'SydDlObj')
      IF !EMPTY(SUBSTR(lcDlgID,9)) .AND. SEEK(SUBSTR(lcDlgID,9),'SydDlObj')
         *B603228,1 AMM end
         lcButton = ALLTRIM(SydDlObj.mDlObj)
      ELSE
         *** If not found set default button to <ok>
         lcButton = "\!\?\<Ok"
      ENDIF
      *B603228,1 AMM end
      *** Get Dialog messag
      *B603228,1 AMM Get the message from table if there is no custom message
      IF EMPTY(lcDlgMessg)
         *B603228,1 AMM end
         IF SEEK(SUBSTR(lcDlgID,3,6),'SydDlObj')
            lcMessag    = ALLTRIM(SydDlObj.mDlObj)
            lcMsgCatgry = IIF(EMPTY(SydDlObj.cMsgCatgry),lcMsgCatgry,SydDlObj.cMsgCatgry)
         ELSE
            *** If not found set default message to null
            lcMessag = ""
         ENDIF
         *B603228,1 AMM If custom message, get it from the fifth parameter
      ELSE
         lcMessag   = ALLTRIM (lcDlgMessg)
      ENDIF
      *B603228,1 AMM end
      *E301077,78 Hesham (Start)
      =IIF(llDlUse,gfSysClose('SYDDLOBJ'),'')
      *E301077,78 Hesham (eND)
      *** Collect variables to be replaced from string to array
      =gfSubStr(lcVarsStr,@laVarsStr,'|')

      *** Replace each ð mark with variabe sent
      FOR lnVarsStr = 1  TO ALEN(laVarsStr,1)
         lcMessag = STRTRAN(lcMessag,'ð',laVarsStr[lnVarsStr],1,1)
      ENDFOR

      *** Remove all controle characters " \ ! < ?" from the button string
      lcButtChk  = STRTRAN(STRTRAN(STRTRAN(STRTRAN(lcButton,'\',''),'!',''),;
         '<',''),'?','')

      *** Collect all Buttons in array
      =gfSubStr(lcButtChk,@laButtNo,';')

      *** Get No of buttons from array len
      lnButtNo   = ALEN(laButtNo,1)

      *** Get the max width of button
      FOR lnButtons = 1 TO lnButtNo
         lnButtSize = MAX(lnButtSize,LEN(laButtNo[lnButtons]))
      ENDFOR

   ELSE
      lcMessag   = ALLTRIM (lcDlgMessg)
      lcButton   = "\!\?\<Ok"
      lnButtNo   = 1
      lnButtSize = 4
   ENDIF


   *** Buttons width should not be less than 10 after adding 4 (< >)
   lnButtSize = MAX(10,lnButtSize+ 4 )


   *** Calculat window width accordin to total buttons size
   lnWinWidth = MAX(50,(lnButtSize * lnButtNo)+6)

   *** Calculat space between buttons
   lnBetwButt = INT((lnWinWidth -(lnButtSize * lnButtNo))/lnButtNo)

   *** Calculat X position of the button to be centerd
   lnButXPos  = (lnWinWidth/2)-((lnButtSize * lnButtNo)+;
      (lnBetwButt*(lnButtNo-1)))/2

   *** Calculat window higth according to the length of the messag
   lnWinHight = INT(LEN(ALLTRIM(lcMessag))/lnWinWidth) + 6

   *MAN Define the title Var.
   IF TYPE("gcSysName") # "C"
      gcSysName = ""
   ENDIF

   *** Define dialog window with right width, hight and color
   #IF _DOS
      DEFINE WINDOW gwdDialog ;
         FROM 0,0 TO lnWinHight,lnWinWidth ;
         SHADOW;
         FLOAT ;
         DOUBLE;
         COLOR SCHEME lnColrSchm
   #ELSE
      *B600914,1 Oraby 17/03/1996 Stop the user abilty to close the dialog messages, from the screen menu
      *    DEFINE WINDOW gwdDialog ;
      AT  0.000, 0.000  ;
      SIZE lnWinHight,lnWinWidth;
      FONT "System", 10 ;
      STYLE "B" ;
      FLOAT ;
      SYSTEM ;
      TITLE (gcSysName);
      CLOSE ;
      NOMINIMIZE ;
      COLOR RGB(,,,192,192,192)

      DEFINE WINDOW gwdDialog ;
         AT  0.000, 0.000  ;
         SIZE lnWinHight,lnWinWidth;
         FONT "System", 10 ;
         STYLE "B" ;
         FLOAT ;
         SYSTEM ;
         TITLE (gcSysName);
         NOMINIMIZE ;
         COLOR RGB(,,,192,192,192)
   #ENDIF

   *** Activate dialog window no show so the window will pop with messag
   *** when activated
   ACTIVATE WINDOW gwdDialog NOSHOW

   *** Move the dialog window to center of the screen
   MOVE WINDOW gwdDialog CENTER

   #IF _DOS
      IF EMPTY(lcDlgMessg)
         @ 0,1,2,5 BOX
         @ 1,3  SAY SUBSTR ("!?I",(AT(lcMsgCatgry,"TRQRIN")+1)/2,1)
      ENDIF

      @ 1,lnTextStrt  SAY lcMessag SIZE lnWinHight-3,lnWinWidth-lnRigtShft
   #ELSE
      @ 0.000,0.000 TO 0.000,lnWinWidth ;
         PEN 1, 8 ;
         STYLE "1" ;
         COLOR RGB(255,255,255,255,255,255)
      @ 0.000,0.000 TO lnWinHight,0.000 ;
         PEN 1, 8 ;
         COLOR RGB(255,255,255,255,255,255)
      @ 0.188,0.429 TO 0.188,lnWinWidth-.6 ;
         PEN 1, 8 ;
         STYLE "1" ;
         COLOR RGB(128,128,128,128,128,128)
      @ 0.188,0.429 TO lnWinHight-.3,0.429 ;
         PEN 1, 8 ;
         COLOR RGB(128,128,128,128,128,128)
      @ 0.000,lnWinWidth TO lnWinHight,lnWinWidth ;
         PEN 1, 8 ;
         COLOR RGB(128,128,128,128,128,128)
      @ 0.188,lnWinWidth-.6 TO lnWinHight-.3,lnWinWidth-.6 ;
         PEN 1, 8 ;
         COLOR RGB(255,255,255,255,255,255)
      @ lnWinHight,0.143 TO lnWinHight,lnWinWidth ;
         PEN 1, 8 ;
         STYLE "1" ;
         COLOR RGB(128,128,128,128,128,128)
      @ lnWinHight-.3,0.4 TO lnWinHight-.3,lnWinWidth-.4 ;
         PEN 1, 8 ;
         STYLE "1" ;
         COLOR RGB(255,255,255,255,255,255)
      *B603228,1 AMM Display the icon if sent as parameter
      *IF EMPTY(lcDlgMessg)
      IF !EMPTY(lcMsgCatgry)
         *B603228,1 AMM end
         lcIcon = ALLTRIM(SUBSTR ("TR  QRY INFO",;
            ((AT(lcMsgCatgry,"TRQRIN")+1)/2-1)*4+1,4))+".BMP"
         @ 1.5,1.5 SAY lcIcon BITMAP STYLE "T"
      ENDIF

      @ 1.5,lnTextStrt SAY lcMessag SIZE lnWinHight-3,lnWinWidth-lnRigtShft
   #ENDIF


   IF EMPTY(lcDlgValid )
      @ lnWinHight -2 ,lnButXPos ;
         GET lnDlgOpTion ;
         PICTURE "@*HT "+lcButton;
         SIZE 1.5,lnButtSize,lnBetwButt;
         MESSAGE gfObj_msg()
   ELSE
      @ lnWinHight -2 ,lnButXPos ;
         GET lnDlgOpTion ;
         PICTURE "@*H "+lcButton;
         SIZE 1.5,lnButtSize,lnBetwButt;
         VALID &lcDlgValid;
         MESSAGE gfObj_msg()
   ENDIF


   *** When the read is activated fox will activate the window
   ACTIVATE WINDOW gwdDialog

   IF !EMPTY(lcDlgMessg)
      SET BELL TO 2000,2
      ?? CHR(7)
      SET BELL TO
   ENDIF

   READ CYCLE MODAL

   IF lnOldRec > 0  .AND. lnOldRec <= RECCOUNT('SydDlObj')
      GO lnOldRec IN ALIAS('SydDlObj')
   ENDIF

   IF !EMPTY(lcCurrDbf)
      SELECT (lcCurrDbf)
   ENDIF

   RELEASE WINDOW gwdDialog
   SET DEVICE TO &lcDevice
   POP KEY
   RETURN lnDlgOpTion

FUNCTION gfObj_msg
   PARAMETERS lcMsg2Disp

   IF gnMsgRec > 0 .AND. USED('syuckmsg')
      GO gnMsgRec IN syuckmsg

      IF !EMPTY(syuckmsg.cmessage)
         =gfPopMesag()
      ENDIF
   ENDIF


   lcMesg_Str = " "

   IF TYPE('lcMsg2Disp') = 'C'
      lcMesg_Str = SUBSTR(lcMsg2Disp,1,51)+IIF(_DOS,gcMesg_Str,'')
   ELSE
      IF TYPE('laFld_msg[1]') <> "U"

         lnAryObj = ASCAN(laFld_msg,SYS(18))

         IF lnAryObj > 0
            lnAryObj = ASUBSCRIPT(laFld_msg,lnAryObj,1)
            lcMesg_Str = ' '+laFld_msg[lnAryObj,2]+IIF(_DOS,gcMesg_Str,'')
         ELSE
            lcMesg_Str = SPACE(51)+IIF(_DOS,gcMesg_Str,'')
         ENDIF
      ELSE
         lcMesg_Str = SPACE(51)+IIF(_DOS,gcMesg_Str,'')
      ENDIF
   ENDIF

   RETURN lcMesg_Str
