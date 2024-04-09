*:************************************************************************
*: Program file  : NotePad
*: Program desc. : Add\Edit the notepads.
*: For screen    : NotePad.
*:         System: A27.
*:         Module: Aria Apparel Series.
*:      Developer: Yasser Mohammed Aly (YMA).
*:*************************************************************
*: Passed Parameters  : lcNoteType : The notepad type, which will
*:                                   be one of the following :
*:                                     Account      = A             
*:                                     Order        = B
*:                                     Invoice      = C
*:                                Commercial Invoice= E
*:                                     Style        = F
*:                                     Item         = G
*:                                     Vendor       = H
*:                                     C/T          = I
*:                                     Salesrep     = J
*:                                     L/C          = L
*:                                     Item P/O     = M
*:                                     Style P/O    = P
*:                                     Template     = T
*:                                     User         = U
*:                      Material Manufacturing Order= O
*:                                 Customer Letters = X
*:                                   Vendor Letters = Y
*C200027,7 M.H 07/07/96                Forcasting   = S
*E300620,1 Reham 06/15/97              Credit Memos = R
*C200027,7 M.H 07/07/96 lcNoteKey  : The key.
*:                      llTempltOk : IF .T., the program will run 
*:                                   the notepad template.
*:B602730,1 HDM 04/06/1999      Return Authorization = Z
*:E301318,1 HDM 09/26/1999      Customer System Settings = 'D'
*:C200254,1 ABD 03/25/2002 Material Manufacturing Rework Order= W
*:N000385,1 TMI 05/30/2002 Material Sales Order     = V
*:N000385,1 TMI 05/30/2002 Material Invoice Order   = N
*:C127341,1 TMI 07/26/2005 Add Shipment Type = 'Q'
*:*************************************************************
*: Modifications      :
*B500669,1 MFM 05/03/95 Fixed the bug of not displaying the desc. probebly.
*B500669,1              Changed the arrows direction in the notepad copy screen.
*B500669,1              Added the abilty to go to the end line in the edited reigon.
*B600320,1 MAN 05/28/95 Fixed the bug of freezing the machine if you pressed
*B600320,1              ESC key from the invoice/order notes in the invoice
*B600320,1              inquire program.
*B600408,1 Malak 6-12-95 Fix bug if there is no records in the temp file (lcTmpFile).  
*B600434,1 Malak 6-12-95 Fix the popup color in the copy screen.
*G600745,1 ARH 08/03/95  Fixed the bug "FILE IS IN USE BY ANOTHER"
*G600745,1               if more than one user edited the same kind of notes
*G600745,1               in the same time (Function lfOpnFiles).
*B800149,1 RENEE 09/03/95 Add a Print option on the notepad.
*B800093,1 RENEE 09/04/95 Fix record pointer position if called from templates.
*C200027,1 MFM 06/17/96 Changed the program to cover the general purpose
*C200027,1 MFM          letters for both accounts and vendors.
*B600822,1 Hesham El-Sheltawi ON 07/18/96
*B600822,1 replace setting of color set with global function
*B601168,1 SHA 08/20/96 Fixing the following bugs :-
*B601168,1 SHA 		    1- Prevent error message "begin of file encounterd" &
*B601168,1 SHA			    "end of file encounterd" from appearing when  
*B601168,1 SHA              Undo button or redo button is clicked 
*B601168,1 SHA			2- Make undo button,redo button,redo all and undo all 
*B601168,1 SHA			   working in a proper way
*E300457,1 Reham On 09/22/96 Enhance the bitmaps used in this screen.
*B601471,1 ASH 12/17/96 1- Allowing the copy option on the notepad to copy
*B601471,1 ASH             more than one template during the same session and 
*B601471,1 ASH             save it in the file.
*B601471,1 ASH          2- When selecting a key to copy from, by pressing
*B601471,1 ASH             the "?" mark on the key to activate the browse
*B601471,1 ASH             the system activates the first key in the selected 
*B601471,1 ASH             category regardless the selected key.
*B601471,1 ASH          3- The system displays a confirming message after
*B601471,1 ASH             finishing the copy session, even if the user
*B601471,1 ASH             selected to UNDO all the actions done.
*B601471,1 ASH          4- Error "End of file encountered"  when Undo
*B601471,1 ASH             all.
*E300637,1 WAM 04/08/97 Use the directories pathes
*E300637,1 WAM 04/08/97 change arrays name laData and laScrMode to
*E300637,1 WAM 04/08/97 laDataNote and laModes.
*E300620,1 Reham On 06/15/97 Add type for credit memos.
*B602179,1 AMM 11/25/98 Erase temporary files when quitting the program
*E301176,1 HDM 03/21/1999 Update lHasValue In Master File to indicate 
*                         whether we have notes for this record or not
*B602730,1 HDM 04/06/1999 Add New Type For Return authorization program
*E500305,1 Reham On 11/15/1999
*E500305,1 Enable the edit pad in the when function of the screen: NotePad.Scx by adding 
*E500305,1 the following command: "SET SKIP OF POPUP P02PU02 .F."
*C101518,1 ASH 02/01/2000 Add new type for commercial invoice ('E').
*B803054,1 RAMY 02/18/2000 Fix the bug of 'Record is out of range' of the customer notes in the 
*B803054,1 RAMY            sales order screen if new order
*B803157,1 Reham On 03/29/2000 Change the way of saving by using the scan command
*B803157,1 Instead of the replace all command to optimise the saving time.
*B803249,1 AMM Fix the bug of bieng on the EOF upon saving .
*B803698,1 AAN Fix the bug of When entering notepad for a SO and save it takes a long time.
*B803865,1 MAB 12/13/2000 Update lHasNotes Field to Correct the notes show button.
*:B605079,1 SSE 11/26/2001 Fix bug of printing the notepad with Foreground grey after first print.
*C200259,1 ADEL 01/07/02  Custom V.Fox Arabic notepad for Mexx.
*:C200254,1 ABD 03/25/2002 Add new notes for Material Manufacturing 
*:C200254,1 ABD            Rework Order With type 'W'.
*N000385,1 TMI 05/30/2002 Material Sales Order/Invoice order
*B606731,1 NAD 12/06/2002 Purging removes notepad records from the customer      
*:*************************************************************
*Gehan Add a parameter to pass the data directory so as to be able to 
*Gehan call the notepad from System Manager
*PARAMETER lcNoteType, lcNoteKey, llTempltOk
PARAMETER lcNoteType, lcNoteKey, llTempltOk, lcDataDir
*Gehan 
*E300637,1 WAM 04/08/97
PRIVATE laModes,laDataNote
*E300637,1 (End)

*Gehan If lcDataDir is not passed, use gcDataDir
lcDataDIr = IIF(EMPTY(lcDataDir), gcDataDir, lcDataDir)
*Gehan end

*B600320,1 MAN 05/28/95 (Begin) Fixed the bug of freezing the machine if you
*B600320,1              pressed ESC key from the invoice/order notes in
*B600320,1              the invoice inquire program.
*E301176,1 HDM [Start] Define 2 Variables to tell if notes updated or not
STORE '' TO lcOldValue
STORE .F. TO llNotesUpd
*C200259,1 (Begin) For Mexx Only.
llMex      = FILE("NOTES.DBF")
llRpFrmCan = .F.
*C200259,1 (End)

*E301176,1 HDM [End]
FOR lnKeyNo = 1 TO 10
  lcKeyNo   = ALLTRIM(STR(lnKeyNo))
  lcF&lcKeyNo = ON("KEY",("F"+lcKeyNo))
ENDFOR  
PUSH KEY
ON KEY
FOR lnKeyNo = 1 TO 10
  lcKeyNo = ALLTRIM(STR(lnKeyNo))
  lcKey   = "F"+lcKeyNo + ' ' + lcF&lcKeyNo
  ON KEY LABEL &lcKey 
ENDFOR
*B600320,1 MAN 05/28/95 (End)

IF llTempltOk
  qTopic = 'Notepad Template'
  DO lpEditTemp                                  && NotePad template.
ELSE
  qTopic = 'Notepad'
  DO lpEditNote WITH lcNoteType, lcNoteKey       && NotePad.
ENDIF

*B600320,1 MAN 05/28/95 (Begin) Fixed the bug of freezing the machine if you
*B600320,1              pressed ESC key from the invoice/order notes in
*B600320,1              the invoice inquire program.
POP KEY
*B600320,1 MAN 05/28/95 (End)

*:*************************************************************
*: Program file  : lpEditNote
*: Program desc. : Add\Edit the notepads.
*: For screen    : NotePad.
*:         System: A27.
*:         Module: Aria Apparel Series.
*:      Developer: Yasser Mohammed Aly (YMA).
*:*************************************************************

PROCEDURE lpEditNote
PARAMETER lcType, lcKey
PRIVATE   lcNoteDesc, lmNotes, lnAlias, llNoteUsed

*E300620,1 Reham On 06/15/97  *** Begin ***
*E300620,1 Add row for credit memos.
*C200027,7 M.H 07/07/96 Begin.
*DIMENSION laModes[3], laDataNote[14,4]
*DIMENSION laModes[3], laDataNote[17,4]

*--HDM B602730,1 04/06/1999[Start] Make a new key for return authorization program
*DIMENSION laModes[3], laDataNote[18,4]

*-- E301318,1 HDM [Start] New key for System Settings
*DIMENSION laModes[3], laDataNote[19,4]
*C101518,1 ASH 02/01/2000 (Begin) Add new type for commercial invoice.
*DIMENSION laModes[3], laDataNote[20,4]
DIMENSION laModes[3], laDataNote[21,4]

*:C200254,1 ABD - Make a new key for Material Manuf. 
*:C200254,1 ABD - Rework Order program. [Begin]
*N000385,1 TMI [START] Material Sales Order , Invoice order
*DIMENSION laModes[3], laDataNote[22,4]
DIMENSION laModes[3], laDataNote[24,4]
*N000385,1 TMI [START] Material Sales Order , Invoice order
*:C200254,1 ABD - [End]



*C101518,1 ASH 02/01/2000 (End) 

*-- E301318,1 HDM [End]

*--HDM B602730,1 04/06/1999[End]

*C200027,7 M.H 07/07/96 End.
*E300620,1 Reham On 06/15/97  *** End   ***

IF _WINDOWS 
  *E300457,1 Reham On 09/22/96   ** Begin **
  *E300457,1 Enhance the bitmaps used in this screen.

   *E300637,1 WAM 04/08/97 Use the directories pathes
*  EditBmp   = qBd + "EDIT2.BMP"
*  CopyBmp   = qBd + "COPY1.BMP"
*  SaveBmp   = qBd + "Save1.BMP"
*  CloseBmp  = qBd + "Cls.BMP"
*  CancelBmp = qBd + "Cancel.BMP"
*  NextBmp   = qBd + "Next.BMP"
*  PrevBmp   = qBd + "PRIOR.BMP"
*  *Copy1Bmp  = qBd + "COPY1.BMP"
*  *ClsBmp    = qBd + "Cls.BMP"
  *B800149,1 Add a Print button prompt.
*  PrntBmp   = qBd + "Printer.BMP"

  EditBmp   = gcBmpHome + "EDIT2.BMP"
  CopyBmp   = gcBmpHome + "COPY1.BMP"
  SaveBmp   = gcBmpHome + "Sav.BMP"
  CloseBmp  = gcBmpHome + "Cls.BMP"
  CancelBmp = gcBmpHome + "Can.BMP"
  NextBmp   = gcBmpHome + "Next2.BMP"
  PrevBmp   = gcBmpHome + "PRIOR2.BMP"
  PrntBmp   = gcBmpHome + "Printer.BMP"
  *E300637,1 (End)

  *B800149,1 end.
  *E300457,1 Reham On 09/22/96   ** End **
ELSE
   *B600822,1 Hesham El-Sheltawi (Start)
   *B600822,1 replace setting of color set with global function
*  SET COLOR SET TO APPARELSET
   =gfSetClrSet() && Set the Color set of the screen
   *B600822,1 Hesham El-Sheltawi (End)
ENDIF  

*B500669,1 MFM 05/03/95 (Begin) Added to change the color of the higlight
*B500669,1 MFM          to the same color of the edit color for visual effact.
IF _DOS
  lcOldSchem2 = SCHEME(2)             && Save the old color schem 2.
  lcNewSchem2 = ",,,,," + SCHEME(2,2) + ",,,,,"
  SET COLOR OF SCHEME 2 TO &lcNewSchem2
  *B500669,1 MFM 05/03/95 (End).
ENDIF  

lcWinClr   = 1
lcToEdit   = gfTempName()
lcTmpFile  = gfTempName()
lcDeleteSt = SET ("DELETE")
lcExactSt  = SET ("EXACT")
lcSelCont  = SCHEME(1,2)
lcEnbCont  = SCHEME(1,2)
lcDisCont  = SCHEME(1,2)
lcOldCpKey = SPACE(12)
lcUsedCode = SPACE(01)
laDataNote     = SPACE(01)
lcProm     = SPACE(01)
lcNoteDesc = SPACE(01)
lcEditStat = SPACE(01)
*B606731,1 NAD (START)
lcPrtStat  = SPACE(01)
*B606731,1 NAD (END)
lcEditMode = SPACE(01)
lcCopyStat = SPACE(01)
lcSaveStat = SPACE(01)
lcInitNote = SPACE(01)
llNoteUsed = .T.
laModes  = .F.
llStmpUsed = .F.
llOtFrmEdt = .T.
llOtFrmCpy = .F.
*B601471,1 ASH 12/17/96 (Begin) Removed the flags not used anymore.
*B601168,1 SHA (Begin) 08/20/96 (Added)
*AcfFlag = .F.
*AcfFlag1 = .F.
*B601168,1 SHA (End)
*B601471,1 ASH 12/17/96 (End)

*B800149,1 Add a flag for print button.
llOtFrmPrn = .F.    
*B800149,1 end.

lcTranNum  = "01"
lnObject   = 2

SET DELETE ON
SET EXACT OFF 

lnAlias = SELECT()
= lfFillData()
= lfOpnFiles()
= lfPrepFils()

*E300637,1 Use the directories pathes
*DO (qSd + "NotePad.SPX")
*C200259,1 (Begin) Run Custom V.Fox Arabic notepad for Mexx.
IF laScrMode[4]
  =gfDoTriger('NOTEPAD',PADR('RUNNOTE',10))
ENDIF  
*C200259,1 (End)

DO NotePad.SPX
*E300637,1 (End)

= lfClnUpFil()
SELECT(lnAlias)

IF _DOS
  *B500669,1 MFM 05/03/95 (Begin) Added.
  SET COLOR OF SCHEME 2 TO &lcOldSchem2   && Restore the old color schem 2 
  *B500669,1 MFM 05/03/95 (End).
ENDIF  


*:*************************************************************
*: Program file  : lfGtStamp
*: Program desc. : Add\Edit the notepads.
*: For screen    : NotePad.
*:         System: A26.
*:         Module: Aria Apparel Series.
*:      Developer: Yasser Mohammed Aly (YMA).
*:*************************************************************

FUNCTION lfGtStamp
PRIVATE lcRet

*E300637,1 WAM 04/08/97 Use the directories pathes
*lcRet = CHR(13)+SPACE(2)+ALLTRIM(qUserId)+SPACE(1)+DTOC(DATE())+CHR(13)
lcRet = CHR(13)+SPACE(2)+ALLTRIM(gcUser_id)+SPACE(1)+DTOC(gdSysDate)+CHR(13)
*E300637,1 (End)

llStmpUsed = .T.
RETURN (lcRet)

*! -------------------------------------------------------------

FUNCTION lfGetSeq
PARAMETERS lcLastNum
RETURN PADL(ALLTRIM(STR(VAL(lcLastNum)+1,2)),2,"0")

*! -------------------------------------------------------------

FUNCTION lfPackFile
PARAMETERS llDelFirst
PRIVATE lnAlias

lnAlias = SELECT()
SELECT (lcTmpFile)        
*B601471,1 ASH 12/17/96 (Begin) Allways zap the temp file and add one record
*B601471,1 ASH          with transaction number "01".
*IF llDelFirst
*  DELETE ALL
*  APPEND BLANK
*  REPLACE mNotes   WITH &lcToEdit..mEdtNotes ,;
*          cTranNum WITH "01"
*  lcTranNum  = "01"
*ELSE
*  DELETE ALL FOR cTranNum <> "01"
*  lcTranNum  = "01"
*ENDIF  
DELETE ALL
APPEND BLANK
REPLACE mNotes   WITH &lcToEdit..mEdtNotes ,;
        cTranNum WITH "01"
lcTranNum  = "01"
PACK
PACK MEMO
*B601471,1 ASH 12/17/96 (End)

SELECT(lnAlias)

*! -------------------------------------------------------------

FUNCTION lfShow

DO CASE
 CASE laModes[1]
    
   lcEditStat = "ENABLE"
   *B606731,1 NAD (START)
   lcPrtStat  = "ENABLE" 
   *B606731,1 NAD (END)  
   lcProm     = IIF(_WINDOWS,CloseBmp,"\?\!\<Close")
   STORE "DISABLE" TO lcCopyStat, lcSaveStat
   
 CASE laModes[2] .OR. laModes[3]
   
  lcEditStat = "DISABLE"
   *B606731,1 NAD (START)
   lcPrtStat  = "DISABLE"
   *B606731,1 NAD (END)
   lcProm     = IIF(_WINDOWS,CancelBmp,"\?\!\<Cancel") 
   *B606731,1 NAD (Start)
   *STORE "ENABLE" TO lcCopyStat, lcSaveStat
   STORE IIF(IIF(EMPTY(gcAct_Comp),.F.,gfGetMemVar('LLHIST',gcAct_Comp)),"DISABLE","ENABLE") TO lcCopyStat, lcSaveStat
   *B606731,1 NAD (END)
ENDCASE

SHOW GET pbNoteQuit ,1 PROMPT (lcProm)
SHOW GET pbEdit     &lcEditStat
SHOW GET pbNoteCopy &lcCopyStat
SHOW GET pbNoteSave &lcSaveStat

*! -------------------------------------------------------------

FUNCTION lfvEdit
PRIVATE lnAlias

REPLACE &lcToEdit..mEdtNotes WITH &lcToEdit..mEdtNotes + lfGtStamp()

lnAlias = SELECT()
SELECT (lcTmpFile)        
lcTranNum = lfGetSeq(lcTranNum)
APPEND BLANK
REPLACE cTranNum WITH lcTranNum         ,;
        cType    WITH lcType            ,;
        cKey     WITH lcKey             ,;
        mNotes   WITH &lcToEdit..mEdtNotes ,;
        cDesc    WITH "The user stamp." ,;
        nMode    WITH 2
SELECT(lnAlias)

lcEditMode = ""
lnObject   = 1
llOtFrmEdt = .T.
STORE .F. TO laModes
laModes[2] = .T.
= lfShow()

*C200259,1 (Begin) Run Custom V.Fox Arabic notepad for Mexx.
=gfDoTriger('NOTEPAD',PADR('RUNNOTE',10))
llMex      = FILE("NOTES.DBF")
IF llMex
  RETURN
ENDIF
*C200259,1 (End)
  
CLEAR READ

*! -------------------------------------------------------------

FUNCTION lfvNoteCpy

llOtFrmCpy = .T.
llOtFrmEdt = .T.
lnObject   = 1
CLEAR READ

*! -------------------------------------------------------------

FUNCTION lfCopyNote
PRIVATE lnAlias

lnAlias = SELECT()
*B601471,1 ASH 12/17/96 (Begin) The following lines is commented out.
*B601168,1 SHA (Begin) 08/20/96 (Added)
*SELECT (lcTmpFile)        
*REPLACE mNotes   WITH &lcToEdit..mEdtNotes 
*IF AcfFlag =.T.
*  =lfOpnFiles()
*  =lfVedit()
*ENDIF
*AcfFlag =.T.
*B601471,1 ASH 12/17/96 (End)

SELECT (lcTmpFile)        
lcTranNum = lfGetSeq(lcTranNum)
APPEND BLANK
*B601471,1 ASH 12/17/96 (Begin) Changed the description from "The user stamp"
*B601471,1 ASH          to be "Copy Session", to be used to mark the record 
*B601471,1 ASH          as the first action done when entering the copy mode.

REPLACE cTranNum WITH lcTranNum         ,;
        cType    WITH lcType            ,;
        cKey     WITH lcKey             ,;
        mNotes   WITH &lcToEdit..mEdtNotes ,;
        cDesc    WITH "Copy Session" ,;
        nMode    WITH 2

*SELECT(lnAlias)
*B601168,1 SHA (End) 
*B601471,1 ASH 12/17/96 (End)

SELECT NotePad
= SEEK("*")
lcCpNtDesc = ""
lcCpPrompt = PADR(lfGetProm(lcType),17)
lcCpType   = lcType
puCpType   = AT(lcType,lcUsedCode)
lcCpTypDsc = lfGetProm(lcType)
lcCpKey    = SPACE(12)
lcCpPrevSt = "DISABLE"
lcCpNextSt = "DISABLE"
lcCpCopySt = "DISABLE"
lcOldTran  = lcTranNum
rbCpMode   = 2

*E300637,1 Use the directories pathes
*Do (qSd + "NoteChil.SPX")
Do NoteChil.SPX
*E300637,1 (End)

= SEEK(lcType+lcKey, "NotePad")

*B500669,1 MFM 05/03/95 (Begin) Added.
IF RECNO(lcTmpFile) > RECCOUNT(lcTmpFile)
  SKIP -1 IN &lcTmpFile
ELSE
  GOTO RECNO(lcTmpFile) IN &lcTmpFile
ENDIF
*B500669,1 MFM 05/03/95 (End).

llNotesUpd = IIF(lcOldTran = &lcTmpFile..cTranNum,.F.,.T.)
IF llNotesUpd 
  IF gfDialog("?","Proceed to replace the original note with the "+;
              "copied one, Cancel to preserve it.",;
              "\<Proceed;\<Cancel") = 1
    REPLACE &lcToEdit..mEdtNotes WITH &lcTmpFile..mNotes
    = lfPackFile(.F.)
  ELSE
    SELECT (lcTmpFile)
    = SEEK(lcOldTran)
    *B601471,1 ASH 12/17/96 (Begin) Update the temp file with the first 
    *B601471,1 ASH          action has been done before going to the copy 
    *B601471,1 ASH          mode and pack the rest of the records.

    *IF !EOF()
    *  SKIP
    *  DELETE REST
    *ENDIF
    *lcOldTran = lcTranNum
    *GOTO BOTTOM
    *REPLACE &lcToEdit..mEdtNotes WITH &lcTmpFile..mNotes
    REPLACE &lcToEdit..mEdtNotes WITH &lcTmpFile..mNotes
    = lfPackFile(.F.)
    *B601471,1 ASH 12/17/96 (End)
    
  ENDIF  
  SHOW GET &lcToEdit..mEdtNotes
ENDIF
= lfShow()
SELECT(lnAlias)

*! -------------------------------------------------------------

FUNCTION lfvNoteSav
= lfSavRec()
lcEditMode = "NOEDIT"
lnObject   = 2
llOtFrmEdt = IIF(laModes[2], .T., .F.)
STORE .F. TO laModes
laModes[1] = .T.
= lfShow()
llStmpUsed = .F.
CLEAR READ

*! -------------------------------------------------------------

FUNCTION lfSavRec
*B803865,1 MAB 12/13/2000 Update lHasNotes Field to Correct the notes show button.

PRIVATE lnAlias , lnPrevAl , lcNotes , lcTmpNotes , lcExStat , llDontSav, lnTagNo , lnLastPos


STORE .F. TO llDontSav
STORE 0 TO lnLastPos
lnLastPos = ATC('|',lcSydKey)
*--GET TAG NUMBER
IF lnLastPos <> 0
  lnTagNo = gfGetTagNo(lcBaseFile,SUBSTR(ALLTRIM(lcSydKey),lnLastPos + 1))
  *lnTagNo   = INT(VAL(SUBSTR(ALLTRIM(lcSydKey),lnLastPos + 1)))
ENDIF
lnAlias = SELECT()
SELECT NotePad
*B800093,1 Append a record if not called from templates. 
*B800093,1 Moved the following part into the next IF statement.
*IF laModes[3]
*  APPEND BLANK
*ENDIF
*B800093,1 end.

*C200027,1 MFM 06/17/96 (Begin) Changed the program to cover the 
*C200027,1 MFM          general purpose letters
*IF lcType = 'T'
IF lcType $ 'TXY'
*C200027,1 MFM 06/17/96 (End).
  REPLACE mNotes WITH &lcToEdit..mEdtNotes
ELSE
  
  *B800093,1 Append a record if not called from templates.
  *IF laModes[3]
  IF laModes[3] .AND. !EMPTY(&lcToEdit..mEdtNotes)
    APPEND BLANK
  ENDIF
  *B800093,1 end.

  *--E301176,1 HDM 03/21/1999 Also Replace the {lHasNotes} field in base file[Start]
  *REPLACE Type   WITH lcType     ,;
           Key    WITh lcKey      ,;
           cDesc  WITH lcNoteDesc ,;
           mNotes WITH &lcToEdit..mEdtNotes


  IF ATC('*',lcSydKey) <> 0
    lnStarPos = ATC('*',lcSydKey)
    lcFileKey = ALLTRIM(SUBSTR(lcSydKey,lnStarPos+1,lnLastPos - lnStarPos-1))
  ELSE
    lcFileKey = 'lcKey'
  ENDIF

  lcNotes    = ALLTRIM(NotePad.mNotes)
  lcTmpNotes = ALLTRIM(&lcToEdit..mEdtNotes)
  lcExStat = SET('EXACT')
  SET EXACT ON
  
  IF (lcTmpNotes = lcOldValue) AND !llNotesUpd
    llDontSav = .T.
  ENDIF

  IF EMPTY(lcTmpNotes)
    SELECT NOTEPAD
    IF !EOF()
      DELETE
      *-- Save base file Environment
      lnPrevAl = SELECT(0)
      SELECT (lcBaseFile)
      *B803054,1 RAMY [START]
      *lnBaseRec = RECNO()
      lcSeek = EVALUATE(KEY())
      *B803054,1  RAMY [END]
      *B803249,1 AMM get setting of exact
      lcStExact = SET('EXACT')
      *B803249,1 AMM end
      SET EXACT OFF

      lcXXKey = EVAL(lcFileKey)
      *B803698,1 AAN Remove duplicate line[Start]
      *lcXXKey = EVAL(lcFileKey)
      *B803698,1 AAN Remove duplicate line[End]

      *B803157,1 Reham On 03/29/2000    *** Begin ***
      *B803157,1 Change the way of saving by using the scan command Instead
      *B803157,1 of the replace all command to optimise the saving time.
      *REPLACE ALL lHasNotes WITH .F. ;
      *        FOR EVAL(KEY(lnTagNo)) = lcXXKey
      *B803698,1 AAN Remove (GO TOP) Stat. and change the scan condition format[Start].
      *GO TOP
      *SCAN FOR EVAL(KEY(lnTagNo)) = lcXXKey
        *REPLACE lHasNotes WITH .F.
      *ENDSCAN

      lcUpdKey = KEY(lnTagNo) + "='" + lcxxkey+ "'"
      
      SCAN FOR &lcUpdKey 
        REPLACE lHasNotes WITH .F.
      ENDSCAN
      *B803698,1 AAN Remove (GO TOP) Stat. and change the scan condition format[End].
      *B803157,1 Reham On 03/29/2000    *** End   ***
      
      *B803249,1 AMM comment this line to go back the previous setting of exact
      *SET EXACT ON
      *B803249,1 AMM end
      SHOW GET pbNotePad,1  PROMPT gcBMPHome + "Notes2.BMP"  level RDLEVEL()-1
      *-- Restore BaseFile Environment
      *B803054,1 RAMY [start]
      *GO lnBaseRec
      =SEEK(lcSeek)
      *B803249,1 AMM go back to the previous setting of exact
      SET EXACT &lcStExact
      *B803249,1 AMM end
      *B803054,1 RAMY [END]
      SELECT(lnPrevAl)
    ENDIF
  ELSE
    IF !llDontSav
      REPLACE Type   WITH lcType     ,;
              Key    WITh lcKey      ,;
              cDesc  WITH lcNoteDesc ,;
              mNotes WITH &lcToEdit..mEdtNotes


      *-- Save base file Environment
      lnPrevAl = SELECT(0)
      SELECT (lcBaseFile)
      *B803054,1 RAMY [start]
      *lnBaseRec = RECNO()
      lcSeek = EVALUATE(KEY())
      *B803054,1  RAMY [END]
      *B803249,1 AMM save the setting
      lcSetExact = SET('EXACT')
      *B803249,1 AMM end
      SET EXACT OFF
      lcXXKey = EVAL(lcFileKey)

      *B803157,1 Reham On 03/29/2000    *** Begin ***
      *B803157,1 Change the way of saving by using the scan command Instead
      *B803157,1 of the replace all command to optimise the saving time.
      *REPLACE ALL lHasNotes WITH .T. ;
      *        FOR EVAL(KEY(lnTagNo)) = lcXXKey
      *B803698,1 AAN Remove (GO TOP) Stat. and change the scan condition format[Start].
      *GO TOP
      *SCAN FOR EVAL(KEY(lnTagNo)) = lcXXKey
        *REPLACE lHasNotes WITH .T.
      *ENDSCAN

      lcUpdKey = KEY(lnTagNo) + "='" + lcxxkey+ "'"
      SCAN FOR &lcUpdKey 
        *B803865,1 MAB 12/13/2000 Update lHasNotes Field to Correct the notes show button [Begin] 
        *REPLACE lHasNotes WITH .F.
        REPLACE lHasNotes WITH .T.
        *B803865,1 MAB 12/13/2000 Update lHasNotes Field to Correct the notes show button [End  ] 
      ENDSCAN
      *B803698,1 AAN Remove (GO TOP) Stat. and change the scan condition format[End].

      *B803157,1 Reham On 03/29/2000    *** End   ***
      *B803249,1 AMM comment out
      *SET EXACT ON
      *B803249,1 AMM end
      SHOW GET pbNotePad,1  PROMPT gcBMPHome + "Notes2_F.BMP"  level RDLEVEL()-1
      *-- restore base file Environment
      *B803054,1 RAMY [start]
      *GO lnBaseRec
      =SEEK(lcSeek)
      *B803249,1 AMM go back to the previous setting of exact 
      SET EXACT &lcSetExact
      *B803249,1 AMM end
      *B803054,1 RAMY [END]
      SELECT(lnPrevAl)

    ELSE
      IF laModes[3]
        DELETE
      ENDIF
    ENDIF
  ENDIF


  SET EXACT &lcExStat
  *--E301176,1 HDM 03/21/1999 HDM Also Replace the {lHasNotes} field in base file[End]
ENDIF
= lfPackFile(.T.)
SELECT(lnAlias)

*! -------------------------------------------------------------

FUNCTION lfvNoteQut
PRIVATE lnAlias

lnAlias    = SELECT()

*B500669,1 MFM 05/03/95 (Begin) Changed the condition.
llNotesUpd = IIF(&lcToEdit..mEdtNotes == NotePad.mNotes+lfGtStamp(), .F., .T.)
*llNotesUpd = IIF(&lcToEdit..mEdtNotes == NotePad.mNotes, .F., .T.)
*B500669,1 MFM 05/03/95 (End).

DO CASE
  CASE laModes[1]
    llOtFrmEdt = .F.
    CLEAR READ

  CASE laModes[2]
    IF !(llNotesUpd AND gfDialog("!","You are about to lose all changes ! Are you sure ?","\<Yes;\<No")=2)
      SELECT (lcTmpFile)
      *B601471,1 ASH 12/17/96 (Begin) Zap the temp file and reload the 
      *B601471,1 ASH          original notes from the master notepad file.
      *DELETE ALL FOR cTranNum <> "01"
      *GOTO TOP
      *REPLACE &lcToEdit..mEdtNotes WITH &lcTmpFile..mNotes
            = SEEK(lcType+lcKey, "NotePad")
      lnObject     = 2
      REPLACE &lcToEdit..mEdtNotes WITH NotePad.mNotes
      ZAP
      APPEND BLANK
      REPLACE cTranNum WITH "01"              ,;
              cType    WITH lcType            ,;
              cKey     WITH lcKey             ,;
              mNotes   WITH &lcToEdit..mEdtNotes ,;
              cDesc    WITH "The user stamp." ,;
              nMode    WITH 2
      *B601471,1 ASH 12/17/96 (End)
      
      STORE .F. TO laModes
      laModes[1] = .T.
      = lfShow()
      llOtFrmEdt = .T.
      lcEditMode = "NOEDIT"
      lnObject   = 1
      CLEAR READ
    ELSE
      _CUROBJ = OBJNUM(&lcToEdit..mEdtNotes)
    ENDIF

  CASE laModes[3]

    *B500669,1 MFM 05/03/95 (Begin) Changed the condition.
    IF llNotesUpd        && !EMPTY(&lcTmpFile..mNotes) 
    *B500669,1 MFM 05/03/95 (End).

      lnChoice = gfDialog ("?","Would you like to save changes to the notepad ?",;
                           "\<Yes;\<No;\<Cancel")
      IF lnChoice = 3
        _CUROBJ = OBJNUM(&lcToEdit..mEdtNotes)
      ELSE
        IF lnChoice = 1
          = lfSavRec()
        ENDIF
        llOtFrmEdt = .F.
        CLEAR READ
      ENDIF
    ELSE
      llOtFrmEdt = .F.
      CLEAR READ
    ENDIF
ENDCASE

SELECT(lnAlias)

*! -------------------------------------------------------------

FUNCTION lfwibBrow

lcOldCpKey = lcCpKey
RETURN MDOWN()

*! -------------------------------------------------------------

FUNCTION lfvibBrow
PRIVATE lnAlias

IF EMPTY (lfChkCode())
  _CUROBJ = OBJNUM(lcCpKey)
ELSE
  = lfvCpKey()
ENDIF

*! -------------------------------------------------------------

FUNCTION lfGetProm
PARAMETERS lcPrmType
PRIVATE lcRet
SET EXACT ON
lcRet = IIF(lcPrmType $ lcUsedCode,laDataNote[ASCAN(laDataNote,lcPrmType)-1],"")
SET EXACT OFF
RETURN lcRet

*! -------------------------------------------------------------

FUNCTION lfGetDesc
PARAMETERS lcPrmType, lcPrmKey

PRIVATE lcRet
SET EXACT ON
lcRet = IIF(lcPrmType $ lcUsedCode,laDataNote[ASCAN(laDataNote,lcPrmType)+1],"")
SET EXACT OFF

*B500669,1 MFM 05/03/95 (Begin) Changed the returned value.
*RETURN lcRet + lcPrmKey
RETURN PROPER(lcRet) + UPPER(lcPrmKey)
*B500669,1 MFM 05/03/95 (End).

*! -------------------------------------------------------------

FUNCTION lfGetName
PARAMETERS lcPrmType
PRIVATE lcRet
SET EXACT ON
lcRet = IIF(lcPrmType $ lcUsedCode,laDataNote[ASCAN(laDataNote,lcPrmType)+2],"")
SET EXACT OFF
RETURN lcRet

*! -------------------------------------------------------------

FUNCTION lfvCpNext
PRIVATE lnAlias

lnAlias = SELECT()
SELECT NotePad
SKIP +2
lcCpNextSt = IIF(EOF(),"DISABLE","ENABLE")
lcCpPrevSt = IIF(BOF(),"DISABLE","ENABLE")
SKIP -1  

*B500669,1 MFM 05/03/95 (Begin) Changed the function calling.
*cCpNtDesc = PROPER(lfGetDesc(Type,Key))
lcCpNtDesc = lfGetDesc(Type,Key)
*B500669,1 MFM 05/03/95 (End).

lcCpPrompt = PADR(lfGetProm(Type),17)
lcCpKey    = Key
lcCpType   = Type
puCpType   = AT(Type,lcUsedCode)
lcCpTypDsc = lfGetProm(Type)
SHOW GET pbCpNext &lcCpNextSt
SHOW GET pbCpPrev &lcCpPrevSt
SHOW GET NotePad.mNotes
SHOW GET lcCpKey
SHOW GET lcCpType
SHOW GET lcCpType
SHOW GET lcCpTypDsc
= lfCpShow()
SELECT(lnAlias) 
_CUROBJ = OBJNUM(mNotes)

*! -------------------------------------------------------------

FUNCTION lfvCpPrev
PRIVATE lnAlias

lnAlias = SELECT()
SELECT NotePad
SKIP -2
lcCpNextSt = IIF(EOF(),"DISABLE","ENABLE")
IF BOF() .OR. Type="*"
  lcCpPrevSt = "DISABLE"
  IF Type="*"
    SKIP 1
  ENDIF
ELSE 
  SKIP 1
  lcCpPrevSt = "ENABLE"
ENDIF  

lcCpKey    = Key
lcCpType   = Type
puCpType   = AT(Type,lcUsedCode)

*B500669,1 MFM 05/03/95 (Begin) Changed the function calling.
*lcCpNtDesc = PROPER(lfGetDesc(lcCpType,lcCpKey))
lcCpNtDesc = lfGetDesc(lcCpType,lcCpKey)
*B500669,1 MFM 05/03/95 (End).

lcCpPrompt = PADR(lfGetProm(lcCpType),17)
lcCpTypDsc = lfGetProm(lcCpType)
SHOW GET pbCpNext &lcCpNextSt
SHOW GET pbCpPrev &lcCpPrevSt
SHOW GET NotePad.mNotes
SHOW GET lcCpKey
SHOW GET lcCpType
SHOW GET puCpType
SHOW GET lcCpTypDsc
= lfCpShow()
SELECT(lnAlias) 
_CUROBJ = OBJNUM(mNotes)

*! -------------------------------------------------------------

FUNCTION lfvCpCpy
PRIVATE lnAlias, llGoOn

llGoOn = .F.
lnAlias = SELECT()
SELECT (lcTmpFile)
IF rbCpMode = 1
  IF gfDialog("?","You are about to replace the original note with the selected one! Replace ?","\<Yes;\<No")=1
    llGoOn = .T.
    lcTranNum = lfGetSeq(lcTranNum)
    REPLACE &lcToEdit..mEdtNotes WITH NotePad.mNotes
    APPEND BLANK
    REPLACE mNotes   WITH &lcToEdit..mEdtNotes ,;
            cKey     WITH NotePad.Key      ,;
            cType    WITh NotePad.Type     ,;
            cTranNum WITH lcTranNum         ,;
            cDesc    WITH "Replacing " + PROPER(lfGetDesc(NotePad.Type,NotePad.Key)) ,;
            nMode    WITH 1
  ENDIF  
ELSE
  lcTranNum = lfGetSeq(lcTranNum)
  REPLACE &lcToEdit..mEdtNotes WITH &lcToEdit..mEdtNotes+CHR(13)+NotePad.mNotes
  APPEND BLANK
  REPLACE mNotes   WITH &lcToEdit..mEdtNotes ,;
          cKey     WITH NotePad.Key      ,;
          cType    WITh NotePad.Type     ,;
          cTranNum WITH lcTranNum         ,;
          cDesc    WITH "Appending " + PROPER(lfGetDesc(NotePad.Type,NotePad.Key)) ,;
          nMode    WITH 2
  WAIT "Notes have been copied" WINDOW TIMEOUT 0.1
  llGoOn = .T.
ENDIF
IF llGoOn
  SHOW GET pbCpUnDo    ENABLE
  SHOW GET pbCpUnDoAll ENABLE
ENDIF
SELECT(lnAlias) 

*! -------------------------------------------------------------

FUNCTION lfvCpType
PRIVATE lnAlias 

lnAlias = SELECT()

DO CASE
  CASE _DOS
    *B600434,1 Malak 6-12-95  {Begin}
    ** Fix the popup color in the copy screen.
   *B600822,1 Hesham El-Sheltawi (Start)
   *B600822,1 replace setting of color set with global function
*  SET COLOR SET TO APPARELSET
   =gfSetClrSet() && Set the Color set of the screen
   *B600822,1 Hesham El-Sheltawi (End)
    lcCpType = gfActPop (2,21,17,40,"laDataNote",2,1,@lcCpTypDsc)
    SET COLOR OF SCHEME 2 TO &lcNewSchem2
    *B600434,1 Malak 6-12-95  {End}    
    
  CASE _WINDOWS
    SHOW GET puCpType
    lcCpType = laDataNote[puCpType,2]
ENDCASE

IF !EMPTY(lcCpType)
  lcCpPrompt = PADR(lfGetProm(lcCpType),17)
ENDIF
= SEEK("*")
lcCpNtDesc = ""
lcCpKey    = SPACE(12)
lcCpPrevSt = "DISABLE"
lcCpNextSt = "DISABLE"
lcCpCopySt = "DISABLE"
SHOW GET pbCpNext &lcCpNextSt
SHOW GET pbCpPrev &lcCpPrevSt
SHOW GET pbCpCopy &lcCpCopySt
SHOW GET lcCpTypDsc
SELECT NotePad
SHOW GET mNotes
= lfCpShow()
_CUROBJ = OBJNUM(lcCpKey)
SELECT(lnAlias)

*! -------------------------------------------------------------

FUNCTION lfvCpKey
PRIVATE lnAlias 

lnAlias = SELECT()
SELECT NotePad
IF !EMPTY(lcCpKey) 
  IF !SEEK(lcCpType+lcCpKey) AND EMPTY(lfChkCode())
    _CUROBJ = OBJNUM(lcCpKey)
    RETURN .T.
  ENDIF
  *B601471,1 ASH 12/17/96 (Begin) Adjust the master notepad file pointer to 
  *B601471,1 ASH          the selected key record.
  = SEEK (lcCpType+lcCpKey, "NotePad")
  *B601471,1 ASH 12/17/96 (End)
  
  = lfNxtPrvSt()
  lcCpKey    = Key
  
  *B500669,1 MFM 05/03/95 (Begin) Changed the function calling.
  *lcCpNtDesc = PROPER(lfGetDesc(lcCpType,lcCpKey))
  lcCpNtDesc = lfGetDesc(lcCpType,lcCpKey)
  *B500669,1 MFM 05/03/95 (End).
  
  lcCpCopySt = "ENABLE"
ELSE
  = SEEK("*")
  lcCpKey    = SPACE(12)
  lcCpNtDesc = ""
  lcCpNextSt = "DISABLE"
  lcCpPrevSt = "DISABLE"
  lcCpCopySt = "DISABLE"
ENDIF
SHOW GET mNotes
SHOW GET lcCpKey
SHOW GET pbCpNext &lcCpNextSt
SHOW GET pbCpPrev &lcCpPrevSt
SHOW GET pbCpCopy &lcCpCopySt
= lfCpShow()
SELECT(lnAlias)

*! -------------------------------------------------------------

FUNCTION lfNxtPrvSt
PRIVATE lnAlias

lnAlias = SELECT()
SELECT NotePad
IF BOF() 
  lcCpPrevSt = "DISABLE"
ELSE
  SKIP -1
  lcCpPrevSt = IIF(BOF().OR.Type="*","DISABLE","ENABLE")
  SKIP 1
ENDIF
IF EOF()
  lcCpNextSt = "DISABLE"
ELSE
  SKIP 1
  lcCpNextSt = IIF(EOF(),"DISABLE","ENABLE")
  SKIP -1
ENDIF
SELECT(lnAlias)

*! -------------------------------------------------------------

*B601471,1 ASH 12/17/96 (Begin) Commented out and rewrite the function.
*FUNCTION lfvCpUnDo
*PRIVATE lnAlias
*WAIT "Undo... " + ALLTRIM(&lcTmpFile..cDesc) WINDOW NOWAIT
*lnAlias = SELECT()
*SELECT(lcTmpFile)
*DELETE
*SKIP -1
*B600408,1 Malak 6-12-95  Begin
* Fix bug if there is no records in the temp file (lcTmpFile).
*= SEEK(IIF(&lcTmpFile..cTranNum='01','*',cType+cKey),"NotePad")
*B600408,1 Malak 6-12-95  End
*= lfNxtPrvSt()
*lcCpType   = cType
*puCpType   = AT(cType,lcUsedCode)
*lcCpKey    = cKey
*B500669,1 MFM 05/03/95 (Begin) Changed the function calling.
*lcCpNtDesc = PROPER(lfGetDesc(lcCpType,lcCpKey))
*lcCpNtDesc = lfGetDesc(lcCpType,lcCpKey)
*B500669,1 MFM 05/03/95 (End).
*lcCpPrompt = PADR(lfGetProm(lcCpType),17)
*lcCpTypDsc = lfGetProm(lcCpType)
*rbCpMode   = nMode
*REPLACE &lcToEdit..mEdtNotes WITH &lcTmpFile..mNotes
*IF cDesc = "The user stamp."
*  SHOW GET pbCpUnDo    DISABLE
*  SHOW GET pbCpUnDoAll DISABLE
*ENDIF
*SHOW GET lcCpType
*SHOW GET puCpType
*SHOW GET lcCpKey
*SHOW GET rbCpMode
*SHOW GET pbCpNext &lcCpNextSt
*SHOW GET pbCpPrev &lcCpPrevSt
*SHOW GET pbCpCopy &lcCpCopySt
*SHOW GET pbCpReDo    ENABLE
*SHOW GET pbCpReDoAll ENABLE
*SELECT NotePad
*SHOW GET mNotes
*= lfCpShow()
*SELECT(lnAlias)
*WAIT CLEAR
*B601471,1 ASH 12/17/96 (End)

*! -------------------------------------------------------------

FUNCTION lfvCpUnDoA
PRIVATE lnAlias

lnAlias = SELECT()
SELECT(lcTmpFile)
*B601471,1 ASH 12/17/96 (Begin) Change the condition to check the description 
*B601471,1 ASH          instead of the transaction number, because we do not 
*B601471,1 ASH          know exactly which transaction number was the first
*B601471,1 ASH          action done prior to entering the copy mode.
*DO WHILE cTranNum <> "02"
DO WHILE UPPER(ALLTRIM(cDesc)) <> "COPY SESSION"
*B601471,1 ASH 12/17/96 (End)

= lfvCpUnDo()
ENDDO
SELECT (lnAlias)


*! -------------------------------------------------------------

FUNCTION lfvCpReDo
PRIVATE lnAlias

WAIT "Redo... " + ALLTRIM(&lcTmpFile..cDesc) WINDOW NOWAIT
SET DELETE OFF
lnAlias = SELECT()
SELECT(lcTmpFile)
SKIP 2
IF EOF()
  SHOW GET pbCpReDo    DISABLE
  SHOW GET pbCpReDoAll DISABLE
ENDIF
SKIP -1  
RECALL
= SEEK(cType+cKey,"NotePad")
= lfNxtPrvSt()
lcCpType   = cType
puCpType   = AT(cType,lcUsedCode)
lcCpKey    = cKey

*B500669,1 MFM 05/03/95 (Begin) Changed the function calling.
*lcCpNtDesc = PROPER(lfGetDesc(lcCpType,lcCpKey))
lcCpNtDesc = lfGetDesc(lcCpType,lcCpKey)
*B500669,1 MFM 05/03/95 (End).

lcCpPrompt = PADR(lfGetProm(lcCpType),17)
lcCpTypDsc = lfGetProm(lcCpType)
rbCpMode   = nMode
REPLACE &lcToEdit..mEdtNotes WITH &lcTmpFile..mNotes
SHOW GET lcCpType
SHOW GET puCpType
SHOW GET lcCpKey
SHOW GET rbCpMode
*B601471,1 ASH 12/17/96 (Begin) Enable the copy button.
lcCpCopySt = "ENABLE"          
*B601471,1 ASH 12/17/96 (End)

SHOW GET pbCpNext &lcCpNextSt
SHOW GET pbCpPrev &lcCpPrevSt
SHOW GET pbCpCopy &lcCpCopySt
SHOW GET pbCpUnDo    ENABLE
SHOW GET pbCpUnDoAll ENABLE
SELECT NotePad
SHOW GET mNotes
= lfCpShow()
SELECT (lnAlias)
SET DELETE ON
WAIT CLEAR

*! -------------------------------------------------------------

FUNCTION lfvCpReDoA
PRIVATE lnAlias

lnAlias = SELECT()
SELECT(lcTmpFile)
DO WHILE lcTranNum <> cTranNum
  = lfvCpReDo()
ENDDO
SELECT (lnAlias)

*! -------------------------------------------------------------

FUNCTION lfChkCode
PRIVATE lnAlias,lcBrFields

lnAlias = SELECT()

IF SEEK(lcCpType,'NotePad')
  DIMENSION laTempArr[1]
  STORE SPACE(12) TO laTempArr
  lcHdr      = lfGetProm(lcCpType)
  lcBrFields = IIF(lcCpType<>"T",[key:H=lcHdr],[key:H=lcHdr,cDesc:H="Description"]) 
  SELECT 0 
  lcTmpNote = gfTempName()

  *Gehan use  lcDataDir if  passed,else, use gcDataDir
  *E300637,1 Use the directories pathes
  *USE QDD + 'NOTEPAD' ORDER TAG NOTEPAD AGAIN ALIAS (lcTmpNote)
  *USE gcDataDir + 'NOTEPAD' ORDER TAG NOTEPAD AGAIN ALIAS (lcTmpNote)  
   USE lcDataDir + 'NOTEPAD' ORDER TAG NOTEPAD AGAIN ALIAS (lcTmpNote)  
  *E300637,1 (End)
  *Gehan end
  DO ARIABROW WITH "FOR TYPE=lcCpType","Notes",gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,"",.f.,"key","laTempArr"
  lcCpKey    = laTempArr[1]
  IF USED(lcTmpNote)
    USE IN (lcTmpNote)
  ENDIF  
ELSE
  = gfDialog("I","No notes were found for any of the "+lfGetName(lcCpType)+".")
  lcCpKey  = SPACE(12)
ENDIF  
SELECT(lnAlias)
RETURN lcCpKey

*! -------------------------------------------------------------

FUNCTION lfFillData

*C200027 M.H Begin.
*lcUsedCode = "ABCFGPHIJULMTO"
*--HDM B602730,1 04/06/1999[Start] Make a new key for return authorization program
*lcUsedCode = "ABCFGPHIJULMTOXYS"

*-- E301318,1 HDM [Start] New key for System Settings
*lcUsedCode = "ABCFGPHIJULMTOXYSZ"
*C101518,1 ASH 02/01/2000 (Begin) Add new type for commercial invoice ('E').
*lcUsedCode = "ABCFGPHIJULMTOXYSZD"

*:C200254,1 ABD - Make a new key for Material Manuf. 
*:C200254,1 ABD - Rework Order program Add 'W'. [Begin]
*lcUsedCode = "ABCEFGPHIJULMTOXYSZDW"
*:C200254,1 ABD - [End]

*N000385,1 TMI [START] Material Sales Order & Invoice Sales order
lcUsedCode = "ABCEFGPHIJULMTOXYSZDWVN"
*N000385,1 TMI [END  ] Material Sales Order & Invoice Sales order

*C127341,1  TMI [Start] Add a new type chr for shipment 'Q'
lcUsedCode = "ABCEFGPHIJULMTOXYSZDWVNQ"
*C127341,1  TMI [End  ] 

*C101518,1 ASH 02/01/2000 (End)
*-- E301318,1 HDM [End]

*--HDM B602730,1 04/06/1999[end]
*C200027 M.H End.

* Prompt          1  x-1
* Description     3  x+1
* Name            4  x+2


laDataNote[01,1] = "Account Number"
laDataNote[01,2] = "A"             
laDataNote[01,3] = "Notes For Account Number : "             
laDataNote[01,4] = "Accounts"

laDataNote[02,1] = "Order Number"
laDataNote[02,2] = "B"             
laDataNote[02,3] = "Notes For Order Number : "             
laDataNote[02,4] = "Orders"

laDataNote[03,1] = "Invoice Number"
laDataNote[03,2] = "C"             
laDataNote[03,3] = "Notes For Invoice Number : "             
laDataNote[03,4] = "Invoices"             

laDataNote[04,1] = "Style Number"
laDataNote[04,2] = "F"             
laDataNote[04,3] = "Notes For Style Number : "             
laDataNote[04,4] = "Styles"

laDataNote[05,1] = "Item Number"
laDataNote[05,2] = "G"             
laDataNote[05,3] = "Notes For Item Number : "             
laDataNote[05,4] = "Items"

laDataNote[06,1] = "Style P/O Number"
laDataNote[06,2] = "P"             
laDataNote[06,3] = "Notes For Style P/O Number : "             
laDataNote[06,4] = "Style Purchase Orders"

laDataNote[07,1] = "Vendor Number"
laDataNote[07,2] = "H"             
laDataNote[07,3] = "Notes For Vendor Number : "             
laDataNote[07,4] = "Vendors"

laDataNote[08,1] = "C/T Number"
laDataNote[08,2] = "I"             
laDataNote[08,3] = "Notes For Cutting Ticket Number : "             
laDataNote[08,4] = "Cutting Tickets"

laDataNote[09,1] = "Salesrep Code"
laDataNote[09,2] = "J"             
laDataNote[09,3] = "Notes For Salesrep : "
laDataNote[09,4] = "Sales Representatives"

laDataNote[10,1] = "User Name"
laDataNote[10,2] = "U"             
laDataNote[10,3] = "Notes For User : "
laDataNote[10,4] = "Users"

laDataNote[11,1] = "L/C Number"
laDataNote[11,2] = "L"             
laDataNote[11,3] = "Notes For Letter of Credit : "
laDataNote[11,4] = "Letters of Credit"

laDataNote[12,1] = "Item P/O Number"
laDataNote[12,2] = "M"             
laDataNote[12,3] = "Notes For Item P/O Number : "
laDataNote[12,4] = "Item Purchase Orders"

laDataNote[13,1] = "Template"
laDataNote[13,2] = "T"             
laDataNote[13,3] = "Template : "
laDataNote[13,4] = "Templates"

laDataNote[14,1] = "Material Manufacturing Order"
laDataNote[14,2] = "O"             
laDataNote[14,3] = "Notes For Material Manufacturing Order Number :"
laDataNote[14,4] = "Material Manufacturing Orders"

*B601471,1 ASH 12/17/96 (Begin) Added.
laDataNote[15,1] = "Customer Letters"
laDataNote[15,2] = "X"
laDataNote[15,3] = "Customer General Purpose Letter : "             
laDataNote[15,4] = "General Purpose Letters"

laDataNote[16,1] = "Vendor Letters"
laDataNote[16,2] = "Y"
laDataNote[16,3] = "Vendor General Purpose Letter : "
laDataNote[16,4] = "General Purpose Letters"
*B601471,1 ASH 12/17/96 (End)

*B601471,1 ASH 12/17/96 (Begin) Changed laDataNote[15,1] to laDataNote[17,1].
*C200027,7 M.H 07/07/96 Begin.
laDataNote[17,1] = "Forcasting"
laDataNote[17,2] = "S"
laDataNote[17,3] = "Forcasting : "
laDataNote[17,4] = "Forcasting"
*C200027,7 M.H 07/07/96 End.
*B601471,1 ASH 12/17/96 (End)

*E300620,1 Reham On 06/15/97  *** Begin ***
laDataNote[18,1] = "Credit Memos"
laDataNote[18,2] = "R"
laDataNote[18,3] = "Credit Memos : "
laDataNote[18,4] = "Credit Memos"
*E300620,1 Reham On 06/15/97  *** End   ***


*--HDM B602730,1 04/06/1999[Start] Make a new key for return authorization program
laDataNote[19,1] = "Return Authorization"
laDataNote[19,2] = "Z"
laDataNote[19,3] = "Return Authorization: "
laDataNote[19,4] = "Return Authorization"

*-- E301318,1 HDM [Start] New key for System Settings
laDataNote[20,1] = "Customer System Settings"
laDataNote[20,2] = "D"
laDataNote[20,3] = "Customer : "
laDataNote[20,4] = "Customer System Settings"
*-- E301318,1 HDM [End]
*C101518,1 ASH 02/01/2000 (Begin) Add new type for commercial invoice ('E').
laDataNote[21,1] = "Commercial Invoice"
laDataNote[21,2] = "E"
laDataNote[21,3] = "Invoice : "
laDataNote[21,4] = "Commerical Invoice"
*C101518,1 ASH 02/01/2000 (End)
*--HDM B602730,1 04/06/1999[End]

*:C200254,1 ABD - Make a new key for Material Manuf. 
*:C200254,1 ABD - Rework Order program Add 'W'. [Begin]
laDataNote[22,1] = "Material Manuf. Rework Order"
laDataNote[22,2] = "W"
laDataNote[22,3] = "Notes For Material Manuf. Rework Order Number :"
laDataNote[22,4] = "Material Manuf. Rework Orders"
*:C200254,1 ABD - [End]

*N000385,1 TMI [START] Add note for Material sales order and materil Invoice Order
laDataNote[23,1] = "Material Sales Order"
laDataNote[23,2] = "V"
laDataNote[23,3] = "Notes For Material Sales Order Number :"
laDataNote[23,4] = "Material Sales Orders"
laDataNote[24,1] = "Material Invoice Order"
laDataNote[24,2] = "N"
laDataNote[24,3] = "Notes For Material Invoice Order Number :"
laDataNote[24,4] = "Material Invoice Orders"
*N000385,1 TMI [END  ] Add note for Material sales order and materil Invoice Order

*C127341,1  TMI [Start] Add note for Shipment
DIMENSION laDataNote[25,4]
laDataNote[25,1] = "Shipment"
laDataNote[25,2] = "Q"
laDataNote[25,3] = "Notes For Shipment :"
laDataNote[25,4] = "Shipment"
*C127341,1  TMI [End  ] 

*! -------------------------------------------------------------

FUNCTION lfOpnFiles

*B601471,1 ASH 12/17/96 (Begin) Commented out the if condition.
*B601168,1 SHA (Begin) Added if condition to create lcToEdit cursor only at 
*B601168,1 SHA		   the first time the function is accessed
*IF AcfFlag1 = .F.
  SELECT 0
  CREATE CURSOR (lcToEdit) (mEdtNotes M)
  APPEND BLANK
*ENDIF
*AcfFlag1 = .T.
*B601168,1 SHA (End)
*B601471,1 ASH 12/17/96 (End)

SELECT 0
*B601471,1 ASH 12/17/96 (Begin) Create the temp file as a DBF not a cursor
*B601471,1 ASH          just to be able to zap it at run time.
*CREATE CURSOR (lcTmpFile) (cTranNum C(2), cType C(1), cKey C(12), mNotes M, cDesc C(50), nMode N(1))

*E300637,1 WAM 04/08/97 Use the directories pathes
*CREATE DBF (qWd+lcTmpFile) (cTranNum C(2), cType C(1), cKey C(12), mNotes M, cDesc C(50), nMode N(1))
CREATE DBF (gcWorkDir+lcTmpFile) (cTranNum C(2), cType C(1), cKey C(12), mNotes M, cDesc C(50), nMode N(1))
*E300637,1 (End)

*B601471,1 ASH 12/17/96 (End)

*G600745,1 ARH 08/03/95 (Begin) Fixed the bug "FILE IS IN USE BY ANOTHER"
*G600745,1              if more than one user edited the same kind of notes
*G600745,1              in the same time.

*E300637,1 WAM 04/08/97 Use the directories pathes
*INDEX ON cTranNum TO (qWD+lcTmpFile) ADDITIVE
INDEX ON cTranNum TO (gcWorkDir+lcTmpFile) ADDITIVE
*E300637,1 (End)

*G600745,1 ARH 08/03/95 (End)

APPEND BLANK
REPLACE cTranNum WITH "01"   ,;
        cType    WITH lcType ,;
        cKey     WITH lcKey

lcTagName = IIF(USED('NOTEPAD') .AND. SYS(22,SELECT('NOTEPAD')) = 'EDINOTE','EDINOTE','NOTEPAD')
*Gehan use  lcDataDir if  passed,else, use gcDataDir
*E300637,1 Use the directories pathes
*llNoteUsed = gfOpenFile(QDD+'NOTEPAD',QDD+lcTagName,'SH')
*llNoteUsed = gfOpenFile(gcDataDir+'NOTEPAD',gcDataDir+lcTagName,'SH')

llNoteUsed = gfOpenFile(lcDataDir+'NOTEPAD',lcDataDir+lcTagName,'SH')

*E300637,1 (End)
*Gehan (End)
*llNoteUsed = gfOpenFile(QDD+'NOTEPAD',QDD+'NOTEPAD','SH')

IF !SEEK("*************")
  APPEND BLANK
  REPLACE Key  WITH REPLICATE("*", 12) ,;
          Type WITH "*"
ENDIF
GOTO TOP

*! -------------------------------------------------------------

FUNCTION lfPrepFils

SELECT NotePad
*MALAK {begin} Default is edit mode if there are no notes.
*IF SEEK(lcType+lcKey) 
IF SEEK(lcType+lcKey) AND !EMPTY(mNotes)
*MALAK {end}
  laModes[1] = .T.
  lcProm       = IIF(_WINDOWS,CloseBmp,"\?\!\<Close") 

  *B500669,1 MFM 05/03/95 (Begin) Changed the function calling.
  *lcNoteDesc  = PROPER(lfGetDesc(lcType,lcKey))
  lcNoteDesc   = lfGetDesc(lcType,lcKey)
  *B500669,1 MFM 05/03/95 (End).
  
  *B606731,1 NAD (Start)
  *lcEditStat   = "ENABLE"
  lcPrtState   =  "ENABLE"
  STORE IIF(IIF(EMPTY(gcAct_Comp),.F.,gfGetMemVar('LLHIST',gcAct_Comp)),"DISABLE","ENABLE") TO  lcEditStat 
  *B606731,1 NAD (END) 
  lcEditMode   = "NOEDIT"
  lnObject     = 2
  STORE "DISABLE" TO lcCopyStat, lcSaveStat
  SELECT (lcTmpFile)
  REPLACE &lcTmpFile..mNotes   WITH NotePad.mNotes
  REPLACE &lcToEdit..mEdtNotes WITH &lcTmpFile..mNotes
ELSE
  laModes[3] = .T.
  lcProm       = IIF(_WINDOWS,CancelBmp,"\?\!\<Cancel") 

  *B500669,1 MFM 05/03/95 (Begin) Changed the function calling.
  *lcNoteDesc  = PROPER(lfGetDesc(lcType,lcKey))
  lcNoteDesc   = lfGetDesc(lcType,lcKey)
  *B500669,1 MFM 05/03/95 (End).
  
  lcEditStat   = "DISABLE"
  *B606731,1 NAD (START)
  lcPrtStat   = "DISABLE"
  *B606731,1 NAD (END)
  
  *B606731,1 NAD (Start)
  STORE "ENABLE" TO lcCopyStat, lcSaveStat
  STORE IIF(IIF(EMPTY(gcAct_Comp),.F.,gfGetMemVar('LLHIST',gcAct_Comp)),"DISABLE","ENABLE") TO  lcCopyStat, lcSaveStat 
  *B606731,1 NAD (END)
  lcEditMode   = ""
  lnObject     = 1
  SELECT (lcTmpFile)
  REPLACE &lcToEdit..mEdtNotes WITH &lcTmpFile..mNotes + lfGtStamp()
  REPLACE cTranNum WITH "01"              ,;
          cType    WITH lcType            ,;
          cKey     WITH lcKey             ,;
          mNotes   WITH &lcToEdit..mEdtNotes ,;
          cDesc    WITH "The user stamp." ,;
          nMode    WITH 2

  *B500669,1 MFM 05/03/95 (Begin) Added the call of the function in order to
  *B500669,1              put the cursor at the last line in the edited area.
  =lfGoEnd()
  *B500669,1 MFM 05/03/95 (End).

ENDIF  
lcInitNote = &lcToEdit..mEdtNotes

*! -------------------------------------------------------------

FUNCTION lfClnUpFil

IF llNoteUsed
  USE IN NotePad
ENDIF

USE IN (lcToEdit)
USE IN (lcTmpFile)

*B601471,1 ASH 12/17/96 (Begin) Erase the generated temp file.

*E300637,1 WAM 04/08/97 Use the directories pathes
*ERASE (qWd+lcTmpFile+".DBF")
*ERASE (qWd+lcTmpFile+".FPT")
ERASE (gcWorkDir+lcTmpFile+".DBF")
ERASE (gcWorkDir+lcTmpFile+".FPT")
*B602179,1 AMM Erase temporary files when quitting the program
ERASE (gcWorkDir+lcTmpFile+".IDX")
*B602179,1 AMM  end

*E300637,1 (End)

*B601471,1 ASH 12/17/96 (End)


SET DELETE &lcDeleteSt
SET EXACT  &lcExactSt

*! -------------------------------------------------------------

PROCEDURE lpEditTemp
DO NOTTMP

*:************************************************************************
*: Program file  : lfGoEnd()
*: Program desc. : Go to the last line in the edit reigon.
*: For screen    : 
*:         System: A25.
*:         Module: Aria Apparel Series.
*:      Developer: Mohamed Fahmy Mohamed (MFM)
*:*************************************************************
FUNCTION lfGoEnd

*C200259,1 (Begin) Run Custom V.Fox Arabic notepad for Mexx.
IF !laScrMode[4] AND llMex
  _CUROBJ = OBJNUM(pbNoteSave)
  RETURN
ENDIF  
*C200259,1 (End)

_CUROBJ = OBJNUM(&lcToEdit..mEdtNotes)
KEYBOARD "{SHIFT+CTRL+END}"+"{END}"  

*!*************************************************************
*! Name      : lfvNotePrnt
*! Developer : RENEE - Renee Ezzat
*! Date      : 09/03/1995
*! Purpose   : Calls a function that prints the notepad 
*!             to screen/file/printer after clearing the READ CYCLE.
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Returns            :  None.
*!*************************************************************
*! Example            :  =lfvNotePrnt()
*B800149,1 Notes button validation function.
*!*************************************************************
FUNCTION lfvNotePrnt

llOtFrmEdt = .T.
llOtFrmPrn = .T.
lnObject   = _CUROBJ
CLEAR READ

*!*************************************************************
*! Name      : lfPrntNote
*! Developer : RENEE - Renee Ezzat
*! Date      : 09/03/1995
*! Purpose   : Prints the current notepad to screen/file/printer.
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Called From        : NOTEPAD.SPR CleanUp Section.
*!*************************************************************
*! Calls              : PROCEDURE SEL_DEV
*!                      PROCEDURE PRT_NOTE
*!                      PROCEDURE ENDPRT
*!*************************************************************
*! Returns            :  None.
*!*************************************************************
*! Example            :  =lfPrntNote()
*B800149,1 Prints notepad.
*!*************************************************************
FUNCTION lfPrntNote
*khalid
*PRIVATE lnCurAlias
PRIVATE lnCurAlias,lcSavDfDir
*khalid
*B800149,1 Prepare variables for printing
ROW        = 99
PAGENO     = 0 
TIME       = TIME()
XTITLE     = ''
R_WIDTH    = 'N'
R_TITLE    = lfGetDesc(lcType, lcKey)
XREPORT    = 'NOTEPAD'
*--hdm
Qcompany = gcCom_Name
*--hdm

lcOGWinTitl = ''

lnCurAlias = SELECT()

*B800149,1 Check if printing is not aborted.
*--HDM [Start] SEL_DEV No longer used
*IF SEL_DEV(.T.) .AND. XREPORT = 'NOTEPAD'

*khalid
lcSavDfDir = SET ('DEFAULT')
*khalid

IF XREPORT = 'NOTEPAD'
*--HDM [End]
  CLEAR TYPEAHEAD
  SET DEVICE TO SCREEN
  WAIT 'Report printing - <SPACE BAR> to abort' WINDOW NOWAIT
  SET DEVICE TO PRINT
  
  *B605079,1 Always change the color to the default one. [Begin]
  *DO PRT_NOTE WITH lcType, lcKey
  PRIVATE lcColorSet
  lcColorSet = SET('COLOR')
  SET COLOR TO &lcColorSet
  DO PRT_NOTE WITH lcType, lcKey
  *B605079,1 Always change the color to the default one. [End]
  
  S_CLOSEALL= .F.
  *DO ENDPRT
  lcOldDev = gcDevice
  gcDevice = 'PRINTER'
  DO ENDREPORT
  gcDevice = lcOldDev
  *B800149,1 Restore color setting if DOS or UNIX.
  IF _DOS .OR. _UNIX
   *B600822,1 Hesham El-Sheltawi (Start)
   *B600822,1 replace setting of color set with global function
*  SET COLOR SET TO APPARELSET
   =gfSetClrSet() && Set the Color set of the screen
   *B600822,1 Hesham El-Sheltawi (End)
  ENDIF  
  WAIT CLEAR
ENDIF
SELECT (lnCurAlias)

*khalid
SET DEFAULT TO &lcSavDFDir
RELEASE lcSavDFDir
*khalid

SET DEVICE TO SCREEN
*:------------------------------------------------------
*: Program file      : lfvCpUnDo
*: Program desc.     : Undo a copy action
*: For screen        : NoteChil
*:         System    : ARIA ADVANTAGE SERIES
*:         Module    : Base
*:      Developer    : Ashraf Sherif Mohammed (ASH)
*:  Job/Task Number  : B601471,1
*:------------------------------------------------------
*: Calls             : lfNxtPrvSt()
*:                     gfSetClrSet()
*:                     lfCpShow()
*:------------------------------------------------------
*: Passed Parameters : None
*:------------------------------------------------------
*: Example           : = lfvCpUnDo()
*:------------------------------------------------------
FUNCTION lfvCpUnDo
PRIVATE lnAlias

WAIT "Undo... " + ALLTRIM(&lcTmpFile..cDesc) WINDOW NOWAIT
lnAlias = SELECT()

*-- Delete the current record (action) from the temp file.
SELECT (lcTmpFile)
DELETE
SKIP -1

*-- If we reached the first action in this copy session
IF UPPER(ALLTRIM(&lcTmpFile..cDesc)) = "COPY SESSION"
  *-- Point to the empty note record in the notepad file.
  = SEEK('*',"NotePad")

  *-- Initialize all the used variables as if we are 
  *-- entering a new copy session.
  lcCpType   = lcType
  lcCpKey    = SPACE(12)
  lcCpNtDesc = ""
  rbCpMode   = 2
  SHOW GET pbCpUnDo    DISABLE
  SHOW GET pbCpUnDoAll DISABLE
  STORE "DISABLE" TO lcCpNextSt, lcCpPrevSt, lcCpCopySt
ELSE
  *-- Load all the used variables from the current record (action)
  *-- in the temp file.
  lcCpType   = cType
  lcCpKey    = cKey
  lcCpNtDesc = lfGetDesc(lcCpType,lcCpKey)
  rbCpMode   = nMode

  *-- Point to the action's key in the notepad file.
  *-- Please note that we should select the notepad file before
  *-- performing the seek command in order to take the effect of
  *-- the delete command in the temp file.
  SELECT NotePad
  = SEEK(&lcTmpFile..cType+&lcTmpFile..cKey)
  
  *-- Set the navigation buttons.
  llNothing  = lfNxtPrvSt()
ENDIF
puCpType   = AT(lcCpType,lcUsedCode)
lcCpPrompt = PADR(lfGetProm(lcCpType),17)
lcCpTypDsc = lfGetProm(lcCpType)

REPLACE &lcToEdit..mEdtNotes WITH &lcTmpFile..mNotes

SHOW GET lcCpType
SHOW GET puCpType
SHOW GET lcCpKey
SHOW GET rbCpMode
SHOW GET pbCpNext &lcCpNextSt
SHOW GET pbCpPrev &lcCpPrevSt
SHOW GET pbCpCopy &lcCpCopySt
SHOW GET pbCpReDo    ENABLE
SHOW GET pbCpReDoAll ENABLE
SELECT NotePad
*=gfSetClrSet()
SHOW GET mNotes 
= lfCpShow()
SELECT(lnAlias)
WAIT CLEAR

*:------------------------------------------------------
FUNCTION lfoldvalue

lcOldValue = &lcToEdit..mEdtNotes
RETURN
FUNCTION lftrapky

ON KEY LABEL ctrl+W     lndummy = 1
ON KEY LABEL ctrl+q     lndummy = 1
ON KEY LABEL ctrl+home  lndummy = 1
ON KEY LABEL ctrl+END   lndummy = 1
ON KEY LABEL TAB        DO lptab
ON KEY LABEL backtab    DO lpshifttab
ON KEY LABEL esc        DO lpesc
ON KEY LABEL ctrl+enter DO lpesc
RETURN


*!**************************************************************************
*! Name      : lfGetVslN
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 01/08/2002
*! Purpose   : Get notes from V.FOXPRO For Mexx
*!**************************************************************************
*! Passed Parameters : 
*!**************************************************************************
*! Notes : 
*!**************************************************************************
*! C200259
FUNCTION lfGetVslN

*C200259,1 (Begin) Run Custom V.Fox Arabic notepad for Mexx.
IF llMex
  =gfDoTriger('NOTEPAD',PADR('UPDNOTE',10))
  RETURN
ENDIF  
*C200259,1 (End)


