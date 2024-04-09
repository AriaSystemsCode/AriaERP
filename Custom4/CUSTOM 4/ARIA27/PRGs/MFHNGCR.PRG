*:************************************************************************
*: Program file  : MFHNGCR.PRG
*: Program desc. : Program To Setup Care Labels , Print Care Labels ,
*:                 Print hang tags.
*: For screen    : 
*:         System: 
*:         Module: Manufacturing & Purchase Order & Sales Order (MF & PO & SO)
*:      Developer: Khalid Mohi El-Din Mohamed (KHM)
*:************************************************************************
*: Calls : 
*: Procedures:HANG_CAR.prg (C/T)               (P/O)         
*:             ÃÄlpSetUp                      ÃÄlpSetUp
*:             ³     ÃÄCARE100                ³     ÃÄCARE100
*:             ³     ÀÄCARE200                ³     ÃÄCARE200
*:             ÀÄHANG100   ÀÄCARE210         ÀÄHANG150   ÀÄCARE210    
*:             ÃÄCARE300     ÄÄÄÄÂÄMATPRINT  ÃÄCARE400     ÄÄÄÄÂÄMATPRINT   
*:             ³      ÀÄCARE310  ÃÄSQPRINT   ³      ÀÄCARE410  ÃÄSQPRINTPO
*:             ÃÄHANG110         ÃÄMOVESQ    ÃÄHANG160         ÃÄMOVESQPO
*:             ÃÄHANG110A        ÀÄMOVETXT   ÃÄHANG160A        ÀÄMOVETXTPO
*:             ÀÄHANG110B                     ÀÄHANG160B      
*:      
*:************************************************************************
*: Passed Parameters  : lcMfPoType = "C" : For Cut Ticket
*:                      lcMfPoType = "P" : For Purchase Order
*:                      lcMfPoType = "S" : For Sales Order
*:************************************************************************
*: Example            : DO MFHagCr
*:************************************************************************
*: E301110,1 KHM 12/31/98
*: B802212,1 KHM 05/04/99   Fixed the bug of variable PWidth not found.
*: B602689,1 ABD 06/08/99   Fixed the bug Message " Nesting Error" in Care Lables Report.
*: C101560,1 WAB 06/22/99   Modify The Program To print labels according to Sales Order.
*: E301268   WAB 06/23/99   ADD Parameter (.f.,'C') to gfApVnBrow()
*: B802480,1 WAB            in the setup, the colcod still won't be saved.
*: C101816,1 ABD 04/19/2000 Add new sort to print care label report in sales order
*: C101816,1 ABD            Module only. 
*: B803000,1 ABD 05/23/2000 Correct spelling of word across on bottom of label size .
*: B603762,1 ABD 07/24/2000 Fix bug Alias 'ORDLINE' Not Found when call from PO module
*: B603713,10 HBG 11/22/2000 Increase The Picture Of The pricea 
*:************************************************************************
PARAMETER lcMfPoType

*-- Saving the system color
lcOldClr = SET('COLOR')
*-- To indicate whether U R using the program from manufacturing or PO  module.

*-C101560 WAB   if llpo is true (U R using the program  from PO .OR. SO ) 
*-C101560 WAB Start
*llPo = IIF(lcMfPoType = 'P',.T.,.F.)

llPo = IIF(lcMfPoType $ 'PS',.T.,.F.)

*-C101560 WAB END

*-- To open the necessary files.
=gfOpnFiles()

*-- Initializing the necessary variables.
STORE 0 TO lnClrPo,lnFreeLen , lnColorLen, lnTopCol
DIMENSION XMATRIX(22,70), XAUX(22,10)
qClrNrm   = "GR+/B,GR+/N,N"
qClrSay   = "N/BG,GR+/N"
qClrMag   = "BG+/RB,B/W"
QCLRBLK   = "N/W,GR+/N,N"
QCLRREV   = "W+/R,GR+/N"

*-- To get the lenght of the style major
lnMajLen = LEN(gfItemMask('PM'))

*-- To get the style header
lcStyHdr = gfItemMask("HM")

*-- To get the position of the color segment and the color's length if any.
=lfGtNonMaj()

IF llPo		&& Purchase Order OR Sales Order

  *-C101560 WAB if llpo is true (U R using the program  from PO .OR. SO ) 
  *-C101560 WAB we add if To indicate whether U R using the program from PO OR SO.
  *-C101560 WAB Start

  IF lcMfPoType = 'P'		&& Purchase Order
    SELECT POSLN
    POTEMP = gfTempName()
  ELSE						&& Sales Order
    SELECT ORDLINE
    lcOrdTmp = gfTempName()
    *C101816,1 ABD- Create new temp file to get the ordline sort by stle &   [Begin]
    *C101816,1 ABD- Create new Index On ordline file = cordtype+order+style+STR(lineno,6)
    lcOrdTmp1= gfTempName()    && Index
    lcOrdTmp2= gfTempName()    && Temporary file
    COPY STRUCTURE TO (gcWorkDir+lcOrdTmp2)
    =gfOpenFile(gcWorkDir+lcOrdTmp2,' ','EX')
    SELECT(lcOrdTmp2)
    INDEX ON Style+cOrdtype+Order+STR(lineno,6) TAG  (lcOrdTmp2)
    SELECT ORDLINE
    *C101816,1 ABD- [End]    
  ENDIF  

  *-C101560 WAB END

ELSE  						&& CutTiket
  SELECT CUTTKTL
  CTKTEMP = gfTempName()
ENDIF

SET RELATION TO STYLE INTO STYLE

SELECT STYLE
SET RELATION TO 'S'+SCALE INTO SCALE,FABRIC INTO FABRIC

*-- To allow the user to select his choice.
=lfGetChoic()
SET COLOR TO &lcOldClr 



*-C101560 WAB   DELETE TEMP. FILES
*-C101560 WAB Start
IF USED('&CTKTEMP')
  SELECT &CTKTEMP
  use 
  ERASE(gcWorkDir+CTKTEMP+'.DBF')
  ERASE(gcWorkDir+CTKTEMP+'.CDX')
ENDIF
IF USED('&POTEMP')
  SELECT &POTEMP
  use 
  ERASE(gcWorkDir+POTEMP+'.DBF')
  ERASE(gcWorkDir+POTEMP+'.CDX')
ENDIF
IF USED('&lcOrdTMP')
  SELECT &lcOrdTMP
  use 
  ERASE(gcWorkDir+lcOrdTmp+'.DBF')
  ERASE(gcWorkDir+lcOrdTmp+'.CDX')
ENDIF
*-C101560 WAB END

*C101816,1 ABD-  Erase the new Index On cordtype+order+style+STR(lineno,6) & 
*C101816,1 ABD-  Erase the new temp File on the ordline. [Begin] 
IF USED('ORDLINE')
  *SELECT &lcOrdTmp1
  *USE
  *ERASE(gcWorkDir+lcOrdTmp1+'.DBF')
  SELECT ORDLINE
  CLOSE INDEXES
  ERASE(gcWorkDir+lcOrdTmp1+'.CDX')
ENDIF

IF USED('&lcOrdTMP2')
  SELECT &lcOrdTmp2
  USE
  ERASE(gcWorkDir+lcOrdTmp2+'.DBF')
  ERASE(gcWorkDir+lcOrdTmp2+'.CDX')
ENDIF
*C101816,1 ABD- [End]


*!*************************************************************
*! Name      : gfOpnFiles
*! Developer : Khalid Mohi El-Din Mohamed - (KHM)
*! Date      : 12/31/98
*! Purpose   : To open the necessary files.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  gfOpnFiles
*!*************************************************************
FUNCTION gfOpnFiles

=gfOpenFile(gcDataDir+'STYLE','STYLE','SH')
=gfOpenFile(gcDataDir+'SCALE','SCALE','SH')
=gfOpenFile(gcDataDir+'CODES','CODES','SH')
=gfOpenFile(gcDataDir+'FABRIC','FABRIC','SH')

*-C101560 WAB  case using SO open file Customer
*-C101560 WAB  case using PO open file VENDOR
*-C101560 WAB Start

IF lcMfPoType = 'S'  && case Sales Order
  =gfOpenFile(gcDataDir+'Customer', 'Customer', 'SH')
ELSE				 && Case CutTiket Or Purchase Order
  =gfOpenFile(gcDataDir+'ApVendor', 'VenCode', 'SH')
ENDIF

*-C101560 WAB END

=gfOpenFile(gcDataDir+'CARESIZ','CARESIZ','SH')
=gfOpenFile(gcDataDir+'CARELAB','CARELAB','SH')

IF llPo			&& Purchase Order OR Sales Order

  *-C101560 WAB if llpo is true (U R using the program  from PO .OR. SO ) 
  *-C101560 WAB [if lcMfPoType = 'P'] THEN U R using program from PO ELSE From SO
  *-C101560 WAB Start

  IF lcMfPoType = 'P'		&& Purchase Order
    =gfOpenFile(gcDataDir+'POSHDR','POSHDR','SH')
    =gfOpenFile(gcDataDir+'POSLN' ,'POSLN' ,'SH')
  ELSE						&& Sales Order
    =gfOpenFile(gcDataDir+'ORDHDR','ORDHDR','SH')
    =gfOpenFile(gcDataDir+'ORDLINE','ORDLINE' ,'SH')
  ENDIF

  *-C101560 WAB END

ELSE						&& CUT Ticket
    =gfOpenFile(gcDataDir+'CUTTKTH','CUTTKTH','SH')
    =gfOpenFile(gcDataDir+'CUTTKTL','CUTTKTL','SH')
    =gfOpenFile(gcDataDir+'MfgOprHd','MfgOprHd','SH')
ENDIF

*!*************************************************************
*! Name      : lfGetChoic
*! Developer : Khalid Mohi El-Din Mohamed - (KHM)
*! Date      : 12/31/98
*! Purpose   : To enable the user to select from the available choices
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfGetChoic
*!*************************************************************
FUNCTION lfGetChoic

IF _WINDOWS
  DEFINE WINDOW MainWin FROM 0,0 SIZE 25,80 ;
         FLOAT NONE NOCLOSE FONT "FoxFont", 10 STYLE "R";
         IN WINDOW FNDATION
  MOVE WINDOW MainWin CENTER
  ACTIVATE WINDOW MainWin
ENDIF

ON KEY LABEL ESC

*-- Function to start selecting action.
=lfBgnProg()
IF WEXIST("MainWin")
  RELEASE WINDOW MainWin
ENDIF

ON KEY LABEL ESC KEYBOARD "{ALT+X}"

*!*************************************************************
*! Name      : lfBgnProg
*! Developer : Khalid Mohi El-Din Mohamed - (KHM)
*! Date      : 12/31/98
*! Purpose   : To choose from options.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfBgnProg
*!*************************************************************
FUNCTION lfBgnProg

SET COLOR TO &qClrNrm
CLEA

DO lpPrnHdr WITH 'HANG_CAR','PRINT HANG TAG AND CARE LABELS'
DO DRAWBOX WITH QCLRMAG,'',03,17,07,42

SET MESSAGE TO 24
SET COLOR TO &QCLRNRM
@ 23,00 TO 23,79
SET COLOR TO &QCLRMAG
@ 07,23 SAY '< Esc to exit >'

DO WHILE .T.
  SET COLOR TO &QCLRMAG
  @ 04,19 PROMPT '1. SETUP              ' MESSAGE 'Setup different Care Labels.'
  @ 05,19 PROMPT '2. PRINT CARE LABELS  ' MESSAGE 'Print Care Labels for the selected Cutting Tickets'
  @ 06,19 PROMPT '3. PRINT HANG TAGS    ' MESSAGE 'Print Hang Tags for the selected Cutting Tickets'
  MENU TO XCHOICE
  IF READKEY()=12 
    EXIT
  ENDIF
  SAVE SCREEN TO lsScr1  
  DO CASE
    CASE XCHOICE = 1 
      DO lpSetUp
    CASE XCHOICE = 2 
      IF !llPo			&& CutTiket
 		 DO HANG100 WITH 'C'
 	  ELSE				&& Purchase Order or Sales Order
         DO HANG150 WITH 'C'
      ENDIF
    CASE XCHOICE = 3 
      IF !llPo			&& CutTiket
        DO HANG100 WITH 'H'
      ELSE				&& Purchase Order or Sales Order
        DO HANG150 WITH 'H'
      ENDIF
  ENDCASE
  RESTORE SCREEN FROM lsScr1
ENDDO
RELEASE WINDOW MainWin

*!*************************************************************
*! Name      : lpSetUp
*! Developer : Khalid Mohi El-Din Mohamed - (KHM)
*! Date      : 12/31/98
*! Purpose   : To set the label sizes and design them.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  DO lpSetUp
*!*************************************************************
PROCEDURE lpSetUp

DO DRAWBOX WITH QCLRBLK,'',04,40,07,62
DO WHILE .T.
  SET COLOR TO &QCLRBLK
  @ 07,43 SAY '<Esc to exit>'
  @ 05,41 PROMPT ' A- Care Label Sizes' MESSAGE 'Define Page & Other Specifications for Care Labels'
  @ 06,41 PROMPT ' B- Care Labels     ' MESSAGE 'Design Care Label Forms'
  MENU TO XCHOICE1
  SET COLOR TO &QCLRNRM
  IF READKEY()=12
    EXIT
  ENDIF
  SAVE SCREEN TO CARE_1
  DO CASE
    CASE XCHOICE1=1
      *-- To create the structure of care labels for design.
      DO CARE100 
    CASE XCHOICE1=2
      *-- To design care label forms
      DO CARE200
  ENDCASE
  RESTORE SCREEN FROM CARE_1
  GG=SYS(2002,1)
  EXIT
ENDDO
RETURN

*!*************************************************************
*! Name      : CARE100
*! Developer : Khalid Mohi El-Din Mohamed - (KHM)
*! Date      : 12/31/98
*! Purpose   : To design care label form.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  DO CARE100
*!*************************************************************
PROCEDURE CARE100

PRIVATE ROW

XSIZECODE  = SPACE(1)
XPWIDTH    = 'N'
XLLENGTH   = 16
XLWIDTH    = 12
XNUMACROSS = 1
XPITCH     = '12'
XABC       = 0
lnTopCol   = 78 - LEN(ALLTRIM(lcStyHdr))-2
SELE CARESIZ
GO TOP
IF RECCOUNT()=0 .OR. EOF()
   XABC=1
ENDIF


SET COLOR TO &QCLRNRM
CLEAR
DO lpPrnHdr WITH 'CARE100','LABEL SIZE SPECIFICATIONS BROWSE'
DO WHILE .T.
  GG = SYS(2002)
  QRECNO = 0
  ROW    = 5
  DO DRAWBOX WITH QCLRMAG,'',2,0,21,79
  SET COLOR TO &QCLRMAG
  *B803000,1 ABD  Correct spelling of word across. [Begin]
  *@ 03,05 SAY 'SIZE CODE    PAPER PITCH    LAB. LENGTH    LAB. WIDTH    # LAB. ACCROSS'
   @ 03,05 SAY 'SIZE CODE    PAPER PITCH    LAB. LENGTH    LAB. WIDTH    # LAB. ACROSS'
  *B803000,1 ABD [End]
  @ 4,1 TO 4,78

  DO WHILE .NOT. EOF() .and. ROW < 21
    QRECNO(ROW) = RECNO()
    SET COLOR TO &QCLRSAY
    @ ROW,8 SAY SIZECODE
    @ ROW,22 SAY PITCH
    @ ROW,36 SAY LLENGTH
    @ ROW,52 SAY LWIDTH
    @ ROW,68 SAY NUMACROSS
    SET COLOR TO &QCLRMAG
    ROW=ROW+1
    SKIP
  ENDDO

  BP(2)=2
  BP(6)='Y'
  BP(7)='Y'
  IF XABC=1
     *-- If the program is running from the MF module 
     DO MSG22 WITH 'File is empty, would you like to add a record Y/N ? ','YN'
     IF Choice = 'N'
        EXIT
     ENDIF
     Thiskey=QINSKEY
  ELSE
     Thiskey=BROW()
  ENDIF
  DO CASE
    CASE thiskey = QESCKEY   && escape key
      EXIT

    CASE thiskey = QINSKEY   && insert key
       GG=SYS(2002,1)
       IF XABC=1
           XINSROW=5
        ELSE
           DO DRAWBOX WITH QCLRBLK,'',18,01,20,78
           XINSROW=19
        ENDIF
        DO WHILE .T.
           @ 23,00 TO 23,79 COLOR &qClrNrm
           @ 24,01 SAY 'Enter a Code for this size specifications' COLOR &qClrNrm
           DO WHILE .T.
              @ XINSROW,8 GET XSIZECODE PICTURE '!' COLOR &qClrNrm
              READ
              SEEK XSIZECODE
              IF .NOT. FOUND()
                 EXIT
              ELSE
                 @ 24,01 SAY 'Please enter a UNIQUE CODE for this size specifications' COLOR &qClrNrm
              ENDIF
           ENDDO
           IF XSIZECODE=' ' .OR. READKEY()=12
              EXIT
           ENDIF
           @ XINSROW,8 SAY XSIZECODE PICTURE '!' COLOR &qClrSay
           @ 24,01 SAY 'Enter the printer pitch: 10, 12 or 17                         ' COLOR &qClrNrm
           DO WHILE .T.
              @ XINSROW,22 GET XPITCH PICTURE '!!' COLOR &qClrNrm
              READ
              IF (XPITCH='12' .OR. XPITCH='10' .OR. XPITCH='17')
                 @ XINSROW,22 SAY XPITCH  PICTURE '!!' COLOR &qClrSay
                 EXIT
              ELSE
                @ 24,01 SAY 'Please enter the printer pitch: 10, 12 or 17 ' COLOR &qClrNrm
              ENDIF
           ENDDO

           @ 24,01 SAY 'Enter Label Lenght, # of rows: 1 to 22                  ' COLOR &qClrNrm
           DO WHILE .T.
              @ XINSROW,36 GET XLLENGTH PICTURE '99' COLOR &qClrSay
              READ
              IF XLLENGTH<1 .OR. XLLENGTH>22
                 @ 24,01 SAY 'Please enter Label Lenght, # of rows: 1 to 22           ' COLOR &qClrNrm
              ELSE
                 @ XINSROW,36 SAY XLLENGTH PICTURE '99' COLOR &qClrSay
                 EXIT
              ENDIF
           ENDDO

           @ 24,01 SAY 'Enter Label Width, # of columns: 12 to '+ALLTRIM(STR(lnTopCol-1))+'                         ' COLOR &qClrNrm
           DO WHILE .T.
              @ XINSROW,52 GET XLWIDTH PICTURE '99' COLOR &qClrNrm
              READ
              IF XLWIDTH < 12 .OR. XLWIDTH > lnTopCol - 1
                 @ 24,01 SAY 'Please enter Label Width, # of columns: 12 to '+ALLTRIM(STR(lnTopCol-1))+'              ' COLOR &qClrNrm
              ELSE
                 @ XINSROW,52 SAY XLWIDTH PICTURE '99' COLOR &qClrSay
                 EXIT
              ENDIF
           ENDDO
           XHIGCOL   = 234
           XMAXLABEL = INT(XHIGCOL/XLWIDTH)
           *B803000,1 ABD  Correct spelling of word across. [Begin]
           *@ 24,01 SAY 'Enter the Number of Labels to show accross the page: 1 to     ' COLOR &qClrNrm
           @ 24,01 SAY 'Enter the Number of Labels to show across the page: 1 to     ' COLOR &qClrNrm
           *B803000,1 ABD  [End]
           @ 24,59 SAY XMAXLABEL PICTURE '99' COLOR &qClrNrm
           DO WHILE .T.
              @ XINSROW,68 GET XNUMACROSS PICTURE '99' COLOR &qClrNrm
              READ
              IF XNUMACROSS<1 .OR. XNUMACROSS>XMAXLABEL
                 *B803000,1 ABD  Correct spelling of word across. [Begin]
                 *@ 24,01 SAY 'Please enter the Number of Labels to show accross the page: 1 to     ' COLOR &qClrNrm
                 @ 24,01 SAY 'Please enter the Number of Labels to show across the page: 1 to     ' COLOR &qClrNrm
                 *B803000,1 ABD  [End]
                 @ 24,66 SAY XMAXLABEL PICTURE '99' COLOR &qClrNrm
              ELSE
                 @ XINSROW,68 SAY XNUMACROSS PICTURE '99' COLOR &qClrSay
                 EXIT
              ENDIF
           ENDDO
           SET COLOR TO &qClrNrm
           @ 23,00 CLEAR TO 24,79 
           
           DO MSG22 WITH '<S>ave, <M>odify, or <C>ancel ? ','SMC'

           DO CASE
              CASE CHOICE='S'
                SET DELE OFF
                SEEK XSIZECODE
                IF FOUND()
                   RECALL
                ELSE
                   APPEND BLANK
                ENDIF
                SET DELE ON
                REPLACE SIZECODE  WITH XSIZECODE
                REPLACE PITCH     WITH XPITCH
                REPLACE LLENGTH   WITH XLLENGTH
                REPLACE LWIDTH    WITH XLWIDTH
                REPLACE NUMACROSS WITH XNUMACROSS
                IF XABC=1
                   XABC=0
                ENDIF
                EXIT
              CASE CHOICE='M'
                LOOP
              OTHERWISE
                EXIT
           ENDCASE

        ENDDO
        IF XABC=1
           EXIT
        ENDIF
        SET COLOR TO &qClrNrm
        @ 23,00 CLEAR TO 24,79
        XSIZECODE=SPACE(1)
        GO TOP

    CASE thiskey = QDELKEY   && delete key
        ***** check if record is not used by label forms before deleting
        XNODELE=.F.
        XSIZECODE=SIZECODE
        SELE CARELAB
        GO TOP
        DO WHILE .NOT. EOF()
           IF XSIZECODE=SIZECODE
              XNODELE=.T.
              EXIT
           ENDIF
           SKIP
        ENDDO
        SELE CARESIZ

        IF XNODELE
          DO MSG22 WITH 'Sorry record CANNOT BE DELETED, Label Forms using this Size Specs exist!','$'
        ELSE
          DO MSG22 WITH 'Are you sure of deleting the record <Y/N> ? ','YN'
          IF CHOICE='Y'
            DELE
          ENDIF
        ENDIF
        GO TOP

    CASE thiskey = QRETKEY   && Return key - modifications
        ***** check if record is not used by label forms before modifying
        XNODELE=.F.
        XSIZECODE=SIZECODE
        SELE CARELAB
        GO TOP
        DO WHILE .NOT. EOF()
           IF XSIZECODE=SIZECODE
              XNODELE=.T.
              EXIT
           ENDIF
           SKIP
        ENDDO
        SELE CARESIZ
        IF XNODELE
         DO MSG22 WITH 'WARNING - This size Code is used in existing Label Forms!','$'
        ENDIF
        
        *B802212,1 KHM 05/04/99 (Begin) Commening the following line because
        *B802212,1              the Pwidth is no longer exists in the CareSiz file.
        *XPWIDTH=PWIDTH
        *B802212,1 KHM 05/04/99 (End)
        
        XPITCH=PITCH
        XLLENGTH=LLENGTH
        XLWIDTH=LWIDTH
        XNUMACROSS=NUMACROSS

        DO DRAWBOX WITH QCLRBLK,'',18,01,20,78
        SET COLOR TO &QCLRBLK
        @ 18,24 SAY '® Modification of Size Code..'+XSIZECODE+' ¯'
        SET COLOR TO &QCLRNRM
        XINSROW=19
        @ XINSROW,9 SAY XSIZECODE PICTURE '!'

        DO WHILE .T.
           @ 23,00 TO 23,79
           @ 24,01 SAY 'Enter the printer pitch: 10, 12 or 17                          '
           DO WHILE .T.
              @ XINSROW,22 GET XPITCH PICTURE '!!'
              READ
              IF (XPITCH='12' .OR. XPITCH='10' .OR. XPITCH='17')
                 @ XINSROW,22 SAY XPITCH  PICTURE '!!'
                 EXIT
              ELSE
                @ 24,01 SAY 'Please enter the printer pitch: 10, 12 or 17             '
              ENDIF
           ENDDO

           @ 24,01 SAY 'Enter Label Lenght, # of rows: 1 to 22                  '     &&TAK 08/31/94
           DO WHILE .T.
              @ XINSROW,36 GET XLLENGTH PICTURE '99'
              READ
              IF XLLENGTH<1 .OR. XLLENGTH>22
                 @ 24,01 SAY 'Please enter Label Lenght, # of rows: 1 to 22           '  &&TAK 08/31/94
              ELSE
                 @ XINSROW,36 SAY XLLENGTH PICTURE '99'
                 EXIT
              ENDIF
           ENDDO

           @ 24,01 SAY 'Enter Label Width, # of columns: 12 to 70                         '
           DO WHILE .T.
              @ XINSROW,52 GET XLWIDTH PICTURE '99'
              READ
              IF XLWIDTH<12 .OR. XLWIDTH>70
                 @ 24,01 SAY 'Please enter Label Width, # of columns: 12 to 70              '
              ELSE
                 @ XINSROW,52 SAY XLWIDTH PICTURE '99'
                 EXIT
              ENDIF
           ENDDO
           XHIGCOL  =235
           XMAXLABEL=INT(XHIGCOL/XLWIDTH)
           *B803000,1 ABD  Correct spelling of word across. [Begin]
           *@ 24,01 SAY 'Enter the Number of Labels to show accross the page: 1 to     '
           @ 24,01 SAY 'Enter the Number of Labels to show across the page: 1 to     '
           *B803000,1 ABD  [End]
           @ 24,59 SAY XMAXLABEL PICTURE '99'
           DO WHILE .T.
              @ XINSROW,68 GET XNUMACROSS PICTURE '99'
              READ
              IF XNUMACROSS<1 .OR. XNUMACROSS>XMAXLABEL
                 *B803000,1 ABD  Correct spelling of word across. [Begin]
                 *@ 24,01 SAY 'Please enter the Number of Labels to show accross the page: 1 to     '
                 @ 24,01 SAY 'Please enter the Number of Labels to show across the page: 1 to     '
                 *B803000,1 ABD  [End]
                 @ 24,66 SAY XMAXLABEL PICTURE '99'
              ELSE
                 @ XINSROW,68 SAY XNUMACROSS PICTURE '99'
                 EXIT
              ENDIF
           ENDDO
           SET COLOR TO &qClrNrm
           @ 23,00 CLEAR TO 24,79
           DO MSG22 WITH '<S>ave, <M>odify, or <C>ancel ? ','SMC'                                 
           DO CASE
              CASE CHOICE = 'S'
                REPLACE LLENGTH   WITH XLLENGTH
                REPLACE LWIDTH    WITH XLWIDTH
                REPLACE NUMACROSS WITH XNUMACROSS
                REPLACE PITCH     WITH XPITCH
                EXIT
              CASE CHOICE='M'
                LOOP
              OTHERWISE
                EXIT
           ENDCASE

        ENDDO
        SET COLOR TO &qClrNrm
        @ 23,00 CLEAR TO 24,79
        XSIZECODE=SPACE(1)
        GO TOP
  ENDCASE
ENDDO
SET COLOR TO &QCLRNRM
GG=SYS(2002,1)
RETURN

*!*************************************************************
*! Name      : CARE200
*! Developer : Khalid Mohi El-Din Mohamed - (KHM)
*! Date      : 12/31/98
*! Purpose   : To browse and design care label forms heder.
*!*************************************************************
*! Calls     : Procedures : CARE210
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  DO CARE200
*!*************************************************************
PROCEDURE CARE200

XMATRIX    = SPACE(1)
XAUX       = 0
XLABELCODE = SPACE(8)
XLDESC     = SPACE(25)
XSIZECODE  = SPACE(1)
XPWIDTH    = 'N'
XPITCH     = '12'
XLLENGTH   = 16
XLWIDTH    = 12
XNUMACROSS = 1
XABC       = 0

SELE CARELAB
GO TOP
IF RECCOUNT()=0 .OR. EOF()
   XABC=1
ENDIF

SELE CARESIZ
GO TOP 
IF RECCOUNT()=0 .OR. EOF()
  DO MSG22 WITH 'SIZE SPECS FILE EMPTY, Please create Sizes before creating Label Forms! ','$'
  RETURN
ENDIF

SELE CARELAB
PRIVATE ROW
SET COLOR TO &QCLRNRM
@ 23,00 CLEAR TO 23,79

DO WHILE .T.
  GG = SYS(2002)
  QRECNO = 0
  ROW    = 12
  DO DRAWBOX WITH QCLRMAG,'',09,27,21,77

  SET COLOR TO &QCLRMAG
  @ 10,33 SAY 'LBL CODE  ....LABEL.DESCRIPTION...  SZ CODE'
  @ 11,28 TO 11,76
  SCAN WHILE .NOT. EOF() .and. ROW < 21
    QRECNO(ROW) = RECNO()
    SET COLOR TO &QCLRSAY
    @ ROW,33 SAY LABELCODE
    @ ROW,43 SAY LDESC
    @ ROW,72 SAY SIZECODE
    SET COLOR TO &QCLRMAG
    ROW=ROW+1
  ENDSCAN

  BP(1)=12
  BP(2)=29
  BP(6)='Y'
  BP(7)='Y'
  IF XABC=1
    DO MSG22 WITH 'File is empty, would you like to add a record Y/N ? ','YN'
    IF CHOICE='N'
      EXIT
    ENDIF
    Thiskey=QINSKEY
  ELSE
     Thiskey=BROW()
  ENDIF

  DO CASE
    CASE Thiskey = QESCKEY   && escape key
       EXIT
    CASE Thiskey = QINSKEY   && insert key
        GG = SYS(2002,1)
        IF XABC=1
           XINSROW=12
        ELSE
           DO DRAWBOX WITH QCLRBLK,'',18,28,20,75
           XINSROW=19
        ENDIF

        DO WHILE .T.

           @ 23,00 TO 23,79 COLOR &qClrNrm
           @ 24,01 SAY 'Enter a Unique Code for this Label Form' COLOR &qClrNrm
           DO WHILE .T.
              @ XINSROW,33 GET XLABELCODE PICTURE '!!!!!!!!' COLOR &qClrNrm
              READ
              SEEK XLABELCODE
              IF .NOT. FOUND()
                 EXIT
              ELSE
                 @ 24,01 SAY 'Please enter a UNIQUE CODE for this Label Form' COLOR &qClrNrm
              ENDIF
           ENDDO
           IF XLABELCODE='        ' .OR. READKEY()=12
              EXIT
           ENDIF
           @ XINSROW,33 SAY XLABELCODE PICTURE '!!!!!!!!' COLOR &qClrSay

           @ 24,01 SAY 'Enter Label Form Description                           ' COLOR &qClrNrm
           @ XINSROW,43 GET XLDESC PICTURE '!!!!!!!!!!!!!!!!!!!!!!!!!'
           READ
           @ XINSROW,43 SAY XLDESC PICTURE '!!!!!!!!!!!!!!!!!!!!!!!!!' COLOR &qClrSay
           @ 24,01 SAY 'Enter a Size Specification Code for this Label Form   ' COLOR &qClrNrm
           SELE CARESIZ
           DO WHILE .T.
              @ XINSROW,72 GET XSIZECODE PICTURE '!' COLOR &qClrNrm
              READ
              SEEK XSIZECODE
              IF FOUND() .OR. XSIZECODE=' '
                 IF FOUND()
                    
                    *B802212,1 KHM 05/04/99 (Begin) Commening the following 
                    *B802212,1              line because the Pwidth is no 
                    *B802212,1              longer exists in the CareSiz file.
                    *XPWIDTH=PWIDTH
                    *B802212,1 KHM 05/04/99 (End)
                    
                    XPITCH=PITCH
                    XLLENGTH=LLENGTH
                    XLWIDTH=LWIDTH
                    XNUMACROSS=NUMACROSS
                 ENDIF
                 EXIT
              ELSE
                 @ 24,01 SAY 'Please enter an EXISTANT Size Specification Code for this Label Form' COLOR &qClrNrm
              ENDIF
           ENDDO
           SELE CARELAB
           IF XSIZECODE=' ' .OR. READKEY()=12
              EXIT
           ENDIF
           @ XINSROW,72 SAY XSIZECODE PICTURE '!' COLOR &qClrSay
           SET COLOR TO &qClrNrm
           @ 23,00 CLEAR TO 24,79            
           DO MSG22 WITH '<P>roceed, <M>odify, or <C>ancel ? ','PMC'           
           DO CASE
              CASE CHOICE='P'
                XMATRIX = SPACE(1)
                XAUX    = 0
                SET ORDER TO TAG CARELABL
                SEEK XLABELCODE
                SAVE SCREEN TO CARE200_1
                DO CARE210                     && Create Care Label Design
                RESTORE SCREEN FROM CARE200_1
                SET ORDER TO TAG CARELAB
                EXIT
              CASE CHOICE='M'
                LOOP
              OTHERWISE
                EXIT
           ENDCASE
        ENDDO
        GO TOP
        IF XABC=1
           EXIT
        ENDIF
        XLABELCODE=SPACE(8)
        @ 23,00 CLEAR TO 24,79

    CASE thiskey = QDELKEY   && delete key
        DO MSG22 WITH 'Are you sure of deleting the record <Y/N> ? ','YN'
        XLABELCODE=LABELCODE
        IF CHOICE = 'Y'
           SET ORDER TO TAG CARELABL
           SEEK XLABELCODE
           DELE WHILE XLABELCODE=LABELCODE
           SET ORDER TO TAG CARELAB
        ENDIF
        GO TOP

    CASE thiskey = QRETKEY   && return key - Label inquire and modification
        SET COLOR TO &QCLRNRM
        XLABELCODE=LABELCODE
        SEEK XLABELCODE
        IF FOUND()
           XLDESC    = LDESC
           XSIZECODE = SIZECODE
           SELE CARESIZ
           SEEK XSIZECODE
           
           *B802212,1 KHM 05/04/99 (Begin) Commening the following 
           *B802212,1              line because the Pwidth is no 
           *B802212,1              longer exists in the CareSiz file.
           *XPWIDTH    = PWIDTH
           *B802212,1 KHM 05/04/99 (End)
           
           XPITCH     = PITCH
           XLWIDTH    = LWIDTH
           XLLENGTH   = LLENGTH
           XNUMACROSS = NUMACROSS
           SELE CARELAB
           SET ORDER TO TAG CARELABL
           SEEK XLABELCODE
           ****** restore information
           XL=0                                 && Lenght subindex
           DO WHILE XL<XLLENGTH
              XL = XL+1
              XAUX(XL,1)=CUT
              XAUX(XL,2)=STY
              XAUX(XL,3)=SIZ
              XAUX(XL,4)=CLR
              IF llPo
                *-C101560 WAB if llpo is true (U R using the program  from PO .OR. SO ) 
                *-C101560 WAB [if lcMfPoType = 'P'] THEN U R using program from PO ELSE From SO
                *-C101560 WAB Start
  
                IF lcMfPoType = 'P'		&& Purchase Order
 
                *-C101560 WAB END                
 
                  XAUX(XL,5)=SDC  
                  XAUX(XL,6)=SPR  
                  XAUX(XL,7)=FDC  
                  XAUX(XL,8)=FCN    
                  XAUX(XL,9)=CCD   
 
                *-C101560 WAB START
 
                ELSE					&& Sales Order
                  XAUX(XL,5)=SDC
                  XAUX(XL,6)=SPR
                  XAUX(XL,7)=CCD
                ENDIF
 
                *-C101560 WAB END
 
              ELSE  					 && CutTiket
                XAUX(XL,5)=DYE
                XAUX(XL,6)=SDC
                XAUX(XL,7)=SPR
                XAUX(XL,8)=FDC
                XAUX(XL,9)=FCN
                XAUX(XL,10)=CCD
              ENDIF
              XW = 0                            && Width subindex
              
              DO WHILE XW<XLWIDTH
                 XW = XW+1
                 XMATRIX(XL,XW) = SUBSTR(MATRIX,XW,1)
              ENDDO
              IF !EOF()
                SKIP
              ENDIF 
           ENDDO
           SAVE SCREEN TO CARE200_1
           DO CARE210                     && Create Care Label Design
           RESTORE SCREEN FROM CARE200_1
           SET ORDER TO TAG CARELAB
        ENDIF
        XLABELCODE=SPACE(8)
        GO TOP
  ENDCASE
ENDDO
SET COLOR TO &QCLRNRM
RETURN

*!*************************************************************
*! Name      : CARE210
*! Developer : Khalid Mohi El-Din Mohamed - (KHM)
*! Date      : 12/31/98
*! Purpose   : To design and save care labels using matrixes.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  DO CARE210
*!*************************************************************
PROCEDURE CARE210

CLEAR
DO lpPrnHdr WITH 'HANG_CAR','DESIGN CARE LABELS'

@ 01,05 SAY 'Label.: '+SPACE(8)+'   Desc.: '+SPACE(25)+'   Size Code.: '
SET COLOR TO &QCLRSAY
@ 01,13 SAY XLABELCODE
@ 01,31 SAY SUBSTR(XLDESC,1,25)
@ 01,71 SAY XSIZECODE

* if 0 is contained then that element doesn't exist in that line
XSELEC  = 0
XDELETE =.F.
XFIELD  = ''


*** Print matrix & elements
SET COLOR TO &QCLRBLK

DO MATPRINT
IF llPo
  DO SQPRINTPO
ELSE
  DO SQPRINT
ENDIF

DO WHILE .T.

  SAVE SCREEN TO lsMsgCntr
  DO DRAWBOX WITH qClrBlk,'',20,13,22,68
  SET COLOR TO &qClrBlk
  @ 21,15 SAY '<I>tems, <S>ave, c<O>py, <M>odify, or <C>ancel ? '
  CHOICE = ' '
  DO WHILE .T.
    @ 21,66 GET CHOICE PICTURE '!' VALID CHOICE $ 'ISOMC'
    READ
    IF CHOICE $ 'ISOMC'
      EXIT
    ENDIF
  ENDDO
  RESTORE SCREEN FROM lsMsgCntr
  SET COLOR TO &qClrNrm

  DO CASE
    ****--------- Position Items on Label ----------****
    CASE CHOICE='I'
         
      DO WHILE .T.
        *** Print matrix & elements
        IF XSELEC > 0
          IF XSELEC = 11
            SET COLOR TO &QCLRBLK
            DO MATPRINT
          ENDIF
          IF llPo
            DO SQPRINTPO
          ELSE
            DO SQPRINT
          ENDIF
        ENDIF

        SET MESSAGE TO 24
        SET COLOR TO &QCLRMAG            
        lnTopCol = IIF(LEN(ALLTRIM(lcStyHdr)) <= 6,71,;
                  78 - LEN(ALLTRIM(lcStyHdr))-2)
        @ 04,lnTopCol SAY '®ITEMS¯ '
        DO DRAWBOX WITH QCLRMAG,'',5,lnTopCol,17,78
        SET COLOR TO &QCLRMAG
        IF llPo

          *-C101560 WAB if llpo is true (U R using the program  from PO .OR. SO ) 
          *-C101560 WAB [if lcMfPoType = 'P'] THEN U R using program from PO ELSE From SO
          *-C101560 WAB Start
  
          IF lcMfPoType = 'P'		&& Purchase Order
            *-C101560 WAB END                
 
            @ 06,72 PROMPT 'PO    ' MESSAGE 'Position Purchase Order on Label'

            *-C101560 WAB Start
          ELSE
            @ 06,72 PROMPT 'ORDER' MESSAGE 'Position Sales Order on Label'
          ENDIF  
          *-C101560 WAB END

        ELSE
          @ 06,lnTopCol+1 PROMPT 'CUTTKT' MESSAGE 'Position Cutting Ticket on Label'              
        ENDIF
        @ 07,lnTopCol+1 PROMPT ALLTRIM(lcStyHdr) MESSAGE 'Position '+ALLTRIM(lcStyHdr)+' on Label'
        @ 08,lnTopCol+1 PROMPT 'SIZE  ' MESSAGE 'Position Size on Label'
        @ 09,lnTopCol+1 PROMPT 'COLOR ' MESSAGE 'Position Color description on Label'

        IF !llPO 
          @ 10,lnTopCol+1 PROMPT 'DYELOT' MESSAGE 'Position Dyelot on Label'              
          ROW=11
        ELSE
          ROW=10
        ENDIF

        *-C101560 WAB  Intiaise variable for row count
        *-C101560 WAB  - START

        lnrownm = 0

        *-C101560 WAB  - END
            
        @ ROW,lnTopCol+1 PROMPT 'S.DESC' MESSAGE 'Position Style description on Label' && TAK8/05/93

        *-C101560 WAB  - START--- add 1 to the row counter

        *@ ROW+1,lnTopCol+1 PROMPT 'PRICEA' MESSAGE 'Position Style price A' && TAK8/05/93

        lnrownm = lnrownm + 1
        @ ROW+lnrownm,lnTopCol+1 PROMPT 'PRICEA' MESSAGE 'Position Style price A' && TAK8/05/93

        *-C101560 WAB  - END

        *-C101560 WAB  For CutTicket And Purchaise Order Only
        *-C101560 WAB Start

        IF lcMfPoType <> 'S'		&& Purchase Order

          *-C101560 WAB  - START--- using ROW COUNTER ( lnrownm )

          *@ ROW+2,lnTopCol+1 PROMPT 'F.DESC' MESSAGE 'Position Fabric description on Label' && TAK8/05/93
          *@ ROW+3,lnTopCol+1 PROMPT 'F.CONT' MESSAGE 'Position Fabric Contents on Label' && TAK8/05/93

          lnrownm = lnrownm + 1
          @ ROW+lnrownm,lnTopCol+1 PROMPT 'F.DESC' MESSAGE 'Position Fabric description on Label'
          lnrownm = lnrownm + 1
          @ ROW+lnrownm,lnTopCol+1 PROMPT 'F.CONT' MESSAGE 'Position Fabric Contents on Label'

          *-C101560 WAB  - END
        ENDIF
        *-C101560 WAB  - START--- using ROW COUNTER ( lnrownm )

        *@ ROW+4,lnTopCol+1 PROMPT 'CLRCOD' MESSAGE 'Position Color code Contents on Label' && TAK3/06/94
        *@ ROW+5,lnTopCol+1 PROMPT 'TEXT  ' MESSAGE 'Position Text on Label'

        lnrownm = lnrownm + 1
        @ ROW+lnrownm,lnTopCol+1 PROMPT 'CLRCOD' MESSAGE 'Position Color code Contents on Label'
        lnrownm = lnrownm + 1
        @ ROW+lnrownm,lnTopCol+1 PROMPT 'TEXT  ' MESSAGE 'Position Text on Label'

        *-C101560 WAB  - END

        MENU TO XSELEC

        IF READKEY()=12
          SET COLOR TO &QCLRNRM
          @ 24,00 CLEAR TO 24,79
          EXIT
        ENDIF
        SET COLOR TO &QCLRBLK
        IF llPO 
          *-C101560 WAB WHEN USER CHOSE TEXT FROM POPUP
          *-C101560 WAB Start
          *IF XSELEC = 10 
          IF XSELEC = 10 .OR. ( XSELEC = 8 .AND. lcMfPoType = 'S' )
        *-C101560 WAB END
            DO MOVETXTPO
          ELSE
            IF xSelec = 4 AND lnColorLen = 0
              DO Msg22 WITH 'No color segment found. Therefore the '+;
                          'color item cannot be selected.','$'
              LOOP
            ENDIF
            IF xSelec = 9 AND lnColorLen = 0
              DO Msg22 WITH 'No color segment found. Therefore the '+;
                            'color Desc. item cannot be selected.','$'
              LOOP
            ENDIF
            DO MOVESQPO
          ENDIF
        ELSE
          IF XSELEC = 11
            DO MOVETXT
          ELSE  &&XSELEC=1,2,..,10 
            IF xSelec = 4 AND lnColorLen = 0
              DO Msg22 WITH 'No color segment found. Therefore the '+;
                             'color item cannot be selected.','$'
              LOOP
            ENDIF
            IF xSelec = 10 AND lnColorLen = 0
               DO Msg22 WITH 'No color segment found. Therefore the '+;
                             'color Desc. item cannot be selected.','$'
               LOOP
            ENDIF
            DO MOVESQ
          ENDIF
        ENDIF
      ENDDO
      SET COLOR TO &QCLRNRM
      @ 24,0 SAY SPACE(80)
      @ 04,71 CLEAR TO 17,78
      @ 04,lnTopCol+1 CLEAR TO 17,78

      *-- Save and go to previous screen
      CASE CHOICE='S'
        DO MSG22 WITH 'Please wait, Label being saved...','@'                 
        IF XABC=1
          XABC=0
        ENDIF
        IF XDELETE   && FLAG BECOMES .T. FOR DELETE  
          REPLACE FOR (LABELCODE=XLABELCODE .AND. !EMPTY(&XFIELD)) &XFIELD WITH 0        
        ENDIF

        *** recall deleted records
        SET DELETE OFF
        SEEK XLABELCODE
        SCAN WHILE LABELCODE=XLABELCODE
          IF DELETED()
            RECALL
          ENDIF
        ENDSCAN
        SET DELETE ON

        **-- save or replace records with new information
        XL=0
        DO WHILE XL<XLLENGTH
          XL=XL+1
          SEEK XLABELCODE+STR(XL,2)
          IF .NOT. FOUND()
            APPEND BLANK
          ENDIF
          REPLACE LABELCODE WITH XLABELCODE
          REPLACE LINE      WITH XL
          REPLACE SIZECODE  WITH XSIZECODE
          REPLACE LDESC     WITH XLDESC
          REPLACE CUT       WITH XAUX(XL,1)
          REPLACE STY       WITH XAUX(XL,2)
          REPLACE SIZ       WITH XAUX(XL,3)
          REPLACE CLR       WITH XAUX(XL,4)
          IF llPo
            *-C101560 WAB if llpo is true (U R using the program  from PO .OR. SO ) 
            *-C101560 WAB [if lcMfPoType = 'P'] THEN U R using program from PO ELSE From SO
            *-C101560 WAB Start
  
            IF lcMfPoType = 'P'		&& Purchase Order
 
              *-C101560 WAB END                

              REPLACE SDC     WITH XAUX(XL,5)
              REPLACE SPR     WITH XAUX(XL,6)
              REPLACE FDC     WITH XAUX(XL,7)
              REPLACE FCN     WITH XAUX(XL,8)
              REPLACE CCD     WITH XAUX(XL,9)

              *-C101560 WAB START
 
            ELSE					&& Sales Order
              REPLACE SDC     WITH XAUX(XL,5)
              REPLACE SPR     WITH XAUX(XL,6)

              *B802480,1 WAB ColrCod is the elemnt no 7 in the matrix
              *B802480,1 WAB - START
              *REPLACE CCD     WITH XAUX(XL,9)

              REPLACE CCD     WITH XAUX(XL,7)

              *B802480,1 WAB - END
              
            ENDIF
 
            *-C101560 WAB END

          ELSE
            REPLACE DYE       WITH XAUX(XL,5)
            REPLACE SDC       WITH XAUX(XL,6)
            REPLACE SPR       WITH XAUX(XL,7)
            REPLACE FDC       WITH XAUX(XL,8)
            REPLACE FCN       WITH XAUX(XL,9)
            REPLACE CCD       WITH XAUX(XL,10)
          ENDIF
          XSTR=''
          XW=0
          DO WHILE XW<XLWIDTH
            XW   = XW+1
            XSTR = XSTR+XMATRIX(XL,XW)
          ENDDO
          REPLACE MATRIX    WITH SUBSTR(XSTR,1,70)
        ENDDO
        DO WHILE XL<22
          XL=XL+1
          SEEK XLABELCODE+STR(XL,2)
          IF .NOT. FOUND()
            APPEND BLANK
          ENDIF
          REPLACE LABELCODE WITH XLABELCODE
          REPLACE LINE      WITH XL
          REPLACE SIZECODE  WITH XSIZECODE
          REPLACE LDESC     WITH XLDESC
          REPLACE CUT       WITH 0
          REPLACE STY       WITH 0
          REPLACE SIZ       WITH 0
          REPLACE CLR       WITH 0
          REPLACE DYE       WITH 0
          REPLACE SDC       WITH 0
          REPLACE SPR       WITH 0
          REPLACE FDC       WITH 0
          REPLACE FCN       WITH 0
          REPLACE CCD       WITH 0
          REPLACE MATRIX    WITH SPACE(70)

          WAIT CLEAR

        ENDDO
        EXIT
      ****--------- Copy other Label  ----------****
      CASE CHOICE ='O' 
        SAVE SCREEN TO lsCopySCR
        SET COLOR TO &QCLRNRM
        XLCODE=SPACE(8)
        @ 23,10 SAY '           Enter Label Code to copy from...:           '
        DO WHILE .T.
          @ 23,55 GET XLCODE PICTURE '!!!!!!!!'
          READ
          SEEK XLCODE
          IF FOUND() .OR. XLCODE='        '
            IF FOUND()
              XSIZECODE  = SIZECODE
              SELE CARESIZ
              SEEK XSIZECODE
                  
              *B802212,1 KHM 05/04/99 (Begin) Commening the following 
              *B802212,1              line because the Pwidth is no 
              *B802212,1              longer exists in the CareSiz file.
              *XPWIDTH    = PWIDTH
              *B802212,1 KHM 05/04/99 (End)
                  
              XPITCH     = PITCH
              XLLENGTH   = LLENGTH
              XLWIDTH    = LWIDTH
              XNUMACROSS = NUMACROSS
              SELE CARELAB
            ENDIF
            RESTORE SCREEN FROM lsCopySCR
            EXIT
          ELSE
            @ 24,17 SAY 'Please enter an EXISTANT Label Code to copy from' COLOR &qClrNrm
          ENDIF
        ENDDO
        XSW=.F.
        IF XLCODE<>'        ' .AND. READKEY()<>12 .AND. XLCODE<>XLABELCODE
          XSW=.T.
          XL=0                                 && Lenght subindex
          DO WHILE XL<22
            XL = XL+1
            XAUX(XL,1) = CUT
            XAUX(XL,2) = STY
            XAUX(XL,3) = SIZ
            XAUX(XL,4) = CLR
            IF llPo
              *-C101560 WAB if llpo is true (U R using the program  from PO .OR. SO ) 
              *-C101560 WAB [if lcMfPoType = 'P'] THEN U R using program from PO ELSE From SO
              *-C101560 WAB Start

              IF lcMfPoType = 'P'		&& Purchase Order
              *-C101560 WAB END                
 
                XAUX(XL,5)=SDC  
                XAUX(XL,6)=SPR  
                XAUX(XL,7)=FDC  
                XAUX(XL,8)=FCN    
                XAUX(XL,9)=CCD   

                *-C101560 WAB START
              ELSE					&& Sales Order
                XAUX(XL,5)=SDC
                XAUX(XL,6)=SPR
                XAUX(XL,7)=CCD
              ENDIF
              *-C101560 WAB END
 
            ELSE  					 && CutTiket
              XAUX(XL,5)=DYE
              XAUX(XL,6)=SDC
              XAUX(XL,7)=SPR
              XAUX(XL,8)=FDC
              XAUX(XL,9)=FCN
              XAUX(XL,10)=CCD
            ENDIF
            XW = 0                            && Width subindex
            DO WHILE XW<70
              XW = XW+1
              XMATRIX(XL,XW) = SUBSTR(MATRIX,XW,1)
            ENDDO
            IF ! EOF()
              SKIP
            ENDIF
          ENDDO
       ENDIF
       @ 01,71 SAY XSIZECODE PICTURE '!' COLOR &qClrSay
       @ 24,00 CLEAR TO 24,79
       IF XSW
         SET COLOR TO &qClrNrm
         @ 02,01 CLEAR TO 23,70               && clear old matrix
         SET COLOR TO &QCLRBLK
         DO MATPRINT
         IF llPo
           DO SQPRINTPO
         ELSE
           DO SQPRINT
         ENDIF
       ENDIF
       LOOP
     **-- Modify Label
    CASE CHOICE='M'
      @ 24,01 SAY 'Enter Label Form Description                           ' COLOR &qClrNrm
      @ 01,31 GET XLDESC PICTURE '@!'COLOR &qClrNrm
      READ
      SET COLOR TO &QCLRSAY COLOR &qClrSay
      @ 01,31 SAY XLDESC PICTURE '@!'
      XSC=XSIZECODE
      SET COLOR TO &QCLRNRM
      @ 24,01 SAY 'Enter a Size Specification Code for this Label Form   '
      SELE CARESIZ
      DO WHILE .T.
        @ 01,71 GET XSC PICTURE '!'
        READ
        SEEK XSC
        IF FOUND() .OR. XSC=' '
          IF FOUND()
                 
            *B802212,1 KHM 05/04/99 (Begin) Commening the following 
            *B802212,1              line because the Pwidth is no 
            *B802212,1              longer exists in the CareSiz file.
            *XPWIDTH    = PWIDTH
            *B802212,1 KHM 05/04/99 (End)
                
            XPITCH     = PITCH
            XLLENGTH   = LLENGTH
            XLWIDTH    = LWIDTH
            XNUMACROSS = NUMACROSS
          ENDIF
          EXIT
        ELSE
          @ 24,01 SAY 'Please enter an EXISTANT Size Specification Code for this Label Form' COLOR &qClrNrm
        ENDIF
      ENDDO
        SELE CARELAB
        XSW=.F.
        IF XSC<>' ' .AND. READKEY()<>12 .AND. XSC<>XSIZECODE
          XSW=.T.
          XSIZECODE=XSC
        ENDIF
        SET COLOR TO &QCLRSAY
        @ 01,71 SAY XSIZECODE PICTURE '!' COLOR &qClrSay
        SET COLOR TO &QCLRNRM
        @ 24,00 CLEAR TO 24,79
        IF XSW
          @ 02,01 CLEAR TO 23,70           && clear old matrix
          SET COLOR TO &QCLRBLK
          DO MATPRINT
          IF llPo
            DO SQPRINTPO
          ELSE
            DO SQPRINT
          ENDIF
        ENDIF
        LOOP

      **-- Go to previous screen
      CASE CHOICE='C' 
        EXIT
    ENDCASE
 ENDDO
 SET COLOR TO &QCLRNRM
RETURN


*!*************************************************************
*! Name      : SQPRINT
*! Developer : Khalid Mohi El-Din Mohamed - (KHM)
*! Date      : 12/31/98
*! Purpose   : To print the matrix elements in the screen
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  DO SQPRINT
*!*************************************************************
PROCEDURE SQPRINT

SET COLOR TO &QCLRREV

*** Print matrix Cuttkt, Style, Color ,.........
XL=0
DO WHILE XL<22

   XL=XL+1

   *-- out of range cleaning of elements from memory
   IF XAUX(XL,1) > XLWIDTH-5 .OR. (XAUX(XL,1)>0 .AND. XL>XLLENGTH) && cuttkt
      XAUX(XL,1)=0
   ENDIF
   IF XAUX(XL,2) > XLWIDTH-lnMajLen .OR. (XAUX(XL,2)>0 .AND. XL>XLLENGTH) && style
      XAUX(XL,2)=0
   ENDIF
   IF XAUX(XL,3) > XLWIDTH-4 .OR. (XAUX(XL,3)>0 .AND. XL>XLLENGTH) && size
      XAUX(XL,3)=0
   ENDIF
   IF XAUX(XL,4) > XLWIDTH-2 .OR. (XAUX(XL,4)>0 .AND. XL>XLLENGTH) && color
      XAUX(XL,4)=0
   ENDIF
   IF XAUX(XL,5) > XLWIDTH-9 .OR. (XAUX(XL,5)>0 .AND. XL>XLLENGTH) && dyelot
      XAUX(XL,5)=0
   ENDIF
   IF XAUX(XL,6) > XLWIDTH-4 .OR. (XAUX(XL,6)>0 .AND. XL>XLLENGTH) && styledesc
      XAUX(XL,6)=0
   ENDIF
   IF XAUX(XL,7) > XLWIDTH-5 .OR. (XAUX(XL,7)>0 .AND. XL>XLLENGTH) && stylePRICEA
      XAUX(XL,7)=0
   ENDIF
   IF XAUX(XL,8) > XLWIDTH-4 .OR. (XAUX(XL,8)>0 .AND. XL>XLLENGTH) && FABRIC DESC
      XAUX(XL,8)=0
   ENDIF
   IF XAUX(XL,9) > XLWIDTH-4 .OR. (XAUX(XL,9)>0 .AND. XL>XLLENGTH) && FABRIC CONT
      XAUX(XL,9)=0
   ENDIF
   IF XAUX(XL,10) > XLWIDTH-5 .OR. (XAUX(XL,10)>0 .AND. XL>XLLENGTH) && color CODE
      XAUX(XL,10)=0
   ENDIF
   *-- print on screen
   IF XAUX(XL,1)>0
      @ XL+1,XAUX(XL,1) SAY ('CUTTKT')
   ENDIF
   IF XAUX(XL,2)>0
      @ XL+1,XAUX(XL,2) SAY PADR(ALLTRIM(lcStyHdr),lnMajLen)
   ENDIF
   IF XAUX(XL,3)>0
      @ XL+1,XAUX(XL,3) SAY 'SIZE '
   ENDIF

   IF XAUX(XL,4)>0
      @ XL+1,XAUX(XL,4) SAY 'CLR'
   ENDIF

   IF XAUX(XL,5)>0
      @ XL+1,XAUX(XL,5) SAY 'DYELOT    '
   ENDIF

   IF XAUX(XL,6)>0
      @ XL+1,XAUX(XL,6) SAY 'SDESC'
   ENDIF

   IF XAUX(XL,7)>0
      @ XL+1,XAUX(XL,7) SAY 'PRICEA'
   ENDIF

   IF XAUX(XL,8)>0
      @ XL+1,XAUX(XL,8) SAY 'FDESC'
   ENDIF

   IF XAUX(XL,9)>0
      @ XL+1,XAUX(XL,9) SAY 'FCONT'
   ENDIF

   IF XAUX(XL,10)>0
     @ XL+1,XAUX(XL,10) SAY 'CLRCOD'
   ENDIF
ENDDO
SET COLOR TO &QCLRNRM
RETURN

*!*************************************************************
*! Name      : MATPRINT
*! Developer : Khalid Mohi El-Din Mohamed - (KHM)
*! Date      : 12/31/98
*! Purpose   : To print the matrix in the screen
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  DO MATPRINT
*!*************************************************************
PROCEDURE MATPRINT

*** Print matrix text
XL=0                          && Length subindex
DO WHILE XL<XLLENGTH
   XL=XL+1
   XW=0                       && Width subindex
   DO WHILE XW<XLWIDTH
      XW=XW+1
      @ XL+1,XW SAY XMATRIX(XL,XW)    && begins in row 3 col 1 of screen
   ENDDO
ENDDO

SET COLOR TO &QCLRNRM
RETURN

*!*************************************************************
*! Name      : MOVESQPO
*! Developer : Khalid Mohi El-Din Mohamed - (KHM)
*! Date      : 12/31/98
*! Purpose   : To move Po,Style,Size,Color,Style Description.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  DO MOVESQPO
*!*************************************************************
PROCEDURE MOVESQPO

*** getting elements coordinates for validation
DIMENSION XCOOR(10,2)                    
PRIVATE XCOOR

XCOOR=0
XW=0

*-C101560 WAB [if lcMfPoType = 'P'] THEN U R using program from PO ELSE From SO
*-C101560 WAB Start

*DO WHILE XW<10   

DO WHILE XW< IIF(lcMfPoType = 'P',10,8)                           

*-C101560 WAB END

  XW = XW+1
  XL = 0
  DO WHILE XL<XLLENGTH
    XL=XL+1
    IF XAUX(XL,XW)<>0
      XCOOR(XW,1) = XL
      XCOOR(XW,2) = XAUX(XL,XW)
    ENDIF
  ENDDO
ENDDO

XMESG=SPACE(80)          
DO CASE
   CASE XSELEC = 1   && width for PO=6
      XSELWID = 5
   CASE XSELEC = 2   && width for style=12
     *XSELWID = 11
     XSELWID = lnMajLen
   CASE XSELEC = 3   && width for size=5
      XSELWID = 4
   CASE XSELEC = 4   && width for COLOR=3
      XSELWID = 2  
      XMESG = ' Total length of color description is 20 ch.'
   CASE XSELEC=5   && width for SDESC=5
      XSELWID = 4
      XMESG = ' Total length of style description is 20 ch.'  
   CASE XSELEC = 6   && width for S.PR.A=6
      XSELWID = 5
   CASE XSELEC = 7   && width for FDESC=5

      *-C101560 WAB [if lcMfPoType = 'P'] THEN U R using program from PO ELSE From SO
      *-C101560 WAB Start

      IF lcMfPoType = 'P'  	&& PURCHASE ORDER

      *-C101560 WAB END

        XSELWID = 4
        XMESG = ' Total length of fabric description is 20 ch.'  

      *-C101560 WAB Start  

      ELSE					&& SALES ORDER
        XSELWID=5
        XMESG=' Total color code length is 6 ch.'  
      ENDIF

      *-C101560 WAB END

   CASE XSELEC = 8   && width for Fcont=5
      XSELWID = 4
      XMESG = ' Total length of fabric contents is 60 ch.'  
   CASE XSELEC = 9   && width for CCD=6
      XSELWID=5
      XMESG=' Total color code length is 6 ch.'  
   OTHERWISE
      RETURN
ENDCASE


IF !EMPTY(XMESG)
  SET COLOR TO &QCLRMAG
  @ 24,0 SAY SPACE(80)
  @ 24,0 SAY XMESG
  SET COLOR TO &QCLRNRM
ELSE
  SET COLOR TO &QCLRNRM
  @ 24,0 SAY SPACE(80)
ENDIF  
XMESG=SPACE(80)

XROW   = 0
XCOL   = 0
XL     = 0
XEXIST = .F.          &&FLAG-> IF THE FIELD IS EXIST OR NO

DO WHILE XL<XLLENGTH
  XL=XL+1
  IF XAUX(XL,XSELEC)<>0
    XEXIST = .T. 
    XROW   = XL
    XCOL   = XAUX(XL,XSELEC)
    XAUX(XL,XSELEC)=0
    EXIT
  ENDIF
ENDDO


IF XEXIST
  SAVE SCREEN TO lsChgeScr
  DO DRAWBOX WITH qClrBlk,'',20,13,22,68
  SET COLOR TO &qClrBlk
  @ 21,25 SAY ' <M>ove location  <D>elete ? '
  CHOICE = ' '
  DO WHILE .T.
    @ 21,56 GET CHOICE PICTURE '!' VALID CHOICE $ 'MD'
    READ
    IF CHOICE $ 'MD'
      EXIT
    ENDIF
  ENDDO
  RESTORE SCREEN FROM lsChgeScr
  SET COLOR TO &qClrNrm

  DO CASE
    CASE CHOICE='D'
      SET COLOR TO &QCLRBLK
      DO CASE
        CASE XSELEC = 1
          @ XROW+1,XCOL SAY '      '
          XFIELD = 'CUT'
        CASE XSELEC = 2
          @ XROW+1,XCOL SAY '            '
          XFIELD = 'STY'
        CASE XSELEC = 3
          @ XROW+1,XCOL SAY '     '
          XFIELD = 'SIZ'
        CASE XSELEC = 4
          @ XROW+1,XCOL SAY '   '
          XFIELD = 'CLR'
        CASE XSELEC = 5
          @ XROW+1,XCOL SAY '     '
          XFIELD = 'SDC'
        CASE XSELEC = 6
          @ XROW+1,XCOL SAY '      '
          XFIELD = 'SPR'
        CASE XSELEC = 7
          @ XROW+1,XCOL SAY '     '

          *-C101560 WAB [if lcMfPoType = 'P'] THEN U R using program from PO ELSE From SO
          *-C101560 WAB Start

          *XFIELD = 'FDC'
          
          XFIELD = IIF(lcMfPoType = 'P','FDC','CCD')
          
          *-C101560 WAB END

        CASE XSELEC = 8
          @ XROW+1,XCOL SAY '     '
          XFIELD = 'FCN'
        CASE XSELEC = 9
          @ XROW+1,XCOL SAY '      '
          XFIELD = 'CCD'
      ENDCASE
      XDELETE = .T.
      RETURN
  ENDCASE
ENDIF

IF XROW = 0
  XROW = 1
  XCOL = 1
ENDIF

DO WHILE .T.
  SET COLOR TO &QCLRSAY
  DO CASE
    CASE XSELEC = 1   && PO

      *-C101560 WAB [if lcMfPoType = 'P'] THEN U R using program from PO ELSE From SO
      *-C101560 WAB Start
      
      *@ XROW+1,XCOL SAY 'PO    '
       
      @ XROW+1,XCOL SAY IIF(lcMfPoType = 'P','PO    ','ORDER ')
      
      *-C101560 WAB END
      
    CASE XSELEC = 2   && style
      *@ XROW+1,XCOL SAY 'STYLE       '
      @ XROW+1,XCOL SAY PADR(ALLTRIM(lcStyHdr),lnMajLen)
    CASE XSELEC = 3   && size
      @ XROW+1,XCOL SAY 'SIZE '
    CASE XSELEC = 4   && COLOR
      @ XROW+1,XCOL SAY 'CLR'
    CASE XSELEC = 5   && SDESC
      @ XROW+1,XCOL SAY 'SDESC'
    CASE XSELEC = 6   && PRICEA
      @ XROW+1,XCOL SAY 'PRICEA'
    CASE XSELEC = 7   && FDESC

      *-C101560 WAB [if lcMfPoType = 'P'] THEN U R using program from PO ELSE From SO
      *-C101560 WAB Start

      *@ XROW+1,XCOL SAY 'FDESC'

      @ XROW+1,XCOL SAY IIF(lcMfPoType = 'P','FDESC','CLRCOD')
      
      *-C101560 WAB END
      
    CASE XSELEC = 8   && FCONT
      @ XROW+1,XCOL SAY 'FCONT'
    CASE XSELEC = 9   && COLOR CODE
      @ XROW+1,XCOL SAY 'CLRCOD'
   ENDCASE
   SET COLOR TO &QCLRBLK
   THISKEY=INKEY(0)
   DO CASE
     CASE THISKEY = QESCKEY .OR. THISKEY = QRETKEY
       EXIT

     CASE THISKEY = QLFTKEY .OR. THISKEY = 127   && Left arrow or Backspace
        IF XCOL=1         && out of range
          ? CHR(7)
        ELSE
          @ XROW+1,XCOL+XSELWID SAY XMATRIX(XROW,XCOL+XSELWID)
          XCOL=XCOL-1
        ENDIF

     CASE THISKEY = QRGTKEY                      && Right arrow
       IF XCOL=XLWIDTH-XSELWID  && out of range
         ? CHR(7)
       ELSE
         @ XROW+1,XCOL SAY XMATRIX(XROW,XCOL)
         XCOL=XCOL+1
       ENDIF

     CASE THISKEY = QUPKEY                       && Up arrow
       IF XROW=1         && out of range
         ? CHR(7)
       ELSE
         DO CASE
           CASE XSELEC = 1   && PO
             @ XROW+1,XCOL SAY XMATRIX(XROW,XCOL)+XMATRIX(XROW,XCOL+1)+;
               XMATRIX(XROW,XCOL+2)+XMATRIX(XROW,XCOL+3)+XMATRIX(XROW,XCOL+4)+;
               XMATRIX(XROW,XCOL+5)
           CASE XSELEC = 2   && style
             @ XROW+1,XCOL SAY XMATRIX(XROW,XCOL)+XMATRIX(XROW,XCOL+1)+;
               XMATRIX(XROW,XCOL+2)+XMATRIX(XROW,XCOL+3)+XMATRIX(XROW,XCOL+4)+;
               XMATRIX(XROW,XCOL+5)+XMATRIX(XROW,XCOL+6)+XMATRIX(XROW,XCOL+7)+;
               XMATRIX(XROW,XCOL+8)+XMATRIX(XROW,XCOL+9)+XMATRIX(XROW,XCOL+10)+;
               XMATRIX(XROW,XCOL+11)
           CASE XSELEC = 3   && size
             @ XROW+1,XCOL SAY XMATRIX(XROW,XCOL)+XMATRIX(XROW,XCOL+1)+;
               XMATRIX(XROW,XCOL+2)+XMATRIX(XROW,XCOL+3)+XMATRIX(XROW,XCOL+4)
           CASE XSELEC = 4   && color
             @ XROW+1,XCOL SAY XMATRIX(XROW,XCOL)+XMATRIX(XROW,XCOL+1)+;
               XMATRIX(XROW,XCOL+2)
           CASE XSELEC = 5   && STYLE DESC
             @ XROW+1,XCOL SAY XMATRIX(XROW,XCOL)+XMATRIX(XROW,XCOL+1)+;
               XMATRIX(XROW,XCOL+2)+XMATRIX(XROW,XCOL+3)+XMATRIX(XROW,XCOL+4)
           CASE XSELEC = 6   && STYLE PRICE A
             @ XROW+1,XCOL SAY XMATRIX(XROW,XCOL)+XMATRIX(XROW,XCOL+1)+;
               XMATRIX(XROW,XCOL+2)+XMATRIX(XROW,XCOL+3)+XMATRIX(XROW,XCOL+4)+;
               XMATRIX(XROW,XCOL+5)
           CASE XSELEC = 7   && FABRIC DESC
              
              *-C101560 WAB [if lcMfPoType = 'P'] THEN U R using program from PO ELSE From SO
              *-C101560 WAB Start
              
              IF lcMfPoType = 'P' 		&& PURCHASE ORDER  ( FABRIC DESC )
              
              *-C101560 WAB END
              
                @ XROW+1,XCOL SAY XMATRIX(XROW,XCOL)+XMATRIX(XROW,XCOL+1)+;
                XMATRIX(XROW,XCOL+2)+XMATRIX(XROW,XCOL+3)+XMATRIX(XROW,XCOL+4)
              
              *-C101560 WAB Start
              
              ELSE						&& SALES ORDER  ( color code )
                @ XROW+1,XCOL SAY XMATRIX(XROW,XCOL)+XMATRIX(XROW,XCOL+1)+;
                 XMATRIX(XROW,XCOL+2)+XMATRIX(XROW,XCOL+3)+XMATRIX(XROW,XCOL+4)+;
                 XMATRIX(XROW,XCOL+5)             
              ENDIF
              
              *-C101560 WAB END
              
           CASE XSELEC = 8   && FABRIC CONT
             @ XROW+1,XCOL SAY XMATRIX(XROW,XCOL)+XMATRIX(XROW,XCOL+1)+;
               XMATRIX(XROW,XCOL+2)+XMATRIX(XROW,XCOL+3)+XMATRIX(XROW,XCOL+4)
           CASE XSELEC = 9   && color code
             @ XROW+1,XCOL SAY XMATRIX(XROW,XCOL)+XMATRIX(XROW,XCOL+1)+;
               XMATRIX(XROW,XCOL+2)+XMATRIX(XROW,XCOL+3)+XMATRIX(XROW,XCOL+4)+;
               XMATRIX(XROW,XCOL+5)
           ENDCASE
           XROW=XROW-1
         ENDIF

      CASE THISKEY = QDWNKEY                      && Down arrow
        IF XROW=XLLENGTH    && out of range
          ? CHR(7)
        ELSE
          DO CASE
            CASE XSELEC = 1   && PO
              @ XROW+1,XCOL SAY XMATRIX(XROW,XCOL)+XMATRIX(XROW,XCOL+1)+;
                XMATRIX(XROW,XCOL+2)+XMATRIX(XROW,XCOL+3)+XMATRIX(XROW,XCOL+4)+;
                XMATRIX(XROW,XCOL+5)
             CASE XSELEC = 2   && style
               @ XROW+1,XCOL SAY XMATRIX(XROW,XCOL)+XMATRIX(XROW,XCOL+1)+;
                 XMATRIX(XROW,XCOL+2)+XMATRIX(XROW,XCOL+3)+XMATRIX(XROW,XCOL+4)+;
                 XMATRIX(XROW,XCOL+5)+XMATRIX(XROW,XCOL+6)+XMATRIX(XROW,XCOL+7)+;
                 XMATRIX(XROW,XCOL+8)+XMATRIX(XROW,XCOL+9)+XMATRIX(XROW,XCOL+10)+;
                 XMATRIX(XROW,XCOL+11)
             CASE XSELEC = 3   && size
               @ XROW+1,XCOL SAY XMATRIX(XROW,XCOL)+XMATRIX(XROW,XCOL+1)+;
                 XMATRIX(XROW,XCOL+2)+XMATRIX(XROW,XCOL+3)+XMATRIX(XROW,XCOL+4)
              CASE XSELEC = 4   && color
                @ XROW+1,XCOL SAY XMATRIX(XROW,XCOL)+XMATRIX(XROW,XCOL+1)+;
                  XMATRIX(XROW,XCOL+2)
              CASE XSELEC = 5   && STYLE DESC
                @ XROW+1,XCOL SAY XMATRIX(XROW,XCOL)+XMATRIX(XROW,XCOL+1)+;
                  XMATRIX(XROW,XCOL+2)+XMATRIX(XROW,XCOL+3)+XMATRIX(XROW,XCOL+4)
              CASE XSELEC = 6   && STYLE PRICE A
                @ XROW+1,XCOL SAY XMATRIX(XROW,XCOL)+XMATRIX(XROW,XCOL+1)+;
                  XMATRIX(XROW,XCOL+2)+XMATRIX(XROW,XCOL+3)+XMATRIX(XROW,XCOL+4)+;
                  XMATRIX(XROW,XCOL+5)
              CASE XSELEC = 7   && FABRIC DESC

                *-C101560 WAB [if lcMfPoType = 'P'] THEN U R using program from PO ELSE From SO
                *-C101560 WAB Start
                
                IF lcMfPoType = 'P' 		&& PURCHASE ORDER  ( FABRIC DESC )
                *-C101560 WAB END
 				  @ XROW+1,XCOL SAY XMATRIX(XROW,XCOL)+XMATRIX(XROW,XCOL+1)+;
                  XMATRIX(XROW,XCOL+2)+XMATRIX(XROW,XCOL+3)+XMATRIX(XROW,XCOL+4)
                  
                *-C101560 WAB START
                ELSE						&& SALES ORDER ( color code )
                  @ XROW+1,XCOL SAY XMATRIX(XROW,XCOL)+XMATRIX(XROW,XCOL+1)+;
                    XMATRIX(XROW,XCOL+2)+XMATRIX(XROW,XCOL+3)+;
                    XMATRIX(XROW,XCOL+4)+XMATRIX(XROW,XCOL+5)
                ENDIF
          
                *-C101560 WAB END            
          
              CASE XSELEC = 8   && FABRIC CONT
                @ XROW+1,XCOL SAY XMATRIX(XROW,XCOL)+XMATRIX(XROW,XCOL+1)+;
                  XMATRIX(XROW,XCOL+2)+XMATRIX(XROW,XCOL+3)+XMATRIX(XROW,XCOL+4)
              CASE XSELEC = 9   && color code
                @ XROW+1,XCOL SAY XMATRIX(XROW,XCOL)+XMATRIX(XROW,XCOL+1)+;
                  XMATRIX(XROW,XCOL+2)+XMATRIX(XROW,XCOL+3)+;
                  XMATRIX(XROW,XCOL+4)+XMATRIX(XROW,XCOL+5)
            ENDCASE
            XROW=XROW+1
         ENDIF

   ENDCASE
   DO SQPRINTPO

ENDDO

*** validation for overlaping
XCOOR(XSELEC,1) = XROW
XCOOR(XSELEC,2) = XCOL
IF XCOOR(1,1)   = XCOOR(2,1) .AND. XCOOR(1,1)<>0 .AND. (XSELEC=1 .OR. XSELEC=2)
   IF (XCOOR(1,2)>=XCOOR(2,2) .AND. XCOOR(1,2)<=XCOOR(2,2)+6);
   .OR. (XCOOR(1,2)+5>=XCOOR(2,2) .AND. XCOOR(1,2)+5<=XCOOR(2,2)+6)
       XCOL=0
   ENDIF
ENDIF
IF XCOOR(1,1)=XCOOR(3,1) .AND. XCOOR(1,1)<>0 .AND. (XSELEC=1 .OR. XSELEC=3)
   IF (XCOOR(3,2)>=XCOOR(1,2) .AND. XCOOR(3,2)<=XCOOR(1,2)+5);
   .OR. (XCOOR(3,2)+2>=XCOOR(1,2) .AND. XCOOR(3,2)+2<=XCOOR(1,2)+5)
       XCOL=0
   ENDIF
ENDIF
IF XCOOR(2,1)=XCOOR(3,1) .AND. XCOOR(2,1)<>0 .AND. (XSELEC=2 .OR. XSELEC=3)
   IF (XCOOR(3,2)>=XCOOR(2,2) .AND. XCOOR(3,2)<=XCOOR(2,2)+6);
   .OR. (XCOOR(3,2)+2>=XCOOR(2,2) .AND. XCOOR(3,2)+2<=XCOOR(2,2)+6)
   XCOL=0
   ENDIF
ENDIF

IF XCOL<>0
   XW=0
   DO WHILE XW<XSELWID+1
      XMATRIX(XROW,XCOL+XW)=SPACE(1)
      XW=XW+1
   ENDDO
ELSE
   SET COLOR TO &QCLRBLK
   XW=0
   DO WHILE XW<XSELWID+1
      @ XCOOR(XSELEC,1)+1,XCOOR(XSELEC,2)+XW SAY ' '
      XW=XW+1
   ENDDO
   SET COLOR TO &QCLRNRM
ENDIF

XAUX(XROW,XSELEC)=XCOL
RETURN

*!*************************************************************
*! Name      : MOVESQ
*! Developer : Khalid Mohi El-Din Mohamed - (KHM)
*! Date      : 12/31/98
*! Purpose   : To move the elements around the screen.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  DO MOVESQ
*!*************************************************************
PROCEDURE MOVESQ

*** getting elements coordinates for validation
DIMENSION XCOOR(10,2)
PRIVATE XCOOR

XCOOR = 0
XW    = 0
DO WHILE XW<10
   XW = XW+1
   XL = 0   
   DO WHILE XL<XLLENGTH
      XL = XL+1
      IF XAUX(XL,XW)<>0
         XCOOR(XW,1)=XL
         XCOOR(XW,2)=XAUX(XL,XW)
      ENDIF
   ENDDO
ENDDO

XMESG=SPACE(80)
DO CASE
   CASE XSELEC = 1   && width for cuttkt=6
      XSELWID = 5

   CASE XSELEC = 2   && width for style=12
     *XSELWID=11
     XSELWID = lnMajLen
   CASE XSELEC = 3   && width for size=5
      XSELWID = 4
   CASE XSELEC = 4   && width for COLOR=3
      XSELWID = 2
      XMESG=' Total length of color description is 20 ch.'  && TAK 8/05/93 MESSAGE
   CASE XSELEC = 5   && width for DYELOT=10
      XSELWID = 9
   CASE XSELEC = 6   && width for SDESC=5
      XSELWID = 4
      XMESG = ' Total length of style description is 20 ch.'  && TAK 8/05/93 MESSAGE
   CASE XSELEC = 7    && width for S.PR.A=6
      XSELWID = 5
   CASE XSELEC = 8   && width for FDESC=5
      XSELWID = 4
      XMESG =' Total length of fabric description is 20 ch.'  && TAK 8/05/93 MESSAGE
   CASE XSELEC = 9   && width for Fcont=5
      XSELWID = 4
      XMESG =' Total length of fabric contents is 60 ch.'  && TAK 8/05/93 MESSAGE
   CASE XSELEC = 10   && width for CCD=6
      XSELWID = 5
      XMESG=' Total color code length is 6 ch.'  && TAK 3/06/94 MESSAGE
   OTHERWISE
      RETURN
ENDCASE
IF !EMPTY(XMESG)
  SET COLOR TO &QCLRMAG
  @ 24,0 SAY SPACE(80)
  @ 24,0 SAY XMESG
  SET COLOR TO &QCLRNRM
ELSE
  SET COLOR TO &QCLRNRM
  @ 24,0 SAY SPACE(80)
ENDIF  
XMESG=SPACE(80)

XROW   = 0
XCOL   = 0
XL     = 0
XEXIST =.F.

DO WHILE XL<XLLENGTH
   XL=XL+1
   IF XAUX(XL,XSELEC)<>0
      XEXIST=.T.
      XROW=XL
      XCOL=XAUX(XL,XSELEC)
      XAUX(XL,XSELEC)=0
      EXIT
   ENDIF
ENDDO

IF XEXIST
  SAVE SCREEN TO lsChgeScr
  DO DRAWBOX WITH qClrBlk,'',20,13,22,68
  SET COLOR TO &qClrBlk
  @ 21,25 SAY ' <M>ove location  <D>elete ? '
  CHOICE = ' '
  DO WHILE .T.
    @ 21,56 GET CHOICE PICTURE '!' VALID CHOICE $ 'MD'
    READ
    IF CHOICE $ 'MD'
      EXIT
    ENDIF
  ENDDO
  RESTORE SCREEN FROM lsChgeScr
  SET COLOR TO &qClrNrm

  DO CASE
    CASE CHOICE='D'
      SET COLOR TO &QCLRBLK
      DO CASE
        CASE XSELEC = 1
           @ XROW+1,XCOL SAY '      '
           XFIELD = 'CUT'
        CASE XSELEC = 2
          *@ XROW+1,XCOL SAY '            '
          @ XROW+1,XCOL SAY SPACE(lnMajLen)
          XFIELD = 'STY'
        CASE XSELEC = 3
          @ XROW+1,XCOL SAY '     '
          XFIELD = 'SIZ'
        CASE XSELEC = 4
           @ XROW+1,XCOL SAY '   '
           XFIELD = 'CLR'
        CASE XSELEC = 5
           @ XROW+1,XCOL SAY '          '
           XFIELD = 'DYE'
        CASE XSELEC = 6
           @ XROW+1,XCOL SAY '     '
           XFIELD = 'SDC'
        CASE XSELEC = 7
           @ XROW+1,XCOL SAY '      '
           XFIELD = 'SPR'
        CASE XSELEC = 8
           @ XROW+1,XCOL SAY '     '
           XFIELD = 'FDC'
        CASE XSELEC = 9
           @ XROW+1,XCOL SAY '     '
           XFIELD = 'FCN'
        CASE XSELEC = 10
           @ XROW+1,XCOL SAY '      '
           XFIELD = 'CCD'
      ENDCASE
      XDELETE=.T.
      RETURN
  ENDCASE
ENDIF

IF XROW = 0
  XROW  = 1
  XCOL  = 1
ENDIF

DO WHILE .T.

   SET COLOR TO &QCLRSAY
   DO CASE
      CASE XSELEC = 1   && cuttkt
         @ XROW+1,XCOL SAY 'CUTTKT'
      CASE XSELEC = 2   && style
         *@ XROW+1,XCOL SAY 'STYLE       '
         @ XROW+1,XCOL SAY PADR(ALLTRIM(lcStyHdr),lnMajLen)
      CASE XSELEC = 3   && size
         @ XROW+1,XCOL SAY 'SIZE '
      CASE XSELEC = 4   && COLOR
         @ XROW+1,XCOL SAY 'CLR'
      CASE XSELEC = 5   && DYELOT
         @ XROW+1,XCOL SAY 'DYELOT    '
      CASE XSELEC = 6   && SDESC
         @ XROW+1,XCOL SAY 'SDESC'
      CASE XSELEC = 7   && PRICEA
         @ XROW+1,XCOL SAY 'PRICEA'
      CASE XSELEC = 8   && FDESC
         @ XROW+1,XCOL SAY 'FDESC'
      CASE XSELEC = 9   && FCONT
         @ XROW+1,XCOL SAY 'FCONT'
      CASE XSELEC = 10   && COLOR CODE
         @ XROW+1,XCOL SAY 'CLRCOD'
   ENDCASE

   SET COLOR TO &QCLRBLK

   THISKEY=INKEY(0)

   DO CASE

      CASE THISKEY = QESCKEY .OR. THISKEY = QRETKEY
         EXIT

      CASE THISKEY = QLFTKEY .OR. THISKEY = 127   && Left arrow or Backspace
         IF XCOL=1         && out of range
            ? CHR(7)
         ELSE
            @ XROW+1,XCOL+XSELWID SAY XMATRIX(XROW,XCOL+XSELWID)
            XCOL=XCOL-1
         ENDIF

      CASE THISKEY = QRGTKEY                      && Right arrow
         IF XCOL=XLWIDTH-XSELWID  && out of range
            ? CHR(7)
         ELSE
            @ XROW+1,XCOL SAY XMATRIX(XROW,XCOL)
            XCOL=XCOL+1
         ENDIF

      CASE THISKEY = QUPKEY                       && Up arrow
         IF XROW=1         && out of range
            ? CHR(7)
         ELSE
            DO CASE
               CASE XSELEC=1   && cuttkt
                  @ XROW+1,XCOL SAY XMATRIX(XROW,XCOL)+XMATRIX(XROW,XCOL+1)+;
                    XMATRIX(XROW,XCOL+2)+XMATRIX(XROW,XCOL+3)+XMATRIX(XROW,XCOL+4)+;
                    XMATRIX(XROW,XCOL+5)
               CASE XSELEC=2   && style
                  @ XROW+1,XCOL SAY XMATRIX(XROW,XCOL)+XMATRIX(XROW,XCOL+1)+;
                    XMATRIX(XROW,XCOL+2)+XMATRIX(XROW,XCOL+3)+XMATRIX(XROW,XCOL+4)+;
                    XMATRIX(XROW,XCOL+5)+XMATRIX(XROW,XCOL+6)+XMATRIX(XROW,XCOL+7)+;
                    XMATRIX(XROW,XCOL+8)+XMATRIX(XROW,XCOL+9)+XMATRIX(XROW,XCOL+10)+;
                    XMATRIX(XROW,XCOL+11)
               CASE XSELEC=3   && size
                  @ XROW+1,XCOL SAY XMATRIX(XROW,XCOL)+XMATRIX(XROW,XCOL+1)+;
                    XMATRIX(XROW,XCOL+2)+XMATRIX(XROW,XCOL+3)+XMATRIX(XROW,XCOL+4)
               CASE XSELEC=4   && color
                  @ XROW+1,XCOL SAY XMATRIX(XROW,XCOL)+XMATRIX(XROW,XCOL+1)+;
                    XMATRIX(XROW,XCOL+2)
               CASE XSELEC=5   && DYELOT
                  @ XROW+1,XCOL SAY XMATRIX(XROW,XCOL)+XMATRIX(XROW,XCOL+1)+;
                    XMATRIX(XROW,XCOL+2)+XMATRIX(XROW,XCOL+3)+XMATRIX(XROW,XCOL+4)+;
                    XMATRIX(XROW,XCOL+5)+XMATRIX(XROW,XCOL+6)+XMATRIX(XROW,XCOL+7)+;
                    XMATRIX(XROW,XCOL+8)+XMATRIX(XROW,XCOL+9)
               CASE XSELEC=6   && STYLE DESC
                  @ XROW+1,XCOL SAY XMATRIX(XROW,XCOL)+XMATRIX(XROW,XCOL+1)+;
                    XMATRIX(XROW,XCOL+2)+XMATRIX(XROW,XCOL+3)+XMATRIX(XROW,XCOL+4)
               CASE XSELEC=7   && STYLE PRICE A
                  @ XROW+1,XCOL SAY XMATRIX(XROW,XCOL)+XMATRIX(XROW,XCOL+1)+;
                    XMATRIX(XROW,XCOL+2)+XMATRIX(XROW,XCOL+3)+XMATRIX(XROW,XCOL+4)+;
                    XMATRIX(XROW,XCOL+5)
               CASE XSELEC=8   && FABRIC DESC
                  @ XROW+1,XCOL SAY XMATRIX(XROW,XCOL)+XMATRIX(XROW,XCOL+1)+;
                    XMATRIX(XROW,XCOL+2)+XMATRIX(XROW,XCOL+3)+XMATRIX(XROW,XCOL+4)
               CASE XSELEC=9   && FABRIC CONT
                  @ XROW+1,XCOL SAY XMATRIX(XROW,XCOL)+XMATRIX(XROW,XCOL+1)+;
                    XMATRIX(XROW,XCOL+2)+XMATRIX(XROW,XCOL+3)+XMATRIX(XROW,XCOL+4)
               CASE XSELEC=10   && color code
                  @ XROW+1,XCOL SAY XMATRIX(XROW,XCOL)+XMATRIX(XROW,XCOL+1)+;
                    XMATRIX(XROW,XCOL+2)+XMATRIX(XROW,XCOL+3)+XMATRIX(XROW,XCOL+4)+;
                    XMATRIX(XROW,XCOL+5)
            ENDCASE
            XROW=XROW-1
         ENDIF

      CASE THISKEY = QDWNKEY                      && Down arrow
         IF XROW=XLLENGTH    && out of range
            ? CHR(7)
         ELSE
            DO CASE
               CASE XSELEC = 1   && cuttkt
                  @ XROW+1,XCOL SAY XMATRIX(XROW,XCOL)+XMATRIX(XROW,XCOL+1)+;
                    XMATRIX(XROW,XCOL+2)+XMATRIX(XROW,XCOL+3)+XMATRIX(XROW,XCOL+4)+;
                    XMATRIX(XROW,XCOL+5)
               CASE XSELEC = 2   && style
                  @ XROW+1,XCOL SAY XMATRIX(XROW,XCOL)+XMATRIX(XROW,XCOL+1)+;
                    XMATRIX(XROW,XCOL+2)+XMATRIX(XROW,XCOL+3)+XMATRIX(XROW,XCOL+4)+;
                    XMATRIX(XROW,XCOL+5)+XMATRIX(XROW,XCOL+6)+XMATRIX(XROW,XCOL+7)+;
                    XMATRIX(XROW,XCOL+8)+XMATRIX(XROW,XCOL+9)+XMATRIX(XROW,XCOL+10)+;
                    XMATRIX(XROW,XCOL+11)
               CASE XSELEC = 3   && size
                  @ XROW+1,XCOL SAY XMATRIX(XROW,XCOL)+XMATRIX(XROW,XCOL+1)+;
                    XMATRIX(XROW,XCOL+2)+XMATRIX(XROW,XCOL+3)+XMATRIX(XROW,XCOL+4)
               CASE XSELEC = 4   && color
                  @ XROW+1,XCOL SAY XMATRIX(XROW,XCOL)+XMATRIX(XROW,XCOL+1)+;
                    XMATRIX(XROW,XCOL+2)
               CASE XSELEC = 5   && DYELOT
                  @ XROW+1,XCOL SAY XMATRIX(XROW,XCOL)+XMATRIX(XROW,XCOL+1)+;
                    XMATRIX(XROW,XCOL+2)+XMATRIX(XROW,XCOL+3)+XMATRIX(XROW,XCOL+4)+;
                    XMATRIX(XROW,XCOL+5)+XMATRIX(XROW,XCOL+6)+XMATRIX(XROW,XCOL+7)+;
                    XMATRIX(XROW,XCOL+8)+XMATRIX(XROW,XCOL+9)
               CASE XSELEC = 6   && STYLE DESC
                  @ XROW+1,XCOL SAY XMATRIX(XROW,XCOL)+XMATRIX(XROW,XCOL+1)+;
                    XMATRIX(XROW,XCOL+2)+XMATRIX(XROW,XCOL+3)+XMATRIX(XROW,XCOL+4)
               CASE XSELEC = 7   && STYLE PRICE A
                  @ XROW+1,XCOL SAY XMATRIX(XROW,XCOL)+XMATRIX(XROW,XCOL+1)+;
                    XMATRIX(XROW,XCOL+2)+XMATRIX(XROW,XCOL+3)+XMATRIX(XROW,XCOL+4)+;
                    XMATRIX(XROW,XCOL+5)
               CASE XSELEC = 8   && FABRIC DESC
                  @ XROW+1,XCOL SAY XMATRIX(XROW,XCOL)+XMATRIX(XROW,XCOL+1)+;
                    XMATRIX(XROW,XCOL+2)+XMATRIX(XROW,XCOL+3)+XMATRIX(XROW,XCOL+4)
               CASE XSELEC = 9   && FABRIC CONT
                  @ XROW+1,XCOL SAY XMATRIX(XROW,XCOL)+XMATRIX(XROW,XCOL+1)+;
                    XMATRIX(XROW,XCOL+2)+XMATRIX(XROW,XCOL+3)+XMATRIX(XROW,XCOL+4)
               CASE XSELEC = 10   && color code
                  @ XROW+1,XCOL SAY XMATRIX(XROW,XCOL)+XMATRIX(XROW,XCOL+1)+;
                    XMATRIX(XROW,XCOL+2)+XMATRIX(XROW,XCOL+3)+;
                    XMATRIX(XROW,XCOL+4)+XMATRIX(XROW,XCOL+5)
            ENDCASE
            XROW=XROW+1
         ENDIF
   ENDCASE
   DO SQPRINT
ENDDO

*-- validation for overlaping
XCOOR(XSELEC,1)=XROW
XCOOR(XSELEC,2)=XCOL
IF XCOOR(1,1)=XCOOR(2,1) .AND. XCOOR(1,1)<>0 .AND. (XSELEC=1 .OR. XSELEC=2)
   IF (XCOOR(1,2)>=XCOOR(2,2) .AND. XCOOR(1,2)<=XCOOR(2,2)+6);
   .OR. (XCOOR(1,2)+5>=XCOOR(2,2) .AND. XCOOR(1,2)+5<=XCOOR(2,2)+6)
       XCOL=0
   ENDIF
ENDIF
IF XCOOR(1,1)=XCOOR(3,1) .AND. XCOOR(1,1)<>0 .AND. (XSELEC=1 .OR. XSELEC=3)
   IF (XCOOR(3,2)>=XCOOR(1,2) .AND. XCOOR(3,2)<=XCOOR(1,2)+5);
   .OR. (XCOOR(3,2)+2>=XCOOR(1,2) .AND. XCOOR(3,2)+2<=XCOOR(1,2)+5)
       XCOL=0
   ENDIF
ENDIF
IF XCOOR(2,1)=XCOOR(3,1) .AND. XCOOR(2,1)<>0 .AND. (XSELEC=2 .OR. XSELEC=3)
   IF (XCOOR(3,2)>=XCOOR(2,2) .AND. XCOOR(3,2)<=XCOOR(2,2)+6);
   .OR. (XCOOR(3,2)+2>=XCOOR(2,2) .AND. XCOOR(3,2)+2<=XCOOR(2,2)+6)
   XCOL=0
   ENDIF
ENDIF

IF XCOL<>0
   XW=0
   DO WHILE XW<XSELWID+1
      XMATRIX(XROW,XCOL+XW)=SPACE(1)
      XW=XW+1
   ENDDO
ELSE
   SET COLOR TO &QCLRBLK
   XW=0
   DO WHILE XW<XSELWID+1
      @ XCOOR(XSELEC,1)+1,XCOOR(XSELEC,2)+XW SAY ' '
      XW=XW+1
   ENDDO
   SET COLOR TO &QCLRNRM
ENDIF

XAUX(XROW,XSELEC)=XCOL
RETURN

*!*************************************************************
*! Name      : SQPRINTPO
*! Developer : Khalid Mohi El-Din Mohamed - (KHM)
*! Date      : 12/31/98
*! Purpose   : To print the matrix elements in the screen.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  DO SQPRINTPO
*!*************************************************************
PROCEDURE SQPRINTPO

SET COLOR TO &QCLRREV

*-- Print matrix Cuttkt, Style, Color ,.........
XL=0

DO WHILE XL<22 
  XL=XL+1
  **-- out of range cleaning of elements from memory
  IF XAUX(XL,1) > XLWIDTH-5 .OR. (XAUX(XL,1)>0 .AND. XL>XLLENGTH) && cuttkt
    XAUX(XL,1)=0
  ENDIF
  IF XAUX(XL,2) > XLWIDTH - lnMajLen .OR. (XAUX(XL,2)>0 .AND. XL>XLLENGTH) && style
    XAUX(XL,2)=0
  ENDIF
  IF XAUX(XL,3) > XLWIDTH-4 .OR. (XAUX(XL,3)>0 .AND. XL>XLLENGTH) && size
    XAUX(XL,3)=0
  ENDIF
  IF XAUX(XL,4) > XLWIDTH-2 .OR. (XAUX(XL,4)>0 .AND. XL>XLLENGTH) && color
    XAUX(XL,4)=0
  ENDIF
  IF XAUX(XL,5) > XLWIDTH-4 .OR. (XAUX(XL,5)>0 .AND. XL>XLLENGTH) && styledesc
    XAUX(XL,5)=0
  ENDIF
  IF XAUX(XL,6) > XLWIDTH-5 .OR. (XAUX(XL,6)>0 .AND. XL>XLLENGTH) && stylePRICEA
    XAUX(XL,6)=0
  ENDIF

  *-C101560 WAB [if lcMfPoType = 'P'] THEN U R using program from PO ELSE From SO
  *-C101560 WAB Start

  IF lcMfPoType = 'P'			&& PURCHASE ORDER
 
  *-C101560 WAB Start
 
    IF XAUX(XL,7) > XLWIDTH-4 .OR. (XAUX(XL,7)>0 .AND. XL>XLLENGTH) && FABRIC DESC
      XAUX(XL,7)=0
    ENDIF
    IF XAUX(XL,8) > XLWIDTH-4 .OR. (XAUX(XL,8)>0 .AND. XL>XLLENGTH) && FABRIC CONT
      XAUX(XL,8)=0
    ENDIF
    IF XAUX(XL,9) > XLWIDTH-5 .OR. (XAUX(XL,9)>0 .AND. XL>XLLENGTH) && color CODE
      XAUX(XL,9)=0
    ENDIF

  *-C101560 WAB Start

  ELSE
    IF XAUX(XL,7) > XLWIDTH-5 .OR. (XAUX(XL,7)>0 .AND. XL>XLLENGTH) && color CODE
      XAUX(XL,7)=0
    ENDIF
  ENDIF

  *-C101560 WAB Start

  *-- print on screen
  IF XAUX(XL,1)>0
    
    *-C101560 WAB [if lcMfPoType = 'P'] THEN U R using program from PO ELSE From SO
    *-C101560 WAB Start

    *@ XL+1,XAUX(XL,1) SAY 'PO    '

    @ XL+1,XAUX(XL,1) SAY IIF(lcMfPoType = 'P','PO    ','ORDER ')

    *-C101560 WAB END

  ENDIF
  IF XAUX(XL,2)>0
    *@ XL+1,XAUX(XL,2) SAY 'STYLE       '
    @ XL+1,XAUX(XL,2) SAY PADR(ALLTRIM(lcStyHdr),lnMajLen)
  ENDIF
  IF XAUX(XL,3)>0
    @ XL+1,XAUX(XL,3) SAY 'SIZE '
  ENDIF
  IF XAUX(XL,4)>0
    @ XL+1,XAUX(XL,4) SAY 'CLR'
  ENDIF
  IF XAUX(XL,5)>0
    @ XL+1,XAUX(XL,5) SAY 'SDESC'
  ENDIF
  IF XAUX(XL,6)>0
    @ XL+1,XAUX(XL,6) SAY 'PRICEA'
  ENDIF
 
  *-C101560 WAB [if lcMfPoType = 'P'] THEN U R using program from PO ELSE From SO
  *-C101560 WAB Start

  IF lcMfPoType = 'P'    && PURCHASE ORDER

  *-C101560 WAB END
  
    IF XAUX(XL,7)>0
      @ XL+1,XAUX(XL,7) SAY 'FDESC'    
     ENDIF
    IF XAUX(XL,8)>0
      @ XL+1,XAUX(XL,8) SAY 'FCONT'
    ENDIF
    IF XAUX(XL,9)>0
      @ XL+1,XAUX(XL,9) SAY 'CLRCOD'
    ENDIF
   
   *-C101560 WAB Start
   ELSE
    IF XAUX(XL,7)>0
      @ XL+1,XAUX(XL,7) SAY 'CLRCOD'   
     ENDIF
   ENDIF
   *-C101560 WAB END
   
ENDDO
SET COLOR TO &QCLRNRM
RETURN

*!*************************************************************
*! Name      : MOVETXTPO
*! Developer : Khalid Mohi El-Din Mohamed - (KHM)
*! Date      : 12/31/98
*! Purpose   : To accomodate text on the lable form.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  DO MOVETXTPO
*!*************************************************************
PROCEDURE MOVETXTPO

XROW = 1
XCOL = 1

SET COLOR TO &QCLRNRM
@ 24,0 SAY SPACE(80)

IF ISCOLOR()
  SET COLOR TO &QCLRSAY
ELSE
  SET COLOR TO &QCLRREV
ENDIF

DO MATPRINT
DO WHILE .T.
  DO SQPRINTPO
  @ XROW+1,XCOL SAY ''
  THISKEY=INKEY(0)
  DO CASE
    CASE THISKEY = QESCKEY
      EXIT
    CASE THISKEY>31 .AND. THISKEY<127    && Text
      IF ISCOLOR()
        SET COLOR TO &QCLRSAY
      ELSE
        SET COLOR TO &QCLRREV
      ENDIF
      @ XROW+1,XCOL SAY SUBSTR(CHR(THISKEY),1,1) PICTURE 'X'
        XMATRIX(XROW,XCOL)=SUBSTR(CHR(THISKEY),1,1)

      IF XCOL=XLWIDTH .AND. XROW=XLLENGTH
        ? CHR(7)
      ELSE
        IF XCOL=XLWIDTH
          XCOL = 1
          XROW = XROW+1
        ELSE
          XCOL = XCOL+1
        ENDIF
      ENDIF

    CASE THISKEY = QLFTKEY .OR. THISKEY = 127   && Left arrow or Backspace
      IF XCOL=1         && out of range
        IF XROW=1
          ? CHR(7)
        ELSE
          XROW = XROW - 1
          XCOL = XLWIDTH
        ENDIF
      ELSE
        XCOL=XCOL-1
      ENDIF

    CASE THISKEY = QRGTKEY                      && Right arrow
      IF XCOL=XLWIDTH   && out of range
        IF XROW=XLLENGTH
          ? CHR(7)
        ELSE
          XROW=XROW+1
          XCOL=1
        ENDIF
      ELSE
        XCOL=XCOL+1
      ENDIF

    CASE THISKEY = QUPKEY                       && Up arrow
      IF XROW=1         && out of range
        XROW=XLLENGTH
      ELSE
        XROW=XROW-1
      ENDIF

    CASE THISKEY = QDWNKEY                      && Down arrow
      IF XROW=XLLENGTH    && out of range
        XROW=1
      ELSE
        XROW=XROW+1
      ENDIF

    CASE THISKEY = QRETKEY                      && Return key
       IF XROW=XLLENGTH    && out of range
         ? CHR(7)
       ELSE
         XROW=XROW+1
         XCOL=1
       ENDIF
   ENDCASE
ENDDO

*-- cleaning overlaped text from matrix
XL=0
DO WHILE XL<XLLENGTH
   XL=XL+1
   XCC=1
   DO WHILE XCC<=8

    *-C101560 WAB [if lcMfPoType = 'P'] THEN U R using program from PO ELSE From SO
    *-C101560 WAB Start

    IF lcMfPoType = 'P'    && PURCHASE ORDER

    *-C101560 WAB END
      DO CASE
         CASE XCC = 1
           XCCL=6   && PO is 6 chars
         CASE XCC = 2
           *XCCL = 12  && style is 12 chars
           XCCL = lnMajLen  && style is 12 chars
           XCCL = 5   && size is 5 chars
         CASE XCC = 4
           XCCL = 3   && color is 3 chars
         CASE XCC = 5
           XCCL = 5   && SDESC is 5 chars
         CASE XCC = 6
           *B603713,10 HBG 11/22/2000 Increase The Width Of The pricea [Begin]
           *XCCL = 6     && PRICEA is 6 chars
           XCCL = 12     && PRICEA is 12 chars
           *B603713,10 [END]
         CASE XCC = 7
           XCCL = 5     && FDESC is 5 chars
         CASE XCC = 8
           XCCL = 5     && FCONT is 5 chars
         CASE XCC = 9
           XCCL = 6
       ENDCASE

     *-C101560 WAB Start
     ELSE
      DO CASE
         CASE XCC = 1
           XCCL=6   && PO is 6 chars
         CASE XCC = 2
           *XCCL = 12  && style is 12 chars
           XCCL = lnMajLen  && style is 12 chars
           XCCL = 5   && size is 5 chars
         CASE XCC = 4
           XCCL = 3   && color is 3 chars
         CASE XCC = 5
           XCCL = 5   && SDESC is 5 chars
         CASE XCC = 6
           *B603713,10 HBG 11/22/2000 Increase The Width Of The pricea [Begin]
           *XCCL = 6     && PRICEA is 6 chars
           XCCL = 12     && PRICEA is 12 chars
           *B603713,10 [End]
         CASE XCC = 7
           XCCL = 6
       ENDCASE
     ENDIF
    *-C101560 WAB END


     IF XAUX(XL,XCC)<>0
        XW=0
        DO WHILE XW<XCCL
           XMATRIX(XL,XAUX(XL,XCC)+XW)=SPACE(1)
           XW=XW+1
        ENDDO
     ENDIF
     XCC=XCC+1
   ENDDO
ENDDO
RETURN

*!*************************************************************
*! Name      : MOVETXT
*! Developer : Khalid Mohi El-Din Mohamed - (KHM)
*! Date      : 12/31/98
*! Purpose   : To accomodate text on the label form.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  DO MOVETXT
*!*************************************************************
PROCEDURE MOVETXT

XROW=1
XCOL=1

SET COLOR TO &QCLRNRM
@ 24,0 SAY SPACE(80)

IF ISCOLOR()
  SET COLOR TO &QCLRSAY
ELSE
  SET COLOR TO &QCLRREV
ENDIF

DO MATPRINT
DO WHILE .T.
  DO SQPRINT
  @ XROW+1,XCOL SAY ''
  THISKEY=INKEY(0)
  DO CASE
    CASE THISKEY = QESCKEY
      EXIT
    CASE THISKEY>31 .AND. THISKEY<127    && Text
      IF ISCOLOR()
        SET COLOR TO &QCLRSAY
      ELSE
        SET COLOR TO &QCLRREV
      ENDIF
      @ XROW+1,XCOL SAY SUBSTR(CHR(THISKEY),1,1) PICTURE 'X'
      XMATRIX(XROW,XCOL)=SUBSTR(CHR(THISKEY),1,1)
      IF XCOL=XLWIDTH .AND. XROW=XLLENGTH
        ? CHR(7)
      ELSE
        IF XCOL=XLWIDTH
          XCOL=1
          XROW=XROW+1
        ELSE
          XCOL=XCOL+1
        ENDIF
      ENDIF

      CASE THISKEY = QLFTKEY .OR. THISKEY = 127   && Left arrow or Backspace
        IF XCOL=1         && out of range
          IF XROW=1
            ? CHR(7)
          ELSE
            XROW=XROW-1
            XCOL=XLWIDTH
          ENDIF
        ELSE
          XCOL=XCOL-1
        ENDIF

     CASE THISKEY = QRGTKEY                      && Right arrow
       IF XCOL=XLWIDTH   && out of range
         IF XROW=XLLENGTH
           ? CHR(7)
         ELSE
           XROW=XROW+1
           XCOL=1
         ENDIF
       ELSE
         XCOL=XCOL+1
       ENDIF

     CASE THISKEY = QUPKEY                       && Up arrow
       IF XROW=1         && out of range
         XROW=XLLENGTH
       ELSE
         XROW=XROW-1
       ENDIF

     CASE THISKEY = QDWNKEY                      && Down arrow
       IF XROW=XLLENGTH    && out of range
         XROW=1
       ELSE
         XROW=XROW+1
       ENDIF

     CASE THISKEY = QRETKEY                      && Return key
       IF XROW=XLLENGTH    && out of range
         ? CHR(7)
       ELSE
         XROW=XROW+1
         XCOL=1
       ENDIF

  ENDCASE

ENDDO

*** cleaning overlaped text from matrix
XL=0
DO WHILE XL<XLLENGTH
  XL=XL+1
  XCC=1
  DO WHILE XCC<=9
    DO CASE
      CASE XCC=1
         XCCL=6   && cuttkt is 6 chars
       CASE XCC=2
         *XCCL=12  && style is 12 chars
         XCCL = lnMajLen
       CASE XCC=3
         XCCL=5   && size is 5 chars
       CASE XCC=4
         XCCL=3   && color is 3 chars
       CASE XCC=5
         XCCL=10  && DYLOT is 10 chars
       CASE XCC=6
         XCCL=5   && SDESC is 5 chars
       CASE XCC=7
         *B603713,10 HBG 11/22/2000 Increase The Width Of The pricea [Begin]
         *XCCL=6     && PRICEA is 6 chars
         XCCL=12     && PRICEA is 12 chars
         *B603713,10 [End]
       CASE XCC=8
         XCCL=5     && FDESC is 5 chars
       CASE XCC=9
         XCCL=5     && FCONT is 5 chars
       CASE XCC=10
         XCCL=6
     ENDCASE
     IF XAUX(XL,XCC)<>0
        XW=0
        DO WHILE XW<XCCL
           XMATRIX(XL,XAUX(XL,XCC)+XW)=SPACE(1)
           XW=XW+1
        ENDDO
     ENDIF
     XCC=XCC+1
   ENDDO
ENDDO
RETURN

*!*************************************************************
*! Name      : HANG150
*! Developer : Khalid Mohi El-Din Mohamed - (KHM)
*! Date      : 12/31/98
*! Purpose   : To get the nonmajor segements.
*!*************************************************************
*! Calls     : 
*!     Procedures : HANG160.            PO browse procedure.
*!                  HANG160A,HANG160B.  Print hang tags form A,B procedure.
*!                  CARE400,CARE410.    Print Care Labels procedure.
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  DO HANG150 
*!*************************************************************
PROCEDURE HANG150
PARAMETER lcPrtType

** Selection Criteria HANG150 & Validation
***************************************
*....+....1....+....2....+....3....+....4....+....5....+....6
*    Select : (E)ntered (C)omplete (P)o (V)endor..: X
*    Status : (O)pen    (H)old     (A)ll..........: X
*    Date Range...... (Date Range or Vendor in same line)
*    Vendor.......................................: XXXXXXX
*    Percentage to process........................: 999 %
*    Form code to print (A) or (B)................: X
*C101816,1 ABD -Add new option of sorting in case of comming from 
*C101816,1 ABD -sales order module only. [Begin]
*    Sorted : Style/(S)ize   or   Style/(C)olor...: X
*:*    Sorted : Style -Color/(S)ize   or   Style/(C)olor...: X

*:*C101816,1 ABD After modifications ************************************
*    Sorted : (1) Order/Style -Color /Size      (2) Order/Style  -  Color...: X
*           : (3) Style -Color/Size/Order ....:  X 
*C101816,1 ABD- [End]
*....+....1....+....2....+....3....+....4....+....5....+....6

SET COLOR TO &qClrNrm
CLEA

lcScrHdr=IIF(lcPrtType='H','HANG TAGS REPORTS','CARE LABELS REPORT')
DO lpPrnHdr WITH 'HANG150',lcScrHdr

*C101816,1 ABD - Add new row in the option grid to hold the new type of sort. [Begin]
IF lcMfPoType='S'
  DO GRID WITH 7,'N'
ELSE
  DO GRID WITH 6,'N'
ENDIF
*C101816,1 ABD- [End]

STORE SPACE(1) TO XSEL,XSTAT
STORE SPACE(8) TO XVEN
STORE CTOD('  /  /  ') TO NULLDATE
STORE NULLDATE TO LDATE,HDATE
XSELSIZE = 'A'
XPERC    = 100
*XSORT    = 'C'
*C101816,1 ABD - Add the default option in the option grid that hold the
*C101816,1 ABD -  new type of sort that type (3) "Style - Color /Size/Order" [Begin]
*XSORT    = '2'
IF lcMfPoType='S'
  XSORT    = '3'
ELSE
  XSORT    = '2'
ENDIF
*C101816,1 ABD- [End]
XFILTER  = ''

*-- Form Codes Setting
IF FILE('HANG_CAR.MEM')
  RESTORE FROM HANG_CAR ADDI
ELSE
  FCODE='A'
  SAVE TO HANG_CAR ALL LIKE FCODE
ENDIF

lcStyleHdr = ALLTRIM(gfItemMask("HI"))

*-C101560 WAB if llpo is true (U R using the program  from PO .OR. SO ) 
*-C101560 WAB [if lcMfPoType = 'P'] THEN U R using program from PO ELSE From SO
*-C101560 WAB - Start 

*  @ 03,05 SAY 'Select : (E)ntered (C)omplete (P)o  (V)endor..: '
IF lcMfPoType = 'P' 	&& Purchase Order
  @ 03,05 SAY 'Select : (E)ntered (C)omplete (P)o  (V)endor..: '
ELSE					&& Sales Order
  @ 03,05 SAY 'Select : (E)ntered (C)omplete (S)o  c(U)stomer: '
ENDIF

*-C101560 WAB - END

@ 05,05 SAY 'Status : (O)pen    (H)old     (A)ll..........: '
@ 09,05 SAY 'Percentage to process........................:     %'
@ 11,05 SAY 'Select Size:  (A)LL /1/2/3/4/5/6/7/8 ........: '
IF lcPrtType='H'
  @ 13,05 SAY 'Form code to print (A) or (B)..............: '
ELSE
  *@ 13,05 SAY 'Sorted : Style/(S)ize   or   Style/(C)olor.: '
  *C101816,1 ABD -Add new option of sorting in case of comming from 
  *C101816,1 ABD -sales order module only. [Begin]
  *@ 13,05 SAY 'Sorted : (1)'+lcStyleHdr+"/"+"Size"+" or (2)"+lcStyleHdr+":"
  IF lcMfPoType='S'
    @ 13,05 SAY 'Sorted : (1) Order/'+lcStyleHdr+"/"+"Size"+"  (2) Order/"+lcStyleHdr
    @ 15,05 SAY '       : (3) '+lcStyleHdr+"/"+"Size"+"/"+"Order"+" ...:"
  ELSE
    @ 13,05 SAY 'Sorted : (1)'+lcStyleHdr+"/"+"Size"+" or (2)"+lcStyleHdr+":"
  ENDIF  
  *C101816,1 ABD- [End]
ENDIF 

DO WHILE .T.
  *-- Getting the fields

  *-C101560 WAB if llpo is true (U R using the program  from PO .OR. SO ) 
  *-C101560 WAB [if lcMfPoType = 'P'] THEN U R using program from PO ELSE From SO
  *-C101560 WAB Start

  *@ 03,52 GET XSEL PICTURE '!'  VALID XSEL$'ECPV'

  IF lcMfPoType = 'P' 	&& Purchase Order
    @ 03,52 GET XSEL PICTURE '!'  VALID XSEL$'ECPV'
  ELSE					&& Sales Order
    @ 03,52 GET XSEL PICTURE '!'  VALID XSEL$'ECSU'
  ENDIF

  *-C101560 WAB -End

  @ 05,52 GET XSTAT PICTURE '!' VALID XSTAT$'OHA'
  READ
  IF READKEY() = 12
    RETURN
  ENDIF
  IF XSEL ='V' 
    @ 07,52 SAY SPACE(27)
    @ 07,05 SAY 'Vendor.......................................:'
    @ 07,52 GET XVEN PICTURE '!!!!!!!!'
    READ
    IF !SEEK (xVen,'ApVendor')
      SAVE SCREEN TO lcGridScr

      *- E301268 WAB - ADD Parameter (.F.,'C') to gfApVnBrow
      *- E301268 WAB - START
      *-=gfApVnBrow(@XVen)

      =gfApVnBrow(@XVen,.F.,'C')
 
      *- E301268 WAB - END

      RESTORE SCREEN FROM lcGridScr
      @ 07,52 SAY XVEN COLOR &qClrSay
    ENDIF
    IF EMPTY(XVEN)
      LOOP
    ENDIF
    @ 07,61 SAY SUBSTR(ApVendor.cVenComp,1,16)
    XFILTER='(XVEN=VENDOR)'
  ENDIF

  *-C101560 WAB  - When user choose Customer from grid
  *-C101560 WAB -Start 

  IF XSEL ='U'     && USER choose Customer from grid
    @ 07,52 SAY SPACE(27)
    @ 07,05 SAY 'Customer.......................................:'
    @ 07,52 GET XVEN PICTURE '!!!!!!!!'
    READ
    *khalid
    *IF !SEEK (xVen,'CUSTOMER')
    IF !SEEK ('M'+ALLTRIM(xVen),'CUSTOMER')
      SAVE SCREEN TO lcGridScr
      xAccount = xVen
      DO CUSBROWM WITH xAccount
      xVen = xAccount
      RESTORE SCREEN FROM lcGridScr
      @ 07,52 SAY XVEN COLOR &qClrSay
    ENDIF
    IF EMPTY(XVEN)
      LOOP
    ENDIF
    @ 07,61 SAY SUBSTR(Customer.BtName,1,16)
    XFILTER='(XVEN=ACCOUNT)'
  ENDIF
  
  *-C101560 WAB -END
  
  IF XSEL $ 'EC'          && IF XSEL='E' OR XSEL='C'
    @ 07,52 SAY SPACE(27)
    IF XSEL='E'
      @ 07,05 SAY 'Date Entered range...........................:'
    ELSE
      @ 07,05 SAY 'Date Completed range.........................:'
    ENDIF
    @ 07,52 GET LDATE
    @ 07,63 GET HDATE
    READ
    IF EMPTY(HDATE)
      HDATE=LDATE
    ENDIF
    IF .NOT. EMPTY(LDATE) .AND. (HDATE<LDATE)
      WAIT WINDOW 'Date range error !!!'
      LOOP
    ENDIF
    IF .NOT. EMPTY(HDATE)
      IF XSEL='E'
        XFILTER='(ENTERED >=LDATE .AND. ENTERED<=HDATE)'
      ELSE
        XFILTER='(COMPLETE >=LDATE .AND. COMPLETE <=HDATE)'
      ENDIF
    ENDIF
  ENDIF
  @ 09,52 GET XPERC PICTURE '999' VALID XPERC > 0
  @ 11,52 GET XSELSIZE PICTURE '!' VALID XSELSIZE $ 'A12345678'
  IF lcPrtType='H'
    @ 13,52 GET FCODE PICTURE '!' VALID FCODE $ 'AB'
  ELSE
    *@ 13,52 GET XSORT PICTURE '!' VALID XSORT $ 'SC'  
    *C101816,1 ABD -Add new option of sorting in case of comming from 
    *C101816,1 ABD -sales order module only. [Begin]
    IF lcMfPoType='S'
      @ 15,52 GET XSORT PICTURE '!' VALID XSORT $ '123'    
    ELSE
      @ 13,31+(LEN(lcStyleHdr)*2) GET XSORT PICTURE '!' VALID XSORT $ '12'
    ENDIF  
    *C101816,1 ABD - [End]
  ENDIF
  READ

  IF READKEY()=12
    llCancel = .T.  
    RETURN
  ENDIF

  SAVE TO HANG_CAR ALL LIKE FCODE
 
  IF XSTAT <> 'A'
    IF EMPTY(XFILTER)
      XFILTER = 'STATUS $ XSTAT '
    ELSE
      XFILTER = XFILTER+' .AND. STATUS $ XSTAT'
    ENDIF
  ENDIF
  DO MSG22 WITH '<P>roceed <M>odify <C>ancel ? ','PMC'
  DO CASE
    CASE CHOICE = 'P'
      EXIT
    CASE CHOICE = 'C'
      llCancel = .T.  
      RETURN
  ENDCASE
ENDDO

*-- Temporary file for selected records

*-C101560 WAB To indicate whether U R using the program from PO OR SO.
*-C101560 WAB -Start 

IF lcMfPoType = 'P' 	&& Purchase Order

*-C101560 WAB -END

  IF USED('&POTEMP')
    SELECT &POTEMP
    USE
  ENDIF
  DO MSG22 WITH 'Selecting the records ....','@'
  SELECT POSHDR
  IF ! EMPTY(XFILTER)
    COPY TO &gcWorkDir.&POTEMP FIELDS cStyType,PO,FLAG FOR &XFILTER
  ELSE
    COPY TO &gcWorkDir.&POTEMP FIELDS cStyType,PO,FLAG
  ENDIF
  =gfOpenFile(gcWorkDir+POTEMP,' ','EX')
  INDEX ON PO TAG &POTEMP
  SET ORDER TO TAG &POTEMP
  SET RELATION TO 'P'+PO INTO POSHDR

*-C101560 WAB -Start 

ELSE  				&& Sales Order
  IF USED('&lcOrdTmp')
    SELECT &lcOrdTmp
    USE
  ENDIF
  DO MSG22 WITH 'Selecting the records ....','@'
  SELECT ORDHDR
  IF ! EMPTY(XFILTER)
    COPY TO &gcWorkDir.&lcOrdTmp FIELDS cOrdType,ORDER,FLAG FOR &XFILTER
  ELSE
    COPY TO &gcWorkDir.&lcOrdTmp FIELDS cOrdType,ORDER,FLAG
  ENDIF
  =gfOpenFile(gcWorkDir+lcOrdTmp,' ','EX')
  INDEX ON Order TAG &lcOrdTmp
  SET ORDER TO TAG &lcOrdTmp
  SET RELATION TO 'O'+ORDER INTO ORDHDR

ENDIF

*-C101560 WAB -END 

GO TOP
IF EOF()
  *-- No records selected for the report
  DO MSG22 WITH 'No records selected !!!','$'
  llCancel = .T.  
  RETURN
ENDIF
IF XSEL='P'		
  SELECT &POTEMP
  REPL ALL FLAG WITH 'N'
  SAVE SCREEN TO lsHG100
  DO HANG160          && Select Cutting Tickets to print
  RESTORE SCREEN FROM lsHG100
  SELECT &POTEMP
  SET FILTER TO FLAG='Y'
  GO TOP
  IF EOF()
    *-- No records selected for the report
    DO MSG22 WITH 'No records selected !!!','$'
    llCancel = .T.  
    RETURN
  ENDIF
ENDIF

*-C101560 WAB When user choose SO from  grid
*-C101560 WAB -Start 

IF XSEL='S'				&& USER Choose SO From Grid
  SELECT &lcOrdTmp
  REPL ALL FLAG WITH 'N'
  SAVE SCREEN TO lsHG100
  DO HANG160          
  RESTORE SCREEN FROM lsHG100
  SELECT &lcOrdTmp
  SET FILTER TO FLAG='Y'
  GO TOP
  IF EOF()
    *-- No records selected for the report
    DO MSG22 WITH 'No records selected !!!','$'
    llCancel = .T.  
    RETURN
  ENDIF
ENDIF

*-C101560 WAB -END

IF lcPrtType='H'    && Printing Hang Tag
  R_WIDTH='N'
  S_CLOSEALL=.F.
  DO MSG22 WITH 'Pre<v>iew <P>rint <C>ancel ? ','VPC'
  lcOGDevice  = gcDevice
  lcOGOutFile = gcOutFile
  lcOGWinTitl = ''
  DO CASE
    CASE Choice = "V"
      gcDevice  = "SCREEN"    && set printer to temprory file name
      gcOutFile = gcWorkDir+gfTempName()+".TXT"      
      SET PRINTER TO &gcOutFile
      DO MSG22 WITH 'PRINTING - <SPACE BAR> TO ABORT','@'
      SET DEVICE TO PRINTER
    CASE Choice = "P"

      *-C101560 WAB To let the Psetup() Display The Paper Type & Display Aria Print Setup
      *-C101560 WAB -Start 

      OGPlatForm = 'DOS'
      lcOGPlatForm = OGPlatForm

      *-C101560 WAB - END

      IF !PSETUP(.T.,.F.)
        llCancel = .T.
        SET DEVICE TO SCREEN
        RETURN    
      ENDIF
      gcOutFile = gcWorkDir+gfTempName()+".TXT"      
      SET DEVICE TO SCREEN
      DO MSG22 WITH 'PRINTING - <SPACE BAR> TO ABORT','@'
      SET PRINTER TO &gcOutFile      
      SET DEVICE TO PRINTER
    CASE Choice = "C"
      llCancel = .T. 
      SET DEVICE TO SCREEN 
      RETURN    
  ENDCASE  
  DO HANG160&FCODE
  DO ENDREPORT
  SET PRINTER TO 
  SET DEVICE TO SCREEN
  gcDevice  = lcOGDevice
  gcOutFile = lcOGOutFile
ELSE               && Printing Care Labels
  XMATRIX    = SPACE(1)
  XAUX       = 0
  XLABELCODE = SPACE(8)
  XLDESC     = SPACE(25)
  XSIZECODE  = SPACE(1)
  XPITCH     = '12'
  XPWIDTH    = 'N'
  XLLENGTH   = 16
  XLWIDTH    = 10
  XNUMACROSS = 1

  SELECT CARELAB
  GO TOP
  IF RECCOUNT()=0 .OR. EOF()
  DO MSG22 WITH 'LABEL FORMS FILE EMPTY, Please create Labels Forms before printing Labels! ','$'
  *B602689,1 (start) you don't need to loop
  *LOOP
  *B602689,1 END  
  ENDIF

  SET COLOR TO &QCLRNRM
  @ 23,00 CLEAR TO 23,79
  GG = SYS(2002)

  *3....*....4....*....5....*....6....*....7....*...
  *   LBL CODE  ....LABEL.DESCRIPTION...  SIZ CODE
  *   12345678  1234567890123456789012345    1
  *3....*....4....*....5....*....6....*....7....*...

  DO WHILE .T.
    QRECNO = 0
    ROW    = 12
    DO DRAWBOX WITH QCLRMAG,'',09,27,21,77
    SET COLOR TO &QCLRMAG
    @ 10,33 SAY 'LBL CODE  ....LABEL.DESCRIPTION...  SZ CODE'
    @ 11,28 TO 11,76
    SCAN WHILE .NOT. EOF() .and. ROW < 21
      QRECNO(ROW) = RECNO()
      @ ROW,33 SAY LABELCODE  COLOR &qClrSay
      @ ROW,43 SAY LDESC      COLOR &qClrSay
      @ ROW,72 SAY SIZECODE   COLOR &qClrSay
      ROW=ROW+1
    ENDSCAN
    BP(1)=12
    BP(2)=29
    thiskey=BROW()
    DO CASE
      CASE thiskey = QESCKEY   && escape key
        EXIT
      CASE thiskey = QRETKEY   && return key - Label inquire and modification
        SET COLOR TO &QCLRNRM
        XLABELCODE = LABELCODE
        SEEK XLABELCODE
        IF FOUND()
          XLDESC    = LDESC
          XSIZECODE = SIZECODE
          SELECT CARESIZ
          SEEK XSIZECODE
          
          *B802212,1 KHM 05/04/99 (Begin) Commening the following 
          *B802212,1              line because the Pwidth is no 
          *B802212,1              longer exists in the CareSiz file.
          *XPWIDTH    = PWIDTH
          *B802212,1 KHM 05/04/99 (End)
          
          XPITCH     = PITCH
          XLWIDTH    = LWIDTH
          XLLENGTH   = LLENGTH
          XNUMACROSS = NUMACROSS
          SELECT CARELAB
          SET ORDER TO TAG CARELABL
          SEEK XLABELCODE
          ****** restore information
          XL=0                                 && Lenght subindex

          DO WHILE XL<XLLENGTH
            XL = XL+1
            XAUX(XL,1)=CUT
            XAUX(XL,2)=STY
            XAUX(XL,3)=SIZ
            XAUX(XL,4)=CLR                    
            XAUX(XL,5)=SDC                    
            XAUX(XL,6)=SPR                    
            XAUX(XL,7)=FDC                    
            XAUX(XL,8)=FCN                    
            XAUX(XL,9)=CCD                   
            XW = 0                            && Width subindex
            DO WHILE XW<XLWIDTH
              XW = XW+1
              XMATRIX(XL,XW) = SUBSTR(MATRIX,XW,1)
            ENDDO
            SKIP
          ENDDO
          SET ORDER TO TAG CARELAB
          SAVE SCREEN TO CARE_3

          *-C101560 WAB  Change from CPI?? TO 'W' (WIDE) / 'N' (NARROW)
          *-C101560 WAB -Start 

          *DO CASE
          *  CASE XPITCH='10'
          *    R_WIDTH='CPI10'
          *  CASE XPITCH='12'
          *    R_WIDTH='CPI12'
          *  CASE XPITCH='17'
          *    R_WIDTH='CPI17'
          *  OTHERWISE
          *    R_WIDTH='CPI10'
          *ENDCASE

          DO CASE
            CASE XPITCH='10'
              R_WIDTH='N'
            CASE XPITCH='12'
              R_WIDTH='W'
            CASE XPITCH='17'
              R_WIDTH='W'
            OTHERWISE
              R_WIDTH='N'
          ENDCASE

          *-C101560 WAB - END

          S_CLOSEALL=.F.

          DO MSG22 WITH 'Pre<v>iew <P>rint <C>ancel ? ','VPC'
          lcOGDevice  = gcDevice
          lcOGOutFile = gcOutFile
          lcOGWinTitl = ''
          
          DO CASE
            CASE Choice = "V"
              gcDevice  = "SCREEN"    && set printer to temprory file name
              gcOutFile = gcWorkDir+gfTempName()+".TXT"      
              DO MSG22 WITH 'PRINTING - <SPACE BAR> TO ABORT','@'
              SET PRINTER TO &gcOutFile
              SET DEVICE TO PRINTER
            CASE Choice = "P"
       
              *-C101560 WAB   To let the Psetup() Display The Paper Type & 
              *-C101560 WAB   Display Aria Print Setup
              *-C101560 WAB - Start 

              OGPlatForm = 'DOS'
              lcOGPlatForm = OGPlatForm

              *-C101560 WAB - END
        
              IF !PSETUP(.T.,.F.)
                llCancel = .T.
                SET DEVICE TO SCREEN
                RETURN    
              ENDIF
              gcOutFile = gcWorkDir+gfTempName()+".TXT"      
              SET DEVICE TO SCREEN
              DO MSG22 WITH 'PRINTING - <SPACE BAR> TO ABORT','@'
              SET PRINTER TO &gcOutFile
              SET DEVICE TO PRINTER
           CASE Choice = "C"
             llCancel = .T. 
             SET DEVICE TO SCREEN 
             RETURN    
          ENDCASE  

          DO  CARE400               && Printing Care Labels

          DO ENDREPORT
          SET PRINTER TO 
          SET DEVICE TO SCREEN
          gcDevice  = lcOGDevice
          gcOutFile = lcOGOutFile
          RESTORE SCREEN FROM CARE_3
        ENDIF
        EXIT
    ENDCASE
  ENDDO
  SET COLOR TO &QCLRNRM
ENDIF
IF .NOT. ISCOLOR()
  QCLRBLK=QCLRTMP
ENDIF
RETURN

*!*************************************************************
*! Name      : HANG160
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 12/31/1998
*! Purpose   : Select the cutting tickets to mark for printing..
*!*************************************************************
*....*....1....*....2....*....3....*....4....*....5....*....6....*....7....*...
* PO     VENDOR   S ENTERED  COMPLETE DI ORDERED   RECVD DAMAGED CANCEL   OPEN
* XXXXXX 12345678 X 99/99/99 99/99/99 XX 9999999 9999999 9999999 999999 999999
*....*....1....*....2....*....3....*....4....*....5....*....6....*....7....*...
****************************************************************************
PROCEDURE HANG160

*-C101560 WAB - if purchase order select  potemp
*-C101560 WAB - if sales order select   lcOrdTmp
*-C101560 WAB -start

*SELECT &POTEMP

SELECT IIF(lcMfPoType='P',POTEMP,lcOrdTmp)

*-C101560 WAB -end

GOTO TOP
FF = SELECT()
CLEAR
SET COLOR TO &QCLRNRM
DO lpPrnHdr WITH 'HANG160','SELECT PURCHASE ORDERS TO PRINT '
                           
*----------------------------------
* BROWSE/SELECT PURCHASE ORDER FILE
*----------------------------------
DO WHILE .T.

  *-C101560 WAB - if purchase order select  potemp
  *-C101560 WAB - if sales order select   lcOrdTmp
  *-C101560 WAB -start

  *SELECT &POTEMP

  SELECT IIF(lcMfPoType='P',POTEMP,lcOrdTmp)

  *-C101560 WAB -end

  IF EOF()
    GOTO TOP
  ENDIF

  LPO = SPACE(6)
  HPO = SPACE(6)

  ROW    = 4
  QRECNO = 0
  SAVREC = RECNO()

  DO DRAWBOX WITH QCLRMAG,'',1,0,21,79
  SET COLOR TO &QCLRMAG

  *-C101560 WAB To indicate whether U R using the program from PO OR SO.
  *-C101560 WAB -start

  *@ 02,01 SAY ' PO     VENDOR   S ENTERED  COMPLETE DI ORDERED   RECVD DAMAGED CANCEL   OPEN'

  IF lcMfPoType='P' 
    @ 02,01 SAY ' PO     VENDOR   S ENTERED  COMPLETE DI ORDERED   RECVD DAMAGED CANCEL   OPEN'
  ELSE
    @ 02,01 SAY ' ORDER  ACCOUNT  S ENTERED  COMPLETE DI BOOKED    SHIPED CANCEL OPEN'
  ENDIF

  *-C101560 WAB -start

  @ 03,01 TO 03,78
  SET COLOR TO &QCLRSAY
  SCAN WHILE (.NOT. EOF()) .AND. (ROW<21)
    TMPCLR = IIF(FLAG='Y', QCLRREV, QCLRSAY)
    SET COLOR TO &TMPCLR

	*-C101560 WAB - Case purchaise order browse from table poshdr
    *-C101560 WAB - Case Sales Order browse from table ORDHDR
    *-C101560 WAB -start

    IF lcMfPoType='P'      && case Purchase Order

    *-C101560 WAB - END

      @ ROW,02 SAY POSHDR->PO
      SET COLOR TO &QCLRSAY
      @ ROW,09 SAY POSHDR->VENDOR
      @ ROW,18 SAY POSHDR->STATUS
      @ ROW,20 SAY POSHDR->ENTERED
      @ ROW,29 SAY POSHDR->COMPLETE
      @ ROW,38 SAY ALLTRIM(POSHDR->CDIVISION)
      *@ ROW,41 SAY POSHDR->ORDER   PICTURE '9999999'
      @ ROW,41 SAY POSHDR->nStyOrder   PICTURE '9999999'
      @ ROW,49 SAY POSHDR->RECEIVE PICTURE '9999999'
      @ ROW,57 SAY POSHDR->DAMAGE  PICTURE '9999999' 
      @ ROW,65 SAY POSHDR->CANCEL  PICTURE '999999' 
      @ ROW,72 SAY POSHDR->OPEN    PICTURE '999999'

    *-C101560 WAB -start      

    ELSE						&& Sales Order
      @ ROW,02 SAY ORDHDR->ORDER
      SET COLOR TO &QCLRSAY
      @ ROW,09 SAY ORDHDR->ACCOUNT
      @ ROW,18 SAY ORDHDR->STATUS
      @ ROW,20 SAY ORDHDR->ENTERED
      @ ROW,29 SAY ORDHDR->COMPLETE
      @ ROW,38 SAY ALLTRIM(ORDHDR->CDIVISION)
      @ ROW,41 SAY ORDHDR->BOOK    PICTURE '9999999'
      @ ROW,49 SAY ORDHDR->SHIP    PICTURE '9999999'
      @ ROW,57 SAY ORDHDR->CANCEL  PICTURE '999999' 
      @ ROW,65 SAY ORDHDR->OPEN    PICTURE '999999'
    ENDIF

    *-C101560 WAB - END

    QRECNO(ROW) = RECNO()
    ROW = ROW + 1
  ENDSCAN

  BP(1) = 4
  BP(2) = 1
  SET COLOR TO &QCLRMAG
  THISKEY = BROW()

  IF THISKEY = 27
     RETURN
  ENDIF

  IF THISKEY <> 13
     LOOP
  ENDIF

  *-C101560 WAB - if purchase order select  potemp
  *-C101560 WAB - if sales order select   lcOrdTmp
  *-C101560 WAB -start

  *SELECT &POTEMP

  SELECT IIF(lcMfPoType='P',POTEMP,lcOrdTmp)

  *-C101560 WAB -end

  SET COLOR TO &QCLRNRM
  @ 23,00 CLEAR


  *-C101560 WAB To indicate whether U R using the program from PO OR SO.
  *-C101560 WAB -start
  *  STORE PO TO LPO, HPO
  *@ 23,20 SAY 'SELECT PO# => '

  IF lcMfPoType='P'				&& Purchase Order
    STORE PO TO LPO, HPO
    @ 23,20 SAY 'SELECT PO# => '
  ELSE							&& Sales Order	
    STORE ORDER TO LPO, HPO
    @ 23,20 SAY 'SELECT ORDER# => '
  ENDIF  

  *-C101560 WAB -end

  @ 23,40 GET LPO PICTURE '@!'
  @ 23,48 GET HPO PICTURE '@!'
  READ
  @ 23,00

  IF LEN(TRIM(LPO)) = 0
    GOTO SAVREC
    LOOP
  ENDIF

  *-C101560 WAB - if purchase order select  potemp
  *-C101560 WAB - if sales order select   lcOrdTmp
  *-C101560 WAB -start

  *SELECT &POTEMP

  SELECT IIF(lcMfPoType='P',POTEMP,lcOrdTmp)

  *-C101560 WAB -end

  SEEK TRIM(LPO)
  IF EOF()
    ? CHR(7)
    GOTO SAVREC
    LOOP
  ENDIF
  SAVREC = RECNO()
  HPO = IIF(LEN(TRIM(HPO))<>0.AND.HPO>LPO,HPO,LPO)

  *----------------------------------
  * FLAG ALL PRINTABLE PO'S
  *----------------------------------
  DO WHILE .T.

    *-C101560 WAB - if PO. po >hpo  & if SO ORDER >hpo 
    *-C101560 WAB -start

    * IF EOF() .OR. PO)>HPO

    IF EOF() .OR. IIF(lcMfPoType='P',PO,ORDER)>HPO

      *-C101560 WAB -end

      EXIT
    ENDIF
    IF FLAG='Y'
      REPLACE FLAG WITH 'N'
    ELSE
      REPLACE FLAG WITH 'Y'
    ENDIF
    SKIP
  ENDDO
  
*-C101560 WAB - no need For reselect the tabel again
*-C101560 WAB -START

* SELECT &POTEMP

*-C101560 WAB - END

  GOTO SAVREC

ENDDO
RETURN

*!*************************************************************
*! Name      : CARE400
*! Developer : Khalid Mohi El-Din Mohamed - (KHM)
*! Date      : 12/31/98
*! Purpose   : Print care labels.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  DO CARE400
*!*************************************************************
PROCEDURE CARE400
*C101816,1 ABD -Add new private variables. [Begin]
Private lcOrderKey
STORE '' To lcOrderKey 
*C101816,1 ABD -[End]

DIMENSION XTEMP(25,10)        && temporary storage of values before print
                              && for cuttkt, style, and size AND COLOR

PRIVATE ROW,COL,XTEMP,XSECOND
XPO       = SPACE(6)
ROW       = 1
COL       = 0
XSECOND   = .F.
XLAST     = 0  
XFLAGLAST =.F.



*C101816,1 ABD -Fix Bug in sort by in case of print form SO, make new index on the
*C101816,1 ABD -Ordline file On cOrdtype+Order+Style+STR(lineno,6) TAG on the
*C101816,1 ABD -temp file (lcOrdTmp1) [Begin]

*B603762,1 ABD Select ordline only when run report from so module. [Begin]
IF lcMfPoType = "S" 
  SELECT ORDLINE
  lcOrderKey = ORDER()
ENDIF
*B603762,1 ABD [End]

IF lcMfPoType = 'S'  .AND. XSORT <> "3"
  INDEX ON cOrdtype+Order+Style+STR(lineno,6) TAG (lcOrdTmp1) OF ;
                                 (gcWorkDir+lcOrdTmp1+'.CDX')
ENDIF 
*C101816,1 ABD -[End]

*-C101560 WAB - if purchase order select   potemp
*-C101560 WAB - if sales  order   select   lcOrdTmp
*-C101560 WAB -start

*SELECT &POTEMP

SELECT IIF(lcMfPoType='P',POTEMP,lcOrdTmp)

*-C101560 WAB -end

XA=0   && labels across counter


*IF XSORT='C'    && SORTED BY STYLE/COLOR 
*C101816,1 ABD - Change the If statement to be case in up on add new sort if you 
*C101816,1 ABD - come from sales order module only. [Begin]
*IF XSORT = '2'    && SORTED BY STYLE/COLOR 
Do Case
  Case XSORT = '1'
  *C101816,1 ABD - [End]
    SCAN WHILE (INKEY() <> 32)
      *-C101560 WAB To indicate whether U R using the program from PO OR SO.
      *-C101560 WAB - Start

      *WAIT WINDOW "PO # : " + PO NOWAIT
      *XPO = PO
      *SELECT POSLN
      *SEEK 'P'+XPO

      *-C101560 WAB - Wait window with Purchaise order no .or. with Sales order no 
      WAIT WINDOW  IIF(lcMfPoType='P',"PO # : ","ORDER # : ") +IIF(lcMfPoType='P',PO,order) NOWAIT
      XPO = IIF(lcMfPoType='P',PO,order) 		 && purchaise ordr no .or. sales order no
      SELECT IIF(lcMfPoType='P',"POSLN","ORDLINE") && select Purchase order line 
    										 && or Sales order line
      SEEK IIF(lcMfPoType='P','P','O')+XPO	 

      *-C101560 WAB - END


      XSAVREC=RECNO()

      IF XSELSIZE = 'A'
        XBEG = 1
        XEND = SCALE->CNT
       ELSE
         IF VAL(XSELSIZE) > SCALE->CNT
           LOOP
         ENDIF
         XBEG = VAL(XSELSIZE)
         XEND = XBEG
       ENDIF

      FOR XCOUNT=XBEG TO XEND
        GOTO XSAVREC
        *SCAN WHILE XPO=PO FOR TRANCD='1'   && SCANS THE CUTTKT FILE

        *-C101560 WAB To indicate whether U R using the program from PO OR SO.
        *-C101560 WAB - Start

        *   SCAN WHILE cStyType+Po+Style+STR(LineNo,6)+Trancd+STR(RECNO(),7)="P"+XPO;
        *      FOR TRANCD = '1'
        * SELECT POSLN


        *C101816,1 ABD - Change the way of scan to fix bug in the sort by [Begin]
        *SCAN WHILE IIF(lcMfPoType='P',cStyType+Po+Style+STR(LineNo,6)+Trancd+STR(RECNO(),7),;
        *    cORdType+Order+STR(LineNo,6)+STR(RECNO(),7))=IIF(lcMfPoType='P',"P","O")+XPO;
        *    FOR IIF(lcMfPoType='P',TRANCD,cOrdType) = IIF(lcMfPoType='P','1','O')

        SCAN WHILE IIF(lcMfPoType='P',cStyType+Po+Style+STR(LineNo,6)+Trancd+STR(RECNO(),7),;
            cOrdtype+Order+Style+STR(lineno,6))=IIF(lcMfPoType='P',"P","O")+XPO;
            FOR IIF(lcMfPoType='P',TRANCD,cOrdType) = IIF(lcMfPoType='P','1','O')
        *C101816,1 ABD - [End]
          SELECT IIF(lcMfPoType='P',"POSLN","ORDLINE") && select Purchase order line 
        									   && or Sales order line
        *-C101560 WAB - END
  
          XSCALE_CNT = SCALE->CNT && HOW MANY QUANTITIES SHOULD WE GET?
      
          *-C101560 WAB To indicate whether U R using the program from PO OR SO.
          *-C101560 WAB - Start

          *XPO=PO                  && GET THE CUTTING TICKET AND STYLE FOR PRINTOUT

          XPO= IIF(lcMfPoType='P',PO,ORDER)                  

          *-C101560 WAB - END

          XSTYLE  = SUBSTR(STYLE,1,lnMajLen)
          XSDESC  = STYLE->DESC
          XCLRCOD = SUBSTR(Style,lnClrPo,lnColorLen)
          *B603713,10 HBG 11/22/2000 Increase The Width Of The pricea [Begin]
          *XPRICEA = STR(STYLE->PRICEA,7,2)    
          XPRICEA = STR(STYLE->PRICEA,12,2)    
          *B603713,10 [End]
          XFABRIC = FABRIC->DESC              
          XFABCON = FABRIC->CONTENT           

          SELECT CODES
          XColor = gfCodDes(XCLRCOD,PADR('COLOR',10))
          xColor = IIF(EMPTY(xColor),XCLRCOD,xColor)
          XTHIS  = LEN(XCOLOR)  
  
          *-C101560 WAB - if purchase order select   POSLN
          *-C101560 WAB - if sales  order   select   ORDLINE
          *-C101560 WAB -start

          *SELECT POSLN

          SELECT IIF(lcMfPoType='P',"POSLN","ORDLINE")

          *-C101560 WAB -end

          Y = STR(XCOUNT,1)
          XQTY  = QTY&Y   && INCREMENT THE QUANTITIES
          XSZ = SCALE->SZ&Y       && GET THE SIZES
          IF XQTY<=0
            XQTY=0
          ELSE
            XQTY = INT(ROUND(((XPERC/100) * XQTY),0))
            XQTY = IIF(XQTY = 0, 1, XQTY)   && prints at least 1
          ENDIF

          DO CARE410  && PRINT OUT THE CARE LABELS
        ENDSCAN
      ENDFOR
      *-C101560 WAB - if purchase order select   POTEMP
      *-C101560 WAB - if sales  order   select   lcOrdTmp
      *-C101560 WAB -start

      *SELE &POTEMP   && GO BACK TO THE WORKFILE TO GET THE NEXT CUTTING TICKET

      SELECT IIF(lcMfPoType='P',POTEMP,lcOrdTmp)

      *-C101560 WAB -end

    ENDSCAN
    WAIT CLEAR 
    *C101816,1 ABD - Change the If statement to be case in up on add new sort if you 
    *C101816,1 ABD - come from sales order module only. [Begin]
    *IF XSORT='C'    && SORTED BY STYLE/COLOR     
  Case XSORT = '2'    && SORTED BY STYLE/COLOR 
    *C101816,1 ABD - [End]
    SCAN WHILE (INKEY() <> 32)
      *-C101560 WAB To indicate whether U R using the program from PO OR SO.
      *-C101560 WAB - Start

      *WAIT WINDOW "PO # : " + PO NOWAIT
      *XPO = PO
      *SELECT POSLN
      *SEEK 'P'+XPO
      *-C101560 WAB - Wait window with Purchaise order no .or. with Sales order no 

      WAIT WINDOW  IIF(lcMfPoType='P',"PO # : ","ORDER # : ") +IIF(lcMfPoType='P',PO,order) NOWAIT
      XPO = IIF(lcMfPoType='P',PO,order) 		 && purchaise ordr no .or. sales order no
      SELECT IIF(lcMfPoType='P',"POSLN","ORDLINE") && select Purchase order line 
    										 && or Sales order line
      SEEK IIF(lcMfPoType='P','P','O')+XPO	 

      *-C101560 WAB - END

      *SCAN WHILE XPO=PO FOR TRANCD='1'   && SCANS THE CUTTKT FILE

      *-C101560 WAB To indicate whether U R using the program from PO OR SO.
      *-C101560 WAB - Start

      *   SCAN WHILE cStyType+Po+Style+STR(LineNo,6)+Trancd+STR(RECNO(),7)="P"+XPO;
      *      FOR TRANCD = '1'
      * SELECT POSLN

      *C101816,1 ABD - Change the way of scan to fix bug in the sort by [Begin]
      *SCAN WHILE IIF(lcMfPoType='P',cStyType+Po+Style+STR(LineNo,6)+Trancd+STR(RECNO(),7),;
      *    cORdType+Order+STR(LineNo,6)+STR(RECNO(),7))=IIF(lcMfPoType='P',"P","O")+XPO;
      *    FOR IIF(lcMfPoType='P',TRANCD,cOrdType) = IIF(lcMfPoType='P','1','O')
      
      SCAN WHILE IIF(lcMfPoType='P',cStyType+Po+Style+STR(LineNo,6)+Trancd+STR(RECNO(),7),;
          cOrdtype+Order+Style+STR(lineno,6))=IIF(lcMfPoType='P',"P","O")+XPO;
          FOR IIF(lcMfPoType='P',TRANCD,cOrdType) = IIF(lcMfPoType='P','1','O')
          
      *C101816,1 ABD - [End]

        SELECT IIF(lcMfPoType='P',"POSLN","ORDLINE") && select Purchase order line 
          									   && or Sales order line
        *-C101560 WAB - END

        XSCALE_CNT = SCALE->CNT && HOW MANY QUANTITIES SHOULD WE GET?
 
        *-C101560 WAB To indicate whether U R using the program from PO OR SO.
        *-C101560 WAB - Start

        *XPO=PO                  && GET THE CUTTING TICKET AND STYLE FOR PRINTOUT

        XPO= IIF(lcMfPoType='P',PO,ORDER)                  

        *-C101560 WAB - END
            
        XSTYLE  = SUBSTR(STYLE,1,lnMajLen)
        XSDESC  = STYLE->DESC           
        XCLRCOD = SUBSTR(Style,lnClrPo,lnColorLen)
        *B603713,10 HBG 11/22/2000 Increase The Width Of The pricea [Begin]
        *XPRICEA = STR(STYLE->PRICEA,7,2)
        XPRICEA = STR(STYLE->PRICEA,12,2)    
        *B603713,10 [End]
        XFABRIC = FABRIC->DESC          
        XFABCON = FABRIC->CONTENT       
        SELECT Codes
        XColor = gfCodDes(XCLRCOD,PADR('COLOR',10))
        xColor = IIF(EMPTY(xColor),XCLRCOD,xColor)
        XTHIS  = LEN(XCOLOR)  
        IF XSELSIZE = 'A'
          XBEG = 1
          XEND = SCALE->CNT
         ELSE
           IF VAL(XSELSIZE) > SCALE->CNT
             LOOP
           ENDIF
           XBEG = VAL(XSELSIZE)
           XEND = XBEG
        ENDIF
  
        *-C101560 WAB - if purchase order select   POSLN
        *-C101560 WAB - if sales  order   select   ORDLINE
        *-C101560 WAB -start

        *SELECT POSLN

        SELECT IIF(lcMfPoType='P',"POSLN","ORDLINE")
 
        *-C101560 WAB -end

      
        FOR X=XBEG TO XEND
          Y = STR(X,1)
          XQTY  = QTY&Y   && INCREMENT THE QUANTITIES
          XSZ = SCALE->SZ&Y       && GET THE SIZES
          IF XQTY <= 0
            XQTY=0
          ELSE
            XQTY = INT(ROUND(((XPERC/100) * XQTY),0))
            XQTY = IIF(XQTY = 0, 1, XQTY)   && prints at least 1
          ENDIF
  
          DO CARE410  && PRINT OUT THE CARE LABELS
        ENDFOR
      ENDSCAN

      *-C101560 WAB - if purchase order select   potemp
      *-C101560 WAB - if sales  order   select   lcOrdTmp
      *-C101560 WAB -start

      *SELE &POTEMP   && GO BACK TO THE WORKFILE TO GET THE NEXT CUTTING TICKET

      SELECT IIF(lcMfPoType='P',POTEMP,lcOrdTmp)

      *-C101560 WAB -end
    
    ENDSCAN
    WAIT CLEAR
    *C101816,1 ABD - Change the If statement to be case in up on add new sort if you 
    *C101816,1 ABD - come from sales order module only. [Begin]
    *ENDIF
  Case XSORT = '3'
    *C101816,1 ABD - Add recoreds in the temp file sorted by style.
    SCAN WHILE (INKEY() <> 32)
      XPO = Order
      SELECT ORDLINE
      =SEEK('O'+XPO)
      SCAN  WHIlE cOrdtype+Order+Style+STR(lineno,6)= "O"+XPO 
        SELECT ORDLINE
        SCATTER MEMVAR MEMO
        INSERT INTO &lcOrdTmp2 From MEMVAR
      ENDSCAN
    ENDSCAN
    SELECT ORDLINE
    lcOrdRlton = SET("RELATION")
    Set RELATION TO 

    SELECT (lcOrdTmp2)
    SET RELATION TO STYLE INTO STYLE

    SELECT (lcOrdTmp2)
    GOTO TOP
    SCAN  
      WAIT WINDOW "Style:.. " + Style NOWAIT
      XSCALE_CNT = SCALE.CNT && HOW MANY QUANTITIES SHOULD WE GET?
      XPO= ORDER
      XSTYLE  = SUBSTR(STYLE,1,lnMajLen)
      XSDESC  = STYLE.DESC           
      XCLRCOD = SUBSTR(Style,lnClrPo,lnColorLen)
      *B603713,10 HBG 11/22/2000 Increase The Width Of The pricea [Begin]
      *XPRICEA = STR(STYLE.PRICEA,7,2)
      XPRICEA = STR(STYLE->PRICEA,12,2)    
      *B603713,10 [End]
      XFABRIC = FABRIC.DESC          
      XFABCON = FABRIC.CONTENT       
      SELECT Codes
      XColor = gfCodDes(XCLRCOD,PADR('COLOR',10))
      xColor = IIF(EMPTY(xColor),XCLRCOD,xColor)
      XTHIS  = LEN(XCOLOR)  
      IF XSELSIZE = 'A'
        XBEG = 1
        XEND = SCALE.CNT
      ELSE
        IF VAL(XSELSIZE) > SCALE.CNT
             LOOP
        ENDIF
        XBEG = VAL(XSELSIZE)
           XEND = XBEG
        ENDIF
        SELECT (lcOrdTmp2)
        FOR X=XBEG TO XEND
          Y = STR(X,1)
          XQTY  = QTY&Y   && INCREMENT THE QUANTITIES
          XSZ = SCALE.SZ&Y       && GET THE SIZES
          IF XQTY <= 0
            XQTY=0
          ELSE
            XQTY = INT(ROUND(((XPERC/100) * XQTY),0))
            XQTY = IIF(XQTY = 0, 1, XQTY)   && prints at least 1
          ENDIF
          IF  XQTY # 0
            DO CARE410  && PRINT OUT THE CARE LABELS
          ENDIF
        ENDFOR
    ENDSCAN
    WAIT CLEAR 
    *C101816,1 ABD - Reindex the order line to become as the First Index.[begin]
    SELECT (lcOrdTmp2)
    SET RELATION TO
    GOTO TOP
    DELETE ALL WHILE !EOF()
    SELECT ORDLINE
    SET ORDER TO &lcOrderKey IN ORDLINE
    SET RELATION TO &lcOrdRlton
ENDCASE
*C101816,1 ABD - [End]
*-----------------------------------
* PRINT OUT THE LAST LINE OF LABELS, WHICH MAY OR MAY NOT GO ACROSS
* THE ENTIRE PAGE BECAUSE IT ONLY PRINTS AFTER IT HAS ENOUGH LABELS
* TO GO ACROSS THE ENTIRE PAGE SO SOMETIMES IT IS POSSIBLE THAT THERE
* ARE SOME LABELS LEFT SO THEREFORE WE CALL THE PRINT PROGRAM AGAIN
* TO PRINT THE REMAINING ONES...
XFLAGLAST=.T.
XQTY=0

DO CARE410      && TO PRINT LAST LINE

RETURN

*!*************************************************************
*! Name      : CARE410
*! Developer : Khalid Mohi El-Din Mohamed - (KHM)
*! Date      : 12/31/98
*! Purpose   : Print out the care labels.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  DO CARE410
*!*************************************************************
PROCEDURE CARE410

FOR Y=1 TO XQTY 
  XA=XA+1                       && labels across
  XTEMP(XA,1)=XPO               && TEMP VARS STORING CUTTING TICKET AND STYLE
  XTEMP(XA,2)=XSTYLE
  XTEMP(XA,3)=XSZ   
  XTEMP(XA,4)=XCOLOR 
  XTEMP(XA,5)=XSDESC 
  XTEMP(XA,6)=XPRICEA
  XTEMP(XA,7)=XFABRIC
  XTEMP(XA,8)=XFABCON
  XTEMP(XA,9)=XCLRCOD
  IF XTHIS<XLAST
    XTHIS=XLAST
  ELSE
    XLAST=XTHIS
  ENDIF

  IF XA>=XNUMACROSS              && print if line of labels complete
      XL=0
      DO WHILE XL<XLLENGTH
        XL=XL+1
        XA=0
        COL=0
        DO WHILE XA<XNUMACROSS
        
          XA=XA+1
          *** put PO,style,size on matrix if exists on line
          XW=0
          IF XAUX(XL,8)>0    && FABRIC CONTENTS width is 60 MAX BUT CAN BE LESS
            DO WHILE XW < 60
              XMATRIX(XL,XAUX(XL,8)+XW)=SUBSTR(XTEMP(XA,8),XW+1,1)
              XW=XW+1
            ENDDO
          ENDIF
          XW=0
          IF XAUX(XL,1)>0    && PO width is 6
            DO WHILE XW<6
              XMATRIX(XL,XAUX(XL,1)+XW)=SUBSTR(XTEMP(XA,1),XW+1,1)
              XW=XW+1
            ENDDO
          ENDIF
          XW=0
          IF XAUX(XL,2)>0    && style width is 12
            *DO WHILE XW<12
            DO WHILE XW < lnMajLen
              XMATRIX(XL,XAUX(XL,2)+XW)=SUBSTR(XTEMP(XA,2),XW+1,1)
              XW=XW+1
            ENDDO
          ENDIF
          XW=0
          IF XAUX(XL,3)>0    && size width is 5
            DO WHILE XW<5
              XMATRIX(XL,XAUX(XL,3)+XW)=SUBSTR(XTEMP(XA,3),XW+1,1)
              XW=XW+1
            ENDDO
          ENDIF
          XW=0
          IF XAUX(XL,4)>0    && COLOR width is 15 MAX BUT CAN BE LESS
            DO WHILE XW <  XTHIS  
              XMATRIX(XL,XAUX(XL,4)+XW)=SUBSTR(XTEMP(XA,4),XW+1,1)
              XW=XW+1
            ENDDO
          ENDIF
          *********************************
          XW=0
          IF XAUX(XL,5)>0    && STYLEDESC width is 20 MAX BUT CAN BE LESS
            DO WHILE XW <  20 
              XMATRIX(XL,XAUX(XL,5)+XW)=SUBSTR(XTEMP(XA,5),XW+1,1)
              XW=XW+1
            ENDDO
          ENDIF
          XW=0
          IF XAUX(XL,6)>0    && STYLE PRICE A width is 7
            *B603713,10 HBG 11/22/2000 Increase The Width Of The pricea [Begin]
            *DO WHILE XW <  7
            DO WHILE XW <  12     && STYLE PRICE A width is 12
            *B603713,10 [End]
              XMATRIX(XL,XAUX(XL,6)+XW)=SUBSTR(XTEMP(XA,6),XW+1,1)
              XW=XW+1
            ENDDO
          ENDIF
          XW=0
          IF XAUX(XL,7)>0    && FABRIC DESC width is 20 MAX BUT CAN BE LESS
            DO WHILE XW <  20
              XMATRIX(XL,XAUX(XL,7)+XW)=SUBSTR(XTEMP(XA,7),XW+1,1)
              XW=XW+1
            ENDDO
          ENDIF
          XW=0
          IF XAUX(XL,9)>0    && COLOR CODE width is 6
            DO WHILE XW<6
              XMATRIX(XL,XAUX(XL,9)+XW)=SUBSTR(XTEMP(XA,9),XW+1,1)
              XW=XW+1
            ENDDO
          ENDIF

           *** print one line of one label

           *-C101560 WAB - eject for new page
           *-C101560 WAB - Start
           *IF ROW = 63
           * Eject
           * ROW = 3
           *ENDIF
           *-C101560 WAB - End
           XW=0
           DO WHILE XW<XLWIDTH
             XW=XW+1
             @ ROW,COL SAY XMATRIX(XL,XW)
             COL=COL+1
           ENDDO
        ENDDO
        ROW=ROW+1

        *-C101560 WAB - eject for new page
        *-C101560 WAB - Start
        IF ROW + XLLENGTH > 66
          ROW = 1
        ENDIF
        *-C101560 WAB - End

      ENDDO
      XA=0
  ENDIF  && PRINT IF LINE OF LABELS IS COMPLETE
ENDFOR   && FOR EACH  QUANTITY

*-- TWO VARS, XNUMACROSS, XTNUMACROS.
*-- XTNUMACROS USED IN THIS IF..ENDIF TO MAKE SURE THAT XNUMACROSS REMAINS
*-- UNCHANGED. 

IF XA>0  .AND. XFLAGLAST          && print last labels
   XTNUMACROS=XA
   XL=0
   DO WHILE XL<XLLENGTH
      XL=XL+1
      XA=0
      COL=0
      DO WHILE XA<XTNUMACROS
         XA=XA+1
         *** put PO,style,size on matrix if exists on line
          XW=0
          IF XAUX(XL,8)>0    && FABRIC CONTENTS width is 60
            DO WHILE XW < (LEN(XTEMP(XA,8)))
              XMATRIX(XL,XAUX(XL,8)+XW)=SUBSTR(XTEMP(XA,8),XW+1,1)
              XW=XW+1
            ENDDO
          ENDIF
         XW=0
         IF XAUX(XL,1)>0    && cuttkt width is 6
            DO WHILE XW<6
               XMATRIX(XL,XAUX(XL,1)+XW)=SUBSTR(XTEMP(XA,1),XW+1,1)
               XW=XW+1
            ENDDO
         ENDIF
         XW=0
         IF XAUX(XL,2)>0    && style width is 12
            DO WHILE XW<12
               XMATRIX(XL,XAUX(XL,2)+XW)=SUBSTR(XTEMP(XA,2),XW+1,1)
               XW=XW+1
            ENDDO
         ENDIF
         XW=0
         IF XAUX(XL,3)>0    && size width is 5
            DO WHILE XW<5
               XMATRIX(XL,XAUX(XL,3)+XW)=SUBSTR(XTEMP(XA,3),XW+1,1)
               XW=XW+1
            ENDDO
         ENDIF
         XW=0
         IF XAUX(XL,4)>0    && COLOR width is 15 MAX BUT CAN BE LESS && TAK FIXED 06/09/93
           DO WHILE XW < (LEN(XTEMP(XA,4)))
             XMATRIX(XL,XAUX(XL,4)+XW)=SUBSTR(XTEMP(XA,4),XW+1,1)
             XW=XW+1
           ENDDO
         ENDIF
         *********************************
         XW=0
          IF XAUX(XL,5)>0    && STYLE DESC. width is 20
            DO WHILE XW < (LEN(XTEMP(XA,5)))
              XMATRIX(XL,XAUX(XL,5)+XW)=SUBSTR(XTEMP(XA,5),XW+1,1)
              XW=XW+1
            ENDDO
          ENDIF
          XW=0
          IF XAUX(XL,6)>0      && STYLE PRICE A width is 7
            DO WHILE XW < (LEN(XTEMP(XA,6)))       
              XMATRIX(XL,XAUX(XL,6)+XW)=SUBSTR(XTEMP(XA,6),XW+1,1)
              XW=XW+1
            ENDDO
          ENDIF
          XW=0
          IF XAUX(XL,7)>0    && FABRIC DESC. width is 20
            DO WHILE XW < (LEN(XTEMP(XA,7)))
              XMATRIX(XL,XAUX(XL,7)+XW)=SUBSTR(XTEMP(XA,7),XW+1,1)
              XW=XW+1
            ENDDO
          ENDIF
         XW=0
         IF XAUX(XL,9)>0    && COLOR CODE width is 6
           DO WHILE XW<6
             XMATRIX(XL,XAUX(XL,9)+XW)=SUBSTR(XTEMP(XA,9),XW+1,1)
             XW=XW+1
           ENDDO
         ENDIF

         *** print one line of one label
         XW=0
         DO WHILE XW<XLWIDTH
            XW=XW+1
            @ ROW,COL SAY XMATRIX(XL,XW)
            COL=COL+1
         ENDDO
      ENDDO
      ROW=ROW+1
   ENDDO
  
   XA=0
ENDIF  && IF XA> 0
RETURN

*!*************************************************************
*! Name      : HANG160A
*! Developer : Khalid Mohi El-Din Mohamed - (KHM)
*! Date      : 12/31/98
*! Purpose   : Hang tags for Form Code A.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  DO HANG160A
*!*************************************************************
PROCEDURE HANG160A

XPO  = SPACE(6)
ROW=1

*-C101560 WAB - if purchase order select   POTEMP
*-C101560 WAB - if sales  order   select   lcOrdTmp
*-C101560 WAB -start

*SELE &POTEMP   && GO BACK TO THE WORKFILE TO GET THE NEXT CUTTING TICKET

SELECT IIF(lcMfPoType='P',POTEMP,lcOrdTmp)

*-C101560 WAB -end

GOTO TOP
SCAN WHILE .NOT. EOF() .AND. (INKEY() <> 32)

  *-C101560 WAB Ghange next lines for working whether from PO or SO module.
  *-C101560 WAB - Start

  *WAIT WINDOW "PO # : " + PO NOWAIT
  *XPO = PO
  *SELECT POSLN
  *SEEK 'P'+XPO

  *-C101560 WAB - Wait window with Purchaise order no .or. with Sales order no 
  WAIT WINDOW  IIF(lcMfPoType='P',"PO # : ","ORDER # : ") +IIF(lcMfPoType='P',PO,order) NOWAIT
  XPO = IIF(lcMfPoType='P',PO,order) 		 && purchaise ordr no .or. sales order no
  SELECT IIF(lcMfPoType='P',"POSLN","ORDLINE") && select table Purchase order line 
    									   && or table Sales order line
  SEEK IIF(lcMfPoType='P','P','O')+XPO	   

  *-C101560 WAB - END

  *SCAN WHILE XPO=PO FOR TRANCD='1'  

  *-C101560 WAB Ghange next lines for working whether from PO or SO module.
  *-C101560 WAB - Start

  *   SCAN WHILE cStyType+Po+Style+STR(LineNo,6)+Trancd+STR(RECNO(),7)="P"+XPO;
  *      FOR TRANCD = '1'
  * SELECT POSLN

  SCAN WHILE IIF(lcMfPoType='P',cStyType+Po+Style+STR(LineNo,6)+Trancd+STR(RECNO(),7),;
        cORdType+Order+STR(LineNo,6)+STR(RECNO(),7))=IIF(lcMfPoType='P',"P","O")+XPO;
        FOR IIF(lcMfPoType='P',TRANCD,cOrdType) = IIF(lcMfPoType='P','1','O')

    SELECT IIF(lcMfPoType='P',"POSLN","ORDLINE") && select Purchase order line 
         									   && or Sales order line
  *-C101560 WAB - END

    IF XSELSIZE = 'A'
      XBEG = 1
      XEND = SCALE->CNT
    ELSE
      IF VAL(XSELSIZE) > SCALE->CNT
        LOOP
      ENDIF 
      XBEG = VAL(XSELSIZE)
      XEND = XBEG
    ENDIF
    FOR X=XBEG TO XEND
      Z=STR(X,1)
      IF QTY&Z<=0
        XQTY=0
      ELSE
        XQTY = INT(ROUND(((XPERC/100) * QTY&Z),0))
        XQTY = IIF(XQTY = 0, 1, XQTY)   && prints at least 1
      ENDIF
      FOR Y=1 TO XQTY
          
        *IF gnStyleWid = 7
        *  @ ROW,01 SAY 'STYLE: '+SUBSTR(STYLE,1,7)+'   P/O#: '+PO
        *ELSE
        *  @ ROW,01 SAY 'STYLE: '+STYLE+'   P/O#: '+PO
        *ENDIF
      
        *-C101560 WAB disp style + po# or disp style + ORDER #
        *-C101560 WAB - Start

        *@ ROW,01 SAY 'STYLE: '+ALLTRIM(SUBSTR(STYLE,1,lnMajLen))+'   P/O#: '+PO
      
        IF lcMfPoType = 'P'		&& Purchase Order
          @ ROW,01 SAY 'STYLE: '+ALLTRIM(SUBSTR(STYLE,1,lnMajLen))+'   P/O#: '+PO
        ELSE
          @ ROW,01 SAY 'STYLE: '+ALLTRIM(SUBSTR(STYLE,1,lnMajLen))+'ORDER #: '+ORDER
        ENDIF
        
        *-C101560 WAB - Start
                  
        ROW=ROW+1
        @ ROW,06 SAY STYLE->DESC
        ROW=ROW+1
        @ ROW,06 SAY FABRIC->DESC
        ROW=ROW+1
          
        *IF gnColorWid = 3
        *  @ ROW,01 SAY 'CLR: '+SUBSTR(COLOR,1,3)+' '+ALLTRIM(CODE->CDATA)
        *ELSE
        *  @ ROW,01 SAY 'CLR: '+COLOR+' '+ALLTRIM(CODE->CDATA)
        *ENDIF  
        *-- If the structure contains a color segment.
        IF lnColorLen > 0
          lcClrCode = SUBSTR(Style,lnClrPo,lnColorLen)
          @ ROW,01 SAY 'CLR: '+lcClrCode+;
                           ' '+ALLTRIM(gfCodDes(lcClrCode,PADR('COLOR',10)))
          ROW=ROW+1
        ENDIF
          
        @ ROW,01 SAY 'SIZE: '+SCALE->SZ&Z
        ROW=ROW+2
      ENDFOR
    ENDFOR
  ENDSCAN

  *-C101560 WAB - if purchase order select   POTEMP
  *-C101560 WAB - if sales  order   select   lcOrdTmp
  *-C101560 WAB -start

  *SELE &POTEMP   && 

  SELECT IIF(lcMfPoType='P',POTEMP,lcOrdTmp)

  *-C101560 WAB -end

ENDSCAN
WAIT CLEAR
RETURN

*!*************************************************************
*! Name      : HANG160B
*! Developer : Khalid Mohi El-Din Mohamed - (KHM)
*! Date      : 12/31/98
*! Purpose   : Hang tags for Form Code B.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  DO HANG160B
*!*************************************************************
PROCEDURE HANG160B

XPO  = SPACE(6)
ROW=1

*-C101560 WAB - if purchase order select   POTEMP
*-C101560 WAB - if sales  order   select   lcOrdTmp
*-C101560 WAB -start

*SELE &POTEMP   && GO BACK TO THE WORKFILE TO GET THE NEXT CUTTING TICKET

SELECT IIF(lcMfPoType='P',POTEMP,lcOrdTmp)

*-C101560 WAB -end

GOTO TOP
SCAN WHILE .NOT. EOF() .AND. (INKEY() <> 32)

  *-C101560 WAB Ghange next lines for working whether from PO or SO module.
  *-C101560 WAB - Start

  *WAIT WINDOW "PO # : " + PO NOWAIT
  *XPO = PO
  *SELECT POSLN
  *SEEK 'P'+XPO

  *-C101560 WAB - Wait window with Purchaise order no .or. with Sales order no 
  WAIT WINDOW  IIF(lcMfPoType='P',"PO # : ","ORDER # : ") +IIF(lcMfPoType='P',PO,order) NOWAIT
  XPO = IIF(lcMfPoType='P',PO,order) 		 && purchaise ordr no .or. sales order no
  SELECT IIF(lcMfPoType='P',"POSLN","ORDLINE") && select table Purchase order line 
    									   && or table Sales order line
  SEEK IIF(lcMfPoType='P','P','O')+XPO	   

  *-C101560 WAB - END

  *SCAN WHILE XPO=PO FOR TRANCD='1'

  *-C101560 WAB Ghange next line for working whether from PO or SO module.
  *-C101560 WAB - Start

  *   SCAN WHILE cStyType+Po+Style+STR(LineNo,6)+Trancd+STR(RECNO(),7)="P"+XPO;
  *      FOR TRANCD = '1'

  SCAN WHILE IIF(lcMfPoType='P',cStyType+Po+Style+STR(LineNo,6)+Trancd+STR(RECNO(),7),;
        cORdType+Order+STR(LineNo,6)+STR(RECNO(),7))=IIF(lcMfPoType='P',"P","O")+XPO;
        FOR IIF(lcMfPoType='P',TRANCD,cOrdType) = IIF(lcMfPoType='P','1','O')

  *-C101560 WAB - END

     IF XSELSIZE = 'A'
        XBEG = 1
        XEND = SCALE->CNT
     ELSE
        IF VAL(XSELSIZE) > SCALE->CNT
          LOOP
        ENDIF 
        XBEG = VAL(XSELSIZE)
        XEND = XBEG
     ENDIF
     FOR X=XBEG TO XEND
        Z=STR(X,1)
        IF QTY&Z<=0
           XQTY=0
        ELSE
           XQTY = INT(ROUND(((XPERC/100) * QTY&Z),0))
           XQTY = IIF(XQTY = 0, 1, XQTY)   && prints at least 1
        ENDIF
        FOR Y=1 TO XQTY
          *IF gnStyleWid = 7
          *  @ ROW,01 SAY 'STYLE: '+SUBSTR(STYLE,1,7)+'   PO#: '+PO
          *ELSE
          *  @ ROW,01 SAY 'STYLE: '+STYLE+'   PO#: '+PO
          *ENDIF

          *-C101560 WAB disp style + po# or disp style + ORDER #
          *-C101560 WAB - Start
  
          *@ ROW,01 SAY 'STYLE: '+ALLTRIM(SUBSTR(STYLE,1,lnMajLen))+'   P/O#: '+PO

          IF lcMfPoType = 'P'		&& Purchase Order
            @ ROW,01 SAY 'STYLE: '+ALLTRIM(SUBSTR(STYLE,1,lnMajLen))+'   P/O#: '+PO
          ELSE
            @ ROW,01 SAY 'STYLE: '+ALLTRIM(SUBSTR(STYLE,1,lnMajLen))+'ORDER #: '+ORDER
          ENDIF
        
          *-C101560 WAB - Start

          ROW=ROW+1
          @ ROW,06 SAY STYLE->DESC
          ROW=ROW+1
          @ ROW,06 SAY FABRIC->DESC
          ROW=ROW+1
          
          *IF gnColorWid = 3
          *  @ ROW,01 SAY 'CLR: '+SUBSTR(COLOR,1,3)+' '+ALLTRIM(CODE->CDATA)
          *ELSE
          *  @ ROW,01 SAY 'CLR: '+COLOR+' '+ALLTRIM(CODE->CDATA)
          *ENDIF  
          IF lnColorLen > 0
            lcClrCode = SUBSTR(Style,lnClrPo,lnColorLen)
            @ ROW,01 SAY 'CLR: '+lcClrCode+;
                           ' '+ALLTRIM(gfCodDes(lcClrCode,PADR('COLOR',10)))
            ROW=ROW+1
          ENDIF
          DO CASE
               CASE ALLTRIM(SCALE->SZ&Z) == '34' .OR. ALLTRIM(SCALE->SZ&Z) == '2'
                 @ ROW,01 SAY 'FRENCH SIZE: 34   USA SIZE: 2'
               CASE ALLTRIM(SCALE->SZ&Z) == '36' .OR. ALLTRIM(SCALE->SZ&Z) == '4'
                 @ ROW,01 SAY 'FRENCH SIZE: 36   USA SIZE: 4'
               CASE ALLTRIM(SCALE->SZ&Z) == '38' .OR. ALLTRIM(SCALE->SZ&Z) == '6'
                 @ ROW,01 SAY 'FRENCH SIZE: 38   USA SIZE: 6'
               CASE ALLTRIM(SCALE->SZ&Z) == '40' .OR. ALLTRIM(SCALE->SZ&Z) == '8'
                 @ ROW,01 SAY 'FRENCH SIZE: 40   USA SIZE: 8'
               CASE ALLTRIM(SCALE->SZ&Z) == '42' .OR. ALLTRIM(SCALE->SZ&Z) == '10'
                 @ ROW,01 SAY 'FRENCH SIZE: 42   USA SIZE: 10'
               CASE ALLTRIM(SCALE->SZ&Z) == '44' .OR. ALLTRIM(SCALE->SZ&Z) == '12'
                 @ ROW,01 SAY 'FRENCH SIZE: 44   USA SIZE: 12'
               OTHERWISE
                  @ ROW,01 SAY 'SIZE: '+SCALE->SZ&Z
          ENDCASE
          ROW=ROW+2
        ENDFOR
     ENDFOR
  ENDSCAN

  *-C101560 WAB - if purchase order select   POTEMP
  *-C101560 WAB - if sales  order   select   lcOrdTmp
  *-C101560 WAB -start

  *SELE &POTEMP   && 

  SELECT IIF(lcMfPoType='P',POTEMP,lcOrdTmp)

  *-C101560 WAB -end

ENDSCAN
WAIT CLEAR
RETURN

*!*************************************************************
*! Name      : CARE300
*! Developer : Khalid Mohi El-Din Mohamed - (KHM)
*! Date      : 12/31/98
*! Purpose   : Print care labels.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  DO CARE300
*!*************************************************************
PROCEDURE CARE300

DIMENSION XTEMP(25,10)
PRIVATE ROW,COL,XTEMP,XSECOND
XCUTTKT   = SPACE(6)
ROW       = 1
COL       = 0
XSECOND   = .F.
XLAST     = 0
XFLAGLAST = .F.
SELE &CTKTEMP

XA=0   && labels across counter

*IF XSORT = 'C'
IF XSORT = '2'
  SCAN WHILE (INKEY() <> 32)
    WAIT WINDOW "Cut Ticket #: " + CutTkt NOWAIT
    XCUTTKT = CUTTKT

    SELE CUTTKTL
    SEEK XCUTTKT
    SCAN WHILE CutTkt+Style+Dyelot+TranCd+STR(RECNO(),7) = XCUTTKT ;
         FOR TRANCD = '1'
      SELE CUTTKTL
      XSCALE_CNT = SCALE->CNT && HOW MANY QUANTITIES SHOULD WE GET?
      XCUTTKT    = CUTTKT     && GET THE CUTTING TICKET AND STYLE FOR PRINTOUT
      XSTYLE     = SUBSTR(STYLE,1,lnMajLen)
      XDYELOT    = DYELOT
      XSDESC     = STYLE->DESC
      XCLRCOD    = SUBSTR(Style,lnClrPo,lnColorLen)
      *B603713,10 HBG 11/22/2000 Increase The Width Of The pricea [Begin]
      *XPRICEA = STR(STYLE->PRICEA,7,2)
      XPRICEA = STR(STYLE->PRICEA,12,2)    
      *B603713,10 [End]
      XFABRIC    = FABRIC->DESC
      XFABCON    = FABRIC->CONTENT

      SELECT Codes
      XColor = gfCodDes(XCLRCOD,PADR('COLOR',10))
      xColor = IIF(EMPTY(xColor),XCLRCOD,xColor)
      XTHIS=LEN(XCOLOR)

      IF XSELSIZE = 'A'
        XBEG = 1
        XEND = SCALE->CNT
       ELSE
         IF VAL(XSELSIZE) > SCALE->CNT
           LOOP
         ENDIF
         XBEG = VAL(XSELSIZE)
         XEND = XBEG
      ENDIF

      SELE CUTTKTL
      FOR X = XBEG TO XEND
        Y    = STR(X,1)
        XQTY = QTY&Y             && INCREMENT THE QUANTITIES
        XSZ  = SCALE->SZ&Y       && GET THE SIZES

        IF XQTY <= 0
          XQTY = 0
        ELSE
          XQTY = INT(ROUND(((XPERC/100) * XQTY),0))
          XQTY = IIF(XQTY = 0, 1, XQTY)   && prints at least 1
        ENDIF
                   
        DO CARE310  && PRINT OUT THE CARE LABELS

      ENDFOR

    ENDSCAN

    SELE &CTKTEMP   && GO BACK TO THE WORKFILE TO GET THE NEXT CUTTING TICKET
  ENDSCAN
  WAIT CLEAR
ELSE

  SCAN WHILE (INKEY() <> 32)
    WAIT WINDOW "Cut Ticket #: " + CutTkt NOWAIT
    SET DEVI TO PRINT
    XCUTTKT = CUTTKT

    SELE CUTTKTL
    SEEK XCUTTKT
    XSAVREC = RECNO()

    IF XSELSIZE = 'A'
      XBEG = 1
      XEND = SCALE->CNT
     ELSE
       IF VAL(XSELSIZE) > SCALE->CNT
         LOOP
       ENDIF
       XBEG = VAL(XSELSIZE)
       XEND = XBEG
     ENDIF

    FOR XCOUNT=XBEG TO XEND
      GOTO XSAVREC
      SCAN WHILE XCUTTKT = CUTTKT FOR TRANCD = '1'   && SCANS THE CUTTKT FILE

        SELE CUTTKTL
        XSCALE_CNT = SCALE->CNT && HOW MANY QUANTITIES SHOULD WE GET?
        XCUTTKT    = CUTTKT        && GET THE CUTTING TICKET AND STYLE FOR PRINTOUT
        XSTYLE     = SUBSTR(STYLE,1,lnMajLen)
        XDYELOT    = DYELOT
        XSDESC     = STYLE->DESC
        XCLRCOD    = SUBSTR(Style,lnClrPo,lnColorLen)
        *B603713,10 HBG 11/22/2000 Increase The Width Of The pricea [Begin]
        *XPRICEA = STR(STYLE->PRICEA,7,2)
        XPRICEA = STR(STYLE->PRICEA,12,2)    
        *B603713,10 [End]
        XFABRIC    = FABRIC->DESC
        XFABCON    = FABRIC->CONTENT

        SELECT Codes
        XColor = gfCodDes(XCLRCOD,PADR('COLOR',10))
        xColor = IIF(EMPTY(xColor),XCLRCOD,xColor)        
        XTHIS=LEN(XCOLOR)

        SELE CUTTKTL
        Y    = STR(XCOUNT,1)
        XQTY = QTY&Y   && INCREMENT THE QUANTITIES
        XSZ  = SCALE->SZ&Y       && GET THE SIZES
        IF XQTY <= 0
          XQTY = 0
        ELSE
          XQTY = INT(ROUND(((XPERC/100) * XQTY),0))
          XQTY = IIF(XQTY = 0, 1, XQTY)   && prints at least 1
        ENDIF

        DO CARE310  && PRINT OUT THE CARE LABELS

      ENDSCAN

    ENDFOR

    SELE &CTKTEMP   && GO BACK TO THE WORKFILE TO GET THE NEXT CUTTING TICKET
  ENDSCAN
  WAIT CLEAR
ENDIF

*-----------------------------------
* PRINT OUT THE LAST LINE OF LABELS, WHICH MAY OR MAY NOT GO ACROSS
* THE ENTIRE PAGE BECAUSE IT ONLY PRINTS AFTER IT HAS ENOUGH LABELS
* TO GO ACROSS THE ENTIRE PAGE SO SOMETIMES IT IS POSSIBLE THAT THERE
* ARE SOME LABELS LEFT SO THEREFORE WE CALL THE PRINT PROGRAM AGAIN
* TO PRINT THE REMAINING ONES...
XFLAGLAST = .T.
XQTY      = 0
DO CARE310      && TO PRINT LAST LINE
RETURN

*!*************************************************************
*! Name      : CARE310
*! Developer : Khalid Mohi El-Din Mohamed - (KHM)
*! Date      : 12/31/98
*! Purpose   : Print out the care labels.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  DO CARE310
*!*************************************************************
PROCEDURE CARE310

FOR Y=1 TO XQTY
  XA           = XA+1              && labels across
  XTEMP(XA,1)  = XCUTTKT           && TEMP VARS STORING CUTTING TICKET AND STYLE
  XTEMP(XA,2)  = XSTYLE
  XTEMP(XA,3)  = XSZ
  XTEMP(XA,4)  = XCOLOR
  XTEMP(XA,5)  = XDYELOT
  XTEMP(XA,6)  = XSDESC 
  XTEMP(XA,7)  = XPRICEA
  XTEMP(XA,8)  = XFABRIC
  XTEMP(XA,9)  = XFABCON
  XTEMP(XA,10) = XCLRCOD
  IF XTHIS<XLAST
    XTHIS = XLAST
  ELSE
    XLAST = XTHIS
  ENDIF

  IF XA>=XNUMACROSS              && print if line of labels complete
      XL=0
      DO WHILE XL<XLLENGTH
        XL  = XL+1
        XA  = 0
        COL = 0
        DO WHILE XA<XNUMACROSS
          XA=XA+1
          *-- put cuttkt,style,size on matrix if exists on line
          XW=0
          IF XAUX(XL,9)>0    && FABRIC CONTENTS width is 60 MAX BUT CAN BE LESS
            DO WHILE XW < 60
              XMATRIX(XL,XAUX(XL,9)+XW)=SUBSTR(XTEMP(XA,9),XW+1,1)
              XW=XW+1
            ENDDO
          ENDIF
          XW=0
          IF XAUX(XL,1)>0    && cuttkt width is 6
            DO WHILE XW<6
              XMATRIX(XL,XAUX(XL,1)+XW)=SUBSTR(XTEMP(XA,1),XW+1,1)
              XW=XW+1
            ENDDO
          ENDIF
          XW=0
          IF XAUX(XL,2)>0    && style width is 12
            *DO WHILE XW<12
            DO WHILE XW < lnMajLen
              XMATRIX(XL,XAUX(XL,2)+XW)=SUBSTR(XTEMP(XA,2),XW+1,1)
              XW=XW+1
            ENDDO
          ENDIF
          XW=0
          IF XAUX(XL,3)>0    && size width is 5
            DO WHILE XW<5
              XMATRIX(XL,XAUX(XL,3)+XW)=SUBSTR(XTEMP(XA,3),XW+1,1)
              XW=XW+1
            ENDDO
          ENDIF
          XW=0
          IF XAUX(XL,4)>0    && COLOR width is 15 MAX BUT CAN BE LESS
            DO WHILE XW <  XTHIS  && MODIFY BY TAK 06/09/93
              XMATRIX(XL,XAUX(XL,4)+XW)=SUBSTR(XTEMP(XA,4),XW+1,1)
              XW=XW+1
            ENDDO
          ENDIF
          XW=0
          IF XAUX(XL,5)>0    && DYELOT width is 10
            DO WHILE XW<10
              XMATRIX(XL,XAUX(XL,5)+XW)=SUBSTR(XTEMP(XA,5),XW+1,1)
              XW=XW+1
            ENDDO
          ENDIF
          XW=0
          IF XAUX(XL,6)>0    && STYLEDESC width is 20 MAX BUT CAN BE LESS
            DO WHILE XW <  20 
              XMATRIX(XL,XAUX(XL,6)+XW)=SUBSTR(XTEMP(XA,6),XW+1,1)
              XW=XW+1
            ENDDO
          ENDIF
          XW=0
          IF XAUX(XL,7)>0    && STYLE PRICE A width is 7
            *B603713,10 HBG 11/22/2000 Increase The Width Of The pricea [Begin]
            *DO WHILE XW <  7
            DO WHILE XW <  12    && STYLE PRICE A width is 12
            *B603713,10 [End]
              XMATRIX(XL,XAUX(XL,7)+XW)=SUBSTR(XTEMP(XA,7),XW+1,1)
              XW=XW+1
            ENDDO
          ENDIF
          XW=0
          IF XAUX(XL,8)>0    && FABRIC DESC width is 20 MAX BUT CAN BE LESS
            DO WHILE XW <  20
              XMATRIX(XL,XAUX(XL,8)+XW)=SUBSTR(XTEMP(XA,8),XW+1,1)
              XW=XW+1
            ENDDO
          ENDIF
          XW=0
          IF XAUX(XL,10)>0    && COLOR CODE width is 6
            DO WHILE XW<6
              XMATRIX(XL,XAUX(XL,10)+XW)=SUBSTR(XTEMP(XA,10),XW+1,1)
              XW=XW+1
            ENDDO
          ENDIF
           *** print one line of one label
           XW=0
           DO WHILE XW<XLWIDTH
             XW=XW+1
             @ ROW,COL SAY XMATRIX(XL,XW)
             COL=COL+1
           ENDDO
        ENDDO
        ROW=ROW+1
      ENDDO
      XA=0
  ENDIF  && PRINT IF LINE OF LABELS IS COMPLETE
ENDFOR   && FOR EACH  QUANTITY

*-- TWO VARS, XNUMACROSS, XTNUMACROS.
*-- XTNUMACROS USED IN THIS IF..ENDIF TO MAKE SURE THAT XNUMACROSS REMAINS
*-- UNCHANGED. 

IF XA>0  .AND. XFLAGLAST  && print last labels
   XTNUMACROS=XA
   XL=0
   DO WHILE XL<XLLENGTH
      XL=XL+1
      XA=0
      COL=0
      DO WHILE XA<XTNUMACROS
         XA=XA+1
         *** put cuttkt,style,size on matrix if exists on line
          XW=0
          IF XAUX(XL,9)>0    && FABRIC CONTENTS width is 60
            DO WHILE XW < (LEN(XTEMP(XA,9)))
              XMATRIX(XL,XAUX(XL,9)+XW)=SUBSTR(XTEMP(XA,9),XW+1,1)
              XW=XW+1
            ENDDO
          ENDIF
         XW=0
         IF XAUX(XL,1)>0    && cuttkt width is 6
            DO WHILE XW<6
               XMATRIX(XL,XAUX(XL,1)+XW)=SUBSTR(XTEMP(XA,1),XW+1,1)
               XW=XW+1
            ENDDO
         ENDIF
         XW=0
         IF XAUX(XL,2)>0    && style width is 12
            *DO WHILE XW<12
            DO WHILE XW < lnMajLen
               XMATRIX(XL,XAUX(XL,2)+XW)=SUBSTR(XTEMP(XA,2),XW+1,1)
               XW=XW+1
            ENDDO
         ENDIF
         XW=0
         IF XAUX(XL,3)>0    && size width is 5
            DO WHILE XW<5
               XMATRIX(XL,XAUX(XL,3)+XW)=SUBSTR(XTEMP(XA,3),XW+1,1)
               XW=XW+1
            ENDDO
         ENDIF
         XW=0
         IF XAUX(XL,4)>0    && COLOR width is 15 MAX BUT CAN BE LESS && TAK FIXED 06/09/93
           DO WHILE XW < (LEN(XTEMP(XA,4)))
             XMATRIX(XL,XAUX(XL,4)+XW)=SUBSTR(XTEMP(XA,4),XW+1,1)
             XW=XW+1
           ENDDO
         ENDIF
         XW=0
         IF XAUX(XL,5)>0    && DYELOT width is 10
           DO WHILE XW<10
             XMATRIX(XL,XAUX(XL,5)+XW)=SUBSTR(XTEMP(XA,5),XW+1,1)
             XW=XW+1
           ENDDO
         ENDIF
         XW=0
          IF XAUX(XL,6)>0    && STYLE DESC. width is 20
            DO WHILE XW < (LEN(XTEMP(XA,6)))
              XMATRIX(XL,XAUX(XL,6)+XW)=SUBSTR(XTEMP(XA,6),XW+1,1)
              XW=XW+1
            ENDDO
          ENDIF
          XW=0
          IF XAUX(XL,7)>0      && STYLE PRICE A width is 7
            DO WHILE XW < (LEN(XTEMP(XA,7)))
              XMATRIX(XL,XAUX(XL,7)+XW)=SUBSTR(XTEMP(XA,7),XW+1,1)
              XW=XW+1
            ENDDO
          ENDIF
          XW=0
          IF XAUX(XL,8)>0    && FABRIC DESC. width is 20
            DO WHILE XW < (LEN(XTEMP(XA,8)))
              XMATRIX(XL,XAUX(XL,8)+XW)=SUBSTR(XTEMP(XA,8),XW+1,1)
              XW=XW+1
            ENDDO
          ENDIF
         XW=0
         IF XAUX(XL,10)>0    && COLOR CODE width is 6
           DO WHILE XW<6
             XMATRIX(XL,XAUX(XL,10)+XW)=SUBSTR(XTEMP(XA,10),XW+1,1)
             XW=XW+1
           ENDDO
         ENDIF
         *** print one line of one label
         XW=0
         DO WHILE XW<XLWIDTH
            XW=XW+1
            @ ROW,COL SAY XMATRIX(XL,XW)
            COL=COL+1
         ENDDO
      ENDDO
      ROW=ROW+1
   ENDDO
  
   XA=0
ENDIF  && IF XA> 0
RETURN

*!*************************************************************
*! Name      : HANG100
*! Developer : Khalid Mohi El-Din Mohamed - (KHM)
*! Date      : 12/31/98
*! Purpose   : Print Care Labels and hang tags.
*!*************************************************************
*! Calls     : 
*!             Procedures : HANG110.    Cutting tickets browse procedure.
*!                  HANG110A,HANG110B.  Print hang tags form A,B procedure.
*!                  CARE300,CARE310.    Print Care Labels procedure.
*!*************************************************************
*! Passed Parameters  :  lcPrtType : 'C'-> CutTkt
*!					     lcPrtType : 'P'-> Po
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  DO HANG100
*!*************************************************************
PROCEDURE HANG100
PARAMETER lcPrtType
*....+....1....+....2....+....3....+....4....+....5....+....6
*    Select : (E)ntered (C)omplete (T)icket# (V)endor..: X
*    Status : (O)pen    (H)old     a(C)tual  (A)ll.....: X
*    Date Range...... (Date Range or Vendor in same line)
*    Vendor............................................: XXXXXXX
*    Percentage to process.............................: 999 %
*    Form code to print (A) or (B).....................: X
*    Sorted : Style/(S)ize   or   Style/(C)olor........: X
*....+....1....+....2....+....3....+....4....+....5....+....6

SET COLOR TO &qClrNrm
CLEA

lcScrHdr=IIF(lcPrtType='H','HANG TAGS REPORTS','CARE LABELS REPORT')
DO lpPrnHdr WITH 'HANG100',lcScrHdr

lcStyleHdr = ALLTRIM(gfItemMask("HI"))

DO GRID WITH 6,'N'

STORE SPACE(1) TO XSEL,XSTAT
STORE SPACE(8) TO XVEN
STORE CTOD('  /  /  ') TO NULLDATE
STORE NULLDATE TO LDATE,HDATE
XSELSIZE = 'A'
XPERC=100
*XSORT='C'
XSORT='2'
XFILTER = ''
*** Form Codes Setting
IF FILE('HANG_CAR.MEM')
  RESTORE FROM HANG_CAR ADDI
ELSE
  FCODE='A'
  SAVE TO HANG_CAR ALL LIKE FCODE
ENDIF

@ 03,05 SAY 'Select : (E)ntered (C)omplete (T)icket# (V)endor..: '
@ 05,05 SAY 'Status : (O)pen    (H)old     a(C)tual  (A)ll.....: '
@ 09,05 SAY 'Percentage to process.............................:     %'
@ 11,05 SAY 'Select Size:  (A)LL /1/2/3/4/5/6/7/8 .............: '
IF lcPrtType='H'
  @ 13,05 SAY 'Form code to print (A) or (B).....................: '
ELSE
  *@ 13,05 SAY 'Sorted : Style/(S)ize   or   Style/(C)olor........: '
  @ 13,05 SAY 'Sorted : (1)'+lcStyleHdr+"/"+"Size"+" or (2)"+lcStyleHdr+":"
ENDIF 

DO WHILE .T.
  ** Getting the fields
  @ 03,57 GET XSEL PICTURE '!'  VALID XSEL$'ECTV'
  @ 05,57 GET XSTAT PICTURE '!' VALID XSTAT$'OHAC'
  @ 09,57 GET XPERC PICTURE '999' VALID XPERC > 0
  @ 11,57 GET XSELSIZE PICTURE 'X' VALID XSELSIZE $ 'A12345678'
  IF lcPrtType='H'
    @ 13,57 GET FCODE PICTURE 'X' VALID FCODE $ 'AB'
  ELSE
    *@ 13,57 GET XSORT PICTURE 'X' VALID XSORT $ 'SC'  
    @ 13,31+(LEN(lcStyleHdr)*2) GET XSORT PICTURE 'X' VALID XSORT $ '12'  
  ENDIF
  READ

  IF READKEY()=12
    llCancel = .T.  
    RETURN
  ENDIF

  SAVE TO HANG_CAR ALL LIKE FCODE
 
  IF XSEL ='V'
    @ 07,05 SAY 'Vendor............................................:'
    @ 07,57 SAY SPACE(22)
    @ 07,57 GET XVEN PICTURE '!!!!!!!!'
    READ    
    IF !SEEK(XVEN,'ApVendor')
      =gfApVnBrow(@xVen)
      @ 07,57 SAY XVEN COLOR &qClrSay
    ENDIF
    IF EMPTY(XVEN)
      LOOP
    ENDIF
    @ 07,66 SAY SUBSTR(ApVendor.cVenComp,1,13) COLOR &qClrSay
    *XFILTER='(XVEN=CONTR1 .OR. XVEN=CONTR2 .OR. XVEN=CONTR3)'
  ENDIF

  IF XSEL $ 'EC'          && IF XSEL='E' OR XSEL='C'
    @ 07,57 SAY SPACE(22)
    IF XSEL='E'
      @ 07,05 SAY 'Date Entered range................................:'
    ELSE
      @ 07,05 SAY 'Date Completed range..............................:'
    ENDIF
    @ 07,57 GET LDATE
    @ 07,68 GET HDATE
    READ
    IF EMPTY(HDATE)
      HDATE=LDATE
    ENDIF
    IF .NOT. EMPTY(LDATE) .AND. (HDATE<LDATE)
      DO MSG22 WITH 'Date range error !!!','$'
      LOOP
    ENDIF
    IF .NOT. EMPTY(HDATE)
      IF XSEL='E'
        XFILTER='(ENTERED >=LDATE .AND. ENTERED<=HDATE)'
      ELSE
        XFILTER='(COMPLETE >=LDATE .AND. COMPLETE <=HDATE)'
      ENDIF
    ENDIF
  ENDIF

  IF XSTAT <> 'A'
    XSTAT=IIF(XSTAT='C','A',XSTAT)      && 'C' INPUT IS ACTUALLY STATUS "A"
    IF EMPTY(XFILTER)
      XFILTER='STATUS $ XSTAT '
    ELSE
      XFILTER=XFILTER+' .AND. STATUS $ XSTAT'
    ENDIF
  ENDIF

  DO MSG22 WITH '<P>roceed <M>odify <C>ancel ? ','PMC'
  DO CASE
    CASE CHOICE = 'P'
      EXIT
    CASE CHOICE = 'C'
      llCancel = .T.  
      RETURN
  ENDCASE
ENDDO                                   && End Selection Criteria


** Temporary file for selected records
IF USED('&CTKTEMP')
  SELECT &CTKTEMP
  USE
ENDIF

DO MSG22 WITH 'Selecting the records ....','@'

SELECT CUTTKTH
IF ! EMPTY(XFILTER)
  IF EMPTY(XVen)
    COPY TO &gcWorkDir.&CTKTEMP FIELDS CUTTKT,RECFLAG FOR &XFILTER
  ELSE
    COPY STRUCTURE TO &gcWorkDir.&CTKTEMP
    =gfOpenFile(gcWorkDir+CTKTEMP,' ','EX')
    SCAN FOR IIF(!EMPTY(xFilter),&XFilter,.T.)
      llFound = .F.
      SCATTER MEMVAR MEMO
      IF SEEK('M'+CutTktH.CutTkt,'Mfgoprhd')
        SELECT Mfgoprhd
        SCAN REST WHILE cImTyp+cTktNo+cOprCode = 'M'+CutTktH.CutTkt;
             FOR !lInHouse AND cContCode = XVen
           llFound = .T.
           EXIT  
        ENDSCAN
        IF llFound
          INSERT INTO &CTKTEMP (CutTkt,RecFlag) VALUES(CutTktH.CutTkt,CutTktHRecFlag)
        ENDIF        
        SELECT CutTktH
      ENDIF  
    ENDSCAN
  ENDIF   
ELSE
  COPY TO &gcWorkDir.&CTKTEMP FIELDS CUTTKT,RECFLAG
ENDIF

=gfOpenFile(gcWorkDir+CTKTEMP,' ','EX')
INDEX ON CUTTKT TAG &CTKTEMP
SET ORDER TO TAG &CTKTEMP
SET RELATION TO CUTTKT INTO CUTTKTH

GO TOP
IF EOF()
  DO MSG22 WITH 'No records selected !!!','$'
  llCancel = .T.  
  RETURN
ENDIF

IF XSEL='T'
  SELE &CTKTEMP
  REPL ALL RECFLAG WITH 'N'
  SAVE SCREEN TO lsHG100
  DO HANG110          && Select Cutting Tickets to print
  RESTORE SCREEN FROM lsHG100
  SELE &CTKTEMP
  SET FILTER TO RECFLAG='Y'
  GO TOP
  IF EOF()
    DO MSG22 WITH 'No records selected !!!','$'
    llCancel = .T.  
    RETURN
  ENDIF
ENDIF




IF lcPrtType='H'    && Printing Hang Tag
  R_WIDTH='N'
  S_CLOSEALL=.F.

  DO MSG22 WITH 'Pre<v>iew <P>rint <C>ancel ? ','VPC'
  lcOGDevice  = gcDevice
  lcOGOutFile = gcOutFile
  lcOGWinTitl = ''
       
  DO CASE
    CASE Choice = "V"
      gcDevice  = "SCREEN"    && set printer to temprory file name
      gcOutFile = gcWorkDir+gfTempName()+".TXT"      
      SET PRINTER TO &gcOutFile
      DO MSG22 WITH 'PRINTING - <SPACE BAR> TO ABORT','@'
      SET DEVICE TO PRINTER
    CASE Choice = "P"
      *-C101560 WAB To let the Psetup() Display The Paper Type & Display Aria Print Setup
      *-C101560 WAB -Start 

      OGPlatForm = 'DOS'
      lcOGPlatForm = OGPlatForm

      *-C101560 WAB - END
      
      IF !PSETUP(.T.,.F.)
        llCancel = .T.
        SET DEVICE TO SCREEN
        RETURN    
      ENDIF
      gcOutFile = gcWorkDir+gfTempName()+".TXT"      
      SET DEVICE TO SCREEN 
      DO MSG22 WITH 'PRINTING - <SPACE BAR> TO ABORT','@'
      SET PRINTER TO &gcOutFile
      SET DEVICE TO PRINTER
    CASE Choice = "C"
      llCancel = .T. 
      SET DEVICE TO SCREEN 
      RETURN    
  ENDCASE    
  SET DEVI TO PRINT

  DO HANG110&FCODE             

  DO ENDREPORT
  SET PRINTER TO 
  SET DEVICE TO SCREEN
  gcDevice  = lcOGDevice
  gcOutFile = lcOGOutFile

ELSE               && Printing Care Labels
  XMATRIX=SPACE(1)
  XAUX=0
  XLABELCODE=SPACE(8)
  XLDESC=SPACE(25)
  XSIZECODE=SPACE(1)
  XPITCH='12'
  XPWIDTH='N'
  XLLENGTH=16
  XLWIDTH=10
  XNUMACROSS=1

  SELE CARELAB
  GO TOP
  IF RECCOUNT()=0 .OR. EOF()
  
  DO MSG22 WITH 'LABEL FORMS FILE EMPTY, Please create Labels Forms before printing Labels! ','$'
  
  *B602689,1 (start) you don't need to loop
  *LOOP
  *B602689,1 END  
  
  ENDIF


  SET COLOR TO &QCLRNRM
  @ 23,00 CLEAR TO 23,79
  GG = SYS(2002)

  *3....*....4....*....5....*....6....*....7....*...
  *   LBL CODE  ....LABEL.DESCRIPTION...  SIZ CODE
  *   12345678  1234567890123456789012345    1
  *3....*....4....*....5....*....6....*....7....*...

  DO WHILE .T.
    QRECNO = 0
    ROW    = 12
    DO DRAWBOX WITH QCLRMAG,'',09,27,21,77
    SET COLOR TO &QCLRMAG
    @ 10,33 SAY 'LBL CODE  ....LABEL.DESCRIPTION...  SZ CODE'
    @ 11,28 TO 11,76
    SCAN WHILE .NOT. EOF() .and. ROW < 21
      QRECNO(ROW) = RECNO()
      @ ROW,33 SAY LABELCODE  COLOR &qClrSay
      @ ROW,43 SAY LDESC      COLOR &qClrSay
      @ ROW,72 SAY SIZECODE   COLOR &qClrSay
      ROW=ROW+1
    ENDSCAN
    BP(1)=12
    BP(2)=29
    thiskey=BROW()
    DO CASE
      CASE thiskey = QESCKEY   && escape key
        EXIT
      CASE thiskey = QRETKEY   && return key - Label inquire and modification
        SET COLOR TO &QCLRNRM
        XLABELCODE=LABELCODE
        SEEK XLABELCODE
        IF FOUND()
          XLDESC=LDESC
          XSIZECODE=SIZECODE
          SELE CARESIZ
          SEEK XSIZECODE

          *B802212,1 KHM 05/04/99 (Begin) Commening the following 
          *B802212,1              line because the Pwidth is no 
          *B802212,1              longer exists in the CareSiz file.
          *XPWIDTH=PWIDTH
          *B802212,1 KHM 05/04/99 (End)
          
          XPITCH=PITCH
          XLWIDTH=LWIDTH
          XLLENGTH=LLENGTH
          XNUMACROSS=NUMACROSS
          SELE CARELAB
          SET ORDER TO TAG CARELABL
          SEEK XLABELCODE
          ****** restore information
          XL=0                                 && Lenght subindex
          DO WHILE XL<XLLENGTH
            XL = XL+1
            XAUX(XL,1)=CUT
            XAUX(XL,2)=STY
            XAUX(XL,3)=SIZ
            XAUX(XL,4)=CLR
            XAUX(XL,5)=DYE
            XAUX(XL,6)=SDC
            XAUX(XL,7)=SPR
            XAUX(XL,8)=FDC
            XAUX(XL,9)=FCN
            XAUX(XL,10)=CCD
            XW = 0                            && Width subindex
            DO WHILE XW<XLWIDTH
              XW = XW+1
              XMATRIX(XL,XW) = SUBSTR(MATRIX,XW,1)
            ENDDO
            SKIP
          ENDDO
          SET ORDER TO TAG CARELAB
          SAVE SCREEN TO CARE_3

          *-C101560 WAB  Change from CPI?? TO 'W' (WIDE) / 'N' (NARROW)
          *-C101560 WAB -Start 

          *DO CASE
          *  CASE XPITCH='10'
          *    R_WIDTH='CPI10'
          *  CASE XPITCH='12'
          *    R_WIDTH='CPI12'
          *  CASE XPITCH='17'
          *    R_WIDTH='CPI17'
          *  OTHERWISE
          *    R_WIDTH='CPI10'
          *ENDCASE
          DO CASE
            CASE XPITCH='10'
              R_WIDTH='N'
           CASE XPITCH='12'
              R_WIDTH='W'
            CASE XPITCH='17'
              R_WIDTH='W'
            OTHERWISE
              R_WIDTH='N'
          ENDCASE
          *-C101560 WAB -END

          S_CLOSEALL=.F.
          DO MSG22 WITH 'Pre<v>iew <P>rint <C>ancel ? ','VPC'
          lcOGDevice  = gcDevice
          lcOGOutFile = gcOutFile
          lcOGWinTitl = ''
       
          DO CASE
            CASE Choice = "V"
              gcDevice  = "SCREEN"    && set printer to temprory file name
              gcOutFile = gcWorkDir+gfTempName()+".TXT"      
              SET PRINTER TO &gcOutFile
              DO MSG22 WITH 'PRINTING - <SPACE BAR> TO ABORT','@'
              SET DEVICE TO PRINTER
            CASE Choice = "P"

              *-C101560 WAB To let the Psetup() Display The Paper Type & Display Aria Print Setup
              *-C101560 WAB -Start 
 
              OGPlatForm = 'DOS'
              lcOGPlatForm = OGPlatForm

              *-C101560 WAB - END

              IF !PSETUP(.T.,.F.)
                llCancel = .T.
                SET DEVICE TO SCREEN
                RETURN    
              ENDIF
              gcOutFile = gcWorkDir+gfTempName()+".TXT"      
              DO MSG22 WITH 'PRINTING - <SPACE BAR> TO ABORT','@'
              SET PRINTER TO &gcOutFile
              SET DEVICE TO PRINTER
            CASE Choice = "C"
              llCancel = .T. 
              SET DEVICE TO SCREEN 
              RETURN    
          ENDCASE            
          SET DEVI TO PRINT

          DO  CARE300               && Printing Care Labels
          DO ENDREPORT


          SET PRINTER TO 
          SET DEVICE TO SCREEN
          gcDevice  = lcOGDevice
          gcOutFile = lcOGOutFile

          RESTORE SCREEN FROM CARE_3
        ENDIF
        EXIT
    ENDCASE
  ENDDO
  SET COLOR TO &QCLRNRM
ENDIF
IF .NOT. ISCOLOR()
  QCLRBLK=QCLRTMP
ENDIF
RETURN

*!*************************************************************
*! Name      : HANG110
*! Developer : Khalid Mohi El-Din Mohamed - (KHM)
*! Date      : 12/31/98
*! Purpose   : Select the cutting tickets to mark for printing.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  DO HANG110
*!*************************************************************
*! Layout    :
*....*....1....*....2....*....3....*....4....*....5....*....6....*....7....*...
* CUTKT  STYLE        S ISSUE    COMPLETE SE DI BUDGET  RECVD   DAMAGED  OPEN
* XXXXXX 123456789012 X 99/99/99 99/99/99 XX XX 9999999 9999999 9999999 9999999
*....*....1....*....2....*....3....*....4....*....5....*....6....*....7....*...
****************************************************************************
PROCEDURE HANG110

SELE &CTKTEMP
GOTO TOP
FF = SELECT()
CLEAR
SET COLOR TO &QCLRNRM
DO lpPrnHdr WITH 'HANG110','SELECT CUTTING TICKETS TO PRINT '

*----------------------------------
* BROWSE/SELECT CUTTING TICKET FILE
*----------------------------------
DO WHILE .T.

  SELECT &CTKTEMP
  IF EOF()
    GOTO TOP
  ENDIF

  LCUTTKT = SPACE(6)
  HCUTTKT = SPACE(6)

  ROW    = 4
  QRECNO = 0
  SAVREC = RECNO()

  DO DRAWBOX WITH QCLRMAG,'',1,0,21,79
  SET COLOR TO &QCLRMAG
  @ 02,01 SAY ' CUTKT  STYLE        S ENTERED  COMPLETE SE DI BUDGET  RECVD   DAMAGED    OPEN'
  @ 03,01 TO 03,78
  SET COLOR TO &QCLRSAY

  SCAN WHILE (.NOT. EOF()) .AND. (ROW<21)
    TMPCLR = IIF(RECFLAG='Y', QCLRREV, QCLRSAY)
    SET COLOR TO &TMPCLR
    @ ROW,02 SAY CUTTKTH->CUTTKT
    SET COLOR TO &QCLRSAY
    @ ROW,09 SAY CUTTKTH->STYLE
    @ ROW,22 SAY CUTTKTH->STATUS
    @ ROW,24 SAY CUTTKTH->ENTERED
    @ ROW,33 SAY CUTTKTH->COMPLETE
    @ ROW,42 SAY CUTTKTH->SEASON
    @ ROW,45 SAY CUTTKTH->cDIVISION
    @ ROW,48 SAY CUTTKTH->PCS_BUD PICTURE '9999999'
    @ ROW,56 SAY CUTTKTH->PCS_REC PICTURE '9999999'
    @ ROW,64 SAY CUTTKTH->PCS_DAM PICTURE '9999999'
    @ ROW,72 SAY CUTTKTH->PCS_OPN PICTURE '9999999'
    QRECNO(ROW) = RECNO()
    ROW = ROW + 1
  ENDSCAN

  BP(1) = 4
  BP(2) = 1
  SET COLOR TO &QCLRMAG
  THISKEY = BROW()

  IF THISKEY = 27
     RETURN
  ENDIF

  IF THISKEY <> 13
     LOOP
  ENDIF
  SELECT &CTKTEMP
  SET COLOR TO &QCLRNRM
  @ 23,00 CLEAR
  STORE CUTTKT TO LCUTTKT, HCUTTKT
  @ 23,20 SAY 'SELECT CUTTKT# => '
  @ 23,40 GET LCUTTKT   PICT '@!'
  @ 23,48 GET HCUTTKT   PICT '@!'
  READ
  @ 23,00

  IF LEN(TRIM(LCUTTKT)) = 0
     GOTO SAVREC
     LOOP
  ENDIF

  SELECT &CTKTEMP
  SEEK TRIM(LCUTTKT)
  IF EOF()
    ? CHR(7)
    GOTO SAVREC
    LOOP
  ENDIF
  SAVREC = RECNO()
  HCUTTKT = IIF(LEN(TRIM(HCUTTKT))<>0.AND.HCUTTKT>LCUTTKT, HCUTTKT, LCUTTKT)

  *----------------------------------
  * FLAG ALL PRINTABLE CUTTKTS
  *----------------------------------
  DO WHILE .T.
    IF EOF() .OR. CUTTKT>HCUTTKT
       EXIT
    ENDIF
    IF RECFLAG='Y'
      REPLACE RECFLAG WITH 'N'
    ELSE
      REPLACE RECFLAG WITH 'Y'
    ENDIF
    SKIP
  ENDDO
  SELECT &CTKTEMP
  GOTO SAVREC

ENDDO
RETURN

*!*************************************************************
*! Name      : HANG110
*! Developer : Khalid Mohi El-Din Mohamed - (KHM)
*! Date      : 12/31/98
*! Purpose   : Hang tags for Form Code A.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  DO HANG110A
*!*************************************************************
PROCEDURE HANG110A

XCUTTKT  = SPACE(6)
ROW=1

SELE &CTKTEMP
GOTO TOP
SCAN WHILE .NOT. EOF() .AND. (INKEY() <> 32)
  WAIT WINDOW "CutTkt # : " + CUTTKT NOWAIT
  XCUTTKT = CUTTKT
  SELE CUTTKTL
  SEEK XCUTTKT
  SCAN WHILE CutTkt+Style+Dyelot+TranCd+STR(RECNO(),7) = XCUTTKT;
       FOR TRANCD = '1'
     IF XSELSIZE = 'A'
        XBEG = 1
        XEND = SCALE->CNT
     ELSE
        IF VAL(XSELSIZE) > SCALE->CNT
          LOOP
        ENDIF 
        XBEG = VAL(XSELSIZE)
        XEND = XBEG
     ENDIF
     FOR X=XBEG TO XEND
        Z=STR(X,1)
        IF QTY&Z<=0
           XQTY=0
        ELSE
           XQTY = INT(ROUND(((XPERC/100) * QTY&Z),0))
           XQTY = IIF(XQTY = 0, 1, XQTY)   && prints at least 1
        ENDIF
        FOR Y=1 TO XQTY
          
          *IF gnStyleWid = 7
          *  @ ROW,01 SAY 'STYLE: '+SUBSTR(STYLE,1,7)+'   C/T#: '+CUTTKT
          *ELSE
          *  @ ROW,01 SAY 'STYLE: '+STYLE+'   C/T#: '+CUTTKT 
          *ENDIF
          @ ROW,01 SAY 'STYLE: '+ALLTRIM(SUBSTR(STYLE,1,LnMajLen))+'   C/T#: '+CUTTKT 
          ROW=ROW+1
          @ ROW,06 SAY STYLE->DESC
          ROW=ROW+1
          @ ROW,06 SAY FABRIC->DESC
          ROW=ROW+1
          
          *IF gnColorWid = 3
          *  @ ROW,01 SAY 'CLR: '+SUBSTR(COLOR,1,3)+' '+ALLTRIM(CODE->CDATA)
          *ELSE
          *  @ ROW,01 SAY 'CLR: '+COLOR+' '+ALLTRIM(CODE->CDATA)
          *ENDIF  

          IF lnColorLen > 0
            lcClrCode = SUBSTR(Style,lnClrPo,lnColorLen)
            @ ROW,01 SAY 'CLR: '+lcClrCode+;
                           ' '+ALLTRIM(gfCodDes(lcClrCode,PADR('COLOR',10)))
            ROW=ROW+1
          ENDIF
          
          @ ROW,01 SAY 'SIZE: '+SCALE->SZ&Z
          ROW=ROW+2
        ENDFOR
     ENDFOR
  ENDSCAN
  SELE &CTKTEMP
ENDSCAN
RETURN

*!*************************************************************
*! Name      : HANG110B
*! Developer : Khalid Mohi El-Din Mohamed - (KHM)
*! Date      : 12/31/98
*! Purpose   : Hang tags for Form Code B.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  DO HANG110B
*!*************************************************************
PROCEDURE HANG110B

XCUTTKT  = SPACE(6)
ROW=1
SELE &CTKTEMP
GOTO TOP
SCAN WHILE .NOT. EOF() .AND. (INKEY() <> 32)
  WAIT WINDOW "CutTkt # : " + CUTTKT NOWAIT
  XCUTTKT = CUTTKT
  SELE CUTTKTL
  SEEK XCUTTKT
  SCAN WHILE CutTkt+Style+Dyelot+TranCd+STR(RECNO(),7) = XCUTTKT;
       FOR TranCd = '1'
     IF XSELSIZE = 'A'
        XBEG = 1
        XEND = SCALE->CNT
     ELSE
        IF VAL(XSELSIZE) > SCALE->CNT
          LOOP
        ENDIF 
        XBEG = VAL(XSELSIZE)
        XEND = XBEG
     ENDIF
     FOR X=XBEG TO XEND
        Z=STR(X,1)
        IF QTY&Z<=0
           XQTY=0
        ELSE
           XQTY = INT(ROUND(((XPERC/100) * QTY&Z),0))
           XQTY = IIF(XQTY = 0, 1, XQTY)   && prints at least 1
        ENDIF
        FOR Y=1 TO XQTY

          *IF gnStyleWid = 7
          *  @ ROW,01 SAY 'STYLE: '+SUBSTR(STYLE,1,7)+'   C/T#: '+CUTTKT
          *ELSE
          *  @ ROW,01 SAY 'STYLE: '+STYLE+'   C/T#: '+CUTTKT 
          *ENDIF
          @ ROW,01 SAY 'STYLE: '+ALLTRIM(SUBSTR(STYLE,1,LnMajLen))+'   C/T#: '+CUTTKT 
          
          ROW=ROW+1
          @ ROW,06 SAY STYLE->DESC
          ROW=ROW+1
          @ ROW,06 SAY FABRIC->DESC
          ROW=ROW+1
          
          *IF gnColorWid = 3
          *  @ ROW,01 SAY 'CLR: '+SUBSTR(COLOR,1,3)+' '+ALLTRIM(CODE->CDATA)
          *ELSE
          *  @ ROW,01 SAY 'CLR: '+COLOR+' '+ALLTRIM(CODE->CDATA)
          *ENDIF  
          
          IF lnColorLen > 0
            lcClrCode = SUBSTR(Style,lnClrPo,lnColorLen)
            @ ROW,01 SAY 'CLR: '+lcClrCode+;
                           ' '+ALLTRIM(gfCodDes(lcClrCode,PADR('COLOR',10)))
            ROW=ROW+1
          ENDIF

          DO CASE
               CASE ALLTRIM(SCALE->SZ&Z) == '34' .OR. ALLTRIM(SCALE->SZ&Z) == '2'
                 @ ROW,01 SAY 'FRENCH SIZE: 34   USA SIZE: 2'
               CASE ALLTRIM(SCALE->SZ&Z) == '36' .OR. ALLTRIM(SCALE->SZ&Z) == '4'
                 @ ROW,01 SAY 'FRENCH SIZE: 36   USA SIZE: 4'
               CASE ALLTRIM(SCALE->SZ&Z) == '38' .OR. ALLTRIM(SCALE->SZ&Z) == '6'
                 @ ROW,01 SAY 'FRENCH SIZE: 38   USA SIZE: 6'
               CASE ALLTRIM(SCALE->SZ&Z) == '40' .OR. ALLTRIM(SCALE->SZ&Z) == '8'
                 @ ROW,01 SAY 'FRENCH SIZE: 40   USA SIZE: 8'
               CASE ALLTRIM(SCALE->SZ&Z) == '42' .OR. ALLTRIM(SCALE->SZ&Z) == '10'
                 @ ROW,01 SAY 'FRENCH SIZE: 42   USA SIZE: 10'
               CASE ALLTRIM(SCALE->SZ&Z) == '44' .OR. ALLTRIM(SCALE->SZ&Z) == '12'
                 @ ROW,01 SAY 'FRENCH SIZE: 44   USA SIZE: 12'
               OTHERWISE
                  @ ROW,01 SAY 'SIZE: '+SCALE->SZ&Z
          ENDCASE
          ROW=ROW+2
        ENDFOR
     ENDFOR
  ENDSCAN
  SELE &CTKTEMP
ENDSCAN
RETURN

*!*************************************************************
*! Name      : lfGtNonMaj
*! Developer : Khalid Mohi El-Din Mohamed - (KHM)
*! Date      : 12/31/98
*! Purpose   : To get the nonmajor segements.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfGtNonMaj
*!*************************************************************
FUNCTION lfGtNonMaj

lnMajSeg   = gfItemMask('SM')  && No. of major segments.
lnNonMajPo = 0
DIMENSION laMajSeg[1,1]
= gfItemMask(@laMajSeg)
llStopConc = .F.
STORE '' TO lcNonMajPi,lcFreeClr
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSeg,1)
  lnNonMajPo = IIF(lnNonMajPo = 0,laMajSeg[lnI,4],lnNonMajPo)
  IF laMajSeg[lnI,1] = 'F' AND !llStopConc
    lcFreeClr  = IIF(EMPTY(lcFreeClr),laMajSeg[lnI,1],lcFreeClr)
    lcNonMajPi = IIF(EMPTY(lcNonMajPi),laMajSeg[lnI,3],;
                     lcNonMajPi + laMajSeg[lnI-1,6] + laMajSeg[lnI,3])
    lcNonMajT  = IIF(EMPTY(lcNonMajT),PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])),;
                     lcNonMajT + laMajSeg[lnI-1,6] + PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])))
  ENDIF
  IF laMajSeg[lnI,1] = 'C' OR (!EMPTY(lcFreeClr) AND laMajSeg[lnI,1] != 'F')
    IF laMajSeg[lnI,1] = 'C'
      lnClrPo    = laMajSeg[lnI,4]
      lcFreeClr  = laMajSeg[lnI,1]    && which will be 'C'
      lcNonMajPi = laMajSeg[lnI,3]
      lcNonMajT  = PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3]))
      EXIT
    ELSE
      llStopConc = .T.
    ENDIF
  ENDIF   && end If you Find Color Type or Find Free Type and current type not Free.
ENDFOR    && end Loop Around Non Major elements.
STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen
RETURN ''

*!*************************************************************
*! Name      : lpPrnHdr
*! Developer : Khalid Mohi El-Din Mohamed - (KHM)
*! Date      : 12/31/98
*! Purpose   : To the program's header.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  lcProg : The program's name
*!                       lcTitle: The program's title 
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  DO lpPrnHdr
*!*************************************************************
PROCEDURE lpPrnHdr
PARAMETERS lcProg,lcTitle
SET COLOR TO &QCLRMAG
PRIVATE lnPos
@ 00,01 SAY lcProg+'('+gcAct_Comp+')'
lnPos = ((80-(LEN(TRIM(lcTitle))))/2)
@ 00,lnPos SAY lcTitle
@ 00,68 SAY gdSysDate
SET COLOR TO &QCLRNRM
RETURN


*!*************************************************************
*! Name      : DRAWBOX
*! Developer : Khalid Mohi El-Din Mohamed - (KHM)
*! Date      : 12/31/98
*! Purpose   : To draw the box
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  Color,Title,TLine,TCol,BLine,BCol
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  DO DRAWBOX
*!*************************************************************
PROCEDURE DRAWBOX
PARAMETERS COLOR,TITLE,TLINE,TCOL,BLINE,BCOL

PRIVATE COLOR,TCOL,TLINE,TITLE,BLINE,BCOL

SET COLOR TO &COLOR
@ TLINE+1,TCOL+1 TO BLINE-1,BCOL-1 CLEAR

@ TLINE,TCOL,BLINE,BCOL BOX CHR(219)+CHR(223)+CHR(219)+CHR(219)+CHR(219);
+CHR(220)+CHR(219)+CHR(219)+" "

IF .NOT. EMPTY(TITLE) .AND. LEN(TITLE)>1
  XCOL = CENTER(TCOL,BCOL,TITLE)
  @ TLINE,XCOL SAY TITLE
ENDIF

SET COLOR TO &QCLRNRM
RETURN
