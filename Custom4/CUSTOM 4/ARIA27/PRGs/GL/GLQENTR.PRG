*:************************************************************************
*:
*: Procedure file: GLQENTR.PRG
*:
*:         System: ARIA BUSINESS SYSTEM
*:         Module: General Ledger
*:         Author: Reham Aly Alallamy
*:      Copyright (c) 
*:  Last modified:  /  /
*:
*:  Procs & Fncts: 
*:               
*:      Documented 12/22/1993
*:************************************************************************
*E100242,1 Reham On 06/10/95
*E100242,1 Enhance the account entries browse & the entries details 
*E100242,1 browse  ( DOS  &  WINDOWS )
*E300683,5 AHMED 06/10/97 Add prgs directory path to the calling of programs
*B803122,1 SSE 04/02/2000 Invalid Subscript reference when Pressin OK push
*B803122,1                button in the Mover of the Source module   
*B803361,1 NAD 07/11/2000 Increase the width of the comment field in the browse
*B803361,1                screen of the account entries.   
*C200958,1 NNA 03/05/2008 (T20070920.0003) Add trigger to build an option menu to open the payable invoice screen 
*C200958,1 NNA            this trigger found in Glqentr.scx screen
*:************************************************************************
*
PARAMETERS puCompYrs , puYerPer , lnFrstElm , lnElmNo , llEntry
PRIVATE    puCompYrs , puYerPer , lnFrstElm , lnElmNo , llEntry

lcTitleEnt = "Account Entries"
lcTitleDet = "Transactions details"
=lfgetQuery()
SELECT (lcCursor1)
GO TOP
IF EOF()
  IF llEntry
    =gfModalGen("TRM02182B00000","DIALOG")
  ELSE
    =gfModalGen("TRM02048B00000","DIALOG")
  ENDIF
ENDIF

=lfPushKeys()

*E300683,5 Call *.SPR from screens directory
* DO GLQENTR.SPR 
DO (gcScrDir + gcWinAppl + '\GLQENTR.SPR')
*E300683,5 end   
=lfKeyClean()

*!**************************************************************************
*!
*!       Function: lfgetQuery
*!
*!**************************************************************************
*
*** The get query function that called when any change ***
*** in the selct criteria.

FUNCTION lfgetQuery

IF cbSummary
  lcAccMaj = SUBSTR(ALLTRIM(lcAccount),1,AT('-',lcAccount)-1)+;
             STRTRAN(SUBSTR(ALLTRIM(lcAccount),AT('-',lcAccount),;
             LEN(ALLTRIM(lcAccount))-AT('-',lcAccount)+1),'0','_')

  SELECT GLPTRNDT.CBATCHNO,GLPTRNDT.CTRANNO,GLPTRNHD.CSRCMODUL,;
         GLPTRNHD.CSRCJRNL,GLPTRNHD.DTRNPDATE,GLPTRNDT.CTRDTEXP,;
         IIF(GLPTRNDT.CDRORCR = 'D',' '+STR(GLPTRNDT.NAMOUNT,15,2)+' ',;
         '('+STR(GLPTRNDT.NAMOUNT,15,2)+')') AS 'Deb_Crd';
    FROM &gcDataDir.GLPTRNHD,&gcDataDir.GLPTRNDT;
   WHERE GLPTRNDT.CTRANNO    = GLPTRNHD.CTRANNO .AND.;
         (GLPTRNHD.CTRNPPRD  = puYerPer    .AND.;
          GLPTRNHD.CTRNPYR   = puCompYrs   .AND.;
          GLPTRNDT.CACCTCODE LIKE lcAccMaj .AND.;
          GLPTRNHD.CSRCJRNL  $ lcSJ_IDStr  .AND.;
          GLPTRNHD.CSRCMODUL $ lcAppIdStr) .AND.;
          GLPTRNHD.CTRNSTAT  $ 'PZ';
   ORDER BY GLPTRNDT.CBATCHNO, GLPTRNDT.CTRANNO,;
            GLPTRNHD.CSRCMODUL,GLPTRNHD.CSRCJRNL,;
            GLPTRNHD.DTRNPDATE;
    INTO CURSOR (lcCursor1)
ELSE
  SELECT GLPTRNDT.CBATCHNO,GLPTRNDT.CTRANNO,GLPTRNHD.CSRCMODUL,;
         GLPTRNHD.CSRCJRNL,GLPTRNHD.DTRNPDATE,GLPTRNDT.CTRDTEXP,;
         IIF(GLPTRNDT.CDRORCR = 'D',' '+STR(GLPTRNDT.NAMOUNT,15,2)+' ',;
         '('+STR(GLPTRNDT.NAMOUNT,15,2)+')') AS 'Deb_Crd';
    FROM &gcDataDir.GLPTRNHD,&gcDataDir.GLPTRNDT;
    INTO CURSOR (lcCursor1);
   WHERE GLPTRNDT.CTRANNO    = GLPTRNHD.CTRANNO .AND.;
         (GLPTRNHD.CTRNPPRD  = puYerPer    .AND.;
          GLPTRNHD.CTRNPYR   = puCompYrs   .AND.;
          GLPTRNDT.CACCTCODE = laData[1]   .AND.;
          GLPTRNHD.CSRCJRNL  $ lcSJ_IDStr  .AND.;
          GLPTRNHD.CSRCMODUL $ lcAppIdStr) .AND.;
          GLPTRNHD.CTRNSTAT  $ 'PZ';
   ORDER BY GLPTRNDT.CBATCHNO, GLPTRNDT.CTRANNO,;
            GLPTRNHD.CSRCMODUL,GLPTRNHD.CSRCJRNL,;
            GLPTRNHD.DTRNPDATE
ENDIF

lnFactor = IIF(_DOS,0,2)
    
lcWid1 = STR(MAX(LEN('Batch#')+lnFactor,FSIZE('CBATCHNO')+lnFactor))
lcWid2 = STR(MAX(LEN('Trans#')+lnFactor,FSIZE('CTRANNO')+lnFactor))
lcWid3 = STR(MAX(LEN('SM')+lnFactor,FSIZE('CSRCMODUL')+lnFactor))

*E100242,1 Add 2 to the date width to display it proper in the browse.
lcWid4 = STR(MAX(LEN('Date')+lnFactor+2,FSIZE('DTRNPDATE')+lnFactor+2))

lcWid5 = STR(MAX(LEN('Debit/(Credit)')+lnFactor,FSIZE('Deb_Crd')+lnFactor))
lcWid6 = STR(MAX(LEN('SJ')+lnFactor,FSIZE('CSRCJRNL')+lnFactor))

*E100242,1 Remark the old browse field.
*lcBrowEntr = "CBATCHNO :H='Batch#':&lcWid1,CTRANNO"+;
             ":H='Trans#':&lcWid2,CSRCMODUL :H='SM':&lcWid3,"+;
             "CSRCJRNL :H='SJ':&lcWid6,DTRNPDATE "+;
             ":H='Date':&lcWid4,CTRDTEXP"+;
             ":H='Comment':22,Deb_Crd :H='Debit/(Credit)':&lcWid5"

*E100242,1 Add this new browse field (Make the amount before the comment).

*B803361,1  (Start) Increase the width of the comment field in the browse.
*lcBrowEntr = "CBATCHNO :H='Batch#':&lcWid1,CTRANNO"+;
             ":H='Trans#':&lcWid2,CSRCMODUL :H='SM':&lcWid3,"+;
             "CSRCJRNL :H='SJ':&lcWid6,DTRNPDATE "+;
             ":H='Date':&lcWid4,Deb_Crd :H='Debit/(Credit)':&lcWid5,"+;
             "CTRDTEXP:H='Comment':22"

lcBrowEntr = "CBATCHNO :H='Batch#':&lcWid1,CTRANNO"+;
             ":H='Trans#':&lcWid2,CSRCMODUL :H='SM':&lcWid3,"+;
             "CSRCJRNL :H='SJ':&lcWid6,DTRNPDATE "+;
             ":H='Date':&lcWid4,Deb_Crd :H='Debit/(Credit)':&lcWid5,"+;
             "CTRDTEXP:H='Comment':40"
*B803361,1  (End) 
*!**************************************************************************
*!
*!      Function: lfwCompYrs
*!
*!**************************************************************************
*
*** The years popup when function. ***

FUNCTION lfwCompYrs

lcOldYear = puCompYrs

*!**************************************************************************
*!
*!      Function: lfvCompYrs
*!
*!**************************************************************************
*
*** The years popup valid function. ***

FUNCTION lfvCompYrs

IF lcOldYear <> puCompYrs
  lnYearNo = ASCAN(laCompYrs,puCompYrs)
  IF lnYearNo > 0
    IF lnYearNo = 1
      lnFrstElm = 1
    ELSE
      lnFrstElm = 1
      FOR lnCount = 1 TO lnYearNo - 1
        lnFrstElm = lnFrstElm + VAL(laPrdNo[lnCount])
      ENDFOR
    ENDIF
    lnElmNo = VAL(laPrdNo[lnYearNo])
  ELSE
    lnFrstElm = 1
    lnElmNo   = VAL(laPrdNo[1])
  ENDIF

  puYerPer = '01'

  SHOW GET puYerPer

  =lfgetQuery()
  =lfwEnBrows()
ENDIF

*!**************************************************************************
*!
*!      Function: lfwYerPer
*!
*!**************************************************************************
*
*** The years periods when function. ***

FUNCTION lfwYerPer

lcOldPer = puYerPer

*!**************************************************************************
*!
*!      Function: lfvYerPer
*!
*!**************************************************************************
*
*** The years periods valid function. ***

FUNCTION lfvYerPer

IF lcOldPer <> puYerPer
  =lfgetQuery()
  =lfwEnBrows()
ENDIF

*!**************************************************************************
*!
*!      Function: lfvSM
*!
*!**************************************************************************
*
*** The source module push button valid function. ***

FUNCTION lfvSM

=lfKeyClean()

=gfMover(@laAppDesc,@laAppl,"Source module",.T.)

lcAppIdStr   = ""

*B803122,1 SSE 04/02/2000 Move Source or Target to another Array [Begin]
PRIVATE laSource,laTarget
DIMENSION laSource[1],laTarget[1]
= ACOPY(laAppDesc,laSource)
FOR lnI = 1 TO ALEN(laAppDesc,1)
  laSource[lnI] = STRTRAN(laSource[lnI],"\","")
ENDFOR

IF EMPTY(laAppl)
  laTarget = ""
ELSE
  = ACOPY(laAppl,laTarget)
ENDIF  
*B803122,1 SSE 04/02/2000 [End]

*B803122,1 SSE 04/02/2000 For loop around the Target Array created [Begin]
*IF !EMPTY(laAppl[1])
*  FOR lnCount  = 1 TO ALEN(laAppl,1)
   *lcAppIdStr = lcAppIdStr + IIF(EMPTY(lcAppIdStr) , "" , "," );
   *             + ALLTRIM(laAppID[ASCAN(laAppDesc,laAppl[lnCount])])
*  ENDFOR
*ENDIF

IF !EMPTY(laTarget[1])
  FOR lnCount  = 1 TO ALEN(laTarget,1)
    lcAppIdStr = lcAppIdStr + IIF(EMPTY(lcAppIdStr) , "" , "," );
                 + ALLTRIM(laAppID[ASCAN(laSource,laTarget[lnCount])])
  ENDFOR
ENDIF  
*B803122,1 SSE 04/02/2000 [End]

=lfgetQuery()

=lfPushKeys()

=lfwEnBrows()

*!**************************************************************************
*!
*!      Function:  lfvSJ
*!
*!**************************************************************************
*
*** The source journal push button valid function. ***

FUNCTION  lfvSJ

IF EMPTY(laSJDesc[1])
  =gfModalGen("TRM02087B00000","Dialog")
ELSE
  =lfKeyClean()
  =gfMover(@laSJDesc,@laSorcJor,"Source journal",.T.)

  lcSJ_IDStr   = ""
  
  *B803122,1 SSE 04/02/2000 Move Source or Target to another Array [Begin]
  PRIVATE laSource,laTarget
  DIMENSION laSource[1],laTarget[1]
  = ACOPY(laSJDesc,laSource)
  FOR lnI = 1 TO ALEN(laSJDesc,1)
    laSource[lnI] = STRTRAN(laSource[lnI],"\","")
  ENDFOR

  IF EMPTY(laSorcJor)
    laTarget = ""
  ELSE
    = ACOPY(laSorcJor,laTarget)
  ENDIF  
  *B803122,1 SSE 04/02/2000 [End]

  *B803122,1 SSE 04/02/2000 For loop around the Target Array created [Begin]
  *IF !EMPTY(laSorcJor[1])
  *  FOR lnCount  = 1 TO ALEN(laSorcJor,1)
  *    lcSJ_IDStr = lcSJ_IDStr + IIF(EMPTY(lcSJ_IDStr) , "" , "," );
  *                 + ALLTRIM(laSorcID[ASCAN(laSJDesc,laSorcJor[lnCount])])
  *  ENDFOR
  *ENDIF

  IF !EMPTY(laTarget[1])
    FOR lnCount  = 1 TO ALEN(laTarget,1)
      lcSJ_IDStr = lcSJ_IDStr + IIF(EMPTY(lcSJ_IDStr) , "" , "," );
                   + ALLTRIM(laSorcID[ASCAN(laSource,laTarget[lnCount])])
    ENDFOR
  ENDIF  
  *B803122,1 SSE 04/02/2000 [End]

  =lfgetQuery()

  =lfPushKeys()

  =lfwEnBrows()
ENDIF  

*!**************************************************************************
*!
*!      FUNCTION lfwEnBrows
*!
*!**************************************************************************
*
*** The browse entries that called from the screen when. ***

FUNCTION lfwEnBrows

BROWSE   FIELDS &lcBrowEntr;
         NOAPPEND ;
         NODELETE ;
         NOEDIT   ;
         SAVE     ;
         NOWAIT   ;
         NOMENU   ;
         NOCLEAR  ;
         LOCK 0   ;
         TITLE lcTitleEnt;
         WINDOW GLQENTR3 IN WINDOW AWDGLQENTR

*!**************************************************************************
*!**************************************************************************
*!**************************************************************************
*!**************************************************************************
*!
*!      Function: lfvDetail
*!
*!**************************************************************************
*
*** The details push button valid function. ***

FUNCTION lfvDetail

=lfKeyClean()

lcSaveDtl = ALIAS()

lcBatchno  = CBATCHNO
lcTransNo  = CTRANNO
lcTypCode2 = lcTypCode

lnTotDeb   = 0
lnTotCrd   = 0
lnTotal    = 0

SELECT GLPTRNDT.CACCTCODE ,;
       GLACCHAR.CACCNLDES ,;
       GLPTRNDT.CDRORCR,;
       IIF(CDRORCR='D',STR(NAMOUNT,15,2),STR(0,15,2)) AS 'DEBIT' ,;
       IIF(CDRORCR='C',STR(NAMOUNT,15,2),STR(0,15,2)) AS 'CREDIT' ;
       INTO CURSOR (lcCursor2);
       WHERE GLPTRNDT.CACCTCODE  = GLACCHAR.CACCTCODE;
       .AND. GLPTRNDT.CBATCHNO   = lcBatchno;
       .AND. GLPTRNDT.CTRANNO    = lcTransNo;
       FROM GLPTRNDT,GLACCHAR;
       ORDER BY GLPTRNDT.CDRORCR
       
DO CASE
  CASE _DOS
    lcWidD1  = STR(MAX(LEN('DEBIT'),FSIZE('DEBIT')))
    lcWidD2  = STR(MAX(LEN('Credit'),FSIZE('CREDIT')))
    lcAccLen = ALLTRIM(STR(lnAcsSegSz))
    
    *E100242,1 Make the desc. width in the browse less by 1 under dos.
    *lcDesLen = ALLTRIM(STR(40-lnAcsSegSz))
    lcDesLen = ALLTRIM(STR(39-lnAcsSegSz))
    
    lcAcSeg  = lcAcSegDes
  CASE _WINDOWS
    lcWidD1  = STR(MAX(LEN('DEBIT')+2,FSIZE('DEBIT')+2))
    lcWidD2  = STR(MAX(LEN('Credit')+2,FSIZE('CREDIT')+2))
    lcAccLen = ALLTRIM(STR(lnAcsSegSz+2))
    
    *E100242,1 Under windows make the desc. width less by 7.
    *lcDesLen = ALLTRIM(STR(40-(lnAcsSegSz+2)))
    lcDesLen = ALLTRIM(STR(33-(lnAcsSegSz+2)))
    
    lcAcSeg  = " " + lcAcSegDes
ENDCASE

lcBrowDet = "CACCTCODE:H=lcAcSeg:&lcAccLen,"+;
            "CACCNLDES:H='Description':&lcDesLen,"+;
            "DEBIT:H='Debit':&lcWidD1,CREDIT:H='Credit':&lcWidD2"

SUM VAL(DEBIT),VAL(CREDIT) TO lnTotDeb,lnTotCrd

lnTotal = MAX(lnTotDeb,lnTotCrd)


PUSH KEY
ON KEY LABEL ESC        DO lfTabTrpDt
ON KEY LABEL TAB        DO lfTabTrpDt
ON KEY LABEL BACKTAB    DO lfTabTrpDt
ON KEY LABEL CTRL+W     lnDumi = 1
ON KEY LABEL CTRL+Q     lnDumi = 1
ON KEY LABEL CTRL+END   lnDumi = 1

GO TOP IN (lcCursor2)
*E300683,5 Call *.SPR from screens directory
* DO GLQDETL.SPR 
DO (gcScrDir + gcWinAppl + '\GLQDETL.SPR')
*E300683,5 end   
SELECT (lcSaveDtl)

=lfKeyClean()

=lfPushKeys()

*!**************************************************************************
*!
*!      FUNCTION lfwDtBrows
*!
*!**************************************************************************
*
*** The browse details that called from the screen when function. ***

FUNCTION lfwDtBrows

BROWSE   FIELDS &lcBrowDet;
         NOAPPEND ;
         NODELETE ;
         NOEDIT   ;
         NOMENU   ;
         SAVE     ;
         NOWAIT   ;
         NOCLEAR  ;
         LOCK 0   ;
         TITLE lcTitleDet;
         WINDOW GLQDETL3 IN WINDOW AWDGLQDETL

*!**************************************************************************
*!
*!      Function: lfTabTrpEn
*!
*!**************************************************************************
*
*** The entry browse trap function. ***

FUNCTION lfTabTrpEn

*** Trabing the tab key for the account screen ***

DO CASE
  CASE LASTKEY() = 27              && ESC
     ACTIVATE WINDOW GLQENTR2
    _CUROBJ = OBJNUM(pbEnClose)
    KEYBOARD "{ENTER}"
  CASE LASTKEY() = 9               && TAB
    IF WONTOP() = lcTitleEnt
      ACTIVATE WINDOW GLQENTR2
      _CUROBJ = OBJNUM(pbEnClose)
    ELSE
      IF _CUROBJ = OBJNUM(pbSJ)
        ACTIVATE WINDOW (lcTitleEnt)
      ELSE
        _CUROBJ = _CUROBJ + 1
      ENDIF
    ENDIF
  CASE LASTKEY() = 15              && SHIFT+TAB
    IF WONTOP() = lcTitleEnt
      ACTIVATE WINDOW GLQENTR1
      _CUROBJ = OBJNUM(pbSJ)
    ELSE
      IF _CUROBJ = OBJNUM(pbEnClose)
        ACTIVATE WINDOW (lcTitleEnt)
      ELSE
        _CUROBJ = _CUROBJ - 1
      ENDIF
    ENDIF
ENDCASE

*!**************************************************************************
*!
*!      Function: lfTabTrpDt
*!
*!**************************************************************************
*
*** The detail browse trap function. ***

FUNCTION lfTabTrpDt

*** Traping the tab key for the account screen ***

DO CASE
  CASE LASTKEY() = 27              && ESC
     ACTIVATE WINDOW GLQDETL2
    _CUROBJ = OBJNUM(pbDtClose)
    KEYBOARD "{ENTER}"
  CASE LASTKEY() = 9  .OR. LASTKEY() = 15
    IF WONTOP() = lcTitleDet
      ACTIVATE WINDOW GLQDETL2
      _CUROBJ = OBJNUM(pbDtClose)
    ELSE
      ACTIVATE WINDOW (lcTitleDet)
    ENDIF
ENDCASE

*!**************************************************************************
*!
*!      Function: lfPushKeys
*!
*!**************************************************************************
*
FUNCTION lfPushKeys

PUSH KEY
ON KEY LABEL ESC        DO lfTabTrpEn
ON KEY LABEL TAB        DO lfTabTrpEn
ON KEY LABEL BACKTAB    DO lfTabTrpEn
ON KEY LABEL CTRL+W     lnDumi = 1
ON KEY LABEL CTRL+Q     lnDumi = 1
ON KEY LABEL CTRL+END   lnDumi = 1

*!**************************************************************************
*!
*!      Function: lfKeyClean
*!
*!**************************************************************************
*
*** The cleaning function for all the trapped keys. ***

FUNCTION lfKeyClean

ON KEY LABEL ESC
ON KEY LABEL TAB
ON KEY LABEL BACKTAB
ON KEY LABEL CTRL+W
ON KEY LABEL CTRL+Q
ON KEY LABEL CTRL+END
