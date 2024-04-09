*:************************************************************************
*:
*: Procedure file: GLACSEG.PRG
*:
*:         System: ARIA BUSINESS SYSTEM
*:         Module: General Ledger
*:         Author: Reham Ali Alallami
*:      Copyright (c) 
*:  Last modified:  /  /
*:
*:  Procs & Fncts: 
*:               
*:      Documented 12/22/1993
*:************************************************************************
*E300683,5 AHMED 06/10/97 Add prgs directory path to the calling of programs
*E300692,1 ESSMAT 06/30/97. Change name and path of SYCACCOD, SYCFISHD, 
*E300692,1 					SYCFSPRD, SYCFSHLD
*E301077,71 Walid Abou El-Magd (WAM) 
*E301077,71 Open ACCOD file with gfOpenFile() as this functoin will be called
*E301077,71 many times .
*E300789,6 MAB 03/03/1999 Remove company Id from all indecies and SQLs. 
*B602311,1 KAM 09/11/2000 fix the bug to open all file at any conditions
************************************************************************
 
llPushOk   = .F.
llActBrow  = .T.
lcArrow    = SPACE(lnAcsSegSz)
lcArrow    = STUFF(lcArrow,1,1,"")
lcSavAls   = ALIAS()
lcSegWin   = ""
lcSegFilt  = ".T."
laCtrStat  = 'DISABLE' && To disable the browse pad in the menu
*E301077,71 [BEGIN]
=gfOpenFile(gcDataDir+'ACCOD','','SH')
*E301077,71 [END..]

*E300692,1 CHANGE FILE NAME FROM SYCACCOD TO ACCOD
*SELECT SYCACCOD
SELECT ACCOD
*E300692,1 end

*E300789,6 use file without order[Begin]
*SET ORDER TO COMPID
GO TOP
*E300789,6 use file without order[End  ]

STORE 1   TO lnCont  , lnStrtPos

*E300789,6 use file without order[Begin]
*IF SEEK(gcAct_Comp)
IF !EOF()
*E300789,6 use file without order[End  ]

  *E300692,1 CHANGE FILE NAME FROM SYCACCOD TO ACCOD
  *lnAryLen = SYCACCOD.NACSNOSEG
  lnAryLen = NACSNOSEG
  *E300692,1 end
  DECLARE laSegment[lnAryLen,3]
  SKIP

  *E300789,6 use file without order[Begin]
  *SCAN WHILE CCOMP_ID = gcAct_Comp
  SCAN REST
  *E300789,6 use file without order[End  ]

    laSegment [lnCont,1] = "Sg" + ALLTRIM(STR(lnCont))
    laSegment [lnCont,2] = lnStrtPos
  *E300692,1 CHANGE FILE NAME FROM SYCACCOD TO ACCOD
    *laSegment [lnCont,3] = SYCACCOD.NACSSIZE
    *lnStrtPos            = lnStrtPos + SYCACCOD.NACSSIZE + 1
    laSegment [lnCont,3] = ACCOD.NACSSIZE
    lnStrtPos            = lnStrtPos + ACCOD.NACSSIZE + 1
  *E300692,1 end
    lnCont               = lnCont + 1 
  ENDSCAN
ENDIF

lcAccCode   = lcAcsMask
FOR lnCount = 1 TO ALEN(laSegment,1)
  lcAccCode = stuff(lcAccCode,laSegment[lnCount,2],laSegment[lnCount,3],REPLICATE('0',laSegment[lnCount,3]))
ENDFOR

FOR lnCount = 1 TO ALEN(laSegment,1)

*  laSegment[lnCount,1] = IIF(EMPTY(laSegment[lnCount,1]),"Sg"+STR(lnCount,1),UPPER(ALLTRIM(laSegment[lnCount,1])))
  lcSegWin = lcSegWin + IIF(EMPTY(lcSegWin),'',',')+laSegment[lnCount,1]

  IF !USED(laSegment[lnCount,1])
	SELECT 0
	USE &gcDataDir.GLSEGVAL;
     	AGAIN ALIAS &laSegment[lnCount,1] ;
	    ORDER 1
  ENDIF

ENDFOR

lnCount = 1

PUSH KEY
ON KEY LABEL ESC        DO lfTabTrpSg
ON KEY LABEL TAB        DO lfTabTrpSg
ON KEY LABEL BACKTAB    DO lfTabTrpSg
ON KEY LABEL CTRL+W     lnDumi = 1
ON KEY LABEL CTRL+Q     lnDumi = 1
ON KEY LABEL CTRL+END   lnDumi = 1
ON KEY LABEL RIGHTARROW DO lfvSgNext
ON KEY LABEL LEFTARROW  DO lfvSgPrev

*E300683,5 Call *.SPR from screens directory
* DO GLACSEG.SPR 
DO (gcScrDir + gcWinAppl + '\GLACSEG.SPR')
*E300683,5 end   

RELEASE WINDOW awdglacseg

FOR lnCount = 1 TO ALEN(laSegment,1)
  IF USED(laSegment[lnCount,1])
	USE IN ALIAS(laSegment[lnCount,1])
  ENDIF
ENDFOR

=lfKeyClean()

IF llPushOk              && If pushing Ok button


  IF SEEK(lcAccCode,"GLACCHAR")

    llFoundAcn = .T.
 
    IF FSIZE(lcObj_Nam) > 0
      REPLACE &lcObj_Nam WITH GLACCHAR.cacctcode
    ELSE
      &lcObj_Nam  = GLACCHAR.cacctcode
    ENDIF  

    lcAccDes1  = GLACCHAR.caccnldes
    lcTypCode1 = GLACCHAR.ctypecode
    lcTypDesc1 = LOOKUP(GLTYPES.cTypedesc,lcTypCode1,GLTYPES.cTypecode,'Typecode')

  ELSE
    *** If the value not found in the chart of account file ***
    IF llAddAct1 .AND. glAddAct
      *** If adding new record is allowed in calling program ***
      *** Account {lcAccCode} not found     ***
      *** Browse - Add - Reenter ***

      lnOption = gfModalGen("QRM02025B02004","DIALOG",SUBSTR(lcAccCode,1,lnAcsSegSz))
      DO CASE
        CASE lnOption = 1                    && Browse Accounts
          *E300683,5 Call programs from PRGS directory
          *DO GLACBROW.PRG
          DO (gcAppHome + gcWinAppl + '\GLACBROW.PRG ')
          *E300683,5 end
        CASE lnOption = 2                    && Add account
           **B602311,1 KAM 09/11/2000 [Start]
           *llAddSeg = .F.
           llAddSeg = .T.
           **B602311,1 KAM 09/11/2000 [End]
          *E300683,5 Call programs from PRGS directory
          *DO GLACADD.PRG
           DO (gcAppHome + gcWinAppl + '\GLACADD.PRG ')
          *E300683,5 end
        CASE lnOption = 3                    && Reenter
          llCallRet  = .F.
      ENDCASE
    ELSE
      *** Account {lcAccCode} not found     ***
      *** Browse - Reenter ***

      lnOption = gfModalGen("QRM02025B02005","DIALOG",SUBSTR(lcAccCode,1,lnAcsSegSz))
      DO CASE
        CASE lnOption = 1                    && Browse Accounts
          *E300683,5 Call programs from PRGS directory
          *DO GLACBROW.PRG
          DO (gcAppHome + gcWinAppl + '\GLACBROW.PRG ')
          *E300683,5 end
        CASE lnOption = 2                    && Reenter
          llCallRet  = .F.
      ENDCASE
    ENDIF
  ENDIF
ENDIF
SELECT (lcSavAls)
 
*!**************************************************************************
*!
*!      Function: lfwSegBrow
*!
*!**************************************************************************
*
FUNCTION lfwSegBrow

FOR lnCount = ALEN(laSegment,1) TO 1 STEP -1

  SELECT (laSegment[lnCount,1])

  GO TOP
  IF SEEK(STR(lnCount,1))
    lcSegFld = ALLTRIM(EVALUATE(laSegment[lnCount,1]+".csegvalue"))
    lcAccCode =stuff(lcAccCode,laSegment[lnCount,2],laSegment[lnCount,3],lcSegFld)
  ENDIF

  IF lnCount = 1
    DO CASE
      CASE lcAccType1 = 'A'
        lcSegFilt = "CSTANDARD = 'Y' .OR. CSTANDARD = 'N'"
      CASE lcAccType1 = 'T'
        lcSegFilt = "CSTANDARD = 'Y'"
      CASE lcAccType1 = 'S'
        lcSegFilt = "CSTANDARD = 'N'"
    ENDCASE
  ELSE
    lcSegFilt = ".T."
  ENDIF

  BROWSE FIELDS CSEGVALUE:H='Value',CSEGLNDES:H='Description';
         NODELETE;
         NOEDIT;
         NOAPPEND;
         LOCK 0;
         SAVE;
         NOWAIT;
         KEY STR(lnCount,1);
         WHEN lfwSegment();
         TITLE laSegment[lnCount,1];
         WINDOW GLACSEG3 IN WINDOW AWDGLACSEG;
         FOR &lcSegFilt
  GO TOP
ENDFOR

lnCount = 1
SHOW GET lcAccCode
RETURN .T.

*!**************************************************************************
*!
*!      Function: lfwSegment
*!
*!**************************************************************************
*
FUNCTION lfwSegment

lcAccCode =stuff(lcAccCode,laSegment[lnCount,2],laSegment[lnCount,3],ALLTRIM(csegvalue))

SHOW GET lcAccCode

*!**************************************************************************
*!
*!      Function: lfSegPos
*!
*!**************************************************************************
*
FUNCTION lfSegPos

RETURN AT(ALLTRIM(cAcsshdes),lcAcsegDes)

*!**************************************************************************
*!
*!      Function: lfvSgPrev()
*!
*!**************************************************************************
*
FUNCTION lfvSgPrev

lnCount  = IIF(lnCount -1 < 1 ,ALEN(laSegment,1) , lnCount - 1 )  

lcArrow    = SPACE(lnAcsSegSz)

lcArrow =stuff(lcArrow,laSegment[lnCount,2],1,"")

SHOW GETS

ACTIVATE WINDOW (laSegment[lnCount,1]) TOP

*!**************************************************************************
*!
*!      Function: lfvSgNext
*!
*!**************************************************************************
*
FUNCTION lfvSgNext

lnCount  = IIF(lnCount +1 > ALEN(laSegment,1) ,1 , lnCount + 1 )

lcArrow    = SPACE(lnAcsSegSz)

lcArrow =stuff(lcArrow,laSegment[lnCount,2],1,"")

SHOW GETS

ACTIVATE WINDOW (laSegment[lnCount,1]) TOP

*!**************************************************************************
*!
*!      Function: lfvSgOk
*!
*!**************************************************************************
*
FUNCTION lfvSgOk

llPushOk = .T.         && Means pushing Ok

*!**************************************************************************
*!
*!      Function: lfvSgCanc
*!
*!**************************************************************************
*
FUNCTION lfvSgCanc


*** Valid function for the cancel button in the account screen ***

llPushOk = .F.          && Means pushing Cancel

llCallRet  = .F.

lcAccDes1  = ""
lcTypCode1 = ""
lcTypDesc1 = ""

*!**************************************************************************
*!
*!      Function: lfTabTrpSg
*!
*!**************************************************************************
*
FUNCTION lfTabTrpSg

*** Trabing the tab key for the segments screen ***

DO CASE
  CASE LASTKEY() = 27        && ESCAPE
    llPushOk = .F.
    ACTIVATE WINDOW GLACSEG4
    _CUROBJ = OBJNUM(pbSgCanc)
    KEYBOARD "{ENTER}"
  CASE LASTKEY() = 9         && TAB KEY
    IF WONTOP() = laSegment[lnCount,1]
      ACTIVATE WINDOW GLACSEG2
      _CUROBJ = OBJNUM(pbSgPrev)
    ELSE
      IF _CUROBJ = OBJNUM(pbSgCanc)
        ACTIVATE WINDOW (laSegment[lnCount,1])
      ELSE
       _CUROBJ = _CUROBJ + 1
      ENDIF
    ENDIF
  CASE LASTKEY() = 15         && BACKTAB KEY
    IF WONTOP() = laSegment[lnCount,1]
      ACTIVATE WINDOW GLACSEG4
      _CUROBJ = OBJNUM(pbSgCanc)
    ELSE
      IF _CUROBJ = OBJNUM(pbSgPrev)
        ACTIVATE WINDOW (laSegment[lnCount,1])
      ELSE
       _CUROBJ = _CUROBJ - 1
      ENDIF
    ENDIF
  
ENDCASE

*!**************************************************************************
*!
*!      Function: lfKeyClean
*!
*!**************************************************************************
*
FUNCTION lfKeyClean

ON KEY LABEL ESC
ON KEY LABEL TAB
ON KEY LABEL BACKTAB
ON KEY LABEL CTRL+W
ON KEY LABEL CTRL+Q
ON KEY LABEL CTRL+END
ON KEY LABEL RIGHTARROW
ON KEY LABEL LEFTARROW
