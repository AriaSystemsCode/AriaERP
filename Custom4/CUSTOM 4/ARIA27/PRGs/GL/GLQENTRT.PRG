
*E300683,5 AHMED 06/10/97 Add prgs directory path to the calling of programs
PARAMETERS puCompYrs , puYerPer , lnFrstElm , lnElmNo
PRIVATE    puCompYrs , puYerPer , lnFrstElm , lnElmNo

DECLARE laAppl    [1,1]    && Hold Applications ID

lcEntTitl  = "Transactions details"
laAppl[1]  = LOOKUP(SYDAPPL.cApp_name,gcAct_Appl,SYDAPPL.cApp_id,'cApp_id')
lcAppIdStr = gcAct_Appl

=lfgetQuery()

*E300683,5 Call *.SPR from screens directory
* DO GLQENTR.SPR 
DO (gcScrDir + gcWinAppl + '\GLQENTR.SPR')
*E300683,5 end   
*!**************************************************************************
*!
*!       Function: lfgetQuery
*!
*!**************************************************************************
*
FUNCTION lfgetQuery

SELECT GLTRNSDT.NBATCHNO, GLTRNSDT.NTRNUMBER, GLTRNSHD.CTRNJOUR,;
       GLTRNSHD.DTRNPODAT, GLTRNSDT.CTRDTEXP,;
       IIF(GLTRNSDT.LTRDDEBIT,' '+STR(GLTRNSDT.NTRDAMNT,15,2)+' ',;
       '('+STR(GLTRNSDT.NTRDAMNT,15,2)+')') AS 'Deb_Crd';
       FROM  GLTRNSHD, GLTRNSDT;
       INTO CURSOR (lcCursor1);
       WHERE GLTRNSDT.NTRNUMBER  = GLTRNSHD.NTRNUMBER .AND.;
             (GLTRNSHD.CTRNPOPRD = puYerPer   .AND.;
              GLTRNSHD.CTRNPOYAR = puCompYrs  .AND.;
              GLTRNSDT.CACCTCODE = laData[1]  .AND.;
              GLTRNSHD.CTRNJOUR  $ lcAppIdStr .AND.;
              GLTRNSHD.CTRNSTAT  = 'P' );
       ORDER BY GLTRNSDT.NBATCHNO, GLTRNSDT.NTRNUMBER


lcWid1 = STR(MAX(LEN('Batch#'),FSIZE('NBATCHNO')))
lcWid2 = STR(MAX(LEN('Trans#'),FSIZE('NTRNUMBER')))
lcWid3 = STR(MAX(LEN('SJ'),FSIZE('CTRNJOUR')))
lcWid4 = STR(MAX(LEN('Date'),FSIZE('DTRNPODAT')))
lcWid5 = STR(MAX(LEN('Debit/(Credit)'),FSIZE('Deb_Crd')))

lcBrowEntr = "NBATCHNO :H='Batch#':&lcWid1:P='@K',NTRNUMBER"+;
             ":H='Trans#':&lcWid2 :P='@K',CTRNJOUR :H='SJ':&lcWid3:P='@K',DTRNPODAT "+;
             ":H='Date':&lcWid4:P='@K',cdummi1=SUBSTR(CTRDTEXP,1,25):H='Comment':25:P='@K',"+;
             "Deb_Crd :H='Debit/(Credit)':&lcWid5:P='@K'"


*!**************************************************************************
*!
*!      Function: lfwCompYrs
*!
*!**************************************************************************
*
FUNCTION lfwCompYrs

lcOldYear = puCompYrs

*!**************************************************************************
*!
*!      Function: lfvCompYrs
*!
*!**************************************************************************
*
FUNCTION lfvCompYrs

IF lcOldYear <> puCompYrs
  DO CASE 
    CASE puCompYrs = ALLTRIM(STR(lnCurr_yer-1))
      lnFrstElm    = 1
      lnElmNo      = VAL(laPrdNo[1])
    CASE puCompYrs = ALLTRIM(STR(lnCurr_yer  ))
      lnFrstElm    = VAL(laPrdNo[1]) + 1
      lnElmNo      = VAL(laPrdNo[2])
    CASE puCompYrs = ALLTRIM(STR(lnCurr_yer+1))
      lnFrstElm    = VAL(laPrdNo[1]) + VAL(laPrdNo[2]) + 1
      lnElmNo      = VAL(laPrdNo[3])
  ENDCASE

  puYerPer = '01'

  SHOW GET puYerPer

  =lfgetQuery()
  
  _CUROBJ = OBJNUM(ibBrowEnt)
ENDIF

*!**************************************************************************
*!
*!      Function: lfwYerPer
*!
*!**************************************************************************
*
FUNCTION lfwYerPer

lcOldPer = puYerPer

*!**************************************************************************
*!
*!      Function: lfvYerPer
*!
*!**************************************************************************
*
FUNCTION lfvYerPer

IF lcOldPer <> puYerPer
  =lfgetQuery()
  _CUROBJ = OBJNUM(ibBrowEnt)
ENDIF

*!**************************************************************************
*!
*!      Function: lfvSJ
*!
*!**************************************************************************
*
FUNCTION lfvSJ

POP KEY

=gfMover(@laAppDesc,@laAppl)

lcAppIdStr   = ""

IF !EMPTY(laAppl[1])
  FOR lnCount  = 1 TO ALEN(laAppl,1)
    lcAppIdStr = lcAppIdStr + IIF(EMPTY(lcAppIdStr) , "" , "," );
                 + ALLTRIM(laAppID[ASCAN(laAppDesc,laAppl[lnCount])])
  ENDFOR
ENDIF

=lfgetQuery()

_CUROBJ = OBJNUM(ibBrowEnt)

*!**************************************************************************
*!
*!      FUNCTION lfDoBrowEn
*!
*!**************************************************************************
*
FUNCTION lfDoBrowEn

PUSH KEY
ON KEY LABEL ESC        DO gfKeyTrap               && ESCAPE
ON KEY LABEL TAB        DO gfKeyTrap               && TAB
ON KEY LABEL BACKTAB    DO gfKeyTrap               && SHIFT+TAB
ON KEY LABEL CTRL+W     lnDumi = 1
ON KEY LABEL CTRL+Q     lnDumi = 1
ON KEY LABEL CTRL+END   lnDumi = 1
ON KEY LABEL RIGHTARROW KEYBOARD "{END}"+"{END}"    && RIGHT ARROW
ON KEY LABEL LEFTARROW  KEYBOARD "{HOME}"+"{HOME}"  && LEFT ARROW
ON KEY LABEL LEFTMOUSE  DO gfMousTrap WITH 'awbGlentdt','laObjCord2','awdGLQentr'

BROWSE   FIELDS &lcBrowEntr;
         NOAPPEND ;
         NODELETE ;
         NOEDIT   ;
         NOMENU   ;
         NOCLEAR  ;
         LOCK 0   ;
         TITLE "Entries for account "+ALLTRIM(laData[1]);
         WHEN gfDclick('lfvDetail',.T.);
         WINDOW awbGlentdt IN WINDOW awdGLQentr


*!**************************************************************************
*!**************************************************************************
*!**************************************************************************
*!**************************************************************************
*!
*!      Function: lfvDetail
*!
*!**************************************************************************
*
FUNCTION lfvDetail

POP KEY

lcSaveDtl = ALIAS()

lnBatchNo  = NBATCHNO
lnTransNo  = NTRNUMBER
lcTypCode2 = lcTypCode

lnTotDeb   = 0
lnTotCrd   = 0
lnTotal    = 0

SELECT GLTRNSDT.CACCTCODE ,;
       GLACCHAR.CACCNLDES ,;
       IIF( lTrddebit,STR(NTRDAMNT,15,2),STR(0,15,2)) AS 'DEBIT' ,;
       IIF(!lTrddebit,STR(NTRDAMNT,15,2),STR(0,15,2)) AS 'CREDIT' ;
       INTO CURSOR (lcCursor2);
       WHERE GLTRNSDT.CACCTCODE = GLACCHAR.CACCTCODE;
       AND GLTRNSDT.NBATCHNO = lnBatchNo;
       AND GLTRNSDT.NTRNUMBER = lnTransNo;
       FROM GLTRNSDT,GLACCHAR

lcWidD1 = STR(MAX(LEN('DEBIT'),FSIZE('DEBIT')))
lcWidD2 = STR(MAX(LEN('Credit'),FSIZE('CREDIT')))

lcBrowDet = "CACCTCODE:H=lcAcSegDes:&lcAccLen:P='@K',"+;
            "CACCNLDES:H='Description':&lcDesLen:P='@K',"+;
            "DEBIT:H='Debit':&lcWidD1:P='@K',CREDIT:H='Credit':&lcWidD2:P='@K'"

SUM VAL(DEBIT),VAL(CREDIT) TO lnTotDeb,lnTotCrd

lnTotal = MAX(lnTotDeb,lnTotCrd)

lcAccLen = ALLTRIM(STR(lnAcsSegSz))
lcDesLen = ALLTRIM(STR(40-lnAcsSegSz))

*E300683,5 Call *.SPR from screens directory
* DO GLQDETL.SPR 
DO (gcScrDir + gcWinAppl + '\GLQDETL.SPR')
*E300683,5 end   
_CUROBJ = OBJNUM(ibBrowEnt)

SELECT (lcSaveDtl)

*!**************************************************************************
*!
*!      FUNCTION lfDoBrowDt
*!
*!**************************************************************************
*
FUNCTION lfDoBrowDt

PUSH KEY
ON KEY LABEL ESC        DO gfKeyTrap               && ESCAPE
ON KEY LABEL TAB        DO gfKeyTrap               && TAB
ON KEY LABEL BACKTAB    DO gfKeyTrap               && SHIFT+TAB
ON KEY LABEL CTRL+W     lnDumi = 1
ON KEY LABEL CTRL+Q     lnDumi = 1
ON KEY LABEL CTRL+END   lnDumi = 1
ON KEY LABEL RIGHTARROW KEYBOARD "{END}"+"{END}"    && RIGHT ARROW
ON KEY LABEL LEFTARROW  KEYBOARD "{HOME}"+"{HOME}"  && LEFT ARROW
ON KEY LABEL LEFTMOUSE  DO gfMousTrap WITH 'awbtrBatch','laObjCord3','awdGLQdetl'

BROWSE   FIELDS &lcBrowDet;
         NOAPPEND ;
         NODELETE ;
         NOEDIT   ;
         NOMENU   ;
         NOCLEAR  ;
         LOCK 0   ;
         TITLE "Transaction Details";
         WINDOW awbtrBatch IN WINDOW awdGLQdetl

