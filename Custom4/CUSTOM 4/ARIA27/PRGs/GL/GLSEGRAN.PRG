*:************************************************************************
*:
*: Procedure file: GLSEGRAN.PRG
*:
*:         System: ARIA BUSINESS SYSTEM
*:         Module: General Ledger
*:         Author: Yasser Saad Ibrahim
*:      Copyright (c) 
*:  Last modified:  /  /
*:
*:  Procs & Fncts: 
*:               
*:      Documented 12/22/1993
*:************************************************************************
*E300683,5 AHMED 06/10/97 Add prgs directory path to the calling of programs
*

DECLARE laObjdisp[6]


lnMaxSiz   = 1
laObjdisp  = .F.

FOR lnCount = 1 TO lnSegNo
  lnMaxSiz = MAX(lnMaxSiz,laSegSiz[lnCount,1])
  laObjdisp[lnCount] = .T. 
ENDFOR  

lnHight  = (lnSegNo*3)+4
lnWidth  = (lnMaxSiz*3)+32

SELECT GLSEGVAL


FOR lnCount = 1 TO lnSegNo 
  SELECT (laSegSiz[lnCount,3])
  lcPop1   = "puSegRng"+STR(lnCount,1)+"1"
  lcPop2   = "puSegRng"+STR(lnCount,1)+"2"
  lcAlFile = "&laSegSiz[lnCount,3].csegvalue"

  DEFINE POPUP &lcPop1 PROMPT FIELD &lcAlFile MARK "" MARGIN 
  DEFINE POPUP &lcPop2 PROMPT FIELD &lcAlFile MARK "" MARGIN 
ENDFOR
  
*E300683,5 Call *.SPR from screens directory
* DO GLSEGRAN.SPR 
DO (gcScrDir + gcWinAppl + '\GLSEGRAN.SPR')
*E300683,5 end   



*!**************************************************************************
*!
*!      Function: lfwSegRng
*!
*!**************************************************************************
*
FUNCTION lfwSegRng

lnAliasNum = VAL(SUBSTR(SYS(18),9,1))
lcAliasFl2 = "&laSegsiz[lnAliasNum,3].cSeglnDes"
lcSeglndes = EVALUATE(lcAliasFl2)

SHOW GET lcSeglndes DISABLE

*IF EMPTY(lsSegRng11)
*  lsSegRng11 = ALLTRIM(Act.cSegValue)
*  SHOW GET lsSegRng11  
*ENDIF


*!**************************************************************************
*!
*!      Function: lfvExclud
*!
*!**************************************************************************
*
FUNCTION lfvExclud
DECLARE laSource[1]

lcObjNam   = SYS(18)
lnAliasNum = VAL(SUBSTR(lcObjNam,9,1))
lcFromSeg  = "lsSegRng"+STR(lnAliasNum,1)+"1"
lcTOSeg    = "lsSegRng"+STR(lnAliasNum,1)+"2"
lcExcAry   = "laExcAry"+STR(lnAliasNum,1)

SELECT RTRIM(cSegValue) FROM &laSegsiz[lnAliasNum,3];
       WHERE cAcsSegNo = STR(lnAliasNum,1)   .AND.;
             cSegValue>= EVALUATE(lcFromSeg) .AND.;
             cSegValue<= EVALUATE(lcTOSeg);
       ORDER BY  cSegValue ;
       INTO ARRAY laSource   

lnAryCount = 1

FOR lnCount = 1 TO ALEN(&lcExcAry,1)
  lnElement = ASCAN(laSource,&lcExcAry[lnAryCount])
  IF lnElement = 0
    =ADEL(&lcExcAry,lnAryCount)
    IF ALEN(&lcExcAry,1) > 1    
      DIMENSION &lcExcAry[ALEN(&lcExcAry,1)-1]
    ENDIF  
  ELSE
    lnAryCount = lnAryCount + 1
  ENDIF
ENDFOR

=gfMover(@laSource,@&lcExcAry)

RELEASE laSource
