*!********************************************************************
*: Program file  : POSTYPTW.PRG
*: Program desc. : OpTIONAL PROGRAM TO GET STYLE AND PO NOTEPAD.
*:         System: ARIA APPAREL SERIES
*:         Module: PO
*:      Developer: Adel Mohammed El Gazzar (ADEL)
*:           Date: 13/06/2000
*!********************************************************************
*! Refer to (101850)
*!********************************************************************
*! Mofifications:
*! B803430,1 ADEL 07/24/2000 Fix some bugs and do enhancements in FRX.
*!********************************************************************


PRIVATE lnAlias
lnAlias = ALIAS()
IF llOgFltCh
  IF (llPrntBoth OR llRpPrtSn OR llRpPrtPn)
    IF !USED(lcCheckNote)
      lcCheckNote = gfTempName()
      CREATE TABLE (lcCheckNote) (KEY C(60),Title C(30),Notes M, POCHNG L,llOneNote L)
      INDEX ON KEY TAG (lcCheckNote)
    ELSE
      SELECT (lcCheckNote)
      ZAP
    ENDIF
    lcRec = ' '
    SELECT POSHDR
    SET SKIP TO
    SET SKIP TO POSLN
    SCAN FOR &lcRpExp
        SELECT POSLN
        SCAN WHILE cstytype+po+cwarecode+account+store+style+trancd = IIF(lcRpForm='A',POSHDR.cSTYType,lcRpForm)+POSHDR.PO
          lcPo = PO
          lcKey = cstytype+po+cwarecode+account+store+LEFT(style,lnMajSize)
          SCAN WHILE cstytype+po+cwarecode+account+store+style+trancd = lcKey AND trancd  = '1'
          ENDSCAN
          SKIP -1
          lcKeyVal = EVAL(KEY())
          lcStyMjr = STYLE.cStyMajor
          *--To make sure that we skipped all other trancd for the same style.
          SCAN WHILE cstytype+po+cwarecode+account+store+style+trancd = lcKey
          ENDSCAN
          llPoChng = PO <> lcPo
          =lfGetNot()
          SKIP-1
        ENDSCAN
    ENDSCAN
  ELSE
    IF USED(lcCheckNote) AND RECCOUNT(lcCheckNote) >0
      SELECT (lcCheckNote)
      ZAP
    ENDIF
  ENDIF
ENDIF  
IF USED(lcCheckNote) AND RECCOUNT(lcCheckNote) >0
  SELECT POSLN
  SET RELATION TO cstytype+po+cwarecode+account+store+style+trancd INTO (lcCheckNote) ADDI
  SELECT POSHDR
  lcSkipExpr = [POSLN,&lcCheckNote]
  SET SKIP TO &lcSkipExpr
ENDIF  
SELECT (lnAlias)

*!********************************************************************
*: Program file  : lfGetNot
*: Program desc. : To get notepad
*:         System: ARIA APPAREL SERIES
*:         Module: PO
*:      Developer: Adel Mohammed El Gazzar (ADEL)
*:           Date: 13/06/2000
*!********************************************************************
*! Refer to (101850)
*!********************************************************************
FUNCTION lfGetNot

PRIVATE lnAlias
lnAlias = ALIAS()
IF llrpPrtSn  AND SEEK('F'+lcStyMjr,'Notepad') .AND. !EMPTY(ALLTRIM(Notepad.MNotes))
   INSERT INTO   (lcCheckNote) (KEY,Title,Notes,POCHNG,llOneNote);
          VALUES (lcKeyVal,'Style Notepad',ALLTRIM(Notepad.MNotes),.F.,.T.)
ENDIF  
IF llrpPrtPn AND llPoChng AND SEEK('P' + POSHDR.PO , 'NOTEPAD') .AND. !EMPTY(ALLTRIM(NOTEPAD.mNotes))
   IF SEEK(SUBSTR(lcKeyVal,1,7),lcCheckNote)
     SELECT (lcCheckNote)
     LOCATE REST WHILE KEY = SUBSTR(lcKeyVal,1,7) FOR POCHNG
     IF FOUND()
       SELECT (lnAlias)
       RETURN
     ENDIF  
   ENDIF
   llNoteUpd  = .T.
   IF SEEK(lcKeyVal,lcCheckNote)
     REPLACE &lcCheckNote..llOneNote WITH .T. 
     llNoteUpd = .F.
   ENDIF
   INSERT INTO   (lcCheckNote) (KEY,Title,Notes,POCHNG,llOneNote );
          VALUES (lcKeyVal,'Purchase Order Notepad',ALLTRIM(Notepad.MNotes),.T.,llNoteUpd)
ENDIF
SELECT (lnAlias)