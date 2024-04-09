*:**************************************************************************
*: Program file  : ICCLGPSH.PRG
*: Program desc. : Custom "Group Sheet" Report for COL20
*: Date          : 03/03/2004
*: System        : Aria Advantage Series.
*: Module        : INVENTORY CONTROL (IC)
*: Developer     : TMI - TAREK MOHAMED IBRAHIM
*: Purpose       : To print a report with one group of styles at a time along with 
*:               : the style information including style pictures.
*: Entry #       : C# 037416,1 
*:**************************************************************************
*C037416,4 TMI 07/22/2004 Add a waiting window , Use pictures in the Pictures folder 
*C037416,6 TMI 08/09/2004 Get Pictures form custom file PictJPG , pictures are added to this file from \Pictures\ folder
*C127656,1 MMR 06/22/2005 Add 2 New options Print Price Blank,A,B,C 2nd Print Notes yes/No.
*:***************************************************************************

Private lcCollect ,lnClrPos,lnClrLen 

lcInitClct = gfTempName()
lcCollect = gfTempName()

lcMjrPct  = gfItemMask('PM')
lcNMjrPt  = gfItemMask('PN')
lnstylewid=LEN(lcMjrPct)
lncolorwid=LEN(lcNMjrPt)

STORE 0 TO lnClrPos,lnClrLen 
*- Get color position and lenght
=lfGtClr()

*- Create needed temp file
=lfCrtTmpFl()

*- Collect data
=lfCollect()

SELECT &lcCollect
LOCATE
IF !EOF()
  DO gfDispRe WITH gcRepHome + 'IC\ICCLGPSH'
ELSE
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'No records to display.')
ENDIF  

*- Close files and erase temp files
=lfErase()


*:**************************************************************************
*:* Name        : lfMajTtGet
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 03/08/2004
*:* Purpose     : Get Major Title
*:***************************************************************************
*:* Called from : OG
*:***************************************************************************
FUNCTION lfMajTtGet

RETURN gfItemMask("HM")

*:**************************************************************************
*:* Name        : lfMajPic
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 03/08/2004
*! Purpose      : get major segment picture
*:***************************************************************************
*:* Called from : OG
*:***************************************************************************
FUNCTION lfMajPic

lcMajPic = "@! " + gfItemMask("PM")

RETURN lcMajPic

*:**************************************************************************
*:* Name        : lfvStyle
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 03/08/2004
*! Purpose      : validate style
*:***************************************************************************
*:* Called from : OG
*:***************************************************************************
FUNCTION lfvStyle

lcStyle = VARREAD()

lcTag = ORDER('STYLE')

SET ORDER TO cStyle IN STYLE

*C037416,3  TMI [Start] Ask also about tab/BACKTAB key since style disappear when tabbing
*IF LASTKEY() = 13 AND !MDOWN()
lnstylewid=LEN(gfItemMask('PM'))
lcOgValueF = PADR(lcOgValueF,lnstylewid)
lcOgValueT = PADR(lcOgValueT,lnstylewid)
IF INLIST(LASTKEY(),9,13,15) AND !MDOWN()
  *C037416,3  TMI [End  ] 
  IF SEEK(&lcStyle.,'Style') 
    &lcStyle = STYLE.cStyMajor
  ELSE
    *C037416,3  TMI [Start] Check that &lcStyle is not empty to show the browse screen
    IF !EMPTY(&lcStyle.)
      *C037416,3  TMI [End  ] 
      &lcStyle = gfStyBrw('M',"","",.F.)
      *C037416,3  TMI [Start]        
    ENDIF
    *C037416,3  TMI [End  ] 
  ENDIF
ELSE
  &lcStyle = ''
ENDIF

SET ORDER TO lcTag IN STYLE
*:**************************************************************************
*:* Name        : lfGtClr
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 02/09/2004
*:* Purpose     : Get color data
*:***************************************************************************
FUNCTION lfGtClr
PRIVATE laItemSeg
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnClrLen = LEN(laItemSeg[lnCount,3])
    lnClrPos = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR
*-- End of lfGtClr.

*:**************************************************************************
*:* Name        : lfCrtTmpFl
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 02/09/2004
*:* Purpose     : Create temp files needed
*:***************************************************************************
FUNCTION lfCrtTmpFl
PRIVATE laFileStru,laIndx,lnI

************************ lcInitClct temp file
lnI = 0 
lnI = lnI + 1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'CPURCODE'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = lnI + 1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'SEASON'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = lnI + 1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'CSTYGROUP'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = lnI + 1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'STYLE'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 19
laFileStru[lnI,4] = 0

CREATE TABLE (gcWorkDir+lcInitClct) FROM ARRAY laFileStru
INDEX ON CSTYGROUP+CPURCODE+SEASON+STYLE TAG &lcInitClct

************************ lcCollect temp file
lnI = 0

lnI = lnI + 1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'STY1'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 19
laFileStru[lnI,4] = 0

lnI = lnI + 1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'STY2'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 19
laFileStru[lnI,4] = 0

lnI = lnI + 1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'STY3'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 19
laFileStru[lnI,4] = 0

lnI = lnI + 1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'CLR1'
laFileStru[lnI,2] = 'M'
laFileStru[lnI,3] = 10
laFileStru[lnI,4] = 0

lnI = lnI + 1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'CLR2'
laFileStru[lnI,2] = 'M'
laFileStru[lnI,3] = 10
laFileStru[lnI,4] = 0

lnI = lnI + 1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'CLR3'
laFileStru[lnI,2] = 'M'
laFileStru[lnI,3] = 10
laFileStru[lnI,4] = 0

lnI = lnI + 1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'PRICEA_1'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 13
laFileStru[lnI,4] = 0

lnI = lnI + 1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'PRICEA_2'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 13
laFileStru[lnI,4] = 0

lnI = lnI + 1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'PRICEA_3'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 13
laFileStru[lnI,4] = 0

lnI = lnI + 1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'DESC_1'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 20
laFileStru[lnI,4] = 0

lnI = lnI + 1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'DESC_2'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 20
laFileStru[lnI,4] = 0

lnI = lnI + 1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'DESC_3'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 20
laFileStru[lnI,4] = 0

lnI = lnI + 1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'DESC1_1'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 60
laFileStru[lnI,4] = 0

lnI = lnI + 1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'DESC1_2'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 60
laFileStru[lnI,4] = 0

lnI = lnI + 1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'DESC1_3'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 60
laFileStru[lnI,4] = 0

lnI = lnI + 1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'NOTES1'
laFileStru[lnI,2] = 'M'
laFileStru[lnI,3] = 10
laFileStru[lnI,4] = 0

lnI = lnI + 1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'NOTES2'
laFileStru[lnI,2] = 'M'
laFileStru[lnI,3] = 10
laFileStru[lnI,4] = 0

lnI = lnI + 1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'NOTES3'
laFileStru[lnI,2] = 'M'
laFileStru[lnI,3] = 10
laFileStru[lnI,4] = 0

lnI = lnI + 1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'CPURCODE'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = lnI + 1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'SEASON'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = lnI + 1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'CSTYGROUP'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = lnI + 1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'PAGE'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 3
laFileStru[lnI,4] = 0

lnI = lnI + 1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'PAGES'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 3
laFileStru[lnI,4] = 0

*C037416,6  TMI [Start] no need for these fields , we will use PictJPG file instead
*/* *C037416,4  TMI [Start] Add three fields for Picture
*/* lnI = lnI + 1
*/* DIMENSION laFileStru[lnI,4]
*/* laFileStru[lnI,1] = 'gPicture1'
*/* laFileStru[lnI,2] = 'G'
*/* laFileStru[lnI,3] = 10
*/* laFileStru[lnI,4] = 0
*/* 
*/* lnI = lnI + 1
*/* DIMENSION laFileStru[lnI,4]
*/* laFileStru[lnI,1] = 'gPicture2'
*/* laFileStru[lnI,2] = 'G'
*/* laFileStru[lnI,3] = 10
*/* laFileStru[lnI,4] = 0
*/* 
*/* lnI = lnI + 1
*/* DIMENSION laFileStru[lnI,4]
*/* laFileStru[lnI,1] = 'gPicture3'
*/* laFileStru[lnI,2] = 'G'
*/* laFileStru[lnI,3] = 10
*/* laFileStru[lnI,4] = 0
*/* *C037416,4  TMI [End  ] 
*C037416,6  TMI [End  ] 

CREATE TABLE (gcWorkDir+lcCollect) FROM ARRAY laFileStru
INDEX ON CSTYGROUP+CPURCODE+SEASON TAG CSTYGROUP

*-- end of lfCrtTmpFl.

*:**************************************************************************
*:* Name        : lfCollect
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 03/07/2004
*:* Purpose     : Collect function
*:***************************************************************************
FUNCTION lfCollect

*C037416,5  TMI [Start] set order to STYLE in style file to get all colours
SET ORDER TO STYLE    IN STYLE
*C037416,5  TMI [End  ] 

*C037416,6  TMI [Start] Check the existance of the file PictJPG , if not found create it,open it
*/                      This file stores an embedded bmps for styles , it is appended from 
*/                      Pictures folder for the company
IF !FILE(gcDataDir+'PictJPG.DBF')
  CREATE TABLE (gcDataDir+'PictJPG.DBF') (CSTYMAJOR C(19),JPG G)
  INDEX ON CSTYMAJOR TAG PictJPG
  USE IN PictJPG
ENDIF
*- Open pictjpg file three times to show pictures in the three colums of the report
=gfOpenFile(gcDataDir+'PictJPG','PictJPG','SH')
*C037416,6  TMI [End  ] 

SELECT STYLE
LOCATE
SCAN FOR &lcRpExp 
  INSERT INTO (gcWorkDir+lcInitClct) VALUES ;
              (STYLE.CPURCODE,STYLE.SEASON,STYLE.CSTYGROUP,STYLE.STYLE)
  *C037416,4  TMI [Start] Add a waiting window to inform user data is collected
  WAIT WINDOW NOWAIT STYLE.STYLE
  *C037416,4  TMI [End  ] 
ENDSCAN

*-Open picture file
=gfOpenFile(gcDef_Path+'ICCOL20','','SH')

*- Set appropriate orders
SET ORDER TO STYLE    IN STYLE
SET ORDER TO SCALE    IN SCALE
SET ORDER TO CODES    IN CODES
SET ORDER TO OBJECTID IN OBJECTS   
SET ORDER TO OBJLNKTY IN OBJLINK
SET ORDER TO NOTEPAD  IN NOTEPAD

*- Set these needed relations
SELECT STYLE
SET RELATION TO 'F'+CSTYMAJOR INTO NOTEPAD
LOCATE

SELECT &lcInitClct
SET RELATION TO STYLE INTO STYLE
LOCATE

DO WHILE !EOF(lcInitClct)
  m.CSTYGROUP = &lcInitClct..CSTYGROUP
  m.CPURCODE  = &lcInitClct..CPURCODE  
  m.SEASON    = &lcInitClct..SEASON   
  m.CSTYMAJOR = SPACE(lnstylewid)
  lnPg = 1      && Hold page# , each page contains 9 styles
  lnLn = 1      && Line # each line contains 3 styles
  lnNr = 0      && style #  within the line
  SELECT &lcInitClct
  DO WHILE CSTYGROUP+CPURCODE+SEASON+STYLE = m.CSTYGROUP+m.CPURCODE+m.SEASON
    
    *C037416,4  TMI [Start] waiting window 
    WAIT WINDOW NOWAIT STYLE
    *C037416,4  TMI [End  ] 
    
    llContinue = ( m.CSTYMAJOR = SUBSTR(&lcInitClct..STYLE,1,lnstylewid) )
    m.CSTYMAJOR = SUBSTR(&lcInitClct..STYLE,1,lnstylewid)
        
    lnNr = lnNr + 1
    && if more than 3 styles in the line skip to the next line
    IF lnNr > 3  
      lnNr = 1
      lnLn = lnLn + 1
      && if more than 3 lines in the page skip to the next page
      IF lnLn > 3  
        lnLn = 1
        lnPg = lnPg + 1
      ENDIF
      
    ENDIF
    && Add a line for each 3 styles
    IF lnNr = 1    
      SELECT &lcCollect
      APPEND BLANK
      REPLACE PAGE      WITH lnPg ;
              CSTYGROUP WITH M.CSTYGROUP ;
              CPURCODE  WITH M.CPURCODE ;
              SEASON    WITH M.SEASON 
    ENDIF
    SELECT &lcCollect    
    && Update main style data
    lcNr = STR(lnNr,1)
*C127656,1 MMR 06/22/2005 Add 2 New options Print Price Blank,A,B,C 2nd Print Notes yes/No.
    *REPLACE STY&lcNr     WITH m.CSTYMAJOR ;
            *DESC_&lcNr   WITH IIF(llContinue,'          Continued',STYLE.DESC)  ;
            *DESC1_&lcNr  WITH IIF(llContinue,'          Continued',STYLE.DESC1) ;
            *PRICEA_&lcNr WITH '$'+STR(STYLE.PRICEA,12,2) ;
            *NOTES&lcNr   WITH NOTEPAD.MNOTES
    REPLACE STY&lcNr     WITH m.CSTYMAJOR ;
            DESC_&lcNr   WITH IIF(llContinue,'          Continued',STYLE.DESC)  ;
            DESC1_&lcNr  WITH IIF(llContinue,'          Continued',STYLE.DESC1) 
DO CASE
  CASE lcRPPrice='A'
    REPLACE PRICEA_&lcNr WITH '$'+STR(STYLE.PRICEA,12,2) 
  CASE lcRPPrice='B'
    REPLACE PRICEA_&lcNr WITH '$'+STR(STYLE.PRICEB,12,2) 
  CASE lcRPPrice='C'
    REPLACE PRICEA_&lcNr WITH '$'+STR(STYLE.PRICEC,12,2) 
  OTHERWISE   
    REPLACE PRICEA_&lcNr WITH ''
ENDCASE
IF llRpInvNot
    REPLACE NOTES&lcNr   WITH NOTEPAD.MNOTES
ELSE
    REPLACE NOTES&lcNr   WITH ''
ENDIF
*C127656,1 MMR [END]        
    *C037416,6  TMI [Start] Embed the picture to the custom file PictJPG    
    *C037416,7  TMI [Start] check first existance of jpg file, for first time append a record for this jpg in pictjpg file, else update the link
    *IF !SEEK(STY&lcNr,'PICTJPG')
    *C037416,7  TMI [End  ] 
    lcPict = gcDef_Path+'Pictures\'+gcAct_Comp+'\'+LEFT(SEASON,3)+'\'+ALLTRIM(SUBSTR(STY&lcNr,4))+'.JPG'
    IF FILE(lcPict)        
      *C037416,7  TMI [Start] 
      *lcBmpFl = gcWorkDir+ALLTRIM(SUBSTR(STY&lcNr,4))+'.BMP'
      *COPY FILE (lcPict) TO (lcBmpFl)
      lcBmpFl = gcDef_Path+'Pictures\'+gcAct_Comp+'\'+LEFT(SEASON,3)+'\'+ALLTRIM(SUBSTR(STY&lcNr,4))+'.BMP'
      ERASE (lcBmpFl)
      RENAME (lcPict) TO (lcBmpFl)
      IF !SEEK(STY&lcNr,'PICTJPG')
        *C037416,7  TMI [End  ]       
        SELECT PICTJPG
        APPEND BLANK
        REPLACE CSTYMAJOR WITH &lcCollect..STY&lcNr
        *C037416,7  TMI [Start] Comment these two lines
        *APPEND GENERAL JPG FROM (lcBmpFl)
        *ERASE (lcBmpFl)
        *C037416,7  TMI [End  ] 
      ENDIF
      *C037416,7  TMI [Start] link rather than embed to preserve memory
      SELECT PICTJPG
      APPEND GENERAL JPG FROM (lcBmpFl) LINK
      *C037416,7  TMI [End  ] 
    ENDIF
    *C037416,6  TMI [End  ] 
    
    && Update colours
    SELECT &lcInitClct
    lnClrCnt = 0
    SCAN REST WHILE CSTYGROUP+CPURCODE+SEASON+STYLE = m.CSTYGROUP+m.CPURCODE+m.SEASON+m.CSTYMAJOR
      lcClr = PADC(ALLTRIM(SUBSTR(&lcInitClct..STYLE,lnClrPos,lnClrLen)),lnClrLen)
      REPLACE &lcCollect..CLR&lcNr WITH lcClr+CHR(13) ADDITIVE
      lnClrCnt = lnClrCnt + 1
      IF lnClrCnt = 9  && No more than 9 colours in each section of the report
        SKIP
        EXIT
      ENDIF
    ENDSCAN  
  ENDDO
  
  SELECT &lcCollect
  && if less than 3 lines in the page add records so that each page has exactly 3 lines 
  DO WHILE lnLn < 3
    APPEND BLANK
    REPLACE PAGE      WITH lnPg ;
            CSTYGROUP WITH M.CSTYGROUP ;
            CPURCODE  WITH M.CPURCODE ;
            SEASON    WITH M.SEASON 
    lnLn = lnLn + 1
  ENDDO  
  
  *- Update total# of pages with the last page # reached
  REPLACE PAGES WITH lnPg FOR EMPTY(PAGES)
  
ENDDO

*C037416,6  TMI [Start] no need to open OBJECTS or OBJLINK files, we will use PICTJPG instead
*/* *-Open "OBJECTS" & "OBJLINK" three times to be used to show pictures of the three styles in each line
*/* IF !USED('OBJECTS2')
*/*   USE (gcDataDir+'OBJECTS') AGAIN ALIAS OBJECTS2 ORDER OBJECTID SHARE IN 0
*/* ENDIF
*/* IF !USED('OBJECTS3')
*/*   USE (gcDataDir+'OBJECTS') AGAIN ALIAS OBJECTS3 ORDER OBJECTID SHARE IN 0
*/* ENDIF
*/* 
*/* IF !USED('OBJLINK2')
*/*   USE (gcDataDir+'OBJLINK') AGAIN ALIAS OBJLINK2 ORDER OBJLNKTY SHARE IN 0
*/* ENDIF
*/* IF !USED('OBJLINK3')
*/*   USE (gcDataDir+'OBJLINK') AGAIN ALIAS OBJLINK3 ORDER OBJLNKTY SHARE IN 0
*/* ENDIF
*C037416,6  TMI [End  ] 

*C037416,4  TMI [Start] No need for these relation , sice as per COl20, they will put all pictures in the folder \Pictures
*C037416,4              we will embed each picture in its related field in the temp file lcCollect
*-* SELECT &lcCollect
*-* SET RELATION TO 'S'+STY1 INTO OBJLINK  ADDITIVE
*-* SET RELATION TO 'S'+STY2 INTO OBJLINK2 ADDITIVE
*-* SET RELATION TO 'S'+STY3 INTO OBJLINK3 ADDITIVE
*-* 
*-* *-Set relations for picture showing
*-* SELECT OBJLINK
*-* SET RELATION TO COBJECT_ID INTO OBJECTS
*-* SELECT OBJLINK2
*-* SET RELATION TO COBJECT_ID INTO OBJECTS2
*-* SELECT OBJLINK3
*-* SET RELATION TO COBJECT_ID INTO OBJECTS3
*C037416,4  TMI [End  ] 

*C037416,6  TMI [Start] Be sure that PictJPG file is opened three times and Set relations to show pictures
IF !USED('PICTJPG2')
  USE (gcDataDir+'PICTJPG') AGAIN ALIAS PICTJPG2 ORDER PICTJPG SHARE IN 0
ENDIF  
IF !USED('PICTJPG3')
  USE (gcDataDir+'PICTJPG') AGAIN ALIAS PICTJPG3 ORDER PICTJPG SHARE IN 0
ENDIF  

SELECT &lcCollect
SET RELATION TO STY1 INTO PICTJPG  ADDITIVE
SET RELATION TO STY2 INTO PICTJPG2 ADDITIVE
SET RELATION TO STY3 INTO PICTJPG3 ADDITIVE
*C037416,6  TMI [End  ] 

*-Rlease relation of style file
SELECT STYLE
SET RELATION TO

*-- end of lfCollect.

*:**************************************************************************
*:* Name        : lfErase
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 03/04/2004
*:* Purpose     : *- Close temp created file
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfErase()
*:***************************************************************************
FUNCTION lfErase

*-Close files
IF USED('OBJECTS2')
  USE IN OBJECTS2
ENDIF
IF USED('OBJECTS3')
  USE IN OBJECTS3
ENDIF
IF USED('OBJLINK2')
  USE IN OBJLINK2
ENDIF  
IF USED('OBJLINK3')
  USE IN OBJLINK3
ENDIF  

*-Erase temp files
IF USED(lcCollect)
  USE IN &lcCollect
ENDIF  
ERASE (gcWorkDir+lcCollect+'.DBF')
ERASE (gcWorkDir+lcCollect+'.CDX')

IF USED(lcInitClct)
  USE IN &lcInitClct
ENDIF  
ERASE (gcWorkDir+lcInitClct+'.DBF')
ERASE (gcWorkDir+lcInitClct+'.CDX')
  
*-- end of lfErase.


*:**************************************************************************
*:* Name        : lfStySum
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 07/22/2004
*! Purpose   : sum a specific field for the current style in style file
*:***************************************************************************
FUNCTION lfStySum
PARAMETERS lcSty,lccomp,lnAddToVar
PRIVATE lnStyRec
lnStyRec = IIF(RECNO('STYLE') <= RECCOUNT('STYLE'),RECNO('STYLE'),1)
lnTotcomp = 0
SELECT Style_X
SUM &lcCOMP TO lnTotcomp WHILE Style = ALLTRIM(lcSty)
SELECT Style
GO lnStyRec
DO CASE
  CASE lnAddToVar = 1
  	lnO_T_S = lnTotcomp
  CASE lnAddToVar = 2
  	lnO_T_S = lnO_T_S + lnTotcomp
  CASE lnAddToVar = 3
  	lnO_T_S = lnO_T_S - lnTotcomp
ENDCASE
RETURN INT(lnTotcomp)
*-- end of lfStySum.

*:**************************************************************************
*:* Name        : lfSRVSty
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 07/22/2004
*:* Purpose     : Rise change style flag, in range browse screen.
*:***************************************************************************
FUNCTION lfSRVSty
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'  && Set code
    *-- open this file in another alias to set order to Style Major 
    *-- unique index.
    USE (gcDataDir+'Style') AGAIN ALIAS STYLE_X ORDER TAG Style IN 0
    SELECT STYLE
    SET ORDER TO TAG Cstyle
    SET RELATION TO STYLE.STYLE INTO STYLE_X
    GO TOP IN STYLE
    llChStyle = .T.
  CASE lcParm = 'R'  && Reset code
    USE IN STYLE_X
    SELECT STYLE
    SET ORDER TO TAG STYLE
ENDCASE
*-- end of lfsrvSty.

*:**************************************************************************
*:* Name        : lfAddJPG
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/09/2004
*:* Purpose     : Add JPG picture to the file PictJPG
*:***************************************************************************
*C122756,5 
FUNCTION lfAddJPG
PRIVATE lnSlct,lcPict,lcBmpFl
lnSlct = SELECT()

  lcPict = gcDef_Path+'Pictures\'+gcAct_Comp+'\'+LEFT(STYLE.STYLE,3)+'\'+ALLTRIM(SUBSTR(STYLE.STYLE,4))+'.JPG'
  IF FILE(lcPict)
    lcBmpFl = gcWorkDir+ALLTRIM(SUBSTR(STYLE.STYLE,4))+'.BMP'
    COPY FILE (lcPict) TO (lcBmpFl)
    INSERT INTO PICTJPG (CSTYMAJOR) VALUES (STYLE.CSTYMAJOR)
    SELECT PICTJPG
    APPEND GENERAL JPG FROM (lcBmpFl)
    ERASE (lcBmpFl)
  ENDIF

SELECT (lnSlct)
*-- end of lfAddJPG.

