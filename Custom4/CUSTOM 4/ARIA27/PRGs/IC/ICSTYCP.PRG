**********************************************************************************************
* PROGRAM               : ICSTYCP.PRG
* APPLICATION           : ARIA27 
* MODULE                : IC2
* PURPOR                : To copy multiple styles as a bulk creating new styles
* DEVELOPER             : TAREK MOHAMED IBRAHIM
* DATE                  : 10/02/2002
* REFERS TO             : C102733,1
**********************************************************************************************
*MODIFICATIONS ...
* E302052,1 TMI 12/25/2002 Revise the interface language of the program.
**********************************************************************************************

*-- lcSepart     && the style separator
*-- lnstylewid   && style major width 
*-- lncolorwid   && style non major width
*-- lnClrLen     && color part width
*-- lnClrPos     && color part position 
*-- llMScale     && use extended size scale 
*-- llMulCurr    && Multi currency
*-- lnScaleLen   && Extended size Scale ID Length.
*-- lcErroSrc    && Error source file
*-- lcError      && Temp error file 
*-- lcSlcted     && temp file to hold Selected styles 
*-- lcExpr1      && Return value from 1st optin grid
*-- lcExpr2      && Return value from 2nd optin grid

* define variables needed for building style code structure
*-- laMajSeg     && Array hold style structure information
*-- lnFrPos      && Free part position of a style
*-- lnFrLen      && Free part lenght of a style
*-- lnPreFrLen   && Lenght of the part pre the free part of style
*-- lnPstFrLen   && Lenght of the part after the free part of style
*-- lnPstFrPos   && Posision of the part after the free part of the style

*Define variables for Option Grid1
*-- lnSeasnPos   && Season   Position in laOgFxFlt array
*-- lnDivisPos   && Division Position in laOgFxFlt array
*-- lcSesKey     && Holds key based on season for style file
*-- lcDivkey     && Holds key based on division for style file
*-- lcWhereExp   && The bowse key expresson used for inlist in Option grid
*-- lcSvOrdr     && Save the order other than CSTYLE if used to reset to 

*--hidden variables used in option grid2
*-- lnGrpPos     && Group    position in laOgFxFlt array 
*-- lnDivPos     && Division position in laOgFxFlt array
*-- lnWarePos    && warehous position in laOgFxFlt array
*-- lnPrpkPos    && Prepack  position in laOgFxFlt array
*-- lnSclPos     && Scale    position in laOgFxFlt array

*-- variables used in Option grid
*-- laFxFlt      && Holds a copy of laOgFxFlt
*-- lcRpPrefix   && Prefix
*-- lcRpPostfx   && Postfix
*-- lcRpStatus   && Style Status
*-- llRpNotes    && copy Notepad (Y / N)

*-- lcMemfl1     && hold temp memo file name that saves OG variables
*-- lcMemfl2     && hold temp memo file name that saves OG variables

*:***************************************************************************

*--Initialize variables used in option grids
STORE 0 TO lnSeasnPos,lnDivisPos,;
           lnGrpPos,lnDivPos,lnWarePos,lnPrpkPos,lnSclPos
DIME laFxFlt[1,7]
STORE '' TO lcRpPrefix,lcRpPostfx,lcRpStatus
STORE .F. TO llRpNotes

STORE '' TO lcSesKey,lcDivkey,lcWhereExp,lcSvOrdr


STORE '' TO lcSepart
STORE 0  TO lnstylewid,lncolorwid,lnClrLen,lnClrPos

*--Get style picture information
DIMENSION laMajSeg[1,1]
STORE 0 TO lnFrPos,lnFrLen,lnPreFrLen,lnPstFrLen,lnPstFrPos

=lfGetPict()

llMScale   = gfGetMemVar('M_USEEXSSC')        && Use extended size scale
llMulCurr  = gfGetMemVar('llMulCurr')         && Multi currency
llMultWare = gfGetMemVar('M_WareHouse')='Y'   && Multi Warehous
lnScaleLen = gfGetMemVar('M_EXTWIDTH')        && Extended size Scale ID Length.

*--If code not setuped , warn the user and exit
lcCodName = lfFillcode()
IF !EMPTY(lcCodName)
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,lcCodName + ;
  ' codes have not been set up. Please set up and try again.')
  RETURN 
ENDIF

lcErroSrc  = gcWorkDir+'RejStyls.txt'
lcMemfl1   = gfTempName()   && Save this variable as a memo file
lcMemfl2   = gfTempName()   && Save this variable as a memo file
lcError    = gfTempName()
lcSlcted   = gfTempName()

*--Create the temp file that hold the selected styles
CREATE TABLE (gcWorkDir+lcSlcted) (CSTYMAJOR  C(19) )
USE IN &lcSlcted

*--Call the first option grid
lcExpr1 = gfOpGrid('ICSTYCP1' , .T.)

*--IF close button pressed , exit
IF EMPTY(lcExpr1)
  RETURN
ENDIF

*--Fill the temp file with the selected styles to copy from.
=lfFilTmpFl()

SELECT (lcSlcted)
*-- if no styles selected , exit
IF RECCOUNT()=0
  *Message 02053 : No ð selected.
  *Button  'Ok'
  =gfModalGen('QRM02053B00000','DIALOG','styles')
  =lfClearFls()  
  RETURN
ENDIF

llFrstRun = .T.   && Indicate thet this is first time Option grid is open
llContinue = .T.  && Used to break to loop
DO WHILE llContinue
   *--Call the second option grid
   lcExpr2 = gfOpGrid('ICSTYCP2' , .T.)
   
   *--IF close button pressed , exit
   IF EMPTY(lcExpr2)
     =lfClearFls()
     RETURN
   ENDIF
   
   llFrstRun = .F.
   
  *--Open the needed files to use
  =lfOpenFiles()

  *--Create the ERROR temp files
  =lfCrtTmpFl()

  *--Get errors
  =lfGetErrs()
ENDDO

*--The copy to process
IF RECCOUNT(lcError) < RECCOUNT(lcSlcted)
  =lfCopyTo()
ENDIF

*--Erase unneeded files.
=lfClearFls()

*:**************************************************************************
*:* Name        : lffillcode
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 10/03/2002
*:* Purpose     : Check codes  
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lffillcode()
*:***************************************************************************
FUNCTION lffillcode
IF !USED('CODES')
  =gfOpenFile(gcDataDir+'CODES','IDRLTFNAME','SH')  && Key : CDEFCODE+CRLTFIELD+CFLD_NAME
ENDIF  
SELECT CODES

IF !SEEK('NNSEASON    ','CODES')
  RETURN 'Season'
ENDIF

IF !SEEK('NNCDIVISION ','CODES')
  RETURN 'Division'
ENDIF

RETURN ''
*-- end of lffillcode.

*:**************************************************************************
*:* Name        : lfOpenFiles      
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 10/03/2002
*:* Purpose     : Open the needed files to use
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfOpenFiles()
*:***************************************************************************
FUNCTION lfOpenFiles
IF !USED('CODES')
  =gfOpenFile(gcDataDir+'CODES','IDRLTFNAME','SH')
ENDIF  
IF !USED('STYLE')
  =gfOpenFile(gcDataDir+'STYLE','STYLE','SH')
ENDIF  
IF !USED('STYDYE')
  =gfOpenFile(gcDataDir+'STYDYE','STYDYE','SH')
ENDIF  
IF !USED('SCALE')
  =gfOpenFile(gcDataDir+'SCALE','SCALE','SH')
ENDIF  
IF !USED('WAREHOUS')
  =gfOpenFile(gcDataDir+'WAREHOUS','WAREHOUS','SH')
ENDIF  
IF !USED('STYPRICE')
  =gfOpenFile(gcDataDir+'STYPRICE','STYPRICE','SH')
ENDIF  
IF !USED('OBJLINK')
  =gfOpenFile(gcDataDir+'OBJLINK','OBJLNKTY','SH')
ENDIF  
IF !USED('OBJECTS')
  =gfOpenFile(gcDataDir+'OBJECTS','OBJECTID','SH')
ENDIF  
IF !USED('NOTEPAD')
  =gfOpenFile(gcDataDir+'NOTEPAD','NOTEPAD','SH')
ENDIF  
IF !USED('ICSTYHST')
  =gfOpenFile(gcDataDir+'ICSTYHST','STYHST','SH') 
ENDIF  
IF !USED('FISHD')
  =gfOpenFile(gcDataDir+'FISHD','COMPFYEAR','SH')
ENDIF  
IF !USED('ICSEGVAL')
  =gfOpenFile(gcDataDir+'ICSEGVAL','SEGVAL','SH')
ENDIF  
*-- end of lfOpenFiles.

*:**************************************************************************
*:* Name        : lfCrtTmpFl      
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 10/03/2002
*:* Purpose     : Create the needed temp files
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfCrtTmpFl()
*:***************************************************************************
FUNCTION lfCrtTmpFl
CREATE TABLE (gcWorkDir+lcError) (CSTYMAJOR C(19),cError C(100))
INDEX ON CSTYMAJOR TAG lcError
*-- end of lfCrtTmpFl.

*:**************************************************************************
*:* Name        : lfFilTmpFl      
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 10/07/2002
*:* Purpose     : Fill the created temp file with styles to copy from
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfFilTmpFl()
*:***************************************************************************
FUNCTION lfFilTmpFl
PRIVATE lcWhrExp
IF !USED('STYLE')
  =gfOpenFile(gcDataDir+'STYLE' , 'CSTYLE','SH')
ENDIF  
USE (gcWorkDir+lcSlcted) IN 0
IF RECCOUNT(lcSlcted) = 0
  SELECT STYLE
  SET ORDER TO CSTYLE
  lcWhrExp = '.T.'
  GO TOP
  IF !EMPTY(laFxFLt[lnSeasnPos,6]) .OR. !EMPTY(laFxFLt[lnDivisPos,6])
    IF !EMPTY(lcSvOrdr)
      SET INDEX TO (gcWorkDir+lcSvOrdr+'.IDX')
    ENDIF
    =SEEK(lcWhereExp)
    lcWhrExp = KEY() + [='] + lcWhereExp + [']    
  ENDIF  
  SCAN REST WHILE EVAL(lcWhrExp)
    M.CSTYMAJOR = STYLE.CSTYMAJOR
    WAIT WINDOW NOWAIT 'Adding style: ' + M.CSTYMAJOR + ' to temp file.'
    INSERT INTO (lcSlcted) VALUES (M.CSTYMAJOR)
  ENDSCAN
ENDIF

SELECT (lcSlcted)
INDEX ON CSTYMAJOR TAG &lcSlcted
=gfCloseFile('STYLE')
*-- end of lfFilTmpFl.

*:**************************************************************************
*:* Name        : lfGetErrs    
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 10/08/2002
*:* Purpose     : Exclude styles after adding post/pre(fix) that are wider than lnStylewid
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfGetErrs()
*:***************************************************************************
FUNCTION lfGetErrs

SELECT (lcSlcted)
GO TOP
SCAN  
  *--Build the new major according to style code structure
  lcNewMajor = SUBSTR(&lcSlcted..CSTYMAJOR,1,lnPreFrLen)+ALLT(lcRpPrefix)+;
               ALLTRIM(SUBSTR(&lcSlcted..CSTYMAJOR,lnFrPos,lnFrLen))+ALLT(lcRpPostfx)+;
               SUBSTR(&lcSlcted..CSTYMAJOR,lnPstFrPos,lnPstFrLen)
               
  IF LEN(ALLTRIM(lcNewMajor)) > lnStyleWid
    WAIT WINDOW NOWAIT 'Adding style: '+lcNewMajor + ' to Error file.'
    INSERT INTO (lcError) VALUES (&lcSlcted..CSTYMAJOR,PADR(lcNewMajor,lnStylewid+4)+' => Style Exceeds the style code structur for the style Major')
    LOOP    
  ENDIF
  
  *--Exclude styles that were added befor
  lcNewMajor = SUBSTR(lcNewMajor,1,lnStyleWid)
  IF SEEK(lcNewMajor,'STYLE')
    WAIT WINDOW NOWAIT 'Adding style: '+lcNewMajor + ' to Error file.'
    INSERT INTO (lcError) VALUES (&lcSlcted..CSTYMAJOR,PADR(lcNewMajor,lnStylewid+4)+' => Style already exist')
    LOOP
  ENDIF
ENDSCAN
WAIT CLEAR

IF RECCOUNT(lcError) > 0
  SELECT (lcError)
  DO (gcScrDir + 'IC\ICERROR.SPX')

  *-- Copy only the error field
  COPY FIELDS cError TO (lcErroSrc) SDF
  IF RECCOUNT(lcError) < RECCOUNT(lcSlcted)
    *E302052,1 TMI [START] Change the message as per Omar
    *lcMsg = 'Do you want to proceed with remaining styles?'
    lcMsg = 'Some styles can be created. Would you like to create them?'
    *E302052,1 TMI [END  ] Change the message as per Omar
    lcBtn = 'B42021'     && Proceed ; Return
  ELSE
    *E302052,1 TMI [START] Change the message as per Omar
    *lcMsg = 'There are no styles to copy.'
    lcMsg = 'No styles can be created.'
    *E302052,1 TMI [END  ] Change the message as per Omar
    lcBtn = 'B42022'     && Ok      ; Return
  ENDIF
  IF gfModalGen('QRM00000'+lcBtn,.F.,.F.,.F.,lcMsg) = 1
    llContinue = .F.
  ENDIF                
ELSE
  llContinue = .F.  
ENDIF
*-- end of lfGetErrs.

*:**************************************************************************
 *:* Name        : lfCopyTo      
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 10/07/2002
*:* Purpose     : The copy styles process
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfCopyTo()
*:***************************************************************************
FUNCTION lfCopyTo
PRIVATE lcStyle,lcStyleFl,lcStyDyeFl,lnCnt,lnSzCnt
lcStyleFl  = gfTempName()
lcStyDyeFl = gfTempName()
lcStyPrFl  = gfTempName()

SET ORDER TO STYLE  IN STYLE
SET ORDER TO STYDYE IN STYDYE
USE (gcDataDir+'STYLE')  AGAIN ALIAS &lcStyleFl  ORDER TAG STYLE  IN 0
USE (gcDataDir+'STYDYE') AGAIN ALIAS &lcStyDyeFl ORDER TAG STYDYE IN 0
USE (gcDataDir+'STYPRICE') AGAIN ALIAS &lcStyPrFl ORDER STYPRICE  IN 0 

*--Get Fiscal years
DIMENSION laFisYrs[3]
SELECT FISHD
LOCATE FOR CFISYSTAT = 'P'
FOR lnCnt = 1 TO 3
  laFisYrs[lnCnt] = CFISFYEAR 
  SKIP
ENDFOR

*--Define array laSegInfo needed in adding lines to the file icsegval
DECLARE laSegInfo[1,9]
STORE ' ' TO laSegInfo

lnMajSeg  = gfItemMask('SM')
lnNonSeg  = gfItemMask('SN')
=lffllSegAr()

SELECT (lcSlcted)
*-- Set a relation to error styles files to exclude them
SET RELATION TO CSTYMAJOR INTO &lcError
GO TOP

SCAN FOR EOF(lcError) 
  lcNewMajor = SUBSTR(&lcSlcted..CSTYMAJOR,1,lnPreFrLen)+ALLT(lcRpPrefix)+;
               ALLTRIM(SUBSTR(&lcSlcted..CSTYMAJOR,lnFrPos,lnFrLen))+ALLT(lcRpPostfx)+;
               SUBSTR(&lcSlcted..CSTYMAJOR,lnPstFrPos,lnPstFrLen)
  
  WAIT WINDOW NOWAIT 'Coping style : [' + &lcSlcted..CSTYMAJOR + '] to style [' + lcNewMajor + ']'  
  DIMENSION laClr[1],laExtSz[1]
  STORE '' TO laClr[1],laExtSz[1]

  *--Get colors of this style 
  lcCrit = SUBSTR(&lcSlcted..CSTYMAJOR,1,lnStyleWid)
  SELECT DISTINCT SUBSTR(STYLE,lnClrPos,lnClrLen) ;
  FROM &lcStyleFl ;
  WHERE STYLE = lcCrit ;
  ORDER BY 1 ;
  INTO ARRAY laClr
  
  *--Locate the pointer at the style in lcStyleFl file
  =SEEK(SUBSTR(&lcSlcted..CSTYMAJOR,1,lnStyleWid),lcStyleFl)
  M.SCALE     = IIF(!EMPTY(laFxFlt[lnSclPos,6]) ,laFxFlt[lnSclPos,6],&lcStyleFl..SCALE)
  
  SELECT Scale ;
  FROM SCALE ;
  WHERE Type+Scale='S'+SUBSTR(M.SCALE,1,lnScaleLen) ;
  ORDER BY 1 ;
  INTO ARRAY laExtSz

  FOR lnCnt = 1 TO ALEN(laClr)  
    SELECT &lcStyleFl
    =SEEK(SUBSTR(&lcSlcted..CSTYMAJOR,1,lnStyleWid),lcStyleFl)
    LOCATE REST FOR SUBSTR(STYLE,lnClrPos,lnClrLen) = laClr[lnCnt]
    SCATTER MEMVAR MEMO
    lcStyle = SUBSTR(M.CSTYMAJOR,1,lnStyleWid) + SUBSTR(&lcStyleFl..STYLE,lnStyleWid+1,lnColorWid+IIF(llMScale,-2,1))
    lcNewStyle = PADR(lcNewMajor,lnStyleWid) + SUBSTR(&lcStyleFl..STYLE,lnStyleWid+1,lnColorWid+IIF(llMScale,-2,1))
    lcDesc = m.Desc
    
    *--Assign the field variables that depends on OG
    M.CDIVISION = IIF(!EMPTY(laFxFlt[lnDivPos,6]) ,laFxFlt[lnDivPos,6] ,M.CDIVISION)
    M.CSTYGROUP = IIF(!EMPTY(laFxFlt[lnGrpPos,6]) ,laFxFlt[lnGrpPos,6] ,M.CSTYGROUP)
    M.CDEFWARE  = IIF(!EMPTY(laFxFlt[lnWarePos,6]),laFxFlt[lnWarePos,6],M.CDEFWARE)
    M.STATUS    = lcRpStatus
    M.SCALE     = IIF(!EMPTY(laFxFlt[lnSclPos,6]) ,laFxFlt[lnSclPos,6] ,M.SCALE)
    M.PREPAK    = IIF(!EMPTY(laFxFlt[lnSclPos,6]),laFxFlt[lnPrpkPos,6],M.PREPAK)  && Prepack depends on scale
      
    *-- Zero all fields that holds transactions summations 
    STORE 0 TO  M.stk1    ,M.stk2    ,M.stk3    ,M.stk4    ,M.stk5   ,M.stk6  ,M.stk7 ,M.stk8  ,;
                M.alo1    ,M.alo2    ,M.alo3    ,M.alo4    ,M.alo5   ,M.alo6  ,M.alo7 ,M.alo8  ,;
                M.wip1    ,M.wip2    ,M.wip3    ,M.wip4    ,M.wip5   ,M.wip6  ,M.wip7 ,M.wip8  ,;
                M.ord1    ,M.ord2    ,M.ord3    ,M.ord4    ,M.ord5   ,M.ord6  ,M.ord7 ,M.ord8  ,;
                M.shp1    ,M.shp2    ,M.shp3    ,M.shp4    ,M.shp5   ,M.shp6  ,M.shp7 ,M.shp8  ,;
                M.ret1    ,M.ret2    ,M.ret3    ,M.ret4    ,M.ret5   ,M.ret6  ,M.ret7 ,M.ret8  ,;
                M.ra1     ,M.ra2     ,M.ra3     ,M.ra4     ,M.ra5    ,M.ra3   ,M.ra7  ,M.ra8   ,;
                M.nwo1    ,M.nwo2    ,M.nwo3    ,M.nwo4    ,M.nwo5   ,M.nwo6  ,M.nwo7 ,M.nwo8  ,;
                M.plan1   ,M.plan2   ,M.plan3   ,M.plan4   ,M.plan5  ,M.plan6 ,M.plan7,M.plan8 ,;
                M.totstk  ,M.totalo  ,M.totwip  ,M.totord  ,M.totshp ,M.totret,M.totra,M.ntotwo,;
                M.totplan ,M.dmgqty  ,M.ave_cost                     ,;
                M.nmcost1 ,M.nmcost2 ,M.nmcost3 ,M.nmcost4 ,M.nmcost5,;
                M.nicost1 ,M.nicost2 ,M.nicost3 ,M.nicost4 ,M.nicost5,;
                M.nprcost2,M.nprcost3,M.nprcost4,M.nprcost5,M.totcost,;
                M.MarkA   ,M.MarkB   ,M.MarkC   ,M.GROS_PRICE
    
    *-NOTE : Related fields are copied among the command Scatter memvar above    
                
    *-- Add lines to style file
    FOR lnSzCnt = 1 TO ALEN(laExtSz)
      M.STYLE = lcNewStyle+IIF(llMScale,laExtSz[lnSzCnt],'')
      M.SCALE = IIF(llMScale,laExtSz[lnSzCnt],M.SCALE)
      m.CSTYMAJOR = lcNewMajor
      IF !SEEK(M.STYLE,'STYLE')
        INSERT INTO STYLE FROM MEMVAR   
        =lfAddUsrDt('STYLE')
      ENDIF
    ENDFOR      

    *--Add records to icstyhst  
    SELECT ICSTYHST        
    FOR lnSzCnt = 1 TO ALEN(laExtSz)
      M.STYLE = lcNewStyle+IIF(llMScale,laExtSz[lnSzCnt],'')
      FOR lnK = 1 TO 3
        IF !SEEK(M.STYLE+laFisYrs[lnK],'ICSTYHST')
          INSERT INTO ICSTYHST (STYLE,CFISFYEAR) VALUES (M.STYLE,laFisYrs[lnK])
          =lfAddUsrDt('ICSTYHST')
        ENDIF
      ENDFOR        
    ENDFOR

    *-- Get the array laWh that holds Warehouses of the style we copy from 
    DIMENSION laWH[1]
    SELECT DISTINCT CWARECODE ;
    FROM &lcStyDyeFl ;
    WHERE STYLE = lcStyle AND EMPTY(DYELOT) ;
    INTO ARRAY laWH
    
    *--if no warehous was entred , copy existings ones, otherwise , the specified WH is the only 
    *--one to use in copied style   
    IF !EMPTY(laFxFlt[lnWarePos,6])
      DIMENSION laWH[1]
      laWH[1] = laFxFlt[lnWarePos,6]
    ELSE
      laWH[1] = IIF( EMPTY(laWH[1]) , &lcStyleFl..CDEFWARE , laWH[1] )
    ENDIF
    
    *--Add lines to stydye file
    SELECT &lcStyDyeFl      
    SEEK lcStyle
    FOR lnSzCnt = 1 TO ALEN(laExtSz)
      FOR lnWHCnt = 1 TO ALEN(laWH)
        SELECT &lcStyDyeFl
        *SEEK lcStyle
        SEEK lcStyle+laWH[lnWHCnt]
        SCATTER MEMVAR BLANK
        M.STYLE     = lcNewStyle+IIF(llMScale,laExtSz[lnSzCnt],'')
        m.Desc      = &lcStyDyeFl..Desc    
        m.cWareCode = laWH[lnWHCnt]
        m.gl_link   = &lcStyDyeFl..Gl_Link  
        m.cdisccode = &lcStyDyeFl..cDiscCode
        INSERT INTO STYDYE FROM MEMVAR 
        =lfAddUsrDt('STYDYE')
      ENDFOR
    ENDFOR
    
    *-- Add lines to StyPrice file 
    IF llMulCurr
      DIMENSION laCur[1]
      SELECT CCURRCODE ;
      FROM STYPRICE ;
      WHERE STYLE = lcStyle ;
      ORDER BY 1 ;
      INTO ARRAY laCur
      
      IF !EMPTY(laCur[1])
        FOR lnSzCnt = 1 TO ALEN(laExtSz)
          FOR lnPriCnt = 1 TO ALEN(laCur)
            SELECT &lcStyPrFl
            lcStyCurr = lcStyle+IIF(llMScale,laExtSz[lnSzCnt],'') + laCur[lnPriCnt]
            SEEK lcStyCurr
            SCATTER MEMVAR MEMO
            M.STYLE = lcNewStyle+IIF(llMScale,laExtSz[lnSzCnt],'')
            INSERT INTO STYPRICE FROM MEMVAR        
            =lfAddUsrDt('STYPRICE')
          ENDFOR
        ENDFOR
      ENDIF
    ENDIF 
    
  ENDFOR

  *--Add  line to ICSEGVAL file 
  *--Update the segment value file if there is a free segment in the major.
  FOR lnI=1 TO lnMajSeg
    IF laSegInfo[lnI,3]='F'
      SELECT ICSEGVAL
      IF !SEEK(laSegInfo[lnI,2]+SUBSTR(lcNewMajor,laSegInfo[lnI,4],laSegInfo[lnI,5]),'ICSEGVAL')
        APPEND BLANK               
        REPLACE cISegNo   WITH laSegInfo[lnI,2],;
                cISegVal  WITH SUBSTR(lcNewMajor,laSegInfo[lnI,4],laSegInfo[lnI,5]),;
                cISgValSd WITH lcDesc,;
                cISgValLd WITH lcDesc
        =lfAddUsrDt()
      ENDIF
    ENDIF
  ENDFOR          

  *--  Copy Object links values
  SELECT OBJLINK
  SET ORDER TO TAG OBJLNKTY
  IF SEEK('S'+PADR(&lcSlcted..CSTYMAJOR,20),'OBJLINK')
    SCATTER MEMVAR MEMO
    M.COBJLINK = PADR(lcNewMajor,20)
    IF !SEEK('S'+M.COBJLINK,'OBJLINK')
      INSERT INTO OBJLINK FROM MEMVAR
    ENDIF
  ENDIF
  
  *-- Add note pad 
  IF llRpNotes AND SEEK('F'+PADR(&lcSlcted..CSTYMAJOR,20),'NOTEPAD')
    SELECT NOTEPAD
    SCATTER MEMVAR MEMORY
    M.KEY = PADR(lcNewMajor,20)
    m.cDesc = STRTRAN(m.cDesc , ALLT(&lcSlcted..CSTYMAJOR) , ALLT(lcNewMajor) )
    IF !SEEK('F'+M.KEY,'NOTEPAD')
      INSERT INTO NOTEPAD FROM MEMVAR
    ENDIF
  ENDIF  
  
ENDSCAN

WAIT CLEAR

*--Close reopened files
USE IN (lcStyleFl )
USE IN (lcStyDyeFl)
USE IN (lcStyPrFl )

=gfModalGen('QRM00000B02014',.F.,.F.,.F.,'Styles copied successfully.') 

*-- end of lfCopyTo.

*:**************************************************************************
*:* Name        : lffllSegAr
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 10/09/2002
*:* Purpose     : Fill array laSegInfo[1,9] that is to be used for file icsegval 
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lffllSegAr()
*:***************************************************************************
FUNCTION lffllSegAr

FOR lnCnt=1 TO ALEN(laMajSeg,1)
  DIME laSegInfo[lnCnt,9]
  IF lnCnt<=lnMajSeg
     laSegInfo[lnCnt,1] = 'M' 
  ELSE
    IF lnCnt<=lnNonSeg+lnMajSeg
      laSegInfo[lnCnt,1] = 'N'
    ELSE
      laSegInfo[lnCnt,1] = ' '
    ENDIF
  ENDIF
  laSegInfo[lnCnt,2]=STR(lnCnt,1)
  laSegInfo[lnCnt,3]=laMajSeg[lnCnt,1]
  laSegInfo[lnCnt,4]=laMajSeg[lnCnt,4]
  laSegInfo[lnCnt,5]=LEN(laMajSeg[lnCnt,3])
  laSegInfo[lnCnt,6]=IIF(laMajSeg[lnCnt,1]$'FOQT','ICSEGVAL',;
                     IIF(laMajSeg[lnCnt,1]='S','SCALE','CODES'))
  laSegInfo[lnCnt,7]=IIF(laMajSeg[lnCnt,1]$'FOQT','SEGVAL',;
                     IIF(laMajSeg[lnCnt,1]='S','SCALE','Idrltfname'))
  
  laSegInfo[lnCnt,8]=IIF(laMajSeg[lnCnt,1]$'FOQT',STR(lnCnt,1),;
                     IIF(laMajSeg[lnCnt,1]='S','S','NN'))
  IF laMajSeg[lnCnt,1]$'CZDGF'
    DO CASE
      CASE laSegInfo[lnCnt,3]='C'
        lcCdTyp = 'COLOR     '
      CASE laSegInfo[lnCnt,3]='Z'
        lcCdTyp = 'SEASON    '
      CASE laSegInfo[lnCnt,3]='D'
        lcCdTyp = 'CDIVISION '
      CASE laSegInfo[lnCnt,3]='G'
        lcCdTyp = 'CSTYGROUP '
      CASE laSegInfo[lnCnt,3]='F'
        lcCdTyp  =''
        lcFreeDc = laMajSeg[lnCnt,2]
    ENDCASE    
    laSegInfo[lnCnt,8] = laSegInfo[lnCnt,8]+lcCdTyp
  ENDIF
  laSegInfo[lnCnt,9] = IIF(laMajSeg[lnCnt,1]$'FOQT',"ciSegVal",;
                       IIF(laMajSeg[lnCnt,1]='S',"Scale","cCode_no"))
ENDFOR
*-- end of lffllSegAr.

*:**************************************************************************
*:* Name        : lfClearFls      
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 10/07/2002
*:* Purpose     : Erase unneeded files
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfClearFls()
*:***************************************************************************
FUNCTION lfClearFls

IF USED(lcSlcted) 
  SELECT &lcSlcted
  USE IN &lcSlcted
  CLOSE INDEXES
ENDIF  
ERASE (gcWorkDir+lcSlcted+'.DBF')
ERASE (gcWorkDir+lcSlcted+'.CDX')
ERASE (gcWorkDir+lcSesKey+'.IDX') 
ERASE (gcWorkDir+lcDivKey+'.IDX') 

IF USED(lcError)
  USE IN &lcError
ENDIF
ERASE (gcWorkDir+lcError+'.DBF')

ERASE (gcWorkDir+lcMemfl1+'.MEM')
ERASE (gcWorkDir+lcMemfl2+'.MEM')

*-- Erase the temp indeces on style file
IF USED('STYLE')
  SELECT STYLE
  CLOSE INDEXES
ENDIF
ERASE (gcWorkDir+lcSesKey+'.IDX')
ERASE (gcWorkDir+lcDivkey+'.IDX')

*-- end of lfClearFls.

*:**************************************************************************
*:* Name        : lfwWhen1      
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 10/03/2002
*:* Purpose     : Option grid1 when fn.
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfwWhen()
*:***************************************************************************
FUNCTION lfwWhen1

lnStyPos   = ASUBSCRIPT( laOGFxFlt , ASCAN(laOGFxFlt,'STYLE.CSTYMAJOR ') , 1 )
lnSeasnPos = ASUBSCRIPT( laOGFxFlt , ASCAN(laOGFxFlt,'STYLE.SEASON    ') , 1 )
lnDivisPos = ASUBSCRIPT( laOGFxFlt , ASCAN(laOGFxFlt,'STYLE.CDIVISION ') , 1 )

IF lnOgSeting = 1
  =lfvSeasDiv()
ENDIF
*-- end of lfwWhen1.

*:**************************************************************************
*:* Name        : lfwWhen2      
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 10/03/2002
*:* Purpose     : Option grid 2 when fn.
*:***************************************************************************
*:* Called from : OG
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfwWhen2()
*:***************************************************************************
FUNCTION lfwWhen2

lnGrpPos  = ASUBSCRIPT(laOGFxFlt, ASCAN(laOGFxFlt,'STYLE.CSTYGROUP ') , 1 )
lnDivPos  = ASUBSCRIPT(laOGFxFlt, ASCAN(laOGFxFlt,'STYLE.CDIVISION ') , 1 )
lnSclPos  = ASUBSCRIPT(laOGFxFlt, ASCAN(laOGFxFlt,'STYLE.SCALE     ') , 1 )
lnPrpkPos = ASUBSCRIPT(laOGFxFlt, ASCAN(laOGFxFlt,'STYLE.PREPAK    ') , 1 )
lnWarePos = ASUBSCRIPT(laOGFxFlt, ASCAN(laOGFxFlt,'STYLE.CDEFWARE  ') , 1 )

*--Remove the word "All" from the array "LAOGPOPARY" and replace it with the work "N/A"
=lfRmvAll()

*-- if it is the default case
IF lnOgSeting = 1
   
  IF llFrstRun

    laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOGFxFlt,1) + lnPrpkPos] = .F.
    =lfOGShowGet('laOGFxFlt[' + ALLTRIM(STR(lnPrpkPos)) + ',6]')  
  
    SELECT WAREHOUS
    GO TOP                      
    laOGFxFlt[lnWarePos,6] = WAREHOUS.CWARECODE
  
    SHOW GET pbRun DISABLE  
  
  ELSE
    
    *--Restore saved option grid variables
    RESTORE FROM (gcWorkDir+lcMemfl1) ADDITIVE 
    RESTORE FROM (gcWorkDir+lcMemfl2) ADDITIVE 

    *--Refresh option grid 
    =lfOGShowGet('lcRpStatus')    
    =lfOGShowGet('llRpNotes')
    =lfOGShowGet('laOGFxFlt[' + ALLTRIM(STR(lnGrpPos )) + ',6]')
    =lfOGShowGet('laOGFxFlt[' + ALLTRIM(STR(lnDivPos )) + ',6]')
    =lfOGShowGet('laOGFxFlt[' + ALLTRIM(STR(lnWarePos)) + ',6]')
    =lfOGShowGet('laOGFxFlt[' + ALLTRIM(STR(lnPrpkPos)) + ',6]')
    =lfOGShowGet('laOGFxFlt[' + ALLTRIM(STR(lnSclPos )) + ',6]')    
    
    llFrstRun = .T.

  ENDIF
ENDIF
  laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOGFxFlt,1) + lnWarePos] = llMultWare
  =lfOGShowGet('laOGFxFlt[' + ALLTRIM(STR(lnWarePos)) + ',6]')      

*-- end of lfwWhen2.

*:**************************************************************************
*:* Name        : lfRmvAll
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 10/09/2002
*:* Purpose     : Remove the word "All" from the array "LAOGPOPARY" and replace 
*:*             : it with the work "N/A"
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfRmvAll()
*:***************************************************************************
FUNCTION lfRmvAll
PRIVATE lnCnt
FOR lnCnt = 1 TO ALEN(LAOGPOPARY)
  IF LAOGPOPARY[lnCnt] == 'All'
    LAOGPOPARY[lnCnt] = 'N/A'
  ENDIF
ENDFOR

*-- end of lfRmvAll.

*:**************************************************************************
*:* Name        : lfPrefix      
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 10/07/2002
*:* Purpose     : Valid Fn for prefix part
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfPrefix()
*:***************************************************************************
FUNCTION lfvPrefix
PRIVATE lcRunStat
lcRunStat = IIF(LEN(ALLT(lcRpPrefix))>=1 OR LEN(ALLT(lcRpPostfx))=2,'ENABLE','DISABLE')
SHOW GET pbRun &lcRunStat
*-- end of lfPrefix.

*:**************************************************************************
*:* Name        : lfPstfix      
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 10/07/2002
*:* Purpose     : Valid Fn for postfix part
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfPstfix()
*:***************************************************************************
FUNCTION lfvPstfix
PRIVATE lcRunStat
lcRpPostfx = IIF( LEN(ALLT(lcRpPostfx)) < 2 , '' , lcRpPostfx )
lcRunStat = IIF(LEN(ALLT(lcRpPrefix))>=1 OR LEN(ALLT(lcRpPostfx))=2,'ENABLE','DISABLE')
SHOW GET pbRun &lcRunStat

*-- end of lfPstfix.
*:**************************************************************************
*:* Name        : lfvStyle      
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 10/03/2002
*:* Purpose     : Valid style func.
*:***************************************************************************
*:* Called from : OG
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvStyle()
*:***************************************************************************
FUNCTION lfvStyle

lcStyle = VARREAD()

lcTag = ORDER('STYLE')

SET ORDER TO cStyle IN STYLE

IF LASTKEY() = 13 AND !MDOWN()
  IF SEEK(&lcStyle.,'Style') 
    &lcStyle = STYLE.cStyMajor
  ELSE
    &lcStyle = gfStyBrw('M',"","",.F.)
  ENDIF
ELSE
  &lcStyle = ''
ENDIF

SET ORDER TO lcTag IN STYLE

*-- end of lfvStyle.

*:**************************************************************************
*:* Name        : lfMajTtGet      
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 10/03/2002
*:* Purpose     : Major title get
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfMajTtGet()
*:***************************************************************************
FUNCTION lfMajTtGet

RETURN gfItemMask("HM")

*-- end of lfMajTtGet.

*!*************************************************************
*! Name      : lfMajPic     
*! Developer : TAREK MOHAMED IBRAHIM
*! Date      : 10/09/2002
*! Purpose   : get major segment picture
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfMajPic()
*!*************************************************************

FUNCTION lfMajPic

lcMajPic = "@! " + gfItemMask("PM")

RETURN lcMajPic

*!*************************************************************
*! Name      : lfsrvSty      
*! Developer : TMI - TAREK MOHAMED IBRAHIM
*! Date      : 10/09/2002
*! Purpose   : Rise change style flag, in range browse screen.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid1
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfsrvSty()
*!*************************************************************
*! Note      : SRV symbol is [S,Set -- R,Reset -- V,Valid]
*!*************************************************************
FUNCTION lfSRVSty
PARAMETERS lcParm

PRIVATE lnSlct
lnSlct = SELECT()
SELECT STYLE

DO CASE
  CASE lcParm = 'S'  && Set code
  
    SELECT STYLE
    SET ORDER TO TAG Cstyle
    IF !EMPTY(lcSvOrdr)
      SET INDEX TO (gcWorkDir+lcSvOrdr+'.IDX')
    ENDIF

  CASE lcParm = 'R'  && Reset code
    SELECT (laOGFxFlt[lnStyPos,6])
    COPY TO (gcWorkDir+lcSlcted)
    SELECT STYLE
    SET ORDER TO TAG STYLE

ENDCASE

*-- Select the old work aria
SELECT (lnSlct)
*-- end of lfsrvSty.

*:**************************************************************************
*:* Name        : lfvSeasDiv      
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 10/03/2002
*:* Purpose     : Valid fn. for Season and division 
*:***************************************************************************
*:* Called from : OG1
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvSeasDiv()
*:***************************************************************************
FUNCTION lfvSeasDiv

PRIVATE llFound
llFound = .F.

SELECT STYLE
SET ORDER TO CSTYLE

STORE '' TO lcWhereExp , lcSvOrdr 

IF !EMPTY(laOgFxFLt[lnSeasnPos,6]) .OR. !EMPTY(laOgFxFLt[lnDivisPos,6])
  DO CASE
  
    CASE laMajSeg[1,1] = 'Z' 
      DO CASE
        CASE !EMPTY(laOgFxFlt[lnSeasnPos,6]) .AND. EMPTY(laOgFxFlt[lnDivisPos,6])
          lcWhereExp = PADR(laOgFxFlt[lnSeasnPos,6], LEN(laMajSeg[1,3]) )
        CASE !EMPTY(laOgFxFlt[lnDivisPos,6])
          IF FILE(gcWorkDir+lcDivKey+'.IDX')
            SET INDEX TO (gcWorkDir+lcDivKey+'.IDX')
          ELSE
            WAIT WINDOW NOWAIT 'Collecting Styles ...'
            INDEX ON CDIVISION+SEASON+CSTYMAJOR TO (gcWorkDir+lcDivKey+'.IDX') UNIQUE
            WAIT CLEAR
          ENDIF
          lcSvOrdr = ORDER()
          lcWhereExp = PADR(laOgFxFlt[lnDivisPos,6],6)+;
                       IIF(!EMPTY(laOgFxFlt[lnSeasnPos,6]),PADR(laOgFxFlt[lnSeasnPos,6],6),'')
      ENDCASE
      
    CASE laMajSeg[1,1] = 'D' 
      DO CASE
        CASE !EMPTY(laOgFxFlt[lnDivisPos,6]) .AND. EMPTY(laOgFxFlt[lnSeasnPos,6])
          lcWhereExp = PADR(laOgFxFlt[lnDivisPos,6], LEN(laMajSeg[1,3]) )
        CASE !EMPTY(laOgFxFlt[lnSeasnPos,6])
          IF FILE(gcWorkDir+lcSesKey+'.IDX')
            SET INDEX TO (gcWorkDir+lcSesKey+'.IDX')
          ELSE          
            WAIT WINDOW NOWAIT 'Collecting Styles ...'
            INDEX ON SEASON+CDIVISION+CSTYMAJOR TO (gcWorkDir+lcSesKey+'.IDX') UNIQUE
            WAIT CLEAR
          ENDIF
          lcSvOrdr = ORDER()
          lcWhereExp = PADR(laOgFxFlt[lnSeasnPos,6],6)+;
                       IIF(!EMPTY(laOgFxFlt[lnDivisPos,6]),PADR(laOgFxFlt[lnDivisPos,6],6),'')
      ENDCASE
  
    CASE !EMPTY(laOgFxFlt[lnSeasnPos,6])
      IF FILE(gcWorkDir+lcSesKey+'.IDX')
        SET INDEX TO (gcWorkDir+lcSesKey+'.IDX')
      ELSE
        WAIT WINDOW NOWAIT 'Collecting Styles ...'
        INDEX ON SEASON+CDIVISION+CSTYMAJOR TO (gcWorkDir+lcSesKey+'.IDX') UNIQUE
        WAIT CLEAR
      ENDIF
      lcSvOrdr = ORDER()
      lcWhereExp = PADR(laOgFxFlt[lnSeasnPos,6],6)+;
                   IIF(!EMPTY(laOgFxFlt[lnDivisPos,6]),PADR(laOgFxFlt[lnDivisPos,6],6),'')
      
    CASE !EMPTY(laOgFxFlt[lnDivisPos,6])
      IF FILE(gcWorkDir+lcDivKey+'.IDX')
        SET INDEX TO (gcWorkDir+lcDivKey+'.IDX')
      ELSE      
        WAIT WINDOW NOWAIT 'Collecting Styles ...'
        INDEX ON CDIVISION+SEASON+CSTYMAJOR TO (gcWorkDir+lcDivKey+'.IDX') UNIQUE
        WAIT CLEAR
      ENDIF
      lcSvOrdr = ORDER()
      lcWhereExp = PADR(laOgFxFlt[lnDivisPos,6],6)+;
                   IIF(!EMPTY(laOgFxFlt[lnSeasnPos,6]),PADR(laOgFxFlt[lnSeasnPos,6],6),'')
  ENDCASE        
  llFound = SEEK(lcWhereExp)

ELSE
  
  GO TOP
  llFound = !EOF()
    
ENDIF

lcRunStat = IIF(llFound,'ENABLE','DISABLE')         
SHOW GET pbRun &lcRunStat

*-- end of lfvSeasDiv.

*:**************************************************************************
*:* Name        : lfvScale      
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 10/03/2002
*:* Purpose     : 
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvScale()
*:***************************************************************************
FUNCTION lfvScale
PRIVATE lcScle,llBrowse
llBrowse = .T.
lcScle = SUBSTR(laOGFxFlt[lnSclPos,6],1,lnScaleLen)
IF !EMPTY(lcScle) .AND. !SEEK('S'+lcScle,'SCALE')
  =gfscalbrow(@lcScle,llMScale)
  laOGFxFlt[lnSclPos,6] = lcScle   
ENDIF
IF lcScle <> PADR(lcOldValue,lnScaleLen)
  laOGFxFlt[lnPrpkPos,6] = ''
  laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOGFxFlt,1) + lnPrpkPos] = !EMPTY(laOGFxFlt[lnSclPos,6])
  =lfOGShowGet('laOGFxFlt[' + ALLTRIM(STR(lnPrpkPos)) + ',6]')   
ENDIF  

*-- end of lfvScale.

*:**************************************************************************
*:* Name        : lfvPrepk      
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 10/03/2002
*:* Purpose     : Valid fn. for prepack
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvPrepk()
*:***************************************************************************
FUNCTION lfvPrepk
PRIVATE lcScle,lcPrpk,llBrowse
llBrowse = .T.
lcScle = laOGFxFlt[lnSclPos ,6]
lcPrpk = laOGFxFlt[lnPrpkPos,6]

IF !EMPTY(lcScle) .AND. !EMPTY(lcPrpk) .AND. !SEEK('P'+lcScle+lcPrpk,'SCALE')
  =gfprepbrow( lcScle ,@lcPrpk)
  laOGFxFlt[lnPrpkPos,6] = lcPrpk   
ENDIF

*-- end of lfvPrepk.

*:**************************************************************************
*:* Name        : lfwScale      
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 10/03/2002
*:* Purpose     : When function for scale field
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfwScale()
*:***************************************************************************
FUNCTION lfwScale
lcOldValue = laOGFxFlt[lnSclPos,6]
*-- end of lfwScale.

*:**************************************************************************
*:* Name        : lfvChErSrc      
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 10/08/2002
*:* Purpose     : Change error source file
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvChErSrc()
*:***************************************************************************
FUNCTION lfvChErSrc
PRIVATE lcNewSrcFl
lcNewSrcFl = ''
lcNewSrcFl = GETFILE('TXT','Select TXT file')
IF !EMPTY(lcNewSrcFl)
  lcErroSrc = lcNewSrcFl
ENDIF
SHOW GET lcErroSrc
*-- end of lfvChErSrc.

*:**************************************************************************
*:* Name        : lfGetPict
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 10/07/2002
*:* Purpose     : Get style picture information 
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfGetPict()
*:***************************************************************************
FUNCTION lfGetPict
PRIVATE lcMjrPct,lcNMjrPt,lcIMjrPt,lnCnt

=gfItemMask(@laMajSeg)
lcMjrPct  = gfItemMask('PM')
lcNMjrPt  = gfItemMask('PN')
lcIMjrPt  = gfItemMask('PI')
lnstylewid=LEN(lcMjrPct)
lncolorwid=LEN(lcNMjrPt)
lcSepart  =SUBSTR(lcIMjrPt,lnstylewid+1,1)
FOR lnCnt = 1 TO ALEN(laMajSeg,1)
  IF laMajSeg[lnCnt,1]='C'
    lnClrLen = LEN(laMajSeg[lnCnt,3])
    lnClrPos = laMajSeg[lnCnt,4]
  ENDIF
ENDFOR
*-- Get the free position and lenght
STORE 0 TO lnFrIndx,lnFrSep

FOR lnCnt = 1 TO ALEN(laMajSeg,1)  
  IF laMajSeg[lnCnt,1]='F'
    lnFrIndx = lnCnt
    lnFrLen = LEN(laMajSeg[lnCnt,3])
    lnFrPos = laMajSeg[lnCnt,4]
    lnFrSep = IIF(laMajSeg[lnCnt,7],0,LEN(ALLT(laMajSeg[lnCnt,6])) )
    EXIT
  ENDIF
ENDFOR

*--Get post fre len & position 
lnPreFrLen = IIF(laMajSeg[lnFrIndx,4] = 1 , 0 , laMajSeg[lnFrIndx,4] - 1 )
IF !laMajSeg[lnFrIndx,7]
  lnPstFrLen = lnStylewid - ( lnPreFrLen + lnFrLen + lnFrSep )
  lnPstFrPos = laMajSeg[lnFrIndx+1,4] - lnFrSep
ENDIF  

*-- end of lfGetPict.

*:**************************************************************************
*:* Name        : lfSavVars
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 10/09/2002
*:* Purpose     : Save 2nd Option grid variables to a memo files in case it 
*:*             : will be run more than once
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfSavVars()
*:***************************************************************************
FUNCTION lfSavVars
SAVE TO (gcWorkDir+lcMemfl1+'.MEM') ALL LIKE L?RP*
SAVE TO (gcWorkDir+lcMemfl2+'.MEM') ALL LIKE LAOGFXFLT
*-- end of lfSavVars.

*:**************************************************************************
*:* Name        : lfAddUsrDt
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 10/09/2002
*:* Purpose     : Add audit data for inserted records
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfAddUsrDt()
*:***************************************************************************
FUNCTION lfAddUsrDt
PARAMETERS lcAlias
PRIVATE lnSlct
lnSlct = SELECT()
IF !EMPTY(lcAlias)
  SELECT (lcAlias)
ENDIF  
REPLACE cAdd_User WITH gcUser_ID,;
        dAdd_Date WITH gdSysDate,;
        cAdd_Time WITH Time()

SELECT (lnSlct)        
*-- end of lfAddUsrDt.
