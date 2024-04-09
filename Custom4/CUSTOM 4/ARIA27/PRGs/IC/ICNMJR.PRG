*:************************************************************************
*:           File: ICNMjr.PRG                                           :*
*:         System: ARIA APPAREL SYSTEM 2.7                              :*
*:         Module: Inventory Control                                    :*
*:        Program: Add,Change,Delete Styles Non Majors.                 :*
*:         Author: Timour Abdalla Khalil                                :*
*:           Date: 06/02/97                                             :* 
*:**********************************************************************:*
*:Modifications :
*B602183,1 HDM 01/10/1999 If U select cancel from mover screen the message
*B602183,1                of color changing shouldn't appear.
*B801907,1 HDM 02/07/1999 If the style is copied from another style we have
*B801907,1                to update the style Desc Too.
*E301209,4 TAK 05/28/1999 Change the major scale code to be one or two chracters
*E301209,4                depends on ic setup in case of extended size scale.
*B802618,1 TAK 09/16/1999 Fixed overwiting of long description when add/remove colors.
*B603123,1 ADEL 06/21/2000 Fixed overwiting of long description when add/remove colors.
*B604115,1 KAM 13/02/2001  Fixed using short description with description instead
*B604115,1 KAM             existing long description
*B604965,1 ABD 09/30/2001  Fix problem in the Planning Qty when Company use Use extended size scale
*B604965,1 ABD             The planning field calculate all planning Qty for all color when add new color,
*B604965,1 ABD             We will not update the Planning Qty in case use Use extended size scale. [Begin]
*B604614,1 ABD 10/04/2001  Fix problem when remove first color, the description still take the description
*B604614,1 ABD             For the first color that we remove.
*C102466,1 ABD 10/21/2001 Restrict adding "Obsolete" colors to style.
*B606695,1 ABD 03/12/2003 Fix problem that when add a new color for exist style it get the 
*B606695,1 ABD            Wrong style description.
*B123464,1 NNA 08/06/2004 Fix bug that when you add a new color it does not show in the color folder
*B123464,1 NNA            that if you entered to the related Fields screen the pressed cancel, in this case
*B123464,1 NNA            the Crltd_vlu filed for this Color will Be empty
*B607888,1 TMI 12/19/2006 Fix a bug that when adding a new color the tax break of the first scale is copied to all scales ( ticket # T20060816.0008 )
*B608235,1 TMI 08/23/2007 the fix B607888 caused a problem that when the used added a style with ext. scale x and then he added
*                         more subscales to this scale ,i.e added more lines in the scale file, then the FOR loop in B607888 may
*                         reach the EOF of file, to fix I take the tax break of the nearest subscale. (ticket T20070816.0014)
*:************************************************************************

lnAlias=SELECT()
*--Checking for free segments in Non Major.
STORE 0 TO lnNoMsg,lnFSegNo,lnArLn
STORE '' TO lcSclSepr,lcFSgPct
llFreeExst=.F.

FOR I=1 TO ALEN(laSegInfo,1)
  IF laSegInfo[I,1]='M'
    lnNoMsg=lnNoMsg+1
  ENDIF
  IF laSegInfo[I,1]='N' AND laSegInfo[I,3]='F'
    lcFSgPct=REPLICATE('X',laSegInfo[I,5])
    lnFSegNo=I-lnMajSeg
    llFreeExst=.T.
    EXIT
  ENDIF
ENDFOR
IF llMScale
  lcSclSepr=IIF(SUBSTR(lcIMjrPt,laSegInfo[ALEN(laSegInfo,1),4]-1,1)<>'X',SUBSTR(lcIMjrPt,laSegInfo[ALEN(laSegInfo,1),4]-1,1),'')
ENDIF

IF llFreeExst OR lnNonSeg>2 OR (lnNonSeg=2 AND !llMScale)
  *--More than non major segment or Free. 
  =lfNMScrn()
ELSE
  *--Only one non major segment. 
  =lfNMMovr()
ENDIF

IF llMScale
  =lfUpdScle()
ENDIF

SELECT(lnAlias)
RETURN



*!*************************************************************
*! Name      : lfNMMovr
*! Purpose   : Edit non majors from mover for segment.
*!*************************************************************
FUNCTION lfNMMovr
PRIVATE lnLoop , lcOldArStr , lcTArStr



STORE '' TO lcOldArStr , lcTArStr
STORE .T. TO llClrChng
*--Get the segment no.
lnSegN=1
FOR lnSegN=1 TO ALEN(laSegInfo,1)
  IF laSegInfo[lnSegN,1]='N'
    IF ( laSegInfo[lnSegN,3] <> 'F' )
      EXIT
    ENDIF  
  ENDIF
ENDFOR

lcFile=laSegInfo[lnSegN,6]
lcTag =laSegInfo[lnSegN,7]
lnCdSz=laSegInfo[lnSegN,5]
lnStrt=laSegInfo[lnSegN,4]
SELECT (lcFile)
SET ORDER TO TAG &lcTag

IF laSegInfo[lnSegN,3] $ 'TQO'
  SELECT PADR(ALLTRIM(ciSegVal),lnCdSz,' ')+"-"+cISgValSd FROM ICSEGVAL ;
           WHERE cISegNO=laSegInfo[lnSegN,8] INTO ARRAY laSource
ELSE
  *C102466,1 ABD - Restrict adding "Obsolete" colors to style. [Begin]
  *SELECT cCode_no+"-"+cDiscrep FROM CODES ;
  *         WHERE CDEFCODE + cRltField + cFld_Name = laSegInfo[lnSegN,8];
  *         INTO ARRAY laSource
  PRIVATE lcTmpCodes , llTmpCodes
  *-- Open the codes file with anther alias.
  lcTmpCodes = gfTempName()
  llTmpCodes = gfOpenFile(gcDataDir+'CODES',gcDataDir+'Ccode_no','SH',@lcTmpCodes,.T.) 
  SELECT CODES
  lnLenSour = 0
  = SEEK(laSegInfo[lnSegN,8])
  *-- Scan and get colors with Obsolete No only.

  SCAN REST WHILE CDEFCODE + cRltField + cFld_Name = laSegInfo[lnSegN,8]
    *-- Add Colors to the array that llObsolete with 'N'.
    
    *B123464,1 NNA 08/06/2004 (Begin) Seek For Empty Crltd_Vlu Because if you in the Related
    *B123464,1 NNA            Fields screen then press cancel this field will be empty so this
    *B123464,1 NNA            color will not appear in the color mover screen
    *IF !SEEK('N'+'COLOR     '+cCode_no+SPACE(30)+'LLOBSOLETE',lcTmpCodes)  .OR. ALLTRIM(&lcTmpCodes..cRltd_Vlu) = "F" 
    IF !SEEK('N'+'COLOR     '+cCode_no+SPACE(30)+'LLOBSOLETE',lcTmpCodes)  .OR.;
         (ALLTRIM(&lcTmpCodes..cRltd_Vlu) = "F" .OR. EMPTY(&lcTmpCodes..cRltd_Vlu))
    *B123464,1 NNA (End)
    
      DECLARE laSource[lnLenSour + 1]
      laSource[ALEN(laSource)] = cCode_no+"-"+cDiscrep
      lnLenSour = lnLenSour +1
    ENDIF
  ENDSCAN
  *-- Close code file.
  IF llTmpCodes
    = gfCloseFile(lcTmpCodes)
  ENDIF
  IF TYPE('laSource') = 'U'
    *- Message Text   :- All the system colors are Obsolete, Can not add new color(s).
    *- Message Number :- 42224.
    *- button message :- OK.
    *- button Number  :- 42001
   = gfModalGen('INM42224B42001','DIALOG','the system'+'|'+'add new color(s).')
    RETURN
  ENDIF
  *C102466,1 ABD - [End]  
ENDIF

SELECT (lcColorFil)
LOCATE FOR cstatus <> 'A'
IF !FOUND()
  DIME laOldTrAr[1]
  laOldTrAr=' '
ENDIF
GO TOP
DIME laTarget[1]
laTarget=' '
IF !EOF()
  STORE 1 TO I,J
  SCAN 
    IF ASCAN(laTarget,SUBSTR(Style,lnStrt,lnCdSz))<>0
      LOOP
    ENDIF  
    DIME laTarget[I]
    lcCdDesc=''
    = lfGetCode(laSegInfo[lnSegN,3],laSegInfo[lnSegN,8],SUBSTR(Style,lnStrt,lnCdSz),lcFile,.T.)
    laTarget[I]=SUBSTR(Style,lnStrt,lnCdSz)+"-"+lcCdDesc
    IF cstatus <> 'A' 
      DIME laOldTrAr[J]
      laOldTrAr[J]=laTarget[I]
      J=J+1 
    ENDIF
    I=I+1 
  ENDSCAN
ENDIF
*--HDM
=ASORT(laSource)
=ASORT(laTarget)
*--HDM
DIMENSION laOldArr[1]

=ACOPY(laTarget,laOldArr)

=gfMover(@laSource,@laTarget,"Select "+lcNMjrTl,.T.,'lfValdClr')

*--B602183,1 HDM Compare (laOldArr,laTarget)[start]
*--Fill 2 Strings with the array elements
*--Then Compare the 2 Strings
*--If Identical ==> no changes where done changes flag  = .F.
*--else     make the changes flag  = .T.

IF ALEN(laTarget) = ALEN(laOldArr)
  FOR lnLoop = 1 TO ALEN(laTarget)
    lcOldArStr = lcOldArStr + ALLTRIM(laOldArr[lnLoop])
    lcTArStr   = lcTArStr   + ALLTRIM(laTarget[lnLoop])
  ENDFOR
  IF lcOldArStr == lcTArStr
    llClrChng = .F.
  ELSE
    llClrChng = .T.
  ENDIF
ENDIF

IF LASTKEY()=27 .OR. !llClrChng
  *--B602183,1 HDM Compare (laOldTrAr,laTarget)[end]
  =ACOPY(laOldTrAr,laTarget)
  RETURN
ENDIF
SELECT (lccolorfil)

*B604614,1 ABD - Check for the First color description if this color was remove. [Begin]
GOTO TOP
Private lcFrstDesc, llChngDesc,lcDefltclr ,llDefDesc
STORE '' TO lcFrstDesc,lcDefltclr 
STORE .F. TO llChngDesc ,llDefDesc
IF !EOF()
  
  *B606695,1 ABD - Get the Correct Color Width. [Begin]
  *lcFrstDesc = ALLTRIM(gfCodDes(SUBSTR(Style,lnstylewid+2,lncolorwid),'COLOR'))
   lcFrstDesc = ALLTRIM(gfCodDes(SUBSTR(Style,lnstylewid+2,laSegInfo[lnSegN,5]),'COLOR'))
  *B606695,1 ABD - [End]
  
  llChngDesc = ATC(lcFrstDesc,laOldArr[1]) > 0
  llDefDesc  = ATC(lcFrstDesc,Desc1) > 0
  = ASORT(laTarget)
ENDIF
*B604614,1 ABD -  [End]


DELETE ALL FOR cstatus='A'
SET DELE OFF
IF ! (ALEN(latarget)=1 AND EMPTY(latarget[1]))
  IF laScrMode[3] AND ;
    ( laData[21] OR (!laData[21]) ) AND SEEK(lcMajor,'BOM')
    * AND llimpcost
    *--One or more color have been added, Please update the style cost sheet.  
    =gfModalGen('INM42149B42000','DIALOG')
  ENDIF
    
  SELECT (lccolorfil)
  FOR I=1 TO ALEN(laTarget)
    lcNmjrCd = SUBSTR(laTarget[I],1,laSegInfo[lnSegN,5])
    lcCodeDsc= SUBSTR(laTarget[I],laSegInfo[lnSegN,5]+2,20)
    llCange=.F.
    IF !SEEK(lcMajor+lcSepart+lcNmjrCd)
      glupdated=.T.
      APPEND BLANK
      *B604614,1 ABD - Fix problem when remove first color, the description still take the description
      *B604614,1 ABD - For the first color that we remove. 
      *B604614,1 ABD - I don't know who comment this line. [Begin]
*     lcDesc1=lcCodeDsc
      lcDesc1=lcCodeDsc
      *lcDesc1=' '
      *B604614,1 ABD - [End]
      
      llCange=.T.
    ELSE
      IF DELETED()
        RECALL
        lcDesc1=Desc1
        llCange=.T.
      ENDIF       
      
    ENDIF
    IF llCange 
      GATHER FROM laData FIELDS &lcScFields MEMO    
      REPLACE Style   WITH PADR(ALLTRIM(laData[1]),lnStyleWid,' ')+lcSepart+lcNmjrCd,;
              cStatus WITH 'A'
              
      *B604965,1 ABD - Fix problem in the Planning Qty when Company use Use extended size scale
      *B604965,1 ABD - The planning field calculate all planning Qty for all color when add new color,
      *B604965,1 ABD - We will not update the Planning Qty in case use Use extended size scale. [Begin]
      *-- Update the Plan filds with Zero Qty.
      IF llMScale
        REPLACE Plan1   WITH 0,;
                Plan2   WITH 0,;
                Plan3   WITH 0,;
                Plan4   WITH 0,;
                Plan5   WITH 0,;
                Plan6   WITH 0,;
                Plan7   WITH 0,;
                Plan8   WITH 0,;
                TotPlan WITH 0
      ENDIF
      *B604965,1 ABD - [End]
              
      *--HDM B801907,1 [Start]If the style is copied from another style
      *--we have to update the style Desc Too
      *B802618,1 Start.
      IF EMPTY(Desc1) OR laScrMode[3]
      *IF EMPTY(Desc1) OR laScrMode[3] .OR. llCopySt
      *B802618,1 End.
      *--HDM B801907,1 [End]
        *B603123,1 (Begin) If the user wants the long desc to apply for all colors , do so.
        *REPLACE Desc1 WITH ALLTRIM(ALLTRIM(Desc) +'  '+lcCodeDsc)
        *B604115,4 KAM  if all have  user long description the new color use it else
        *B604115,4 KAM new color use short desc plus color [start]
        *REPLACE Desc1 WITH IIF(!llAllClrs,ALLTRIM(ALLTRIM(Desc) +'  '+lcCodeDsc),Desc1)
        IF laScrMode[3] .AND. lfSamDsc()
          REPLACE Desc1 WITH IIF(!llAllClrs,ALLTRIM(ALLTRIM(Desc) +'  '+lcCodeDsc),Desc1)
        ELSE
          *- Abdou
          *REPLACE Desc1 WITH IIF(!llAllClrs AND EMPTY(laData[4]),ALLTRIM(ALLTRIM(Desc) +'  '+lcCodeDsc),Desc1)          
          IF llDefDesc
            REPLACE Desc1 WITH ALLTRIM(ALLTRIM(Desc)+'  '+lcCodeDsc)
          ELSE  
            REPLACE Desc1 WITH IIF(!llAllClrs AND EMPTY(laData[4]),ALLTRIM(ALLTRIM(Desc) +'  '+lcCodeDsc),Desc1)
          ENDIF
          *- Abdou
          
        ENDIF
        *B604115,4 KAM [end]
        *B603123,1 (End)
      ELSE
        *B604614,1 ABD - Check for the First color description if this color was remove, we
        *B604614,1 ABD - will change the description for this color in Field Desc1. [Begin]
        *REPLACE Desc1 WITH IIF(!EMPTY(lcDesc1),lcDesc1,Desc1)
        IF llChngDesc
          llChngDesc = .F.
          IF ATC(lcFrstDesc,Desc1) > 0
            lcOldDesc = DESC1
            lcOldDesc = STRTRAN(lcOldDesc,lcFrstDesc,lcCodeDsc)
            lcDefltclr = lcCodeDsc
            REPLACE Desc1 WITH lcOldDesc
            laData[4] = lcOldDesc
           ENDIF
         ELSE
           IF EMPTY(lcDesc1)
             REPLACE Desc1 WITH Desc1
           ELSE
             lcOldDesc = DESC1
             lcNewDeflt = STRTRAN(lcOldDesc,lcDefltclr,lcCodeDsc)
             REPLACE Desc1 WITH lcNewDeflt
           ENDIF  
         ENDIF
         *B604614,1 ABD - [End]
      ENDIF
    ENDIF
  ENDFOR

*B604614,1 ABD - Else For If Statment. [Begin]
ELSE
  *-- Empty the Long desc.
  laData[4] = ''
  SHOW GET Ladata[4]
  *B604614,1 ABD - [End]
ENDIF
SET DELE ON
RETURN
*!*************************************************************
*! Name      : lfNMScrn
*! Purpose   : Edit non majors from screen by segment.
*!*************************************************************
FUNCTION lfNMScrn

lcWindTitl = 'Add / Remove '+lcNMjrTl+'.'
lcBrowTtl1 = 'Segment_Values'
lcBrowTtl2 = 'Non Major'
lcWith     = 'ICNMjr,Codes,Segment_Values,Scales,Non'
STORE ' ' TO lcBr1Fields,lcBr2Fields
STORE 1 TO lnActSeg,lnCurRec1,lnCurRec2,lnSize,lnStart
STORE "" TO lcEField,lcCField
lcSegVal = SPACE(lnColorWid)

DIMENSION lafilestru[4,4]
lcTmpSgVal = gfTempName()
lafilestru[1,1] = 'nRecNo'
lafilestru[1,2] = 'N'
lafilestru[1,3] = 6
lafilestru[1,4] = 0
lafilestru[2,1] = 'cMarker'
lafilestru[2,2] = 'C'
lafilestru[2,3] = 1
lafilestru[2,4] = 0
lafilestru[3,1] = 'nActSeg'
lafilestru[3,2] = 'N'
lafilestru[3,3] = 1
lafilestru[3,4] = 0
lafilestru[4,1] = 'cFree'
lafilestru[4,2] = 'C'
lafilestru[4,3] = 19
lafilestru[4,4] = 0


CREATE DBF (gcWorkDir+lcTmpSgVal) FROM ARRAY lafilestru
INDEX ON STR(nRecNo,6)+STR(nActSeg,1) TAG lcRecExp
INDEX ON cMarker+STR(nActSeg,1)+STR(nRecNo,6) TAG lcSegExp
SET ORDER TO TAG lcRecExp

DIMENSION laSegVal[9]
laSegVal  = 0
lcTmpNMgr = gfTempName()
SELECT (lcColorFil)
LOCATE FOR cStatus='A'
lcRemPBs=IIF(FOUND(),'ENABLE','DISABLE')
SELECT * FROM (lcColorFil) INTO DBF (gcWorkDir+lcTmpNMgr)
SELECT (lcTmpNMgr)
IF llMScale
  REPLACE ALL Style WITH SUBSTR(Style,1,laSegInfo[ALEN(laSegInfo,1),4]-2)+lcSclSepr
ENDIF
INDEX ON Style TAG (lcTmpNMgr) UNIQUE
SET ORDER TO TAG (lcTmpNMgr)
GO TOP

=ACOPY(laSegInfo,laValues,(lnNoMsg*9)+1,-1)
lnArLn=ALEN(laSegInfo,1)-lnNoMsg
DIME laValues[lnArLn,9]
IF llMScale
  *--Remove scale array row.
  lnArLn=ALEN(laValues,1)
  DIME laValues[lnArLn-1,9]
ENDIF
lnArLn=ALEN(laValues,1)

FOR I=1 TO lnArLn
  lcFile=laValues[I,6]
  lcTag =laValues[I,7]
  lcKey =laValues[I,8]
  = gfOpenFile(gcDataDir+lcFile,lcTag,'SH')

  SELECT (lcFile)
  DO CASE
    CASE lcFile='ICSEGVAL'
      lcWhilEXp = "cISegNo+cISegVal=lcKey"
    CASE lcFile='CODES'
      *E301209,4 Fixed wrong seek on code key.
      *lcWhilEXp = "crltfield+cfld_name=lcKey"
      lcWhilEXp = "CDEFCODE+crltfield+cfld_name=lcKey"
      *E301209,4 End.
  ENDCASE
  SEEK lcKey
  SCAN WHILE &lcWhilEXp
    SELECT (lcTmpSgVal)
    IF !SEEK(STR(RECNO(lcFile),6)+STR(I,1))
      APPEND BLANK
      REPLACE nRecNo WITH RECNO(lcFile),nActSeg WITH I
    ENDIF
  ENDSCAN
ENDFOR

SELECT (lcTmpSgVal)
PUSH KEY
ON KEY
=lftrapky()
DO (gcScrDir+gcWinAppl+"\ICNMjr.SPX")
=lfUntrapky()
ON KEY
POP KEY
RETURN



*!*************************************************************
*! Name      : lfUpdScle
*! Purpose   : Update for scale segment.
*!*************************************************************
FUNCTION lfUpdScle

*B607888,1 TMI [Start] fill the tax break array
PRIVATE laTxBrArr
DIMENSION laTxBrArr[ALEN(laExtSz,1)]
SELECT &lcColorFil
SET FILTER TO CSTATUS = 'S'
GO TOP
IF !EOF()
  FOR lnI = 1 TO ALEN(laTxBrArr)
    laTxBrArr[lnI] = nTaxBreak
    *B608235,1 TMI [Start] check if the existing of the scale of laExtSz in &lcColorFil..SCALE 
    IF &lcColorFil..SCALE == PADR(laExtSz[lnI],3)
      *B608235,1 TMI [End  ] 
      SKIP
      *B608235,1 TMI [Start] if eof go bottom
      IF EOF()
        GO BOTTOM
      ENDIF
    ENDIF
    *B608235,1 TMI [End  ] 
  ENDFOR
ENDIF
SET FILTER TO 
GO TOP
*B607888,1 TMI [Start] 

SELECT (lccolorfil)    
SELECT * FROM (lccolorfil) INTO CURSOR lcTmpCurs
SELECT (lccolorfil)    
DELETE ALL FOR cStatus='A'
SELECT lcTmpCurs
SCAN FOR cStatus='A'
  lcStyCode=SUBSTR(Style,1,laSegInfo[ALEN(laSegInfo,1),4]-IIF(EMPTY(lcSclSepr),1,2))+lcSclSepr
  SCATTER MEMVAR
  SELECT (lcColorfil)
  SET DELE OFF
  FOR I=1 TO ALEN(laExtSz)
    IF !SEEK(lcStyCode+laExtSz[I])      
      APPEND BLANK
      GATHER MEMVAR
      REPLACE Style WITH lcStyCode+laExtSz[I],;
              Desc1 WITH ALLTRIM(Desc1)+' '+IIF(SEEK('S'+laExtSz[I],'SCALE'),SCALE.cScl_Desc,'')
    ELSE
      IF DELETED()
        RECALL
      ENDIF   
    ENDIF 
    *B607888,1 TMI [Start] update the tax break field
    IF !EMPTY(laTxBrArr)
      REPLACE NTAXBREAK  WITH laTxBrArr[I]
    ENDIF
    *B607888,1 TMI [End  ] 
  ENDFOR
  SET DELE ON
ENDSCAN
USE IN lcTmpCurs
RETURN





*!*************************************************************
*! Name      : lfActBr1
*! Purpose   : Segment value browse.
*!*************************************************************
FUNCTION lfActBr1

=lfActPad()
lcFile =laValues[lnActSeg,6]
lcKey  =laValues[lnActSeg,8]
lnSize =laValues[lnActSeg,5]
lnStart=laValues[lnActSeg,4]-lnStyleWid-1

DO CASE
  CASE lcFile='CODES'
    lcCField = "cCode_no :R :H='Segment Val' :20,cDiscrep :R :H='Description' :40 "
    lcEField = "cCode_no"
    lcSeekExp = lcKey
    lcBrowTtl1='Codes'
  CASE lcFile='ICSEGVAL'
    lcCField = "ciSegVal :R :H='Segment Val' :20,ciSgValsD :R :H='Description' :40 "
    lcEField = "ciSegVal"
    lcSeekExp = lcKey+SUBSTR(lcSegVal,lnStart,lnSize)
    lcBrowTtl1='Segment_Values'
ENDCASE


SELECT (lcFile)
llFound=.F.
IF ! EMPTY(SUBSTR(lcSegVal,lnStart,lnSize))
  SEEK lcSeekExp
  IF lcFile='CODES'
    *LOCATE REST WHILE ccomp_id+crltfield+cfld_name=lcSeekExp FOR cCode_no=SUBSTR(lcSegVal,lnStart,lnSize)
    LOCATE REST WHILE cDefCode+crltfield+cfld_name=lcSeekExp FOR cCode_no=SUBSTR(lcSegVal,lnStart,lnSize)
  ENDIF
  llFound=FOUND()
ENDIF
IF !llFound
  SEEK lcKey
ENDIF


lnCurRec1 = RECNO()
=lfwBr1(.T.)
*E301209,4 Fixed wrong code display.
*lcBr1Fields = "lcPoint=IIF(lnCurRec1=RECNO(),'>',' '):1:H=' ':W=.F.,"+;
              "lcMarkr=IIF(SEEK(STR(RECNO(lcFile),6),lcTmpSgVal),&lcTmpSgVal..cMarker,' ') :R :1 :H=' ',"+lcCField
lcBr1Fields = "lcPoint=IIF(lnCurRec1=RECNO(),'>',' '):1:H=' ':W=.F.,"+;
              "lcMarkr=IIF(SEEK(STR(RECNO(lcFile),6)+STR(lnActSeg,1),lcTmpSgVal),&lcTmpSgVal..cMarker,' ') :R :1 :H=' ',"+lcCField
*E301209,4 End.

BROWSE FIELDS &lcBr1Fields ;
       LOCK 0   ;
	   NOAPPEND ;
	   NOCLEAR  ;
	   NODELETE ;
	   NOEDIT   ;
	   NOMENU   ;
	   NOWAIT   ;
	   SAVE     ;
       KEY lcKey ;
       WHEN lfwBr1(.F.) ;
	   TITLE lcBrowTtl1    ;
	   WINDOW ICNMjr2 IN WINDOW ICNMjr

IF laValues[lnActSeg,3]='F'
  SHOW GET pbAddFree ENABLE
ELSE
  SHOW GET pbAddFree DISABLE
ENDIF
RETURN


*!*************************************************************
*! Name      : lfwBr1
*! Purpose   : When function for Segment value browse.
*!*************************************************************
FUNCTION lfwBr1
PARA llOut

SELECT (lcFile)
=SEEK(STR(RECNO(lcFile),6)+STR(lnActSeg,1),lcTmpSgVal)
lnCurRec1=RECNO()
lcSegVal =SUBSTR(lcSegVal,1,lnStart-1)+PADR(ALLTRIM(EVALUATE(lcEField)),lnSize,' ')+;
          SUBSTR(lcSegVal,lnStart+lnSize,lnColorWid-lnStart+lnSize)

IF !llOut
  SHOW WINDOW (lcBrowTtl1) REFRESH
ENDIF
=lfRefresh('ICNMjr1')
RETURN

*!*************************************************************
FUNCTION lfwSegChng
IF MDOWN()
  lnActSeg = VAL(SUBSTR(VARREAD(),ATC('(',VARREAD())+1,1))
  =lfActBr1()
  =lfRefresh()
  KEYBOARD '{ALT+B}'
ENDIF
RETURN .F.
*!*************************************************************
FUNCTION lfMarkSeg
lnMark = 1.00
IF lnActSeg>1
  lnMark = ((laValues[lnActSeg,4]-lnStyleWid-1)*1.700)  
ENDIF
RETURN lnMark
*!*************************************************************
FUNCTION lfIncSeg
PARAMETERS lnIncSeg
lnActSeg = lnActSeg+lnIncSeg
lnActSeg = IIF(lnActSeg>lnArLn,1,IIF(lnActSeg<1,lnArLn,lnActSeg))
=lfActBr1()
=lfRefresh()
RETURN




*!*************************************************************
*! Name      : lfvSel1
*! Purpose   : Select valid function for Segment value browse.
*!*************************************************************
FUNCTION lfvSel1

SELECT (lcTmpSgVal)
=SEEK(STR(RECNO(lcFile),6)+STR(lnActSeg,1))
REPLACE cMarker WITH IIF(EMPTY(cMarker),CHR(187),' ')
=lfwBr1(.F.)

*!*************************************************************
*! Name      : lfvSel1A
*! Purpose   : Select all valid function for Segment value browse.
*!*************************************************************
FUNCTION lfvSel1A

SELECT (lcFile)
lnSvRcn=IIF(!EOF(),RECNO(),0)
SELECT (lcTmpSgVal)
REPLACE ALL cMarker WITH CHR(187) FOR nActSeg=lnActSeg
SELECT (lcFile)
IF lnSvRcn<>0
  GOTO lnSvRcn
  =SEEK(STR(RECNO(lcFile),6)+STR(lnActSeg,1),lcTmpSgVal)
ENDIF
=lfwBr1(.F.)
RETURN


*!*************************************************************
*! Name      : lfvSel1N
*! Purpose   : Select non valid function for Segment value browse.
*!*************************************************************
FUNCTION lfvSel1N

SELECT (lcFile)
lnSvRcn=IIF(!EOF(),RECNO(),0)
SELECT (lcTmpSgVal)
REPLACE ALL cMarker WITH ' ' FOR nActSeg=lnActSeg
SELECT (lcFile)
IF lnSvRcn<>0
  GOTO lnSvRcn
  =SEEK(STR(RECNO(lcFile),6)+STR(lnActSeg,1),lcTmpSgVal)
ENDIF
=lfwBr1(.F.)
RETURN


*!*************************************************************
*! Name      : lfvInvrt
*! Purpose   : Invert valid function for Segment value browse.
*!*************************************************************
FUNCTION lfvInvrt

SELECT (lcFile)
lnSvRcn=IIF(!EOF(),RECNO(),0)
SELECT (lcTmpSgVal)
REPLACE ALL cMarker WITH IIF(EMPTY(cMarker),CHR(187),' ') FOR nActSeg=lnActSeg
SELECT (lcFile)
IF lnSvRcn<>0
  GOTO lnSvRcn
  =SEEK(STR(RECNO(lcFile),6)+STR(lnActSeg,1),lcTmpSgVal)
ENDIF
=lfwBr1(.F.)
RETURN





*!*************************************************************
*! Name      : lfActBr2
*! Purpose   : Non Majors codes browse.
*!*************************************************************
FUNCTION lfActBr2

SELECT (lcTmpNMgr)
lnCurRec2 = RECNO()

=lfwBr2(.T.)
lcBr2Fields = "lcPoint1=IIF(lnCurRec2=RECNO(),'>',' '):1:H=' ':W=.F.,"+;
              "lcSegm=SUBSTR(Style,lnStyleWid+2,lnColorWid) :20 :R :H='Code',"+;
              "Desc  :30 :R :H='Description',"+;
              "Desc1 :60 :R :H='Long Description'"
BROWSE FIELDS &lcBr2Fields ;
       LOCK 0   ;
	   NOAPPEND ;
	   NOCLEAR  ;
	   NODELETE ;
	   NOEDIT   ;
	   NOMENU   ;
	   NOWAIT   ;
	   SAVE     ;
       WHEN lfwBr2(.F.) ;
	   TITLE lcBrowTtl2 ;
	   WINDOW ICNMjr5 IN WINDOW ICNMjr

SELECT (lcFile)
RETURN


*!*************************************************************
*! Name      : lfwBr2
*! Purpose   : When function for Non Majors browse.
*!*************************************************************
FUNCTION lfwBr2
PARA llOut

lnCurRec2=RECNO()
IF !llOut
  SHOW WINDOW (lcBrowTtl2) REFRESH
ENDIF
RETURN

*!*************************************************************
*! Name      : lfvClsExit
*! Purpose   : Close and Exit with updating main temp.
*!*************************************************************
FUNCTION lfvClsExit

SELECT (lcColorFil)
DELETE ALL FOR cstatus='A'
SELECT (lcTmpNMgr)
SET ORDER TO TAG (lcTmpNMgr)
SCAN FOR cstatus='A'
  IF !SEEK(&lcTmpNMgr..Style,lcColorFil)
    glupdated=.T.
    SELECT (lcColorFil)
    APPEND BLANK
    GATHER FROM laData FIELDS &lcScFields MEMO    
    REPLACE Style   WITH &lcTmpNMgr..Style,;
            Desc    WITH &lcTmpNMgr..Desc ,;    
            Desc1   WITH IIF(!EMPTY(laData[4]),laData[4],&lcTmpNMgr..Desc1),;
            cStatus WITH 'A'
    SELECT (lcTmpNMgr)
  ENDIF
ENDSCAN


*!*************************************************************
*! Name      : lfvAddSeg
*! Purpose   : Add Segments.
*!*************************************************************
FUNCTION lfvAddSeg

*--Check that all segment are marked.
FOR I=1 TO lnArLn
  SELECT (lcTmpSgVal)
  LOCATE FOR nActSeg=I AND !EMPTY(cMarker)
  IF !FOUND()
    *--You have to select segment value no : X
    =gfModalGen('TRM42077B42001','DIALOG',STR(I,1))
    RETURN
  ENDIF  
ENDFOR

SELECT (lcTmpSgVal)
SET ORDER TO TAG lcSegExp
SET FILTER TO !EMPTY(cMarker)
=lfAddSty(lnArLn,1,lcMajor+lcSepart,@laValues,@lcTmpNMgr,laData[3])
SELECT (lcTmpSgVal)
SET FILTER TO
REPLACE ALL cMarker WITH ' '
GO TOP
SET ORDER TO TAG lcRecExp
SELECT (lcTmpNMgr)
SET ORDER TO
SCAN FOR cStatus='A'
  lcCodeVl=''
  FOR I=lnStyleWid+2 TO LEN(Style)
    IF SUBSTR(lcIMjrPt,I,1)<>'X'
      lcCodeVl=lcCodeVl+SUBSTR(lcIMjrPt,I,1)
      IF SUBSTR(lcIMjrPt,I,1)<>SUBSTR(Style,I,1)
        lcCodeVl=lcCodeVl+SUBSTR(Style,I,1)
      ENDIF
    ELSE
      lcCodeVl=lcCodeVl+SUBSTR(Style,I,1)        
    ENDIF
  ENDFOR
  REPLACE Style WITH lcMajor+lcSepart+lcCodeVl
  STORE '' TO lcCdDesc,lcDesc1
  FOR I=1 TO lnArLn
    =lfGetCode(laValues[I,3],laValues[I,8],SUBSTR(Style,laValues[I,4],laValues[I,5]),laValues[I,6],.T.)
    lcDesc1=lcDesc1+ALLTRIM(lcCdDesc)+' '
  ENDFOR
  REPLACE Desc1 WITH IIF(!EMPTY(laData[4]),laData[4],ALLTRIM(Desc)+' '+lcDesc1)

ENDSCAN
SET ORDER TO TAG (lcTmpNMgr)
GO TOP
IF EOF()
  SHOW GET pbRemove DISABLE
ELSE
  SHOW GET pbRemove ENABLE
ENDIF  
=lfwBr2(.F.)
SHOW WINDOW (lcBrowTtl1) REFRESH
RETURN


****************************************
FUNCTION lfAddSty
PARA lnTotSeg,lnCurrSeg,lcStyle,laSgSize,lcTmpfile,lcSdesc
PRIVATE ALL LIKE l* 

SCAN FOR nActSeg=lnCurrSeg
  lnRec=RECNO()
  lnSize = laSgSize[nActSeg,5]
  lcFile = laSgSize[nActSeg,6]
  lnCurRc= nRecNo
  GOTO lnCurRc IN (lcFile)
  lcEField=IIF(lcFile='CODES',"cCode_no","ciSegVal")
  lcDField=IIF(lcFile='CODES',"cDiscrep","ciSgValSd")
  lcCode = PADR(ALLTRIM(&lcFile..&lcEField),lnSize,' ')
  
  IF lnCurrSeg = lnTotSeg
     IF !SEEK(lcStyle+lcCode,lcTmpfile) 
       lnWEra=SELECT()
       SELECT (lcTmpfile)
       APPEND BLANK 
       REPLACE Style WITH lcStyle+lcCode,;
               Desc  WITH lcSdesc,;
               Desc1 WITH lcSdesc,;  
               cStatus WITH 'A' 
       SELECT(lnWEra)
     ENDIF 
  ELSE
    =lfAddSty(lnTotSeg,(lnCurrSeg+1),lcStyle+lcCode,@laSgSize,@lcTmpfile,lcSdesc)
  ENDIF
  GO lnRec
ENDSCAN
RETURN



*!*************************************************************
*! Name      : lfvRemNMjr
*! Purpose   : Remove Selected Non Major.
*!*************************************************************
FUNCTION lfvRemNMjr

SELECT (lcTmpNMgr)
GOTO lnCurRec2
lcDelRec=Style
IF cStatus<>'A'
  *--You cannot remove this non major because it has not been entered in this session, Use delete option instead.
  =gfModalGen('TRM42050B42001','DIALOG')
  RETURN
ENDIF
SET ORDER TO
DELETE FOR Style=lcDelRec AND cStatus='A'
PACK
SET ORDER TO TAG (lcTmpNMgr)
GO TOP
IF EOF()
  SHOW GET pbRemove DISABLE
ELSE
  lnCurRec2=RECNO()
  SHOW GET pbRemove ENABLE
ENDIF  
=lfActBr2()
SHOW WINDOW (lcBrowTtl2) REFRESH
RETURN


*!*************************************************************
*! Name      : lfAddFree
*! Purpose   : Add free segments.
*!*************************************************************
FUNCTION lfAddFree

STORE ' ' TO lcFreeSeg,lcFSDesc,lcFLDesc 
PUSH KEY
ON KEY
DO (gcScrDir+gcWinAppl+"\ICNMjrA.SPX")
ON KEY
POP KEY
RETURN

*!*************************************************************
*! Name      : lfAddFsg
*! Purpose   : Edit a free segments.
*!*************************************************************
FUNCTION lfAddFsg

SELECT ICSEGVAL
APPEND BLANK
REPLACE cisegno   WITH STR(lnFSegNo+lnMajSeg,1),;
        cisegval  WITH lcFreeSeg,;
        cisgvalsd WITH lcFSDesc ,;
        cisgvalld WITH lcFLDesc ,;
        dAdd_date WITH gdSysDate,;
        cAdd_time WITH TIME()   ,;
        cAdd_user WITH gcUser_id

SELECT (lcTmpSgVal)
APPEND BLANK
REPLACE nRecNo  WITH RECNO('ICSEGVAL'),;
        nActSeg WITH lnFSegNo

SHOW WINDOW (lcBrowTtl1) REFRESH


*!*************************************************************
FUNCTION lfvFSeg
IF SEEK(STR(lnFSegNo+lnMajSeg,1)+lcFreeSeg,'ICSEGVAL')
  *--This segment value already exist.
  =gfModalGen('TRM42078B42001','DIALOG')
  lcFreeSeg=' ' 
ENDIF
RETURN


*!*************************************************************
*! Name    : lfActPad,lfTrapky,lfUntrapky,lpTrpEntr,
*            lpTrEsc,lpTrtab,lpTrBtab
*! Purpose : Screen Traping functions.
*!*************************************************************
FUNCTION lfActPad

DEFINE BAR 100 OF P01PU01 PROMPT " " KEY ALT+B
DEFINE BAR 101 OF P01PU01 PROMPT "Next Segment" KEY RIGHTARROW
DEFINE BAR 102 OF P01PU01 PROMPT "Previous Segment" KEY LEFTARROW
ON SELECTION BAR 100 OF P01PU01 ACTIVATE WINDOW (lcBrowTtl1)
ON SELECTION BAR 101 OF P01PU01 DO lfIncSeg WITH 1
ON SELECTION BAR 102 OF P01PU01 DO lfIncSeg WITH -1
RETURN

FUNCTION lfTrapky
ON KEY LABEL TAB      DO lpTrtab
ON KEY LABEL backtab  DO lpTrBtab
ON KEY LABEL ESC      DO lpTrEsc
ON KEY LABEL ENTER    DO lpTrpEntr
RETURN

FUNCTION lfUntrapky
RELEASE BAR 100 OF P01PU01 
RELEASE BAR 101 OF P01PU01 
RELEASE BAR 102 OF P01PU01 
ON KEY LABEL TAB    
ON KEY LABEL backtab
ON KEY LABEL ESC
ON KEY LABEL ENTER  
RETURN

FUNCTION lpTrpEntr
IF WONTOP(lcBrowTtl1)
  =lfvSel1()
ENDIF
RETURN

FUNCTION lpTrEsc
_CUROBJ=OBJNUM(pbClose)
KEYBOARD '{ENTER}' 
RETURN

FUNCTION lpTrtab
DO CASE
  CASE WONTOP(lcBrowTtl1)
     ACTIVATE WINDOW ICNMJR3
    _CUROBJ=OBJNUM(pbSel1)
  CASE WONTOP(lcBrowTtl2) AND !EOF(lcTmpNMgr)
     ACTIVATE WINDOW ICNMJR6
     _CUROBJ=OBJNUM(pbRemove)
  CASE WONTOP(lcBrowTtl2) AND EOF(lcTmpNMgr)
     ACTIVATE WINDOW ICNMJR6
     _CUROBJ=OBJNUM(pbClose)
  CASE _CUROBJ=OBJNUM(pbinvrt1)
     ACTIVATE WINDOW ICNMJR1
    _CUROBJ=OBJNUM(pbAdd)
  CASE _CUROBJ=OBJNUM(pbAdd)
    ACTIVATE WINDOW (lcBrowTtl2)
  CASE _CUROBJ=OBJNUM(pbClose)
    ACTIVATE WINDOW (lcBrowTtl1)
  OTHE 
    _CUROBJ=_CUROBJ+1
ENDCASE

FUNCTION lpTrBtab
DO CASE
  CASE WONTOP(lcBrowTtl1)
     ACTIVATE WINDOW ICNMJR6
    _CUROBJ=OBJNUM(pbClose)
  CASE WONTOP(lcBrowTtl2)
     ACTIVATE WINDOW ICNMJR1
    _CUROBJ=OBJNUM(pbAdd)  
  CASE _CUROBJ=OBJNUM(pbRemove) AND !EOF(lcTmpNMgr)
    ACTIVATE WINDOW (lcBrowTtl2)
  CASE _CUROBJ=OBJNUM(pbClose) AND EOF(lcTmpNMgr)
    ACTIVATE WINDOW (lcBrowTtl2)
  CASE _CUROBJ=OBJNUM(pbAdd)
     ACTIVATE WINDOW ICNMJR3
    _CUROBJ=OBJNUM(pbinvrt1)
  CASE _CUROBJ=OBJNUM(pbSel1)
    ACTIVATE WINDOW (lcBrowTtl1)
  OTHE 
    _CUROBJ=_CUROBJ-1
ENDCASE
RETURN


*!*************************************************************
*! Name      : lfSamDsc
*! Developer : ABD ELRAHIM KOURA (KAM)
*! Date      : 20/02/2001
*! Purpose   : To check if all color use user ong description or not
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : non
*!*************************************************************
*! Return      : .t. or .f.
*!*************************************************************
*! Example     : =lfSamDsc ()
*!*************************************************************
*!B604115,4
*!*************************************************************
FUNCTION lfSamDsc
PRIVATE lcOldArea,lnRecNo,lcDesc,llRetVal
lcOldArea=SELECT()
lnRecNo=RECNO()
llRetVal=.F.
SELECT (lcColorFil)
GO TOP
*-- get first desc1
lcDesc=DESC1
*--scan all records if found differnce return .t. else  return .f.
SCAN
  IF ALLTRIM(DESC1)<>ALLTRIM(lcDesc)
    llRetVal=.T.
    EXIT
  ENDIF
ENDSCAN
SELECT(lcOldArea)
IF BETWEEN(lnRecNo,1,RECCOUNT())
  GO lnRecNo
ENDIF
RETURN llRetVal
*-- end function lfSamDsc