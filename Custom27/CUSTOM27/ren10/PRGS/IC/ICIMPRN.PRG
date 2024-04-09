*:****************************************************************************
*: Program file         : ICIMPRN.PRG
*: Program description  : Import Styles from ASCII text file for RENA LANG
*: For System           : Aria Advantage Series - Version 2.7
*: For Module           : IC2 - INVENTORY CONTROL
*: Developer Name       : TMI - TAREK MOHAMED IBRAHIM
*: Tracking Entry Number: C102763
*: DATE                 : 11/19/2002
*:****************************************************************************
*MODIFICATIONS
*C102763,4 TMI 12/12/2002 Add a popup for Division on the Final browse screen
*:***************************************************************************

*--Check codes
lcCodName = lfChkCode()
IF !EMPTY(lcCodName)
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,lcCodName + ;
  ' codes has not been set up. Please, set up and try again.')
  RETURN 
ENDIF
*C102763,4 TMI [Start] *Define default codes vars
STORE ' ' TO lcDefDiv,lcDefRoy,lcDefDsc,lcDefPrc
*--Get default codes values
=lfGetDef()
*C102763,4 TMI [End  ] 

lcTempName = gfTempName()
lcVldSty   = gfTempName()
STORE 0 TO lnLogHndl,lnDMarker
STORE '' TO lcSlctedfl
lcDefWH = SPACE(6)
llViewLog = .F.
llLogHdr = .F.

DIMENSION laStyle[11],laLogs[11]
*C102763,4 TMI [Start] Define Array for Division to fill DIvision Popup 
DECLARE laCodes[1,10],laDivision[1,2]
laCodes = ''
laCodes[1,1] = 'CDIVISION'
laCodes[1,2] = 'laDivision'
laCodes[1,3] = 'lnDivision'
laCodes[1,4] = ''
laCodes[1,5] = .F.
laCodes[1,6] = .F.
laCodes[1,10] = 'cDivision'
lnDivision = 1
=gfwCodePop(@laCodes,'CDIVISION','L')
lnDivision = ASUBSCRIPT( laDivision , ASCAN(laDivision,lcDefDiv) , 1 )
*C102763,4 TMI [End  ] 

DO (gcScrDir+'IC\ICIMPRN.SPX')

*:**************************************************************************
*:* Name        : lfvSlctfl
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/19/2002
*:* Purpose     : Select ASCII text file
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvSlctfl()
*:***************************************************************************
FUNCTION lfvSlctfl
PRIVATE lcImp
SET DEFA TO &gcDEF_PATH
lcSlctedfl = GETFILE('TXT','Select Style ASCII file','Select')
lcSlctedfl = IIF(!FILE(lcSlctedfl),'',lcSlctedfl)
lcImp = IIF(!EMPTY(lcSlctedfl),'ENABLE','DISABLE')
SHOW GET lcSlctedfl DISABLE
SHOW GET pbImport   &lcImp
_CUROBJ = OBJNUM(pbImport)
  
*-- end of lfvSlctfl.

*:**************************************************************************
*:* Name        : lfvImport
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/19/2002
*:* Purpose     : The import process of styles from the ASCII text file
*:***************************************************************************
*:* Called from : Screen
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvImport()
*:***************************************************************************
FUNCTION lfvImport

lnHndl = FOPEN(lcSlctedfl)
IF lnHndl < 0
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'The file '+lcSlctedfl+' cannot be opened')
  RETURN
ELSE  
  =FCLOSE(lnHndl)
ENDIF

*-- Open needed files
=lfOpenfls()

*--Get Default WH
SELECT WAREHOUS
GO TOP
lcDefWH = WAREHOUS.CWARECODE 

*--Create log file
lnLogHndl = FCREATE(gcWorkDir+lcTempName+'.TXT')
IF lnLogHndl < 0 
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Cannot create the log file')
  RETURN
ENDIF 

*--Create needed temp files
=lfCrtTmp()

*--Loop throgh text file to process importing

SELECT &lcTempName
SCAN
  lcReadStr = ALLTRIM('|'+&lcTempName..STYINFO)
  STORE ' ' TO laStyle,laLogs  
  IF lfVldStyle()              && if this style is valid
    M.STYLE   = PADR(laStyle[1],12)+'-'+PADR(laStyle[7],6)
    M.DESC    = PADR(laStyle[2],20)
    M.CLRDESC = ALLT(gfCodDes(laStyle[7],'COLOR'))
    M.SCALE   = SCALE.SCALE
    M.SCLDESC = SCALE.CSCL_DESC 
    M.SEASON  = laStyle[4]
    *C102763,4 TMI [Start] Populate the DIvision field with the Default Divisio code
    M.CDIVISION = laDivision[lnDivision,2]
    *C102763,4 TMI [End  ] 
    M.GROUP   = laStyle[5]
    M.PATTERN = laStyle[6]
    M.COST    = VAL(laStyle[9])/100
    M.PRICEA  = VAL(laStyle[10])/100
    M.PRICEB  = VAL(laStyle[11])/100
    M.IMPORT  = 'Yes'
    INSERT INTO &lcVldSty FROM MEMVAR
  ELSE    
    =lfPopLgFil()
  ENDIF
ENDSCAN

*-- View rejection log file , if any
=lfViewRLog()

*-- Show the final browse to select and edit styles
=lfPopDtFls()

USE IN &lcTempName
USE IN &lcVldSty
*-- end of lfvImport.

*:**************************************************************************
*:* Name        : lfOpenfls
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/19/2002
*:* Purpose     : Open needded files
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfOpenfls()
*:***************************************************************************
FUNCTION lfOpenfls
=gfOpenFile(gcDataDir+'STYLE','STYLE','SH')
=gfOpenFile(gcDataDir+'STYDYE','STYDYE','SH')
=gfOpenFile(gcDataDir+'ICSTYHST','STYHST','SH') 
=gfOpenFile(gcDataDir+'FISHD','COMPFYEAR','SH')
=gfOpenFile(gcDataDir+'STYPRICE','STYPRICE','SH')
=gfOpenFile(gcDataDir+'ICSEGVAL','SEGVAL','SH')
=gfOpenFile(gcDataDir+'SCALE','SCALE','SH')
=gfOpenFile(gcDataDir+'BOM','BOM','SH')
=gfOpenFile(gcDataDir+'WAREHOUS','WAREHOUS','SH')
=gfOpenFile(gcDataDir+'CODES','CCODE_NO','SH')     && CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM

*-- end of lfOpenfls.

*:**************************************************************************
*:* Name        : lfCrtTmp
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/19/2002
*:* Purpose     : Create needed temp files
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfCrtTmp()
*:***************************************************************************
FUNCTION lfCrtTmp

*--Create and fill the temp file from the imported text file
CREATE CURSOR &lcTempName (STYINFO C(150))
APPEND FROM &lcSlctedfl SDF

*--The file that contains Valid styles
*C102763,4 TMI [Start] Add CDIVISION field
*CREATE CURSOR &lcVldSty (STYLE C(19),DESC C(20),CLRDESC C(20),SCALE C(3),SCLDESC C(20),;
   SEASON C(6),GROUP C(6),PATTERN C(10),COST N(7,2),PRICEA N(7,2),PRICEB N(7,2),IMPORT C(3))
CREATE CURSOR &lcVldSty (STYLE C(19),DESC C(20),CLRDESC C(20),SCALE C(3),SCLDESC C(20),;
   SEASON C(6),CDIVISION C(6),GROUP C(6),PATTERN C(10),COST N(7,2),PRICEA N(7,2),PRICEB N(7,2),IMPORT C(3))
*C102763,4 TMI [End  ]    
INDEX ON STYLE TAG &lcVldSty

*-- end of lfCrtTmp.

*:**************************************************************************
*:* Name        : lfVldStyle
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/19/2002
*:* Purpose     : Apply validation conditions on text file fields
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfVldStyle()
*:***************************************************************************
FUNCTION lfVldStyle
PRIVATE llValid
llValid = .T.

*--Fill the array laStyle with line data
FOR lnCnt = 1 TO OCCURS('|',lcReadStr) - 1
  laStyle[lnCnt] = SUBSTR(lcReadStr,AT('|',lcReadStr,lnCnt)+1,;
                                    AT('|',lcReadStr,lnCnt+1)-AT('|',lcReadStr,lnCnt)-1)
  laStyle[lnCnt] = UPPER(ALLTRIM(laStyle[lnCnt]))                                    
ENDFOR  

DIMENSION laClr[1]     
*--Check if color is in codes file
IF !SEEK('NCOLOR     '+PADR(laStyle[7],6),'CODES')
  llValid = .F.
  laLogs[1] = 'Color ' + laStyle[7] + ' does not exist in the codes library.'  
ELSE
  IF SEEK(PADR(laStyle[1],12)+'-'+PADR(laStyle[7],6) , 'STYLE')
    llValid = .F.
    laLogs[1] = 'Style color ' + PADR(laStyle[1],12)+'-'+PADR(laStyle[7],6) + ;
                   ' already exists in the STYLE file.'
  ENDIF
ENDIF

*--Check that Description is not empty
IF EMPTY(laStyle[2])
  llValid = .F.
  laLogs[2] = 'Style color ' + PADR(laStyle[1],12)+'-'+PADR(laStyle[7],6) + ;
                 ' has no description.'
ENDIF                 

*--Check scale
SELECT SCALE
LOCATE FOR PADR(UPPER(ALLT(SCALE.CSCL_DESC)),10) = PADR(laStyle[3],10)
IF !FOUND()
  llValid = .F.
  laLogs[3] = 'Scale ' + laStyle[3] +' Not found.'
ENDIF 

*--Check season 
IF !SEEK('NSEASON    '+PADR(laStyle[4],6),'CODES')
  llValid = .F.
  laLogs[4] = 'Season ' + laStyle[4] + ' does not exist in the codes library.'  
ENDIF

*--Check Group
IF !SEEK('NCSTYGROUP '+PADR(laStyle[5],6),'CODES')
  llValid = .F.
  laLogs[5] = 'Style Group ' + laStyle[5] + ' does not exist in the codes library.'  
ENDIF

RETURN llValid
*-- end of lfVldStyle.


*:**************************************************************************
*:* Name        : lfPopLgFil
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/20/2002
*:* Purpose     : Populate Log file with one style a time
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfPopLgFil()
*:***************************************************************************
FUNCTION lfPopLgFil
PRIVATE lnI,lcSpace
IF !EMPTY(laStyle[1])

  *--Log Header
  IF !llLogHdr
    =FPUTS(lnLogHndl,'')
    =FPUTS(lnLogHndl,'STYLE ASCII FILE REJECTION REPORT')
    =FPUTS(lnLogHndl,'---------------------------------')
    =FPUTS(lnLogHndl,'')
    llLogHdr = .T.
  ENDIF

  lcSpace = SPACE(7)
  =FPUTS(lnLogHndl,'')
  =FPUTS(lnLogHndl,'Style color '+PADR(laStyle[1],12)+'-'+PADR(laStyle[7],6))
  =FPUTS(lnLogHndl,'')
  =FPUTS(lnLogHndl,lcSpace+lcReadStr)
  =FPUTS(lnLogHndl,'')
  FOR lnI = 1 TO 11
    IF !EMPTY(laLogs[lnI])
      =FPUTS(lnLogHndl,lcSpace+laLogs[lnI])
      llViewLog = .T.
    ENDIF
  ENDFOR

ENDIF

*-- end of lfPopLgFil.

*:**************************************************************************
*:* Name        : lfViewRLog
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/21/2002
*:* Purpose     : View the log file
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfViewRLog()
*:***************************************************************************
FUNCTION lfViewRLog

=FFLUSH(lnLogHndl)
**lnSize = FSEEK(lnLogHndl,0,2)
*-- Close the text file
DO WHILE !FCLOSE(lnLogHndl)
  wait window nowait 'Wait until closing log file..'
ENDDO

IF llViewLog 
  CREATE CURSOR TMPSTR (mStrRep M(10))
  APPEND BLANK
  APPEND MEMO mStrRep FROM (gcWorkDir+lcTempName+'.TXT') OVERWRITE
  lcWinTitl = 'STYLE ASCII FILE REJECTION REPORT'
  DO (gcScrDir + 'IC\ICSTRREP.SPX')
  USE IN TMPSTR
ENDIF

*-- end of lfViewRLog.

*:**************************************************************************
*:* Name        : lfvPrnt
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/21/2002
*:* Purpose     : print log report
*:***************************************************************************
*:* Called from : lfViewRLog()
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvPrnt()
*:***************************************************************************
FUNCTION lfvPrnt

IF pSetup(.T.)
  gcOutFile = gcWorkDir+gfTempName()+'.TXT'
  COPY MEMO TMPSTR.mStrRep TO &gcOutFile
  gcDevice = 'PRINTER'
  DO ENDREPORT
  gcDevice = 'SCREEN'  
ENDIF
SET DEVICE TO SCREEN

*-- end of lfvPrnt.

*:**************************************************************************
*:* Name        : lfPopDtFls
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/20/2002
*:* Purpose     : Populate Data files
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfPopDtFls()
*:***************************************************************************
FUNCTION lfPopDtFls

*--* =>* lcTmpOrd = gfTempName()
*--* =>* CREATE CURSOR (lcTmpOrd) (STYLE C(19))

SELECT &lcVldSty
GO TOP

IF RECCOUNT(lcVldSty) > 0
  llCancel = .F.
  =lfShowBrow()
  
  IF llCancel
    RETURN
  ELSE
  *-- If Import field is "N" for all styles , exit
    SELECT &lcVldSty
    LOCATE FOR IMPORT = 'Y'
    IF !FOUND()
      =gfModalGen('INM00000B00000',.F.,.F.,.F.,'All valid styles are ignored')
      RETURN
    ENDIF
  ENDIF
ELSE
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'No valid styles to Import.')
  RETURN
ENDIF

*--Get Fiscal years
DIMENSION laFisYrs[3]
SELECT FISHD
LOCATE FOR CFISYSTAT = 'P'
FOR lnCnt = 1 TO 3
  laFisYrs[lnCnt] = CFISFYEAR 
  SKIP
ENDFOR

*--variables needed for file icsegval
DECLARE laSegInfo[1,9],laMajSeg[1,1]
STORE ' ' TO laSegInfo

lnMajSeg  = gfItemMask('SM')
lnNonSeg  = gfItemMask('SN')
=gfItemMask(@laMajSeg)

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

*:       *****************************< Importing >*************************       :*


*C102763,4 TMI [Start] Comment these lines and move it at start of the program
**--Define default codes vars
*STORE ' ' TO lcDefDiv,lcDefRoy,lcDefDsc,lcDefPrc
**--Get default codes values
*=lfGetDef()
*C102763,4 TMI [End  ] 

*--Initialize memo variables
SELECT STYLE
SCATTER MEMVAR BLANK

llStyAdded = .F.

SELECT &lcVldSty
GO TOP
SCAN FOR IMPORT = 'Y'
  m.Style = &lcVldSty..STYLE
  IF !SEEK(m.Style,'STYLE')
    WAIT WINDOW NOWAIT 'Adding Style : '+m.Style

    m.CSTYMAJOR  = PADR(m.Style,12)
    m.CDYE_FLG   = 'N'
    m.Scale      = &lcVldSty..SCALE
    m.Desc       = &lcVldSty..DESC  
    m.Desc1      = ALLT(&lcVldSty..DESC  ) + '  ' + gfCodDes(SUBSTR(m.Style,14,6),'COLOR')
    m.Season     = &lcVldSty..SEASON
    m.cStyGroup  = &lcVldSty..GROUP     
    m.PATTERN    = &lcVldSty..PATTERN       
    m.PRICEA     = &lcVldSty..PRICEA    
    m.PRICEB     = &lcVldSty..PRICEB
    
    STORE  &lcVldSty..COST TO m.NMCOST1,m.NICOST1,m.GROS_PRICE,m.AVE_COST,m.TOTCOST
    m.lDetCost   = .F.
                     
    m.cDefware   = lcDefWH
    m.cStyGrade  = '1'
    m.Status     = 'A'
    m.cPriceCur  = 'USD'
    m.cDutyCur   = 'USD'
    m.NCURRRATE  = 1
    *C102763,4 TMI [Start] Fill Division field from the Temp file for accepted valid sltyles
    *m.cDivision  = lcDefDiv
    m.cDivision  = laDivision[lnDivision,2]
    *C102763,4 TMI [End  ] 
    m.ROYALTY    = lcDefRoy
    m.CDISCCODE  = lcDefDsc
    m.CPURCODE   = lcDefPrc
    m.LINVSTY    = .T.
    M.LMAKE      = .F.
    m.cComcode   = ' '
    m.Vendor     = ' '
    m.cSlsGlLink = 'DEF'    
    m.Link_Code  = 'DEFDEF'
    M.COMMISSION = .T.
    m.nTaxBreak  = 1
    m.cTaxCode   = ' '
    
    *--Add a record to style file
    INSERT INTO STYLE FROM MEMVAR
    =lfAddUsrDt('STYLE')
    
    llStyAdded = .T.
    
    *--Add a record to stydye        
    SELECT STYDYE
    IF !SEEK(m.Style+lcDefWH,'STYDYE')
      APPEND BLANK
      REPLACE STYLE WITH m.Style ,;
              CWARECODE WITH lcDefWH,;
              DESC WITH m.Desc,;
              GL_LINK WITH 'DEFDEF',;
              AVE_COST WITH m.Ave_Cost ,;
              CDISCCODE WITH M.CDISCCODE 
      =lfAddUsrDt('STYDYE')
    ENDIF
   
    *--Create a Bom record
    SELECT BOM
    *key is : CITMMAJOR+TYP+CITMMASK+MFGCODE+ITEM+ICLR
    IF !SEEK(PADR(m.cStyMajor,19)+'1'+PADR(m.Style,19)+'*1    '+SPACE(25),'BOM')
      SELECT BOM
      APPEND BLANK
      REPLACE CITMMAJOR  WITH m.cStyMajor,;
              TYP        WITH '1',;
              CITMMASK   WITH m.Style,;
              MFGCODE    WITH '*1    ',;
              UOM        WITH 'EAC',;
              UNTCOST    WITH m.TotCost,;
              TOTCOST    WITH m.TotCost,;
              NBOMTOTQTY WITH 1  ,;
              CCATGTYP   WITH 'P'
      =lfAddUsrDt()
    ENDIF
   
    *--Add records to icstyhst  
    SELECT ICSTYHST        
    FOR lnK = 1 TO 3
      IF !SEEK(M.STYLE+laFisYrs[lnK],'ICSTYHST')
        APPEND BLANK
        REPLACE STYLE     WITH m.Style,;
                cFisfyear WITH laFisYrs[lnK]
        =lfAddUsrDt('ICSTYHST')
      ENDIF
    ENDFOR        
    
    *--Add  line to ICSEGVAL file
    *--Update the segment value file if there is a free segment in the major.
    FOR lnI=1 TO lnMajSeg
      IF laSegInfo[lnI,3]='F'
        SELECT ICSEGVAL
        IF !SEEK(laSegInfo[lnI,2]+SUBSTR(m.cStyMajor,laSegInfo[lnI,4],laSegInfo[lnI,5]),'ICSEGVAL')
          APPEND BLANK               
          REPLACE cISegNo   WITH laSegInfo[lnI,2],;
                  cISegVal  WITH SUBSTR(m.cStyMajor,laSegInfo[lnI,4],laSegInfo[lnI,5]),;
                  cISgValSd WITH m.Desc,;
                  cISgValLd WITH m.Desc
          =lfAddUsrDt()
        ENDIF
      ENDIF
    ENDFOR          
    WAIT CLEAR

  ENDIF

  *--Re Initialize memo variables
  SELECT STYLE
  SCATTER MEMVAR BLANK
  
ENDSCAN

=gfModalGen('INM00000B00000',.F.,.F.,.F.,;
            IIF(llStyAdded,'Styles imported successfully.','No Styles Imported.'))

*-- end of lfPopDtFls.

*:**************************************************************************
*:* Name        : lfGetDef
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/28/2002
*:* Purpose     : 
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfGetDef()
*:***************************************************************************
FUNCTION lfGetDef
SELECT CODES
SET ORDER TO CCODE_NO

lcDefDiv = lfDefCode('CDIVISION ')   &&*--Get default division
lcDefRoy = lfDefCode('ROYALTY   ')   &&*--Get default Season
lcDefDsc = lfDefCode('CDISCCODE ')   &&*--Get default Discount code
lcDefPrc = lfDefCode('CPURCODE  ')   &&*--Get default Discount code

*-- end of lfGetDef.

*:**************************************************************************
*:* Name        : lfDefCode
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/28/2002
*:* Purpose     : 
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfDefCode()
*:***************************************************************************
FUNCTION lfDefCode
PARAMETERS lcCodeName
PRIVATE lcDefCod
lcDefCod = ' '
IF SEEK('D'+lcCodeName,'CODES')
  lcDefCod = CODES.CCODE_NO  
ENDIF
RETURN lcDefCod
*-- end of lfDefCode.

*:**************************************************************************
*:* Name        : lfShowBrow
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/24/2002
*:* Purpose     : show browse
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfShowBrow()
*:***************************************************************************
FUNCTION lfShowBrow

lcWinCh0    = gfTempName()
lcWinCh2    = gfTempName()
lcWinCh3    = gfTempName()

PUSH KEY
lcScrTtl  = 'Imported Style screen'
*lcDet_Ttl = 'Imported styles browse'
lcDet_Ttl = 'The following styles have successfully passed the validations from the style import file'
ON KEY LABEL TAB     DO lpTap
ON KEY LABEL BACKTAB DO lpBackTap
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcDet_Ttl)
PRIVATE lnAccept
lnAccept = 1

DO (gcScrDir+'IC\ICIMPBR.SPX')

POP KEY

*-- end of lfShowBrow.

*:**************************************************************************
*:* Name        : lfwBrow
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/24/2002
*:* Purpose     : When browse function.
*:***************************************************************************
*:* Called from : ICIMPBR.SPX browse 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfwBrow()
*:***************************************************************************
FUNCTION lfwBrow
SELECT (lcVldSty)
  
lnDMarker  = RECNO()
lnAccept = IIF(IMPORT='Y',1,2)
SHOW GET lnAccept ENABLE

*C102763,4 TMI [Start] Add Division to the browse
*lcBrowFlds = [cMarker =IIF(RECNO()=lnDMarker ,'>',' '):H=' ':R:1:W=.F.,]+;
             [IMPORT  :R :H = 'Accept'      :6    ,]+;
             [STYLE   :R :H = 'Style-Color' :18   ,]+;
             [DESC       :H = 'Style Desc.' :18   ,]+;
             [CLRDESC :R :H = 'Color Desc.' :10   ,]+;
             [SCALE   :R :H = 'Scale'       :5    ,]+;
             [SCLDESC :R :H = 'Scale Desc'  :12   ,]+;
             [SEASON  :R :H = 'Season'      :8    ,]+;
             [GROUP   :R :H = 'Group'       :8    ,]+;
             [PATTERN :R :H = 'Fabric'      :6    ,]+;
             [COST    :R :H = 'Cost'        :6    ,]+;
             [PRICEA  :R :H = 'Price A'     :6    ,]+;
             [PRICEB  :R :H = 'Price B'     :6    ]

lcBrowFlds = [cMarker =IIF(RECNO()=lnDMarker ,'>',' '):H=' ':R:1:W=.F.,]+;
             [IMPORT    :R :H = 'Accept'      :6    ,]+;
             [STYLE     :R :H = 'Style-Color' :18   ,]+;
             [DESC         :H = 'Style Desc.' :18   ,]+;
             [CLRDESC   :R :H = 'Color Desc.' :10   ,]+;
             [SCALE     :R :H = 'Scale'       :5    ,]+;
             [SCLDESC   :R :H = 'Scale Desc'  :12   ,]+;
             [SEASON    :R :H = 'Season'      :8    ,]+;
             [CDIVISION :R :H = 'Division'    :10   ,]+;
             [GROUP     :R :H = 'Group'       :8    ,]+;
             [PATTERN   :R :H = 'Fabric'      :6    ,]+;
             [COST      :R :H = 'Cost'        :6    ,]+;
             [PRICEA    :R :H = 'Price A'     :6    ,]+;
             [PRICEB    :R :H = 'Price B'     :6    ]             
*C102763,4 TMI [End  ] 

BROWSE FIELDS &lcBrowFlds;
       NOAPPEND ;
       NOCLEAR  ;
       NODELETE ;
       NOMENU   ;
       NOWAIT   ;
       SAVE     ;
       VALID :F lfvBrow();       
       WHEN lfwBrowUp();
       TITLE lcDet_Ttl ;
       WINDOW (lcWinCh2) IN WINDOW (lcWinCh0)

ACTIVATE WINDOW (lcDet_Ttl)

*-- end of lfwBrow.

*:**************************************************************************
*:* Name        : lfvBrow
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/24/2002
*:* Purpose     : 
*:***************************************************************************
*:* Called from : lfwBrow
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvBrow()
*:***************************************************************************
FUNCTION lfvBrow

*-- end of lfvBrow.

*:**************************************************************************
*:* Name        : lfwBrowUp
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/27/2002
*:* Purpose     : 
*:***************************************************************************
*:* Called from : lfwBrow
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfwBrowUp()
*:***************************************************************************
FUNCTION lfwBrowUp
lnDMarker = RECNO(lcVldSty)
lnAccept = IIF(IMPORT='Y',1,2)
SHOW GET lnAccept ENABLE

*lnDivision = ASUBSCRIPT( laDivision , ASCAN(laDivision,CDIVISION) , 1 )
*SHOW GET lnDivision ENABLE

SHOW WINDOW (lcDet_Ttl) REFRESH

*:**************************************************************************
*:* Name        : lfChkCode
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 07/17/2002
*:* Purpose     : Check codes fn
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfChkCode()
*:***************************************************************************
FUNCTION lfChkCode
IF !USED('CODES')
  =gfOpenFile(gcDataDir+'CODES','IDRLTFNAME','SH')  && Key : CDEFCODE+CRLTFIELD+CFLD_NAME
ENDIF  

SELECT CODES
SET ORDER TO TAG IDRLTFNAME
IF !SEEK('NNSEASON    ','CODES')
  RETURN 'Season'
ENDIF

IF !SEEK('NNCSTYGROUP ','CODES')
  RETURN 'Style Group'
ENDIF

IF !SEEK('NNCDIVISION ','CODES')
  RETURN 'Division'
ENDIF

*--Chanege the order for later use in this prg
SELECT CODES
SET ORDER TO TAG CCODE_NO
LOCATE

RETURN ''
*-- end of lfChkCode.

*:**************************************************************************
*:* Name        : lfvCancel
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/28/2002
*:* Purpose     : Cancel button pressed
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvCancel()
*:***************************************************************************
FUNCTION lfvCancel

IF gfModalGen('INM00000B00006',.F.,.F.,.F.,'Are you sure you want to cancel?') = 1
  llCancel = .T.
  CLEAR READ
ENDIF

*-- end of lfvCancel.

*:**************************************************************************
*:* Name        : lfvOk
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/02/2002
*:* Purpose     : "Import Button"
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvOk()
*:***************************************************************************
FUNCTION lfvOk
CLEAR READ
*-- end of lfvOk.

*:**************************************************************************
*:* Name        : lpTap
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/28/2002
*:* Purpose     : Tab fn.
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lpTap()
*:***************************************************************************
FUNCTION lpTap

IF WONTOP() = lcDet_Ttl
  ACTIVATE WINDOW (lcWinCh3)
  _CUROBJ=OBJNUM(lnAccept)  
ELSE
  _CUROBJ=_CUROBJ+1
ENDIF

*-- end of lpTap.

*:**************************************************************************
*:* Name        : lpBackTap
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/28/2002
*:* Purpose     : Backtap fn.
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lpBackTap()
*:***************************************************************************
FUNCTION lpBackTap
IF WONTOP() = lcDet_Ttl
  ACTIVATE WINDOW (lcWinCh3)
  _CUROBJ=OBJNUM(pbClose)
ELSE
  _CUROBJ=_CUROBJ-1
  IF _CUROBJ=OBJNUM(lnAccept)
    ACTIVATE WINDOW (lcDet_Ttl)    
  ENDIF
ENDIF
*-- end of lpBackTap.

*:**************************************************************************
*:* Name        : lfAddUsrDt
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/01/2002
*:* Purpose     : 
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
IF !EMPTY(lcAlias)
  SELECT (lcAlias)
ENDIF  
REPLACE cAdd_User WITH gcUser_ID,;
        dAdd_Date WITH gdSysDate,;
        cAdd_Time WITH Time()
*-- end of lfAddUsrDt.

*:**************************************************************************
*:* Name        : lfvAccept
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/04/2002
*:* Purpose     : Valid function for Accept popup
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvAccept()
*:***************************************************************************
FUNCTION lfvAccept
PRIVATE lcAlias
lcAlias = ALIAS()

SELECT (lcVldSty)
REPLACE Import WITH IIF(lnAccept=1,'Yes','No')
IF !EMPTY(lcAlias)
  SELECT (lcAlias)
ENDIF
=lfwBrowUp()

*-- End of lfvAccept

*!*************************************************************
*! Name      : lfvDivision                      *C102763,4 
*! Developer : WAM
*! Date      : 07/01/1996
*! Purpose   : Valid order division code
*!*************************************************************
*! Calls     : CODECHK,lfvCommision
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvDivision()
*!*************************************************************
FUNCTION lfvDivision
PRIVATE lcAlias
*lcAlias = ALIAS()

*IF &lcVldSty..CDIVISION  <> laDivision[lnDivision,2]
*  SELECT (lcVldSty)  
*  REPLACE CDIVISION WITH laDivision[lnDivision,2]
*ENDIF

*IF !EMPTY(lcAlias)
*  SELECT (lcAlias)
*ENDIF
*=lfwBrowUp()
