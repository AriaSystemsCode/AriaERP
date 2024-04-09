*:************************************************************
*: Program file  : icSKU
*: Program desc. : Create SKU for each size for each scale of specific style
*:                 on specific account,or create prepak for the for the
*:                 scale of style on specific account
*: For screen    : icSKU
*:        System : Aria Advantage Series.
*:        Module : Inventory Control (IC).
*:     Developer : Ahmed Amer (AHM)
*:************************************************************
*: Calls : 
*:         Procedures : lpShow,lpSavscr
*:         Functions  : gfSetup,lfvData_1,lfvData_2,lfvData_3,lfvData_1_2
*:                    :   
*:                    : 
*:************************************************************
*: Passed Parameters  : None
*:************************************************************
*: Example : DO ICSKU
*:************************************************************
*: Modifications :
*: B802356 06/20/99 AHM concatinate "*ONLY" text to SKU filed
*: B802356              in spck_hdr file if it is prepack for
*: B802356              customer using skutmpl "JCP"
*: HDM 07/14/1999 We should update pack_ID field instead of sku 
*:                field IN SPCK_LIN,SPCK_HDR
*: B802780,1 12/07/99 WAM Fix updtae sku header
*: B603433,1 02/06/2000 NAD More than one SKU for code the same account\style\size    
*: B603463,1 02/20/2000 NAD Fixing bug when editing SKU that alrday exist it dosn't save 
*: B803158,1 ADEL 03/30/2000 Fix the bug of duplicating all old lines in Spck_Lin when
*: B803158,1                 adding any new line in editing or adding mode.Also fix the bug 
*: B803158,1                 of not deletinbg sku lines.
*: B803176,1 05/03/2000  NAD Fix Bug sku dose not delete
*: B803176,1 05/03/2000  NAD Fix Bug  when entering a valid Style the browse screen appear
*: B803176,1 05/03/2000  NAD Sku sizes dosent appear if when account and style are empty
*: B803336,1 06/13/2000  AHM Sizes decription is not showen 
*: B603823,1 08/21/2000  AMH Update SKU templete File if it is Empty
*: B803703,1 10/02/2000  ABD Fix Bug "Too few arguments".
*: C102566,1 04/15/2002  MHM add trigger for gma to follow concept of Pack_Color_size
*: B605894,1 ASH 04/30/2002 The style browse always at BOF even you entered the first chr of the style.
*: B606160,1 HBG 08/22/2002 Validate if the same SKU# exist or not on the size level (Spck_lin file).
*: B606160,1                The SKU (main + size) should be unique for the same account.
*: B038431,1 NNA 09/08/2004 Fix bug that Sku ID screen has wrong title (Pack_ID)
*: B124629,1 MHM 12/15/2004 Fix bug of not correct retrive of size 
*! B124944,1 MHM 12/28/2004 Fix bug of not correct delete and save SKU 
*:************************************************************

*-- laData     Array that hold the data of the basefile
*-- laScrMode  Array that hold the screen modes
*-- laKeyField Array that hold the key elements of the screen
*-- laStySeg   Array that hold all data about style segments
*-- laSzCode   Array that hold the code of all sizes for the scale
*-- laDefproc  Array that hold the status of each button of the control panel.                 && This is to prevent the global save
DIMENSION laData[1], laScrMode[4], laKeyField[3,4], laStySeg[1],;
          laCodInfo[2,10], laSzCode[8]

*-- lcPrePack  Variable that hold the prepak code
*-- lcPrPkDesc Variable that hold the prepak desc
*-- llPPak     Variable that show if this scale use prepak or it is SKU
*-- llNoShow   Variable that prevent or enable the program go into    ;
               lpshow once the program is executed (.F. go in lpshow,.T. don't)
*-- llExtSize  Variable that show if the system use extended size scale or not
*-- llUpDn     Variable that show if the UP/DOWN buttons are clicked
*-- lcScale    Variable that hold the scale
*-- lcScaDes   Variable that hold the scale desc
*-- lcScFields Variable that hold the name of the fields we used from the base file
*-- lcLinFil   Variable that hold the name of the temp file that created for 'Spck_Lin' file
*-- llBrowse   Variable that show if the browse button of any control is clicked
*-- lcPPStat   Variable that hold the status of the prepak control
*-- lcUsePPS   Variable that hold the status of the use prepak 
*-- lcDivDesc  Variable that hold the desc. of the division
*-- lcSeaDesc  Variable that hold the desc. of the dseason
*-- lcStyPic   Variable that hold the picture of the style
*-- lcStyTtl   Variable that hold the title of the style
*-- lcStyDesc  Variable that hold the style desc.
*-- lnStyleWid Vraiable that hold wedth of the style
*-- LCSzPic    Variable that hold picture of the sizes
*-- lcSzSca    Variable that hold the title of the sizes group
*-- lcOldVal   Variable that hold the old value for each size befor modificate it
*-- lcSep      Variable that hold the seperator that located befor scale segment 
*-- lnStPos    Variable that hold the start position of scale segment in style

STORE SPACE(0) TO laStySeg,laSzCode,lcPrePack,lcPrPkDesc,lcScale,lcScFields,;
                  lcScaDes,lcLinFil,lcDivDesc,lcSeaDesc,lcStyPic,lcStyTtl,;
                  lcSzPic,lcSzSca,lcStyDesc,lcOldVal,lcSep
STORE 0 TO lnStyleWid,lnStPos
STORE .F. TO llPPak,llExtSize,llUpDn,llBrowse,llFrom_1_2

*-- lcPPStat   Variable that hold enabling status of prepack control
*-- lcUpStat   Variable that hold enabling status of Up Button 
*-- lcDnStat   Variable that hold enabling status of Down Button
*-- llFrom_1_2 Variable that showes if lfvData_3 is called from function 
*              lfvData_1_2 or It is from lfvdata_3
STORE "DISABLE" TO lcPPStat,lcUpStat,lcDnStat,lcUsePPS

laDefproc[07] = .F.                 && This is to Enable local delete
laDefproc[09] = .F.                 && This is to prevent the global save

laKeyField[1,1] = "laData[1]"
laKeyField[1,2] =.F.
laKeyField[1,3] = "Sku_Style"
laKeyField[1,4] = 1

laKeyField[2,1] = "laData[2]"
laKeyField[2,2] = .F.
laKeyField[2,3] = "Sku_Style"
laKeyField[2,4] = 2

laKeyField[3,1] = "laData[3]"
laKeyField[3,2] = .T.
laKeyField[3,3] = "Sku_Style"
laKeyField[3,4] = 3

IF !gfSetup()
  RETURN
ENDIF  

*B606160,1 HBG 08/22/2002 Add the Style to the browse fields of the screen [Begin]
lcBrFields = "Account   :R :H='Account' :16," +;
             "Style     :R :H='Style'   :19," +;
             "Pack_ID   :R :H='SKU#'    :16," +;
             "Desc      :R :H='Desc'    :30," +;
             "cDivision :R :H='Devision'  :8,"  +;
             "Season    :R :H='Season':8"
*B606160,1 [End]


*-- This is to check if the system use extended size or not
llExtSize  = gfGetMemVar('M_USEEXSSC') 

*-- llEditDiv  Variable that show if the system use editable division code
*-- llEditSea  Variable that show if the system use editable season   code
*-- lnDivFW    Variable that hold the wedith of division code
*-- lnSeaFW    Variable that hold the wedith of season   code
STORE .F. TO llEditDiv,llEditSea 
STORE 0 TO lnDivFW,lnSeaFW
llEditDiv = gfIsEdtble("CDIVISION", @lnDivFW)
llEditSea = gfIsEdtble("SEASON"   , @lnSeaFW)

IF !WEXIST(gcBaseWind)

  *B603823,1 08/21/2000  AMH Update SKU templete File if it is Empty [Start]
  lnAlias = SELECT(0)
  SELECT SKUTMPL
  LOCATE
  IF EOF() .OR. !SEEK('S'+'DEF')
    APPEND BLANK
    REPLACE TYPE     WITH 'S'                    ,;
            CSKUCODE WITH 'DEF'                  ,;
            DESC     WITH 'Default SKU         ' ,;
            FIELD1   WITH 'Sku #     '           ,;
            LEN1     WITH 8                      ,;
            LEN2     WITH 0                      ,;
            LEN3     WITH 0                      ,;
            FIELD4   WITH 'Sizes     '           ,;
            LEN4     WITH 8                      ,;
            PREPACK  WITH ''
  ENDIF
  SELECT (lnAlias)
  *B603823,1 08/21/2000  AMH Update SKU templete File if it is Empty [End  ]

  SELECT Scale
  SET ORDER TO Scale
  SCATTER FIELDS cnt,Sz1,Sz2,Sz3,Sz4,Sz5,Sz6,Sz7,Sz8 MEMVAR BLANK
  lcStyPic   = gfItemMask("PI")
  lcStyTtl   = gfItemMask("HI")
  IF llExtSize
    =gfItemMask(@laStySeg)
    *-- lnStPos  Variable that hold the start position of scale segment
    *-- lcSep    Variable that hold the seperator that preceed scale segment
    *-- lcStyPic Variable that hold style picture(format)
   
    lnStPos  = laStySeg(ALEN(laStySeg,1),4)
    
    *B803336,1 (Start)
    *lcSep    = laStySeg(ALEN(laStySeg,1)-1,6)    
    lcSep    = ALLTRIM(laStySeg(ALEN(laStySeg,1)-1,6))
    *B803336,1 (End)
    lcStyPic = SUBSTR(lcStyPic,1,lnStPos-1-LEN(lcSep))
  ELSE
    *-- lnStPos = 20 because in the whole program
    *-- ladata[2] is used as SUBSTR(laData[2],1,lnStPos-1-LEN(lcSep))
    *-- to get the style with out the scale because :
    *-- in case system uses extended size this will be essential because 
    *-- we add different scales to the style and pading it with 19 char
    *-- so we make lnStPos = 20 in case system dosen't use
    *-- extended size to get the style as it is from style field
    lnStPos = 20
    lcSep   = SPACE(0)
  ENDIF
  lnStyleWid = LEN(lcStyPic)  
  lcLinFil   = gfTempName()         && Hold the temp file of warehouse.
  lcScFields = "Account, Style, Pack_ID, Desc, cDivision, Season"
  SELECT Spck_Hdr
  SET FILTER TO Type = 'S'
  SET ORDER TO Sku_Style
  SCATTER FIELDS &lcScFields TO laData BLANK
  SELECT Spck_Lin
  = AFIELDS(laFileStru)  
  lnFileStru = ALEN(laFileStru,1)
  CREATE TABLE (gcWorkDir + lcLinFil) FROM ARRAY laFileStru
  INDEX ON Type+Account+Style+Pack_ID TAG StyPack ADDITIVE
ELSE
  PRIVATE lnCurAlias
  lnCurAlias = SELECT(0)

  SELECT Spck_Hdr
  SET FILTER TO Type = 'S'
  SET ORDER TO Sku_Style

  SELECT Spck_Lin
  SET FILTER TO Type = 'S'
  SET ORDER TO SpckLins

  SELECT Spck_Hdr
  SET ORDER TO Sku_style

  SELECT Style
  SET ORDER TO Style
  SET RELATION TO 'S'+Scale INTO Scale

  SELECT Scale
  SCATTER FIELDS cnt,Sz1,Sz2,Sz3,Sz4,Sz5,Sz6,Sz7,Sz8 MEMVAR  
  SELECT(lnCurAlias)
ENDIF

*: C102566,1 04/15/2002  MHM add trigger for gma to follow concept of Pack_Color_size[Start]
IF ASCAN(laEvntTrig,PADR('INITVAR',10))<>0
  =gfDoTriger('ICSKU','INITVAR')
ENDIF
*: C102566,1 04/15/2002  [End]

SELECT Spck_Hdr
SET FILTER TO Type = 'S'
SET ORDER TO Sku_Style

SELECT Spck_Lin
SET FILTER TO Type = 'S'
SET ORDER TO SpckLins

SELECT Spck_Hdr
SET ORDER TO Sku_style

SELECT Style
SET ORDER TO Style
SET RELATION TO 'S'+Scale INTO Scale

PUSH KEY                                     && To save the the current on key label
DO (gcScrDir + gcWinAppl + '\ICSKU.SPR')     && calling the screen ICSKU
POP KEY                                      && To Restore the previous assignments for on key label

SELECT Spck_Hdr
SET FILTER TO 

SELECT Spck_Lin
SET FILTER TO 

SELECT Style
SET RELATION TO 

*-- If we realy quitting the screen 
IF glQuitting                 
  IF USED(lcLinFil)
    USE IN (lcLinFil)
  ENDIF
  ERASE (gcWorkDir+lcLinFil +".DBF")
  ERASE (gcWorkDir+lcLinFil +".CDX")
ENDIF  

*!*************************************************************
*! Name      : lpShow
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Handling the screen mode
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfWHBrow,lfwBrow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : DO lpShow
*!*************************************************************

PROCEDURE lpShow

lnCurAlias = SELECT(0)

SELECT Scale
SCATTER FIELDS cnt,Sz1,Sz2,Sz3,Sz4,Sz5,Sz6,Sz7,Sz8 MEMVAR  

DO CASE
  *-- Select mode
  CASE laScrMode[1]
    STORE SPACE(0) TO lcStyle,lcStyDesc,lcPrePack,lcPrPkDesc,;
                      lcSzSca,laSzCode,lcDivDesc,lcSeaDesc
    STORE .F. TO llPPak,llFrom_1_2
    SELECT(lcLinFil)
    ZAP
    SELECT Spck_Hdr
    SCATTER FIELDS &lcScFields TO laData BLANK
    _CUROBJ = OBJNUM(laData[1])
    SELECT Scale
    SCATTER FIELDS cnt,Sz1,Sz2,Sz3,Sz4,Sz5,Sz6,Sz7,Sz8 MEMVAR BLANK
     
  *-- View mode
  CASE laScrMode[2]
  
    SELECT Spck_Hdr
    *B803336,1 (Start) Make sure that this is the correct record in the style file
    =SEEK(ALLTRIM(Spck_Hdr.Style),'Style')
    *B803336,1 (End)     
    SCATTER FIELDS &lcScFields TO laData
    laData[2] = SUBSTR(ladata[2],1,lnStPos-1-LEN(lcSep))
    lcScale = Scale.Scale  
    = lfStyDesc()
    _CUROBJ = OBJNUM(pbCls)

  *-- Edit mode
  CASE laScrMode[3]
    laData[2] = SUBSTR(ladata[2],1,lnStPos-1-LEN(lcSep))       
    *B803158,1 (Begin) When editing,select only the lines of the current sku no.
    *SELECT * FROM Spck_Lin INTO DBF gcWorkDir + (lcLinFil);
      WHERE Type+Account+Style+Pack_ID = 'S'+laData[1]+ladata[2] 
    SELECT * FROM Spck_Lin INTO DBF gcWorkDir + (lcLinFil);
      WHERE Type+Account+Style+Pack_ID = 'S'+laData[1]+ALLTRIM(ladata[2]);
      AND   Type+Account+ Pack_ID = 'S'+laData[1]+ALLTRIM(ladata[3])
    *B803158,1 end

    SELECT(lcLinFil)
    INDEX ON Type+Account+Style+Pack_ID TAG StyPack ADDITIVE

ENDCASE

= lfChkMulSca()
*: C102566,1 04/15/2002  MHM Cotrol show as GMA logic[Start]
IF ASCAN(laEvntTrig,PADR('CONTSHW',10))<>0
  =gfDoTriger('ICSKU','CONTSHW')
ENDIF
*: C102566,1 [End]

SELECT(lnCurAlias)

*!*************************************************************
*! Name      : lfvData_1
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Validat the screen key (Account).
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfvData_1_2
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvData_1()
*!*************************************************************

FUNCTION lfvData_1

PRIVATE lnCurAlias

lnCurAlias = SELECT(0)

laData[1] = PADR(ALLTRIM(laData[1]), FSIZE('Account', 'Spck_Hdr'))
XACCOUNT = laData[1]
IF llBrowse OR (!EMPTY(laData[1]) AND !SEEK('M'+XACCOUNT,'Customer'))
  llBrowse = .F.
  PRIVATE lcBrFields, lcBrowTitle  
  DO CUSBROWM WITH XACCOUNT
  laData[1] = XACCOUNT
  IF EMPTY(laData[1])
    _CUROBJ = _CUROBJ
  ENDIF  
ENDIF
*B603433,1 NAD (Start) 02/06/2000 Cancel calling lfvData_1_2 
*IF !EMPTY(laData[1])
*  = lfvData_1_2()
*ELSE
*  _CURIBJ = _CUROBJ  
*ENDIF
*B603433,1  NAD (End)
SELECT (lnCurAlias)

*!*************************************************************
*! Name      : lfvData_2
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Validat the screen key (Style).
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfvData_1_2
*!*************************************************************
*! Passed Parameters  : NONE 
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfvdata_2()
*!*************************************************************

FUNCTION lfvData_2

PRIVATE lnCurAlias

lnCurAlias = SELECT(0)

IF llBrowse .OR. !EMPTY(laData[2])

  *: C102566,1 04/15/2002  validate Style or pack [Start]
  IF ASCAN(laEvntTrig,PADR('VALDPACK',10))<>0
    =gfDoTriger('ICSKU','VALDPACK')
    RETURN
  ENDIF
  *: C102566,1 04/15/2002  [End]

  laData[2] = SUBSTR(ladata[2],1,lnStPos-1-LEN(lcSep))
  *B803158,1 If extended size scale, always browse to select a style/size
  *IF llBrowse OR !SEEK(laData[2],"Style")
  
  *B803176,1 (Start)
  *IF llBrowse OR llExtSize OR !SEEK(laData[2],"Style")
  IF llBrowse OR !SEEK(laData[2],"Style")
  *B803176,1 (End)

  *B803158,1 end
    llBrowse = .F.
    *B605894,1 ASH 04/30/2002 (Begin) The style browse always at BOF even you entered the first chr of the style.
    *laData[2]  = gfStyBrw('I',"","",.F.)
    laData[2]  = gfStyBrw('I',laData[2],"",.F.)
    *B605894,1 ASH 04/30/2002 (End)
    laData[2]  = PADR(laData[2],19)
  ENDIF
  IF !EMPTY(laData[2])
   
    *B803176,1 (Start)
    *= lfStyDesc()  
    *B803176,1 (End)
     
    laData[2] = SUBSTR(ladata[2],1,lnStPos-1-LEN(lcSep))

    SELECT Scale
    SCATTER FIELDS cnt,Sz1,Sz2,Sz3,Sz4,Sz5,Sz6,Sz7,Sz8 MEMVAR

    *B803176,1 (Start)
    lcScale = Scale.Scale
    = lfStyDesc()
    *B803176,1 (End)
    
    * B603433,1 NAD 02/06/2000 Cancel calling lfvData_1_2 
    * = lfvData_1_2()
    * B603433,1 NAD (End)
  ELSE
    _CUROBJ = _CUROBJ
  ENDIF
  
ENDIF

SELECT(lnCurAlias)

*!*************************************************************
*! Name      : lfvData_3
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Validat the screen key (SKU).
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvData_3()
*!*************************************************************

FUNCTION lfvData_3

lnLstKey = LASTKEY()
laData[3] = IIF(EMPTY(laData[3]),SPACE(16),laData[3])
*B603433,1 NAD (Start) 02/06/2000
*IF lnLstKey = 13 OR llFrom_1_2 OR llBrowse
IF lnLstKey = 13 OR  llBrowse
*B603433,1 NAD (End) 

  *: C102566,1 04/15/2002  validate SKU field for Gma[Start]
  IF ASCAN(laEvntTrig,PADR('VALDSKU',10))<>0
    =gfDoTriger('ICSKU','VALDSKU')
    RETURN
  ENDIF
  *: C102566,1 [End]

  IF laData[3] = "?"
    llBrowse = .T.
  ENDIF
  *B603433,1 NAD 02/06/2000 (Start) Add THE Sku to the expression
  *IF !EMPTY(laData[1]) AND !EMPTY(laData[2]) OR llBrowse
  IF !EMPTY(laData[1]) AND !EMPTY(laData[2]) AND !EMPTY(laData[3])  OR llBrowse
    *IF !SEEK('S'+laDaza[1]+PADR(laData[2],19),'Spck_Hdr') OR llBrowse
    IF !SEEK('S'+laData[1]+PADR(laData[2],19)+PADR(laData[3],16),'Spck_Hdr') OR llBrowse
  *B603433,1 NAD (End)     
      IF !llBrowse AND !EMPTY(laData[3])
        *-- THIS SKU IS NOT FOUND DO YOU WANT TO BROWSE OR ADD
        *-- < Add > < Browse > < Reenter >
        lnABR = gfModalGen("QRM42090B42003","Dialog",'SKU'+'|'+laData[1]+' and style '+laData[2])
        DO CASE
          CASE lnABR = 1            &&Add
            IF lfPackScr() = 1    && OK
              laScrMode    = .F.
              laScrMode[4] = .T.
              _CUROBJ = OBJNUM(laData[4])
              SHOW GETS
            ELSE                  && Cancel
              _CUROBJ = OBJNUM(laData[3])
            ENDIF
          CASE lnABR = 2            &&Browse
            = lfSKUBrow()
          CASE lnABR = 3            &&Reenter
            STORE SPACE(0) TO laData[3]
            _CUROBJ = OBJNUM(laData[3])
        ENDCASE
      ELSE
        llBrowse = .F.
        = lfSKUBrow()
      ENDIF
    ELSE
      laScrMode    = .F.
      laScrmOde[2] = .T.
      SHOW GETS
    ENDIF
  ELSE
    IF !EMPTY(laData[3])
      =lfSKUBrow()
    ENDIF
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lfSKUBrow
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Browse all sku.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfSKUBrow()
*!*************************************************************

FUNCTION lfSKUBrow

PRIVATE lcBrFields ,lcBrTitle,lcKey,lnCurAlias
PRIVATE laBrow    &&This is to hold the data of the sku form sku browse
DIMENSION laBrow[6]
STORE SPACE(0) TO laBrow

lnCurAlias = SELECT(0)

lcBrTitle  = 'Sku #'
lcBrFields = "Account   :R :H='Account' :16," +;
             "Style     :R :H='Style'   :19," +;
             "Pack_ID   :R :H='SKU#'    :16," +;
             "Desc      :R :H='Desc'    :30," +;
             "cDivision :R :H='Devision'  :8,"  +;
             "Season    :R :H='Season':8"

llFound = .F.
*B603433,1 02/06/2000 NAD (Start)
SELECT Spck_Hdr
=SEEK('S')
COUNT REST WHILE Type+Account+Style+Pack_Id='S' FOR Pack_Id=laData[3] TO lnNoSku
*B603433,1 NAD (End)

*B803176 (Start)
PRIVATE llShowGet
llShowGet = .F.
*B803176 (End)

DO CASE
  *B603433,1 02/06/2000 NAD (Start)
  CASE !EMPTY(laData[3]) AND lnNoSku > 0
    IF lnNoSku = 1
      =SEEK('S')
      LOCATE REST WHILE Type+Account+Style+Pack_Id='S' FOR Pack_Id=laData[3]
      
      *B803176,1 (Start)
      
      *laScrMode    = .F.
      *laScrMode[2] = .T.
      *SHOW GETS
      *RETURN
      =SEEK (ALLTRIM(Style),'STYLE')
      llShowGet = .T.

      *B803176,1 (End)     

    ELSE  
      llFound = .T.
      lcKey = "FOR Type = 'S' AND Pack_ID=laData[3]"
    ENDIF
   *B603433,1 NAD (End)


  CASE EMPTY(laData[1]) AND EMPTY(laData[2])
    IF SEEK('S','Spck_Hdr')
      llFound = .T.
      lcKey = "FOR Type = 'S' "        
    ELSE 
      llFound = .F.
    ENDIF

  CASE EMPTY(laData[1]) AND !EMPTY(laData[2])
    SELECT Spck_Hdr
    LOCATE FOR Type = 'S' AND Style = laData[2]
    IF FOUND()
      llFound = .T.
      lcKey = "FOR Type+Style = 'S' + laData[2]"        
    ELSE 
      llFound = .F.
    ENDIF

  CASE !EMPTY(laData[1]) AND EMPTY(laData[2])
    IF SEEK('S'+laData[1],'Spck_Hdr')
      llFound = .T.
      lcKey = "FOR Type+Account = 'S' + laData[1]"
    ELSE 
      llFound = .F.
    ENDIF
    
  CASE !EMPTY(laData[1]) AND !EMPTY(laData[2]) 
    *B603433,1 02/06/2000 NAD (End)   
    *lcKey = "FOR Type+Account+Style = 'S' + laData[1]+PADR(laData[2],19)"
    IF SEEK('S'+laData[1]+PADR(laData[2],19),'Spck_Hdr')
      llFound=.T.
      *B803703,1 ABD Fix Bug "Too few arguments". [Begin]
      *lcKey = "'S' + laData[1]+PADR(laData[2],19)"
       lcKey = "'S' + laData[1]+laData[2]"
      *B803703,1 ABD [Ebd]
    ELSE
      llFound=.F.  
    ENDIF    
    *B603433,1 NAD (End)
         
ENDCASE

*B803176,1 (Start)
IF llShowGet
  laScrMode    = .F.
  laScrMode[2] = .T.
  SHOW GETS
ELSE
*B803176,1 (End)
  IF llFound
    SELECT Spck_Hdr
    = gfBrows(lcKey,"Account,Style,Pack_ID,Desc,cDivision,Season",;
              "laBrow",lcBrTitle)
    IF !EMPTY(laBrow)
      = ACOPY(laBrow,laData)
      laData[2] = SUBSTR(ladata[2],1,lnStPos-1-LEN(lcSep))
      *B803176,1 (Start)
      =SEEK (ALLTRIM(laData[2]),'STYLE')
      *B803176,1 (End)
      laScrMode    = .F.
      laScrMode[2] = .T.
      SHOW GETS
    ELSE
      _CUROBJ = OBJNUM(laData[3])
    ENDIF
  ELSE
    *--There are no records to browse.
    *-- OK
    = gfModalGen("INM42097B42001","Dialog")  
    _CUROBJ = _CUROBJ
  ENDIF
*B803176,1 (Start)
ENDIF
*B803176,1 (End)

SELECT(lnCurAlias)

*!*************************************************************
*! Name      : lfPackScr
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : To formalize the SKU templete.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfPackScr()
*!*************************************************************

FUNCTION lfPackScr

PRIVATE lnChoice,lnCurAlias

lnChoice = 0

IF SEEK('S'+Customer.Skutmpl,'SkuTmpl') OR SEEK('S'+'DEF','SkuTmpl')
  lcSzSca = SPACE(1)+ALLTRIM(SkuTmpl.Field4)+SPACE(1)
  lcSzPic = REPLICATE('X',SkuTmpl.Len4)

  lcSPTtl = IIF(!EMPTY(SkuTmpl.Field1),ALLTRIM(SkuTmpl.Field1)+;
            IIF(!EMPTY(SkuTmpl.Field2),"-"+ALLTRIM(SkuTmpl.Field2)+;
            IIF(!EMPTY(SkuTmpl.Field3),"-"+ALLTRIM(SkuTmpl.Field3),SPACE(0)),;
            IIF(!EMPTY(SkuTmpl.Field3),"-"+ALLTRIM(SkuTmpl.Field3),SPACE(0))),;
            IIF(!EMPTY(SkuTmpl.Field2),ALLTRIM(SkuTmpl.Field2)+;
            IIF(!EMPTY(SkuTmpl.Field3),"-"+ALLTRIM(SkuTmpl.Field3),SPACE(0)),;
                ALLTRIM(SkuTmpl.Field3) ) ) + ":"
  lCSPPic = SPACE(0)
  lCSPPic = IIF(!EMPTY(SkuTmpl.Len1),PADL(lCSPPic,SkuTmpl.Len1,'!')+;    
            IIF(!EMPTY(SkuTmpl.Len2),"-"+PADL(lCSPPic,SkuTmpl.Len2,'!')+;
            IIF(!EMPTY(SkuTmpl.Len3),"-"+PADL(lCSPPic,SkuTmpl.Len3,'!'),SPACE(0)),;
            IIF(!EMPTY(SkuTmpl.Len3),"-"+PADL(lCSPPic,SkuTmpl.Len3,'!'),SPACE(0))),;
            IIF(!EMPTY(SkuTmpl.Len2),PADL(lCSPPic,SkuTmpl.Len2,'!')+;
            IIF(!EMPTY(SkuTmpl.Field3),"-"+PADL(lCSPPic,SkuTmpl.Len3,'X'),SPACE(0)),;
                PADL(lCSPPic,SkuTmpl.Len3,'!') ) )
  lnCodeTtl  = LEN(lcSPPic)
  lcSPCode   = SPACE(lnCodeTtl)
  lcSPCode   = laData[3]
  lnPackWid  = LEN(ALLTRIM(lcSPTtl))
  lcWName    = "SPTmp"
  
  *B038431,1 NNA 09/08/2004  (Begin) Correct the Title of the Sku Screen
  *lcWSPTitl  = "Pack_ID"
  lcWSPTitl  = "Sku_ID"
  *B038431,1 NNA (End)
  
  PUSH KEY
  ON KEY
  DO (gcScrDir + gcWinAppl + '\ICSPTMP.SPR')
  POP KEY     
 
  IF lnChoice = 1
    lnSepCount = OCCURS('-',lcSPPic)
    laData[3] = SUBSTR(lcSPCode,1,lnCodeTtl-lnSepCount)
  ELSE
    STORE SPACE(0) TO laData[3]
    _CUROBJ = _CUROBJ
  ENDIF

ELSE
  *--There is no SKU templete.
  *-- OK
  = gfModalGen("INM42101B42001","Dialog",'SKU')  
  _CUROBJ = OBJNUM(laData[1])
ENDIF

RETURN lnChoice

*!*************************************************************
*! Name      : lpSavscr
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : To make local save.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : DO lpSavscr
*!*************************************************************

PROCEDURE lpSavscr
PRIVATE lnCurAlias,lcCurNdx,llReturn,lnChoice

*: C102566,1 04/15/2002  custom save for GMA [Start]
IF ASCAN(laEvntTrig,PADR('SAVSRGMA',10))<>0
  =gfDoTriger('ICSKU','SAVSRGMA')
  RETURN
ENDIF
*: C102566,1 [End]

lnCurAlias = SELECT(0)
llReturn = .T.
SELECT (lcLinFil)
GO TOP
IF EOF() OR BOF() OR DELETED()
  *-- You have to enter at least one SKU or create prepak
  *-- <Ok>
  = gfModalGen("INM42099B42001","Dialog")
  llReturn = .F.
ELSE
  SCAN
    IF !EMPTY(&lcLinFil..SKU) AND EMPTY(&lcLinFil..Prepak) 
      *-- You have to select prepak
      *-- <OK>
      = gfModalGen("QRM42100B42001","Dialog")
      llReturn = .F.
      EXIT
    ENDIF
  ENDSCAN
  IF llReturn
    SELECT Spck_Lin
    *B603433,1 NAD (Start) Delte all the records of this sku to add new one
    * from the temp file  
    *IF SEEK('S'+laData[1]+laData[2],'Spck_Lin')   
    *  SCAN  WHILE Type+Account+Style+Pack_ID = "S"+laData[1]+laData[2]
    *    BLANK
    *    DELETE
    *  ENDSCAN
    *ENDIF

    *B803176,1 (Start)
    *Comment out this part of code and rewrite it.
    
    **B803158,1 (Begin) Remark the following lines and let the style have the scale if case
    **B803158,1         of extended size scale and be as it is otherwise.
    **    IF SEEK('S'+laData[1]+laData[2]+ALLTRIM(LaData[3]),'Spck_Lin')   
    **      DELETE REST WHILE Type+Account+Style+Pack_ID = "S"+laData[1]+laData[2]+ALLTRIM(LaData[3])
    **    ENDIF
    **-- In Add mode, the temp file has only the new sku lines, so there is no need for
    **-- deleting SPCK_Lines
    **-- In Edit mode, we must add the Pack_ID (i.e LaData[3]) to the condition
    **-- so as not to delete any other skus for the same style. 
    **IF SEEK('S'+laData[1]+PADR((laData[2]+lcSep+lcScale),19)+IIF(laScrMode[4],ALLTRIM(LaData[3]),''),'Spck_Lin')
    **  DELETE REST WHILE Type+Account+Style+Pack_ID = "S"+laData[1]+PADR((laData[2]+lcSep+lcScale),19)+IIF(laScrMode[4],ALLTRIM(LaData[3]),'')
    *IF !laScrMode[4] .AND. SEEK('S'+laData[1]+ALLTRIM(laData[2]),'Spck_Lin')
    *  DELETE REST WHILE Type+Account+Style+Pack_ID = "S"+laData[1]+ALLTRIM(laData[2]);
    *  		FOR Pack_ID = ALLTRIM(laData[3])  	
    *ENDIF
    **B803158,1 (End)
    
    IF SEEK('S'+laData[1]+laData[2],'Spck_Lin')
      SELECT Spck_Lin
      SCAN REST FOR Type+Account+Style+Pack_ID = "S"+laData[1]+laData[2]
        IF ALLTRIM(Pack_ID) = ALLTRIM(laData[3])
          BLANK
          DELETE
        ENDIF
      ENDSCAN
    ENDIF
    *B803176,1 (End)
    

    *B603433,1 NAD (End)
    SELECT(lcLinFil)
    SCAN
      SCATTER MEMVAR
      SELECT Spck_Lin
      APPEND BLANK
      GATHER MEMVAR 
      REPLACE TotQty    WITH IIF(EMPTY(&lcLinFil..PrePak),1,0) ,;
              dAdd_Date WITH gdSysDate  ,;
              cAdd_Time WITH gfGettime(),;
              cAdd_User WITH gcUser_Id
     lcHdrKey = "S"+ladata[1]+PADR(ladata[2],19)+m.Pack_Id
      SELECT Spck_Hdr
      IF SEEK(lcHdrKey,'Spck_Hdr')
        REPLACE Spck_Hdr.Desc      WITH laData[4],;
                Spck_Hdr.cDivision WITH laData[5],;
                Spck_Hdr.Season    WITH laData[6]       
      ELSE
        *-- HDM [START] We should update pack_ID field instead of sku field
        *INSERT INTO Spck_Hdr (Type,Account,Style,Pack_ID,Desc,cDivision,Season,;
                              dAdd_Date,cAdd_Time,cAdd_User);
                    VALUES   ('S' ,laData[1],PADR(laData[2],19),ladata[3],;
                              ladata[4],laData[5],laData[6],gdSysDate,gfGettime(),gcUser_Id)
        *B802780,1 Commented out
        *INSERT INTO Spck_Hdr (Type,Account,Style,Pack_ID,Sku,Desc,cDivision,Season,;
                              dAdd_Date,cAdd_Time,cAdd_User) VALUES ;
        ('S' ,laData[1], PADR(laData[2],19),m.Pack_ID,;
          m.Sku,ladata[4],laData[5],laData[6],gdSysDate,gfGettime(),gcUser_Id)
        *B802780,1 (End)
         *-- HDM [End]
      ENDIF
    ENDSCAN
  ENDIF
ENDIF
IF llReturn
  lcHdrKey = "S"+ladata[1]+PADR(ladata[2],19)+ALLTRIM(laData[3])
  SELECT Spck_Hdr
  IF SEEK(lcHdrKey,'Spck_Hdr')
    REPLACE Spck_Hdr.Desc      WITH laData[4],;
            Spck_Hdr.cDivision WITH laData[5],;
            Spck_Hdr.Season    WITH laData[6]       
  ELSE
    *-- HDM [START] We should update pack_ID field instead of sku field
    *INSERT INTO Spck_Hdr (Type,Account,Style,Pack_ID,Desc,cDivision,Season,;
                          dAdd_Date,cAdd_Time,cAdd_User);
                VALUES   ('S' ,laData[1],PADR(laData[2],19),ladata[3],;
                          ladata[4],laData[5],laData[6],gdSysDate,gfGettime(),;
                          gcUser_Id)
    INSERT INTO Spck_Hdr (Type,Account,Style,Pack_ID,Sku,Desc,cDivision,Season,;
                          dAdd_Date,cAdd_Time,cAdd_User) VALUES ;
    ('S' ,laData[1], PADR(laData[2],19),ladata[3]+IIF(llPPak and Customer.Skutmpl='JCP',"*ONLY",""),;
                          laData[3],ladata[4],laData[5],laData[6],gdSysDate,gfGettime(),gcUser_Id)
    *-- HDM [End]
  ENDIF
ENDIF  
llCSave = llReturn

*!*************************************************************
*! Name      : lpDelScr
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : To make local delete.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : DO lpDelScr
*!*************************************************************

PROCEDURE lpDelScr

PRIVATE lnCurAlias,lcTag,lnCurRec,llOpen,lnI,llContinue

*! B124944,1 MHM 12/28/2004 Fix bug of not correct delete and save SKU [Start]
IF ASCAN(laEvntTrig,PADR('GMADLSKU',10))<>0
  =gfDoTriger('ICSKU','GMADLSKU')
  RETURN
ENDIF
*! B124944,1 MHM [End]

lnCurAlias = SELECT(0)
*B603433,1 NAD  (Start) Add laData[3] to the expresion  
*IF SEEK('S'+laData[1]+PADR(laData[2],19),'Spck_Hdr')
 IF SEEK('S'+laData[1]+PADR(laData[2],19)+ALLTRIM(laData[3]),'Spck_Hdr') 
  SELECT Spck_Hdr
  BLANK
  DELETE
ENDIF
 
*B803176,1 (Star)
*Comment out this part of code and rewrite it

**IF SEEK('S'+laData[1]+laData[2],'Spck_Lin')
**  SELECT Spck_Lin
**  SCAN REST WHILE Type+Account+Style+Pack_ID = "S"+laData[1]+laData[2]
**    BLANK
**    DELETE
**  ENDSCAN
**ENDIF

**B803158,1 (Begin) Remark the following lines and let the style have the scale if case
**B803158,1         of extended size scale and be as it is otherwise.
**IF SEEK('S'+laData[1]+laData[2]+ALLTRIM(LaData[3]),'Spck_Lin')
**  SELECT Spck_Lin   
**  DELETE REST WHILE Type+Account+Style+Pack_ID = "S"+laData[1]+laData[2]+ALLTRIM(LaData[3])       
**ENDIF
*IF SEEK('S'+laData[1]+ALLTRIM(laData[2]),'Spck_Lin')
*  SELECT Spck_Lin   
**  DELETE REST WHILE Type+Account+Style+Pack_ID = "S"+laData[1]+PADR((laData[2]+lcSep+lcScale),19)+ALLTRIM(LaData[3])
*   DELETE REST WHILE Type+Account+Style = "S"+laData[1]+ALLTRIM(laData[2]);
*      		FOR Pack_ID = ALLTRIM(laData[3])  	
*
*  *B803176,1 (End)
*ENDIF
**B803158,1 (End)
**B603433,1 NAD (End)

IF SEEK('S'+laData[1]+laData[2],'Spck_Lin')
  SELECT Spck_Lin
  SCAN REST FOR Type+Account+Style+Pack_ID = "S"+laData[1]+laData[2]
    IF ALLTRIM(Pack_ID) = ALLTRIM(laData[3])
      BLANK
      DELETE
    ENDIF
  ENDSCAN
ENDIF
*B803176,1 (End)

STORE .F. TO laScrMode
laScrMode[1]  = .T.

SELECT (lnCurAlias)

*!*************************************************************
*! Name      : lfvSzCode
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : To put the code of the SKU for size in the temp file
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfvSzCode()
*!*************************************************************

FUNCTION lfvSzCode

PRIVATE lnCurAlias

lnCurAlias = SELECT(0)

lcField = VARREAD()
*B603463,1 NAD Chek if the sku exist in other size
*&lcField = SUBSTR(ALLTRIM(&lcField),1,LEN(lcSzPic))
&lcField = SUBSTR(LTRIM(&lcField),1,LEN(lcSzPic))
lnCount=0
FOR lnLoop=1 TO 8
  IF LTRIM(laSzCode(lnloop))=LTRIM(EVAL(VARREAD()))
    lnCount=lnCount+1
  ENDIF   
ENDFOR
*B603463,1 NAD (End)

lcScale = IIF(llExtSize,Scale.Scale,SPACE(0))

*B603463,1 NAD Chek if the sku exist in other size
*&lcField = ALLTRIM(&lcField)
*lnSize = ASCAN(laSzCode,ALLTRIM(EVAL(VARREAD())))
*IF !EMPTY(EVAL(VARREAD())) AND lnSize <> 0 AND;
   lnSize <> VAL(SUBSTR(VARREAD(),AT('(',VARREAD())+1,1))   
  *&lcField = ''
IF !EMPTY(EVAL(VARREAD())) AND lnCount> 1
  &lcField= lcOldVal
*B603463,1 NAD (End) 
  *--This SKU Code is existed for another size.
  *-- OK
  = gfModalGen("INM42131B42001","Dialog")  
  _CUROBJ = _CUROBJ
ELSE
  
  *B606160,1 HBG 08/22/2002 Validate if the same SKU# exist or not on the size level(Spck_lin file).
  *B606160,1                The SKU (main + size) should be unique for the same account. [Begin]
  lcSiz = ALLTRIM(EVAL(lcField))
  IF !EMPTY(lcSiz)
    lcKey = "S"+ladata[1]+ALLTRIM(ladata[3])+lcSiz
    lcPrvOrd = ORDER('SPCK_LIN')
    SET ORDER TO SPCK_LIN IN SPCK_LIN 
    IF SEEK(lcKey,'SPCK_LIN')
      = gfModalGen("INM42131B00000","Dialog")  
      SET ORDER TO (lcPrvOrd) IN SPCK_LIN
      &lcField = ""
      _CUROBJ = _CUROBJ
      RETURN
    ENDIF
    SET ORDER TO (lcPrvOrd) IN SPCK_LIN
  ENDIF  
  *B606160,1 [End]
  
  lcKey = "S"+ladata[1]+PADR((laData[2]+lcSep+lcScale),19)+ALLTRIM(ladata[3])+lcOldVal
  IF SEEK(lcKey,lcLinFil)
    SELECT (lcLinFil)
    IF EMPTY(&lcField)
      DELETE
    ELSE
      REPLACE Pack_ID WITH ALLTRIM(ladata[3])+&lcField
    ENDIF
  ELSE
    IF !EMPTY(&lcField)
      lcQtyNo = SUBSTR(lcField,10,1)
      INSERT INTO (lcLinFil) (Type,Account,Style,Pack_ID,Qty&lcQtyNo) ;
                      VALUES ('S' ,laData[1],PADR((laData[2]+lcSep+lcScale),19),;
                              ALLTRIM(ladata[3])+&lcField,1)
    ENDIF
  ENDIF
ENDIF
SELECT (lnCurAlias)

*!*************************************************************
*! Name      : lfChkMulSca
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : To check the count of the scales for style and to move to the
*!             prev. or next scale and handle the status of UP/DOWN buttons
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfChkMulSca()
*!*************************************************************

FUNCTION lfChkMulSca

PRIVATE lnCurAlias

lnCurAlias = SELECT(0)

lcUpStat = "DISABLE"
lcDnStat = "DISABLE"

DO CASE
  CASE llUpDn AND VARREAD() $ UPPER('pbUp')
    SKIP -1 IN "Style"  
  CASE llUpDn AND VARREAD() $ UPPER('pbDn')
    IF !EOF('Style')
      SKIP 1 IN "Style"
    ENDIF
ENDCASE

IF !EOF('Style')
  SKIP 1 IN "Style"
  IF !EOF('Style')
    IF SUBSTR(Style.Style,1,LEN(laData[2])) = laData[2]
      lcDnStat = IIF(!laScrMode[1],"ENABLE","DISABLE")
    ELSE
      lcDnStat = "DISABLE"
    ENDIF
    SKIP -1 IN "Style"
  ELSE
    SKIP -1 IN "Style"
  ENDIF
ENDIF

SKIP -1 IN "Style"

IF !BOF('Style')
  IF SUBSTR(Style.Style,1,LEN(laData[2])) = laData[2]
    lcUpStat = IIF(!laScrMode[1],"ENABLE","DISABLE")
  ELSE
    lcUpStat = "DISABLE"
  ENDIF
  SKIP 1 IN "Style"
ENDIF  

= lfGetSzCode()

SHOW GET pbUp &lcUpStat
SHOW GET pbDn &lcDnStat

=lfSzRefr()

SELECT (lnCurAlias)

*!*************************************************************
*! Name      : lfGetSzCode
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : To get the scale information form scale file for the current
*!             scale
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfGetSzCode()
*!*************************************************************

FUNCTION lfGetSzCode

PRIVATE lnScCuRec,lnJ,lnCurAlias

STORE SPACE(0) TO lcPrePack,lcPrPkDesc 

lnCurAlias = SELECT(0)

lcFileToUSe = IIF(laScrMode[2],'Spck_Lin',lcLinFil)

= SEEK('M'+laData[1],'Customer')
IF SEEK('S'+Customer.Skutmpl,'SkuTmpl') OR SEEK('S'+'DEF','SkuTmpl')
  lcSzSca = SPACE(1)+ALLTRIM(SkuTmpl.Field4)+SPACE(1)
  lcSzPic = REPLICATE('X',SkuTmpl.Len4)
ENDIF

IF !EMPTY(laData[2])
  SELECT Scale
  SCATTER FIELDS cnt,Sz1,Sz2,Sz3,Sz4,Sz5,Sz6,Sz7,Sz8 MEMVAR
  lcScale = IIF(llExtSize,Scale.Scale,SPACE(0))
ENDIF

STORE SPACE(0) TO laSzCode
*-- The next part to fill the sizes array and I used this way to detremine
*-- which size the record in lines file repesents and put the size code 
*-- with it's SKU Code
SELECT (lcFileToUse)

*! B124629,1 MHM 12/15/2004 Fix bug of not correct retrive of size [Start]
IF ASCAN(laEvntTrig,PADR('GMAGTSCD',10))<>0
  =gfDoTriger('ICSKU','GMAGTSCD')
  RETURN
ENDIF
*! B124629,1 MHM [End]

SCAN FOR Type+Account+Style+Pack_ID = ;
         'S'+laData[1]+PADR(ladata[2]+lcSep+lcScale,19)+ALLTRIM(laData[3])
  lnJ = 1
  llPPak = !EMPTY(&lcFileToUse..Sku)
  IF !llPPak
    *-- Looping 8 times because there are 8 fields represent 8 quantities 
    *-- in file 'Spck_lin' form Qty1->Qty8
    FOR lnJ = 1 TO 8
      IF EVAL(lcFileToUse+'.Qty'+ALLTRIM(STR(lnJ))) = 1
        laSzCode[lnJ] = SUBSTR(Pack_ID,LEN(ALLTRIM(laData[3]))+1)
        EXIT
      ENDIF
    ENDFOR
  ELSE
    lcPrePack  = &lcFileToUse..Prepak
    lnScCuRec  = RECNO('Scale')
    = SEEK('P'+Style.Scale+lcPrePack,'Scale')
    SELECT Scale
    SCATTER FIELDS Pp1,Pp2,Pp3,Pp4,Pp5,Pp6,Pp7,Pp8 TO laSzCode
    FOR lnI = 1 TO 8
      laSzCode[lnI] = IIF(lnI<=m.Cnt,LTRIM(STR(laSzCode[lnI])),'')
    ENDFOR

    SELECT (lcFileToUse)
    lcPrPkDesc = Scale.Cscl_Desc
    IF RECCOUNT('Scale') >= lnScCuRec
      GOTO lnScCuRec IN ('Scale')
    ENDIF
    EXIT
  ENDIF
ENDSCAN

*-- This is because we needn't to call lfRefresh in view mode becuse 
*-- the say fields have been refreshed by control panel when we have
*-- been transfered to view mode
*-- but we need to refresh say fields when we are in view mode when 
*-- UP OR DOWN button is clicked

*B803176,1 (Start)
  = lfStyDesc()
*B803176,1 (End)

IF !laScrMode[2] OR llUpDn
  = lfRefresh()
ENDIF

SELECT(lnCurAlias)

*!*************************************************************
*! Name      : lfvPPak
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : To detremine if the scale create prepak or SKU
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfvPPak()
*!*************************************************************

FUNCTION lfvPPak
PRIVATE lcKey,lnCurRec,lnCurAlias

lnCurAlias = SELECT(0)

lcScale = IIF(llExtSize,Scale.Scale,SPACE(0))

lcKey = "S"+ladata[1]+laData[2]
IF SEEK(lcKey,lcLinFil)
  SELECT (lcLinFil)
  DELETE REST FOR Type+Account+Style+Pack_ID = "S"+ladata[1]+laData[2]
ENDIF                    

IF llPPak
  STORE SPACE(0) TO laSzCode
  SELECT Style
  lnCurRec = RECNO('Style')
  GO TOP 
  SCAN FOR Style = laData[2]
    lcScale = IIF(llExtSize,Scale.Scale,SPACE(0))
    *B802356 (Start)
    *INSERT INTO (lcLinFil) (Type,Account,Style,Pack_ID,Sku) ;
                    VALUES ('S' ,laData[1],PADR((laData[2]+lcSep+lcScale),19),;
                            ALLTRIM(ladata[3]),ALLTRIM(ladata[3]))

    *-- HDM [START] We should update pack_ID field instead of sku field
    *INSERT INTO (lcLinFil) (Type,Account,Style,Pack_ID,Sku) ;
                    VALUES ('S' ,laData[1],PADR((laData[2]+lcSep+lcScale),19),;
                            ALLTRIM(ladata[3]),;
                            ALLTRIM(ladata[3])+IIF(Customer.Skutmpl='JCP',"*ONLY",""))
                            
    INSERT INTO (lcLinFil) (Type,Account,Style,Pack_ID,Sku) ;
                    VALUES ('S' ,laData[1],PADR((laData[2]+lcSep+lcScale),19),;
                            ALLTRIM(ladata[3]+IIF(Customer.Skutmpl='JCP',"*ONLY","")),;
                            ALLTRIM(ladata[3]))
    *-- HDM [End]
    *B802356 (End)
  ENDSCAN                            
  IF RECCOUNT('Style') >= lnCurRec
    GOTO lnCurRec IN ('Style')
  ENDIF
ELSE
  STORE SPACE(0) TO lcPrePack,lcPrPkDesc 
ENDIF

SELECT (lnCurAlias)

=lfSzRefr()

*!*************************************************************
*! Name      : lfvPrePack
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : To validate the prepak code
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfvPrePack()
*!*************************************************************

FUNCTION lfvPrePack

PRIVATE lcOldRec,lcKey,lnCurAlias
PRIVATE lcBrFields, lcBrowTitle

lnCurAlias = SELECT(0)

SELECT Scale
lcScale = Scale.Scale
lcOldRec=Recno()

IF llBrowse OR !SEEK('P'+Style.Scale+lcPrePack,'Scale')
  =gfprepbrow(lcScale,@lcPrePack)
  IF !EMPTY(lcPrePack)
    SCATTER FIELDS Pp1,Pp2,Pp3,Pp4,Pp5,Pp6,Pp7,Pp8 TO laSzCode
    FOR lnI = 1 TO 8
      laSzCode[lnI] = IIF(lnI<=m.Cnt,LTRIM(STR(laSzCode[lnI])),'')
    ENDFOR
  ELSE
    lcPrePack = lcOldVal
  ENDIF
ENDIF
IF RECCOUNT() >= lcOldRec
  GOTO lcOldRec
ENDIF
lcKey = "S"+ladata[1]+PADR((laData[2]+lcSep+lcScale),19)+ALLTRIM(ladata[3])
IF !EMPTY(lcPrePack) AND SEEK(lcKey,lcLinFil)
  SELECT (lcLinFil)
  REPLACE PrePak WITH lcPrePack REST FOR Type+Account+Style+Pack_ID = ;
                         "S"+laData[1]+laData[2]
  = SEEK('P'+Style.Scale+lcPrePack,'Scale')
  lcPrPkDesc = Scale.Cscl_Desc
  IF RECCOUNT('Scale') >= lcOldRec
    GOTO lcOldRec IN ('Scale')
  ENDIF    
ENDIF  

SELECT(lnCurAlias)
=lfRefresh()

=lfSzRefr()

*!*************************************************************
*! Name      : lfSzRefr
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : To refresh the prepak control and sizes controls
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfSzRefr()
*!*************************************************************

FUNCTION lfSzRefr

lcPPStat = IIF(llPPak AND laScrMode[4],"ENABLE","DISABLE")
lcUsePPS = IIF(m.cnt >=1 AND laScrMode[4],"ENABLE","DISABLE")

SHOW GET llPPak    &lcUsePPS 
SHOW GET lcPrePack &lcPPStat
SHOW GET pbPPkBr   &lcPPStat

SHOW GET laSzCode[1] 
SHOW GET laSzCode[2] 
SHOW GET laSzCode[3] 
SHOW GET laSzCode[4] 
SHOW GET laSzCode[5] 
SHOW GET laSzCode[6] 
SHOW GET laSzCode[7] 
SHOW GET laSzCode[8] 
=LFREFRESH()
*!*************************************************************
*! Name      : lfOldValue
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : To save the old value of the size beror change it
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfOldValue()
*!*************************************************************

FUNCTION lfOldValue

PRIVATE lcFldNme
lcFldNme = VARREAD()
lcOldVal = PADR(&lcFldNme,LEN(lcSzPic))

*!*************************************************************
*! Name      : lfvData_1_2
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : To restore the data if Account and Style are valid and there 
*!             is a previous SKU for them because in this case there is no
*!             need to get into SKU control because there is only one sku
*!             for style with account
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Exampl    : =lfvData_1_2()
*!*************************************************************

FUNCTION lfvData_1_2
IF !EMPTY(laData[1]) AND !EMPTY(laData[2])
  IF SEEK('S'+laData[1]+PADR(laData[2],19),'Spck_Hdr')
    laData[3] = Spck_Hdr.Pack_ID
    llFrom_1_2 = .T.
    =lfvData_3()
  ELSE
    = lfChkMulSca()
  ENDIF  
ENDIF

*!*************************************************************
*! Name      : lfStyDesc
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : To get the desc. ,division andseason for the style
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Exampl    : =lfStyDesc()
*!*************************************************************

FUNCTION lfStyDesc

IF !EMPTY(ladata[2])
  IF SEEK(PADR((laData[2]+lcSep+lcScale),19),'Style')
    lcStyDesc = Style.Desc
    laData[5] = Style.cDivision
    laData[6] = Style.Season
    lcDivDesc = gfCodDes(Style.cDivision, "CDIVISION")
    lcSeaDesc = gfCodDes(Style.Season  , "SEASON"   )
  ENDIF
ENDIF

*-- There is no need to make refresh if it isn't select mode
*-- because in other modes lfChkMulSca is called which by it's part calls 
*-- lfGetSzCode which calls lfRefresh
IF laScrMode[1]
  = lfRefresh()
ENDIF  

*!*************************************************************
*! Name      : lfPackScrAct
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Trap the BackTab Key
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfPackScrEsc()
*!*************************************************************

FUNCTION lfPackScrAct

ON KEY LABEL ESCAPE DO lfPackScrEsc

*!*************************************************************
*! Name      : lfPackScrAct
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Trap the BackTab Key
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfPackScrEsc()
*!*************************************************************

FUNCTION lfPackScrEsc

ON KEY LABEL ESCAPE

_CUROBJ = OBJNUM(pbCancel)
KEYBOARD "{ENTER}" CLEAR PLAIN
