*:****************************************************************
*: Program file  : HLCHDLN.PRG
*: Program desc. : Add,Change,Delete and Inquire Styles line.
*: System        : Aria Apparel System (A27).
*: Module        : H.L.C. System. (HL)
*: Developer     : ABDOU ELGENDI - (ABD) Due to C#200246,1
*: Date          : 12/31/2001
*:****************************************************************
*: Calls 
*:               : FUNCTIONS  : lfwLine  , lfvNew   , lpClose   ,
*:               :            : lfvRemove, lfvOk    , lfvCancel ,
*:               :            : lfvDefalt, lfvStatus, lfvItemPr ,
*:               :            : lfvImport.
*:               -----------------------------------------------
*:               : PROCEDURE  : lfShow
*:****************************************************************
*: Passed Parameters  : None.
*:****************************************************************
*:C#200246,1.
*:****************************************************************
*:Modifications  :
*:B605874,1 ABD 04/17/2002 Add new field cFabric to hold the 
*:B605874,1 ABD            Fabric name while import from aria27.
*:****************************************************************
*:
*-- Begin Declaration variables.
*-- lcStyNum :- Variable to hold the style no from the style file
*-- lnUS807  :- Variable to hold the U.S.807 for Cancel.
*-- lnDut    :- Variable to hold the Dutable for Cancel.
*-- lnUS800  :- Variable to hold the U.S.800 for Cancel.
*-- lnNetDoz :- Variable to hold the Net/Dozen for Cancel.

*-- End Declaration variables.

EXTERNAL ARRAY laData,laScrMode

lnPoint  = 0         && Variable to hold the RECNO
lcStatus = ' ' 
llImport = .T.       && Variable to hold if the user Import data

lcStyNum =laData[01]
lnUS807  =laData[26]
lnDut    =laData[27]
lnUS800  =laData[28]
lnNetDoz =laData[30]

PUSH KEY
ON KEY LABEL ESC 
ON KEY LABEL ESC DO lpClose
DO (gcScrDir+gcAct_Appl+"\Hlchdln.SPX")
POP KEY

RETURN
*-- End of program code.
*:*************************************************************
*: Name      : lfwLine
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 12/31/2001
*: Purpose   : To high light the new line.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : = lfwLine()
*:*************************************************************
*:
FUNCTION lfwLine

** To high light the new line **

lsLine=ccvllinno+' '+SUBSTR(ccvlitmso,1,15)+' '+SUBSTR(ccvsctdis,1,15)+' '+STR(ROUND(nCvLitMqt,1),7,1)+'/'+SUBSTR(cCvLitMuo,1,3)+' '+STR(ROUND(nCvLitMpr,1),7,1)+'/'+SUBSTR(cCvLitMpu,1,3)+' '+STR(ROUND(ncvlitmex,1),8,1)

rbDefalt=IIF(EMPTY(cCvsDefTy),1,AT(cCvsDefTy,'7D8')) && CVS default

SHOW GET rbDefalt  

SHOW GETS ONLY

*-- End OF lfwLine.
*:*************************************************************
*: Name      : lfShow
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 12/31/2001
*: Purpose   : Show function.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : Do lfShow
*:*************************************************************
*:
PROCEDURE lfShow

EXTERNAL ARRAY laScrMode

DO CASE

  CASE laScrMode[2]                      && View Mode
    SHOW GETS DISABLE ONLY
    SHOW GET  pbZoom ENABLE
    SHOW GET  pbOk   ENABLE
    SHOW GET  lsLine ENABLE

  CASE laScrMode[1] .OR. laScrMode[4]    && Select or Add Mode
    SHOW GET pbNew      ENABLE  
    SHOW GET pbCancel   ENABLE
    SHOW GET laData[30]
    SHOW GET ncvlitmex

    IF llImport OR lnLinNo = 0
      SHOW GET pbImport ENABLE
    ENDIF

    IF lnLinNo > 0
      SHOW GET PBNew      ENABLE 
      SHOW GET pbCancel   ENABLE
      SHOW GET pbZoom     ENABLE
      SHOW GET laData[29] ENABLE
      SHOW GET laData[30] ENABLE
      SHOW GET ccvsctdis  ENABLE
      SHOW GET ccvlitmds  ENABLE
      SHOW GET ncvlitmqt  ENABLE
      SHOW GET ccvlitmuo  ENABLE
      SHOW GET ncvlitmpr  ENABLE
      SHOW GET ccvlitmpu  ENABLE
      SHOW GET rbDefalt   ENABLE
      SHOW GET ccvlitmso  ENABLE
      SHOW GET ncvlconv   ENABLE
    ELSE
      SHOW GET laData[30] DISABLE
      SHOW GET ccvsctdis  DISABLE
      SHOW GET ccvlitmds  DISABLE
      SHOW GET ncvlitmqt  DISABLE
      SHOW GET ccvlitmuo  DISABLE
      SHOW GET ncvlitmpr  DISABLE
      SHOW GET ccvlitmpu  DISABLE
      SHOW GET rbDefalt   DISABLE
      SHOW GET ccvlitmso  DISABLE
      SHOW GET ncvlconv   DISABLE     && New 10/24/1993
    ENDIF

  CASE laScrMode[3]                   && Edit Mode
    SHOW GET PBNew      ENABLE 
    SHOW GET pbCancel   ENABLE
    SHOW GET pbZoom     ENABLE
    SHOW GET laData[29] ENABLE
    SHOW GET laData[30] ENABLE
    SHOW GET ccvsctdis  ENABLE
    SHOW GET ccvlitmds  ENABLE
    SHOW GET ncvlitmqt  ENABLE
    SHOW GET ccvlitmuo  ENABLE
    SHOW GET ncvlitmpr  ENABLE
    SHOW GET ccvlitmpu  ENABLE
    SHOW GET rbDefalt   ENABLE
    SHOW GET ccvlitmso  ENABLE
    SHOW GET ncvlconv   ENABLE

    IF llImport OR lnLinNo = 0
      SHOW GET pbImport ENABLE
    ENDIF

ENDCASE

rbDefalt=IIF(EMPTY(cCvsDefTy),1,AT(cCvsDefTy,'7D8'))

IF laScrMode[2] = .F.

  IF lnLinNo > 0 
    SHOW GET pbRemove ENABLE
  ELSE
    SHOW GET pbRemove DISABLE
    llImport = .T.
  ENDIF 

ENDIF  

SHOW GET rbDefalt  
SHOW GET lcStyNum 
SHOW GET laData[26] 
SHOW GET laData[27] 
SHOW GET laData[28]    

*-- End OF lfShow. 
*:*************************************************************
*: Name      : lfvNew
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 12/31/2001
*: Purpose   : For adding new line in the list
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : = lfvNew()
*:*************************************************************
*:
FUNCTION lfvNew

COUNT TO lnFcount
lnFcount = lnFcount + 1
lnLinNo  = lnLinNo  + 1

** For adding new line in the list **
APPEND BLANK

REPLACE cStatus   WITH "A",;
        ccvsdefty WITH "7",;
        cStyNum   WITH laData[1],;
        cCvlLinNo WITH IIF(len(ALLTRIM(STR(lnFcount)))=1,;
                           '0'+ALLTRIM(STR(lnFcount)),;
                            ALLTRIM(STR(lnFcount)))

** To high light the new line **

lsLine=ccvllinno+' '+SUBSTR(ccvlitmso,1,15)+' '+SUBSTR(ccvsctdis,1,15)+' '+STR(ROUND(nCvLitMqt,1),7,1)+'/'+SUBSTR(cCvLitMuo,1,3)+' '+STR(ROUND(nCvLitMpr,1),7,1)+'/'+SUBSTR(cCvLitMpu,1,3)+' '+STR(ROUND(ncvlitmex,1),8,1)

rbDefalt = 1           && Default value for the radio button Default

SHOW GET ncvlitmex  
SHOW GET lsLine
SHOW GET lcStyNum   
SHOW GET pbZoom     ENABLE
SHOW GET ccvsctdis  ENABLE
SHOW GET ccvlitmds  ENABLE
SHOW GET ncvlitmqt  ENABLE
SHOW GET ccvlitmuo  ENABLE
SHOW GET ncvlitmpr  ENABLE
SHOW GET ccvlitmpu  ENABLE
SHOW GET rbDefalt   ENABLE
SHOW GET laData[29] ENABLE
SHOW GET laData[30] ENABLE
SHOW GET pbRemove   ENABLE
SHOW GET ccvlitmso  ENABLE
SHOW GET ncvlconv   ENABLE    && New 10/24/1993

_CUROBJ = OBJNUM(ccvlitmso)

*-- End OF lfvNew.
*:*************************************************************
*: Name      : lfvRemove
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 12/31/2001
*: Purpose   : For Remove line from the list.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : = lfvRemove()
*:*************************************************************
*:
FUNCTION lfvRemove

lcStatus = SUBSTR('DDS',AT(cStatus,'SMA'),1)  && DELETE

REPLACE cStatus WITH lcStatus

DELETE

IF !EOF()
  SKIP
ELSE
  SKIP-1  
ENDIF  

REPLACE REST ccvllinno WITH;
             IIF(LEN(ALLTRIM(STR(VAL(ccvllinno)-1,2)))=1,;
                 "0"+STR(VAL(ccvllinno)-1,1),;
                 STR(VAL(ccvllinno)-1,2)),;
            cStatus WITH SUBSTR('MMA',AT(cStatus,'SMA'),1)

** To high light the new line **
lsLine=ccvllinno+' '+SUBSTR(ccvlitmso,1,15)+' '+SUBSTR(ccvsctdis,1,15)+' '+STR(ROUND(nCvLitMqt,1),7,1)+'/'+SUBSTR(cCvLitMuo,1,3)+' '+STR(ROUND(nCvLitMpr,1),7,1)+'/'+SUBSTR(cCvLitMpu,1,3)+' '+STR(ROUND(ncvlitmex,1),8,1)

lnFcount = IIF(RECNO() > RECCOUNT(),0,RECNO())

** Summsion on the totals **
SUM ROUND(ncvLitmex,2) TO laData[26] FOR ccvsdefty ="7"
SUM ROUND(ncvLitmex,2) TO laData[27] FOR ccvsdefty ="D"
SUM ROUND(ncvLitmex,2) TO laData[28] FOR ccvsdefty ="8"

IF lnFcount > 0
  GO lnFcount
ENDIF  

IF lnLinNo > 0
  lnLinNo = lnLinNo-1
ENDIF  

IF lnLinNo > 0 
  SHOW GET pbRemove ENABLE
ELSE
  SHOW GET pbRemove DISABLE
  llImport = .T.
ENDIF 

SHOW GET laData[26]
SHOW GET laData[27]
SHOW GET laData[28]
SHOW GET lsLine
 
_CUROBJ = OBJNUM(lsLine)

*-- End Of lfvRemove.
*:*************************************************************
*: Name      : lfvOk
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 12/31/2001
*: Purpose   : clear read the screen to update the files.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : = lfvOk()
*:*************************************************************
*:
FUNCTION lfvOk

llupdCvs=.T.

*-- End Of lfvOk.
*:*************************************************************
*: Name      : lfvCancel
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 12/31/2001
*: Purpose   : Cancel the Screen
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : = lfvCancel()
*:*************************************************************
*:
FUNCTION lfvCancel

laData[26] = lnUS807  
laData[27] = lnDut    
laData[28] = lnUS800  
laData[30] = lnNetDoz 

llupdCvs   = .F.
*-- End Of lfvCancel.
*:*************************************************************
*: Name      : lfvDefalt
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 12/31/2001
*: Purpose   : Get the default value
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : = lfvDefalt()
*:*************************************************************
*:
FUNCTION lfvDefalt

lnRecNo = IIF(RECNO() > RECCOUNT(),0,RECNO())

DO CASE

  CASE rbDefalt = 1
    REPLACE ccvsdefty WITH "7"

  CASE rbDefalt = 2  
    REPLACE ccvsdefty WITH "D"

  CASE rbDefalt = 3
    REPLACE ccvsdefty WITH "8"

ENDCASE

** Summision of the totals **
SUM ROUND(ncvLitmex,2) TO laData[26] FOR ccvsdefty ="7"
SUM ROUND(ncvLitmex,2) TO laData[27] FOR ccvsdefty ="D"
SUM ROUND(ncvLitmex,2) TO laData[28] FOR ccvsdefty ="8"

IF lnRecNo > 0
  GO lnRecNo
ENDIF  

SHOW GET laData[26] 
SHOW GET laData[27] 
SHOW GET laData[28]    

lcStatus = SUBSTR('MMA',AT(cStatus,'SMA'),1)  && Modify

REPLACE cStatus WITH lcStatus

*-- End Of lfvDefalt.
*:*************************************************************
*: Name      : lfvStatus
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 12/31/2001
*: Purpose   : Valid the satus.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : = lfvStatus()
*:*************************************************************
*:
FUNCTION lfvStatus

REPLACE cStatus WITH SUBSTR('MMA',AT(cStatus,'SMA'),1)  && Modify

** To high light the new line **
lsLine=ccvllinno+' '+SUBSTR(ccvlitmso,1,15)+' '+SUBSTR(ccvsctdis,1,15)+' '+STR(ROUND(nCvLitMqt,1),7,1)+'/'+SUBSTR(cCvLitMuo,1,3)+' '+STR(ROUND(nCvLitMpr,1),7,1)+'/'+SUBSTR(cCvLitMpu,1,3)+' '+STR(ROUND(ncvlitmex,1),8,1)

SHOW GET lsLine
*-- End OF lfvStatus.
*:*************************************************************
*: Name      : lfvItemPr
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 12/31/2001
*: Purpose   : Valid the Item Price.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : = lfvItemPr()
*:*************************************************************
*:
FUNCTION lfvItemPr

lnSavobj = _CUROBJ

lnExt = ROUND(IIF(ncvlconv > 0,;
        ((ROUND(ncvlitmpr,2) / ncvlconv) *;
        ROUND(ncvlitmqt,1)),ROUND(ncvlitmqt,1)*;
        ROUND(ncvlitmpr,2)),2)

IF lnExt > 999999.999
  *- Message Text   :- Computing Extension is too long.
  *- Message No.    :- 00000.
  *- Buttom Message :- OK.
  *- Buttom Number  :- 00000.
  = gfModalGen('QRM00000B00000','F','ALERT',' ','Computing Extension is too long.')
   _CUROBJ=lnSavobj  && For saving the object number that I came from 
ELSE
  lcStatus = SUBSTR('MMA',AT(cStatus,'SMA'),1)  && Modify

  ** New 10/24/1993 **
  
  REPLACE ncvlitmex WITH ROUND(IIF(ncvlconv > 0,;
                         ((ROUND(ncvlitmpr,2) / ncvlconv) *;
                         ROUND(ncvlitmqt,1)),ROUND(ncvlitmqt,1)*;
                         ROUND(ncvlitmpr,2)),2),;
            cStatus WITH lcStatus 
ENDIF  

lnFcount = IIF(RECNO() > RECCOUNT(),0,RECNO())

SUM ROUND(ncvLitmex,2) TO laData[26] FOR ccvsdefty ="7"
SUM ROUND(ncvLitmex,2) TO laData[27] FOR ccvsdefty ="D"
SUM ROUND(ncvLitmex,2) TO laData[28] FOR ccvsdefty ="8"

IF lnFcount > 0
  GO lnFcount
ENDIF  

SHOW GET laData[26] 
SHOW GET laData[27] 
SHOW GET laData[28]    
SHOW GET ncvlitmex

** To high light the new line **
lsLine=ccvllinno+' '+SUBSTR(ccvlitmso,1,15)+' '+SUBSTR(ccvsctdis,1,15)+' '+STR(ROUND(nCvLitMqt,1),7,1)+'/'+SUBSTR(cCvLitMuo,1,3)+' '+STR(ROUND(nCvLitMpr,1),7,1)+'/'+SUBSTR(cCvLitMpu,1,3)+' '+STR(ROUND(ncvlitmex,1),8,1)

SHOW GET lsLine

*-- End of lfvItemPr.
*:*************************************************************
*: Name      : lfvImport
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 12/31/2001
*: Purpose   : Import the style.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : = lfvImport()
*:*************************************************************
*:
FUNCTION lfvImport
PRIVATE llHlcvsetu

STORE .F. TO llHlcvsetu , llImport
lnHold   = 0

*- Message Text   :- Are you sure you want to import data from Aria27 system ?
*- Message No.    :- 38230.
*- Buttom Message :- \<Yes;\<No
*- Buttom Number  :- 38006.
IF gfModalGen('INM38230B38006','DIALOG') = 2
  _CUROBJ = OBJNUM(lsLine)
  RETURN .F.
ELSE
  *-- Open Needed File.
  IF !USED('HLCVSETU')
    llHlcvsetu = gfOpenFile(gcDataDir+'HLCVSETU',gcDataDir+'Ccvsptord','SH')
  ENDIF
  SELECT HLCVSETU
  GO TOP
  SCAN WHILE !EOF()
    DO CASE
      CASE cCvsImpMt = "M"      && Manual Import
        SELECT (lc_Temp)
        COUNT TO lnFcount
        lnFcount=lnFcount+1

        APPEND BLANK

        REPLACE cStatus   WITH "A"                              ,;
                ccvsdefty WITH HLCVSETU.ccvsdefty               ,;
                cStyNum   WITH laData[1]                        ,;
                ccvstype  WITH HLCVSETU.cCvSimpco               ,;
                cCvSctDis WITH HLCVSETU.cCvSctDis               ,;
                cCvlLinNo WITH IIF(len(ALLTRIM(STR(lnFcount)))=1,;
                                      '0'+ALLTRIM(STR(lnFcount)),;
                                       ALLTRIM(STR(lnFcount)))   ;
        
      CASE cCvsImpMt = "D"      && Defautl setup Import
        SELECT HLCVSETU
        lcCvSImpco=ALLTRIM(cCvSImpco)
        IF !EMPTY(lcCvSImpco) .AND. TYPE('HLSETUP.&lcCvSImpco.') = "N"
          lnHold = HLSETUP.&lcCvSImpco
        ENDIF

        SELECT (lc_Temp)
        COUNT TO lnFcount
        lnFcount=lnFcount+1

        APPEND BLANK
        
        REPLACE cStatus   WITH "A"                                   ,;
                cStyNum   WITH laData[1]                             ,;
                cCvSctDis WITH HLCVSETU.cCvSctDis                    ,;
                ncvlitmqt WITH 1                                     ,;
                ncvlitmpr WITH lnHold                                ,;
                ccvsdefty WITH HLCVSETU.ccvsdefty                    ,;
                ccvstype  WITH HLCVSETU.cCvSimpco                    ,;
                ncvlitmex WITH ROUND(IIF(ncvlconv > 0                ,;
                               ((ROUND(ncvlitmpr,2) / ncvlconv) *     ;
                               ROUND(ncvlitmqt,1)),ROUND(ncvlitmqt,1)*;
                               ROUND(ncvlitmpr,2)),2)                ,;
                cCvlLinNo WITH IIF(len(ALLTRIM(STR(lnFcount)))=1     ,;
                                   '0'+ALLTRIM(STR(lnFcount))        ,;
                                   ALLTRIM(STR(lnFcount)))
                               
      CASE cCvsImpMt = "A"       && Import from OLD ARIA
        lcCvSImpco = ALLTRIM(HLCVSETU.cCvSImpco)
        *-- Set Order to Item_Key To speed up the Performance.
        SELECT (lc_Temp)
        lcOldOrder = ORDER()
        SET ORDER TO Item_Key
        IF !EMPTY(lcCvSImpco)
          SELECT BOM
          = SEEK(PADR(laData[24]+'-'+laData[1],19))
          SCAN REST WHILE citmmajor+typ+citmmask+mfgcode+item+iclr = ;
                    PADR(laData[24]+'-'+laData[1],19)
            IF !SEEK (PADR(Item,7),'Fabric') .OR. !(&lcCvSImpco)
              LOOP
            ENDIF
            STORE '' TO lcSource , lcUOMUse , lcUOMBuy , lcTyp, lcItmDesc , lcType
            STORE 0  TO lnItemPr , lnConv   , lnQty 
            DO CASE
              CASE TYP = '1'
                lcType = ''
              *- Abdou  
              *CASE TYP = '2'
              *  lcType = 'L'
              *CASE TYP = '3'
              *- Abdou
              CASE TYP = '2'
                lcType = 'T'
            ENDCASE 
            *-- For Selecting only one record per each  time.
            *-- Add the marker to the Item_key ,If the record is alredy exist in the file loop.
            IF SEEK(TYP+citmmajor+Item+cMARKER,lc_Temp)
              SELECT BOM
              LOOP
            ENDIF
            
            *-- Get the Qty From BOM.
            SELECT BOM
            lcTyp     = TYP 
            lnQty     = IIF(lcType='T',NBOMTOTQTY*12,NBOMTOTQTY)
            lcItmDesc = DESC

            SELECT FABRIC
            IF !EMPTY(Bom.Item) .AND. SEEK(LEFT(Bom.Item,7))
              lcUOMUse = UOMUSE
              lcUOMBuy = UOMBUY
              lnItemPr = IIF(BOM.TYP= '3',nFabCost + nItemQuota,0)
              lnConv   = Conv
            ENDIF
              
            SELECT Apvendor
            IF !EMPTY(Fabric.Vendor) .AND. SEEK(Fabric.Vendor)
              lcSource = CShrtNam
            ENDIF
              
            SELECT (lc_Temp)
            COUNT TO lnFcount
            lnFcount=lnFcount+1
            
            APPEND BLANK
            *B605874,1 ABD Add new field cFabric to hold the 
            *B605874,1 ABD Fabric name while import from aria27. [Begin]
            *REPLACE cStatus   WITH "A"                                       ,;
                    cStyNum   WITH laData[1]                                 ,;
                    ccvlitmso WITH lcSource                                  ,;
                    cCvSctDis WITH HLCVSETU.cCvSctDis                        ,;
                    cCvlItmDs WITH lcItmDesc                                 ,;
                    nCvlItmQt WITH lnQty                                     ,;   
                    cCvlItmUO WITH lcUOMUse                                  ,;
                    nCvlItmPr WITH lnItemPr                                  ,;
                    ncvlconv  WITH lnConv                                    ,;
                    cCvlItmPu WITH lcUOMBuy                                  ,;
                    ncvlitmex WITH ROUND(IIF(ncvlconv > 0                    ,;
                                   ((ROUND(ncvlitmpr,2) / ncvlconv) *         ;
                                   ROUND(ncvlitmqt,1)),ROUND(ncvlitmqt,1)*    ;
                                   ROUND(ncvlitmpr,2)),2)                    ,;
                    ccvsdefty WITH HLCVSETU.ccvsdefty                        ,;
                    ccvstype  WITH lcTyp                                     ,;
                    Item_key  WITH BOm.TYP+Bom.citmmajor+Bom.Item+Bom.cMARKER,;
                    cCvlLinNo WITH IIF(len(ALLTRIM(STR(lnFcount)))=1         ,;
                                     '0'+ALLTRIM(STR(lnFcount))              ,;
                                      ALLTRIM(STR(lnFcount)))


            REPLACE cStatus   WITH "A"                                       ,;
                    cStyNum   WITH laData[1]                                 ,;
                    ccvlitmso WITH lcSource                                  ,;
                    cCvSctDis WITH HLCVSETU.cCvSctDis                        ,;
                    cCvlItmDs WITH lcItmDesc                                 ,;
                    nCvlItmQt WITH lnQty                                     ,;   
                    cCvlItmUO WITH lcUOMUse                                  ,;
                    nCvlItmPr WITH lnItemPr                                  ,;
                    ncvlconv  WITH lnConv                                    ,;
                    cCvlItmPu WITH lcUOMBuy                                  ,;
                    ncvlitmex WITH ROUND(IIF(ncvlconv > 0                    ,;
                                   ((ROUND(ncvlitmpr,2) / ncvlconv) *         ;
                                   ROUND(ncvlitmqt,1)),ROUND(ncvlitmqt,1)*    ;
                                   ROUND(ncvlitmpr,2)),2)                    ,;
                    ccvsdefty WITH HLCVSETU.ccvsdefty                        ,;
                    ccvstype  WITH lcTyp                                     ,;
                    Item_key  WITH BOm.TYP+Bom.citmmajor+Bom.Item+Bom.cMARKER,;
                    cCvlLinNo WITH IIF(len(ALLTRIM(STR(lnFcount)))=1         ,;
                                     '0'+ALLTRIM(STR(lnFcount))              ,;
                                      ALLTRIM(STR(lnFcount)))                ,;
                    cFabric   WITH  PADR(Bom.Item,7)

              *B605874,1 ABD [End]

            SELECT BOM
          ENDSCAN
        ENDIF
        SELECT (lc_Temp)
        SET ORDER TO &lcOldOrder
    ENDCASE
    SELECT HLCVSETU
  ENDSCAN
  SELECT (lc_Temp)
  lnPoint = IIF(RECNO() > RECCOUNT(),0,RECNO())
  GO TOP

  IF lnPoint > 0

    SUM ROUND(ncvLitmex,2) TO laData[26] FOR ccvsdefty ="7"
    SUM ROUND(ncvLitmex,2) TO laData[27] FOR ccvsdefty ="D"
    SUM ROUND(ncvLitmex,2) TO laData[28] FOR ccvsdefty ="8"

    GO lnPoint
    SHOW GET laData[26]
    SHOW GET laData[27]
    SHOW GET laData[28]
    SHOW GET laData[29] ENABLE
    SHOW GET laData[30] ENABLE

    IF lnFcount > 0
      SHOW GET pbRemove ENABLE
    ENDIF  

    SHOW GET PBNew      ENABLE 
    SHOW GET pbCancel   ENABLE
    SHOW GET pbZoom     ENABLE
    SHOW GET cCvlItmSo  ENABLE
    SHOW GET cCvsCtDis  ENABLE
    SHOW GET cCvlItmDs  ENABLE
    SHOW GET nCvlItmQt  ENABLE
    SHOW GET cCvlItmUo  ENABLE
    SHOW GET nCvlItmPr  ENABLE
    SHOW GET ncvlconv   ENABLE       && New 10/24/1993
    SHOW GET cCvlItmPu  ENABLE
    SHOW GET rbDefalt   ENABLE
    SHOW GET nCvlItmEx
    SHOW GET cCvlItmSo  ENABLE
    SHOW GET lcStyNum   
    SHOW GET pbImport   DISABLE

  ENDIF
ENDIF  

lnLinno = lnFcount

** To high light the new line **

lsLine=ccvllinno+' '+SUBSTR(ccvlitmso,1,15)+' '+SUBSTR(ccvsctdis,1,15)+' '+STR(ROUND(nCvLitMqt,1),7,1)+'/'+SUBSTR(cCvLitMuo,1,3)+' '+STR(ROUND(nCvLitMpr,1),7,1)+'/'+SUBSTR(cCvLitMpu,1,3)+' '+STR(ROUND(ncvlitmex,1),8,1)

_CUROBJ = OBJNUM(lsLine)

SHOW GET lsLine

*-- Close Open File.
IF llHlcvsetu
  = gfCloseFile('HLCVSETU')
ENDIF
*-- End Of lfvImport.
*:*************************************************************
*: Name      : lpClose
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 12/31/2001
*: Purpose   : Trap Esc for lines entry.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : None.
*:*************************************************************
*: Returns   :  None.
*:*************************************************************
*: Example   :  DO lpClose
*:*************************************************************
*
PROCEDURE lpClose

IF laScrMode [3] .OR. laScrMode [4]
  _CUROBJ = OBJNUM(pbCancel)
ELSE
  _CUROBJ = OBJNUM(pbOk)
ENDIF  
KEYBOARD '{ENTER}'
RETURN

*-- End Of lpClose
*:*************************************************************