*:********************************+****************************
*: Program file   : ROB3900
*: Program desc.  : Declaration of Value (Add\Modify\Inquire)
*: For screen     : ROB3900
*:         System : Aria Apparel System
*:         Module : ( Custom Program ) - Robin Meridith
*:      Developer : Haytham El_Sheltawi
*:*************************************************************
*: Calls : 
*:         Procedures : None
*:         Functions  : None
*:*************************************************************
*: Passed Parameters  : None
*:*************************************************************
*: Example : DO ROB3900
*:*************************************************************
*C100877,1 This Program was added by HS for the custom program
*:*************************************************************
*: Modifcations:
*: B601888,1 ASH 11/04/97 Fix some spelling mistakes in the screens.
*C101059,1 ASH 12/22/97 Fix some problems due to specs.
*:*************************************************************
*
EXTERNAL ARRAY laData 
DECLARE laCompAdd[6,1]
STORE SPACE(0) TO laCompAdd
laDefProc = .T.
laScrMode[1] = .T.
laDefProc[7]  = .F.              && Disable the control panel delete proc.(lpDelScr)
laDefProc[9]  = .F.              && Disable the control panel save proc.  (lpSavScr)
STORE .T. TO llAlowNew
IF !gfSetup()
  RETURN
ENDIF

Dimension laCatgTyp[5,3]
laCatgTyp[1,2]  = ALLTRIM(PROPER(gfGetMemVar('M_CITYPE1')))
laCatgTyp[2,2]  = ALLTRIM(PROPER(gfGetMemVar('M_CITYPE2')))
laCatgTyp[3,2]  = ALLTRIM(PROPER(gfGetMemVar('M_CITYPE3')))
laCatgTyp[4,2]  = ALLTRIM(PROPER(gfGetMemVar('M_CITYPE4')))
laCatgTyp[5,2]  = ALLTRIM(PROPER(gfGetMemVar('M_CITYPE5')))
laCatgTyp[1,1]  = ALLTRIM(PROPER(gfGetMemVar('M_CISLBL1')))
laCatgTyp[2,1]  = ALLTRIM(PROPER(gfGetMemVar('M_CISLBL2')))
laCatgTyp[3,1]  = ALLTRIM(PROPER(gfGetMemVar('M_CISLBL3')))
laCatgTyp[4,1]  = ALLTRIM(PROPER(gfGetMemVar('M_CISLBL4')))
laCatgTyp[5,1]  = ALLTRIM(PROPER(gfGetMemVar('M_CISLBL5')))
laCatgTyp[1,3]  = 1
laCatgTyp[2,3]  = 2
laCatgTyp[3,3]  = 3
laCatgTyp[4,3]  = 4
laCatgTyp[5,3]  = 5

DIME laMajSeg[1,1]
=gfItemMask(@laMajSeg)
lcMjrPct  = gfItemMask('PM')
lnstylewid = LEN(lcMjrPct)


*=gfGetMemvar(@laCatgTyp,gcAct_Comp)

lcCatgDes = laCatgTyp[1,1]
lcCatgTyp = laCatgTyp[1,2]
lnCatgTyp = 1

=gfOpenFile(gcSysHome+'SYCCOMP','Ccomp_id','SH')
SEEK gcAct_Comp

lcCompName = cCom_Name
lcCompPhon = cCom_Phon             && Variable to hold the Company Phone
lcPhonPict  = gfPhoneTem()          && Variable to hold the Company Phone Format
* Get the company addresses
laCompAdd[1]    = gfGetAdr('SYCCOMP' , '' , '' , '' , 1)
laCompAdd[2]    = gfGetAdr('SYCCOMP' , '' , '' , '' , 2)
laCompAdd[3]    = gfGetAdr('SYCCOMP' , '' , '' , '' , 3)
laCompAdd[4]    = gfGetAdr('SYCCOMP' , '' , '' , '' , 4)
laCompAdd[5]    = gfGetAdr('SYCCOMP' , '' , '' , '' , 5)
laCompAdd[6]    = 'Phone# : '+TRANSFORM(lcCompPhon , lcPhonPict)

DIMENSION laData[23]
laData = ' '
lcHeader = 'DecValH'    && Varible to hold the Haeder screen name
lcTitle = 'Declaration of value'    && Varible to hold the Haeder screen title
llFromBrow = .F.    && Flag to hold .T. if we are coming from the Browse
llMadeChng = .F.    && Flag to know if the user has made any changes [used in the Edit mode only]

STORE '' TO lcEscTrap , lcCtrETrap , lcCtrHTrap , lcCtrNTrap , laOldVal

lcParent = 'DecValD'    && Varible to hold the Detail main screen name
lcDTitle = ''           && Varible to hold the Detail main screen title
lcRobCh1 = 'ICDECVL1'   && Varible to hold the Detail chilled1 screen name
lcRobCh2 = 'ICDECVL2'   && Varible to hold the Detail chilled2 screen name
lcRobCh3 = 'ICDECVL3'   && Varible to hold the Detail chilled3 screen name

lcBrowTitl = 'Items'    && Varible to hold the Component browse title
lcBrowStr = "lcMarker=IIF(RECNO()=lnBrRecNo,'',' '):1:H=' ':W=.F.,"+;
            "cStyMajor :H= 'Style' :R , " +;
            "Type      :H= 'T' :R , " +;
            "cItem     :H= 'Item' :R , " +;
            "Yield     :H= 'Yield' :P= '999.999' :R , " +;
            "nPrice    :H= 'Price' :P= '999.999' :R , " +;
            "nUS806807 :H= 'US 806/807' :P= '9999.99' :R , " +;
            "Dutiable  :H= 'Dutiable' :P= '9999.99' :R , " +;
            "n800      :H= '800' :P= '9999.99' :R"


lcSource = ''     && Varible to create the Source list
lcTarget = ''     && Varible to create the Target list

lcListStat = 'DISABLE'    && Varible to hold the Source and Target lists Status
lcPOStat = 'ENABLE'    && Varible to hold the PO get field Status
lcDtlStat = 'DISABLE'    && Varible to hold the Detail push button Status
lcFrstStat = 'DISABLE'    && Varible to hold the First push button Status
lcLastStat = 'DISABLE'    && Varible to hold the Last push button Status
lcEdtStat = 'DISABLE'    && Varible to hold the Edit push button Status
lcBrowStat = 'ENABLE'    && Varible to hold the Browse push button Status
lcSaveStat = 'DISABLE'    && Varible to hold the Save push button Status
lcMoveStat = 'DISABLE'    && Varible to hold the Move push button Status
lcRMovStat = 'DISABLE'    && Varible to hold the Remove push button Status
lcColsPrmp = '\<Close'    && Varible to hold the Close [Cancel] push button Prompt

lnSource = 0    && Varible to hold the number of Styles in the Source list
lnTarget = 0    && Varible to hold the number of Styles in the Target list

lnBrRecNo = 0    && Varible to hold the Component browse record number
SELECT Style
SET ORDER TO cStyle
SELECT PoSLn
SET ORDER TO PoSLn

IF !WEXIST(gcBaseWind)
  lcSourcSty = gfTempName()    && Varible to hold a temp. name for the Source list file
  lcTargtSty = gfTempName()    && Varible to hold a temp. name for the Target list file
  lcTmpLines = gfTempName()    && Varible to hold a temp. name for the Component file
  lcTmpIndex = gfTempName()    && Varible to hold a temp. name for one of the Component file Indexs

  CREATE CURSOR &lcSourcSty (cStyMajor C(19) , lSelBefor L , nRecNo N(6))
  INDEX ON cStyMajor TAG &lcSourcSty

  SELECT DECVALST
  =AFIELDS(laFileStru)
  DIMENSION laFileStru(ALEN(laFileStru , 1) + 2 , 4)
  laFileStru(ALEN(laFileStru , 1) - 1 , 1) = 'lSelBefor'
  laFileStru(ALEN(laFileStru , 1) - 1 , 2) = 'L'
  laFileStru(ALEN(laFileStru , 1) - 1 , 3) = 1
  laFileStru(ALEN(laFileStru , 1) - 1 , 4) = 0
  laFileStru(ALEN(laFileStru , 1) , 1) = 'nRecNo'
  laFileStru(ALEN(laFileStru , 1) , 2) = 'N'
  laFileStru(ALEN(laFileStru , 1) , 3) = 6
  laFileStru(ALEN(laFileStru , 1) , 4) = 0

  CREATE CURSOR &lcTargtSty FROM ARRAY laFileStru
  INDEX ON cStyMajor TAG &lcTargtSty

  SELECT DECVALCM

  =AFIELDS(laFileStru)
  DIMENSION laFileStru(ALEN(laFileStru , 1) + 3 , 4)

  laFileStru(ALEN(laFileStru , 1) - 2 , 1) = 'cItem1'
  laFileStru(ALEN(laFileStru , 1) - 2 , 2) = 'C'
  laFileStru(ALEN(laFileStru , 1) - 2 , 3) = 12
  laFileStru(ALEN(laFileStru , 1) - 2 , 4) = 0
  laFileStru(ALEN(laFileStru , 1) - 1 , 1) = 'Status'
  laFileStru(ALEN(laFileStru , 1) - 1 , 2) = 'C'
  laFileStru(ALEN(laFileStru , 1) - 1 , 3) = 1
  laFileStru(ALEN(laFileStru , 1) - 1 , 4) = 0
  laFileStru(ALEN(laFileStru , 1) , 1) = 'nRecNo'
  laFileStru(ALEN(laFileStru , 1) , 2) = 'N'
  laFileStru(ALEN(laFileStru , 1) , 3) = 9
  laFileStru(ALEN(laFileStru , 1) , 4) = 0

  CREATE CURSOR &lcTmpLines FROM ARRAY laFileStru
  INDEX ON cStyMajor + cType + cItem1 TAG &lcTmpIndex
  INDEX ON cStyMajor + cType + cItem  TAG &lcTmpLines

  lcScFields = 'PO        , cManf      , cCompany   ,' + ; 
               'cAddress1 , cAddress2  , cAddress3  ,' + ;
               'cAddress4 , cAddress5  '
  SELECT DecValHd
  SET ORDER TO DecValHd
  SCATTER FIELDS &lcScFields MEMO TO laData BLANK               

ENDIF

PUSH KEY CLEAR

DO (gcScrDir+gcWinAppl+"\ICDECVL.SPX")

POP KEY

*****
FUNCTION lpShow
DO CASE
  ***--- S E L E C T   M O D E ---***
  CASE laScrMode[1]  && Select mode. 
    lnAlias=SELECT()
    SELECT decvalhd
    lcSource = ''
    lcTarget = ''
    SELECT (lcSourcSty)
    DELETE ALL
    SELECT (lcTargtSty)
    DELETE ALL
    lcConName = ' '
    SCATTER FIELDS &lcScFields MEMO TO laData BLANK

  ***--- V I E W   M O D E ---***
  CASE laScrMode[2]  && View mode.
    SHOW GET PbDtl ENABLE
  ***--- E D I T   M O D E ---***
  CASE laScrMode[3]  && Edit mode. 

  ***--- A D D   M O D E ---***
  CASE laScrMode[4]  && Add mode. 
  
    SHOW GET laData[1] ENABLE
    SHOW GET laData[2] ENABLE
    SHOW GET laData[3] ENABLE
    SHOW GET laData[4] ENABLE
    SHOW GET laData[5] ENABLE
    SHOW GET laData[6] ENABLE
    SHOW GET laData[7] ENABLE
    SHOW GET laData[8] ENABLE
    SHOW GET lcSource  ENABLE
    SHOW GET lcTarget  ENABLE
    SHOW GET pbDtl     DISABLE
    SHOW GET pbMove    ENABLE
    SHOW GET PbMoveAll ENABLE
    SHOW GET pbRemove  DISABLE
    SHOW GET pbRemAll  DISABLE
    
ENDCASE



*!*************************************************************
*! Name      : lfwOldVal
*! Developer : Haytham El_Sheltawi
*! Date      : 07/28/1997
*! Purpose   : When function to get the old value
*!*************************************************************
*! Called from : Screens icDecVl , icDecD
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfwOldVal

laOldVal = EVALUATE(SYS(18))    && Varible to hold the old value of the curent field

*!*************************************************************
*! Name      : lfvBrowPO
*! Developer : Haytham El_Sheltawi
*! Date      : 07/28/1997
*! Purpose   : Valid function for the Browse Push Button
*!*************************************************************
*! Called from : Screens icDecVl
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvBrowPO

PRIVATE lcAlias
lcAlias = ALIAS()    && Varible to save the alias

lcBrFields  = "PO        :H= 'P/O' , "+;
              "cManf     :H= 'Manufacturer' , "+;
              "cCompany  :H= 'Company name' , "+;
              "cAddress1 :H= 'Company address 1' , "+;
              "cAddress2 :H= 'Company address 2' , "+;
              "cAddress3 :H= 'Company state' , "+;
              "cAddress4 :H= 'Company city' , "+;
              "cAddress5 :H= 'Company Zip code' "

lcBrowTtl = 'Declaration of value'    && Varible to hold the browse title

DECLARE laReturn[1] 
STORE '' TO laReturn

SELECT DECVALHD

=AriaBrow('', lcBrowTtl, gnBrHSRow1, gnBrHSCol1,;
          gnBrHSRow2, gnBrHSCol2,'','','PO','laReturn')

*IF The user has selected a record from the Browse
IF !EMPTY(laReturn[1])
  lcOldPO = laData[1]    && Varible to save the PO number
  laData[1] = laReturn
  =lfGetHdInf()
  
  *IF The PO number was changed
  IF laData[1] <> lcOldPO
    =lfSelData()
    laScrMode = .F.
    laScrMode[2] = .T.
    =lpShow()
  ENDIF    && End of IF
ENDIF    && End of IF

*IF The varible we used to save the old alias is not empty
IF !EMPTY(lcAlias)
  SELECT (lcAlias)
ENDIF    && End of IF

*!*************************************************************
*! Name      : lfvPO
*! Developer : Haytham El_Sheltawi
*! Date      : 07/28/1997
*! Purpose   : Valid function for the PO Number get field
*!*************************************************************
*! Called from : Screens icDecVl
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvPO

*IF The PO number is empty
IF EMPTY(laData[1])
  RETURN
ENDIF    && End of IF

*IF the user want to Browse or the PO number is not in the POSHDR file
IF '?' $ laData[1] .OR. !SEEK('P'+laData[1] , 'POSHDR')
  SELECT POSHDR
  DO POSBrow WITH laData[1],'','P'
  laData[1] = PO
ENDIF    && End of IF

*IF the PO number is not empty
IF !EMPTY(laData[1])
  
  *IF the PO is not in the DECVALHD file and the PO Status is canceled or
  *complete or closed
  IF SEEK('P'+laData[1] , 'POSHDR') .AND. POSHDR.Status $ 'XCS' .AND. !SEEK(laData[1] , 'DECVALHD')
    lcStatDesc  = IIF(POSHDR.Status = 'X' , 'canceled' ,;
                      IIF(POSHDR.Status = 'C' , 'complete' , 'closed'))

    =gfModalGen('INM42204B42000','DIALOG' , lcStatDesc )    
    
    laData[1] = SPACE(6)
    _CUROBJ = OBJNUM(laData[1])
    RETURN
  ELSE    && Else
    
    *IF the PO is not on the DECVALHD file
    IF !SEEK(laData[1] , 'DECVALHD')
      lnChoice=gfModalGen('QRM42205B42003','DIALOG')    
      
      *DO CASE Statment
      DO CASE
        
        *Case of Add
        CASE lnChoice = 1
          laData[2] = PADR(POSHDR.Vendor , 12)
          laData[3] = laCompAdd[1]
          laData[4] = laCompAdd[2]
          laData[5] = laCompAdd[3]
          laData[6] = laCompAdd[4]
          laData[7] = laCompAdd[5]
          laData[8] = laCompAdd[6]
          laScrMode = .F.
          laScrMode[4] = .T.
          SHOW GETS
        
        *Case of Reenter
        CASE lnChoice = 3
          laData[1] = SPACE(6)
          _CUROBJ = OBJNUM(laData[1])
          RETURN
          
        *Case of Browse
        CASE lnChoice = 2
          laData[1] = SPACE(6)
          =lfvBrowPO()
          
          *IF the PO number is empty
          IF EMPTY(laData[1])
            _CUROBJ = OBJNUM(laData[1])
          ENDIF    && End of IF
          RETURN
          
      ENDCASE    && End of DO CASE Statment
    ELSE    && Else
      =lfGetHdInf()
      laScrMode = .F.
      laScrMode[2] = .T.
    ENDIF    && End of IF
    =lfSelData()
    *=lpShow()
    SHOW GETS
  ENDIF    && End of IF
ENDIF    && End of IF
    
*!*************************************************************
*! Name      : lfSelData
*! Developer : Haytham El_Sheltawi
*! Date      : 07/28/1997
*! Purpose   : Function to select the data in the Temp. files
*!*************************************************************
*! Called from : lfvBrowPO() , lfvPO() , lfvClose() , lfvFirst()
*!               lfvPrev() , lfvLast() , lfvNext()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfSelData

PRIVATE lcAlias

lnSource = 0    && Varible to hold the number of Styles in the Source list
lnTarget = 0    && Varible to hold the number of Styles in the Target list

lcAlias = ALIAS()    && Varible to save the alias

SELECT (lcSourcSty)
ZAP
SET ORDER TO TAG &lcSourcSty

SELECT (lcTargtSty)
ZAP
SET ORDER TO TAG &lcTargtSty

SELECT (lcTmpLines)
ZAP
SET ORDER TO TAG &lcTmpIndex

M.PO = laData[1]    && Varible to hold the PO number
SELECT DECVALST
SET ORDER TO TAG DECVALST 
*IF the PO number exist in the DECVALST file
IF SEEK(laData[1])
  M.lSelBefor = .T.    &&    Flag to know if the Style was selected befor in the Declaration of value for this PO number
  
  *SCAN Loop to scan the DECVALST file for this PO number
  SCAN REST WHILE PO = laData[1]
    lnTarget = lnTarget + 1
    SCATTER MEMVAR
    INSERT INTO &lcTargtSty FROM MEMVAR
    INSERT INTO &lcSourcSty FROM MEMVAR
    SELECT (lcTargtSty)
    REPLACE nRecNo WITH RECNO(lcSourcSty)
    SELECT (lcSourcSty)
    REPLACE nRecNo WITH RECNO(lcTargtSty)
    DELETE
    SELECT DECVALST
    =lfGetItems(.T. , cStyMajor)
  ENDSCAN    && End of SCAN Loop
ENDIF    && End of IF

SELECT POSLN

*IF the PO number exist in the POSLN file
IF SEEK('P'+laData[1])
  M.lSelBefor = .F.    &&    Flag to know if the Style was selected befor in the Declaration of value for this PO number
  lcOldStyle = ''    && Varible to hold the Style which we are colecting data for
  
  *SCAN Loop to scan the POSLN file for the PO number and for Styles that
  *was not selected befor
  SCAN REST;
      WHILE cStyType+PO = 'P'+laData[1];
        FOR !SEEK(SUBSTR(Style,1,lnstylewid) , lcTargtSty)
        
    lnOpen = 0                    && Varible to hold the Open Qty. for every line
    lcStyle = Style  && Varible to hold the Style
    lcLineNo = STR(LineNo , 4)    && Varible to hold the Line number in string
    SUM REST IIF(TranCd = '1' , TotQty , -TotQty);
       WHILE cStyType + PO + Style + STR(LineNo , 4) = 'P' + laData[1] + lcStyle + lcLineNo;
          TO lnOpen
          
    *IF the Line is open
    IF lnOpen > 0
      *IF this is a new Style
      IF lcOldStyle <> SUBSTR(lcStyle,1,lnstylewid)
        lcOldStyle = SUBSTR(lcStyle,1,lnstylewid)
        lnSource = lnSource + 1
        SELECT STYLE
        SEEK SUBSTR(lcStyle,1,lnstylewid)
        M.cStyMajor= cStyMajor && MEMVAR to hold the Style
        M.Date    = DATE()   && MEMVAR to hold the Date
        M.Desc    = Desc     && MEMVAR to hold the Style Description
        M.Pattern = Pattern  && MEMVAR to hold the Pattern
        =SEEK(Fabric,'Fabric')
        M.FibreCont = Fabric.Desc    && MEMVAR to hold the Fabric
        INSERT INTO &lcTargtSty (cStyMajor , Date , Desc , Pattern , FibreCont) ;
               VALUES  (M.cStyMajor , M.Date , M.Desc , M.Pattern , M.FibreCont)
        INSERT INTO &lcSourcSty FROM MEMVAR
        SELECT (lcSourcSty)
        REPLACE nRecNo WITH RECNO(lcTargtSty)
        SELECT (lcTargtSty)
        REPLACE nRecNo WITH RECNO(lcSourcSty)
        DELETE
        INSERT INTO &lcTmpLines (cStyMajor , cItem , cType , nUS806807 , Dutiable , n800) ;
                         VALUES (SUBSTR(lcStyle,1,lnstylewid)   , '*****Total*****' , '7' , 0 , 0 , 0)
        =lfGetItems(.F. , SUBSTR(lcStyle,1,lnstylewid) , .T.)
      ELSE    && Else
        =lfGetItems(.F. , SUBSTR(lcStyle,1,lnStyleWid) , .F.)
      ENDIF    && End of IF
      SELECT POSLN
      
      *LOCATE Statment to locate the Next Style Color
      LOCATE REST;
            WHILE PO = laData[1];
              FOR Style  > lcStyle
    ENDIF    && End of IF
    SKIP - 1
  ENDSCAN    && End of Scan
ENDIF    && End of IF

GO TOP IN (lcSourcSty)
lcSource = &lcSourcSty..cStyMajor

GO TOP IN (lcTargtSty)
lcTarget = &lcTargtSty..cStyMajor

SELECT (lcTmpLines)
SET ORDER TO TAG &lcTmpLines

*IF The varible we used to save the old alias is not empty
IF !EMPTY(lcAlias)
  SELECT (lcAlias)
ENDIF    && End of IF

*!*************************************************************
*! Name      : lfGetItems
*! Developer : Haytham El_Sheltawi
*! Date      : 07/28/1997
*! Purpose   : Function to select the Item Lines in the Temp. file lcTmpLines
*!*************************************************************
*! Called from : lfSelData()
*!*************************************************************
*! Passed Parameters : 1).T. IF the PO STYLE exist in DECVALST file ,
*!                       .F. Otherwise
*!                     2)The Style
*!                     3).T. if the POSLN record is the first record with
*!                       this Style , .F. Otherwise
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfGetItems

PARAMETERS llExist , lcStyle , llFirst

*IF the PO Style exist in DECVALST file
IF llExist
  INSERT INTO &lcTmpLines (cStyMajor , cItem , cType , nUS806807 , Dutiable , n800) ;
                   VALUES (lcStyle , '*****Total*****' , '7' , 0 , 0 , 0)
  SELECT DECVALCM
  
  *IF the PO Style exist in DECVALCM file
  IF SEEK(laData[1] + lcStyle)
    M.Status = 'S'
    
    *SCAN Loop to scan the DECVALCM for the PO and the Style
    SCAN REST;
        WHILE PO + cStyMajor = laData[1] + lcStyle
        
      SCATTER MEMVAR
      M.nRecNo = RECNO()
      M.cType = IIF(M.cType = '1' , '6' , M.cType)
      INSERT INTO &lcTmpLines FROM MEMVAR
      =SEEK(PADR(lcStyle,19) + '7' , lcTmpLines)
      REPLACE &lcTmpLines..nUS806807 WITH &lcTmpLines..nUS806807 + M.nUS806807 ,;
              &lcTmpLines..Dutiable  WITH &lcTmpLines..Dutiable + M.Dutiable ,;
              &lcTmpLines..n800      WITH &lcTmpLines..n800 + M.n800
              
    ENDSCAN    && End of SCAN Loop
  ENDIF    && End of IF
ELSE    && Else
  SELECT BOM
  SET ORDER TO
  
  *SCAN Loop to scan the BOM file for Style 
  SCAN FOR citmmajor+typ+citmmask+mfgcode+item+iclr = lcStyle
           
    SELECT (lcTmpLines)
    
    *IF the Style Type Item is not in the Temp. file lcTmpLines
    IF !SEEK(PADR(lcStyle,19) + IIF(BOM.TYP = '1' , '6' , BOM.TYP) + SUBSTR(BOM.Item,1,12))
      APPEND BLANK
    ENDIF    && End of IF

    REPLACE PO        WITH laData[1] ,;
            cStyMajor WITH lcStyle ,;
            cType     WITH IIF(BOM.Typ = '1' , '6' , BOM.Typ) ,;
            Type      WITH BOM.cCatgTyp ,;
            cItem     WITH IIF(BOM.Typ = '1' , 'Assembly Labor' , BOM.Desc) ,;
            cItem1    WITH BOM.Item ,;
            Yield     WITH Yield + BOM.nbomtotqty,;
            nPrice    WITH nPrice + IIF(llFirst , Bom.Untcost, 0) ,;
            Status    WITH 'M' ,;
            nRecNo    WITH 0
  ENDSCAN    && End of SCAN Loop
ENDIF    && End of IF

*!*************************************************************
*! Name      : lfvMove
*! Developer : Haytham El_Sheltawi
*! Date      : 07/28/1997
*! Purpose   : Valid function for the Move Push Button
*!*************************************************************
*! Called from : Screen icDecVl
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvMove

*IF Select mode or View mode
IF laScrMode[1] .OR. laScrMode[2]
  RETURN
ENDIF    && End of IF

SELECT (lcTargtSty)

GO &lcSourcSty..nRecNo
RECALL
lcTarget = &lcTargtSty..cStyMajor

SELECT (lcSourcSty)
DELETE
SKIP
SHOW GET lcSource
SHOW GET lcTarget

lnSource = lnSource - 1
lnTarget = lnTarget + 1
lcMoveStat = IIF(lnSource > 0 , 'ENABLE' , 'DISABLE')
lcRMovStat = IIF(lnTarget > 0 , 'ENABLE' , 'DISABLE')
SHOW GET pbMove &lcMoveStat
SHOW GET pbMoveAll &lcMoveStat
SHOW GET pbRemove &lcRMovStat
SHOW GET pbRemAll &lcRMovStat
SHOW GET pbDtl &lcRMovStat

*!*************************************************************
*! Name      : lfvMoveAll
*! Developer : Haytham El_Sheltawi
*! Date      : 07/28/1997
*! Purpose   : Valid function for the Move All Push Button
*!*************************************************************
*! Called from : Screen icDecVl
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvMoveAll

*IF Select mode or View mode
IF laScrMode[1] .OR. laScrMode[2]
  RETURN
ENDIF    && End of IF

SELECT (lcSourcSty)

*SCAN Loop to scan the Temp. file lcSourcSty [The Source list file]
SCAN
  SELECT (lcTargtSty)
  GO &lcSourcSty..nRecNo
  RECALL
  
  SELECT (lcSourcSty)
  DELETE
ENDSCAN    && End of SCAN Loop
lcTarget = &lcTargtSty..cStyMajor

SHOW GET lcSource
SHOW GET lcTarget

lnTarget = lnTarget + lnSource
lnSource = 0
lcMoveStat = 'DISABLE'
lcRMovStat = IIF(lnTarget > 0 , 'ENABLE' , 'DISABLE')
SHOW GET pbMove &lcMoveStat
SHOW GET pbMoveAll &lcMoveStat
SHOW GET pbRemove &lcRMovStat
SHOW GET pbRemAll &lcRMovStat
SHOW GET pbDtl &lcRMovStat

*!*************************************************************
*! Name      : lfvRmov
*! Developer : Haytham El_Sheltawi
*! Date      : 07/28/1997
*! Purpose   : Valid function for the Remove Push Button
*!*************************************************************
*! Called from : Screen icDecVl
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvRmov

*IF Select mode or View mode or the All Styles record
IF laScrMode[1] .OR. laScrMode[2] .OR. &lcTargtSty..nRecNo = 0
  RETURN
ENDIF    && End of IF

SELECT (lcSourcSty)
GO &lcTargtSty..nRecNo
RECALL
lcSource = &lcSourcSty..cStyMajor

SELECT (lcTargtSty)
DELETE

SHOW GET lcSource
SHOW GET lcTarget

lnSource = lnSource + 1
lnTarget = lnTarget - 1
lcMoveStat = IIF(lnSource > 0 , 'ENABLE' , 'DISABLE')
lcRMovStat = IIF(lnTarget > 0 , 'ENABLE' , 'DISABLE')
SHOW GET pbMove &lcMoveStat
SHOW GET pbMoveAll &lcMoveStat
SHOW GET pbRemove &lcRMovStat
SHOW GET pbRemAll &lcRMovStat
SHOW GET pbDtl &lcRMovStat


*!*************************************************************
*! Name      : lfvRmovAll
*! Developer : Haytham El_Sheltawi
*! Date      : 07/28/1997
*! Purpose   : Valid function for the Remove All Push Button
*!*************************************************************
*! Called from : Screen icDecVl
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvRmovAll

*IF Select mode or View mode
IF laScrMode[1] .OR. laScrMode[2]
  RETURN
ENDIF    && End of IF

SELECT (lcTargtSty)


*SCAN Loop to scan the Temp. file lcTargtSty [The Target list file] for all
*the records except the All Styles record
SCAN FOR &lcTargtSty..nRecNo <> 0
  SELECT (lcSourcSty)
  GO &lcTargtSty..nRecNo
  RECALL
  
  SELECT (lcTargtSty)
  DELETE
ENDSCAN
lcSource = &lcSourcSty..cStyMajor

SHOW GET lcSource
SHOW GET lcTarget

lnSource = lnSource + lnTarget
lnTarget = 0
lcMoveStat = IIF(lnSource > 0 , 'ENABLE' , 'DISABLE')
lcRMovStat = 'DISABLE'
SHOW GET pbMove &lcMoveStat
SHOW GET pbMoveAll &lcMoveStat
SHOW GET pbRemove &lcRMovStat
SHOW GET pbRemAll &lcRMovStat
SHOW GET pbDtl &lcRMovStat

*!*************************************************************
*! Name      : lfvFirst
*! Developer : Haytham El_Sheltawi
*! Date      : 07/28/1997
*! Purpose   : Valid function for the First Push Button
*!*************************************************************
*! Called from : Screen icDecVl
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvFirst

GO TOP IN DECVALHD
laData[1] = DECVALHD.PO    && Varible to hold the PO number and create the PO get field
=lfGetHdInf()
=lfSelData()
=lpShow()

*!*************************************************************
*! Name      : lfvPrev
*! Developer : Haytham El_Sheltawi
*! Date      : 07/28/1997
*! Purpose   : Valid function for the Previous Push Button
*!*************************************************************
*! Called from : Screen icDecVl
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvPrev

SKIP -1 IN DECVALHD
laData[1] = DECVALHD.PO    && Varible to hold the PO number and create the PO get field
=lfGetHdInf()
=lfSelData()
=lpShow()

*!*************************************************************
*! Name      : lfvLast
*! Developer : Haytham El_Sheltawi
*! Date      : 07/28/1997
*! Purpose   : Valid function for the Last Push Button
*!*************************************************************
*! Called from : Screen icDecVl
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvLast

GO BOTTOM IN DECVALHD
laData[1] = DECVALHD.PO    && Varible to hold the PO number and create the PO get field
=lfGetHdInf()
=lfSelData()
=lpShow()

*!*************************************************************
*! Name      : lfvNext
*! Developer : Haytham El_Sheltawi
*! Date      : 07/28/1997
*! Purpose   : Valid function for the Next Push Button
*!*************************************************************
*! Called from : Screen icDecVl
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvNext

SKIP 1 IN DECVALHD
laData[1] = DECVALHD.PO    && Varible to hold the PO number and create the PO get field
=lfGetHdInf()
=lfSelData()
=lpShow()


*!*************************************************************
*! Name      : lfGetHdInf
*! Developer : Haytham El_Sheltawi
*! Date      : 07/28/1997
*! Purpose   : Function to get the Header information from the DECVALHD file
*!*************************************************************
*! Called from : lfvBrowPO() , lfvPO() , lfvClose() , lfvFirst ,
*!               lfvPrev() , lfvLast() , lfvNext()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfGetHdInf

laData[2] = DECVALHD.cManf         && Varible to hold the Manufacturar [Vendor]
laData[3] = DECVALHD.cCompany     && Varible to hold the Importer of Company name
laData[4] = DECVALHD.cAddress1     && Varible to hold the Importer of Addrees 1
laData[5] = DECVALHD.cAddress2     && Varible to hold the Importer of Addrees 2
laData[7] = DECVALHD.cAddress4     && Varible to hold the Importer of City
laData[6] = DECVALHD.cAddress3    && Varible to hold the Importer of State
laData[8] = DECVALHD.cAddress5       && Varible to hold the Importer of Zip code

*!*************************************************************
*! Name      : lfvDetail
*! Developer : Haytham El_Sheltawi
*! Date      : 07/28/1997
*! Purpose   : Valid function for the Detail Push Button
*!*************************************************************
*! Called from : Screen icDecVl
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvDetail

SELECT (lcTargtSty)

llAllStyle = IIF(nRecNo = 0 , .T. , .F.)    && Flag to know if the user want to get the detail for all selected Styles 

lcBrowFor = IIF(llAllStyle , 'FOR  !EOF(lcTargtSty)' ,;
                'FOR cStyMajor+cItem = &lcTargtSty..cStyMajor') +;
                ' .AND. Status <> "D"'    && Varible to Hold the Component Browse For condition

*IF the user want to get the detail for one Style
IF !llAllStyle
  
  *IF we are not at the begin of the lcTargtSty file [the Target List file]
  IF !BOF()
    SKIP -1
    lcDFrsStat = IIF(BOF() .OR. nRecNo = 0  , 'DISABLE' , 'ENABLE')    && Varible to hold the Status of the First and Previous Push Buttons in the Detail screen
    
    *IF we are at the begin of the lcTargtSty file [the Target List file]
    IF BOF()
      GO RECNO()
    ELSE    && Else
      SKIP 1
    ENDIF    && End of IF
  ELSE    && Else
    lcDFrsStat = 'DISABLE'    && Varible to hold the Status of the First and Previous Push Buttons in the Detail screen
  ENDIF    && End of IF
  
  *IF we are not at the end of the lcTargtSty file [the Target List file]
  IF !EOF()
    SKIP 1
    lcDLstStat = IIF(EOF() , 'DISABLE' , 'ENABLE')    && Varible to hold the Status of the Last and Next Push Buttons in the Detail screen
    SKIP -1
  ELSE    && Else
    lcDLstStat = 'DISABLE'    && Varible to hold the Status of the Last and Next Push Buttons in the Detail screen
  ENDIF    && End of IF
ELSE    && Else
  SKIP 1
  lcDFrsStat = 'DISABLE'    && Varible to hold the Status of the First and Previous Push Buttons in the Detail screen
  lcDLstStat = 'DISABLE'    && Varible to hold the Status of the Last and Next Push Buttons in the Detail screen
ENDIF    && End of IF

lcStylStat = IIF(laScrMode[2] .OR. EOF() , 'DISABLE' , 'ENABLE')    && Varible to hold the Status of the Style data Get fields in the Detail screen

SET ORDER TO TAG (lcTargtSty) IN (lcTargtSty)

SELECT (lcTmpLines)
SET ORDER TO TAG (lcTmpLines)
GO TOP

*IF the user want to get the detail for all selected Styles 
IF llAllStyle
  lcDTitle = 'P/O# : ' + laData[1] + '  Style : All Styles'    && Varible to hold the Detail screen title
  SET RELATION TO cStyMajor INTO (lcTargtSty)
  LOCATE FOR !EOF(lcTargtSty)
ELSE    && Else
 
  lcDTitle = 'P/O# : ' + laData[1] + '  Style : ' + &lcTargtSty..cStyMajor    && Varible to hold the Detail screen title
  
  SEEK &lcTargtSty..cStyMajor
ENDIF    && End of IF
lcDetlStat = IIF(laScrMode[2] .OR. EOF() .OR. cType = '7' , 'DISABLE' , 'ENABLE')    && Varible to hold the Status of the Item data Get fields in the Detail screen

SELECT (lcTargtSty)
SCATTER FIELDS EXCEPT cStyMajor , PO  MEMVAR

SELECT (lcTmpLines)
SCATTER MEMVAR

lcCatgDes = IIF(M.cType = '7' .OR. EMPTY(M.cType) , ' ' , laCatgTyp[IIF(M.cType = '6' , 1 , VAL(M.cType)) ,1])    && Varible to hold the Item Type description
lnCatgTyp = IIF(M.cType = '7' .OR. EMPTY(M.cType) , 0 , IIF(M.cType = '6' , 1 , VAL(M.cType)))    && Varible to create the Item Type Popup

PUSH KEY
ON KEY LABEL ALT+I ACTIVATE WINDOW (lcBrowTitl)
ON KEY LABEL TAB
ON KEY LABEL ESC CLEAR READ
ON KEY LABEL BACKTAB
DO (gcScrDir+gcWinAppl+"\ICDECD.SPX")
ON KEY LABEL ALT+I 
POP KEY

*IF the user has select to get the detail for all selected Styles 
IF llAllStyle
  SELECT (lcTmpLines)
  SET RELATION TO
  GO TOP IN (lcTargtSty)
ELSE    && Else
  lcTarget = &lcTargtSty..cStyMajor
ENDIF    && End of IF

SHOW GET lcTarget
SHOW GET lcSource

*!*************************************************************
*! Name      : lfBrowTrap
*! Developer : Haytham El_Sheltawi
*! Date      : 07/28/1997
*! Purpose   : Function to trap the keys for the Browse
*!*************************************************************
*! Called from : Scrren icDecD Deactivate Function
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfBrowTrap

*IF The window on top is the Browse
IF WONTOP(lcBrowTitl)
  llFromBrow = .T.    && Flag to hold .T. if we are coming from the Browse
  ON KEY LABEL TAB DO lfTraps WITH 1
  ON KEY LABEL BACKTAB DO lfTraps WITH 2
  ON KEY LABEL ALT+I
  lcEscTrap = ON('KEY' , 'ESC')                && Varible to hold the ESC key traping
  lcCtrETrap = ON('KEY' , 'CTRL+END')          && Varible to hold the CTRL+END key traping
  lcCtrHTrap = ON('KEY' , 'CTRL+HOME')         && Varible to hold the CTRL+HOME key traping
  lcCtrNTrap = ON('KEY' , 'CTRL+ENTER')        && Varible to hold the CTRL+ENTER key traping
  ON KEY LABEL ESC DO lfTraps WITH 3
  ON KEY LABEL CTRL+END DO lfTraps WITH 4
  ON KEY LABEL CTRL+HOME DO lfTraps WITH 5
  ON KEY LABEL CTRL+ENTER DO lfTraps WITH 6
ENDIF    && End of IF

RETURN .F.

*!*************************************************************
*! Name      : lfBrwUnTrp
*! Developer : Haytham El_Sheltawi
*! Date      : 07/28/1997
*! Purpose   : Function to untrap the keys for the Browse
*!*************************************************************
*! Called from : Scrren icDecD Activate Function
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfBrwUnTrp

*IF The window on top is not the Browse and coming from the Browse
IF !WONTOP(lcBrowTitl) .AND. llFromBrow
  llFromBrow = .F.    && Flag to hold .T. if we are coming from the Browse
  ON KEY LABEL TAB
  ON KEY LABEL BACKTAB
  ON KEY LABEL ALT+I ACTIVATE WINDOW (lcBrowTitl)
  ON KEY LABEL ESC &lcEscTrap
  ON KEY LABEL CTRL+END &lcCtrETrap
  ON KEY LABEL CTRL+HOME &lcCtrHTrap
  ON KEY LABEL CTRL+ENTER &lcCtrNTrap
  
ENDIF    && End of IF

*!*************************************************************
*! Name      : lfTraps
*! Developer : Haytham El_Sheltawi
*! Date      : 07/28/1997
*! Purpose   : Function to be called from some traped keys
*!*************************************************************
*! Called from : lfBrowTrap()
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : 1 , 2
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfTraps

PARAMETERS lnTrap

*DO CASE Statment
DO CASE 
  
  *Case TAB
  CASE lnTrap = 1
    ACTI WINDOW (lcRobCh3) TOP
    
    *DO CASE Statment
    DO CASE
      
      *Case Edit mode or Add mode
      CASE laScrMode[3] .OR. laScrMode[4]
        _CUROBJ = OBJNUM(M.cItem)
      
      *Case the First Push Button is Enable
      CASE lcDFrsStat = 'ENABLE'
        _CUROBJ = OBJNUM(pbTop)
      
      *Case the Next Push Button is Enable
      CASE lcDLstStat = 'ENABLE'
        _CUROBJ = OBJNUM(pbNxt)
      
      *Otherwise
      OTHERWISE
        _CUROBJ = OBJNUM(pbCls)
    ENDCASE    && End of DO CASE Statment

  *Case BACKTAB
  CASE lnTrap = 2
    
    *IF Edit mode or Add mode
    IF laScrMode[3] .OR. laScrMode[4]
      ACTI WINDOW (lcRobCh1) TOP
      _CUROBJ = OBJNUM(M.Pattern)
    ELSE    && Else
      ACTI WINDOW (lcRobCh3) TOP
      _CUROBJ = OBJNUM(pbCls)
    ENDIF    && End of IF
    
  *Case ESC
  CASE lnTrap = 3
    RETURN
    
  *Case CTRL+END
  CASE lnTrap = 4
    GO BOTTOM
  
  *Case CTRL+HOME
  CASE lnTrap = 5
    GO TOP
  
  *Case CTRL+ENTER
  CASE lnTrap = 6
    RETURN

ENDCASE    && End of DO CASE Statment

*!*************************************************************
*! Name      : lfDispBrow
*! Developer : Haytham El_Sheltawi
*! Date      : 07/28/1997
*! Purpose   : Function to display the Component Browse
*!*************************************************************
*! Called from : Screen icDecD setup
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfDispBrow

SELECT (lcTmpLines)

lnBrRecNo  = RECNO(lcTmpLines)            && Varible to hold the browse record number

BROWSE FIELDS &lcBrowStr &lcBrowFor;
       WINDOW (lcRobCh2) ;
       WHEN lfwBrows() ;
       IN WINDOW (lcParent) ;
       LOCK 0;
       NOAPPEND;
       NOCLEAR;
       NODELETE;
       NOWAIT;
       NOEDIT;
       NOMENU;
       SAVE;
       TITLE lcBrowTitl

*!*************************************************************
*! Name      : lfwIbBrow
*! Developer : Haytham El_Sheltawi
*! Date      : 07/02/1997
*! Purpose   : When function for the Invisible Button IbBrow
*!*************************************************************
*! Called from : Screen icDecVl1
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfwIbBrow

*IF the last key was Tab or Backtab
IF LASTKEY() = 9 .OR. LASTKEY() = 15
  KEYBOARD '{ALT+I}'
ELSE    && Else
 RETURN .F.  
ENDIF    && End of IF


*!*************************************************************
*! Name      : lfwBrows
*! Developer : Haytham El_Sheltawi
*! Date      : 07/02/1997
*! Purpose   : WHEN Function for the Component Browse
*!*************************************************************
*! Called from : The Component Browse
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfwBrows

*IF the user has select to get the detail for all selected Styles 
IF llAllStyle
  SELECT (lcTargtSty)
  SCATTER FIELDS EXCEPT cStyMajor , PO  MEMVAR
  SHOW GETS WINDOW (lcRobCh1)
ENDIF    && End of IF

SELECT (lcTmpLines)
lnBrRecNo  = RECNO()            && Varible to hold the browse record number
SCATTER MEMVAR
lcCatgDes = IIF(M.cType = '7' .OR. EMPTY(M.cType) , ' ' , laCatgTyp[IIF(M.cType = '6' , 1 , VAL(M.cType)) ,1])
lnCatgTyp = IIF(M.cType = '7' .OR. EMPTY(M.cType) , 0 , IIF(M.cType = '6' , 1 , VAL(M.cType)))

*IF Select mode or View mode or we are ate the end of lcTmpLines file
*[The Temp. Component file] or we are at one of the total records

IF laScrMode[1] .OR. laScrMode[2] .OR. EOF() .OR. cType = '7'
  SHOW GET M.cStyMajor 
  SHOW GET lcCatgDes
  SHOW GET lnCatgTyp
  SHOW GET M.cItem DISABLE
  SHOW GET M.Yield DISABLE
  SHOW GET M.nPrice DISABLE
  SHOW GET M.nUS806807 DISABLE
  SHOW GET M.Dutiable DISABLE
  SHOW GET M.n800 DISABLE
  SHOW GET pbRemLine DISABLE
ELSE    && Else
  SHOW GET M.cStyMajor 
  SHOW GET lcCatgDes
  SHOW GET lnCatgTyp
  SHOW GET M.cItem ENABLE
  SHOW GET M.Yield ENABLE
  SHOW GET M.nPrice ENABLE
  SHOW GET M.nUS806807 ENABLE
  SHOW GET M.Dutiable ENABLE
  SHOW GET M.n800 ENABLE
  SHOW GET pbRemLine ENABLE
ENDIF    && End of IF

SHOW WINDOW (lcBrowTitl) REFRESH

*!*************************************************************
*! Name      : lfvDFirst
*! Developer : Haytham El_Sheltawi
*! Date      : 07/28/1997
*! Purpose   : Valid function for the First Push Button in the Detail Screen
*!*************************************************************
*! Called from : Screen icDecVl3
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvDFirst

SELECT (lcTargtSty)
GO TOP
SKIP 1
SCATTER FIELDS EXCEPT cStyMajor , PO  MEMVAR
=SEEK(cStyMajor , lcTmpLines)
lnBrRecNo  = RECNO(lcTmpLines)
SHOW WINDOW (lcBrowTitl) REFRESH
=lpDShow()

*IF Windows
IF _WINDOWS
  lcDTitle = 'P/O# : ' + laData[1] + '  Style : ' + &lcTargtSty..cStyMajor
  MODIFY WINDOW (lcParent) TITLE lcDTitle
ENDIF    && End of IF

*!*************************************************************
*! Name      : lfvDPrev
*! Developer : Haytham El_Sheltawi
*! Date      : 07/28/1997
*! Purpose   : Valid function for the Previous Push Button in the Detail Screen
*!*************************************************************
*! Called from : Screen icDecVl3
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvDPrev

SELECT (lcTargtSty)
SKIP -1
SCATTER FIELDS EXCEPT cStyMajor , PO  MEMVAR
=SEEK(cStyMajor , lcTmpLines)
lnBrRecNo  = RECNO(lcTmpLines)
SHOW WINDOW (lcBrowTitl) REFRESH
=lpDShow()

*IF Windows
IF _WINDOWS
  lcDTitle = 'P/O# : ' + laData[1] + '  Style : ' + &lcTargtSty..cStyMajor
  MODIFY WINDOW (lcParent) TITLE lcDTitle
ENDIF    && End of IF

*!*************************************************************
*! Name      : lfvDLast
*! Developer : Haytham El_Sheltawi
*! Date      : 07/28/1997
*! Purpose   : Valid function for the Last Push Button in the Detail Screen
*!*************************************************************
*! Called from : Screen icDecVl3
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvDLast

SELECT (lcTargtSty)
GO BOTTOM
SCATTER FIELDS EXCEPT cStyMajor , PO  MEMVAR
=SEEK(cStyMajor , lcTmpLines)
lnBrRecNo  = RECNO(lcTmpLines)
SHOW WINDOW (lcBrowTitl) REFRESH
=lpDShow()

*IF Windows
IF _WINDOWS
  lcDTitle = 'P/O# : ' + laData[1] + '  Style : ' + &lcTargtSty..cStyMajor
  MODIFY WINDOW (lcParent) TITLE lcDTitle
ENDIF    && End of IF

*!*************************************************************
*! Name      : lfvDNext
*! Developer : Haytham El_Sheltawi
*! Date      : 07/28/1997
*! Purpose   : Valid function for the Next Push Button in the Detail Screen
*!*************************************************************
*! Called from : Screen icDecVl3
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvDNext

SELECT (lcTargtSty)
SKIP 1
SCATTER FIELDS EXCEPT cStyMajor , PO  MEMVAR
=SEEK(cStyMajor , lcTmpLines)
lnBrRecNo  = RECNO(lcTmpLines)
SHOW WINDOW (lcBrowTitl) REFRESH
=lpDShow()

*IF Windows
IF _WINDOWS
  lcDTitle = 'P/O# : ' + laData[1] + '  cStyMajor : ' + &lcTargtSty..cStyMajor
  MODIFY WINDOW (lcParent) TITLE lcDTitle
ENDIF    && End of IF

*!*************************************************************
*! Name      : lpDShow
*! Developer : Haytham El_Sheltawi
*! Date      : 07/28/1997
*! Purpose   : Show procedure for icDecD screen [Declaration of value Detail screen]
*!*************************************************************
*! Called from : lfvDFirst() , lfvDPrev() , lfvDLast() , lfvDNext ,
*!               lfvRmvLine() , lfvAddLine() , lfvStyle()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
PROCEDURE lpDShow

PRIVATE lcAlias

lcAlias = ALIAS()    && Varible to save the alias

*IF the user has select to get the detail for one Style
IF !llAllStyle
  SELECT (lcTargtSty)
  
  *IF we are not at the begin of the lcTargtSty file [the Target List file]
  IF !BOF()
    SKIP -1
    lcDFrsStat = IIF(BOF() .OR. nRecNo = 0 , 'DISABLE' , 'ENABLE')
    
    *IF we are at the begin of the lcTargtSty file [the Target List file]
    IF BOF()
      GO RECNO()
    ELSE    && Else
      SKIP 1
    ENDIF    && End of IF
  ELSE    && Else
    lcDFrsStat = 'DISABLE'
  ENDIF    && End of IF
  
  *IF we are not at the end of the lcTargtSty file [the Target List file]
  IF !EOF()
    SKIP 1
    lcDLstStat = IIF(EOF() , 'DISABLE' , 'ENABLE')
    SKIP -1
  ELSE    && Else
    lcDLstStat = 'DISABLE'
  ENDIF    && End of IF
ENDIF    && End of IF

SELECT (lcTmpLines)
SCATTER MEMVAR
lcCatgDes = IIF(M.cType = '7' .OR. EMPTY(M.cType) , ' ' , laCatgTyp[IIF(M.cType = '6' , 1 , VAL(M.cType)) ,1])
lnCatgTyp = IIF(M.cType = '7' .OR. EMPTY(M.cType) , 0 , IIF(M.cType = '6' , 1 , VAL(M.cType)))

*IF Select mode or View mode
IF laScrMode[1] .OR. laScrMode[2]
  SHOW GETS WINDOW (lcRobCh1) DISABLE
ELSE    && Else
  SHOW GETS WINDOW (lcRobCh1) ENABLE
ENDIF    && End of IF

*IF Select mode or View mode or we are at the end of the lcTmpLines [the
* Temp. Component file] or at one of the total records

IF laScrMode[1] .OR. laScrMode[2] .OR. EOF() .OR. cType = '7'
  SHOW GET M.cStyMajor 
  SHOW GET lcCatgDes
  SHOW GET lnCatgTyp
  SHOW GET M.cItem DISABLE
  SHOW GET M.Yield DISABLE
  SHOW GET M.nPrice DISABLE
  SHOW GET M.nUS806807 DISABLE
  SHOW GET M.Dutiable DISABLE
  SHOW GET M.n800 DISABLE
  SHOW GET pbRemLine DISABLE
ELSE    && Else
  SHOW GET M.cStyMajor 
  SHOW GET lcCatgDes
  SHOW GET lnCatgTyp
  SHOW GET M.cItem ENABLE
  SHOW GET M.Yield ENABLE
  SHOW GET M.nPrice ENABLE
  SHOW GET M.nUS806807 ENABLE
  SHOW GET M.Dutiable ENABLE
  SHOW GET M.n800 ENABLE
  SHOW GET pbRemLine ENABLE
ENDIF    && End of IF

SHOW GET pbTop &lcDFrsStat
SHOW GET pbPrvs &lcDFrsStat
SHOW GET pbBtm &lcDLstStat
SHOW GET pbNxt &lcDLstStat

*IF The varible we used to save the old alias is not empty
IF !EMPTY(lcAlias)
  SELECT (lcAlias)
ENDIF    && End of IF

*!*************************************************************
*! Name      : lfvFields
*! Developer : Haytham El_Sheltawi
*! Date      : 07/28/1997
*! Purpose   : Valid function for all the Get Fields in the Detail Screen
*!*************************************************************
*! Called from : Screen icDecVl1 , icDecVl3
*!*************************************************************
*! Passed Parameters : 1)The Temp. file name
*!                     2)The Field name
*!                     3) .T. if the field can not be negative
*!                        .F. Otherwise
*!                     4) .T. if the field has a total record
*!                        .F. Otherwise
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvFields

PARAMETERS lcFile , lcField , llPosetive , llTotal

PRIVATE lcAlias , lcVarName , laVarVal , lnRecNo , lnOldVal

lcAlias = ALIAS()    && Varible to save the alias

lcVarName = SYS(18)    && Varible to hold the name of the Varible that created the current get field
laVarVal = M.&lcVarName    && Varible to hold the value of the current get field

*IF the field can not be negative and the get field value is negative
IF llPosetive .AND. laVarVal < 0
  &lcVarName = laOldVal    && Restore the old value
  _CUROBJ = OBJNUM(&lcVarName)
  RETURN
ENDIF    && End of IF

SELECT (lcFile)

*IF the field has a total record
IF llTotal
  lnRecNo = RECNO()    && Varible to save the current record number
  lnOldVal = &lcField    && Varible to hold the the old value of the Field [from the file]
  SEEK M.cStyMajor + '7'
  REPLACE &lcField WITH &lcField - lnOldVal + laVarVal
  GO lnRecNo
ENDIF    && End of IF

REPLACE &lcField WITH laVarVal

*IF the file is the Temp. Component file
IF lcFile  = lcTmpLines
  REPLACE Status WITH 'M'
  SHOW WINDOW (lcBrowTitl) REFRESH SAME
ENDIF    && End of IF

*IF The varible we used to save the old alias is not empty
IF !EMPTY(lcAlias)
  SELECT (lcAlias)
ENDIF    && End of IF

*!*************************************************************
*! Name      : lfvRmvLine
*! Developer : Haytham El_Sheltawi
*! Date      : 07/28/1997
*! Purpose   : Valid function for the Remove Push Button in the Detail screen
*!*************************************************************
*! Called from : Screen icDecVl3
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvRmvLine

*IF Statment to cofirm the Remove action
*IF gfDialog('!','Are you sure you want to remove this component.' , '\<Yes;\<No') = 2
IF gfModalGen('QRM42214B42002','DIALOG')  = 2  
  RETURN
ENDIF    && End of IF

PRIVATE lcAlias , lnRecNo
lcAlias = ALIAS()    && Varible to save the alias

SELECT (lcTmpLines)

lnRecNo = RECNO()    && Varible to Save record number
SEEK M.cStyMajor + '7'
REPLACE nUS806807 WITH nUS806807 - M.nUS806807 ,;
        Dutiable  WITH Dutiable - M.Dutiable ,;
        n800      WITH n800 - M.n800

GO lnRecNo

*IF this Item line exist in the DECVALCM file
IF nRecNo <> 0
  REPLACE Status WITH 'D'
ELSE    && Else
  REPLACE Status WITH 'S'
ENDIF    && End of IF
DELETE

*IF the user has select to get the detail for all selected Styles 
IF llAllStyle
  LOCATE REST FOR !EOF(lcTargtSty) .AND. !DELETED()
ELSE    && Else
  LOCATE REST FOR cStyMajor = &lcTargtSty..cStyMajor .AND. !DELETED()
ENDIF    && End of IF

*IF we are at the end of lcTmpLines file [the Temp. Component file]
IF EOF()
  *IF the user has select to get the detail for all selected Styles 
  IF llAllStyle
    LOCATE FOR !EOF(lcTargtSty) .AND. !DELETED()
  ELSE    && Else
    SEEK &lcTargtSty..cStyMajor
  ENDIF    && End of IF
ENDIF    && End of IF

lnBrRecNo = RECNO()
SHOW WINDOW (lcBrowTitl) REFRESH
=lpDShow()

*IF The varible we used to save the old alias is not empty
IF !EMPTY(lcAlias)
  SELECT (lcAlias)
ENDIF    && End of IF

*!*************************************************************
*! Name      : lfvAddLine
*! Developer : Haytham El_Sheltawi
*! Date      : 07/28/1997
*! Purpose   : Valid function for the Add Push Button in the Detail screen
*!*************************************************************
*! Called from : Screen icDecVl3
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvAddLine

SELECT (lcTmpLines)
SCATTER MEMVAR BLANK

lnCatgTyp = 0
SHOW GET lnCatgTyp

SHOW GET M.cItem
SHOW GET M.Yield
SHOW GET M.nPrice
SHOW GET M.nUS806807
SHOW GET M.Dutiable
SHOW GET M.n800

*IF the user has select to get the detail for all selected Styles 
IF llAllStyle
  SHOW GET M.cStyMajor ENABLE
  _CUROBJ = OBJNUM(M.cStyMajor)
ELSE    && Else
  APPEND BLANK
  M.cStyMajor = &lcTargtSty..cStyMajor
  REPLACE cStyMajor    WITH M.cStyMajor ,;
          Status   WITH 'M' ,;
          nRecNo   WITH 0
  
  lnBrRecNo = RECNO()
  SHOW WINDOW (lcBrowTitl) REFRESH
  =lpDShow()
  SELECT (lcTargtSty)
  SCATTER FIELDS EXCEPT cStyMajor , PO  MEMVAR
  SHOW GETS WINDOW (lcRobCh1)
  SHOW GET M.cStyMajor
  SELECT (lcTmpLines)
  
  SHOW GET lnCatgTyp ENABLE
  _CUROBJ = OBJNUM(lnCatgTyp)
ENDIF    && End of IF

RETURN

*!*************************************************************
*! Name      : lfvStyle
*! Developer : Haytham El_Sheltawi
*! Date      : 07/28/1997
*! Purpose   : Valid function for the Style Get Field in the Detail screen
*!*************************************************************
*! Called from : Screen icDecVl3
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvStyle

*IF the Style is not empty and the user want to browse or the Style
*is not one of the selected Styles
IF !EMPTY(M.cStyMajor) .AND. ('?' $ M.cStyMajor .OR. !SEEK(M.cStyMajor , lcTargtSty))
  SELECT (lcTargtSty)
  lcBrFields  = "cStyMajor         :H= 'Style' , "+;
                "Date    :H= 'Date' , "+;
                "Desc    :H= 'Style description' , "+;
                "Pattern :H= 'Pattern' , "+;
                "FibreCont :H= 'Fibre content'"
  
  lcBrowTtl = 'Declaration of value styles'    && Varible to hold the title for the selected Styles Browse
  DECLARE laReturn[1] 
  STORE '' TO laReturn
  =AriaBrow('FOR nRecNo <> 0', lcBrowTtl, gnBrHSRow1, gnBrHSCol1,;
            gnBrHSRow2, gnBrHSCol2,'','','cStyMajor','laReturn')
  
  M.cStyMajor = laReturn    && Get the user selection
ENDIF    && End of IF

SELECT (lcTmpLines)

*IF the Style was empty
IF EMPTY(M.cStyMajor)
  SCATTER MEMVAR
  SHOW GET M.cItem
  SHOW GET M.Yield
  SHOW GET M.nPrice
  SHOW GET M.nUS806807
  SHOW GET M.Dutiable
  SHOW GET M.n800
  SHOW GET M.cStyMajor DISABLE
  
  lnCatgTyp = IIF(M.cType = '7' .OR. EMPTY(M.cType) , 0 , IIF(M.cType = '6' , 1 , VAL(M.cType)))
  SHOW GET lnCatgTyp

ELSE    && Else
  APPEND BLANK
  REPLACE cStyMajor WITH M.cStyMajor ,;
          Status WITH 'M' ,;
          nRecNo WITH 0
  
  lnBrRecNo = RECNO()
  SELECT (lcTargtSty)
  SCATTER FIELDS EXCEPT cStyMajor  , PO  MEMVAR
  SHOW GETS WINDOW (lcRobCh1)
  SHOW WINDOW (lcBrowTitl) REFRESH
  =lpDShow()
  
  SHOW GET lnCatgTyp ENABLE
  _CUROBJ = OBJNUM(lnCatgTyp)
ENDIF    && End of IF
SHOW GET M.cStyMajor DISABLE

*!*************************************************************
*! Name      : lpSavscr
*! Developer : Haytham El_Sheltawi
*! Date      : 07/28/1997
*! Purpose   : Valid function for the Save Push Button in the Hedaer screen
*!             [The Save function for the screen]
*!*************************************************************
*! Called from : Screen icDecVl
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
PROCEDURE lpSavscr
IF EMPTY(laData[1])
  RETURN
ENDIF
M.PO = laData[1]    && Varible to hold the PO number

SELECT (lcTmpLines)
RECALL ALL

SET ORDER TO TAG DECVALHD IN DECVALHD
SET ORDER TO TAG DECVALST IN DECVALST
SET ORDER TO TAG DECVALCM IN DECVALCM
SET ORDER TO TAG (lcTmpLines) IN (lcTmpLines)

SELECT (lcSourcSty)

*SCAN Loop to scan the lcSourcSty file [the Source List file] for the
*Styles that was selected before and the user Removed it
SCAN FOR lSelBefor
  
  SELECT DECVALCM
  *IF the PO Style exist in the DECVALCM file
  IF SEEK(laData[1] + &lcSourcSty..cStyMajor )
    *SCAN Loop to scan the DECVALCM file for This PO Style
    SCAN REST WHILE PO + cStyMajor  = laData[1] + &lcSourcSty..cStyMajor 
      DELETE
    ENDSCAN    && End of SCAN Loop
  ENDIF    && End of IF
  
  SELECT DECVALST
  *IF the PO Style exist in the DECVALST file
  IF SEEK(laData[1] + &lcSourcSty..cStyMajor )
    DELETE
  ENDIF    && End of IF
  SELECT (lcSourcSty)
ENDSCAN    && End of SCAN Loop

SELECT (lcTargtSty)

*SCAN Loop to scan the lcTargtSty file [the Target List file] for
*all records except the All Styles record
SCAN FOR nRecNo <> 0
  SCATTER FIELDS EXCEPT PO MEMVAR
  SELECT DECVALST
  
  *IF the Style was selected befor
  IF &lcTargtSty..lSelBefor
    *IF the PO Style exist in the DECVALST
    IF SEEK(laData[1] + &lcTargtSty..cStyMajor )
      GATHER MEMVAR
    ENDIF    && End of IF
  ELSE    && Else
    APPEND BLANK
    GATHER MEMVAR
  ENDIF    && End of IF
  
  SELECT (lcTmpLines)
  *IF there is one or more record for this style in the lcTmpLines file
  *[the Temp. Component file]
  IF SEEK(&lcTargtSty..cStyMajor )
    *SCAN Loop to scan the lcTmpLines file [the Temp. Component file]
    *For this Style and Status Other than 'S' [Same] and not for the total
    *Records
    SCAN REST ;
         WHILE cStyMajor  = &lcTargtSty..cStyMajor  ;
          FOR Status <> 'S' .AND. cType <> '7'
      
      SCATTER FIELDS EXCEPT PO MEMVAR
      M.cType = IIF(M.cType = '6' , '1' , M.cType)    && Varible to hold the fixed Type
      SELECT DECVALCM
      
      *IF This is a new line which dose not exist in the DECVALCM file
      IF &lcTmpLines..nRecNo = 0
        APPEND BLANK
        GATHER MEMVAR
      ELSE    && Else
        GO &lcTmpLines..nRecNo
        
        *IF The line was Removed
        IF &lcTmpLines..Status = 'D'
          DELETE
        ELSE    && Else
          GATHER MEMVAR
        ENDIF    && End of IF
      ENDIF    && End of IF
      SELECT (lcTmpLines)
    ENDSCAN
  ENDIF    && End of IF
  SELECT (lcTargtSty)
ENDSCAN

SELECT DECVALHD
*IF Add mode
IF laScrMode[4]
  APPEND BLANK
  REPLACE PO        WITH laData[1] ,;
          cManf    WITH laData[2] ,;
          cCompany WITH laData[3] ,;
          cAddress1 WITH laData[4] ,;
          cAddress2 WITH laData[5] ,;
          cAddress3 WITH laData[6] ,;
          cAddress4 WITH laData[7] ,;
          cAddress5  WITH laData[8]
  
ELSE    && Else
  SEEK laData[1]
  REPLACE cManf    WITH laData[2] ,;
          cCompany WITH laData[3] ,;
          cAddress1 WITH laData[4] ,;
          cAddress2 WITH laData[5] ,;
          cAddress3 WITH laData[6] ,;
          cAddress4 WITH laData[7] ,;
          cAddress5  WITH laData[8]
  
ENDIF    && End of IF

=lfSelData()
laScrMode = .F.
laScrMode[2] = .T.


*!*************************************************************
*! Name      : lpDelScr
*! Developer : Haytham El_Sheltawi
*! Date      : 07/28/1997
*! Purpose   : Valid function for the Delete Push Button in the Header screen
*!             [The Delete function for the screen]
*!*************************************************************
*! Called from : Screen icDecVl
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lpDelScr


SET ORDER TO TAG DECVALHD IN DECVALHD
SET ORDER TO TAG DECVALST IN DECVALST
SET ORDER TO TAG DECVALCM IN DECVALCM

SELECT DECVALCM
*IF There is one or more record for this PO number in the DECVALCM file
IF SEEK(laData[1])
  DELETE REST WHILE PO = laData[1]
ENDIF    && End of IF

SELECT DECVALST
*IF There is one or more record for this PO number in the DECVALST file
IF SEEK(laData[1])
  DELETE REST WHILE PO = laData[1]
ENDIF    && End of IF

SELECT DECVALHD
*IF There is record for this PO number in the DECVALHD file
IF SEEK(laData[1])
  DELETE
ENDIF    && End of IF

laScrMode = .F.
laScrMode[1] = .T.


*!*************************************************************
*! Name      : lfvCatgTyp
*! Developer : Haytham El_Sheltawi
*! Date      : 08/05/1997
*! Purpose   : Valid function for the Type Popup in the Detail screen
*!*************************************************************
*! Called from : Screen icDecVl3
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvCatgTyp


REPLACE cType   WITH IIF(lnCatgTyp = 1 , '6' , STR(lnCatgTyp , 1)) ,;
        Type    WITH laCatgTyp[lnCatgTyp , 2]

SHOW GET lnCatgTyp DISABLE
SHOW WINDOW (lcBrowTitl) REFRESH SAME
ON KEY LABEL ESC CLEAR READ

*!*************************************************************
*! Name      : lfwCatgTyp
*! Developer : Haytham El_Sheltawi
*! Date      : 08/05/1997
*! Purpose   : When function for the Type Popup in the Detail screen
*!*************************************************************
*! Called from : Screen icDecVl3
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfwCatgTyp

ON KEY LABEL ESC llDum = .T.
KEYBOARD '{SPACEBAR}' CLEAR

