*:----------------------------------------------------------------------------------------------
*: Program file        : ICSPGMA2.PRG
*: Program description : INVENTORY BY STYLE AVAILABILITY REPORT DATA PREPERATION
*: For System          : Aria Advantage Series - Version 2.7
*: For Module          : System Manager - (IC)
*: Developer Name      : TMI TAREK MOHAMED IBRAHIM
*: Tracking Job Number : C102223,1
*: NOTE                : The generated EXE MUST named "w:\aria27\reports\ic\ICSPCLCT.EXE" and must be located 
*:                     : at w:\aria27\reports\ic
*:----------------------------------------------------------------------------------------------
*:MODIFICATIONS
*!*C102223,4 TMI 07/04/01 Data collecting for the report
*!*B605188,1 TMI 12/03/2001 Fix a bug when the apostrof "'" is included in data eg.( JAK'S WARE )
*:----------------------------------------------------------------------------------------------

LPARAMETERS lcMemoFile
PRIVATE lnErrFlag
lnErrFlag = .F.

IF TYPE('lcMemoFile')#'C'
  RETURN
ENDIF
RESTORE FROM (lcMemoFile) ADDI

*--ltDTRun -->Hold the date time the report run
lcDTRun = DTOS(DATE())+' '+TIME()

HIDE MENU _MSYSMENU
*C102224,1 TMI [Start] Let Messages appear in the form frmMsg.scx
*!*	if .t.
*!*	  DO FORM w:\aria27\screens\ic\FRMMSG.SCX
*!*	  _SCREEN.VISIBLE = .t.
*!*	  _SCREEN.WINDOWSTATE = 2 && Minimize fox window
*!*	  _SCREEN.LEFT = 0    && To prevent the VFP flashing when closing the screen
*!*	else
*--Hide VFP screen
_SCREEN.VISIBLE = .F.
_SCREEN.WINDOWSTATE = 1 && Minimize fox window
_SCREEN.LEFT = -1000    && To prevent the VFP flashing when closing the screen
*--Run the form that used to display messages during collecting data.
DO FORM D:\ARIA27\SCREENS\IC\FRMMSG.SCX
*!*	endif
FRMMSG.SHOW
*--Change the waiting message
*frmmsg.lblMsg.CAPTION = 'Wait while connecting to SQL server and getting collected data.'
frmmsg.lblMsg.CAPTION = 'Please wait while collecting data.'

*C102224,1 TMI [End  ]

*--Settings
=lfSettings()

*-- Open needed tables
=lfOpenFiles()

*-- Set relations
=lfSetRelations()

*C102223,4 TMI [Start]  No need to retrive the server name from the registry because I added 
*C102223,4              an entry to the IC setup Named "SQL Server name"
*!*	LOCAL lcSrvr, lcOldClassLib
*!*	lcSrvr = ''
*!*	lcOldClassLib = SET('CLASSLIB')

*!*	SET CLASSLIB TO (lcRpDefPth+'classes\cathy.vcx')
*!*	oReg = CREATEOBJECT("Registry")
*!*	lnError = oReg.GetRegKey("SQLServer",@lcSrvr,"Aria Connections\AriaServer",0)
*!*	lcConnStr = 'DRIVER={SQL Server};Server='+lcSrvr+';Persist Security Info=True;'+;
*!*	            'Initial Catalog=DB'+lcRpComp+';uid=aria;pwd=ariaadmn;database=DB'+lcRpComp

lcConnStr = 'DRIVER={SQL Server};Server='+lcSQLSvr+';Persist Security Info=True;'+;
            'Initial Catalog=Master;uid=aria;pwd=ariaadmn' 
*--Disable ODBC Error messages
=SQLSetProp(0,'DispLogin',3)
*C102223,4 TMI [End  ]            

lnConnectionHandle = SQLSTRINGCONNECT(lcConnStr)
IF lnConnectionHandle > 0
  lnConnHandler = SQLSETPROP(lnConnectionHandle , "Asynchronous",.F.)
  IF lnConnHandler > 0
    *-- Check if 'SLSPRDDATA' is not in the database Create it    
    IF lfCreateTable()
      *-- lnRowCount : Number of rows sent to SQL Server.
      lnRowCount = 0
      *-- Start Collecting data , and adding it to the table 'SLSPRDDATA'
      =lfCollectData()
    ELSE
      lnErrFlag = .T.
    ENDIF         
  ELSE  
    lnErrFlag = .T.
  ENDIF
ELSE
  lnErrFlag = .T.
ENDIF

IF lnErrFlag
  =Server_Error()
  RELEASE WINDOW PRTWIND
  SHOW MENU _MSYSMENU
  RETURN      
ENDIF

*!* SET CLASSLIB TO &lcOldClassLib.

*-- Diconnect existing connection
IF lnConnectionHandle > 0
  SQLDISCONNECT(lnConnectionHandle)
ENDIF

*--Close all opened tables.
=lfClose()

*-- Finishing message
IF lnRowCount > 0 
  *C102224,1 TMI [Start] Show messages useing the always on top form frmmsg.scx
  *=MESSAGEBOX('Collecting and sending data to SQL Server Completed successfully.',0,'Aria Advantage Series')
  frmmsg.lblMsg.CAPTION = 'Collecting and sending data to SQL Server Completed successfully.'
  *C102224,1 TMI [End  ]     
ELSE
  *C102224,1 TMI [Start] Show messages useing the always on top form frmmsg.scx
  *=MESSAGEBOX('No Data collected.',0,'Aria Advantage Series')
  frmmsg.lblMsg.CAPTION = 'No Data collected.'
  *C102224,1 TMI [End  ]
ENDIF  

*C102224,1 TMI [Start] Enable OK button
*FRMMSG.cmdOk.ENABLED = .T.
*READ EVENTS
*C102224,1 TMI [End  ]

*--Delete memo file
ERASE (lcMemoFile)

RELEASE WINDOW PRTWIND
SHOW MENU _MSYSMENU

*:**************************************************************************
*:* Name        : Server_Error
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/18/01
*:* Purpose     : sql Server Error handler
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = Server_Error()
*:***************************************************************************
FUNCTION Server_Error
LOCAL laServerErrors , lcErrMsg
DECLARE laServerErrors[1]
AERROR(laServerErrors)
DO CASE
  CASE laServerErrors[1,5]=11
    lcErrMsg = 'Server not available, check your network connections and be sure that the server '+;
               lcSqlSvr + ' is turned on.'
  CASE laServerErrors[1,5]=6
    lcErrMsg = 'Specified SQL server not found, check the spelling of the server name.'
  CASE laServerErrors[1,5]=18456
    lcErrMsg = "Login failed for user 'ARIA', please check that there is a user called 'ARIA' with correct password."
  CASE laServerErrors[1,5]=916
    lcErrMsg = "The server user 'ARIA' does not have the right to access this report data."
  OTHERWISE               
    lcErrMsg = laServerErrors[1,2]
ENDCASE
=MESSAGEBOX(lcErrMsg,0 + 16, "Aria Advantage Series")
*-- end of Server_Error.


*:**************************************************************************
*:* Name        : lfOpenFiles
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/06/01
*:* Purpose     : Open Needed files
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


=lfOpen('STYLE','STYLE')
=lfOpen('POSHDR','POSHDR')

=lfOpen('POSLN','Poslns')           && KEY => STYLE+CSTYTYPE+PO+STR(LINENO,6)+TRANCD
=lfOpen('POSLN','Poslns','POSLN2')  && KEY => STYLE+CSTYTYPE+PO+STR(LINENO,6)+TRANCD

=lfOpen('ORDHDR','ORDHDR')
=lfOpen('ORDLINE','ORDLINE')
=lfOpen('CUTPICK','CUTPICK')

=lfOpen('BOM','BOM')

=lfOpen('SPCK_HDR','SPCK_HDR')
=lfOpen('SPCK_LIN','Spcklnst')
=lfOpen('STYLEUPC','STYLEUPC')
=lfOpen('SCALE','SCALE')
*=lfOpen('ICDEPTHD','Depthd')
=lfOpen('ICDEPTHD','Cgroup')
*=lfOpen('ICDEPTDT','Deptdts')
=lfOpen('CODES','CODES')
=lfOpen('CUTPICK')
=lfOpen('BOMLINE')

*-- end of lfOpenFiles.


*:**************************************************************************
*:* Name        : lfOpen
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/18/01
*:* Purpose     : Open needed files
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfOpen()
*:***************************************************************************
FUNCTION lfOpen
PARAMETERS lcFile,lcOrder,lcAlias
lcOrder = IIF(EMPTY(lcOrder),'',lcOrder)
lcAlias = IIF(EMPTY(lcAlias),lcFile,lcAlias)
IF !USED(lcAlias)
  USE (ALLTRIM(lcRpDtDir)+lcFile) AGAIN ORDER (lcOrder) in 0 SHARE ALIAS (lcAlias)
ENDIF
*-- end of lfOpen.


*:**************************************************************************
*:* Name        : lfSetRelations
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/06/01
*:* Purpose     : Set needed relations
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfSetRelations()
*:***************************************************************************
FUNCTION lfSetRelations

SELECT STYLE
*!*	SET RELATION TO STYLE.STYLE     INTO ICDEPTDT,;
*!*	                'S'+STYLE.STYLE INTO SPCK_LIN,;
*!*	                STYLE.STYLE     INTO STYLEUPC,;
*!*	                'S'+STYLE.SCALE INTO SCALE
SET RELATION TO STYLE.CSTYGROUP INTO ICDEPTHD,;
                'S'+STYLE.STYLE INTO SPCK_LIN,;
                STYLE.STYLE     INTO STYLEUPC,;
                'S'+STYLE.SCALE INTO SCALE
                
*--These relations to Calculate qty allocated for Bulk orders
SELECT POSLN
SET RELATION TO "2"+Posln.po+Posln.style INTO CUTPICK,;
                'P'+POSLN.PO INTO POSHDR ,;
                STYLE+CSTYTYPE+PO+STR(LINENO,6)+'2' INTO POSLN2
SELECT CUTPICK
SET RELATION TO "O"+Cutpick.order INTO ORDLINE
SELECT ORDLINE
SET RELATION TO Ordline.cordtype+Ordline.order INTO ORDHDR

SELECT SPCK_LIN
SET RELATION TO "P"+Spck_lin.account+Spck_lin.PACK_ID INTO SPCK_HDR

*!*	SELECT ICDEPTDT
*!*	SET RELATION TO Icdeptdt.dept INTO ICDEPTHD

*-- end of lfSetRelations.

*:**************************************************************************
*:* Name        : lfClose
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/19/01
*:* Purpose     : Close opend tables
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : lcDBF
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfClose('STYLE')
*:***************************************************************************
FUNCTION lfClose
PARAMETERS lcDBF
IF EMPTY(lcDBF)
  CLOSE TABLES ALL
ELSE
  IF USED(lcDBF)
    USE IN (lcDBF)
  ENDIF
ENDIF
*-- end of lfClose.

*:**************************************************************************
*:* Name        : lfCreateTable
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/18/01
*:* Purpose     : Check if 'SLSPRDDATA' is not in the database Create it
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfCreateTable()
*:***************************************************************************
FUNCTION lfCreateTable
LOCAL lnErr,llRet,llRet2,llDbRet
*C102223,4 TMI [Start] If database is not found create it
lnErr = SQLEXEC(lnConnectionHandle,'sp_databases', 'GMADBS')
IF lnErr>0 
  SELECT GMADBS
  LOCATE FOR ALLT(UPPER(GMADBS.DATABASE_NAME))= ALLT(UPPER('DB'+lcRpComp))
  IF FOUND()
    llDbRet = .T.
    USE IN GMADBS
  ELSE
    IF SQLEXEC(lnConnectionHandle,'CREATE DATABASE DB'+lcRpComp) = -1
      =Server_Error()
      USE IN GMADBS
      RETURN .F.
    ENDIF
  ENDIF
ELSE
  RETURN .F.
ENDIF  

*--Let the created database the current 
=SQLEXEC(lnConnectionHandle,'USE DB'+lcRpComp)
*C102223,4 TMI [End  ]
lnErr = SQLTABLES(lnConnectionHandle,'TABLE', 'GMATBLS')
IF lnErr=1 
  SELECT 'GMATBLS'
  LOCATE FOR UPPER(ALLT(GMATBLS.table_name)) = 'SLSPRDDATA'
  IF FOUND()
    llRet = .T.
  ELSE
  *--C102223,4 TMI [START] change Depatment field to be char(6) as in sepcs 
  *--C102223,4     add a new field to hold department description
  *--C102223,4     add a new field to hold the size with 6
  *--C102223,4     change the width of CDESC to 15 
*!*	    lcTblStr = 'CREATE TABLE SLSPRDDATA  ('+;
*!*	                             'ROWTYPE        CHAR(1),'  +;
*!*	                             'DEPT           CHAR(36),' +;
*!*	                             'CSTYGROUP      CHAR(6),'  +;
*!*	                             'CCATG          CHAR(6),'  +;
*!*	                             'CCLASS         CHAR(6),'  +;
*!*	                             'CSUB_CLASS     CHAR(6),'  +;
*!*	                             'STYLE          CHAR(12),' +;
*!*	                             'COLOR          CHAR(6),'  +;
*!*	                             'CDESC          CHAR(20),' +;
*!*	                             'PO             CHAR(6),'  +;
*!*	                             'IMMOTS         INT,'      +;
*!*	                             'ONORDER        INT,'      +;
*!*	                             'ALLOCATED      INT,'      +;
*!*	                             'OTS            INT,'      +;
*!*	                             'ONHOLD         INT,'      +;
*!*	                             'PRICEA         MONEY,'    +;
*!*	                             'PRICEB         MONEY,'    +;
*!*	                             'NSUGRETPRI     MONEY,'    +;
*!*	                             'MSPRICE        MONEY,'    +;
*!*	                             'ETD            DATETIME,' +;
*!*	                             'ETA            DATETIME,' +;
*!*	                             'CUSTDELDT      DATETIME,' +;
*!*	                             'UPC            CHAR(20),' +; && 'UPC            CHAR(13),' +; wab
*!*	                             'INNERPACK      INT,'      +;
*!*	                             'MASTPACK       INT,'      +;
*!*	                             'MPLENGHT       INT,'      +;
*!*	                             'MPWIDTH        INT,'      +;
*!*	                             'MPDEPTH        REAL,'     +;
*!*	                             'MPCUBE         REAL,'     +;
*!*	                             'MPWEIGHT       REAL,'      +;
*!*	                             'PACK_ID        CHAR(16),' +;
*!*	                             'PACK_DESC      CHAR(20),' +;
*!*	                             'SELLPRICE      MONEY,'    +;
*!*	                             'SHIPVIA        CHAR(1),'  +;
*!*	                             'FOBCOST        MONEY,'    +;
*!*	                             'CMTCOST        MONEY,'    +;
*!*	                             'DUTY           MONEY,'    +;
*!*	                             'FREIGHT        MONEY,'    +;
*!*	                             'TICKETING      MONEY,'    +;
*!*	                             'FABRICCOST     MONEY,'    +;
*!*	                             'LANDEDCOST     MONEY,'    +;
*!*	                             'MFG            CHAR(8),'  +;
*!*	                             'COMMISSION     CHAR(10),' +;
*!*	                             'EXTENSION      MONEY,'    +;
*!*	                             'HTS            INT,'      +;
*!*	                             'ADD_DATETIME   DATETIME)'
    lcTblStr = 'CREATE TABLE DB'+lcRpComp+'.DBO.SLSPRDDATA  ('+;   
                             'ROWTYPE        CHAR(2),'  +;
                             'DEPT           CHAR(6),'  +;
                             'CSTYGROUP      CHAR(6),'  +;
                             'CCATG          CHAR(6),'  +;
                             'CCLASS         CHAR(6),'  +;
                             'CSUB_CLASS     CHAR(6),'  +;
                             'STYLE          CHAR(12),' +;
                             'COLOR          CHAR(6),'  +;
                             'CDESC          CHAR(15),' +;
                             'PO             CHAR(6),'  +;
                             'IMMOTS         INT,'      +;
                             'ONORDER        INT,'      +;
                             'ALLOCATED      INT,'      +;
                             'OTS            INT,'      +;
                             'ONHOLD         INT,'      +;
                             'PRICEA         MONEY,'    +;
                             'PRICEC         MONEY,'    +;
                             'NSUGRETPRI     MONEY,'    +;
                             'MSPRICE        MONEY,'    +;
                             'ETD            DATETIME,' +;
                             'ETA            DATETIME,' +;
                             'CUSTDELDT      DATETIME,' +;
                             'UPC            CHAR(13),' +; 
                             'INNERPACK      INT,'      +;
                             'MASTPACK       INT,'      +;
                             'MPLENGHT       INT,'      +;
                             'MPWIDTH        INT,'      +;
                             'MPDEPTH        REAL,'     +;
                             'MPCUBE         REAL,'     +;
                             'MPWEIGHT       REAL,'      +;
                             'PACK_ID        CHAR(16),' +;
                             'PACK_DESC      CHAR(20),' +;
                             'SELLPRICE      MONEY,'    +;
                             'SHIPVIA        CHAR(1),'  +;
                             'FOBCOST        MONEY,'    +;
                             'CMTCOST        MONEY,'    +;
                             'DUTY           MONEY,'    +;
                             'FREIGHT        MONEY,'    +;
                             'TICKETING      MONEY,'    +;
                             'FABRICCOST     MONEY,'    +;
                             'LANDEDCOST     MONEY,'    +;
                             'MFG            CHAR(8),'  +;
                             'COMMISSION     CHAR(10),' +;
                             'EXTENSION      MONEY,'    +;
                             'HTS            INT,'      +;
                             'ADD_DATETIME   DATETIME,' +;
                             'DEPTDESC       CHAR(30),' +;
                             'SIZE           CHAR(6),' +;
                             'STYLECOLOR     CHAR(19))'                            
*--C102223,4 TMI [END  ]

    STORE .T. TO llRet,llRet2
    llRet =  SQLEXEC(lnConnectionHandle,lcTblStr) <> -1 
    
    IF !llRet
      =Server_Error()
    ENDIF
    
*--No need for index since Crystal report DO its own indexing
*!*	    lcIndexStr = 'CREATE INDEX DGCCSSCR ON SLSPRDDATA ('+;
*!*	                              'DEPT,CSTYGROUP,CCATG,CCLASS,CSUB_CLASS,STYLE,COLOR,ROWTYPE)'
*!*	    llRet2 = SQLEXEC(lnConnectionHandle,lcIndexStr) <> -1 
*!*	    IF !llRet2
*!*	      =Server_Error()
*!*	    ENDIF     
    
    llRet = llRet AND llRet2 
        
  ENDIF
  
  USE IN 'GMATBLS'
ENDIF  
RETURN llRet
*-- end of lfCreateTable.


*:**************************************************************************
*:* Name        : lfCollectData
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/18/01
*:* Purpose     : Collecting data
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfCollectData()
*:***************************************************************************
FUNCTION lfCollectData
*--First delete all existing records from the SQL Server table
lnErr = SQLEXEC(lnConnectionHandle,'DELETE DB'+lcRpComp+'.DBO.SLSPRDDATA')

*-- Define field variables , Initialize all with NULL 
PRIVATE lcAppendFields,lcInsrtExp
PRIVATE laData
  *--C102223,4 TMI [START] change Depatment field to be char(6) as in sepcs AND add a new field 
  *--C102223,4 TMI         to hold department description,add new field to hold size
  *--C102223,4 TMI         Redefine laData lenght depends on field in the string lcAppendFields  
*DIMENSION laData[46]
*--------------'-1-------------,2-------------,3-------------,4--------------,5-----------*
*!*	lcAppendFields=' ROWTYPE       ,DEPT          ,CSTYGROUP     ,CCATG          ,CCLASS'    +; && 5
*!*	               ',CSUB_CLASS    ,STYLE         ,COLOR         ,CDESC          ,PO'        +; && 10
*!*	               ',IMMOTS        ,ONORDER       ,ALLOCATED     ,OTS            ,ONHOLD'    +; && 15
*!*	               ',PRICEA        ,PRICEB        ,NSUGRETPRI    ,MSPRICE        ,ETD'       +; && 20
*!*	               ',ETA           ,CUSTDELDT     ,UPC           ,INNERPACK      ,MASTPACK'  +; && 25
*!*	               ',MPLENGHT      ,MPWIDTH       ,MPDEPTH       ,MPCUBE         ,MPWEIGHT'  +; && 30
*!*	               ',PACK_ID       ,PACK_DESC     ,SELLPRICE     ,SHIPVIA        ,FOBCOST'   +; && 35
*!*	               ',CMTCOST       ,DUTY          ,FREIGHT       ,TICKETING      ,FABRICCOST'+; && 40
*!*	               ',LANDEDCOST    ,MFG           ,COMMISSION    ,EXTENSION      ,HTS'       +; && 45
*!*	               ',ADD_DATETIME'                                                              && 47 
lcAppendFields=' ROWTYPE       ,DEPT          ,CSTYGROUP     ,CCATG          ,CCLASS'    +; && 5
               ',CSUB_CLASS    ,STYLE         ,COLOR         ,CDESC          ,PO'        +; && 10
               ',IMMOTS        ,ONORDER       ,ALLOCATED     ,OTS            ,ONHOLD'    +; && 15
               ',PRICEA        ,PRICEC        ,NSUGRETPRI    ,MSPRICE        ,ETD'       +; && 20
               ',ETA           ,CUSTDELDT     ,UPC           ,INNERPACK      ,MASTPACK'  +; && 25
               ',MPLENGHT      ,MPWIDTH       ,MPDEPTH       ,MPCUBE         ,MPWEIGHT'  +; && 30
               ',PACK_ID       ,PACK_DESC     ,SELLPRICE     ,SHIPVIA        ,FOBCOST'   +; && 35
               ',CMTCOST       ,DUTY          ,FREIGHT       ,TICKETING      ,FABRICCOST'+; && 40
               ',LANDEDCOST    ,MFG           ,COMMISSION    ,EXTENSION      ,HTS'       +; && 45
               ',ADD_DATETIME  ,DEPTDESC      ,SIZE          ,STYLECOLOR'                   && 49
DIMENSION laData[OCCURS(',',lcAppendFields)+1]                 
  *--C102223,4 TMI [end  ]

***lcAppendFields=' ROWTYPE,DEPT,CSTYGROUP,CCATG,CCLASS,CSUB_CLASS,STYLE,COLOR,CDESC,PO,IMMOTS,ONORDER,ALLOCATED,OTS,ONHOLD,PRICEA,PRICEB,NSUGRETPRI,MSPRICE,ETD,ETA,CUSTDELDT,UPC,INNERPACK,MASTPACK,MPLENGHT,MPWIDTH,MPDEPTH,MPCUBE,MPWEIGHT,PACK_ID,PACK_DESC,SELLPRICE,SHIPVIA,FOBCOST,CMTCOST,DUTY,FREIGHT,TICKETING,FABRICCOST,LANDEDCOST,MFG,COMMISSION,EXTENSION,HTS,ADD_DATETIME'
lcAppendFields = STRTRAN(lcAppendFields,' ')               
lcInsrtExp = 'INSERT DB'+lcRpComp+'.DBO.SLSPRDDATA ('+ lcAppendFields + ') VALUES '

*!*	*--Spck search expressions
*!*	lcCatg  = lfGetTEntries('laRpTCatg')
*!*	lcClas  = lfGetTEntries('laRpTClass')
*!*	lcSbCls = lfGetTEntries('laRpTSbCls')

*--Get Filters
STORE '.T.' TO lcDeptExp
IF !EMPTY(laRpTDep[1])  
*C102224,4 TMI [Start] Due to a bug in filling the table ICDEPTDT I'll not depend on it in DEPT
*C102224,4 TMI [Start] criteria
  *lcDeptExp = "ICDEPTDT.DEPT $ '"+lfDepts()+"'"
  lcDeptExp = "ICDEPTHD.DEPT $ '"+lfDepts()+"'"
*C102224,4 TMI [End  ]
ENDIF

*--Create ranges cursor
=lfRangeCurs()

*C102224,1 TMI [Start] Define array to hold cost items values of an order recived in more than one time
DIME laCostItem[5]
*C102224,1 TMI [End  ]

SELECT STYLE

SCAN FOR &lcRpExp AND &lcDeptExp
  *C102224,1 TMI [Start] Let the message box be on top of all windows screens
  *WAIT WINDOW 'COLLECTING DATA FOR STYLE :' + STYLE.STYLE ;
  *            NOWAIT ;
  *            AT 5,40  
  *FRMMSG.lblMsg.CAPTION = 'COLLECTING DATA FOR STYLE :' + STYLE.STYLE
  FRMMSG.lblMsg.CAPTION =  STYLE.STYLE
  *C102224,1 TMI [End  ]
    
  laData = 'NULL'                    && Initialize collecting data array with 'NULL'            
  STORE 0 TO laData[11],laData[12],laData[13],laData[14],laData[15]
  laData[46] = lcDTRun               && Set time for running the report
  
  *--Always for a style fill ROWTYPE with 'S',
  *--then go to styleUPC , get all upc's for this style
  *--then go to PosLine get all PO for this style.
  
  laData[1 ] = '0'                                         && ROWTYPE 
  *C102223,4 TMI [Start] Change the DEPT width to 6
  *laData[2 ] = ICDEPTDT.DEPT+'-'+ICDEPTHD.CDEPTDESC        && Deprtment
  *laData[2 ] = ICDEPTDT.DEPT
  laData[2 ] = ICDEPTHD.DEPT
  laData[47] = ICDEPTHD.CDEPTDESC   
  *C102223,4 TMI [End  ]
  laData[3 ] = STYLE.CSTYGROUP
  laData[4 ] = STYLE.CSTYCATG
  laData[5 ] = STYLE.CSTYCLASS
  laData[6 ] = STYLE.CSTYSBCLAS
  laData[7 ] = SUBSTR(STYLE.STYLE,1,lnRpClrPos-2)  
  laData[8 ] = SUBSTR(STYLE.STYLE,lnRpClrPos,lnRpClrLen)  
  *C102224,4 TMI [Start] Add a new field "style-color"
  laData[49] = STYLE.STYLE
  *C102224,4 TMI [End  ]       
  *C102223,4 TMI [Start] cdesc field in specs is length 15  
  *laData[9 ] = STYLE.DESC
  laData[9 ] = SUBSTR(STYLE.DESC,1,15)
  *C102223,4 TMI [End  ]  
  
  laData[11] = STYLE.TOTSTK - STYLE.TOTALO
  laData[16] = STYLE.PRICEA
  *C102223,4 TMI [Start] this should be priceC not priceB
  *laData[17] = STYLE.PRICEB
  laData[17] = STYLE.PRICEC
  *C102223,4 TMI [End  ] 
  laData[18] = STYLE.NSUGRETPRI
  laData[19] = STYLE.NMSPRICE

  laData[24] = STYLE.NINPACK
  laData[25] = STYLE.NMPACK

  laData[26] = STYLE.NMPDIM
  laData[27] = STYLE.NMPCUBE
  laData[28] = STYLE.NMPDEPTH
  
  *C102224,1 TMI [Start] Collecting data is done by feet
  *laData[29] = laData[26]*laData[27]*laData[28]*(1/1728)*IIF(lcRpMsUnit='M',0.304801,1)
  laData[29] = laData[26]*laData[27]*laData[28]*(1/1728)
  *C102224,1 TMI [End  ]
  
  laData[30] = STYLE.NMPWEIGHT
  
  IF STYLE.NICOST&laTypef[1]=0  && if the material cost = 0
    laData[35] = STYLE.NICOST1
    laData[36] = 0
  ELSE                          && if the material cost <> 0
    laData[35] = 0
    laData[36] = STYLE.NICOST1
  ENDIF

  laData[37] = STYLE.NICOST&laTypeD[2]  && This depends on the GMA setup where Duty   is the cost element#3
  laData[38] = STYLE.NICOST&laTypeD[1]  && This depends on the GMA setup where Fright is the cost element#2

  laData[39] = STYLE.NICOST&laTypeM[1].
  laData[40] = STYLE.NICOST&laTypeF[1].
  
  laData[41] = laData[35]+laData[36]+laData[37]+laData[38]+laData[39]+laData[40]
  
  *--If no records of this style in STYLEUPC then add a line
  IF EOF('STYLEUPC')
    =lfInsert()
  ELSE
    *--Add UPC for each size
    SELECT STYLEUPC
    SCAN REST WHILE STYLEUPC.STYLE = STYLE.STYLE
      lcSz = STYLEUPC.SIZE
      *C102223,4 TMI [Start] Add a separete field for the size and change the upc field width to 13      
      *laData[23] = SCALE.SZ&lcSz+":"+STYLEUPC.CUPCNUM1+STYLEUPC.CUPCNUM2+STYLEUPC.CUPCNUM3
      laData[23] = STYLEUPC.CUPCNUM1+STYLEUPC.CUPCNUM2+STYLEUPC.CUPCNUM3
      laData[48] = SCALE.SZ&lcSz
      
      laData[1 ] = '0' + ALLT(STYLEUPC.SIZE)
      *--Add a field to hold record order
      *C102223,4 TMI [End  ]            
      
      =lfInsert()
    ENDSCAN
  ENDIF

  *--Data from POSLN
  SELECT POSLN
  && KEY :: STYLE+CSTYTYPE+PO+STR(LINENO,6)+TRANCD
  IF SEEK(STYLE.STYLE+'P','POSLN','POSLNS')
    SCAN REST WHILE POSLN.STYLE+POSLN.CSTYTYPE=STYLE.STYLE+'P' FOR TRANCD='1'
        =SEEK('P'+POSLN.PO,'POSHDR','POSHDR')
        *C102224,1 TMI [Start] include only Open and hold PO's
        IF !(POSHDR.STATUS $ 'OH')
          LOOP
        ENDIF
        *C102224,1 TMI [End  ]       
                
        laData[1 ] = '1'
        laData[10] = POSLN.PO
        laData[12] = POSLN.TOTQTY
        laData[13] = POSLN.TOTORD                          
        laData[14] = laData[12] - laData[13]               
        laData[15] = lfBulkOrd()
  
        laData[20] = POSHDR.COMPLETE
        laData[21] = POSHDR.AVAILABLE
        laData[22] = POSHDR.AVAILABLE + POSHDR.NDELDAYS
  
        laData[24] = POSLN.NINPACK
        laData[25] = POSLN.NMPACK
  
        laData[26] = POSLN.NMPDIM
        laData[27] = POSLN.NMPCUBE
        laData[28] = POSLN.NMPDEPTH
  
        *C102224,1 TMI [Start] Collecting data is by feet
        *laData[29] = laData[26]*laData[27]*laData[28]*(1/1728)*IIF(lcRpMsUnit='M',0.304801,1)
        laData[29] = laData[26]*laData[27]*laData[28]*(1/1728)
        *C102224,1 TMI [End  ]
        
        laData[30] = POSLN.NMPWEIGHT
        
        laData[34] = lfShipVia()
        *C102224,1 TMI [Start] Calculate Landed cost if style is recived more than once        
        laCostItem = 0        
        =lfCostItem()
        *C102224,1 TMI [End  ]        

        IF POSLN.NCOST&laTypeF[1]=0
          *C102224,1 TMI [Start] Calculate Landed cost if style is recived more than once
          *laData[35] = POSLN2.NLAN_CST1
          laData[35] = laCostItem[1]
          *C102224,1 TMI [End  ]                    
          laData[36] = 0
        ELSE
          laData[35] = 0
          *C102224,1 TMI [Start] Calculate Landed cost if style is recived more than once
          *laData[36] = POSLN2.NLAN_CST1
          laData[36] = laCostItem[1]
          *C102224,1 TMI [End  ]          
        ENDIF

        *C102224,1 TMI [Start] Calculate cost items if style is recived more than once
        *laData[37] = POSLN.NCOST&laTypeD[2]   && This array element depends on the GMA setup where Duty is the cost element#3
        *laData[38] = POSLN.NCOST&laTypeD[1]   && This array element depends on the GMA setup where Fright is the cost element#2

        *laData[39] = POSLN.NCOST&laTypeM[1].
        *laData[40] = POSLN.NCOST&laTypeF[1].

        laData[37] = laCostItem[2]   && This array element depends on the GMA setup where Duty is the cost element#3
        laData[38] = laCostItem[3]   && This array element depends on the GMA setup where Fright is the cost element#2

        laData[39] = laCostItem[4]
        laData[40] = laCostItem[5]
        *C102224,1 TMI [End  ]
           
        laData[41] = laData[35]+laData[36]+laData[37]+laData[38]+laData[39]+laData[40]            
        
        laData[42] = POSLN.VENDOR
        laData[44] = laData[14]*laData[41]
        laData[45] = POSHDR.NHTS

        *--Add a line for each P/O that this style is included in      
        =lfInsert()        
        
    ENDSCAN
  ENDIF
  
  *--Range Data
  IF SEEK("P"+Style.style,'SPCK_LIN','SPCKLNST')
    SELECT SPCK_LIN
    SCAN REST WHILE TYPE+STYLE="P"+Style.style
      *C102224,1 TMI [Start] Include only generic packs
      *IF SPCK_HDR.LRANGE
      IF SPCK_HDR.ACCOUNT='*****' AND SPCK_HDR.LRANGE
      *C102224,1 TMI [End  ]      
        =SEEK(Spck_hdr.account+Spck_hdr.pack_id,'STYLEUPC','PACKUPC')
        laData[1 ] = '2'
        laData[23] = STYLEUPC.CUPCNUM1+STYLEUPC.CUPCNUM2+STYLEUPC.CUPCNUM3
        laData[31] = SPCK_HDR.PACK_ID
        laData[32] = SPCK_HDR.DESC
        laData[33] = SPCK_HDR.NPKSLPRICE   && This field name need to be reviowed with Waleed          
*!*	          lcSQL = "SELECT * FROM SLSPRDDATA WHERE ROWTYPE='2'"+;
*!*	                            " AND DEPT='"      +laData[2] +"'"+;
*!*	                            " AND CSTYGROUP='" +laData[3] +"'"+;
*!*	                            " AND CCATG='"     +laData[4] +"'"+;
*!*	                            " AND CCLASS='"    +laData[5] +"'"+;
*!*	                            " AND UPC='"       +laData[23]+"'"+;
*!*	                            " AND PACK_ID='"   +laData[31]+"'"
*!*	          lnErr = SQLEXEC(lnConnectionHandle,lcSQL,'lcResult')
*!*	          IF lnErr<>-1
*!*	            SELECT lcResult
*!*	            IF RECCOUNT()=0
*!*	              =lfInsert()
*!*	              USE IN lcResult
*!*	            ENDIF
*!*	          ENDIF
          
          *--Use a cursor to check if a range field is added or not , instead of checking on the SERVER
*       IF NOT SEEK(laData[2]+laData[3]+laData[4]+laData[5]+laData[23]+laData[31],'RANGECUR','RANGECUR')
        IF NOT SEEK(laData[2]+laData[3]+laData[4]+laData[5]+laData[6]+laData[23]+laData[31],'RANGECUR','RANGECUR')
          INSERT INTO RANGECUR VALUES (laData[2],laData[3],laData[4],laData[5],laData[6],laData[23],laData[31])
*         STORE 'NULL' TO laData[6],laData[7],laData[8]
          =lfInsert()
        ENDIF
          
      ENDIF
    ENDSCAN
  ENDIF
  
ENDSCAN
*-- end of lfCollectData.

*:**************************************************************************
*:* Name        : lfValues
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/19/01
*:* Purpose     : Create a stirng containing the values to be inserted in the 
*:*             : SQL Server table.
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfValues()
*:***************************************************************************
FUNCTION lfValues
LOCAL lnCnt,lcRet
lcRet = ''
FOR lnCnt=1 TO ALEN(laData,1)
  DO CASE
    CASE VARTYPE(laData[lnCnt])='C'
      IF laData[lnCnt]='NULL'
        lcRet = lcRet + laData[lnCnt] + ","
      ELSE
        *B605188,1 TMI [START] Add extra qotation mark to eliminate apostrof error
        *lcRet = lcRet + "'" + laData[lnCnt] + "',"
        lcRet = lcRet + "'" + STRTRAN(laData[lnCnt],"'","''") + "',"
        *B605188,1 TMI [END  ] Add extra qotation mark to eliminate apostrof error
      ENDIF

    CASE VARTYPE(laData[lnCnt])='N'
     lcRet = lcRet + ALLT(STR(laData[lnCnt],16,2)) +  ","

    CASE VARTYPE(laData[lnCnt])='D'
     lcRet = lcRet + "'" + DTOS(laData[lnCnt]) + "',"

  ENDCASE
NEXT

*-- Add brackets and Remove last comma
lcRet = '(' + LEFT(lcRet,LEN(lcRet)-1 ) + ')'
RETURN lcRet
*-- end of lfValues.

*:**************************************************************************
*:* Name        : lfSettings
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/19/01
*:* Purpose     : Set needed settings
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfSettings()
*:***************************************************************************
FUNCTION lfSettings
SET DELETED ON
*-- end of lfSettings.

*:**************************************************************************
*:* Name        : lfGetTEntries
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/21/01
*:* Purpose     : Build expression contains Target Arrays entries
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfGetTEntries()
*:***************************************************************************
FUNCTION lfGetTEntries
PARAMETERS lcTArrName
LOCAL lnCnt,lcRet
lcRet = ''
IF EMPTY(&lcTArrName.[1])
  RETURN lcRet
ENDIF

FOR lnCnt = 1 TO ALEN(&lcTArrName,1)
  lcRet = lcRet + PADR(&lcTArrName.[lnCnt],6) + '|'
NEXT
RETURN lcRet
*-- end of lfGetTEntries.

*:**************************************************************************
*:* Name        : lfInsert
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/21/01
*:* Purpose     : Inserts a record into SQL SERVER
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfInsert()
*:***************************************************************************
FUNCTION lfInsert
LOCAL lcInsert,lnCnt
lcInsert = lcInsrtExp + lfValues() 
lnErr = SQLEXEC(lnConnectionHandle,lcInsert)
IF lnErr>0
  lnRowCount = lnRowCount + 1
  *--Reset laData except grouping fields
  FOR lnCnt = 9 TO ALEN(laData,1)    
    laData[lnCnt] = 'NULL'
  NEXT
  *STORE 0 TO laData[11],laData[12],laData[13],laData[14],laData[15]  
  STORE lcDTRun TO laData[46]    && Set time for running the report      
  *C102224,4 TMI [Start] A new field for style color added
  laData[47] = laData[2]
  laData[49] = laData[7]+'-'+laData[8]
  *C102224,4 TMI [End  ]  
ELSE
  IF lnErr<0
    =Server_Error()
    RETURN TO MASTER
  ENDIF
ENDIF  
*-- end of lfInsert.

*:**************************************************************************
*:* Name        : lfShipVia
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/21/01
*:* Purpose     : Get first charachter of the Ship Via desc
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfShipVia()
*:***************************************************************************
FUNCTION lfShipVia
LOCAL lcRet
lcRet = ''
&& KEY :: CDEFCODE+CCODE_NO+CRLTFIELD+CFLD_NAME
IF SEEK('N'+POSHDR.SHIPVIA+'N'+'SHIPVIA   ','CODES','CODES')
  lcRet = LEFT(CODES.CDISCREP,1)
ENDIF
RETURN lcRet
*-- end of lfShipVia.

*:**************************************************************************
*:* Name        : lfBulkOrd
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/23/01
*:* Purpose     : Sum quantities allocated from PO to usual orders 
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : none
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfBulkOrd()
*:***************************************************************************
FUNCTION lfBulkOrd
LOCAL lnSum
lnSum = 0
SELECT CUTPICK
SCAN REST WHILE TRANCD+CTKTNO+STYLE="2"+Posln.po+Posln.style FOR NOT EMPTY(ORDHDR.CFROMORDER)
  lnSum = lnSum + CUTPICK.TOTQTY
ENDSCAN
RETURN lnSum   
*-- end of lfBulkOrd.

*:**************************************************************************
*:* Name        : lfDepts
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/27/01
*:* Purpose     : Builds a Dept inlist expression
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfDepts()
*:***************************************************************************
FUNCTION lfDepts
LOCAL lnCnt , lcStr
lcStr = ''
FOR lnCnt = 1 TO ALEN(laRpTDep,1)
  lcStr = lcStr + PADR(laRpTDep[lnCnt],6) + ' | '
NEXT
RETURN lcStr
*-- end of lfDepts.

*:**************************************************************************
*:* Name        : lfRangeCurs
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/27/01
*:* Purpose     : Create range cursor-->a temp. cursor that hold range data
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfRangeCurs()
*:***************************************************************************
FUNCTION lfRangeCurs
SELECT 0
*!*	CREATE CURSOR RANGECUR ;
*!*	(laData_2 C(6),laData_3 C(6),laData_4 C(6),laData_5 C(6),laData_23 C(13),laData_31 C(16))
*!*	INDEX ON laData_2+laData_3+laData_4+laData_5+laData_23+laData_31 TAG RANGECUR
CREATE CURSOR RANGECUR ;
(laData_2 C(6),laData_3 C(6),laData_4 C(6),laData_5 C(6),laData_6 C(6),laData_23 C(13),laData_31 C(16))
INDEX ON laData_2+laData_3+laData_4+laData_5+laData_6+laData_23+laData_31 TAG RANGECUR
*-- end of lfRangeCurs.

*:**************************************************************************
*:* Name        : lfCostItem
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/07/01
*:* Purpose     : Calculate Landed cost items for styles
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : COST ITEM FIELD
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfCostItem('NLAN_CST1')
*:***************************************************************************
FUNCTION lfCostItem
LOCAL lcOldSlct,lnRcvd
lcOldSlct = SELECT()
STORE 0 TO lnCost,lnRcvd

SELECT POSLN2
SCAN REST WHILE STYLE+      CSTYTYPE+      PO+STR(      LINENO,6)+TRANCD = ;
          POSLN.STYLE+POSLN.CSTYTYPE+POSLN.PO+STR(POSLN.LINENO,6)+'2'
  laCostItem[1] = laCostItem[1] + TOTQTY * NLAN_CST1
  laCostItem[2] = laCostItem[2] + TOTQTY * NLAN_CST&laTypeD[2]
  laCostItem[3] = laCostItem[3] + TOTQTY * NLAN_CST&laTypeD[1]
  laCostItem[4] = laCostItem[4] + TOTQTY * NLAN_CST&laTypeM[1]
  laCostItem[5] = laCostItem[5] + TOTQTY * NLAN_CST&laTypeF[1]
  lnRcvd = lnRcvd + TOTQTY
ENDSCAN
SELECT (lcOldSlct)

IF lnRcvd > 0
  laCostItem[1] = laCostItem[1]/lnRcvd
  laCostItem[2] = laCostItem[2]/lnRcvd
  laCostItem[3] = laCostItem[3]/lnRcvd
  laCostItem[4] = laCostItem[4]/lnRcvd
  laCostItem[5] = laCostItem[5]/lnRcvd
ENDIF

*-- end of lfLandedCost.