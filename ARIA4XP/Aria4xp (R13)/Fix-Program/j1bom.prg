**************************************************************************************************
*T20140812.0010	Style PO's - many costs are doubling
* Developer : Tarek Mohamed Ibrahim
* 

*As discussed with Mariam we need to check only for styles with Detailed Cost = ‘No’
*Get the related lines in the BOM
*For styles with scale other than KK, just replace the *** with the scale ID
*For KK - styles make sure that you have two lines for each style, one for KKA and one for KKC and remove the *** line if it is there.
*
**************************************************************************************************


* The following code from here until the sign **T20140812.0010** is a copy from ExtConv.prg with minor modification, the program starts after that line
PARAMETERS lcCoID,lcStyParam

lcCoID = 'J1'

lcCoID = IIF(EMPTY(lcCoID),'  ',lcCoID)
lnCnt = 0

lnStart = DATETIME() 
?lnStart 

CLOSE DATABASES 

lnAbort = 0

SET EXCLUSIVE OFF
SET DELETED ON 
SET CPDIALOG OFF
SET SAFETY OFF

IF EMPTY(lcCoID)
  MESSAGEBOX('Pls specifiy the Co. id as a prarmeter to the program')
  RETURN 
ELSE
  lcMsg = 'run a fix data on the Co. &lcCoID to replace the *** in BOM.citmmask with the scale value'
  MESSAGEBOX('Pls confirm that you will '+lcMsg)
  IF MESSAGEBOX('Are you sure you want to '+lcMsg,4+256)<>6
    RETURN 
  ENDIF 
ENDIF 

*lcSyspath = 'x:\aria4xp\sysfiles\'
lcSyspath = GETDIR('','Sysfiles Path')
IF EMPTY(lcSyspath) OR !FILE(lcSyspath+'SYDFDCHG.dbf')
  MESSAGEBOX('invalid sysfiles path')
  RETURN 
ENDIF 
 
*- connect to sql server database
SELECT 0
lnSqlConn = 0
USE (lcSyspath+'syccomp') ORDER 1
IF SEEK( lcCoID )  
  WAIT WINDOW NOWAIT 'CONNECT TO THE SQL DATABASE'
  lnSqlConn = SQLSTRINGCONNECT('driver=sql server;server='+ALLTRIM(syccomp.cconserver)+';database='+ALLTRIM(syccomp.ccondbname)+';uid='+ALLTRIM(syccomp.cconuserid)+';pwd='+ALLTRIM(syccomp.cconpaswrd)+';')
  IF lnSqlConn<0
    MESSAGEBOX('unable to connect to the sql database')
    RETURN 
  ENDIF 
ELSE
  MESSAGEBOX('wrong Co. ID passed')
  RETURN 
ENDIF 

lcStyleDataPath = ADDBS(ALLTRIM(SYCCOMP.CCOM_DDIR))

*- "Updating Scale File"
IF !FILE(lcStyleDataPath+'xls_scale.xls')
  MESSAGEBOX('Can not find the mapping file xls_scale.xls, get this file by importing the mapping SCALE excel file',0,'process will be aborted')
  RETURN
ENDIF 

SELECT 0
CD &lcStyleDataPath
llErr = .F.
*ON ERROR llErr = .T.
*IMPORT from xls_scale.xls TYPE xl5
IF llErr
  MESSAGEBOX('error while importing file xls_scale')
  RETURN 
ENDIF 
IF !FILE(lcStyleDataPath+'FldMapList.DBF')
  MESSAGEBOX('Can not find the field mapping file FldMapList',0,'process will be aborted')
  RETURN
ENDIF 

IF !FILE(lcStyleDataPath+'STY_EXC.xls')
  MESSAGEBOX('Can not find the STY_EXC file for the excluded style list with the shift applied')
  RETURN
ENDIF
SELECT 0
*IMPORT from sty_exc.xls TYPE xl5
IF llErr
  MESSAGEBOX('error while importing file xls_scale')
  RETURN
ENDIF
**T20140812.0010**

* Start of the program

*- Take a backup of BOM table 
lcTime=STRTRAN(TIME(),':')
lnEx =SQLEXEC( lnSqlConn , "SELECT * INTO bom_bk_&lcTime FROM bom")
 
*- Get the field list of the BOM file
lnEx =SQLEXEC( lnSqlConn , "SELECT * FROM BOM WHERE 1=2","BOM_1")
AFIELDS(laBOM)
lcFldList = ''
FOR i = 1 TO ALEN(laBOM,1)
  IF laBOM[i,1] = 'REC_NO'
    LOOP
  ENDIF
  lcFldList = lcFldList + '['+laBOM[i,1]+'],'
ENDFOR 
lcFldList = LEFT(lcFldList,LEN(lcFldList)-1)

USE STYLE
SCAN FOR LDETCOST = .F. AND  STYLE = IIF(EMPTY(lcStyParam),'',lcStyParam)
  lcSty = LEFT(STYLE.STYLE,19-3)+'***'
  WAIT WINDOW NOWAIT lcSty
  lnEx = SQLEXEC( lnSqlConn , "SELECT * FROM BOM WHERE citmmask = '&lcSty.' ","BOM_1")
  IF RECCOUNT('BOM_1')=0
    LOOP
  ENDIF 
  
  lcStyScl = STYLE.STYLE
  lnEx =SQLEXEC( lnSqlConn , "SELECT * FROM BOM WHERE citmmask = '&lcStyScl.' ","BOM_2")

  *- if this is not KK style then update the *** with the scale value provided that this line is not added, otherwise remove this line
  IF LEFT(style.scale,2) <> 'KK'
    SELECT BOM_1
    SCAN 
      lcRec_no = BOM_1.rec_no
      IF RECCOUNT('BOM_2') = 0
        lnEx =SQLEXEC( lnSqlConn , "UPDATE BOM SET citmmask = '&lcStyScl.' WHERE rec_no = '&lcRec_no' ")
      ELSE
        lnEx =SQLEXEC( lnSqlConn , "DELETE BOM WHERE rec_no = '&lcRec_no' ")
      ENDIF 
    ENDSCAN 
    
  ELSE
  
    *- the style is KK  
    SELECT BOM_1
    SCAN 
      lcRec_no = BOM_1.rec_no
      =lfAdd('KKA')
      =lfAdd('KKC')
      lnEx =SQLEXEC( lnSqlConn , "DELETE BOM WHERE rec_no = '&lcRec_no' ")
    ENDSCAN
    
    
  ENDIF
  *-check if there are lines with KK
ENDSCAN

************************************************************************************
*
*    FUNCTION lfAdd
*
************************************************************************************
FUNCTION lfAdd
PARAMETERS lcScl

SCATTER MEMVAR MEMO
lcItmmask = LEFT(BOM_1.citmmask,19-3)+lcScl
WAIT WINDOW NOWAIT lcItmmask
*T20140812.0010,4 TMI 08/24/2014 16:27 [Start] as per discussed with Mariam, when the customer edited the cost sheets the NLINENO 
*                                              was incremented to avoid 'Error while updating'
*                                              I have to check all fields, NLINENO not included, if this line found before then, don't add
*lcWhere = ;
    "[cinvtype]='"+BOM_1.cinvtype+"' and "+;
	"[citmmajor]='"+BOM_1.citmmajor+"' and "+;
	"[ccstshttyp]='"+BOM_1.ccstshttyp+"' and "+;
	"[ccstsht_id]='"+BOM_1.ccstsht_id+"' and "+;
	"[typ]='"+BOM_1.typ+"' and "+;
	"[citmmask]='"+lcItmmask+"' and "+;
	"[mfgcode]='"+BOM_1.mfgcode+"' and "+;
	"[cinvtypc]='"+BOM_1.cinvtypc+"' and "+;
	"[item]='"+BOM_1.item+"' and "+;
	"[nlineno]="+STR(BOM_1.nlineno,10)+" "
lcWhere = ;
    "[cinvtype]='"+BOM_1.cinvtype+"' and "+;
	"[citmmajor]='"+BOM_1.citmmajor+"' and "+;
	"[ccstshttyp]='"+BOM_1.ccstshttyp+"' and "+;
	"[ccstsht_id]='"+BOM_1.ccstsht_id+"' and "+;
	"[typ]='"+BOM_1.typ+"' and "+;
	"[citmmask]='"+lcItmmask+"' and "+;
	"[mfgcode]='"+BOM_1.mfgcode+"' and "+;
	"[cinvtypc]='"+BOM_1.cinvtypc+"' and "+;
	"[item]='"+BOM_1.item+"' "
*T20140812.0010,4 TMI 08/24/2014 16:29 [End  ] 
lnEx =SQLEXEC( lnSqlConn , "SELECT * FROM BOM WHERE "+lcWhere,"BOM_3")
IF RECCOUNT('BOM_3')=0
  lcValues = lfGetValues()
  lcSql = "INSERT INTO BOM ("+lcFldList+") SELECT "+ lcValues + " from bom where rec_no = '&lcRec_no.'"
  lnEx =SQLEXEC( lnSqlConn , lcSql)
ENDIF 
*- end of lfAdd

************************************************************************************
*
*    FUNCTION lfAdd
*
************************************************************************************
FUNCTION lfGetValues
LOCAL lcVal
lcVal = STRTRAN(lcFldList,"[CITMMASK]","'&lcItmmask' as [citmmask]")
RETURN lcVal
*- end of lfGetValues