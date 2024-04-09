*:***************************************************************************
*TMI 05/25/2009 [Start] 
* This program will update the BININVJL from the WHBINLOC file
* Sums the qty's in the former for a specific bin, compare it with the qty in the later , if there is a difference
* then add an adjustment line to the BININVJL so they both coincide 
*:***************************************************************************

lcShowMessage = ;
" This program will update the BININVJL from the WHBINLOC file "+;
" Sums the qty's in the former for a specific bin, compare it with the qty in the later , if there is a difference "+;
" then add an adjustment line to the BININVJL so they both coincide "

IF MESSAGEBOX(  lcShowMessage  ,4+256 , 'Data Fix')=7  && NO ( default = 'No' )
  RETURN
ENDIF

lcAria27Sysfolder = GETDIR('','Select the A27 System folder')
IF EMPTY(lcAria27Sysfolder)
  MESSAGEBOX('No folder selected',0,'Data Fix')
  RETURN
ENDIF
IF !FILE(lcAria27Sysfolder+'SYCCOMP.DBF')
  MESSAGEBOX('Wrong folder selected',0,'Data Fix')
  RETURN
ENDIF

llErr = .F.
ON ERROR llErr = .T.
USE (lcAria27Sysfolder+'SYCCOMP.DBF') IN 0 SHARED NOUPDATE
IF llErr = .T.
  MESSAGEBOX('Can not open the file syccomp',0,'Data Fix')
  RETURN 
ENDIF 
ON ERROR

WAIT WINDOW NOWAIT 'Select the company you need to update its bininvjl'
SELECT syccomp
LOCATE
BROWSE FIELDS CCOMP_ID,CCOM_NAME,CCOM_DDIR 
SCATTER MEMVAR MEMO

lcDataDir = ADDBS(ALLTRIM(SYCCOMP.CCOM_DDIR))

CLOSE ALL
SET EXCLUSIVE OFF
SET SAFETY OFF

lcCr = CHR(13)
lcQt = CHR(34)

*{SQL Server}
m.CCONSERVER = ALLTRIM(m.CCONSERVER )
*DATABASE
m.CCONDBNAME = ALLTRIM(m.CCONDBNAME )
*User ID
m.CCONUSERID = ALLTRIM(m.CCONUSERID )
*pwd
m.CCONPASWRD = ALLTRIM(m.CCONPASWRD )

*lnSqlConnection = -1
lcConnStr = "Driver={SQL Server};server="+ALLTRIM(m.CCONSERVER)+";DATABASE="+ALLTRIM(m.CCONDBNAME)+;
            ";uid="+ALLTRIM(m.CCONUSERID)+";pwd="+ALLTRIM(m.CCONPASWRD)
 
lnSqlConnection = SQLSTRINGCONNECT(lcConnStr)

IF lnSqlConnection<=0
  MESSAGEBOX('Error connection data')
  RETURN
ENDIF  

IF EMPTY(lcDataDir) OR !FILE(lcDataDir+'STYDYE.DBF')
  MESSAGEBOX('WRONG OR NO DATA FOLDER SELECTED')
  RETURN
ENDIF

*--Read the adjustment code reason to get the GL Account.
  SELECT 0
  USE (lcDataDir+'CODES') ORDER TAG CCODE_NO   && CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM
  =SEEK ('DCADJREASON')
  lcAdjCdRsn = CODES.CCODE_NO
  
  SET ORDER TO CODES   && CDEFCODE+CCODE_NO+CRLTFIELD+CFLD_NAME
  LOCATE
  =SEEK('N'+lcAdjCdRsn+'Y'+'CADJREASON')
  lcAdjAcct = PADR(CODES.CRLTD_VLU,24)
  
  SELECT 0
  USE (lcDataDir+'GL_LINK') ORDER 
  SET ORDER TO GL_LINK   && LINK_CODE+CATGKEY
  =SEEK('DEFDEF006')  
  lcCICACNT = GL_LINK.GLACNT

*- create error log file, failed styles to update will be added to this file
SELECT 0
Errlog = PROGRAM()
CREATE TABLE &Errlog (style c(19),cwarecode c(6),clocation c(10))

*- open needed files
USE (lcDataDir+'STYDYE') IN 0 ORDER STYDYE   
USE (lcDataDir+'SEQUENCE') IN 0 ORDER CSEQ_TYPE   && CSEQ_TYPE+CSEQ_GROUP
=SEEK('GLSESSION','SEQUENCE')


*- get the WHBINLOC lines
lnRun = SQLEXEC(lnSqlConnection , "SELECT * FROM WHBINLOC","WHBINLOC")
INDEX ON STYLE+CWARECODE+CLOCATION TAG WHBINLOC

*- get the BININVJL lines
lnRun = SQLEXEC(lnSqlConnection , "SELECT * FROM BININVJL","BININVJL")
INDEX ON STYLE+CWARECODE+CLOCATION TAG BININVJL

*- get thier sums
lcStySums = lfTempName()
SELECT BININVJL
LOCATE
TOTAL ON STYLE+CWARECODE+CLOCATION TO &lcStySums
SELECT 0
USE &lcStySums EXCLUSIVE
INDEX ON STYLE+CWARECODE+CLOCATION TAG STYLE

*- flag to check data is updated
llDataUpdated = .F.
*- The main loop
SELECT &lcStySums
SCAN   
  lcBin = &lcStySums..CLOCATION
  lcWh  = &lcStySums..CWARECODE
  lcSty = &lcStySums..STYLE
  
  WAIT WINDOW NOWAIT lcSty+lcWh+lcBin
  
  =SEEK(lcSty+lcWh+lcBin,'WHBINLOC')  
  
  IF &lcStySums..NTOTSTK <> WHBINLOC.TOTQTY  
      =SEEK(STYLE+CWARECODE,'STYDYE')

      lnQty1 = WHBINLOC.QTY1 - &lcStySums..NSTK1
      lnQty2 = WHBINLOC.QTY2 - &lcStySums..NSTK2
      lnQty3 = WHBINLOC.QTY3 - &lcStySums..NSTK3
      lnQty4 = WHBINLOC.QTY4 - &lcStySums..NSTK4
      lnQty5 = WHBINLOC.QTY5 - &lcStySums..NSTK5
      lnQty6 = WHBINLOC.QTY6 - &lcStySums..NSTK6
      lnQty7 = WHBINLOC.QTY7 - &lcStySums..NSTK7
      lnQty8 = WHBINLOC.QTY8 - &lcStySums..NSTK8
      lnTotQty = WHBINLOC.TOTQTY - &lcStySums..NTOTSTK
      
      lcCIRTYPE = IIF(lnTotQty>0,'R','I')
      
      lcSess = PADL(SEQUENCE.NSEQ_NO,6,'0')

      lcInsert = lfSqlStatement(lcSty,lcWh,lcBin,;
                                lnQty1,lnQty2,lnQty3,lnQty4,lnQty5,lnQty6,lnQty7,lnQty8,lnTotQty,;
                                lcCIRTYPE,lcSess)
      
      lnRun = SQLEXEC(lnSqlConnection,lcInsert)
      IF lnRun<0
        INSERT INTO &Errlog (style,cwarecode,clocation)value(lcsty,lcwh,lcbin)
      ELSE
        SELECT SEQUENCE
        REPLACE NSEQ_NO WITH NSEQ_NO+2
        llDataUpdated = .T.
      ENDIF
    ENDIF
  
ENDSCAN

lcMsg = IIF(llDataUpdated,"BININVJL updated ","All styles are Ok, no updates has been occured")
MESSAGEBOX(lcMsg,0,"Data Fix")

CLOSE ALL
ERASE (lcStySums+'.*')


*:**************************************************************************
*:* Name        : lfSqlStatement
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 10/28/2008
*:* Purpose     : Get the sql statement
*:***************************************************************************
FUNCTION lfSqlStatement
LPARAMETERS lcSty,lcWh,lcBin,lnQty1,lnQty2,lnQty3,lnQty4,lnQty5,lnQty6,lnQty7,lnQty8,lnTotQty,lcCIRTYPE,lcSess

lcInsert = "INSERT INTO BININVJL ("+;
           "CADD_USER ,"+lcCr+;
           "DADD_DATE ,"+lcCr+;
           "CADJACCT  ,"+lcCr+;
           "CADJREASON,"+lcCr+;
           "CICACNT   ,"+lcCr+;
           "CIRTYPE   ,"+lcCr+;
           "CLOCATION ,"+lcCr+;
           "CRSESSION ,"+lcCr+;
           "CSESSION  ,"+lcCr+;
           "CTRCODE   ,"+lcCr+;
           "CTRTYPE   ,"+lcCr+;
           "CWARECODE ,"+lcCr+;
           "DTRDATE   ,"+lcCr+;
           "NCOST     ,"+lcCr+;
           "NSTK1     ,"+lcCr+;
           "NSTK2     ,"+lcCr+;
           "NSTK3     ,"+lcCr+;
           "NSTK4     ,"+lcCr+;
           "NSTK5     ,"+lcCr+;
           "NSTK6     ,"+lcCr+;
           "NSTK7     ,"+lcCr+;
           "NSTK8     ,"+lcCr+;
           "NTOTSTK   ,"+lcCr+;
           "NSTKVAL   ,"+lcCr+;
           "REFERENCE ,"+lcCr+;
           "STYLE     ,"+lcCr+;
           "CADD_TIME ,"+lcCr+;
           "CADJREF  ,"+lcCr+;
           "CDYELOT ,"+lcCr+;
           "CEDIT_TIME ,"+lcCr+;
           "CEDIT_USER ,"+lcCr+;
           "CEDT_VER ,"+lcCr+;
           "CISESSION ,"+lcCr+;
           "CLOK_TIME ,"+lcCr+;
           "CLOK_USER ,"+lcCr+;
           "CLOTNO ,"+lcCr+;
           "COPRCODE ,"+lcCr+;
           "COWNER ,"+lcCr+;
           "DEDIT_DATE ,"+lcCr+;
           "DLOK_DATE ,"+lcCr+;
           CHR(34)+"LINENO"+CHR(34)+" ,"+lcCr+;
           "LLOCKFLG ,"+lcCr+;
           "LLOK_STAT ,"+lcCr+;
           "NPRVSQTY ,"+lcCr+;
           "NPRVSVAL ,"+lcCr+;
           "UI_PKEY )"+lcCr+;
           "VALUES ("+lcCr+;
           "'Ariafx/tmi',"+lcCr+;
           "'"+DTOS(DATE())+"'"+","+lcCr+;
           "'&lcAdjAcct'"+","+lcCr+;
           "'&lcAdjCdRsn'"+","+lcCr+;
           "'&lcCICACNT'"+","+lcCr+;
           "'&lcCIRTYPE'"+","+lcCr+;
           "'&lcBin'"+","+lcCr+;
           "'&lcSess'"+","+lcCr+;
           "'&lcSess'"+","+lcCr+;
           "'&lcSess'"+","+lcCr+;
           "'1'"+","+lcCr+;
           "'&lcWh'"+","+lcCr+;
           "'"+DTOS(DATE())+"'"+","+lcCr+;
           STR(STYDYE.AVE_COST,12,3)+","+lcCr+;
           STR(lnQTY1)+","+lcCr+;
           STR(lnQTY2)+","+lcCr+;
           STR(lnQTY3)+","+lcCr+;
           STR(lnQTY4)+","+lcCr+;
           STR(lnQTY5)+","+lcCr+;
           STR(lnQTY6)+","+lcCr+;
           STR(lnQTY7)+","+lcCr+;
           STR(lnQTY8)+","+lcCr+;
           STR(lnTOTQTY)+","+lcCr+;
           STR(lnTOTQTY*STYDYE.AVE_COST,12,3)+","+lcCr+;
           "'QACO - QUANTITY AMENDMENT CONV'"+","+lcCr+;
           "'&lcSty'," +;
           " '', "+;
           " '' , "+;
           " '' , "+;
           " '', "+;
           " '', "+;
           " '', "+;
           " '', "+;
           " '', "+;
           " '', "+;
           " '', "+;
           " '', "+;
           " 'AriaFix', "+;
           " '', "+;
           " '', "+;
           "  0, "+;
           "  0, "+;
           "  0, "+;
           "  0, "+;
           "  0, "+;
           "  '' )"
_CLIPTEXT = lcInsert             
RETURN lcInsert             
*-- end of lfSqlStatement.

*:**************************************************************************
*:* Name        : lfTempName
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 10/28/2008
*:* Purpose     : get temp name
*:***************************************************************************
FUNCTION lfTempName
RETURN 'x'+SUBSTR(SYS(2015),4)
*-- end of lfTempName.



