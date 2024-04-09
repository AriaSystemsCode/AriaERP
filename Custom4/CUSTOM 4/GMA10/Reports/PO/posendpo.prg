****************************************************************************
*: Program file      : POSENDPO.PRG (C#038209)
*: Program desc.     : Sending POs to Cargo System
*: System            : Aria Apparel System (ARIA4XP).
*: Module            : Style purchese order (PO)
*: Developer         : Mariam Mazhar (MMT)
*: Date              : 09/28/2004
****************************************************************************
*:Modifications:
*! B128014,1 MMT,05/17/2005 FIX bug of not updating user data in potosend file
*:**************************************************************************
**lntime=SECONDS()
*-- Get the E-mail address.
lcEmail = ALLTRIM(gfGetMemVar('M_CARGMAIL'))
IF EMPTY(lcEmail)
  *-- The user must enter the e-mail address.
  = gfModalGen("TRM00000B34000","DIALOG","Sending POs to Cargo System",.F.,"You have to enter the e-mail address. Cannot proceed.")
  RETURN
ENDIF

*--Collecting Data 
*-- The SQL files will retreive data from it 
lcPosFile = "PosHdr(INDEX=POSHDR),PosLn(INDEX=POSLN)" 
lcHdrFile = loOgScroll.gfTempName()
*--connction object creation
loSqlConnection = CREATEOBJECT('remotedataaccess')

*-- The initial selection condition
lcWhereCond = "PosLn.cStyType = PosHdr.cStyType AND  POSLN.CBUSDOCU = POSHDR.CBUSDOCU AND PosLn.Po=PosHdr.Po AND PosHdr.cStyType = 'P' AND POSHDR.CBUSDOCU = 'P' and POSLN.TranCD = '1'"

*-- The fields will be retreived from POTOSEND file
lcSelFldsPoToSnd =" PoToSend.cstytype,PoToSend.po,PoToSend.cstatus,PoToSend.type "
 *lcSelFldsPoToSnd = " PoToSend.* "
lcTablePo        = 'PoToSend'

IF lcRpSelect = "A" && If user select "Add/Modify/Cancel Option"
  *--selecting records have status N only
  lcWhereCondPo = " CSTATUS+CSTYTYPE+PO= 'N'"
  *-- selecting the required records from PoToSend file
  =lfOpenFox(lcSelFldsPoToSnd,lcTablePo,lcPoToSend,lcWhereCondPo)
  SELECT(lcPoToSend)
  LOCATE 
  IF !EOF()
    *--converting temp. PotToSend to SQL 
    lcCurName = lcPoToSend
    IF !EMPTY(lcCurName)
      SELECT &lcCurName    
      IF (RECCOUNT() > 0) 
        lcSQLOrder = loOgScroll.gfSQLTempName('','PO C(6),CSTYTYPE C(1)',lcCurName,'PO,CSTYTYPE')
        *--Updating the conditions of selection from PosLn and PosHdr files           
        lcPosFile = lcPosFile  + "," + lcSQLOrder 
        lcWhereCond = lcWhereCond + " AND  POSHDR.cStyType ="
        lcWhereCond = lcWhereCond +lcSQLOrder+ ".CSTYTYPE AND POSHDR.PO ="
        lcWhereCond = lcWhereCond +lcSQLOrder+".PO"
      ENDIF 
    ENDIF
  ELSE    
    *--The Temp. PoToSend File is empty so no records to send message will appear
    = gfModalGen("TRM00000B34000","DIALOG","Sending POs to Cargo System",.F.,"There are no more POs to send.")
    RETURN
  ENDIF
ELSE && If user select "Selected Option"
  *-- checking if user has selected POs or not  
  =lfCheckPOSelction()
ENDIF 
*--B127437,mmt,04/17/05,fix problem of files not opened using foxpro[Start]
lcPolines = loOgScroll.gfTempName()
lcPOheader = loOgScroll.gfTempName()
*--B127437,mmt,04/17/05,fix problem of files not opened using foxpro[End]
*--Creating the dbfs which will hold the data will be sent 
*--B127437,mmt,04/17/05,fix problem of files not opened using foxpro[Start]
CREATE TABLE (oAriaApplication.WorkDir+ lcPOheader +".DBF") ;
 (cPO C(6), cVendor C(8), cVendName C(30), cAddress1 C(30), cAddress2 C(30),;
  cAddress3 C(30), cAddress4 C(30), cAddress5 C(30), cAddress6 C(20),;
  cPhone C(16), cOrigin C(20), cFOB C(18), cShipVia C(30), dPODate D,;
  dCancel D ,WH_Date D, ACD C(1),;
  CFNLDSTDSC C(20))

*!*	CREATE TABLE (oAriaApplication.WorkDir+ "POHDR.DBF") ;
*!*	 (cPO C(6), cVendor C(8), cVendName C(30), cAddress1 C(30), cAddress2 C(30),;
*!*	  cAddress3 C(30), cAddress4 C(30), cAddress5 C(30), cAddress6 C(20),;
*!*	  cPhone C(16), cOrigin C(20), cFOB C(18), cShipVia C(30), dPODate D,;
*!*	  dCancel D ,WH_Date D, ACD C(1),;
*!*	  CFNLDSTDSC C(20))
*--B127437,mmt,04/17/05,fix problem of files not opened using foxpro[End]
IF !lluse_config   
*--B127437,mmt,04/17/05,fix problem of files not opened using foxpro[Start]
*--B127437,1 MMT 05/08/2005 ,add style group and description to poln file[Start]
*!*	  CREATE TABLE (oAriaApplication.WorkDir + lcPolines +".DBF") ;
*!*	   (cPO C(6), cItem C(12), cColor C(6), cSize C(5), nQty N(7),;
*!*	   cDesc C(80),LINENO N(6))
     CREATE TABLE (oAriaApplication.WorkDir + lcPolines +".DBF") ;
   (cPO C(6), cItem C(12), cColor C(6), cSize C(5), nQty N(7),;
   cDesc C(80),LINENO N(6), cstygroup C(6) ,cstygDec C(30))
*--B127437,1 MMT 05/08/2005 ,add style group and description to poln file[End]
*!*	  CREATE TABLE (oAriaApplication.WorkDir + "POLN.DBF") ;
*!*	   (cPO C(6), cItem C(12), cColor C(6), cSize C(5), nQty N(7),;
*!*	   cDesc C(80),LINENO N(6))
*--B127437,mmt,04/17/05,fix problem of files not opened using foxpro[End]
ELSE
*--B127437,mmt,04/17/05,fix problem of files not opened using foxpro[Start]
*--B127437,1 MMT 05/08/2005 ,add style group and description to poln file[Start]
*!*	  CREATE TABLE (oAriaApplication.WorkDir + lcPolines+".DBF") ;
*!*	   (cPO C(6), cItem C(12), cColor C(6),cConfig C(10), cSize C(5), nQty N(7),;
*!*	   cDesc C(80),LINENO N(6))
  CREATE TABLE (oAriaApplication.WorkDir + lcPolines+".DBF") ;
   (cPO C(6), cItem C(12), cColor C(6),cConfig C(10), cSize C(5), nQty N(7),;
   cDesc C(80),LINENO N(6), cstygroup C(6) ,cstygDec C(30))
*--B127437,1 MMT 05/08/2005 ,add style group and description to poln file[End]
*!*	  CREATE TABLE (oAriaApplication.WorkDir + "POLN.DBF") ;
*!*	   (cPO C(6), cItem C(12), cColor C(6),cConfig C(10), cSize C(5), nQty N(7),;
*!*	   cDesc C(80),LINENO N(6))
*--B127437,mmt,04/17/05,fix problem of files not opened using foxpro[End]

ENDIF 
*--B127437,mmt,04/17/05,fix problem of files not opened using foxpro[Start]
SELECT (lcPolines)
COPY TO  oAriaApplication.WorkDir + "POLN.DBF"  FOX2X 
IF !USED('POLN')
  USE oAriaApplication.WorkDir + "POLN.DBF" IN 0
ENDIF 
SELECT(lcPOheader )
COPY TO  oAriaApplication.WorkDir+ "POHDR.DBF"  FOX2X 
IF !USED('POHDR')
  USE oAriaApplication.WorkDir+ "POHDR.DBF" IN 0
ENDIF 
*--B127437,mmt,04/17/05,fix problem of files not opened using foxpro[End]
 *-- The Fields will be retrieved from PosHdr
 lcSelFldsHdr = "POSHDR.cStyType,POSHDR.CBUSDOCU,POSHDR.PO,POSHDR.VENDOR,POSHDR.SHIPVIA,"
 lcSelFldsHdr = lcSelFldsHdr + "POSHDR.ORIGIN,POSHDR.CFOB,POSHDR.ENTERED,POSHDR.COMPLETE,POSHDR.AVAILABLE,POSHDR.CFNLDST"

 *-- The Fields will be retreived from PosLn  
 lcSelFldsLine ="POSLN.STYLE,POSLN.QTY1,POSLN.CINVTYPE,"
 lcSelFldsLine = lcSelFldsLine  +"POSLN.QTY2,POSLN.QTY3,POSLN.QTY4,POSLN.QTY5,POSLN.QTY6"
 lcSelFldsLine = lcSelFldsLine  + ",POSLN.QTY7,POSLN.QTY8,[LINENO],POSLN.TRANCD "
 
 IF lluse_config 
   lcSelFldsLine = lcSelFldsLine  + ",Dyelot"
 ENDIF 
 *-- the fields will retrieve from poshdr and posln
 lcFldsPos = lcSelFldsHdr +","+lcSelFldsLine 

 *-- get the vendors of POs
 lcSelFldsPoVen = "Distinct ApVendor.CVENDCODE,ApVendor.lUseCargo,ApVendor.CVENCOMP,;
                   ApVendor.cAddress1,ApVendor.cAddress2,ApVendor.cAddress3,;
                   ApVendor.cAddress4,ApVendor.cAddress5,ApVendor.cAddress6,;
                   ApVendor.cPhoneNo"
lcTablePoVen   = "APVENDOR "
*,"+oariaapplication.workdir+lcHdrFile+".dbf " 
*lcTablePoVen   = "APVENDOR INNER JOIN  "+oariaapplication.workdir+lcHdrFile+".dbf on " +" APVENDOR.CVENDCODE ="+lcHdrFile+".VENDOR"
lcWhereCondVen = "APVENDOR.lUseCargo= .T. "
*+" AND "+" APVENDOR.CVENDCODE ="+lcHdrFile+".VENDOR"

*-- get The style data
*lcSelFldsStyle = "Distinct Style.Style,Style.Desc,Style.Desc1,style.scale,,Scale.cnt,scale.SZ1,;
                  scale.SZ2,scale.SZ3,scale.SZ4,scale.SZ5,scale.SZ6,scale.SZ7,scale.SZ8"
*--B127437,1 MMT 05/08/2005 ,add style group and description to poln file[Start]
*!*	lcSelFldsStyle = "Distinct Style.Style,Style.Desc,Style.Desc1,style.scale"
lcSelFldsStyle = "Distinct Style.Style,Style.Desc,Style.Desc1,style.scale,style.cstygroup"
*--B127437,1 MMT 05/08/2005 ,add style group and description to poln file[End]
lcTableStyle = "Style INNER JOIN '"+oAriaApplication.WorkDir+lcHdrFile +".dbf' on "+" Style.Style ="+lcHdrFile +".Style "
*INNER JOIN Scale ON Scale.Type+Scale.Scale+Scale.PrePak='S'+Style.Scale"
*lcTableStyle = "Style INNER JOIN "+oAriaApplication.WorkDir+lcHdrFile +".dbf on "+" Style.Style ="+lcHdrFile +".Style INNER JOIN Scale ON Scale.Type+Scale.Scale+Scale.PrePak='S'+Style.Scale"

lcSelFldsScale="scale.scale,Scale.cnt,scale.SZ1,;
                  scale.SZ2,scale.SZ3,scale.SZ4,scale.SZ5,scale.SZ6,scale.SZ7,scale.SZ8,type,prepak"
lcSclecond = "Scale.Type+Scale.Scale+Scale.PrePak='S'"
lcScaleTable = "Scale"
*-- If user selected the Add/Modify/Cancel option
IF lcRpSelect = "A"
  *-- If the remote data access successed in retreiving data from PosHdr
  IF lfOpenSql(lcFldsPos, lcPosFile, lcPosLn , lcWhereCond)
    SELECT(lcPosLn)
    SET RELATION TO 
    LOCATE 
    IF !EOF(lcPosLn)
      SELECT(lcPosLn)   
      *-- get the vendor information form the ApVendor file
      COPY TO oAriaApplication.WorkDir+lcHdrFile+".dbf"
      =lfOpenFox(lcSelFldsPoVen,lcTablePoVen,lcVendors,lcWhereCondVen)
      *--in case of the correct excusion of the select statment
      *-- gets the style information from style files
     * SELECT(lcPosLn) 
    *  COPY TO oariaapplication.workdir+lcDetFile+".dbf"
      =lfOpenFox(lcSelFldsStyle,lcTableStyle,lcStyles,"")  
      =lfOpenFox(lcSelFldsScale  ,lcScaleTable ,lcScales,lcSclecond )   
      SELECT(lcPosLn)
      SET RELATION TO 
      LOCATE
    ENDIF   
  ENDIF 
  
  SELECT(lcPoToSend)
  SET ORDER TO TAG PoToSend
  LOCATE
  SCAN 
    *-- Add record to the header and details files.
    IF SEEK('P'+EVALUATE(lcPoToSend+'.cStyType')+EVALUATE(lcPoToSend+'.PO'),lcPosLn)
      IF SEEK(EVALUATE(lcPosLn+'.VENDOR'),lcVendors)
 **       IF EVALUATE(lcVendors+'.LUSECARGO')
          *-- Updating the files will be sent
          =lfAddRec()
          *--updating the PoToSend file
          SELECT PoToSend
          SET ORDER TO TAG PoToSend
          IF SEEK(EVALUATE(lcPoToSend+'.cStyType')+EVALUATE(lcPoToSend+'.PO'))
            REPLACE cStatus WITH "S" 
            *-- Stamp the user information.
            *!B128014,1 MMT fix bug of not updating user information  [Start]
            =lfUserAdd_Info('PoToSend')          
            *!B128014,1 MMT fix bug of not updating user information  [End]
          ENDIF 
   **     ENDIF
      ENDIF
    ENDIF
  ENDSCAN
  SELECT PoToSend
  SET ORDER TO TAG POSTATUS
ELSE
  SELECT(lcPoToSend)
  SET ORDER TO TAG POTOSEND
  *-- To check if there are any selected POs
  lcPOFile = ''
  lnPosition = ASUBSCRIPT(loOgScroll.laOGFxFlt,ASCAN(loOgScroll.laOGFxFlt,'POSHDR.PO'),1)
  IF lnPosition > 0
    lcPOFile = loOgScroll.laOGFxFlt[lnPosition,6]
    IF !EMPTY(lcPOFile) AND USED(lcPOFile)
      SELECT (lcPOFile)
      LOCATE 
      IF !EOF()
        lcCurName = lcPOFile
        IF !EMPTY(lcCurName)
          SELECT &lcCurName    
          IF (RECCOUNT() > 0) 
            lcSQLSelOrder = loOgScroll.gfSQLTempName('','PO C(6)',lcCurName,'PO')
          ENDIF 
        ENDIF
        lcPosFile = lcPosFile + "," + lcSQLSelOrder
        lcWhereCond = lcWhereCond + " AND POSHDR.PO ="+lcSQLSelOrder+".PO "
      ENDIF
      IF lfOpenSql(lcFldsPos, lcPosFile, lcPosLn , lcWhereCond)
        SELECT( lcPosLn )
        SET RELATION TO 
        LOCATE 
        IF !EOF(lcPosLn)
          *--Get the Vendor information
          COPY TO oariaapplication.workdir+lcHdrFile+".dbf"
          lcTablePoVen   = "APVENDOR INNER JOIN  '"+oariaapplication.workdir+lcHdrFile+".dbf' on " +" APVENDOR.CVENDCODE ="+lcHdrFile+".VENDOR"
          =lfOpenFox(lcSelFldsPoVen,lcTablePoVen,lcVendors,lcWhereCondVen)
          *--get the style information        
      *    SELECT(lcPosLn)
       *   COPY TO oariaapplication.workdir+lcDetFile+".dbf"
          =lfOpenFox(lcSelFldsStyle,lcTableStyle,lcStyles,"")   
          =lfOpenFox(lcSelFldsScale  ,lcScaleTable ,lcScales,lcSclecond )   
          SELECT(lcPosLn)
          SET RELATION TO 
          LOCATE 
          SELECT(lcPOFile)
          LOCATE 
        ENDIF   
      ENDIF 
    ENDIF  
  ENDIF
  IF !EMPTY(lcPOFile) .AND. USED(lcPOFile) .AND. !EOF(lcPOFile)
    SELECT(lcPoToSend)
    SET ORDER TO TAG POTOSEND
    SELECT (lcPOFile)
    SCAN
      IF SEEK ("P"+PO,lcPoToSend)
        IF SEEK('P'+EVALUATE(lcPoToSend+'.cStyType')+EVALUATE(lcPoToSend+'.PO'),lcPosLn)
          IF SEEK(EVALUATE(lcPosLn+'.VENDOR'),lcVendors)
**            IF EVALUATE(lcVendors + '.LUSECARGO')
              *-- Add record to the header and details files.
              =lfAddRec()
              SELECT PoToSend
              SET ORDER TO TAG PoToSend
              IF SEEK(EVALUATE(lcPoToSend+'.cStyType')+EVALUATE(lcPoToSend+'.PO'))
                REPLACE cStatus WITH "S"        
                *-- Stamp the user information.
                =lfUserAdd_Info('PoToSend')          
              ENDIF 
  **          ENDIF
          ENDIF
        ENDIF
      ENDIF  
    ENDSCAN
  ELSE
  *********************************************
*!*      SELECT(lcPoToSend)
*!*      LOCATE 
*!*      IF !EOF()
*!*        *--Converting temp. PotToSend to SQL 
*!*        lcCurName = lcPoToSend
*!*        IF !EMPTY(lcCurName)
*!*          SELECT &lcCurName    
*!*          IF (RECCOUNT() > 0) 
*!*            lcSQLOrder = loOgScroll.gfSQLTempName('','PO C(6),CSTYTYPE C(1)',lcCurName,'PO,CSTYTYPE')
*!*            *--Updating the conditions of selection from PosLn and PosHdr files           
*!*            lcPosFile = lcPosFile  + "," + lcSQLOrder 
*!*            lcWhereCond = lcWhereCond + " AND  POSHDR.cStyType ="
*!*            lcWhereCond = lcWhereCond +lcSQLOrder+ ".CSTYTYPE AND POSHDR.PO ="
*!*            lcWhereCond = lcWhereCond +lcSQLOrder+".PO"
*!*          ENDIF 
*!*        ENDIF
*!*      ENDIF 
*!*      *-- If the remote data access successed in retreiving data
    IF  lfOpenSql(lcFldsPos, lcPosFile, lcPosLn , lcWhereCond)
      SELECT(lcPosLn)
      SET RELATION TO 
      LOCATE 
      IF !EOF(lcPosLn)
        SELECT &lcPosLn..* from &lcPosLn,&lcpotosend WHERE &lcPosLn..po=&lcpotosend..po INTO CURSOR &lcHdrFile
        SELECT(lcHdrFile)
        COPY TO oariaapplication.workdir+lcHdrFile+".dbf"
        =lfOpenFox(lcSelFldsPoVen,lcTablePoVen,lcVendors,lcWhereCondVen)
        *-- get the style information
*!*          SELECT(lcPosLn)
*!*          COPY TO oariaapplication.workdir+lcDetFile+".dbf"
         =lfOpenFox(lcSelFldsStyle,lcTableStyle,lcStyles,"")
         =lfOpenFox(lcSelFldsScale  ,lcScaleTable ,lcScales,lcSclecond )   
         SELECT(lcPosLn)
         SET RELATION TO 
         LOCATE 
       ENDIF   
     ENDIF 
    SELECT(lcPoToSend)
    SCAN 
      IF SEEK('P'+EVALUATE(lcPoToSend+'.cStyType')+EVALUATE(lcPoToSend+'.PO'),lcPosLn)
        IF SEEK(EVALUATE(lcPosLn+'.VENDOR'),lcVendors)
**          IF EVALUATE(lcVendors+'.LUSECARGO')
            *-- Add record to the header and details files.
            =lfAddRec()
            SELECT PoToSend
            SET ORDER TO TAG PoToSend
            IF SEEK(EVALUATE(lcPoToSend+'.cStyType')+EVALUATE(lcPoToSend+'.PO'))
              REPLACE cStatus WITH "S"        
              *-- Stamp the user information.
              =lfUserAdd_Info('PoToSend')          
            ENDIF 
  **        ENDIF
        ENDIF
      ENDIF      
    ENDSCAN
  ENDIF  
ENDIF

SELECT POHDR
LOCATE 

SELECT POLN
LOCATE 
*--removing the object that is created from remote data access class
loSqlConnection = NULL

IF EOF("POHDR") OR EOF("POLN")
  = gfModalGen("TRM00000B34000","DIALOG","Sending POs to Cargo System",.F.,"No records have been selected to send.")
  IF FILE(oAriaApplication.WorkDir +"POHDR.DBF")
    USE IN PoHdr
    ERASE oAriaApplication.WorkDir+"POHDR.DBF"
  ENDIF

  IF FILE(oAriaApplication.WorkDir+ "POLN.DBF")
    USE IN PoLn
    ERASE oAriaApplication.WorkDir+ "POLN.DBF"
  ENDIF
  ERASE   oariaapplication.workdir+lcHdrFile+".dbf"
  RETURN
ENDIF
ERASE   oariaapplication.workdir+lcHdrFile+".dbf"

SELECT POHDR
USE 

SELECT POLN
USE 

**WAIT WINDOW "time "+STR(SECONDS()-lntime,10,5)
lcSubject = "GMA PO File"
lcFile1   = oAriaApplication.WorkDir + "POHDR.DBF"
lcFile2   = oAriaApplication.WorkDir + "POLN.DBF"
lcZipFile = oAriaApplication.WorkDir + "GMA_PO.ZIP"
lcPath    = oAriaApplication.ApplicationHome + "POSend~1.EXE"

run /n &lcPath &lcEmail,&lcSubject,,&lcZipFile,&lcFile1;&lcFile2



*!*************************************************************
*! Name      : lfAddRec
*! Developer : Mariam Mazhar (MMT)
*! Date      : 09/28/2004
*! Purpose   : To add record to Tmp Files.
*!*************************************************************
*! Example     : = lfAddRec()
*!*************************************************************
FUNCTION lfAddRec

PRIVATE lnAlias
lnAlias = SELECT(0)
SELECT(lcPosln)
lcShipDesc  = gfCodDes(ShipVia,'SHIPVIA   ')
lcPoNo      = &lcPosln..PO
lcPOBusdocu = &lcPosln..Cbusdocu
lcPostytype = &lcPosln..Cstytype

INSERT INTO (oAriaApplication.WorkDir + "POHDR") ;
(cPO,cVendor,cVendName,cAddress1,cAddress2,cAddress3,cAddress4,;
cAddress5,cAddress6,cPhone,cOrigin,cFOB,cShipVia,dPODate,;
dCancel,WH_Date,ACD,;
CFNLDSTDSC) VALUES;
(EVALUATE(lcPosln+'.PO'),EVALUATE(lcPosln+'.Vendor'),EVALUATE(lcVendors+'.CVENCOMP'),;
 EVALUATE(lcVendors+'.cAddress1'),EVALUATE(lcVendors+'.cAddress2'),EVALUATE(lcVendors+'.cAddress3'),;
 EVALUATE(lcVendors+'.cAddress4'),EVALUATE(lcVendors+'.cAddress5'),EVALUATE(lcVendors+'.cAddress6'),;
 EVALUATE(lcVendors+'.cPhoneNo'),EVALUATE(lcPosln+'.ORIGIN'),EVALUATE(lcPosln+'.CFOB'),lcShipDesc,;
 EVALUATE(lcPosln+'.Entered'),EVALUATE(lcPosln+'.Complete'),;
 EVALUATE(lcPosln+'.Available'),EVALUATE(lcPoToSend+'.type'),;
 PADR(gfCodDes(EVALUATE(lcPosln+'.CFNLDST'),'CFNLDST'),20) )
      
SELECT(lcPosLn)
**=SEEK(EVALUATE(lcPosln+'.cBusDocu')+EVALUATE(lcPosln+'.cStyType')+EVALUATE(lcPosln+'.PO'))
*SCAN REST WHILE cBusDocu+cstytype+po+style+STR(lineno,6)+trancd = ;
                EVALUATE(lcPosln+'.cBusDocu')+EVALUATE(lcPosln+'.cStyType')+;
                EVALUATE(lcPosln+'.PO') 
SCAN REST WHILE cBusDocu+cstytype+po = lcPOBusdocu+lcPostytype+lcPoNo

*SCAN REST WHILE cBusDocu+cstytype+po+style+STR(lineno,6)+trancd = ;
                lcPOBusdocu+lcPostytype+lcPoNo

                *      FOR TRANCD = '1'
          
  *-- Gets the style information from Style and Scale files          
* =SEEK(Style,lcStyles)
 IF !SEEK(Style,lcStyles)
   SELECT(lcStyles)
   GO BOTTOM 
 ENDIF 
  =SEEK('S'+&lcStyles..Scale,lcScales)
   FOR lnCounter = 1 TO EVALUATE(lcScales+'.Cnt')
     lcCounter = STR(lnCounter,1)            
     IF &lcPosln..Qty&lcCounter > 0
       IF lluse_config           
*--B127437,1 MMT 05/08/2005 ,add style group and description to poln file[Start]
*!*	         INSERT INTO (oAriaApplication.WorkDir + "POLN") ;
*!*	         (cPO,cItem,cColor,cConfig,cSize,nQty,cDesc,Lineno) VALUES ;
*!*	         (&lcPOSLN..PO,SUBSTR(EVALUATE(lcPOSLN+'.Style'),1,12),ALLTRIM(RIGHT(EVALUATE(lcPOSLN+'.Style'),6)),;
*!*	         &lcPosLn..Dyelot,&lcScales..Sz&lcCounter,&lcPosLn..Qty&lcCounter,&lcStyles..Desc+'-'+&lcStyles..Desc1,&lcPosLn..Lineno)
         INSERT INTO (oAriaApplication.WorkDir + "POLN") ;
         (cPO,cItem,cColor,cConfig,cSize,nQty,cDesc,Lineno,cstygroup,cstygDec) VALUES ;
         (&lcPOSLN..PO,SUBSTR(EVALUATE(lcPOSLN+'.Style'),1,12),ALLTRIM(RIGHT(EVALUATE(lcPOSLN+'.Style'),6)),;
         &lcPosLn..Dyelot,&lcScales..Sz&lcCounter,&lcPosLn..Qty&lcCounter,&lcStyles..Desc+'-'+&lcStyles..Desc1,&lcPosLn..Lineno,;
         &lcStyles..cstygroup,gfCodDes(&lcStyles..cstygroup,'CSTYGROUP '))
*--B127437,1 MMT 05/08/2005 ,add style group and description to poln file[End]
       ELSE
*--B127437,1 MMT 05/08/2005 ,add style group and description to poln file[Start]
*!*	         INSERT INTO (oAriaApplication.WorkDir + "POLN") ;
*!*	         (cPO,cItem,cColor,cSize,nQty,cDesc,Lineno) VALUES ;
*!*	         (&lcPOSLN..PO,SUBSTR(EVALUATE(lcPOSLN+'.Style'),1,12),ALLTRIM(RIGHT(EVALUATE(lcPOSLN+'.Style'),6)),;
*!*	         &lcScales..Sz&lcCounter,&lcPosLn..Qty&lcCounter,&lcStyles..Desc+'-'+&lcStyles..Desc1,&lcPosLn..Lineno)
         INSERT INTO (oAriaApplication.WorkDir + "POLN") ;
         (cPO,cItem,cColor,cSize,nQty,cDesc,Lineno,cstygroup,cstygDec) VALUES ;
         (&lcPOSLN..PO,SUBSTR(EVALUATE(lcPOSLN+'.Style'),1,12),ALLTRIM(RIGHT(EVALUATE(lcPOSLN+'.Style'),6)),;
         &lcScales..Sz&lcCounter,&lcPosLn..Qty&lcCounter,&lcStyles..Desc+'-'+&lcStyles..Desc1,&lcPosLn..Lineno,;
         &lcStyles..cstygroup,gfCodDes(&lcStyles..cstygroup,'CSTYGROUP '))
*--B127437,1 MMT 05/08/2005 ,add style group and description to poln file[End]
       ENDIF   
     ENDIF
  ENDFOR
ENDSCAN               
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Mariam Mazhar (MMT)
*! Date      : 09/28/2004 
*! Purpose   : When function of OG.
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************
FUNCTION lfwRepWhen
*--Disabling The print and send email buttons
loogscroll.parent.ogToolBar.cntExternal.cmdEmail.Enabled = .F.
loogscroll.parent.ogtoolbar.cntPrint.cmdprint.enabled = .F.
loogscroll.parent.ogtoolbar.cntPrint.cmdexport.enabled = .F.
*!*************************************************************
*! Name      : lfvSelPos
*! Developer : Mariam Mazhar (MMT)
*! Date      : 09/28/2004 
*! Purpose   : Valid function of Select in OG.
*!*************************************************************
*! Example     : = lfvSelPos()
*!*************************************************************
Function lfvSelPos
*--Clearing filters in case of changing the selection filter
CLEARREAD()

*!*************************************************************
*! Name      : lfOpenSql
*: Developer : Mariam Mazhar (MMT)
*: Date      : 09/28/2004
*! Purpose   : function to open SQL tables
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfOpenSql

LPARAMETERS lcSelFlds,lcTable,lcCursor,lcWhereCond,llIsInitial
LOCAL lnConnectionHandlar, lnBuffering, lcSqlStatment , loSqlConnection
PRIVATE laIndex
DIMENSION laIndex[1,2]

lcSqlStatment   = "SELECT  " + lcSelFlds + "  FROM " + lcTable + IIF(TYPE('lcWhereCond') = 'C' AND !EMPTY(lcWhereCond)," WHERE " + lcWhereCond ,"")

lnConnectionHandlar = loOGScroll.oRDA.sqlrun(lcSqlStatment,lcCursor,lcTable,oAriaApplication.ActiveCompanyConStr,3,;
                                      'BROWSE',SET("DATASESSION"))

IF lnConnectionHandlar = 1
  lnBuffering = CURSORGETPROP("Buffering",lcCursor)
  =CURSORSETPROP("Buffering",3,lcCursor)
  *-- To initialize the indecis that will be created for each file
  =lfCrtindex(lcCursor)
  SELECT (lcCursor)
  FOR lnI = 1 TO ALEN(laIndex,1)
    lcIndex = laIndex[lnI,1]
    lcTag   = laIndex[lnI,2]
    INDEX ON &lcIndex. TAG (lcTag) &&OF (lcCursor)
  ENDFOR
  lcTag = laIndex[1,2]
  SET ORDER TO TAG (lcTag)

ELSE
  =loOGScroll.oRDA.CheckRetResult("sqlrun",lnConnectionHandlar,.T.)
  RETURN .F.
ENDIF
*-- end of lfOpenSql.

*!*************************************************************
*! Name      : lfCrtindex
*: Developer : Mariam Mazhar (MMT)
*: Date      : 09/28/2004
*! Purpose   : function to Set the index for the SQL files
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfCrtindex

LPARAMETERS lcTable
DO CASE
  CASE UPPER(lcTable) = lcPosHdr
    DIMENSION laIndex[1,2]
    laIndex[1,1] = 'CBUSDOCU+CSTYTYPE+PO'
    laIndex[1,2] = lcPosHdr
    
  CASE UPPER(lcTable) = lcPosLn 
    DIMENSION laIndex[1,2]
*    laIndex[1,1] = 'CBUSDOCU+CSTYTYPE+PO+CINVTYPE+STYLE+STR(LINENO,6)+TRANCD'
    laIndex[1,1] = 'CBUSDOCU+CSTYTYPE+PO'
    laIndex[1,2] = lcPosLn 
                                                             
  CASE UPPER(lcTable) = lcPoToSend
    DIMENSION laIndex[2,2]
    laIndex[1,1] = 'CSTYTYPE+PO+CSTATUS'
    laIndex[1,2] = 'POTOSEND'
    laIndex[2,1] = 'CSTATUS+CSTYTYPE+PO '
    laIndex[2,2] = 'POSTATUS'
    
  CASE UPPER(lcTable) = lcVendors    
    DIMENSION laIndex[1,2]
    laIndex[1,1] = 'CVENDCODE'
    laIndex[1,2] = 'VENCODE'

  CASE UPPER(lcTable) = lcStyles   
    DIMENSION laIndex[1,2]
    laIndex[1,1] = 'Style'
    laIndex[1,2] = 'Style'

  CASE UPPER(lcTable) = lcScales     
    DIMENSION laIndex[1,2]
    laIndex[1,1] = 'type+scale+prepak'
    laIndex[1,2] = 'Scale'
ENDCASE

*!*************************************************************
*! Name      : lfOpenFox
*: Developer : Mariam Mazhar (MMT)
*: Date      : 09/28/2004
*! Purpose   : function to open FOX tables
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfOpenFox

LPARAMETERS lcSelFlds,lcTable,lcCursor,lcWhereCond,llIsInitial
LOCAL lnConnectionHandlar, lnBuffering, lcSqlStatment , loSqlConnection
PRIVATE laIndex
DIMENSION laIndex[1,2]

lcSqlStatment   = "SELECT  " + lcSelFlds + "  FROM " + lcTable + IIF(TYPE('lcWhereCond') = 'C' AND !EMPTY(lcWhereCond)," WHERE " + lcWhereCond ,"")

IF TYPE('loSqlConnection') <> 'O'
  loSqlConnection = CREATEOBJECT('remotedataaccess')
ENDIF 
lnConnectionHandlar = loSqlConnection.sqlrun(lcSqlStatment,lcCursor,lcTable,oAriaApplication.cAriaNativeDataFilesConStr,3,;
                                      'SAVE',SET("DATASESSION"))
*lnConnectionHandlar = loOGScroll.oRDA.sqlrun(lcSqlStatment,lcCursor,lcTable,;
                                             oAriaApplication.cAriaNativeDataFilesConStr,3,;
                                            'BROWSE',SET("DATASESSION"))

IF lnConnectionHandlar = 1
  lnBuffering = CURSORGETPROP("Buffering",lcCursor)
  =CURSORSETPROP("Buffering",3,lcCursor)
  *-- To initialize the indecis that will be created for each file
  =lfCrtindex(lcCursor)
  SELECT (lcCursor)
  FOR lnI = 1 TO ALEN(laIndex,1)
    lcIndex = laIndex[lnI,1]
    lcTag   = laIndex[lnI,2]
    INDEX ON &lcIndex. TAG (lcTag) &&OF (lcCursor)
  ENDFOR
  lcTag = laIndex[1,2]
  SET ORDER TO TAG (lcTag)
  =CURSORSETPROP("Buffering",5,lcCursor)
ELSE
  =loSqlConnection.CheckRetResult("sqlrun",lnConnectionHandlar,.T.)
*  =loOGScroll.oRDA.CheckRetResult("sqlrun",lnConnectionHandlar,.T.)
  RETURN .F.
ENDIF
*-- end of lfOpenSql.
*!*************************************************************
*! Name      : lfCheckPOSelction
*: Developer : Mariam Mazhar (MMT)
*: Date      : 10/03/2004
*! Purpose   : function to check if user select POs or not
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfCheckPOSelction
lcWhereCondPo = " "
lcSlctPo = ''
lcSelFldsPoToSnd = " PoToSend.cstytype,PoToSend.po,PoToSend.cstatus,PoToSend.type "
lcTablePo        = 'POTOSEND'
lcPoSendFile = loOgScroll.gfTempName()
lnPos= ASUBSCRIPT(loOgScroll.laOGFxFlt,ASCAN(loOgScroll.laOGFxFlt,'POSHDR.PO'),1)
IF lnPos > 0
  lcSlctPo = loOgScroll.laOGFxFlt[lnPos,6]
  IF !EMPTY(lcSlctPo) AND USED(lcSlctPo)
    SELECT (lcSlctPo)
    LOCATE 
    IF !EOF() 
      lcWhereCondPo    = " "
      lcExpPOSlect     = " "
      SELECT(lcSlctPo)
      COPY TO oAriaApplication.workdir+lcPoSendFile+".dbf"
      lcTablePo = lcTablePo + " INNER JOIN '"+oAriaApplication.workdir+lcPoSendFile+".dbf'"+" ON "+"POTOSEND.CSTYTYPE+POTOSEND.PO+POTOSEND.CSTATUS='P'+"+lcPoSendFile+".PO"
      =lfOpenFox(lcSelFldsPoToSnd,lcTablePo,lcPoToSend,lcWhereCondPo)
      ERASE oAriaApplication.workdir+lcPoSendFile+".dbf"
    ELSE 
      =lfOpenFox(lcSelFldsPoToSnd,lcTablePo,lcPoToSend,lcWhereCondPo)
    ENDIF 
  ELSE 
    =lfOpenFox(lcSelFldsPoToSnd,lcTablePo,lcPoToSend,lcWhereCondPo)
  ENDIF  
ENDIF   
*!*************************************************************
*! Name      : lfUpdateFox
*: Developer : Mariam Mazhar (MMT)
*: Date      : 09/28/2004
*! Purpose   : function to Update FOX tables
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION  lfUpdateFox
PARAMETERS lcTable,lcIndex

IF TYPE('loSqlConnection') <> 'O'
  loSqlConnection = CREATEOBJECT('remotedataaccess')
ENDIF 

lcTranCode = loSqlConnection.BeginTran(oAriaApplication.cAriaNativeDataFilesConStr,3,'',.T.)
IF TYPE('lcTranCode') = 'N'
  =loSqlConnection.CheckRetResult("BEGINTRAN",lcTranCode,.T.)
ELSE 
  lnResult = loSqlConnection.SqlUpdate(lcTable, lcTranCode,SET("DATASESSION"),lcIndex,'PoToSend')
  IF lnResult <=0
    =loSqlConnection.CheckRetResult("SQLUPDATE",lnResult,.T.)
    =loSqlConnection.RollBackTran(lcTranCode)
  ELSE
    IF loSqlConnection.CommitTran(lcTranCode,.T.) # 1
      =loSqlConnection.CheckRetResult("COMMITTRAN",lnResult,.T.)
    ENDIF 
  ENDIF
ENDIF
*!*************************************************************
*! Name      : lfUserAdd_Info
*: Developer : Mariam Mazhar (MMT)
*: Date      : 05/17/2005
*! Purpose   : function to Update user data fields
*!*************************************************************
*! Parameters: file name
*!*************************************************************
*! Returns   : None
*!*************************************************************
*B127018
FUNCTION lfUserAdd_Info
LPARAMETERS lcFileName
PRIVATE    lcFileName,lcSavAlias, lnOldDataSession

lnOldDataSession = SET ("DATASESSION")
lcSavAlias = SELECT(0)

IF !(TYPE('lcFileName') $ "UL")
  SELECT (lcFileName)
ENDIF  
lcFileName = ALIAS()

LOCAL llEdtFld,llAddFld
llAddFld = (TYPE(lcFileName+'.cAdd_user') <> 'U') AND ;
		   (TYPE(lcFileName+'.dAdd_Date') <> 'U') AND ;
      	   (TYPE(lcFileName+'.cAdd_Time') <> 'U') 

*-- New Record
IF llAddFld 
  *** stamp the record for this user with date and time
  REPLACE cAdd_User  WITH oariaapplication.user_id ,;
          dAdd_Date  WITH DATE()    ,;
          cAdd_Time  WITH gfGetTime()
ENDIF
SET DATASESSION TO lnOldDataSession
SELECT (lcSavAlias)
