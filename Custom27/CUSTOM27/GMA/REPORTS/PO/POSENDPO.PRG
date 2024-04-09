****************************************************************************
*: Program file      : POSENDPO.PRG (C#102087)
*: Program desc.     : Sending POs to Cargo System
*: System            : Aria Apparel System (A27).
*: Module            : Style purchese order (PO)
*: Developer         : Ahmed Maher (AMH)
*: Date              : 12/31/2000
*:**************************************************************************
*:Modifications  :
*:B604314,1 KHM 03/25/2001 Fix the bug of not stamping the userid, data and time.
*:C200508,1 TMI 03/11/2003 Add the "Final Destination " description field
*:C200508,1 TMI            Add the posln.lineno field to the POLN file 
*:C200508,4 TMI 04/01/2003 Include only lines with Trancd='1'
*:C124187,1 MHM 11/16/2004 Add the Style Group code And Style Group Description fields to the POLN file 
*:**************************************************************************

*-- Check if the are new POs to send.
IF lcRpSelect = "A" AND !SEEK('N','PoToSend')
  *-- Display a message to inform the user that there are no more POs to send.
  = gfModalGen("TRM00000B34000","DIALOG","Sending POs to Cargo System",.F.,"There are no more POs to send.")
  RETURN
ENDIF

*-- Get the E-mail address.
lcEmail = ALLTRIM(gfGetMemVar('M_CARGMAIL'))

IF EMPTY(lcEmail)
  *-- The user must enter the e-mail address.
  = gfModalGen("TRM00000B34000","DIALOG","Sending POs to Cargo System",.F.,"You have to enter the e-mail address. Cannot proceed.")
  RETURN
ENDIF

*C200508,1 TMI [Start] Add the "Final Destination " description field
*CREATE TABLE (gcWorkdir + "POHDR.DBF") ;
 (cPO C(6), cVendor C(8), cVendName C(30), cAddress1 C(30), cAddress2 C(30),;
  cAddress3 C(30), cAddress4 C(30), cAddress5 C(30), cAddress6 C(20),;
  cPhone C(16), cOrigin C(20), cFOB C(18), cShipVia C(30), dPODate D,;
  dCancel D ,WH_Date D, ACD C(1))
CREATE TABLE (gcWorkdir + "POHDR.DBF") ;
 (cPO C(6), cVendor C(8), cVendName C(30), cAddress1 C(30), cAddress2 C(30),;
  cAddress3 C(30), cAddress4 C(30), cAddress5 C(30), cAddress6 C(20),;
  cPhone C(16), cOrigin C(20), cFOB C(18), cShipVia C(30), dPODate D,;
  dCancel D ,WH_Date D, ACD C(1),;
  CFNLDSTDSC C(20))
*C200508,1 TMI [End  ]     
   
*C200508,1 TMI [Start] Add the posln.lineno field to the POLN file 
*CREATE TABLE (gcWorkdir + "POLN.DBF") ;
 (cPO C(6), cItem C(12), cColor C(6), cSize C(5), nQty N(7),;
  cDesc C(80))

*:C124187,1 MHM 11/16/2004 Add the Style Group code And Style Group Description 
*:C124187,1 MHM 11/16/2004 fields to the POLN file [Start]
*CREATE TABLE (gcWorkdir + "POLN.DBF") ;
 (cPO C(6), cItem C(12), cColor C(6), cSize C(5), nQty N(7),;
  cDesc C(80),LINENO N(6))

CREATE TABLE (gcWorkdir + "POLN.DBF") ;
 (cPO C(6), cItem C(12), cColor C(6), cSize C(5), nQty N(7),;
  cDesc C(80),LINENO N(6) , cstygroup C(6) ,cstygDec C(30) )

*:C124187,1 MHM 11/16/2004 [End]

*C200508,1 TMI [End  ] 

IF lcRpSelect = "A"
  SELECT POTOSEND
  SET ORDER TO TAG POTOSEND
  LOCATE
  SCAN FOR cStatus = "N"
    *-- Add record to the header and details files.
    IF SEEK(PoToSend.cStyType+PoToSend.PO,'PosHdr')
      IF SEEK(POSHDR.VENDOR,'APVENDOR')
        IF APVENDOR.LUSECARGO
          =lfAddRec()
          SELECT PoToSend
          REPLACE cStatus WITH "S"
          *B604314,1 KHM 03/25/2001 (Begin) Stamp the user information.
          =gfAdd_Info('PoToSend')          
          *B604314,1 KHM 03/25/2001 (End)
        ENDIF
      ENDIF
    ENDIF
      
  ENDSCAN
  SET ORDER TO TAG POSTATUS
ELSE
  SELECT POTOSEND
  SET ORDER TO TAG POTOSEND
  *-- To check if there are any selected POs
  lcPOFile = ''
  lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'POSHDR.PO'),1)
  IF lnPosition > 0
    lcPOFile = laOGFxFlt[lnPosition,6]
    IF !EMPTY(lcPOFile) AND USED(lcPOFile)
      SELECT (lcPOFile)
      GOTO TOP
    ENDIF  
  ENDIF
  IF !EMPTY(lcPOFile) .AND. USED(lcPOFile) .AND. !EOF(lcPOFile)
    SELECT (lcPOFile)
    SCAN
      IF SEEK ("P"+PO,"PoToSend")
        IF SEEK(PoToSend.cStyType+PoToSend.PO,'PosHdr')
          IF SEEK(POSHDR.VENDOR,'APVENDOR')
            IF APVENDOR.LUSECARGO
              *-- Add record to the header and details files.
              =lfAddRec()
              SELECT PoToSend
              REPLACE cStatus WITH "S"        
              *B604314,1 KHM 03/25/2001 (Begin) Stamp the user information.
              =gfAdd_Info('PoToSend')          
              *B604314,1 KHM 03/25/2001 (End)
            ENDIF
          ENDIF
        ENDIF
      ENDIF  
    ENDSCAN
  ELSE
    SELECT PoToSend
    SCAN 
      IF SEEK(PoToSend.cStyType+PoToSend.PO,'PosHdr')
        IF SEEK(POSHDR.VENDOR,'APVENDOR')
          IF APVENDOR.LUSECARGO
            *-- Add record to the header and details files.
            =lfAddRec()
            SELECT PoToSend
            REPLACE cStatus WITH "S"        
            *B604314,1 KHM 03/25/2001 (Begin) Stamp the user information.
            =gfAdd_Info('PoToSend')          
            *B604314,1 KHM 03/25/2001 (End)
          ENDIF
        ENDIF
      ENDIF      
    ENDSCAN
  ENDIF  
ENDIF

SELECT POHDR
GOTO TOP 
SELECT POLN
GOTO TOP
IF EOF("POHDR") OR EOF("POLN")
  = gfModalGen("TRM00000B34000","DIALOG","Sending POs to Cargo System",.F.,"No records have been selected to send.")
  IF FILE(gcWorkdir +"POHDR.DBF")
    USE IN PoHdr
    ERASE gcWorkdir +"POHDR.DBF"
  ENDIF

  IF FILE(gcWorkdir + "POLN.DBF")
    USE IN PoLn
    ERASE gcWorkdir + "POLN.DBF"
  ENDIF
  RETURN
ENDIF


SELECT POHDR
USE 
SELECT POLN
USE 

lcSubject = "GMA PO File"
lcFile1   = gcWorkdir + "POHDR.DBF"
lcFile2   = gcWorkdir + "POLN.DBF"
lcZipFile = gcWorkdir + "GMA_PO.ZIP"
lcPath    = gcapphome+"POSend~1.EXE"

run /n &lcPath &lcEmail,&lcSubject,,&lcZipFile,&lcFile1;&lcFile2



*!*************************************************************
*! Name      : lfAddRec
*! Developer : Ahmed Maher (AMH)
*! Date      : 12/31/2000 
*! Purpose   : To add record to Tmp Files.
*!*************************************************************
*! Example     : = lfAddRec()
*!*************************************************************
FUNCTION lfAddRec

PRIVATE lnAlias
lnAlias = SELECT(0)
SELECT PosHdr
lcShipDesc = gfCodDes(ShipVia,'SHIPVIA   ')
        
*C200508,1 TMI [Start] Add the field "Final Destination"
*- INSERT INTO (gcWorkdir + "POHDR") ;
*- (cPO,cVendor,cVendName,cAddress1,cAddress2,cAddress3,cAddress4,;
*- cAddress5,cAddress6,cPhone,cOrigin,cFOB,cShipVia,dPODate,;
*- dCancel,WH_Date,ACD) VALUES;
*- (POSHDR.PO,POSHDR.Vendor,ApVendor.CVENCOMP,ApVendor.cAddress1,ApVendor.cAddress2,ApVendor.cAddress3,;
*- ApVendor.cAddress4,ApVendor.cAddress5,ApVendor.cAddress6,;
*- ApVendor.cPhoneNo,POSHDR.ORIGIN,POSHDR.CFOB,lcShipDesc,POSHDR.Entered,POSHDR.Complete,;
*- POSHDR.Available,PoToSend.type)
INSERT INTO (gcWorkdir + "POHDR") ;
(cPO,cVendor,cVendName,cAddress1,cAddress2,cAddress3,cAddress4,;
cAddress5,cAddress6,cPhone,cOrigin,cFOB,cShipVia,dPODate,;
dCancel,WH_Date,ACD,;
CFNLDSTDSC) VALUES;
(POSHDR.PO,POSHDR.Vendor,ApVendor.CVENCOMP,ApVendor.cAddress1,ApVendor.cAddress2,ApVendor.cAddress3,;
ApVendor.cAddress4,ApVendor.cAddress5,ApVendor.cAddress6,;
ApVendor.cPhoneNo,POSHDR.ORIGIN,POSHDR.CFOB,lcShipDesc,POSHDR.Entered,POSHDR.Complete,;
POSHDR.Available,PoToSend.type,;
PADR(gfCodDes(POSHDR.CFNLDST,'CFNLDST'),20) )
*C200508,1 TMI [End  ] 
      
SELECT PosLn 
=SEEK(PosHdr.cStyType+PosHdr.PO)
*C200508,4 TMI [Start] Include only lines with Trancd='1'
*SCAN REST WHILE cstytype+po+style+STR(lineno,6)+trancd = ;
                PosHdr.cStyType+PosHdr.PO
SCAN REST WHILE cstytype+po+style+STR(lineno,6)+trancd = ;
                PosHdr.cStyType+PosHdr.PO ;
          FOR TRANCD = '1'
  *C200508,1 TMI [End  ]   
  =SEEK(Style,'Style')
  =SEEK('S'+Scale,'Scale')

  FOR lnCounter = 1 TO Scale.Cnt
     lcCounter = STR(lnCounter,1)            
     IF Qty&lcCounter > 0
       *C200508,1 TMI [Start] Update the added field "Lineno" to the POLN file 
       *INSERT INTO (gcWorkdir + "POLN") ;
       (cPO,cItem,cColor,cSize,nQty,cDesc) VALUES ;
       (POSLN.PO,SUBSTR(POSLN.Style,1,12),ALLTRIM(RIGHT(POSLN.Style,6)),;
        Scale.Sz&lcCounter,POSLN.Qty&lcCounter,Style.Desc+'-'+Style.Desc1)

       *:C124187,1 MHM 11/16/2004 Add the Style Group code And Style Group Description 
       *:C124187,1 MHM 11/16/2004 fields to the POLN file [Start]
       *INSERT INTO (gcWorkdir + "POLN") ;
       (cPO,cItem,cColor,cSize,nQty,cDesc,Lineno) VALUES ;
       (POSLN.PO,SUBSTR(POSLN.Style,1,12),ALLTRIM(RIGHT(POSLN.Style,6)),;
        Scale.Sz&lcCounter,POSLN.Qty&lcCounter,Style.Desc+'-'+Style.Desc1,POSLN.Lineno)

       INSERT INTO (gcWorkdir + "POLN") ;
       (cPO,cItem,cColor,cSize,nQty,cDesc,Lineno,cstygroup,cstygDec) VALUES ;
       (POSLN.PO,SUBSTR(POSLN.Style,1,12),ALLTRIM(RIGHT(POSLN.Style,6)),;
        Scale.Sz&lcCounter,POSLN.Qty&lcCounter,Style.Desc+'-'+Style.Desc1,POSLN.Lineno,;
        Style.cstygroup,gfCodDes(Style.cstygroup,'CSTYGROUP '))
       *:C124187,1 MHM 11/16/2004 [End]
       
       *C200508,1 TMI [End  ]         
     ENDIF
  ENDFOR
ENDSCAN               
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Ahmed Maher (AMH)
*! Date      : 12/31/2000 
*! Purpose   : When function of OG.
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************
FUNCTION lfwRepWhen

SHOW GET pbOGPrevie DISABLED

*!*************************************************************
*! Name      : lfvSelPos
*! Developer : Ahmed Maher (AMH)
*! Date      : 12/31/2000 
*! Purpose   : Valid function of Select in OG.
*!*************************************************************
*! Example     : = lfvSelPos()
*!*************************************************************
Function lfvSelPos
CLEAR READ

*!*************************************************************
*! Name      : lfPrevRun
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 04/25/2000 
*! Purpose   : To do not display the printing selection device
*!*************************************************************
*! Example     : = lfPrevRun()
*!*************************************************************
FUNCTION lfPrevRun

PRIVATE llContinue
llContinue = .T.
llOGEscPrsd = .F.
lnOGHD=IIF(EMPTY(laOGHdFlt[1,1]),0,ALEN(laOGHdFlt,1))
lnOGFX=IIF(EMPTY(laOGFxFlt[1,1]),0,ALEN(laOGFxFlt,1))
lnOGvr=IIF(EMPTY(laOGVrFlt[1,1]),0,ALEN(laOGVrFlt,1))
lcOGSelect=SELECT()
=lfInitValue('laOGVrFlt','T')
=lfInitValue('laOGFXFlt','T')
  
lcOGHDFlt=gfGenFlt('laOGHDFlt' , llOGFilter , .T.)
lcOGFxFlt=gfGenFlt('laOGFxFlt' , llOGFilter , .T.)
lcOGVrFlt=gfGenFlt('laOGVrFlt' , llOGFilter , .T.)
  
lnOGHD=IIF(EMPTY(lcOGHDFlt),0,ALEN(laOGHdFlt,1))
lnOGFX=IIF(EMPTY(lcOGFxFlt),0,ALEN(laOGFxFlt,1))
lnOGvr=IIF(EMPTY(lcOGVrFlt),0,ALEN(laOGVrFlt,1))  
=lfInitValue('laOGVrFlt','C')
=lfInitValue('laOGFXFlt','C')

IF VARREAD() = "PBRUN"  
  lcRpExp=lcOGHDFlt+IIF((lnOGFx+lnOGVr>0) AND lnOGHd>0,' AND (','')+;
        lcOGFxFlt+IIF(lnOGVr>0,IIF(lnOgFx>0,' AND ',''),'')+lcOGVrFlt+;
        IIF((lnOGFx+lnOGVr>0) AND lnOGHd>0,')','')
  IF llContinue
    lcOGTmpExp=IIF(!EMPTY(lcOGHDFlt),lcOGHDFlt+'|Ÿ',lcOGHDFlt)+IIF(!EMPTY(lcOGFXFlt) AND !EMPTY(lcOGVRFlt),lcOGFXFlt+'|Ÿ',lcOGFxFlt)+;
               lcOGVrFlt+IIF(!EMPTY(lcOGHDFlt),'Ÿ','')+IIF(!EMPTY(lcOGFXFlt)AND !EMPTY(lcOGVRFlt),'Ÿ','')
           
    IF EMPTY(lcOGFxFlt) AND EMPTY(lcOGVrFlt)
      lcRpExp=lcOgHdFlt
    ELSE
      lcRpExp=STRTRAN(STRTRAN(lcOGTmpExp,IIF(OCCURS('|Ÿ|Ÿ',lcOGTmpExp)=0,'|Ÿ','|Ÿ|Ÿ');
                ,' AND('),IIF(OCCURS('|Ÿ|Ÿ',lcOGTmpExp)=0,'Ÿ','ŸŸ'),')')
    ENDIF          

    lcOGDevice=gcDevice
    lcOGOutFile=gcOutFile
    gcDevice="SCREEN"    && set printer to temprory file name
    gcOutFile=gcWorkDir+gfTempName()+".TXT"      
    llOGHeader = glHeader
    glHeader = .T.
  ENDIF
ENDIF
IF llContinue
  lcRpExp=IIF(EMPTY(lcRpExp),'.T.',lcRpExp)
  lcogExc=SET('EXACT')        
  SET EXACT ON
  llOGFltCh = IIF(llOGFltCh,llOGFltCh,lcogfltr<>lcrpexp)        

  SET EXACT &lcOGExc
  IF llOGFltCh
    lcOGFltr=lcRpExp
  ENDIF
  SET CONSOLE OFF

  IF !EMPTY(lcOGFormV)
    IF !(lcOGLastForm==&lcOGFormV)
      
      lcOGFormArr='laRpObj'+SUBSTR(lcOGPlatForm,1,3)
      
      lcOGLastForm=&lcOGFormV
      llOGStyleCh=.T.
    ENDIF  

    IF llOGRefForm .OR. lcRepAvlM = 'B' .OR. llOGStyleCh
      =gfCrtFrm(EVAL(lcOGFormV),lcOGFormArr,llOGRefForm)
    ENDIF

  ENDIF  

  DO (gcRepHome+gcAct_Appl+'REPORT.APP') WITH lcOGPrgName, .T.

  SET CONSOLE ON

  IF !VARREAD() == "PBRUN")
    gcDevice=lcOGDevice
    gcOutFile=lcOGOutFile
    glHeader = llOGHeader
  ENDIF  

  llOGFltCh = .F.
ENDIF  
RETURN .F.
*-- End of lfPrevRun.
