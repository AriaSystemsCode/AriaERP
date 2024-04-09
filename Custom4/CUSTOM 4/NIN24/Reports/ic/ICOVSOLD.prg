*:***************************************************************************
*: Program file  : ICOVSOLD.prg
*: Program desc. : Custom Oversold report for NIN24 
*: For Report    : (ICOVSOLD.FRX)
*: System        : Aria Advantage Series.4XP (NEW Framework)
*: Module        : IC
*: Developer     : Mariam MAzhar (MMT)
*: Date          : 04/27/2022
*: Tracking Entry: C202435 {T20220407.0002}
*:***************************************************************************

PARAMETERS LCREQUESTID, LCXMLFILENAME, CLIENTID
IF TYPE('lcXMLFileName') = 'C'
  PUBLIC GCREQUESTID, GCCLIENTID
  GCREQUESTID = LCREQUESTID
  GCCLIENTID = CLIENTID

  PRIVATE LOAGENT
  *E303361,1 SAB 02/28/2013 Merge RB R13 modification with R12 and update R13 VSS [Aria5.2 R13 Media][Start]
  *loAgent = CREATEOBJECT("Aria.EnterpriseServices.RequestHandler.AriaRequestAgent")
  LOAGENT = GOREMOTECALL.GETREMOTEOBJECT("Aria.EnterpriseServices.RequestHandler.AriaRequestAgent")
  *E303361,1 SAB 02/28/2013 Merge RB R13 modification with R12 and update R13 VSS [Aria5.2 R13 Media][End]

  PRIVATE LOPROGRESS
  *E303361,1 SAB 02/28/2013 Merge RB R13 modification with R12 and update R13 VSS [Aria5.2 R13 Media][Start]
  LOPROGRESS = CREATEOBJECT("Aria.DataTypes.RequestHandler.AriaRequestProgress")
  *loProgress = goRemoteCall.GetRemoteObject("Aria.DataTypes.RequestHandler.AriaRequestProgress")
  *E303361,1 SAB 02/28/2013 Merge RB R13 modification with R12 and update R13 VSS [Aria5.2 R13 Media][End]

  LOPROGRESS.PERCENT = 0
  LOPROGRESS.DESCRIPTION = "Opening Data Files..."
  *T20100512.0026 Hassan 2010 05 23 [BEGIN]
  *loAgent.UpdateObjectProgress(lcRequestID, loProgress)

  LOAGENT.UPDATEOBJECTPROGRESS(LCREQUESTID, LOPROGRESS, CLIENTID)
  *T20100512.0026 Hassan 2010 05 23 [END]

  LOCAL LOENVIRONMENT

  *E3028015 HIA Consider the client pathes after assign the code pathes [Begin]

  *T20100512.0026 Hassan 2010 05 23 [BEGIN]
  *loEnvironment = CREATEOBJECT("Aria.Environment.AriaEnviromentVariables")
  *loEnvironment.ClientId = ClientId
  *loEnvironment.ConnectionsRefresh()
  *loEnvironment.GetAria27CompanyDataConnectionString(loAgent.GetRequestCompany(lcRequestID, ClientId))

  *T20100512.0026 Hassan 2010 05 23 [END]
  *LOCAL lcCurrentProcedure
  *lcCurrentProcedure = STRTRAN(UPPER(loEnvironment.Aria40SystemFilesPath), UPPER("SQLDictionary\"), "", -1, 1, 1)
  *lcCurrentProcedure = STRTRAN(UPPER(loEnvironment.Aria40SystemFilesPath), UPPER("SQLDictionary"), "", -1, 1, 1)

  *E303361,1 SAB 02/28/2013 Merge RB R13 modification with R12 and update R13 VSS [Aria5.2 R13 Media][Start]
  *loEnvironment = CREATEOBJECT("Aria.Environment.AriaEnviromentVariables")
  LOENVIRONMENT = GOREMOTECALL.GETREMOTEOBJECT("Aria.Environment.AriaEnviromentVariables")
  *E303361,1 SAB 02/28/2013 Merge RB R13 modification with R12 and update R13 VSS [Aria5.2 R13 Media][End]
  LOENVIRONMENT.CLIENTID = CLIENTID

  LOCAL LCCURRENTPROCEDURE
  *!* T20110801.0008 MAH 8/2/2011
  *--lcCurrentProcedure = STRTRAN(ADDBS(UPPER(loEnvironment.Aria40SystemFilesPath)), UPPER("SQLDictionary\"), "", -1, 1, 1)
  LCCURRENTPROCEDURE =    LOENVIRONMENT.ARIA40SHAREDPATH
  *!* T20110801.0008 MAH 8/2/2011 End

  LOENVIRONMENT.CONNECTIONSREFRESH()

  *BADRAN
  LOCAL LCREQUESTCOMPANY, LCCLIENTROOT, LCENVOUTPUT
  LCREQUESTCOMPANY = LOAGENT.GETREQUESTCOMPANY(LCREQUESTID, CLIENTID)

  LCCLIENTROOT = LOENVIRONMENT.ARIA40SHAREDPATH
  LCENVOUTPUT = LOENVIRONMENT.GETARIA27COMPANYDATACONNECTIONSTRING(LCREQUESTCOMPANY)

  DO (LCCURRENTPROCEDURE + "SRVPRGS\SY\ariamain.fxp") WITH LCREQUESTCOMPANY , CLIENTID, LCCURRENTPROCEDURE, LOENVIRONMENT

  OARIAENVIRONMENT.XML.RESTOREFROMXML(FILETOSTR(LCXMLFILENAME),.T.)
  OARIAENVIRONMENT.REPORT.GCACT_APPL = 'IC'
  OARIAENVIRONMENT.ACTIVEMODULEID = 'IC'
  OARIAENVIRONMENT.REQUESTID = LCREQUESTID

  PUBLIC GCACT_APPL
  GCACT_APPL ='IC'

  IF LEFT(GCDEVICE, 7) = "PRINTER"
    OARIAENVIRONMENT.GCDEVICE = "PRINTER"
  ELSE
    OARIAENVIRONMENT.GCDEVICE = "FILE"
  ENDIF
  OARIAENVIRONMENT.REPORT.CCRORIENTATION = 'P'
ELSE
  LOOGSCROLL.CCRORIENTATION = 'P'
ENDIF

IF TYPE('lcXMLFileName') = 'C' OR loogscroll.llOGFltCh
SET STEP ON 
IF !USED('ORDHDR')
  =gfOpenTable('ORDHDR','ORDHDR')
ENDIF
IF !USED('ORDLINE')  
  =gfOpenTable('ORDLINE','ORDLINE')
ENDIF
IF !USED('STYLE')  
  =gfOpenTable('STYLE','STYLE')
ENDIF
IF !USED('SCALE')  
  =gfOpenTable('SCALE','SCALE')
ENDIF
IF !USED('CUSTOMER')  
  =gfOpenTable('CUSTOMER','CUSTOMER')
ENDIF

CREATE CURSOR (lcTmpOrdLine) (Order C(6), Account C(5), CUSTPO C(15),Start D(8),BTNAME C(30),STYLE C(19),Size C(5),CWARECODE C(6), Note1 C(30),;
                            Rep1 C(3),Rep2 C(3),desc1 C(30),Price N(12,2),Entered D(8),Complete D(8),Store C(8),LINENO N(6),;
                             QTY N(7),SizeNo N(1))
SELECT (lcTmpOrdLine)
INDEX ON STYLE+ORDER+STR(LINENO,6)+STR(SizeNo ,1) TAG  (lcTmpOrdLine)                           
INDEX ON STYLE+STR(SizeNo ,1) TAG  'TmOrdLns'  ADDITIVE &&+STR(LINENO,6) 
CREATE CURSOR 'TMPORDL' (Order C(6),STYLE C(19),QTY1 N(6),QTY2 N(6),QTY3 N(6),QTY4 N(6),QTY5 N(6),QTY6 N(6),QTY7 N(6),QTY8 N(6),TOTQTY N(7),LINENO N(6),Store C(8),Price N(12,2))
INDEX on STYLE TAG 'TMPORDL'
SELECT ORDHDR
SET ORDER TO ORDHDR DESC
=gfSeek('O')
SCAN REST WHILE CORDTYPE+ORDER = 'O' FOR STATUS $ 'OH' 
  SELECT ORDLINE
  =gfSeek(ORDHDR.CORDTYPE+ORDHDR.ORDER)
  SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = ORDHDR.CORDTYPE+ORDHDR.ORDER FOR TOTQTY > 0
    IF TYPE('lcXMLFileName') <> 'C' 
    WAIT WINDOW 'Collecting open/hold orders lines....' NOWAIT 
    ENDIF
    SCATTER MEMO MEMVAR
    INSERT INTO 'TMPORDL' FROM MEMVAR 
  ENDSCAN  
ENDSCAN 

SELECT DISTINCt STYLE FROM 'TMPORDL' INTO CURSOR 'OpnOrdSt'

SELECT 'OpnOrdSt'
LOCATE
SCAN
  =gfSeek(OpnOrdSt.Style,'STYLE','STYLE')
  IF (STYLE.STK1+STYLE.WIP1-STYLE.ORD1) < 0 OR  (STYLE.STK2+STYLE.WIP2-STYLE.ORD2) < 0 OR  (STYLE.STK3+STYLE.WIP3-STYLE.ORD3) < 0 OR;
     (STYLE.STK4+STYLE.WIP4-STYLE.ORD4) < 0 OR  (STYLE.STK5+STYLE.WIP5-STYLE.ORD5) < 0 OR  (STYLE.STK6+STYLE.WIP6-STYLE.ORD6) < 0 OR;
     (STYLE.STK7+STYLE.WIP7-STYLE.ORD7) < 0 OR  (STYLE.STK8+STYLE.WIP8-STYLE.ORD8) < 0  
    =gfSeek('S'+STYLE.SCALE,'SCALE','SCALE') 
    STORE 0 TO lnOts1, lnOts2,lnOts3,lnOts4,lnOts5,lnOts6,lnOts7,lnOts8&&,m.totots,m.Ots1,m.Ots2,m.Ots3,m.Ots4,m.Ots5,m.Ots6,m.Ots7,m.Ots8
    m.Desc1 = STYLE.Desc1
    IF TYPE('lcXMLFileName') <> 'C' 
      WAIT WINDOW 'Collecting open/hold orders lines for style:'+OpnOrdSt.Style NOWAIT
    ENDIF
    FOR lnSC = 1 TO SCALE.CNT
      lcSC = STR(lnSC,1)
      IF (STYLE.STK&lcSC.+STYLE.WIP&lcSC.-STYLE.ORD&lcSC.) < 0
        lnOts&lcSC. = (STYLE.STK&lcSC.+STYLE.WIP&lcSC.-STYLE.ORD&lcSC.)
*!*          m.Ots&lcSC. = (STYLE.STK&lcSC.+STYLE.WIP&lcSC.-STYLE.ORD&lcSC.)
*!*          m.totots = m.totots + m.Ots&lcSC. 
      ENDIF
    ENDFOR 
    FOR lnSC = 1 TO SCALE.CNT   
      lcSC = STR(lnSC,1)
      llEndLoop = .F.
      DO WHILE (lnOts&lcSC. < 0) AND !llEndLoop
        SELECT 'TMPORDL' 
        =SEEK(OpnOrdSt.Style)
        SCAN REST WHILE STYLE = OpnOrdSt.Style
          =gfSeek('O'+TMPORDL.ORDER,'ORDHDR','ORDHDR')
          =gfSeek(IIF(!EMPTY(TMPORDL.Store),'S','M')+ORDHDR.Account+IIF(!EMPTY(TMPORDL.Store),TMPORDL.Store,''),'CUSTOMER','CUSTOMER')
          IF TMPORDL.QTY&lcSC.> 0 
            IF  TMPORDL.QTY&lcSC. <= ABS(lnOts&lcSC.)
              lnOts&lcSC. = lnOts&lcSC. + TMPORDL.QTY&lcSC. 
              IF !SEEK(TMPORDL.STYLE+TMPORDL.ORDER+STR(TMPORDL.LINENO,6)+STR(lnSC,1) ,lcTmpOrdLine ,lcTmpOrdLine)
                SELECT 'TMPORDL' 
                SCATTER MEMO MEMVAR
               * STORE 0 TO m.Qty1,m.Qty2,m.Qty3,m.Qty4,m.Qty5,m.Qty6,m.Qty7,m.Qty8,m.TotQty
                m.Qty = TMPORDL.QTY&lcSC. 
                m.CUSTPO = ORDHDR.Custpo
                m.Start =ordhdr.start
                m.BTNAME = Customer.BTNAME
                m.Complete = ORDHDR.Complete
                m.Entered = ORDHDR.Entered
                m.Rep1 = ORDHDR.Rep1
                m.Rep2 = ORDHDR.Rep2              
                m.Account = ORDHDR.Account  
                m.rep1 = ORDHDR.REP1
                m.rep2 = ORDHDR.REP2
                m.SizeNo = lnSC
                m.cwareCode = ordhdr.cwarecode
*!*                  m.ccartrack = ORDHDR.ccartrack 
*!*                  m.TotQty = m.Qty&lcSC.
                m.price = TMPORDL.Price
                m.Size = Scale.SZ&lcSC. 
*!*                  m.Sz2 = Scale.SZ2
*!*                  m.Sz3 = Scale.SZ3
*!*                  m.Sz4 = Scale.SZ4
*!*                  m.Sz5 = Scale.SZ5
*!*                  m.Sz6 = Scale.SZ6
*!*                  m.Sz7 = Scale.SZ7
*!*                  m.Sz8 = Scale.SZ8                                                                                                  
                INSERT INTO (lcTmpOrdLine) FROM MEMVAR
              ELSE
                REPLACE Qty WITH Qty + TMPORDL.QTY&lcSC. IN (lcTmpOrdLine)
              ENDIF
            ELSE
             IF !SEEK(TMPORDL.STYLE+TMPORDL.ORDER+STR(TMPORDL.LINENO,6)+STR(lnSC,1) ,lcTmpOrdLine ,lcTmpOrdLine)
                SELECT 'TMPORDL' 
                SCATTER MEMO MEMVAR
               * STORE 0 TO m.Qty1,m.Qty2,m.Qty3,m.Qty4,m.Qty5,m.Qty6,m.Qty7,m.Qty8,m.TotQty
                m.Qty = TMPORDL.QTY&lcSC. 
                m.CUSTPO = ORDHDR.Custpo
                m.Start =ordhdr.start
                m.BTNAME = Customer.BTNAME
                m.Complete = ORDHDR.Complete
                m.Entered = ORDHDR.Entered
                m.Rep1 = ORDHDR.Rep1
                m.Rep2 = ORDHDR.Rep2              
                m.Account = ORDHDR.Account  
                m.rep1 = ORDHDR.REP1
                m.rep2 = ORDHDR.REP2
                m.SizeNo = lnSC
                m.cwareCode = ordhdr.cwarecode
*!*                  m.ccartrack = ORDHDR.ccartrack 
*!*                  m.TotQty = m.Qty&lcSC.
                m.price = TMPORDL.Price
                m.Size = Scale.SZ&lcSC. 
*!*                  m.Sz2 = Scale.SZ2
*!*                  m.Sz3 = Scale.SZ3
*!*                  m.Sz4 = Scale.SZ4
*!*                  m.Sz5 = Scale.SZ5
*!*                  m.Sz6 = Scale.SZ6
*!*                  m.Sz7 = Scale.SZ7
*!*                  m.Sz8 = Scale.SZ8                                                                                                  
                INSERT INTO (lcTmpOrdLine) FROM MEMVAR
              ELSE
                REPLACE Qty WITH Qty + TMPORDL.QTY&lcSC. IN (lcTmpOrdLine)
              ENDIF
              lnOts&lcSC. = 0
            ENDIF
          ENDIF
          IF (lnOts&lcSC. >= 0)
            EXIT 
          ENDIF
        ENDSCAN 
        llEndLoop = .T.
      ENDDO
    ENDFOR 
  ENDIF   
ENDSCAN 

ldStart = {}
ldEnd = {}
IF lcRpDtFlt = 'R'
  lnPosDate = ASCAN(loogScroll.laOgFXFlt,"ORDHDR.ENTERED")
  IF lnPosDate > 0
    lnPosDate = ASUBSCRIPT(loogScroll.laOgFXFlt, lnPosDate,1)
    IF !EMPTY(laOGFxFlt[lnPosDate,6])
      ldStart = CTOD(SUBSTR(laOGFxFlt[lnPosDate,6],1,ATC('|',laOGFxFlt[lnPosDate,6])-1))
      ldEnd = CTOD(SUBSTR(laOGFxFlt[lnPosDate,6],  ATC('|',laOGFxFlt[lnPosDate,6])+1))
    ENDIF  
  ENDIF
ENDIF

lcRepSel = ''
llSelSalesRep = .F.
lnPosRep = ASCAN(loogScroll.laOgFXFlt,"ORDHDR.REP1")
IF lnPosRep > 0
  lnPosRep = ASUBSCRIPT(loogScroll.laOgFXFlt,lnPosRep ,1)
  lcRepSel =IIF(!EMPTY(laOgFXFlt[lnPosRep ,6]),laOgFXFlt[lnPosRep ,6],'')
  IF !EMPTY(lcRepSel ) AND USED(lcRepSel )
    SELECT (lcRepSel )
    LOCATE 
    IF EOF()
      lcRepSel = ''
    ELSE
      llSelSalesRep = .T.  
    ENDIF
  ENDIF
ENDIF
lcDelSetV = SET("Deleted" )
SET DELETED OFF 
IF !EMPTY(lcRepSel)
  *SELECT (lcRepSel)
  SELECT  (lcTmpOrdLine)
  LOCATE
  SCAN
    IF (!SEEK(REP1,lcRepSel) AND !SEEK(REP2,lcRepSel)) OR IIF(lcRpDtFlt = 'S',Entered !=oAriaApplication.SystemDate - lnRpDays,IIF(lcRpDtFlt = 'R'  AND (!EMPTY(ldStart) OR !EMPTY(ldEnd)),!BETWEEN(Entered,ldStart,ldEnd),.F.))
      DELETE
    ENDIF
  ENDSCAN
ELSE
  IF lcRpDtFlt != 'A'  
    SELECT  (lcTmpOrdLine)
    LOCATE
    SCAN
      IF IIF(lcRpDtFlt = 'S',Entered !=oAriaApplication.SystemDate - lnRpDays,IIF(lcRpDtFlt = 'R' AND (!EMPTY(ldStart) OR !EMPTY(ldEnd)),!BETWEEN(Entered,ldStart,ldEnd),.F.))
        DELETE
      ENDIF
    ENDSCAN
  ENDIF
ENDIF
SET DELETED &lcDelSetV.
ENDIF
IF USED(lcTmpOrdLine)
  SELECT  (lcTmpOrdLine)
  SET ORDER TO  'TmOrdLns'
  LOCATE
  IF !EOF()
   * loogScroll.cCROrientation = 'P'
   IF TYPE('lcXMLFileName') <> 'C'
      =gfDispRe ()
   ELSE
     OARIAENVIRONMENT.REPORT.OGLASTFORM = lcRpForm  
     LOPROGRESS.PERCENT = 0.9
     LOPROGRESS.DESCRIPTION = "Printing Report..."
     LOAGENT.UPDATEOBJECTPROGRESS(LCREQUESTID, LOPROGRESS, CLIENTID)
     PRIVATE LOPROXY
     LOPROXY = GOREMOTECALL.GETREMOTEOBJECT("Aria.EnterpriseServices.RequestHandler.Proxy.AriaRequestProxy")
     OARIAENVIRONMENT.REPORT.PRINT(OARIAENVIRONMENT.REPORT.OGLASTFORM)
     LOPROGRESS.PERCENT = 1.0
     LOPROGRESS.DESCRIPTION = "Printing Report..."
     LOAGENT.UPDATEOBJECTPROGRESS(LCREQUESTID, LOPROGRESS, CLIENTID)   
   ENDIF  
  ELSE
    IF TYPE('lcXMLFileName') <> 'C' 
       =gfModalGen('TRM00052B00000','DIALOG')
    ENDIF   
    RETURN
  ENDIF
ELSE
  IF TYPE('lcXMLFileName') <> 'C' 
    =gfModalGen('TRM00052B00000','DIALOG')
  ENDIF  
  RETURN
ENDIF  

*!************************************************************************
*! Name      : lfwRepWhen
*: Developer     : Mariam MAzhar (MMT)
*: Date          : 04/27/2022
*! Purpose   : OG When function
*!************************************************************************
FUNCTION lfwRepWhen