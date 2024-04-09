*:***************************************************************************
*: Program file  : ALMSDESP.prg
*: Program desc. : CUSTOM M & S DESPATCH Perfromance report For Low Profile
*: Date          : 09/26/2007
*: System        : Aria Ad vantage Series.
*: Module        : SALES ORDER  Allocation (AL)
*: Developer     : Mariam Mazhar[MMT] 
*: Tracking Job Number: C200863
*: 
*:***************************************************************************
*:Modifications:
*: B608356,1 11/19/2007 MMT fix bug of Duplicating Shipped Qty                 [T20070822.0012]
*: C200890,1 11/19/2007 MMT Add new 3 changes to the report as customer request[T20071031.0057]
*: C200890,2 05/18/2008 MMT Add warehouse to the report as customer request    [T20080513.0001]
*: C200890,3 10/29/2008 MMT Some Lines are not printed in Excel   [T20081008.0001]
*:***************************************************************************
IF loOGScroll.llOGFltCh     && Variable to detect OG filter changes
  lfCrtTemp()
  lfCollectData()
ELSE
  IF !USED(lcRpTmp)
    USE oAriaApplication.WorkDir + lcRpTmp + ".DBF" SHARED IN 0 
  ENDIF    
ENDIF

IF RECCOUNT(lcRpTmp) = 0
  =gfModalGen('TRM00052B40011','ALERT')
  RETURN .F.
ENDIF 


SELECT(lcRpTmp)

USE 

lcStart = ""
lcComplete = ""

lnDatePos = ASCAN(loOGScroll.laOGFxFlt,'ALCALOFF.DADD_DATE')
IF lnDatePos > 0
  lnDatePos = ASUBSCRIPT(loOGScroll.laOGFxFlt,lnDatePos,1)
  IF !EMPTY(loOGScroll.laOGFxFlt[lnDatePos,6])
    lcStart = IIF(EMPTY(SUBSTR(loOGScroll.laOGFxFlt[lnDatePos,6],1,10)),DTOC(CTOD("")),SUBSTR(loOGScroll.laOGFxFlt[lnDatePos,6],1,10))
    lcComplete = IIF(EMPTY(SUBSTR(loOGScroll.laOGFxFlt[lnDatePos,6],12,21)),DTOC(CTOD("")),SUBSTR(loOGScroll.laOGFxFlt[lnDatePos,6],12,21))
  ENDIF  
ENDIF  



loOgScroll.cCRorientation = 'L'
DIMENSION loOgScroll.laCRParams[3,2]

loOgScroll.laCRParams[1,1] = 'UserName'
loOgScroll.laCRParams[1,2] = oAriaApplication.User_Name                        


loOgScroll.laCRParams[2,1] = 'DateRange'
loOgScroll.laCRParams[2,2] = IIF(!EMPTY(lcStart) AND !EMPTY(lcComplete),lcStart + "  TO   "+ lcComplete,"")                       

loOgScroll.laCRParams[3,1] = 'WeekNo'
loOgScroll.laCRParams[3,2] = lnRpWeek


lcReportFileName = oAriaApplication.ReportHome+"\AL\"+"ALMSDESP.RPT"                       
lcDestination    = PUTFILE("","",'XLS')

IF EMPTY(lcDestination)
  =gfModalGen('TRM00052B40011','ALERT')
  RETURN 
ENDIF


loMainCr = CREATEOBJECT('CrystalRuntime.Application') 
loMain = CREATEOBJECT('CrystalRuntime.Report') 
loMain = loMainCr.OpenReport(lcReportFileName)

loMain.Database.Tables.Item[1].Setlogoninfo ( oAriaApplication.WorkDir + lcRpTmp + ".DBF")
loMain.Database.Tables.Item[1].SetTableLocation ( oAriaApplication.WorkDir + lcRpTmp + ".DBF",'','')
loMain.Database.Verify() && verify database 
loMain.DiscardSavedData()
loMain.ConvertDateTimeType = 1  && crConvertDateTimeToDate 
loMain.CaseInsensitiveSQLData = .T.
lfSendParam()


loMain.ExportOptions.FormatType = 29  && crEFTExcel70 
loMain.ExportOptions.DestinationType = 1  && crEDTDiskFile
loMain.ExportOptions.DiskFileName = lcDestination    
loMain.ExportOptions.ExcelUseConstantColumnWidth = .F.
loMain.ExportOptions.ExcelUseTabularFormat = .T. 

*: C200890,1 11/19/2007 MMT Add new 3 changes to the report as customer request[Start]
lcOldNameDest = loMain.ExportOptions.DiskFileName
lcFullPath = JUSTPATH(loMain.ExportOptions.DiskFileName)
lcFileName = JUSTSTEM(loMain.ExportOptions.DiskFileName)
loMain.ExportOptions.DiskFileName = ALLTRIM(lcFileName)+".XLS"
lcFileName  = loMain.ExportOptions.DiskFileName 
lcOldDef = SET("Default")
SET DEFAULT TO (lcFullPath)
*: C200890,1 11/19/2007 MMT Add new 3 changes to the report as customer request[End]

loMain.Export(.F.)

loMainCr  = NULL
loMain    = NULL

*: C200890,1 11/19/2007 MMT Add new 3 changes to the report as customer request[Start]
*IF FILE(lcDestination)
IF FILE(lcFileName)
*: C200890,1 11/19/2007 MMT Add new 3 changes to the report as customer request[End]

  loRun = CreateObject("WScript.Shell")
  
  *: C200890,1 11/19/2007 MMT Add new 3 changes to the report as customer request[Start]
  *loRun.Run(lcDestination, 3)
  loRun.Run(lcFileName , 3)
  *: C200890,1 11/19/2007 MMT Add new 3 changes to the report as customer request[End]
  
  loRun = NULL
ENDIF

*: C200890,1 11/19/2007 MMT Add new 3 changes to the report as customer request[Start]
SET DEFAULT TO (lcOldDef)
*: C200890,1 11/19/2007 MMT Add new 3 changes to the report as customer request[End]



*!*****************************************************************************************
*! Name      : lfWhenRep
*! Developer : Mariam Mazhar [MMT] 
*! Date      : 09/26/2007
*! Purpose   : report When Function
*!*****************************************************************************************
FUNCTION lfWhenRep
=gfOpenTable('PIKLINE','PIKLINE','SH')
=gfOpenTable('ALCALOFF','ALCALOFF','SH')

*: C200890,1 11/19/2007 MMT Add new 3 changes to the report as customer request[Start]
=gfOpenTable('PIKTKT','PIKTKT','SH')
*: C200890,1 11/19/2007 MMT Add new 3 changes to the report as customer request[End]

loogscroll.parent.ogToolBar.cntExternal.cmdEmail.Enabled = .F.
loogscroll.parent.ogtoolbar.cntPrint.cmdprint.enabled = .F.
loogscroll.parent.ogtoolbar.cntPrint.cmdexport.enabled = .F.

*!*****************************************************************************************
*! Name      : lfvWeek
*! Developer : Mariam Mazhar [MMT] 
*! Date      : 09/26/2007
*! Purpose   : Validate Week no.
*!*****************************************************************************************
FUNCTION lfvWeek
IF !BETWEEN(lnRpWeek,1,53)
  WAIT WINDOW "Week No. must be bewteen 1 and 53"
  lnRpWeek = Week(oAriaApplication.SystemDate)
ENDIF 

*!*****************************************************************************************
*! Name      : lfvYear
*! Developer : Mariam Mazhar [MMT] 
*! Date      : 09/26/2007
*! Purpose   : Validate year Value
*!*****************************************************************************************
FUNCTION lfvYear
IF LEN(ALLTRIM(STR(lnRpYear))) < 4

  IF LEN(ALLTRIM(STR(lnRpYear))) = 2 
    lnRpYear= VAL(ALLTRIM("20"+ALLTRIM(STR(lnRpYear))))
  ENDIF 
  
  IF LEN(ALLTRIM(STR(lnRpYear))) = 1 
    lnRpYear= VAL(ALLTRIM("200"+ALLTRIM(STR(lnRpYear))))
   
  ENDIF 
  
  IF LEN(ALLTRIM(STR(lnRpYear))) = 3
    lnRpYear= VAL(ALLTRIM("2"+ALLTRIM(STR(lnRpYear))))
  ENDIF 
ENDIF 

*!*****************************************************************************************
*! Name      : lfCrtTemp
*! Developer : Mariam Mazhar [MMT] 
*! Date      : 09/26/2007
*! Purpose   : create temp. file
*!*****************************************************************************************
FUNCTION lfCrtTemp

*: C200890,1 11/19/2007 MMT Add new 3 changes to the report as customer request[Start]
*DIMENSION laTempStru[19,4]
*: C200890,2 05/18/2008 MMT Add warehouse to the report as customer request    [Start]
*DIMENSION laTempStru[21,4]
DIMENSION laTempStru[22,4]
*: C200890,2 05/18/2008 MMT Add warehouse to the report as customer request    [End]
*: C200890,1 11/19/2007 MMT Add new 3 changes to the report as customer request[End]

laTempStru[1,1] ='CCALLOFF'
laTempStru[1,2] = 'C'
laTempStru[1,3] = 6
laTempStru[1,4] = 0

laTempStru[2,1] ='Dadd_date'
laTempStru[2,2] = 'D'
laTempStru[2,3] = 8
laTempStru[2,4] = 0

laTempStru[3,1] ='Cdept'
laTempStru[3,2] = 'C'
laTempStru[3,3] = 4
laTempStru[3,4] = 0

laTempStru[4,1] ='Style'
laTempStru[4,2] = 'C'
laTempStru[4,3] = 19
laTempStru[4,4] = 0

laTempStru[5,1] ='ClrDesc'
laTempStru[5,2] = 'C'
laTempStru[5,3] = 30
laTempStru[5,4] = 0

laTempStru[6,1] ='TOTORGQty'
laTempStru[6,2] = 'N'
laTempStru[6,3] = 6
laTempStru[6,4] = 0

laTempStru[7,1] ='TOTQty'
laTempStru[7,2] = 'N'
laTempStru[7,3] = 7
laTempStru[7,4] = 0



laTempStru[8,1] ='SametotP'
laTempStru[8,2] = 'N'
laTempStru[8,3] = 6
laTempStru[8,4] = 0


laTempStru[9,1] ='P1totP'
laTempStru[9,2] = 'N'
laTempStru[9,3] = 6
laTempStru[9,4] = 0

laTempStru[10,1] ='P2totP'
laTempStru[10,2] = 'N'
laTempStru[10,3] = 6
laTempStru[10,4] = 0


laTempStru[11,1] ='P3totP'
laTempStru[11,2] = 'N'
laTempStru[11,3] = 6
laTempStru[11,4] = 0

laTempStru[12,1] ='P4totP'
laTempStru[12,2] = 'N'
laTempStru[12,3] = 6
laTempStru[12,4] = 0


laTempStru[13,1] ='P5totP'
laTempStru[13,2] = 'N'
laTempStru[13,3] = 6
laTempStru[13,4] = 0

laTempStru[14,1] ='P6totP'
laTempStru[14,2] = 'N'
laTempStru[14,3] = 6
laTempStru[14,4] = 0



laTempStru[15,1] ='Over6totP'
laTempStru[15,2] = 'N'
laTempStru[15,3] = 6
laTempStru[15,4] = 0


laTempStru[16,1] ='TOTSHIP'
laTempStru[16,2] = 'N'
laTempStru[16,3] = 8
laTempStru[16,4] = 0

laTempStru[17,1] ='NOTINV'
laTempStru[17,2] = 'N'
laTempStru[17,3] = 8
laTempStru[17,4] = 0

laTempStru[18,1] ='TStyle'
laTempStru[18,2] = 'C'
laTempStru[18,3] = 19
laTempStru[18,4] = 0

laTempStru[19,1] ='PIKTKT'
laTempStru[19,2] = 'C'
laTempStru[19,3] = 6
laTempStru[19,4] = 0

*: C200890,1 11/19/2007 MMT Add new 3 changes to the report as customer request[Start]
laTempStru[20,1] ='invoiceNO'
laTempStru[20,2] = 'C'
laTempStru[20,3] = 6
laTempStru[20,4] = 0

laTempStru[21,1] ='desp_date'
laTempStru[21,2] = 'D'
laTempStru[21,3] = 8
laTempStru[21,4] = 0
*: C200890,1 11/19/2007 MMT Add new 3 changes to the report as customer request[End]


*: C200890,2 05/18/2008 MMT Add warehouse to the report as customer request    [Start]
laTempStru[22,1] ='CWARECODE'
laTempStru[22,2] = 'C'
laTempStru[22,3] = 6
laTempStru[22,4] = 0
*: C200890,2 05/18/2008 MMT Add warehouse to the report as customer request    [End]

= gfCrtTmp(lcRpTmp,@laTempStru,"CCALLOFF" ,lcRpTmp,.F.)


*!*****************************************************************************************
*! Name      : lfCollectData
*! Developer : Mariam Mazhar [MMT] 
*! Date      : 09/26/2007
*! Purpose   : data collecting
*!*****************************************************************************************
FUNCTION lfCollectData



lcStart = ""
lcComplete = ""

lnDatePos = ASCAN(loOGScroll.laOGFxFlt,'ALCALOFF.DADD_DATE')
IF lnDatePos > 0
  lnDatePos = ASUBSCRIPT(loOGScroll.laOGFxFlt,lnDatePos,1)
  IF !EMPTY(loOGScroll.laOGFxFlt[lnDatePos,6])
    lcStart = IIF(EMPTY(SUBSTR(loOGScroll.laOGFxFlt[lnDatePos,6],1,10)),DTOC(CTOD("")),SUBSTR(loOGScroll.laOGFxFlt[lnDatePos,6],1,10))
    lcComplete = IIF(EMPTY(SUBSTR(loOGScroll.laOGFxFlt[lnDatePos,6],12,21)),DTOC(CTOD("")),SUBSTR(loOGScroll.laOGFxFlt[lnDatePos,6],12,21))
  ENDIF  
ENDIF  


lnStyMajor = LEN(gfItemMask('PM'))

*C200863,2 MMT 10/03/2007 fix bug of wrong color desc.[Start]
*lnclrlen  = LEN(gfitemmask("PN"))
lnClrLnGl   = 0
lnClrPosGL = 0

DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnClrLnGl  = LEN(laItemSeg[lnCount,3])
    lnClrPosGL = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR
*C200863,2 MMT 10/03/2007 fix bug of wrong color desc.[End]


lcWeek=IIF(LEN(ALLTRIM(STR(lnRpWeek)))=1,PADL(ALLTRIM(STR(lnRpWeek)),2,"0"),ALLTRIM(STR(lnRpWeek)))

IF !EMPTY(lcWeek)
  IF gfseek(lcWeek,'ALCALOFF')
    SELECT ALCALOFF
    SCAN REST WHILE CCALLOFF =lcWeek FOR;
      YEAR(ALCALOFF.DADD_DATE) = lnRpYear 
      
* AND ;
      IIF(!EMPTY(lcStart) AND !EMPTY(lcStart),BETWEEN(ALCALOFF.DADD_DATE,CTOD(lcStart),CTOD(lcComplete)),.T.)     
      
      *: C200890,1 11/19/2007 MMT Add new 3 changes to the report as customer request[Start]
      m.desp_date = {}
      m.invoiceNO  = ""
      *: C200890,1 11/19/2007 MMT Add new 3 changes to the report as customer request[End]
      
      m.Dadd_date = {}
      m.Cdept     = ''
      m.Style     = ''
      m.ClrDesc   = ''
      M.TOTORGQty = 0
      m.TOTQty    = 0
      m.TOTSHIP   = 0
      m.Over6totP = 0
      m.P6totP    = 0
      m.P5totP    = 0
      m.P4totP    = 0
      m.P3totP    = 0
      m.P3totP    = 0
      m.P2totP    = 0
      m.P1totP    = 0
      m.SametotP  = 0
      m.NOTINV    = 0
      
      SCATTER MEMO MEMVAR
      m.Dadd_date = ALCALOFF.Dadd_date
      m.piktkt 	  = ALCALOFF.piktkt
      
      
      *: C200890,1 11/19/2007 MMT Add new 3 changes to the report as customer request[Start]
      =gfSeek(m.piktkt,'PIKTKT','PIKTKT')
      m.desp_date = piktkt.csenddate
      *: C200890,1 11/19/2007 MMT Add new 3 changes to the report as customer request[End]
      
      
      *C200863,2 MMT 10/03/2007 fix bug of wrong Department Value.[Start]
      *m.Cdept = m.cdepot
      m.Cdept = m.Cdept
      *C200863,2 MMT 10/03/2007 fix bug of wrong Department value.[End]
      
      m.Style = SUBSTR(ALCALOFF.Style,1,lnStyMajor)
      
      *C200863,2 MMT 10/03/2007 fix bug of wrong color desc.[Start]
      *m.ClrDesc = gfCodDes(SUBSTR(ALCALOFF.Style , lnStyMajor +2 , lnclrlen),'COLOR') 
      m.ClrDesc = gfCodDes(SUBSTR(ALCALOFF.Style , lnClrPosGL , lnClrLnGl),'COLOR') 
      *C200863,2 MMT 10/03/2007 fix bug of wrong color desc.[End]
      
      M.TOTORGQty = ALCALOFF.norgtotqty
      m.TOTQty = ALCALOFF.totqty
      SELECT PIKLINE
      
      =gfSetOrder('PIKLINE')&& PIKTKT+ORDER+STR(LINENO,6)
      IF gfSeek(ALCALOFF.PIKTKT+ALCALOFF.ORDER)
        SELECT PIKLINE
        SCAN REST WHILE PIKTKT+ORDER+STR(LINENO,6)=ALCALOFF.PIKTKT+ALCALOFF.ORDER FOR Style = ALCALOFF.Style AND !EMPTY(PIKLINE.Invoice) 
        
        
        
        
        *: C200890,1 11/19/2007 MMT Add new 3 changes to the report as customer request[Start]
        m.invoiceNO= PIKLINE.INVOICE
        *: C200890,1 11/19/2007 MMT Add new 3 changes to the report as customer request[End]
        
      * AND !EMPTY(PIKLINE.Invoice) 
        *+ALCALOFF.ORDER+STR(ALCALOFF.LINENO,6)
        *DO CASE 
          *CASE PIKLINE.Invdate = ALCALOFF.DADD_DATE 
          
           *: C200890,3 10/29/2008 MMT Some Lines are not printed in Excel   [Start]
           lnRecno = RECNO()
           = gfSeek(ALCALOFF.PIKTKT+ALCALOFF.ORDER)
           *: C200890,3 10/29/2008 MMT Some Lines are not printed in Excel   [End]

          
           SUM PIKLINE.totPik REST WHILE PIKTKT+ORDER+STR(LINENO,6)=ALCALOFF.PIKTKT+ALCALOFF.ORDER FOR Style = ALCALOFF.Style AND;
             !EMPTY(PIKLINE.Invoice) AND PIKLINE.Invdate = ALCALOFF.DADD_DATE TO m.SametotP 
             
           =gfSeek(ALCALOFF.PIKTKT+ALCALOFF.ORDER)
           
           SUM PIKLINE.totPik REST WHILE PIKTKT+ORDER+STR(LINENO,6)=ALCALOFF.PIKTKT+ALCALOFF.ORDER FOR Style = ALCALOFF.Style AND;
             !EMPTY(PIKLINE.Invoice) AND (PIKLINE.Invdate - ALCALOFF.DADD_DATE = 1) TO m.P1totP 
           
           
           
          =gfSeek(ALCALOFF.PIKTKT+ALCALOFF.ORDER)
           
          SUM PIKLINE.totPik REST WHILE PIKTKT+ORDER+STR(LINENO,6)=ALCALOFF.PIKTKT+ALCALOFF.ORDER FOR Style = ALCALOFF.Style AND;
             !EMPTY(PIKLINE.Invoice) AND (PIKLINE.Invdate - ALCALOFF.DADD_DATE = 2) TO m.P2totP 
             
             
          =gfSeek(ALCALOFF.PIKTKT+ALCALOFF.ORDER)
           
          SUM PIKLINE.totPik REST WHILE PIKTKT+ORDER+STR(LINENO,6)=ALCALOFF.PIKTKT+ALCALOFF.ORDER FOR Style = ALCALOFF.Style AND;
             !EMPTY(PIKLINE.Invoice) AND (PIKLINE.Invdate - ALCALOFF.DADD_DATE = 3) TO m.P3totP   
             
             
          =gfSeek(ALCALOFF.PIKTKT+ALCALOFF.ORDER)
           
          SUM PIKLINE.totPik REST WHILE PIKTKT+ORDER+STR(LINENO,6)=ALCALOFF.PIKTKT+ALCALOFF.ORDER FOR Style = ALCALOFF.Style AND;
             !EMPTY(PIKLINE.Invoice) AND (PIKLINE.Invdate - ALCALOFF.DADD_DATE = 4) TO m.P4totP    
           
                   
          =gfSeek(ALCALOFF.PIKTKT+ALCALOFF.ORDER)
           
          SUM PIKLINE.totPik REST WHILE PIKTKT+ORDER+STR(LINENO,6)=ALCALOFF.PIKTKT+ALCALOFF.ORDER FOR Style = ALCALOFF.Style AND;
             !EMPTY(PIKLINE.Invoice) AND (PIKLINE.Invdate - ALCALOFF.DADD_DATE = 5) TO m.P5totP  
             
             
          =gfSeek(ALCALOFF.PIKTKT+ALCALOFF.ORDER)
           
          SUM PIKLINE.totPik REST WHILE PIKTKT+ORDER+STR(LINENO,6)=ALCALOFF.PIKTKT+ALCALOFF.ORDER FOR Style = ALCALOFF.Style AND;
             !EMPTY(PIKLINE.Invoice) AND (PIKLINE.Invdate - ALCALOFF.DADD_DATE = 6) TO m.P6totP  
             
             
          =gfSeek(ALCALOFF.PIKTKT+ALCALOFF.ORDER)
           
          SUM PIKLINE.totPik REST WHILE PIKTKT+ORDER+STR(LINENO,6)=ALCALOFF.PIKTKT+ALCALOFF.ORDER FOR Style = ALCALOFF.Style AND;
             !EMPTY(PIKLINE.Invoice) AND (PIKLINE.Invdate - ALCALOFF.DADD_DATE > 6) TO m.Over6totP     
   
*!*		        m.SametotP = PIKLINE.totPik
	        
*!*		      CASE   PIKLINE.Invdate - ALCALOFF.DADD_DATE = 1
*!*	  	  	    m.P1totP  = PIKLINE.totPik
*!*	  	  	    
*!*	  	      CASE   PIKLINE.Invdate - ALCALOFF.DADD_DATE = 2
*!*	  	  	    m.P2totP  = PIKLINE.totPik

*!*	  	  	  CASE   PIKLINE.Invdate - ALCALOFF.DADD_DATE = 3
*!*	  	  	    m.P3totP  = PIKLINE.totPik  
*!*	  	  	    
*!*	  	  	  CASE  PIKLINE.Invdate - ALCALOFF.DADD_DATE  = 4
*!*	  	  	    m.P4totP  = PIKLINE.totPik  
*!*	  	  	    
*!*	  	  	  CASE  PIKLINE.Invdate - ALCALOFF.DADD_DATE  = 5
*!*	  	  	    m.P5totP  = PIKLINE.totPik  
*!*	  	  	    
*!*	  	  	  CASE  PIKLINE.Invdate - ALCALOFF.DADD_DATE  = 6
*!*	  	  	    m.P6totP  = PIKLINE.totPik  
*!*	  	  	    
*!*	  	  	  CASE  PIKLINE.Invdate - ALCALOFF.DADD_DATE  > 6
*!*	  	  	    m.Over6totP = PIKLINE.totPik  
*!*	        ENDCASE
	  
	  *: C200890,3 10/29/2008 MMT Some Lines are not printed in Excel   [Start]
  	  IF BETWEEN(lnRecno ,1,RECCOUNT())
  		 GO RECORD lnRecno 
	  ENDIF 
	  *: C200890,3 10/29/2008 MMT Some Lines are not printed in Excel   [End]
	  
       ENDSCAN  
       SELECT ALCALOFF
      ENDIF
      
      *: B608356,1 11/18/2007 MMT fix bug of Duplicating Shipped Qty[Start]
      *m.TOTSHIP = m.P6totP+m.P5totP +m.P4totP +m.P3totP+m.P3totP  + m.P2totP  +m.P1totP+m.SametotP 
      *m.TOTSHIP = m.P6totP+ m.P5totP + m.P4totP + m.P3totP + m.P2totP  + m.P1totP+ m.SametotP 
      
      *: C200890,3 10/29/2008 MMT Some Lines are not printed in Excel   [Start]
      m.TOTSHIP = m.P6totP+ m.P5totP + m.P4totP + m.P3totP + m.P2totP  + m.P1totP+ m.SametotP+m.Over6totP   
      *: C200890,3 10/29/2008 MMT Some Lines are not printed in Excel   [End]
      
      *: B608356,1 11/18/2007 MMT fix bug of Duplicating Shipped Qty[End]
      m.NOTINV  = m.TOTQty - m.TOTSHIP 
      m.TStyle = ALCALOFF.style
      
      *: C200890,2 05/18/2008 MMT Add warehouse to the report as customer request    [Start]
      m.cwarecode = alcaloff.cwarecode
      *: C200890,2 05/18/2008 MMT Add warehouse to the report as customer request    [End]
      
      SELECT (lcRpTmp)
      APPEND BLANK 
      GATHER MEMo MEMVAR  
    ENDSCAN
  ENDIF
ENDIF




*!*****************************************************************************************
*! Name      : lfSendParam
*! Developer : Mariam Mazhar [MMT] 
*! Date      : 09/26/2007
*! Purpose   : Adjust crystal report parameters
*!*****************************************************************************************
FUNCTION lfSendParam
LOCAL lnParamCnt, lnArrayCnt, llParamPrompt
*-- Adding Default Parameters (Company Name, User Name) [start]



*-- Adding Custom parameters (Added by developer) if any [start]
IF !EMPTY(loogscroll.laCRParams[1]) 
  FOR lnParamCnt= 1 TO loMain.ParameterFields.count
  FOR lnArrayCnt=1 TO ALEN(loogscroll.laCRParams,1)
  DO CASE 
    CASE (UPPER(loMain.ParameterFields.item[lnParamCnt].ParameterFieldName) == ;
    UPPER(loogscroll.laCRParams[lnArrayCnt,1])  )
      loMain.ParameterFields.item[lnParamCnt].AddCurrentValue(loogscroll.laCRParams[lnArrayCnt,2])
    Endcase
  ENDFOR 
  ENDFOR 
ENDIF
*-- Adding Custom parameters (Added by developer) if any [END]

*-- Checking if a parameter needs a value [start]
llParamPrompt = .F.
FOR lnParamCnt= 1 TO loMain.ParameterFields.count
  IF loMain.ParameterFields.item[lnParamCnt].NeedsCurrentValue        
  llParamPrompt = .T.      
  ENDIF
ENDFOR
*-- Checking if a parameter needs a value [END]
*-- THIS.oReport.EnableParameterPrompting = lParamPrompt && Enable/Disable Cryatal parameter prompting
IF llParamPrompt 
  = MESSAGEBOX("Parameter is missing", 16, '')
  RETURN .F.
ENDIF
  

