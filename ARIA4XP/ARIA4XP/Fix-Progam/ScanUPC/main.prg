_Screen.Visible = .F.

LOCAL lcA27Sys

lcA27Sys = GETDIR("C:\","Please select Aria27 SysFiles path.","Select")

IF !DIRECTORY(lcA27Sys)
  RETURN
ENDIF 

USE ADDBS(lcA27Sys)+'SYCCOMP.DBF' SHARED ALIAS 'SYCCOMP_A' IN 0 
SELECT SYCCOMP_A
SCAN FOR lRunfroma4
  WAIT WINDOW 'Updating SCAN_BATCH_HEADER_T table of company:'+SYCCOMP_A.ccomp_id NOWAIT 
  SELECT SYCCOMP_A
  IF EMPTY(ALLTRIM(CCONSERVER)) OR EMPTY(ALLTRIM(CCONDBNAME))
    LOOP
  ENDIF
  lcActiveCompanyConStr = "Driver={SQL Server};server="+ALLTRIM(CCONSERVER)+";DATABASE="+ALLTRIM(CCONDBNAME)+";uid="+ALLTRIM(CCONUSERID)+";pwd="+ALLTRIM(CCONPASWRD)

  lnConH=SQLSTRINGCONNECT(lcActiveCompanyConStr)
  IF lnConH<0
    LOOP 
  ENDIF
  =lfCreateTables()
  =SQLDISCONNECT(lnConH)
ENDSCAN 

USE IN 'SYCCOMP_A'


******************************************************************************************************************************************
*-                          Function to Create Scan Tables
******************************************************************************************************************************************
FUNCTION lfCreateTables

lnScanHdr = SQLEXEC(lnConH,"Select * From SCAN_BATCH_HEADER_T","SCAN_BATCH_HEADER_T")  
IF lnScanHdr < 0
  *Create Scan Batch Header Table
  lcCreateState = "CREATE TABLE [dbo].[SCAN_BATCH_HEADER_T]("+;
                                     "[SCAN_BATCH_HEADER_KEY] [bigint] IDENTITY(1,1) NOT NULL,"+;
                                     "[BATCH] [varchar](20) NULL,"+;
                                     "[DESCRIPTION] [text] NULL,"+;
                                     "[DATE] [datetime] NULL,"+;
                                     "[USER] [varchar](20) NULL,"+;
                                     "[STATUS] [varchar](20) NULL,"+;
                                     "[VENDOR] [varchar](20) NULL,"+;
                                     "[ACCOUNT] [varchar](5) NULL,"+;
                                     "[TRANSACTION_TYPE] [varchar](2) NULL,"+;
                                     "[TRANSACTION_NO] [varchar](6) NULL,"+;
                                     "CONSTRAINT [UK_SCAN_BATCH_HEADER_T] UNIQUE NONCLUSTERED "+;
                                     "([BATCH] ASC)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]"
  lnScanHdr = SQLEXEC(lnConH, lcCreateState, "SCAN_BATCH_HEADER_T")
ENDIF
  
lnScanLin = SQLEXEC(lnConH,"Select * From SCAN_BATCH_DETAILS_T","SCAN_BATCH_DETAILS_T")  

IF lnScanLin < 0                  
  *Create Scan Batch Details Table
  lcCreateState = "CREATE TABLE [dbo].[SCAN_BATCH_DETAILS_T]("+;
                                     "[SCAN_BATCH_DETAILS_KEY] [bigint] IDENTITY(1,1) NOT NULL,"+;
                                     "[SCAN_BATCH_HEADER_KEY] [bigint] NULL,"+;
                                     "[LINE_NO] [int] NULL,"+;
                                     "[ITEM_NUMBER] [varchar](13) NULL,"+;
                                     "[CARTON_NUMBER] [varchar](30) NULL,"+;
                                     "[PACK_NUMBER] [varchar](20) NULL,"+;
                                     "[CSTYMAJOR] [varchar](19) NULL,"+;
                                     "[STYLE] [varchar](19) NULL,"+;
                                     "[SCALE] [varchar](3) NULL,"+;
                                     "[SIZE] [varchar](10) NULL,"+;
                                     "[QUANTITY] [int] NULL,"+;
                                     "[STATUS] [varchar](20) NULL,"+;
                                     "[REJECTION_REASON] [text] NULL,"+;
                                     "CONSTRAINT [UK_SCAN_BATCH_DETAILS_T] UNIQUE NONCLUSTERED "+;
                                     "([SCAN_BATCH_HEADER_KEY] ASC,[STYLE] ASC,[SCALE] ASC,[SIZE] ASC)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]"
                                     
  lnScanLin = SQLEXEC(lnConH,lcCreateState ,"SCAN_BATCH_DETAILS_T")    
ENDIF

ENDFUNC