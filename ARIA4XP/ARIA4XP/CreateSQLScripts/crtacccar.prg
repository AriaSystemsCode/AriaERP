*E302944,1 MMT 07/25/2011 Save Carriers account info. per account in customer screen[T20101207.0006]
LPARAMETERS lcA27Sys

USE ADDBS(lcA27Sys)+'SYCCOMP.DBF' SHAR ALIAS 'SYCCOMP_A' IN 0 
SELECT SYCCOMP_A
*lcOldComp = oAriaApplication.ActiveCompanyId
SCAN FOR lRunfroma4
  WAIT WINDOW 'Updating ACCOUNT_CARRIERS_T table of company:'+SYCCOMP_A.ccomp_id NOWAIT 
  SELECT SYCCOMP_A
  IF EMPTY(ALLTRIM(CCONSERVER)) OR EMPTY(ALLTRIM(CCONDBNAME))
    LOOP
  ENDIF
*!*	  oAriaApplication.activecompanyid = SYCCOMP_A.ccomp_id
*!*	  oAriaApplication.GetCompanyInformation(SYCCOMP_A.ccomp_id)
  lcactivecompanyconstr = "Driver={SQL Server};server="+ALLTRIM(CCONSERVER)+";DATABASE="+ALLTRIM(CCONDBNAME)+;
                ";uid="+ALLTRIM(CCONUSERID)+";pwd="+ALLTRIM(CCONPASWRD)

  lnConH=SQLSTRINGCONNECT(lcactivecompanyconstr)
  IF lnConH<0
    LOOP 
  ENDIF
  lnEmailForm = SQLEXEC(lnConH,"Select * From ACCOUNT_CARRIERS_T","ACCOUNT_CARRIERS_T")  
  IF lnEmailForm < 0
  *Create  
  lnEmailForm = SQLEXEC(lnConH,"CREATE TABLE [ACCOUNT_CARRIERS_T]([ACCOUNT] [char](5) NULL,	[STORE] [char](8) NULL,"+;
	"[CARRIER_ID] [char](20) NULL,"+;
	"[Overwritten] [bit] NULL,"+;
	"[CARRIER_ACCOUNT] [char](20) NULL,"+;
	"[ACCOUNT_CARRIERST_KEY] [uniqueidentifier])","ACCOUNT_CARRIERS_T")  
   IF lnEmailForm > 0
      lnEmailForm = SQLEXEC(lnConH,"ALTER TABLE [ACCOUNT_CARRIERS_T] ADD  DEFAULT (newid()) FOR [ACCOUNT_CARRIERST_KEY]","ACCOUNT_CARRIERS_T")  
    ENDIF
  ENDIF
  
  lnShipT = SQLEXEC(lnConH,"Select Top 1 * From CARRIER_SHIPMENT_T ","CARRIER_SHIPMENT_T")  
  IF lnShipT < 0                
  lcCreateState = "CREATE TABLE [CARRIER_SHIPMENT_T]("+;
  	"[ACCOUNT] [char](5) NOT NULL,"+;
	"[ORDER] [char](6) NULL,"+;
	"[PICK_TICKET] [char](6) NULL,"+;
	"[INVOICE] [char](6) NULL,"+;
	"[CUSTOMER_PO] [char](15) NULL,"+;
	"[CARTON_NO] [numeric](6, 0) NULL,"+;
	"[STATUS] [char](1) NULL,"+;
	"[STORE] [char](8) NULL,"+;
	"[SHIPTO_NAME] [varchar](100) NULL,"+;
	"[SHIPTO_TITLE] [varchar](100) NULL,"+;
	"[SHIPTO_COMPANY] [varchar](100) NULL,"+;
	"[SHIPTO_ADDRESS1] [varchar](100) NULL,"+;
	"[SHIPTO_ADDRESS2] [varchar](100) NULL,"+;
	"[SHIPTO_ADDRESS3] [varchar](100) NULL,"+;
	"[SHIPTO_ADDRESS4] [varchar](100) NULL,"+;
	"[SHIPTO_CITY] [varchar](100) NULL,"+;
	"[SHIPTO_STATE] [varchar](100) NULL,"+;
	"[SHIPTO_ZIP] [varchar](100) NULL,"+;
	"[SHIPTO_COUNTRY] [varchar](100) NULL,"+;
	"[SHIPTO_PHONE] [varchar](100) NULL,"+;
	"[SHIP_FROM_NAME] [varchar](100) NULL,"+;
	"[SHIP_FROM_ADDRESS1] [varchar](100) NULL,"+;
	"[SHIP_FROM_ADDRESS2] [varchar](100) NULL,"+;
	"[SHIP_FROM_ADDRESS3] [varchar](100) NULL,"+;
	"[SHIP_FROM_ADDRESS4] [varchar](100) NULL,"+;
	"[SHIP_FROM_CITY] [varchar](100) NULL,"+;
	"[SHIP_FROM_STATE] [varchar](100) NULL,"+;
	"[SHIP_FROM_ZIP] [varchar](100) NULL,"+;
	"[SHIP_FROM_COUNTRY] [varchar](100) NULL,"+;
	"[SHIP_FROM_PHONE] [varchar](100) NULL,"+;
	"[CARRIER_SERVICE_CODE] [varchar](20) NULL,"+;
	"[CARRIER_SERVICE_TYPE] [varchar](100) NULL,"+;
	"[PACKAGE_TYPE] [varchar](50) NULL,"+;
	"[CARTON_WEIGHT] [decimal](13, 2) NULL,"+;
	"[BILLING_WEIGHT] [decimal](13, 2) NULL,"+;
	"[CARTON_WIDTH] [decimal](6, 2) NULL,"+;
	"[CARTON_LENGTH] [decimal](6, 2) NULL,"+;
	"[CARTON_HEIGHT] [decimal](6, 2) NULL,"+;
    "[TRACKING_ID_TYPE] [varchar](10) NULL,"+;
	"[TRACKING_NO] [varchar](50) NULL,"+;
	"[RETURN_TRACKING_NO] [varchar](50) NULL,"+;
	"[NFREIGHT] [decimal](13, 2) NULL,"+;
	"[CDECL_VAL] [decimal](13, 2) NULL,"+;
	"[CCOD] [decimal](13, 2) NULL,"+;
	"[GOODS_DESCRIPTION] [varchar](60) NULL,"+;
	"[CCOD_AMT] [decimal](13, 2) NULL,"+;
	"[CARRIER_SHIPMENT_KEY] [uniqueidentifier] NULL,"+;
	"[BILLING_ACCOUNT] [varchar](30) NULL,"+;
	"[CARRIER_SHIPMENT_DIGEST] [text] NULL,"+;
	"[CARRIER_SHIPMENT_ID] [varchar](50) NULL,"+;
	"[CARRIER] [varchar](30) NULL,"+;
	"[PACKAGE] [varchar](50) NULL"+;
	") ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]"
lnShipT = SQLEXEC(lnConH,lcCreateState ,"CARRIER_SHIPMENT_T")  	
IF lnShipT > 0
  lnShipT = SQLEXEC(lnConH,"ALTER TABLE [dbo].[CARRIER_SHIPMENT_T] ADD  CONSTRAINT [DF_CARRIER_SHIPMENT_T_CARRIER_SHIPMENT_KEY]  DEFAULT (newid()) FOR [CARRIER_SHIPMENT_KEY]","CARRIER_SHIPMENT_T")  
ENDIF
                
ENDIF
=SQLDISCONNECT(lnConH)  
           
ENDSCAN 
*oAriaApplication.activecompanyid = lcOldComp 
*oAriaApplication.GetCompanyInformation(lcOldComp)
USE IN 'SYCCOMP_A'