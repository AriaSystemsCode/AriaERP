SET DELETED ON 
_Screen.Visible =.F.
Source_DBFs = "X:\ARIA4XP\DBFs\"

SET DEFAULT TO (Source_DBFs)

USE 01/Style		 SHARED ALIAS SourceStyle IN 0
USE 01/Stydye 		 SHARED ALIAS SourceStydye IN 0 
USE 01/Scale 		 SHARED ALIAS SourceScale IN 0
*USE 01/Icstyhst		 SHARED ALIAS SourceIcstyhst 
USE 01/Customer		 SHARED ALIAS SourceCustomer IN 0
USE 01/StyleUpc 	 SHARED ALIAS SourceStyleUpc IN 0
USE 01/Warehous 	 SHARED ALIAS SourceWarehous IN 0
USE 01/Ordline 		 SHARED ALIAS SourceOrdline IN 0
USE 01/ARCUSHST	     SHARED ALIAS SourceARCUSTHST IN 0
USE 01/ordhdr 		 SHARED ALIAS Sourceordhdr IN 0
USE 01/SalesRep 	 SHARED ALIAS SourceSalesRep IN 0
USE 01/Codes 		 SHARED ALIAS SourceCodes IN 0 

USE 04/Style 		 SHARED ALIAS DestinationStyle IN 0 
USE 04/Stydye 		 SHARED ALIAS DestinationStydye IN 0
USE 04/Scale		 SHARED ALIAS DestinationScale IN 0
*USE 04/Icstyhst		 SHARED ALIAS DestinationIcstyhst 
USE 04/Customer		 SHARED ALIAS DestinationCustomer IN 0
USE 04/StyleUpc		 SHARED ALIAS DestinationStyleUpc IN 0
USE 04/Warehous		 SHARED ALIAS DestinationWarehous IN 0
USE 04/Ordline 		 SHARED ALIAS DestinationOrdline IN 0
USE 04/ARCUSHST 	 SHARED ALIAS DestinationARCUSTHST IN 0
USE 04/Ordhdr 		 SHARED ALIAS DestinationOrdhdr IN 0
USE 04/SalesRep	     SHARED ALIAS DestinationSalesRep IN 0
USE 04/Codes		 SHARED ALIAS DestinationCodes IN 0

SELECT DestinationStyle
*APPEND FROM SourceStyle FOR Cdivision = "UPTOWN" 
SELECT SourceStyle 
SCAN FOR Cdivision = "UPTOWN" 
  SCATTER MEMO MEMVAR 
  INSERT INTO DestinationStyle FROM MEMVAR  
ENDSCAN
*INSERT INTO DestinationStyle SELECT * FROM SourceStyle WHERE Cdivision = "UPTOWN" 

*Ordline*
SELECT * FROM SourceOrdline WHERE BETWEEN(order,'030714','030760') AND CORDTYPE ='O' INTO CURSOR 'OrderlineRangeValues'
*Ordhdr*
SELECT * FROM Sourceordhdr WHERE BETWEEN(order,'030714','030760') AND CORDTYPE ='O' INTO CURSOR 'OrderHeaderRangeValues'



*style*
SELECT distinct cdisccode      FROM DestinationStyle WHERE Cdivision = "UPTOWN"   	INTO CURSOR 'CdisccodeValues'
SELECT distinct season         FROM DestinationStyle WHERE Cdivision = "UPTOWN"  	INTO CURSOR 'seasonValues'
SELECT distinct royalty		   FROM DestinationStyle WHERE Cdivision = "UPTOWN"  	INTO CURSOR 'royaltyValues'
SELECT distinct CPURCODE	   FROM DestinationStyle WHERE Cdivision = "UPTOWN" 	INTO CURSOR 'CPURCODEValues'
SELECT distinct CSTYGROUP	   FROM DestinationStyle WHERE Cdivision = "UPTOWN"  	INTO CURSOR 'CSTYGROUPValues'
SELECT distinct CTAXCODE 	   FROM DestinationStyle WHERE Cdivision = "UPTOWN" 	INTO CURSOR 'CTAXCODEValues'
SELECT distinc SUBSTR(Style,14) as Color FROM DestinationStyle WHERE Cdivision = "UPTOWN" 	INTO CURSOR 'ColorValues'
**************************************************************************************************************

*customer*
SELECT distinct Region 		    FROM SourceCustomer WHERE Cdivision = "UPTOWN"  INTO CURSOR 'RegionValues'
SELECT distinct Class		    FROM SourceCustomer WHERE Cdivision = "UPTOWN"  INTO CURSOR 'ClassValues'
SELECT distinct CTermCode 	    FROM SourceCustomer WHERE Cdivision = "UPTOWN"  INTO CURSOR 'CTermCodeValues'
SELECT distinct Shipvia 		FROM SourceCustomer WHERE Cdivision = "UPTOWN"  INTO CURSOR 'ShipviaValues'
SELECT distinct Spcinst 		FROM SourceCustomer WHERE Cdivision = "UPTOWN"  INTO CURSOR 'SpcinstValues'

**************************************************************************************************************

*ordhdr*
SELECT distinct Season 		    FROM SourceOrdhdr   WHERE Cdivision = "UPTOWN"  INTO CURSOR 'SeasonValuesOrder'
SELECT distinct CDivision	    FROM SourceOrdhdr   WHERE Cdivision = "UPTOWN"  INTO CURSOR 'CDivisionValues'
SELECT distinct CTermCode 	    FROM SourceOrdhdr   WHERE Cdivision = "UPTOWN"  INTO CURSOR 'CTermCodeValuesOrder'
SELECT distinct Shipvia 		FROM SourceOrdhdr   WHERE Cdivision = "UPTOWN"  INTO CURSOR 'ShipviaValuesOrder'
SELECT distinct Spcinst 		FROM SourceOrdhdr   WHERE Cdivision = "UPTOWN"  INTO CURSOR 'SpcinstValuesOrder'
SELECT distinct Ccancreson		FROM SourceOrdhdr   WHERE Cdivision = "UPTOWN"  INTO CURSOR 'CcancresonValues'
**************************************************************************************************************

*ordline*
SELECT distinct Season 		    FROM SourceOrdline   WHERE Cdivision = "UPTOWN"  INTO CURSOR 'SeasonValuesOrderLine'
**************************************************************************************************************

SELECT SourceCodes 
SET ORDER TO CCODE_NO   && CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM
SELECT DestinationCodes
SET ORDER TO CCODE_NO   && CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM

SELECT SourceStyle
SET ORDER TO STYLE   && STYLE

SELECT SourceWarehous
SET ORDER TO  

SELECT SourceCustomer 
SET ORDER TO CUSTOMER   && TYPE+ACCOUNT+STORE

SELECT DestinationCustomer 
SET ORDER TO CUSTOMER   && TYPE+ACCOUNT+STORE
SELECT DestinationStyle 
SET ORDER TO STYLE   && STYLE


**************************************************************************************************************
*style codes Start *
**************************************************************************************************************

SELECT 'ColorValues'
LOCATE
SCAN 
  valueC= ColorValues.Color
  IF !SEEK('N'+PADR('COLOR',10)+valueC,'DestinationCodes','CCODE_NO')
    SELECT SourceCodes 
    IF SEEK('N'+PADR('COLOR',10)+valueC,'SourceCodes' ,'CCODE_NO')
      SELECT SourceCodes 
      *APPEND FROM SourceCodes FOR CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM='N'+PADR('CDISCCODE',10)+valueC
      SCAN REST WHILE CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM='N'+PADR('COLOR',10)+valueC
        SCATTER MEMO MEMVAR 
        INSERT INTO DestinationCodes FROM MEMVAR  
      ENDSCAN
	ENDIF    
  ENDIF 
ENDSCAN 


*CdisccodeValues*
SELECT CdisccodeValues
LOCATE
SCAN 
  valueC= CdisccodeValues.cdisccode
  IF !SEEK('N'+PADR('CDISCCODE',10)+valueC,'DestinationCodes','CCODE_NO')
    SELECT SourceCodes 
    IF SEEK('N'+PADR('CDISCCODE',10)+valueC,'SourceCodes' ,'CCODE_NO')
      SELECT SourceCodes 
      *APPEND FROM SourceCodes FOR CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM='N'+PADR('CDISCCODE',10)+valueC
      SCAN REST WHILE CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM='N'+PADR('CDISCCODE',10)+valueC
        SCATTER MEMO MEMVAR 
        INSERT INTO DestinationCodes FROM MEMVAR  
      ENDSCAN
	ENDIF    
  ENDIF 
ENDSCAN 
**************************************************************************************************************
SELECT SeasonValuesOrderLine
LOCATE
SCAN 
  valueC= SeasonValuesOrderLine.season
  IF !SEEK('N'+PADR('SEASON',10)+valueC,'DestinationCodes','CCODE_NO')
    SELECT DestinationCodes
   * APPEND FROM SourceCodes FOR CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM='N'+PADR('SEASON',10)+valueC
*    INSERT INTO DestinationCodes SELECT * FROM SourceCodes WHERE CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM='N'+PADR('SEASON',10)+valueC
    SELECT SourceCodes 
    IF SEEK('N'+PADR('SEASON',10)+valueC,'SourceCodes' ,'CCODE_NO')
      SELECT SourceCodes 
      *APPEND FROM SourceCodes FOR CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM='N'+PADR('CDISCCODE',10)+valueC
      SCAN REST WHILE CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM='N'+PADR('SEASON',10)+valueC
        SCATTER MEMO MEMVAR 
        INSERT INTO DestinationCodes FROM MEMVAR  
      ENDSCAN
	ENDIF    

  ENDIF  
ENDSCAN 



*seasonValues*
SELECT seasonValues
LOCATE
SCAN 
  valueC= seasonValues.season
  IF !SEEK('N'+PADR('SEASON',10)+valueC,'DestinationCodes','CCODE_NO')
    SELECT DestinationCodes
   * APPEND FROM SourceCodes FOR CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM='N'+PADR('SEASON',10)+valueC
*    INSERT INTO DestinationCodes SELECT * FROM SourceCodes WHERE CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM='N'+PADR('SEASON',10)+valueC
    SELECT SourceCodes 
    IF SEEK('N'+PADR('SEASON',10)+valueC,'SourceCodes' ,'CCODE_NO')
      SELECT SourceCodes 
      *APPEND FROM SourceCodes FOR CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM='N'+PADR('CDISCCODE',10)+valueC
      SCAN REST WHILE CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM='N'+PADR('SEASON',10)+valueC
        SCATTER MEMO MEMVAR 
        INSERT INTO DestinationCodes FROM MEMVAR  
      ENDSCAN
	ENDIF    

  ENDIF  
ENDSCAN 

SELECT SeasonValuesOrder
LOCATE
SCAN 
  valueC= SeasonValuesOrder.season
  IF !SEEK('N'+PADR('SEASON',10)+valueC,'DestinationCodes','CCODE_NO')
    SELECT DestinationCodes
   * APPEND FROM SourceCodes FOR CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM='N'+PADR('SEASON',10)+valueC
*    INSERT INTO DestinationCodes SELECT * FROM SourceCodes WHERE CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM='N'+PADR('SEASON',10)+valueC
    SELECT SourceCodes 
    IF SEEK('N'+PADR('SEASON',10)+valueC,'SourceCodes' ,'CCODE_NO')
      SELECT SourceCodes 
      *APPEND FROM SourceCodes FOR CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM='N'+PADR('CDISCCODE',10)+valueC
      SCAN REST WHILE CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM='N'+PADR('SEASON',10)+valueC
        SCATTER MEMO MEMVAR 
        INSERT INTO DestinationCodes FROM MEMVAR  
      ENDSCAN
	ENDIF    
  ENDIF  
ENDSCAN 

**************************************************************************************************************

*royaltyValues*
SELECT royaltyValues
LOCATE
SCAN 
  valueC= royaltyValues.royalty
  IF !SEEK('N'+PADR('ROYALTY',10)+valueC,'DestinationCodes','CCODE_NO')
    SELECT DestinationCodes
   * APPEND FROM SourceCodes FOR CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM='N'+PADR('ROYALTY',10)+valueC
   * INSERT INTO DestinationCodes SELECT * FROM SourceCodes WHERE CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM='N'+PADR('ROYALTY',10)+valueC
    SELECT SourceCodes 
    IF SEEK('N'+PADR('ROYALTY',10)+valueC,'SourceCodes' ,'CCODE_NO')
      SELECT SourceCodes 
      *APPEND FROM SourceCodes FOR CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM='N'+PADR('CDISCCODE',10)+valueC
      SCAN REST WHILE CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM='N'+PADR('ROYALTY',10)+valueC
        SCATTER MEMO MEMVAR 
        INSERT INTO DestinationCodes FROM MEMVAR  
      ENDSCAN
	ENDIF
  ENDIF  
ENDSCAN 
**************************************************************************************************************

*CPURCODEValues*
SELECT CPURCODEValues
LOCATE
SCAN 
  valueC=CPURCODEValues.CPURCODE
  IF !SEEK('N'+PADR('CPURCODE',10)+valueC,'DestinationCodes','CCODE_NO')
    SELECT DestinationCodes
    *APPEND FROM SourceCodes FOR CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM='N'+PADR('CPURCODE',10)+valueC
*      INSERT INTO DestinationCodes SELECT * FROM SourceCodes WHERE CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM='N'+PADR('CPURCODE',10)+valueC
    SELECT SourceCodes 
    IF SEEK('N'+PADR('CPURCODE',10)+valueC,'SourceCodes' ,'CCODE_NO')
      SELECT SourceCodes 
      *APPEND FROM SourceCodes FOR CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM='N'+PADR('CDISCCODE',10)+valueC
      SCAN REST WHILE CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM='N'+PADR('CPURCODE',10)+valueC
        SCATTER MEMO MEMVAR 
        INSERT INTO DestinationCodes FROM MEMVAR  
      ENDSCAN
	ENDIF

  ENDIF 
ENDSCAN
**************************************************************************************************************

*CSTYGROUPValues*
SELECT CSTYGROUPValues
LOCATE
SCAN 
  valueC=CSTYGROUPValues.CSTYGROUP
  IF !SEEK('N'+PADR('CSTYGROUP',10)+valueC,'DestinationCodes','CCODE_NO')
    SELECT DestinationCodes
   * APPEND FROM SourceCodes FOR CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM='N'+PADR('CSTYGROUP',10)+valueC
     * INSERT INTO DestinationCodes SELECT * FROM SourceCodes WHERE CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM='N'+PADR('CSTYGROUP',10)+valueC
         SELECT SourceCodes 
    IF SEEK('N'+PADR('CSTYGROUP',10)+valueC,'SourceCodes' ,'CCODE_NO')
      SELECT SourceCodes 
      *APPEND FROM SourceCodes FOR CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM='N'+PADR('CDISCCODE',10)+valueC
      SCAN REST WHILE CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM='N'+PADR('CSTYGROUP',10)+valueC
        SCATTER MEMO MEMVAR 
        INSERT INTO DestinationCodes   FROM MEMVAR  
      ENDSCAN
	ENDIF
  ENDIF 
ENDSCAN
**************************************************************************************************************

*CTAXCODEValues*
SELECT CTAXCODEValues
LOCATE
SCAN 
  valueC=CTAXCODEValues.CTAXCODE
  IF !SEEK('N'+PADR('CTAXCODE',10)+valueC,'DestinationCodes','CCODE_NO')
    SELECT DestinationCodes
    *APPEND FROM SourceCodes FOR CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM='N'+PADR('CTAXCODE',10)+valueC
    *    INSERT INTO DestinationCodes SELECT * FROM SourceCodes WHERE CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM='N'+PADR('CTAXCODE',10)+valueC
     SELECT SourceCodes 
    IF SEEK('N'+PADR('CTAXCODE',10)+valueC,'SourceCodes' ,'CCODE_NO')
      SELECT SourceCodes 
      *APPEND FROM SourceCodes FOR CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM='N'+PADR('CDISCCODE',10)+valueC
      SCAN REST WHILE CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM='N'+PADR('CTAXCODE',10)+valueC
        SCATTER MEMO MEMVAR 
        INSERT INTO DestinationCodes FROM MEMVAR  
      ENDSCAN
	ENDIF
  ENDIF 
ENDSCAN
**************************************************************************************************************
*Customer Codes*
**************************************************************************************************************

*RegionValues*
SELECT RegionValues
LOCATE
SCAN 
  valueC=RegionValues.Region
  IF !SEEK('N'+PADR('REGION',10)+valueC,'DestinationCodes','CCODE_NO')
    SELECT DestinationCodes
    *APPEND FROM SourceCodes FOR CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM='N'+PADR('REGION',10)+valueC
  *  INSERT INTO DestinationCodes SELECT * FROM SourceCodes WHERE CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM='N'+PADR('REGION',10)+valueC
   SELECT SourceCodes 
    IF SEEK('N'+PADR('REGION',10)+valueC,'SourceCodes' ,'CCODE_NO')
      SELECT SourceCodes 
      *APPEND FROM SourceCodes FOR CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM='N'+PADR('CDISCCODE',10)+valueC
      SCAN REST WHILE CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM='N'+PADR('REGION',10)+valueC
        SCATTER MEMO MEMVAR 
        INSERT INTO DestinationCodes FROM MEMVAR  
      ENDSCAN
	ENDIF
  ENDIF 
ENDSCAN

*ClassValues*
SELECT ClassValues
LOCATE
SCAN 
  valueC=ClassValues.Class
  IF !SEEK('N'+PADR('CLASS',10)+valueC,'DestinationCodes','CCODE_NO')
    SELECT DestinationCodes
  * APPEND FROM SourceCodes FOR CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM='N'+PADR('CLASS',10)+valueC
     * INSERT INTO DestinationCodes SELECT * FROM SourceCodes WHERE CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM='N'+PADR('CLASS',10)+valueC
     SELECT SourceCodes 
    IF SEEK('N'+PADR('CLASS',10)+valueC,'SourceCodes' ,'CCODE_NO')
      SELECT SourceCodes 
      *APPEND FROM SourceCodes FOR CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM='N'+PADR('CDISCCODE',10)+valueC
      SCAN REST WHILE CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM='N'+PADR('CLASS',10)+valueC
        SCATTER MEMO MEMVAR 
        INSERT INTO DestinationCodes FROM MEMVAR  
      ENDSCAN
	ENDIF
  ENDIF 
ENDSCAN

SELECT CTermCodeValuesOrder
LOCATE
SCAN 
  valueC=CTermCodeValuesOrder.CTermCode
  IF !SEEK('N'+PADR('CTERMCODE',10)+valueC,'DestinationCodes','CCODE_NO')
    SELECT DestinationCodes
   * APPEND FROM SourceCodes FOR CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM='N'+PADR('CTERMCODE',10)+valueC
    *   INSERT INTO DestinationCodes SELECT * FROM SourceCodes WHERE CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM='N'+PADR('CTERMCODE',10)+valueC
      SELECT SourceCodes 
    IF SEEK('N'+PADR('CTERMCODE',10)+valueC,'SourceCodes' ,'CCODE_NO')
      SELECT SourceCodes 
      *APPEND FROM SourceCodes FOR CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM='N'+PADR('CDISCCODE',10)+valueC
      SCAN REST WHILE CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM='N'+PADR('CTERMCODE',10)+valueC
        SCATTER MEMO MEMVAR 
        INSERT INTO DestinationCodes FROM MEMVAR  
      ENDSCAN
	ENDIF
  ENDIF 
ENDSCAN


*CTermCodeValues*
SELECT CTermCodeValues
LOCATE
SCAN 
  valueC=CTermCodeValues.CTermCode
  IF !SEEK('N'+PADR('CTERMCODE',10)+valueC,'DestinationCodes','CCODE_NO')
    SELECT DestinationCodes
   * APPEND FROM SourceCodes FOR CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM='N'+PADR('CTERMCODE',10)+valueC
    *   INSERT INTO DestinationCodes SELECT * FROM SourceCodes WHERE CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM='N'+PADR('CTERMCODE',10)+valueC
      SELECT SourceCodes 
    IF SEEK('N'+PADR('CTERMCODE',10)+valueC,'SourceCodes' ,'CCODE_NO')
      SELECT SourceCodes 
      *APPEND FROM SourceCodes FOR CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM='N'+PADR('CDISCCODE',10)+valueC
      SCAN REST WHILE CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM='N'+PADR('CTERMCODE',10)+valueC
        SCATTER MEMO MEMVAR 
        INSERT INTO DestinationCodes FROM MEMVAR  
      ENDSCAN
	ENDIF
  ENDIF 
ENDSCAN

*ShipviaValues*
SELECT ShipviaValues
LOCATE
SCAN 
  valueC=ShipviaValues.Shipvia 
  IF !SEEK('N'+PADR('SHIPVIA',10)+valueC,'DestinationCodes','CCODE_NO')
    SELECT DestinationCodes
   * APPEND FROM SourceCodes FOR CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM='N'+PADR('SHIPVIA',10)+valueC
  *        INSERT INTO DestinationCodes SELECT * FROM SourceCodes WHERE CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM='N'+PADR('SHIPVIA',10)+valueC
     SELECT SourceCodes 
    IF SEEK('N'+PADR('SHIPVIA',10)+valueC,'SourceCodes' ,'CCODE_NO')
      SELECT SourceCodes 
      *APPEND FROM SourceCodes FOR CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM='N'+PADR('CDISCCODE',10)+valueC
      SCAN REST WHILE CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM='N'+PADR('SHIPVIA',10)+valueC
        SCATTER MEMO MEMVAR 
        INSERT INTO DestinationCodes FROM MEMVAR  
      ENDSCAN
	ENDIF
  ENDIF 
ENDSCAN

SELECT ShipviaValuesOrder
LOCATE
SCAN 
  valueC=ShipviaValuesOrder.Shipvia 
  IF !SEEK('N'+PADR('SHIPVIA',10)+valueC,'DestinationCodes','CCODE_NO')
    SELECT DestinationCodes
   * APPEND FROM SourceCodes FOR CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM='N'+PADR('SHIPVIA',10)+valueC
  *        INSERT INTO DestinationCodes SELECT * FROM SourceCodes WHERE CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM='N'+PADR('SHIPVIA',10)+valueC
     SELECT SourceCodes 
    IF SEEK('N'+PADR('SHIPVIA',10)+valueC,'SourceCodes' ,'CCODE_NO')
      SELECT SourceCodes 
      *APPEND FROM SourceCodes FOR CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM='N'+PADR('CDISCCODE',10)+valueC
      SCAN REST WHILE CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM='N'+PADR('SHIPVIA',10)+valueC
        SCATTER MEMO MEMVAR 
        INSERT INTO DestinationCodes FROM MEMVAR  
      ENDSCAN
	ENDIF
  ENDIF 
ENDSCAN

*SpcinstValues*
SELECT SpcinstValues
LOCATE
SCAN 
  valueC=SpcinstValues.Spcinst
  IF !SEEK('N'+PADR('SPCINST',10)+valueC,'DestinationCodes','CCODE_NO')
    SELECT DestinationCodes
  *  APPEND FROM SourceCodes FOR CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM='N'+PADR('SPCINST',10)+valueC
           * INSERT INTO DestinationCodes SELECT * FROM SourceCodes WHERE  CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM='N'+PADR('SPCINST',10)+valueC
  SELECT SourceCodes 
    IF SEEK('N'+PADR('SPCINST',10)+valueC,'SourceCodes' ,'CCODE_NO')
      SELECT SourceCodes 
      *APPEND FROM SourceCodes FOR CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM='N'+PADR('CDISCCODE',10)+valueC
      SCAN REST WHILE CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM='N'+PADR('SPCINST',10)+valueC
        SCATTER MEMO MEMVAR 
        INSERT INTO DestinationCodes FROM MEMVAR  
      ENDSCAN
	ENDIF
  ENDIF 
ENDSCAN

SELECT SpcinstValuesOrder
LOCATE
SCAN 
  valueC=SpcinstValuesOrder.Spcinst
  IF !SEEK('N'+PADR('SPCINST',10)+valueC,'DestinationCodes','CCODE_NO')
    SELECT DestinationCodes
  *  APPEND FROM SourceCodes FOR CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM='N'+PADR('SPCINST',10)+valueC
           * INSERT INTO DestinationCodes SELECT * FROM SourceCodes WHERE  CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM='N'+PADR('SPCINST',10)+valueC
  SELECT SourceCodes 
    IF SEEK('N'+PADR('SPCINST',10)+valueC,'SourceCodes' ,'CCODE_NO')
      SELECT SourceCodes 
      *APPEND FROM SourceCodes FOR CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM='N'+PADR('CDISCCODE',10)+valueC
      SCAN REST WHILE CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM='N'+PADR('SPCINST',10)+valueC
        SCATTER MEMO MEMVAR 
        INSERT INTO DestinationCodes FROM MEMVAR  
      ENDSCAN
	ENDIF
  ENDIF 
ENDSCAN

**************************************************************************************************************
*ordhdr Codes*
**************************************************************************************************************


*SeasonValues*
SELECT SeasonValues
LOCATE
SCAN 
  valueC=SeasonValues.Season
  IF !SEEK('N'+PADR('SEASON',10)+valueC,'DestinationCodes','CCODE_NO')
    SELECT DestinationCodes
   * APPEND FROM SourceCodes FOR CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM='N'+PADR('SEASON',10)+valueC
       *       INSERT INTO DestinationCodes SELECT * FROM SourceCodes WHERE  CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM='N'+PADR('SEASON',10)+valueC
  SELECT SourceCodes 
    IF SEEK('N'+PADR('SEASON',10)+valueC,'SourceCodes' ,'CCODE_NO')
      SELECT SourceCodes 
      *APPEND FROM SourceCodes FOR CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM='N'+PADR('CDISCCODE',10)+valueC
      SCAN REST WHILE CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM='N'+PADR('SEASON',10)+valueC
        SCATTER MEMO MEMVAR 
        INSERT INTO DestinationCodes FROM MEMVAR  
      ENDSCAN
	ENDIF
  ENDIF 
ENDSCAN

*CDivisionValues*
SELECT CDivisionValues
LOCATE
SCAN 
  valueC=CDivisionValues.CDivision
  IF !SEEK('N'+PADR('CDIVISION',10)+valueC,'DestinationCodes','CCODE_NO')
    SELECT DestinationCodes
   * APPEND FROM SourceCodes FOR CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM='N'+PADR('CDivision',10)+valueC
         *     INSERT INTO DestinationCodes SELECT * FROM SourceCodes WHERE  CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM='N'+PADR('CDIVISION',10)+valueC
   SELECT SourceCodes 
    IF SEEK('N'+PADR('CDIVISION',10)+valueC,'SourceCodes' ,'CCODE_NO')
      SELECT SourceCodes 
      *APPEND FROM SourceCodes FOR CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM='N'+PADR('CDISCCODE',10)+valueC
      SCAN REST WHILE CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM='N'+PADR('CDIVISION',10)+valueC
        SCATTER MEMO MEMVAR 
        INSERT INTO DestinationCodes FROM MEMVAR  
      ENDSCAN
	ENDIF
  ENDIF 
ENDSCAN

*CTermCodeValues*
SELECT CTermCodeValues
LOCATE
SCAN 
  valueC=CTermCodeValues.CTermCode
  IF !SEEK('N'+PADR('CTERMCODE',10)+valueC,'DestinationCodes','CCODE_NO')
    SELECT DestinationCodes
   * APPEND FROM SourceCodes FOR CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM='N'+PADR('CTERMCODE',10)+valueC
    *    INSERT INTO DestinationCodes SELECT * FROM SourceCodes WHERE CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM='N'+PADR('CTERMCODE',10)+valueC
 SELECT SourceCodes 
    IF SEEK('N'+PADR('CTERMCODE',10)+valueC,'SourceCodes' ,'CCODE_NO')
      SELECT SourceCodes 
      *APPEND FROM SourceCodes FOR CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM='N'+PADR('CDISCCODE',10)+valueC
      SCAN REST WHILE CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM='N'+PADR('CTERMCODE',10)+valueC
        SCATTER MEMO MEMVAR 
        INSERT INTO DestinationCodes FROM MEMVAR  
      ENDSCAN
	ENDIF
  ENDIF 
ENDSCAN

*ShipviaValues*
SELECT ShipviaValues
LOCATE
SCAN 
  valueC=ShipviaValues.Shipvia 
  IF !SEEK('N'+PADR('SHIPVIA',10)+valueC,'DestinationCodes','CCODE_NO')
    SELECT DestinationCodes
    *APPEND FROM SourceCodes FOR CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM='N'+PADR('SHIPVIA',10)+valueC
        *  INSERT INTO DestinationCodes SELECT * FROM SourceCodes WHERE CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM='N'+PADR('SHIPVIA',10)+valueC
  SELECT SourceCodes 
    IF SEEK('N'+PADR('SHIPVIA',10)+valueC,'SourceCodes' ,'CCODE_NO')
      SELECT SourceCodes 
      *APPEND FROM SourceCodes FOR CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM='N'+PADR('CDISCCODE',10)+valueC
      SCAN REST WHILE CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM='N'+PADR('SHIPVIA',10)+valueC
        SCATTER MEMO MEMVAR 
        INSERT INTO DestinationCodes FROM MEMVAR  
      ENDSCAN
	ENDIF
  ENDIF 
ENDSCAN

*SpcinstValues*
SELECT SpcinstValues
LOCATE
SCAN 
  valueC=SpcinstValues.Spcinst
  IF !SEEK('N'+PADR('SPCINST',10)+valueC,'DestinationCodes','CCODE_NO')
    SELECT DestinationCodes
   * APPEND FROM SourceCodes FOR CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM='N'+PADR('SPCINST',10)+valueC
          *  INSERT INTO DestinationCodes SELECT * FROM SourceCodes WHERE CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM='N'+PADR('SPCINST',10)+valueC
 SELECT SourceCodes 
    IF SEEK('N'+PADR('SPCINST',10)+valueC,'SourceCodes' ,'CCODE_NO')
      SELECT SourceCodes 
      *APPEND FROM SourceCodes FOR CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM='N'+PADR('CDISCCODE',10)+valueC
      SCAN REST WHILE CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM='N'+PADR('SPCINST',10)+valueC
        SCATTER MEMO MEMVAR 
        INSERT INTO DestinationCodes FROM MEMVAR  
      ENDSCAN
	ENDIF
  ENDIF 
ENDSCAN

*CcancresonValues*
SELECT CcancresonValues
LOCATE
SCAN 
  valueC=CcancresonValues.Ccancreson
  IF !SEEK('N'+PADR('CCANCRESON',10)+valueC,'DestinationCodes','CCODE_NO')
    SELECT DestinationCodes
    *APPEND FROM SourceCodes FOR CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM='N'+PADR('CCANCRESON',10)+valueC
           *   INSERT INTO DestinationCodes SELECT * FROM SourceCodes WHERE CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM='N'+PADR('CCANCRESON',10)+valueC
 SELECT SourceCodes 
    IF SEEK('N'+PADR('CCANCRESON',10)+valueC,'SourceCodes' ,'CCODE_NO')
      SELECT SourceCodes 
      *APPEND FROM SourceCodes FOR CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM='N'+PADR('CDISCCODE',10)+valueC
      SCAN REST WHILE CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM='N'+PADR('CCANCRESON',10)+valueC
        SCATTER MEMO MEMVAR 
        INSERT INTO DestinationCodes FROM MEMVAR  
      ENDSCAN
	ENDIF
  ENDIF 
ENDSCAN

**************************************************************************************************************
*ordline Codes*
**************************************************************************************************************

*SeasonValues*
SELECT SeasonValues
LOCATE
SCAN 
  valueC=SeasonValues.Season
  IF !SEEK('N'+PADR('SEASON',10)+valueC,'DestinationCodes','CCODE_NO')
    SELECT DestinationCodes
  *  APPEND FROM SourceCodes FOR CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM='N'+PADR('SEASON',10)+valueC
            *    INSERT INTO DestinationCodes SELECT * FROM SourceCodes WHERE CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM='N'+PADR('SEASON',10)+valueC
 SELECT SourceCodes 
    IF SEEK('N'+PADR('SEASON',10)+valueC,'SourceCodes' ,'CCODE_NO')
      SELECT SourceCodes 
      *APPEND FROM SourceCodes FOR CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM='N'+PADR('CDISCCODE',10)+valueC
      SCAN REST WHILE CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM='N'+PADR('SEASON',10)+valueC
        SCATTER MEMO MEMVAR 
        INSERT INTO DestinationCodes FROM MEMVAR  
      ENDSCAN
	ENDIF
  ENDIF 
ENDSCAN

**************************************************************************************************************
*-- Append Codes End-- *
**************************************************************************************************************


**************************************************************************************************************
*-- Append Stydy, StyleUPC, Scale Start-- *
**************************************************************************************************************
SELECT DestinationStyle 
LOCATE
SCAN FOR Cdivision = "UPTOWN"  
   valueC= DestinationStyle.Style
   
   IF !SEEK(valueC,'DestinationStydye','STYDYE')
    SELECT DestinationStydye
    *APPEND FROM SourceStydye FOR Style=valueC
   * INSERT INTO DestinationStydye SELECT * FROM SourceStydye WHERE Style=valueC
    SELECT SourceStydye 
    IF SEEK(valueC,'SourceStydye','STYDYE')
      SELECT SourceStydye 
     
      SCAN REST WHILE Style=valueC
        SCATTER MEMO MEMVAR 
        INSERT INTO DestinationStydye FROM MEMVAR  
      ENDSCAN
	ENDIF
   ENDIF
   
   IF !SEEK(valueC,'DestinationStyleUpc','STYLEUPC')
     SELECT DestinationStyleUpc
   *  APPEND FROM SourceStyleUpc FOR Style=valueC
 *  INSERT INTO DestinationStyleUpc SELECT * FROM SourceStyleUpc WHERE Style=valueC
 SELECT SourceStyleUpc 
    IF SEEK(valueC,'SourceStyleUpc','STYLEUPC')
      SELECT SourceStyleUpc 
     
      SCAN REST WHILE Style=valueC
        SCATTER MEMO MEMVAR 
        INSERT INTO DestinationStyleUpc FROM MEMVAR  
      ENDSCAN
	ENDIF
   ENDIF 
   
   IF !SEEK("S"+DestinationStyle.Scale,'DestinationScale','SCALE')
     SELECT DestinationScale
   *  APPEND FROM SourceScale FOR Type+scale="S"+DestinationStyle.Scale
   *INSERT INTO DestinationScale SELECT * FROM SourceScale  WHERE Type+scale="S"+DestinationStyle.Scale
 
  SELECT SourceScale 
    IF SEEK("S"+DestinationStyle.Scale,'SourceScale','SCALE')
      SELECT SourceScale 
     
      SCAN REST WHILE Type+scale="S"+DestinationStyle.Scale
        SCATTER MEMO MEMVAR 
        INSERT INTO DestinationScale FROM MEMVAR  
      ENDSCAN
	ENDIF
   ENDIF 
ENDSCAN

**************************************************************************************************************
*-- Append Stydy, StyleUPC, Scale End-- *
**************************************************************************************************************

**************************************************************************************************************
*-- Append Warehous Start-- *
**************************************************************************************************************
SELECT distinct Cwarecode FROM DestinationStydye INTO CURSOR 'CwarecodeValues'
SELECT CwarecodeValues
LOCATE
SCAN 
  valueC=CwarecodeValues.Cwarecode 
  IF !SEEK(valueC,'DestinationWarehous','WAREHOUS')
    SELECT DestinationWarehous
    *APPEND FROM SourceWarehous FOR Cwarecode = valueC
  *INSERT INTO DestinationWarehous SELECT * FROM SourceWarehous WHERE Cwarecode = valueC

 SELECT SourceWarehous 
    IF SEEK(valueC,'SourceWarehous','WAREHOUS')
      SELECT SourceWarehous 
     
      SCAN REST WHILE Cwarecode = valueC
        SCATTER MEMO MEMVAR 
        INSERT INTO DestinationWarehous FROM MEMVAR  
      ENDSCAN
	ENDIF
  ENDIF 
ENDSCAN

**************************************************************************************************************
*-- Append Warehous End-- *
**************************************************************************************************************


**************************************************************************************************************
*-- Append Ordline Start-- *
**************************************************************************************************************
SELECT DestinationOrdline 
*APPEND FROM OrderlineRangeValues
*INSERT INTO DestinationOrdline SELECT * FROM OrderlineRangeValues
    SELECT OrderlineRangeValues

      SCAN 
        SCATTER MEMO MEMVAR 
        INSERT INTO DestinationOrdline FROM MEMVAR  
      ENDSCAN
	
   
SELECT DestinationOrdline 
LOCATE
SCAN 
  valueC=DestinationOrdline.Style
  
  IF !SEEK(valueC,'DestinationStyle','STYLE')
  MESSAGEBOX("Style:"+ valueC +" is not found in the style file")  
  ENDIF 
    
ENDSCAN
**************************************************************************************************************
*-- Append Ordline End *
**************************************************************************************************************

**************************************************************************************************************
*-- Append Ordhdr Start*
**************************************************************************************************************
SELECT DestinationOrdhdr
*APPEND FROM OrderHeaderRangeValues
*INSERT INTO DestinationOrdhdr SELECT * FROM OrderHeaderRangeValues

 SELECT OrderHeaderRangeValues

      SCAN 
        SCATTER MEMO MEMVAR 
        INSERT INTO DestinationOrdhdr FROM MEMVAR  
      ENDSCAN
**************************************************************************************************************
*-- Append Ordhdr End *
**************************************************************************************************************


**************************************************************************************************************
*-- Append Customer,ARCUSTHST  Start*
**************************************************************************************************************
SELECT distinct Account 	   FROM DestinationOrdhdr INTO CURSOR 'AccountValues'

SELECT AccountValues
LOCATE
SCAN 
   valueC= AccountValues.Account
   
   IF !SEEK("M"+valueC,'DestinationCustomer','Customer')
     SELECT DestinationCustomer
    * APPEND FROM SourceCustomer FOR ACCOUNT = valueC
  *INSERT INTO DestinationCustomer SELECT * FROM SourceCustomer WHERE ACCOUNT = valueC
  
  SELECT SourceCustomer 
    IF SEEK("M"+valueC,'SourceCustomer','Customer')
      SELECT SourceCustomer 
     
      SCAN FOR ACCOUNT = valueC
        SCATTER MEMO MEMVAR 
        INSERT INTO DestinationCustomer FROM MEMVAR  
      ENDSCAN
	ENDIF
	
  ENDIF 
  
   IF !SEEK(valueC,'DestinationARCUSTHST','ACTHST')
    SELECT DestinationARCUSTHST
   * APPEND FROM SourceARCUSTHST FOR ACCOUNT = valueC
  * INSERT INTO DestinationARCUSTHST SELECT * FROM SourceARCUSTHST WHERE ACCOUNT = valueC
  
  SELECT SourceARCUSTHST 
    IF SEEK(valueC,'SourceARCUSTHST','ACTHST')
      SELECT SourceARCUSTHST 
     
      SCAN REST WHILE ACCOUNT = valueC
        SCATTER MEMO MEMVAR 
        INSERT INTO DestinationARCUSTHST FROM MEMVAR  
      ENDSCAN
	ENDIF
ENDIF 	
ENDSCAN
MESSAGEBOX("copying is finished successfully.")
**************************************************************************************************************
*-- Append Customer,ARCUSTHST  End*
**************************************************************************************************************



**************************************************************************************************************
*-- SQL Table Start*
**************************************************************************************************************
*!*	Source_Sysfiles = "X:\ARIA4XP\SYSFILES\"
*!*	USE (Source_Sysfiles+"SYCCOMP.DBF") SHARED ORDER CCOMP_ID   && CCOMP_ID
*!*	SELECT SYCCOMP
*!*	=SEEK("01",'SYCCOMP',"CCOMP_ID")
*!*	lcSrcConnStr = "Driver={SQL Server};server="+ALLTRIM(CCONSERVER)+";DATABASE="+ALLTRIM(CCONDBNAME)+;
*!*	                ";uid="+ALLTRIM(CCONUSERID)+";pwd="+ALLTRIM(CCONPASWRD)
*!*	SELECT SYCCOMP
*!*	=SEEK("04",'SYCCOMP',"CCOMP_ID")
*!*	lcDstConnStr = "Driver={SQL Server};server="+ALLTRIM(CCONSERVER)+";DATABASE="+ALLTRIM(CCONDBNAME)+;
*!*	                ";uid="+ALLTRIM(CCONUSERID)+";pwd="+ALLTRIM(CCONPASWRD)
*!*	USE IN SYCCOMP                 
*!*	lnSrcHand = SQLSTRINGCONNECT(lcSrcConnStr) 
*!*	lnDstHand = SQLSTRINGCONNECT(lcDstConnStr) 

*!*	SELECT DestinationStyle
*!*	LOCATE 
*!*	SCAN FOR Cdivision = "UPTOWN" 
*!*	  lnResult = SQLEXEC(lnSrcHand ,"Select * from ICSTYHST where Style ='"+DestinationStyle.Style+"'","SrcFile")
*!*	  IF lnResult > 0
*!*	    SELECT SrcFile
*!*	    LOCATE
*!*	    SCAN 
*!*	      lcInsertStat 
*!*	      
*!*	    ENDIF 
*!*	  ENDIF
*!*	ENDSCAN
**************************************************************************************************************
*-- SQL Table End*
**************************************************************************************************************