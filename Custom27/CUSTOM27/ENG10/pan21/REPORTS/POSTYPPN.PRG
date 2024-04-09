*:***************************************************************************
*: Program file  : POSTYPPN
*: Program desc. : Print PO,Form for panach C129442 [END]
*: For Report    : POSTYPPN.FRX
*: System        : Aria Advantage Series VER. 2.7
*: Module        : Purchase Order (PO)
*: Developer     : Ehab Ismail Hamed
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : lfGetCodes, lfEndGroup, lfwOldVal, lfvPO, lfvVend, lfGetLogo,
*:                 lfShiftArr
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example : DO POSTYPO
*:***************************************************************************
*!C129442,1 EIH 10/23/2005 Custom Form for Panach.
*:**********************************************************************************************
DIMENSION laVenAdr[1,8]
*varaible to hold the created temperory file.
lcSlctdLns=gfTempName()
lcNewScl  =gfTempName()
lcPolnTmp =gfTempName()
lcPolnTmp2=gfTempName()
lcSclsTemp=gfTempName()

STORE '' TO lcVenFax , lcVenPhone , lcStPoNote 
=lfStPoNote()
SET DELETE ON
SET CENTURY ON
*Index on SYDFIELD to get the required description for the user defined fields
lcFileOrd = ORDER('SYDFIELD')
SELECT SYDFIELD
SET ORDER TO 1
*Get arrays of codes and description of user defined field
DIMENSION laMerch[1,2] , laMerchTel[1,2] , laMerchFax[1,2],laGarTech[1,2],laGTechTel[1,2],laGTechFax[1,2]
STORE '' TO laMerch, laMerchTel, laMerchFax,laGarTech,laGTechTel,laGTechFax

=lfUDFDesc('CMERCH',@laMerch)
=lfUDFDesc('CMERCHTEL',@laMerchTel)
=lfUDFDesc('CMERCHFAX',@laMerchFax)
=lfUDFDesc('CGTECH',@laGarTech)
=lfUDFDesc('CGTECHTEL',@laGTechTel)
=lfUDFDesc('CGTECHFAX',@laGTechFax)
IF !USED('BOM')
    =gfOpenFile(gcDataDir+'BOM','BOM','SH')
ENDIF

* Get the company addresses
STORE '' TO laCompAdd
laCompAdd[1]    = gfGetAdr('SYCCOMP' , '' , '' , '' , 1)
laCompAdd[2]    = gfGetAdr('SYCCOMP' , '' , '' , '' , 2)
laCompAdd[3]    = gfGetAdr('SYCCOMP' , '' , '' , '' , 3)
laCompAdd[4]    = gfGetAdr('SYCCOMP' , '' , '' , '' , 4)
laCompAdd[5]    = gfGetAdr('SYCCOMP' , '' , '' , '' , 5)


= lfCreatTmp()
= lfColData()
*REINDEX THE FILE WHEN EXIT
IF !EMPTY(lcFileOrd)
  SELECT SYDFIELD
  SET ORDER TO &lcOrder
ELSE
  SELECT SYDFIELD
  SET ORDER TO
ENDIF
IF USED('BOM')
   USE IN ('BOM')
ENDIF

SELECT (lcPolnTmp)
SET ORDER TO lcPolnTmp2

*Set relation To codes file to can print the color discription
SELECT CODES
SET ORDER TO Codes
SELECT (lcPolnTmp)
SET RELATION TO 'N'+&lcPolnTmp..COLOR+'N'+'COLOR' INTO CODES ADDATIVE
LOCATE


*!**************************************************************************
*! Name      : lfCreatTmp
*! Developer : Ehab Ismail Hamed Mohamed
*: Date      : 10/10/2005
*! Purpose   : To create the temporary file of posln
*!**************************************************************************
*! Example   : =lfCreatTmp()
*!**************************************************************************
FUNCTION lfCreatTmp
*Create lcSlctdLns temperory file which contains poslines for selected po's
SELECT POSLN 
lnFStru = AFIELDS(laFStru)
DIMENSION laFStru[lnFStru+13,4]

laFStru[lnFStru+1,1] = 'SZ1'
laFStru[lnFStru+1,2] = 'C'
laFStru[lnFStru+1,3] = 5
laFStru[lnFStru+1,4] = 0

laFStru[lnFStru+2,1] = 'SZ2'
laFStru[lnFStru+2,2] = 'C'
laFStru[lnFStru+2,3] = 5
laFStru[lnFStru+2,4] = 0

laFStru[lnFStru+3,1] = 'SZ3'
laFStru[lnFStru+3,2] = 'C'
laFStru[lnFStru+3,3] = 5
laFStru[lnFStru+3,4] = 0

laFStru[lnFStru+4,1] = 'SZ4'
laFStru[lnFStru+4,2] = 'C'
laFStru[lnFStru+4,3] = 5
laFStru[lnFStru+4,4] = 0

laFStru[lnFStru+5,1] = 'SZ5'
laFStru[lnFStru+5,2] = 'C'
laFStru[lnFStru+5,3] = 5
laFStru[lnFStru+5,4] = 0

laFStru[lnFStru+6,1] = 'SZ6'
laFStru[lnFStru+6,2] = 'C'
laFStru[lnFStru+6,3] = 5
laFStru[lnFStru+6,4] = 0

laFStru[lnFStru+7,1] = 'SZ7'
laFStru[lnFStru+7,2] = 'C'
laFStru[lnFStru+7,3] = 5
laFStru[lnFStru+7,4] = 0

laFStru[lnFStru+8,1] = 'SZ8'
laFStru[lnFStru+8,2] = 'C'
laFStru[lnFStru+8,3] = 5
laFStru[lnFStru+8,4] = 0

laFStru[lnFStru+9,1] = 'cnt'
laFStru[lnFStru+9,2] = 'N'
laFStru[lnFStru+9,3] = 1
laFStru[lnFStru+9,4] = 0

laFStru[lnFStru+10,1] = 'stymaj'
laFStru[lnFStru+10,2] = 'C'
laFStru[lnFStru+10,3] = 19
laFStru[lnFStru+10,4] = 0

laFStru[lnFStru+11,1] = 'COLOR' 
laFStru[lnFStru+11,2] = 'C'
laFStru[lnFStru+11,3] = 6
laFStru[lnFStru+11,4] = 0

laFStru[lnFStru+12,1] = 'Desc1'
laFStru[lnFStru+12,2] = 'C'
laFStru[lnFStru+12,3] = 60
laFStru[lnFStru+12,4] = 0

laFStru[lnFStru+13,1] = 'DifPrice'
laFStru[lnFStru+13,2] = 'C'
laFStru[lnFStru+13,3] = 1
laFStru[lnFStru+13,4] = 0





=gfCrtTmp(lcSlctdLns,@laFStru,'PO+stymaj+Scale',lcSlctdLns)

*Create lcPolnTmp base temperory file which contains posln's with grouped size scale for selected po's
DIMENSION laPolnTmp[77,4]

laPolnTmp[1,1] = 'cstytype'
laPolnTmp[1,2] = 'C'
laPolnTmp[1,3] = 1
laPolnTmp[1,4] = 0

laPolnTmp[2,1] = 'PO'
laPolnTmp[2,2] = 'C'
laPolnTmp[2,3] = 6
laPolnTmp[2,4] = 0

laPolnTmp[3,1] = 'trancd'
laPolnTmp[3,2] = 'C'
laPolnTmp[3,3] = 1
laPolnTmp[3,4] = 0

laPolnTmp[4,1] = 'lineno'
laPolnTmp[4,2] = 'N'
laPolnTmp[4,3] = 6
laPolnTmp[4,4] = 0

laPolnTmp[5,1] = 'vendor'
laPolnTmp[5,2] = 'C'
laPolnTmp[5,3] = 8
laPolnTmp[5,4] = 0

laPolnTmp[6,1] = 'style'
laPolnTmp[6,2] = 'C'
laPolnTmp[6,3] = 19
laPolnTmp[6,4] = 0

laPolnTmp[7,1] = 'scale'
laPolnTmp[7,2] = 'C'
laPolnTmp[7,3] = 15
laPolnTmp[7,4] = 0

laPolnTmp[8,1] = 'dyelot'
laPolnTmp[8,2] = 'C'
laPolnTmp[8,3] = 10
laPolnTmp[8,4] = 0

laPolnTmp[9,1] = 'prepak'
laPolnTmp[9,2] = 'C'
laPolnTmp[9,3] = 1
laPolnTmp[9,4] = 0

laPolnTmp[10,1] = 'ppqty'
laPolnTmp[10,2] = 'N'
laPolnTmp[10,3] = 4
laPolnTmp[10,4] = 0

laPolnTmp[11,1] = 'qty1'
laPolnTmp[11,2] = 'N'
laPolnTmp[11,3] = 6
laPolnTmp[11,4] = 0

laPolnTmp[12,1] = 'qty2'
laPolnTmp[12,2] = 'N'
laPolnTmp[12,3] = 6
laPolnTmp[12,4] = 0

laPolnTmp[13,1] = 'qty3'
laPolnTmp[13,2] = 'N'
laPolnTmp[13,3] = 6
laPolnTmp[13,4] = 0

laPolnTmp[14,1] = 'qty4'
laPolnTmp[14,2] = 'N'
laPolnTmp[14,3] = 6
laPolnTmp[14,4] = 0

laPolnTmp[15,1] = 'qty5'
laPolnTmp[15,2] = 'N'
laPolnTmp[15,3] = 6
laPolnTmp[15,4] = 0

laPolnTmp[16,1] = 'qty6'
laPolnTmp[16,2] = 'N'
laPolnTmp[16,3] = 6
laPolnTmp[16,4] = 0

laPolnTmp[17,1] = 'qty7'
laPolnTmp[17,2] = 'N'
laPolnTmp[17,3] = 6
laPolnTmp[17,4] = 0

laPolnTmp[18,1] = 'qty8'
laPolnTmp[18,2] = 'N'
laPolnTmp[18,3] = 6
laPolnTmp[18,4] = 0

laPolnTmp[19,1] = 'qty9'
laPolnTmp[19,2] = 'N'
laPolnTmp[19,3] = 6
laPolnTmp[19,4] = 0

laPolnTmp[20,1] = 'qty10'
laPolnTmp[20,2] = 'N'
laPolnTmp[20,3] = 6
laPolnTmp[20,4] = 0

laPolnTmp[21,1] = 'qty11'
laPolnTmp[21,2] = 'N'
laPolnTmp[21,3] = 6
laPolnTmp[21,4] = 0

laPolnTmp[22,1] = 'qty12'
laPolnTmp[22,2] = 'N'
laPolnTmp[22,3] = 6
laPolnTmp[22,4] = 0

laPolnTmp[23,1] = 'qty13'
laPolnTmp[23,2] = 'N'
laPolnTmp[23,3] = 6
laPolnTmp[23,4] = 0

laPolnTmp[24,1] = 'pricekey'
laPolnTmp[24,2] = 'C'
laPolnTmp[24,3] = 120
laPolnTmp[24,4] = 0

laPolnTmp[25,1] = 'totlnqty'
laPolnTmp[25,2] = 'N'
laPolnTmp[25,3] = 7
laPolnTmp[25,4] = 0

laPolnTmp[26,1] = 'totqty'
laPolnTmp[26,2] = 'N'
laPolnTmp[26,3] = 7
laPolnTmp[26,4] = 0

laPolnTmp[27,1] = 'cwarecode'
laPolnTmp[27,2] = 'C'
laPolnTmp[27,3] = 6
laPolnTmp[27,4] = 0

laPolnTmp[28,1] = 'account'
laPolnTmp[28,2] = 'C'
laPolnTmp[28,3] = 5
laPolnTmp[28,4] = 0

laPolnTmp[29,1] = 'store'
laPolnTmp[29,2] = 'C'
laPolnTmp[29,3] = 8
laPolnTmp[29,4] = 0

laPolnTmp[30,1] = 'cvensty'
laPolnTmp[30,2] = 'C'
laPolnTmp[30,3] = 19
laPolnTmp[30,4] = 0

laPolnTmp[31,1] = 'price1'
laPolnTmp[31,2] = 'N'
laPolnTmp[31,3] = 12
laPolnTmp[31,4] = 2

laPolnTmp[32,1] = 'price2'
laPolnTmp[32,2] = 'N'
laPolnTmp[32,3] = 12
laPolnTmp[32,4] = 2

laPolnTmp[33,1] = 'price3'
laPolnTmp[33,2] = 'N'
laPolnTmp[33,3] = 12
laPolnTmp[33,4] = 2

laPolnTmp[34,1] = 'price4'
laPolnTmp[34,2] = 'N'
laPolnTmp[34,3] = 12
laPolnTmp[34,4] = 2

laPolnTmp[35,1] = 'price5'
laPolnTmp[35,2] = 'N'
laPolnTmp[35,3] = 12
laPolnTmp[35,4] = 2

laPolnTmp[36,1] = 'price6'
laPolnTmp[36,2] = 'N'
laPolnTmp[36,3] = 12
laPolnTmp[36,4] = 2

laPolnTmp[37,1] = 'price7'
laPolnTmp[37,2] = 'N'
laPolnTmp[37,3] = 12
laPolnTmp[37,4] = 2

laPolnTmp[38,1] = 'price8'
laPolnTmp[38,2] = 'N'
laPolnTmp[38,3] = 12
laPolnTmp[38,4] = 2

laPolnTmp[39,1] = 'price9'
laPolnTmp[39,2] = 'N'
laPolnTmp[39,3] = 12
laPolnTmp[39,4] = 2

laPolnTmp[40,1] = 'price10'
laPolnTmp[40,2] = 'N'
laPolnTmp[40,3] = 12
laPolnTmp[40,4] = 2

laPolnTmp[41,1] = 'price11'
laPolnTmp[41,2] = 'N'
laPolnTmp[41,3] = 12
laPolnTmp[41,4] = 2

laPolnTmp[42,1] = 'price12'
laPolnTmp[42,2] = 'N'
laPolnTmp[42,3] = 12
laPolnTmp[42,4] = 2

laPolnTmp[43,1] = 'price13'
laPolnTmp[43,2] = 'N'
laPolnTmp[43,3] = 12
laPolnTmp[43,4] = 2

laPolnTmp[44,1] = 'sz1'
laPolnTmp[44,2] = 'C'
laPolnTmp[44,3] = 5
laPolnTmp[44,4] = 0

laPolnTmp[45,1] = 'sz2'
laPolnTmp[45,2] = 'C'
laPolnTmp[45,3] = 5
laPolnTmp[45,4] = 0

laPolnTmp[46,1] = 'sz3'
laPolnTmp[46,2] = 'C'
laPolnTmp[46,3] = 5
laPolnTmp[46,4] = 0

laPolnTmp[47,1] = 'sz4'
laPolnTmp[47,2] = 'C'
laPolnTmp[47,3] = 5
laPolnTmp[47,4] = 0

laPolnTmp[48,1] = 'sz5'
laPolnTmp[48,2] = 'C'
laPolnTmp[48,3] = 5
laPolnTmp[48,4] = 0

laPolnTmp[49,1] = 'sz6'
laPolnTmp[49,2] = 'C'
laPolnTmp[49,3] = 5
laPolnTmp[49,4] = 0

laPolnTmp[50,1] = 'sz7'
laPolnTmp[50,2] = 'C'
laPolnTmp[50,3] = 5
laPolnTmp[50,4] = 0

laPolnTmp[51,1] = 'sz8'
laPolnTmp[51,2] = 'C'
laPolnTmp[51,3] = 5
laPolnTmp[51,4] = 0

laPolnTmp[52,1] = 'sz9'
laPolnTmp[52,2] = 'C'
laPolnTmp[52,3] = 5
laPolnTmp[52,4] = 0

laPolnTmp[53,1] = 'sz10'
laPolnTmp[53,2] = 'C'
laPolnTmp[53,3] = 5
laPolnTmp[53,4] = 0

laPolnTmp[54,1] = 'sz11'
laPolnTmp[54,2] = 'C'
laPolnTmp[54,3] = 5
laPolnTmp[54,4] = 0

laPolnTmp[55,1] = 'sz12'
laPolnTmp[55,2] = 'C'
laPolnTmp[55,3] = 5
laPolnTmp[55,4] = 0

laPolnTmp[56,1] = 'sz13'
laPolnTmp[56,2] = 'C'
laPolnTmp[56,3] = 5
laPolnTmp[56,4] = 0

laPolnTmp[57,1] = 'STYMAJ'
laPolnTmp[57,2] = 'C'
laPolnTmp[57,3] = 19
laPolnTmp[57,4] = 0

laPolnTmp[58,1] = 'COLOR'
laPolnTmp[58,2] = 'C'
laPolnTmp[58,3] = 6
laPolnTmp[58,4] = 0

laPolnTmp[59,1] = 'Desc1'
laPolnTmp[59,2] = 'C'
laPolnTmp[59,3] = 60
laPolnTmp[59,4] = 0

laPolnTmp[60,1] = 'entered'
laPolnTmp[60,2] = 'D'
laPolnTmp[60,3] = 8
laPolnTmp[60,4] = 0

laPolnTmp[61,1] = 'COMPLETE'
laPolnTmp[61,2] = 'D'
laPolnTmp[61,3] = 8
laPolnTmp[61,4] = 0

laPolnTmp[62,1] = 'TOTPRNT'
laPolnTmp[62,2] = 'L'
laPolnTmp[62,3] = 1
laPolnTmp[62,4] = 0

laPolnTmp[63,1] = 'TOTValue'
laPolnTmp[63,2] = 'N'
laPolnTmp[63,3] = 12
laPolnTmp[63,4] = 2

laPolnTmp[64,1] = 'Cshpdate'
laPolnTmp[64,2] = 'D'
laPolnTmp[64,3] = 8
laPolnTmp[64,4] = 0

laPolnTmp[65,1] = 'COrdPl'
laPolnTmp[65,2] = 'C'
laPolnTmp[65,3] = 25
laPolnTmp[65,4] = 0

laPolnTmp[66,1] = 'CMerch'
laPolnTmp[66,2] = 'C'
laPolnTmp[66,3] = 50
laPolnTmp[66,4] = 0

laPolnTmp[67,1] = 'CMerchTel'
laPolnTmp[67,2] = 'C'
laPolnTmp[67,3] = 50
laPolnTmp[67,4] = 0

laPolnTmp[68,1] = 'CMerchFax'
laPolnTmp[68,2] = 'C'
laPolnTmp[68,3] = 50
laPolnTmp[68,4] = 0

laPolnTmp[69,1] = 'CGtech'
laPolnTmp[69,2] = 'C'
laPolnTmp[69,3] = 50
laPolnTmp[69,4] = 0

laPolnTmp[70,1] = 'CGtechTel'
laPolnTmp[70,2] = 'C'
laPolnTmp[70,3] = 50
laPolnTmp[70,4] = 0

laPolnTmp[71,1] = 'CGtechFax'
laPolnTmp[71,2] = 'C'
laPolnTmp[71,3] = 50
laPolnTmp[71,4] = 0

laPolnTmp[72,1] = 'CCustRef'
laPolnTmp[72,2] = 'C'
laPolnTmp[72,3] = 10
laPolnTmp[72,4] = 0

laPolnTmp[73,1] = 'cMeremail'
laPolnTmp[73,2] = 'C'
laPolnTmp[73,3] = 45
laPolnTmp[73,4] = 0

laPolnTmp[74,1] = 'cGarEmail'
laPolnTmp[74,2] = 'C'
laPolnTmp[74,3] = 45
laPolnTmp[74,4] = 0

laPolnTmp[75,1] = 'NMaxPage'
laPolnTmp[75,2] = 'N'
laPolnTmp[75,3] = 12
laPolnTmp[75,4] = 0


laPolnTmp[76,1] = 'BEFORKEY'
laPolnTmp[76,2] = 'C'
laPolnTmp[76,3] = 20
laPolnTmp[76,4] = 0

laPolnTmp[77,1] = 'Page'
laPolnTmp[77,2] = 'N'
laPolnTmp[77,3] = 12
laPolnTmp[77,4] = 0


=gfCrtTmp(lcPolnTmp,@laPolnTmp,'PO+Style+Scale+pricekey',lcPolnTmp)
*Create index on lcPolnTmp base temperory file which contains posln's with grouped size scale for selected po's
SELECT (lcPolnTmp)
use (gcWorkDir+lcPolnTmp) EXCLU
INDEX ON PO+stymaj+Scale+pricekey TAG lcPolnTmp2 OF (gcWorkDir+lcPolnTmp+".CDX")
SET ORDER TO (lcPolnTmp) 
SELECT posln

*Create new scales file lcSclsTemp
DIMENSION laSclsTemp[29,4]

laSclsTemp[1,1] = 'sz1'
laSclsTemp[1,2] = 'C'
laSclsTemp[1,3] = 5
laSclsTemp[1,4] = 0

laSclsTemp[2,1] = 'sz2'
laSclsTemp[2,2] = 'C'
laSclsTemp[2,3] = 5
laSclsTemp[2,4] = 0

laSclsTemp[3,1] = 'sz3'
laSclsTemp[3,2] = 'C'
laSclsTemp[3,3] = 5
laSclsTemp[3,4] = 0

laSclsTemp[4,1] = 'sz4'
laSclsTemp[4,2] = 'C'
laSclsTemp[4,3] = 5
laSclsTemp[4,4] = 0

laSclsTemp[5,1] = 'sz5'
laSclsTemp[5,2] = 'C'
laSclsTemp[5,3] = 5
laSclsTemp[5,4] = 0

laSclsTemp[6,1] = 'sz6'
laSclsTemp[6,2] = 'C'
laSclsTemp[6,3] = 5
laSclsTemp[6,4] = 0

laSclsTemp[7,1] = 'sz7'
laSclsTemp[7,2] = 'C'
laSclsTemp[7,3] = 5
laSclsTemp[7,4] = 0

laSclsTemp[8,1] = 'sz8'
laSclsTemp[8,2] = 'C'
laSclsTemp[8,3] = 5
laSclsTemp[8,4] = 0

laSclsTemp[9,1] = 'sz9'
laSclsTemp[9,2] = 'C'
laSclsTemp[9,3] = 5
laSclsTemp[9,4] = 0

laSclsTemp[10,1] = 'sz10'
laSclsTemp[10,2] = 'C'
laSclsTemp[10,3] = 5
laSclsTemp[10,4] = 0

laSclsTemp[11,1] = 'sz11'
laSclsTemp[11,2] = 'C'
laSclsTemp[11,3] = 5
laSclsTemp[11,4] = 0

laSclsTemp[12,1] = 'sz12'
laSclsTemp[12,2] = 'C'
laSclsTemp[12,3] = 5
laSclsTemp[12,4] = 0

laSclsTemp[13,1] = 'sz13'
laSclsTemp[13,2] = 'C'
laSclsTemp[13,3] = 5
laSclsTemp[13,4] = 0

laSclsTemp[14,1] = 'SCLKEY'
laSclsTemp[14,2] = 'C'
laSclsTemp[14,3] = 100
laSclsTemp[14,4] = 0

laSclsTemp[15,1] = 'PO'
laSclsTemp[15,2] = 'C'
laSclsTemp[15,3] = 6
laSclsTemp[15,4] = 0

laSclsTemp[16,1] = 'STYMAJ'
laSclsTemp[16,2] = 'C'
laSclsTemp[16,3] = 19
laSclsTemp[16,4] = 0

laSclsTemp[17,1] = 'price1'
laSclsTemp[17,2] = 'N'
laSclsTemp[17,3] = 12
laSclsTemp[17,4] = 2

laSclsTemp[18,1] = 'price2'
laSclsTemp[18,2] = 'N'
laSclsTemp[18,3] = 12
laSclsTemp[18,4] = 2

laSclsTemp[19,1] = 'price3'
laSclsTemp[19,2] = 'N'
laSclsTemp[19,3] = 12
laSclsTemp[19,4] = 2

laSclsTemp[20,1] = 'price4'
laSclsTemp[20,2] = 'N'
laSclsTemp[20,3] = 12
laSclsTemp[20,4] = 2

laSclsTemp[21,1] = 'price5'
laSclsTemp[21,2] = 'N'
laSclsTemp[21,3] = 12
laSclsTemp[21,4] = 2

laSclsTemp[22,1] = 'price6'
laSclsTemp[22,2] = 'N'
laSclsTemp[22,3] = 12
laSclsTemp[22,4] = 2

laSclsTemp[23,1] = 'price7'
laSclsTemp[23,2] = 'N'
laSclsTemp[23,3] = 12
laSclsTemp[23,4] = 2

laSclsTemp[24,1] = 'price8'
laSclsTemp[24,2] = 'N'
laSclsTemp[24,3] = 12
laSclsTemp[24,4] = 2

laSclsTemp[25,1] = 'price9'
laSclsTemp[25,2] = 'N'
laSclsTemp[25,3] = 12
laSclsTemp[25,4] = 2

laSclsTemp[26,1] = 'price10'
laSclsTemp[26,2] = 'N'
laSclsTemp[26,3] = 12
laSclsTemp[26,4] = 2

laSclsTemp[27,1] = 'price11'
laSclsTemp[27,2] = 'N'
laSclsTemp[27,3] = 12
laSclsTemp[27,4] = 2

laSclsTemp[28,1] = 'price12'
laSclsTemp[28,2] = 'N'
laSclsTemp[28,3] = 12
laSclsTemp[28,4] = 2

laSclsTemp[29,1] = 'price13'
laSclsTemp[29,2] = 'N'
laSclsTemp[29,3] = 12
laSclsTemp[29,4] = 2
=gfCrtTmp(lcSclsTemp,@laSclsTemp,'PO+stymaj+SCLKEY',lcSclsTemp)


*!***************************************************************************
*!* Name       : lfColData
*!* Developer  : Ehab Ismail Hamed (EIH)
*!* Date       : 10/10/2005
*!* Purpose    : Collecting the needed data for PAN21's PO form 
*!*            : 
*!***************************************************************************
*!* Called from : POSTYP.PRG
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfColData()
*!***************************************************************************
FUNCTION lfColData
STORE .T. TO llTmpBrk
STORE  0  TO  lnCntPOLn , lnCntSlLn
*collect selected PO's lines from posln to first file temperary lcSlctdLns
SELECT POSLN
lcRpPosExp = STRTRAN(lcRpExp    , 'POSHDR' ,'POSLN')
lcRpPosExp = STRTRAN(lcRpPosExp , 'Poshdr' ,'POSLN')
SCAN FOR &lcRpPosExp
  SCATTER MEMVAR MEMO
  SELECT (lcSlctdLns)
  APPEND BLANK
  GATHER MEMVAR MEMO
  *Save scale sizes in lcSlctdLns for that line
  =SEEK('S'+M.SCALE,'SCALE')
  REPLACE &lcSlctdLns..cnt      WITH  SCALE.cnt             ,;
          &lcSlctdLns..sz1      WITH  SCALE.sz1             ,;
          &lcSlctdLns..sz2      WITH  SCALE.sz2             ,; 
          &lcSlctdLns..sz3      WITH  SCALE.sz3             ,;
          &lcSlctdLns..sz4      WITH  SCALE.sz4             ,;
          &lcSlctdLns..sz5      WITH  SCALE.sz5             ,;
          &lcSlctdLns..sz6      WITH  SCALE.sz6             ,;
          &lcSlctdLns..sz7      WITH  SCALE.sz7             ,;
          &lcSlctdLns..sz8      WITH  SCALE.sz8             
       
  *Svae style color, style major and desc1 from style file
  =SEEK(M.STYLE ,'STYLE')
  *We refere to dublicated lines with same scale and style but DIFFERENT price with DIFPRICE = '1'
  *color postion is 10 and color length is 6 for panach 
  REPLACE &lcSlctdLns..stymaj WITH  STYLE.cstymajor          ,;
          &lcSlctdLns..COLOR  WITH  SUBSTR(Style.style,11,6) ,;
          &lcSlctdLns..Desc1  WITH  STYLE.Desc1              ,;
          &lcSlctdLns..DIFPRICE  WITH  '2'
          
ENDSCAN
*Determine duplicated lines in lcSlctdLns for different price's
SELECT (lcSlctdLns)
LOCATE
SCAN FOR DIFPRICE = '2'
  lnRecNo = RECNO()
  lcKey = &lcSlctdLns..PO+&lcSlctdLns..Style+&lcSlctdLns..Scale
  SKIP
  SCAN REST WHILE &lcSlctdLns..PO+&lcSlctdLns..Style+&lcSlctdLns..Scale = lcKey
    REPLACE &lcSlctdLns..DIFPRICE  WITH  '1'
  ENDSCAN
  GOTO lnRecNo
ENDSCAN
*Append new lines in lcPolnTmp from appended size scale lines in lcSclsTemp  
=lfNewScals()
SELECT (lcSlctdLns)
LOCATE
*DIFPRICE = '2' Not include dublicated lines.

SCAN FOR DIFPRICE = '2'

  *Append new scale sizes in lcSclsTemp for required scale which in lcSlctdLns
  SELECT (lcSclsTemp)
  LOCATE
  =SEEK(&lcSlctdLns..PO+&lcSlctdLns..stymaj)
  SCAN REST WHILE PO+stymaj+SCLKEY = &lcSlctdLns..PO+&lcSlctdLns..stymaj FOR &lcSlctdLns..scale $ &lcSclsTemp..SCLKEY
    SELECT (lcPolnTmp)
    APPEND BLANK
    *Save base data for selected PO's lines with its new scale (scale consist of 13 size)
    REPLACE &lcPolnTmp..cstytype  WITH &lcSlctdLns..cstytype                ,;
            &lcPolnTmp..PO        WITH &lcSlctdLns..PO                      ,;
            &lcPolnTmp..trancd    WITH &lcSlctdLns..trancd                  ,;
            &lcPolnTmp..lineno    WITH &lcSlctdLns..lineno                  ,;
            &lcPolnTmp..vendor    WITH &lcSlctdLns..vendor                  ,;
            &lcPolnTmp..style     WITH LEFT(&lcSlctdLns..style,16)          ,;
            &lcPolnTmp..scale     WITH &lcSclsTemp..SCLKEY                  ,;
            &lcPolnTmp..dyelot    WITH &lcSlctdLns..dyelot                  ,;
            &lcPolnTmp..prepak    WITH &lcSlctdLns..prepak                  ,;
            &lcPolnTmp..ppqty     WITH &lcSlctdLns..ppqty                   ,;
            &lcPolnTmp..cwarecode WITH &lcSlctdLns..dyelot                  ,;
            &lcPolnTmp..account   WITH &lcSlctdLns..account                 
    *Seek for PO Number in POSHDR to get its data
    =SEEK('P'+&lcSlctdLns..PO,'POSHDR')
    
    REPLACE &lcPolnTmp..store     WITH &lcSlctdLns..store                   ,;
            &lcPolnTmp..stymaj    WITH &lcSlctdLns..stymaj                  ,;
            &lcPolnTmp..COLOR     WITH &lcSlctdLns..COLOR                   ,;
            &lcPolnTmp..Desc1     WITH &lcSlctdLns..Desc1                   ,;
            &lcPolnTmp..cvensty   WITH &lcSlctdLns..cvensty                 ,;
            &lcPolnTmp..entered   WITH POSHDR.entered                       ,;
            &lcPolnTmp..complete  WITH POSHDR.complete                      ,;
            &lcPolnTmp..CShpDate  WITH POSHDR.CShpDate                      ,;
            &lcPolnTmp..CCustRef  WITH POSHDR.CCustRef                       
    
    REPLACE &lcPolnTmp..COrdPL    WITH POSHDR.COrdPL                        ,;
            &lcPolnTmp..cMeremail WITH lfMailNote(POSHDR.CMerch)            
    REPLACE &lcPolnTmp..cGarEmail WITH lfMailNote(POSHDR.CGtech)            
    
    REPLACE &lcPolnTmp..CMerch    WITH IIF(ASCAN(laMerch,ALLTRIM(POSHDR.CMerch)) <> 0 , laMerch[ASCAN(laMerch,ALLTRIM(POSHDR.CMerch)) + 1 ],'')                ,;
            &lcPolnTmp..CMerchTel WITH IIF(ASCAN(laMerchTel,ALLTRIM(POSHDR.CMerchTel)) <> 0 ,laMerchTel[ASCAN(laMerchTel,ALLTRIM(POSHDR.CMerchTel)) + 1 ],'')  ,;
            &lcPolnTmp..CMerchFax WITH IIF(ASCAN(laMerchFax,ALLTRIM(POSHDR.CMerchFAX)) <> 0 , laMerchFax[ASCAN(laMerchFax,ALLTRIM(POSHDR.CMerchFAX)) + 1 ],'') 
            
    REPLACE &lcPolnTmp..CGtech WITH IIF(ASCAN(laGarTech,ALLTRIM(POSHDR.CGtech)) <> 0 , laGarTech[ASCAN(laGarTech,ALLTRIM(POSHDR.CGtech)) + 1 ],'')               ,;
            &lcPolnTmp..CGTECHTEL WITH IIF(ASCAN(laGTechTel,ALLTRIM(POSHDR.CGTECHTEL)) <> 0 , laGTechTel[ASCAN(laGTechTeL,ALLTRIM(POSHDR.CGTECHTEL)) + 1 ],'')  ,;       
            &lcPolnTmp..CGTECHFAX WITH IIF(ASCAN(laGTechFax,ALLTRIM(POSHDR.CGTECHFAX)) <> 0 , laGTechFax[ASCAN(laGTechFax,ALLTRIM(POSHDR.CGTECHFAX)) + 1 ],'')   
    *Set Scale size and default price 
    FOR lnCnt = 1 to 13
      lcCnt = ALLTRIM(STR(lnCnt))
      IF EMPTY(&lcSclsTemp..SZ&lcCnt)
        EXIT
      ENDIF
      REPLACE  &lcPolnTmp..SZ&lcCnt WITH &lcSclsTemp..SZ&lcCnt
      REPLACE  &lcPolnTmp..price&lcCnt WITH &lcSclsTemp..price&lcCnt
    ENDFOR       
    SELECT (lcSclsTemp)
  ENDSCAN
  SELECT (lcSlctdLns)
ENDSCAN




*Here we must delete lines for same 'PO+STYLE+SCLKEY' SO we get lcPolnTmp which have lines with new scale of 
*the style and its related default price
SELECT (lcPolnTmp)
LOCATE
SCAN 
  lcPO       = &lcPolnTmp..po
  lcStyle    = &lcPolnTmp..STYLE
  lcNewScale = &lcPolnTmp..scale
  lnRecNo = RECNO()
  SKIP
  SCAN REST WHILE PO+Style+Scale+pricekey = lcPO+lcStyle+lcNewScale
    DELETE
  ENDSCAN
  SELECT (lcPolnTmp)
  GOTO lnRecNo
ENDSCAN

*Add size Qty in the size Qty location in lcPolnTmp
SELECT (lcSlctdLns)
LOCATE
SCAN 
  =SEEK('S'+&lcSlctdLns..SCALE,'SCALE')
  *SEEK FOR THAT LiNE IN lcPolnTmp
  *Seek for first size postion in that scale in base file lcPolnTmp
  lnSizePos=lfSizePos(&lcSlctdLns..SZ1)
  FOR lnCnt = 0 TO (SCALE.CNT - 1)
    IF (lnSizePos + lnCnt) = 14
      *--MHM2006
      REPLACE  &lcPolnTmp..pricekey    WITH ALLTRIM(STR(&lcPolnTmp..price1,5,2))+'-'+ALLTRIM(STR(&lcPolnTmp..price2,5,2))+'-'+ALLTRIM(STR(&lcPolnTmp..price3,5,2))+'-'+;
                                            ALLTRIM(STR(&lcPolnTmp..price4,5,2))+'-'+ALLTRIM(STR(&lcPolnTmp..price5,5,2))+'-'+ALLTRIM(STR(&lcPolnTmp..price6,5,2))+'-'+;
                                            ALLTRIM(STR(&lcPolnTmp..price7,5,2))+'-'+ALLTRIM(STR(&lcPolnTmp..price8,5,2))+'-'+ALLTRIM(STR(&lcPolnTmp..price9,5,2))+'-'+;
                                            ALLTRIM(STR(&lcPolnTmp..price10,5,2))+'-'+ALLTRIM(STR(&lcPolnTmp..price11,5,2))+'-'+ALLTRIM(STR(&lcPolnTmp..price12,5,2))+'-'+;
                                            ALLTRIM(STR(&lcPolnTmp..price13,5,2))

      *--mhm2006

      SELECT (lcPolnTmp)
      SKIP
      IF (&lcSlctdLns..SCALE $ &lcPolnTmp..SCALE)
        lnSizePos = 1
        FOR lnI = lnCnt TO (SCALE.CNT - 1)
          lcSizePos = ALLTRIM(STR(lnSizePos + (lnI - lnCnt)))
          lcI     = ALLTRIM(STR(lnI+1))
          REPLACE &lcPolnTmp..TotLNQty        WITH &lcPolnTmp..TotLNQty + &lcSlctdLns..QTY&lcI   ,;
   	              &lcPolnTmp..totvalue        WITH &lcPolnTmp..totvalue +(&lcSlctdLns..QTY&lcI * &lcSlctdLns..NCOST1)    	      
   	      IF &lcSlctdLns..DIFPRICE <> '2'
   	        IF  &lcSlctdLns..QTY&lcI <> 0
   	          REPLACE &lcPolnTmp..QTY&lcSizePos   WITH &lcSlctdLns..QTY&lcI      ,;
   	                  &lcPolnTmp..Price&lcSizePos WITH &lcSlctdLns..Gros_Price
   	        ENDIF
   	      ELSE
   	        REPLACE &lcPolnTmp..QTY&lcSizePos   WITH &lcSlctdLns..QTY&lcI      ,;
   	                &lcPolnTmp..Price&lcSizePos WITH &lcSlctdLns..Gros_Price
   	      ENDIF
        ENDFOR
      ENDIF 
      EXIT
    ELSE
      lcSizePos = ALLTRIM(STR(lnSizePos + lnCnt))
      lcCnt     = ALLTRIM(STR(lnCnt+1))
      REPLACE  &lcPolnTmp..TotLNQty        WITH &lcPolnTmp..TotLNQty + &lcSlctdLns..QTY&lcCnt     ,;
	           &lcPolnTmp..totvalue        WITH &lcPolnTmp..totvalue +(&lcSlctdLns..QTY&lcCnt * &lcSlctdLns..NCOST1)    
      IF &lcSlctdLns..DIFPRICE <> '2'
        IF  &lcSlctdLns..QTY&lcCnt <> 0
   	        REPLACE &lcPolnTmp..QTY&lcSizePos   WITH &lcSlctdLns..QTY&lcCnt     ,;
   	                &lcPolnTmp..Price&lcSizePos WITH &lcSlctdLns..Gros_Price
        ENDIF
      ELSE
        REPLACE &lcPolnTmp..QTY&lcSizePos   WITH &lcSlctdLns..QTY&lcCnt         ,;
                &lcPolnTmp..Price&lcSizePos WITH &lcSlctdLns..Gros_Price
   	  
      ENDIF
    ENDIF
  ENDFOR
  *--mhm2006
  *REPLACE  &lcPolnTmp..pricekey    WITH ALLTRIM(STR(&lcPolnTmp..price1))+'-'+ALLTRIM(STR(&lcPolnTmp..price2))+'-'+ALLTRIM(STR(&lcPolnTmp..price3))+'-'+;
                                        ALLTRIM(STR(&lcPolnTmp..price4))+'-'+ALLTRIM(STR(&lcPolnTmp..price5))+'-'+ALLTRIM(STR(&lcPolnTmp..price6))+'-'+;
                                        ALLTRIM(STR(&lcPolnTmp..price7))+'-'+ALLTRIM(STR(&lcPolnTmp..price8))+'-'+ALLTRIM(STR(&lcPolnTmp..price9))+'-'+;
                                        ALLTRIM(STR(&lcPolnTmp..price10))+'-'+ALLTRIM(STR(&lcPolnTmp..price11))+'-'+ALLTRIM(STR(&lcPolnTmp..price12))+'-'+;
                                        ALLTRIM(STR(&lcPolnTmp..price13))
  
  REPLACE  &lcPolnTmp..pricekey    WITH ALLTRIM(STR(&lcPolnTmp..price1,5,2))+'-'+ALLTRIM(STR(&lcPolnTmp..price2,5,2))+'-'+ALLTRIM(STR(&lcPolnTmp..price3,5,2))+'-'+;
                                        ALLTRIM(STR(&lcPolnTmp..price4,5,2))+'-'+ALLTRIM(STR(&lcPolnTmp..price5,5,2))+'-'+ALLTRIM(STR(&lcPolnTmp..price6,5,2))+'-'+;
                                        ALLTRIM(STR(&lcPolnTmp..price7,5,2))+'-'+ALLTRIM(STR(&lcPolnTmp..price8,5,2))+'-'+ALLTRIM(STR(&lcPolnTmp..price9,5,2))+'-'+;
                                        ALLTRIM(STR(&lcPolnTmp..price10,5,2))+'-'+ALLTRIM(STR(&lcPolnTmp..price11,5,2))+'-'+ALLTRIM(STR(&lcPolnTmp..price12,5,2))+'-'+;
                                        ALLTRIM(STR(&lcPolnTmp..price13,5,2))

  *--mhm2006

  SELECT (lcSlctdLns)
ENDSCAN
*we delete empty lines from lcPolnTmp
*Delete lines from lcPolnTmp which its Qty equal zero
SELECT (lcPolnTmp)
LOCATE
DELETE FOR TotLNQty <= 0
SELECT (lcPolnTmp)
LOCATE
*To get Page number of that PO
STORE '' TO lcPoOldNO , lcPriceScl
STORE 0 TO lnLineCnt
STORE 1 TO lnMaxPage
*Number of rows per page
lnRowPerPg = 28
STORE .F. TO llEndFile

SET ORDER TO lcPolnTmp2
SCAN
  lnTotal = 0
  *This for header of the group
  IF !(ALLTRIM(&lcPolnTmp..pricekey)+ALLTRIM(&lcPolnTmp..SCALE) $ lcPriceScl)
    lnLineCnt = lnLineCnt + 2
    lcPriceScl = ALLTRIM(&lcPolnTmp..pricekey)+ALLTRIM(&lcPolnTmp..SCALE)
  ENDIF
  *for every line we increase the lnLineCnt by ONE
  lnLineCnt = lnLineCnt + 1
  *SET the page number for that line
  REPLACE PAGE WITH CEILING(lnLineCnt/lnRowPerPg)
  lcKey = &lcPolnTmp..PO+&lcPolnTmp..STYLE
  lcBeforKey = &lcPolnTmp..PO+&lcPolnTmp..STYMAJ
  lnRecNo = RecNo()
  *REPLACE NMaxPage WITH lnMaxPage
  SKIP
  IF EOF()
    llEndFile = .T.
  ENDIF  
  REPLACE &lcPolnTmp..BEFORKEY WITH lcBeforKey 
  GOTO lnRecNo
  *Check if we print the total qty of this style or not
*  IF lcKey = &lcPolnTmp..PO+&lcPolnTmp..STYLE
*    GOTO lnRecNo
*    REPLACE TOTPRNT WITH .F.
*  ELSE
*    SUM totlnqty FOR po+style = lcKey TO lnTotal
*    GOTO lnRecNo
*    REPLACE TOTPRNT WITH .T.      ,;
*            TOTQTY  WITH lnTotal  
*    lnLineCnt = lnLineCnt + 1        
*  ENDIF
  *here we must update nmaxpage with lnMaxPage 
  IF (lcPoOldNO  <> &lcPolnTmp..PO OR llEndFile ) AND !EMPTY(lcPoOldNO)
    IF llRpPrtPn
      lnLineCnt = lnLineCnt + lfPoNote(lcPoOldNO)
    ENDIF
    IF MOD(lnLineCnt,lnRowPerPg) = 0
      lnMaxPage = FLOOR(lnLineCnt/lnRowPerPg) 
    ELSE
      lnMaxPage = FLOOR(lnLineCnt/lnRowPerPg) + 1  
    ENDIF
    lnRecNo = RecNo()
    REPLACE &lcPolnTmp..nmaxpage WITH lnMaxPage FOR &lcPolnTmp..PO = lcPoOldNO 
    lnLineCnt = 1
    lcPriceScl = ''
    GOTO lnRecNo   
  ENDIF  
  lcPoOldNO = &lcPolnTmp..PO
ENDSCAN

SELECT (lcPolnTmp)
LOCATE
SET ORDER TO lcPolnTmp
SCAN
  lnTotal = 0
  lcKey = &lcPolnTmp..PO+&lcPolnTmp..STYLE
  lnRecNo = RecNo()
  SKIP
  *Check if we print the total qty of this style or not
  IF lcKey = &lcPolnTmp..PO+&lcPolnTmp..STYLE
    GOTO lnRecNo
    REPLACE TOTPRNT WITH .F.
  ELSE
    SUM totlnqty FOR po+style = lcKey TO lnTotal
    GOTO lnRecNo
    REPLACE TOTPRNT WITH .T.      ,;
            TOTQTY  WITH lnTotal  
  ENDIF
ENDSCAN



*--End function lfColData.






*!*************************************************************
*! Name      : lfNewScals
*! Developer : Ehab Ismail Hamed (EIH)
*! Date      : 10/12/2005
*! Purpose   : Get Scales lines for some scale
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfNewScals()
*!*************************************************************
FUNCTION lfNewScals
PRIVATE lnCnt
STORE 1 TO lnCnt ,lnSizePos
STORE '' TO lcScalKey , lcPoStScls
STORE 0 TO lnScalCnt
SELECT (lcSlctdLns)
LOCATE


STORE '' TO lcPOStMaj
SCAN FOR DIFPRICE = '2'

  IF lcPOStMaj <> &lcSlctdLns..po+&lcSlctdLns..stymaj
    
    IF lnSizePos <> 1
      SKIP -1
      SELECT (lcSclsTemp)
      APPEND BLANK
      M.PO     = &lcSlctdLns..po
      M.STYMAJ = &lcSlctdLns..stymaj
      M.SCLKEY = lcScalKey
      GATHER MEMVAR MEMO
      SCATTER MEMVAR BLANK
      SELECT (lcSlctdLns)
      SKIP
      lnSizePos = 1
    ENDIF
    
    
    lcPOStMaj = &lcSlctdLns..po+&lcSlctdLns..stymaj
    lcScalKey = ''
    lcPoStScls= &lcSlctdLns..po+&lcSlctdLns..stymaj
    
    
    
  ENDIF
  
  IF (&lcSlctdLns..SCALE $ lcScalKey AND lnScalCnt = 9 ) OR (&lcSlctdLns..po $ lcPoStScls AND &lcSlctdLns..stymaj $ lcPoStScls AND &lcSlctdLns..SCALE $ lcPoStScls )
    LOOP
  ENDIF
  
  
  IF lnScalCnt = 9 OR lnScalCnt = 0
      lnScalCnt = 1
      =SEEK('S'+&lcSlctdLns..SCALE, 'SCALE')
  ENDIF  
  FOR lnCnt = lnScalCnt TO SCALE.CNT
    lcCnt            = ALLTRIM(STR(lnCnt))
    lcSizePos        = ALLTRIM(STR(lnSizePos))
    M.SZ&lcSizePos = SCALE.sz&lcCnt
    M.price&lcSizePos = &lcSlctdLns..Gros_Price
    lnScalCnt = lnScalCnt + 1
    IF lnSizePos >= 13
      lnSizePos = lnSizePos +1
      EXIT
    ENDIF
    lnSizePos = lnSizePos +1
  ENDFOR
  *If we reach to the end of that scale
  IF lnScalCnt = SCALE.CNT + 1
    lcScalKey = lcScalKey + &lcSlctdLns..SCALE
    lcPoStScls= lcPoStScls + &lcSlctdLns..SCALE
    lnScalCnt = 9
  ELSE
    lcScalKey = lcScalKey + &lcSlctdLns..SCALE
    SELECT (lcSlctdLns)
    SKIP -1
    IF lnSizePos <> 14
      LOOP
    ENDIF
  ENDIF
   
  SELECT (lcSlctdLns)
  SKIP
  IF lcPOStMaj <> &lcSlctdLns..po+&lcSlctdLns..stymaj
    lnSizePos = 14
  ENDIF
  SKIP -1
  IF lnSizePos = 14 
    SELECT (lcSclsTemp)
    APPEND BLANK
    M.PO     = &lcSlctdLns..po
    M.STYMAJ = &lcSlctdLns..stymaj
    M.SCLKEY = lcScalKey
    lcScalKey= ''
    GATHER MEMVAR MEMO
    SCATTER MEMVAR BLANK
    lnSizePos = 1
  ENDIF
  SELECT (lcSlctdLns)
ENDSCAN

*IF !EMPTY(&lcSlctdLns..po) AND !EMPTY(&lcSlctdLns..stymaj) 
IF lnSizePos <> 1 
  SELECT (lcSlctdLns)
  SKIP -1
  
  SELECT (lcSclsTemp)
  APPEND BLANK
  M.PO     = &lcSlctdLns..po
  M.STYMAJ = &lcSlctdLns..stymaj
  M.SCLKEY = lcScalKey
  GATHER MEMVAR MEMO
ENDIF

*!*************************************************************
*! Name      : lfSizePos
*! Developer : Ehab Ismail Hamed (EIH)
*! Date      : 10/12/2005
*! Purpose   : Get Size Postion in lcPolnTmp scale
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfSizePos()
*!*************************************************************
FUNCTION lfSizePos
PARAMETER lcSize
PRIVATE lnCnt
STORE 0 TO lnSizePos
STORE .F. TO llFond 
lcSize = PADR(lcSize ,5,' ')
=SEEK(&lcSlctdLns..PO+LEFT(&lcSlctdLns..style,16),lcPolnTmp)
SELECT (lcPolnTmp)
SCAN REST WHILE PO+Style+Scale+pricekey = &lcSlctdLns..PO+LEFT(&lcSlctdLns..style,16)
  FOR lnCnt=1 TO 13
    lcCnt = ALLTRIM(STR(lnCnt))
    IF &lcPolnTmp..SZ&lcCnt == lcSize
      lnSizePos = lnCnt
      llFond = .T.
      EXIT
    ENDIF
  ENDFOR
  IF llFond 
    EXIT
  ENDIF
ENDSCAN  
RETURN lnSizePos 
*!*************************************************************
*! Name      : lfSizPrice
*! Developer : Ehab Ismail Hamed (EIH)
*! Date      : 10/12/2005
*! Purpose   : Get Size price for style color from BOM file
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfSizPrice()
*!*************************************************************
FUNCTION lfSizPrice
PARAMETER lcStlMaj , lcColor , lcScal , lcSize
PRIVATE lnSizPrice ,lnCnt

lcAlias = ALIAS()
STORE 0 TO lnSizPrice
STORE .F. TO llFound
SELECT BOM
LOCATE
=SEEK(lcStlMaj+'1'+LEFT(lcStlMaj,9)+'-'+lcColor)
IF FOUND()
  SCAN REST WHILE citmmajor+typ+citmmask+mfgcode+item+iclr = lcStlMaj+'1'+LEFT(lcStlMaj,9)+'-'+lcColor
    *Search for the scale and the target size to get size price
    FOR lnCnt = 1 TO MEMLINES(msizes) 
      IF lcScal $ MLINE(msizes , lnCnt ) AND lcSize $ SUBSTR(MLINE(msizes , lnCnt ) , 4)
        lnSizPrice = BOM.untcost
        llFound = .T.
        EXIT
      ENDIF
    ENDFOR
    IF llFound
      EXIT
    ENDIF
  ENDSCAN
ENDIF

IF !llFound
  =SEEK(lcStlMaj+'1'+LEFT(lcStlMaj,9))
  SCAN REST WHILE citmmajor+typ+citmmask+mfgcode+item+iclr = lcStlMaj+'1'+LEFT(lcStlMaj,9)
    *Search for the scale and the target size to get size price
    FOR lnCnt = 1 TO MEMLINES(msizes) 
      IF lcScal $ MLINE(msizes , lnCnt ) AND lcSize $ SUBSTR(MLINE(msizes , lnCnt ) , 4)
        lnSizPrice = BOM.untcost
        llFound = .T.
        EXIT
      ENDIF
    ENDFOR
    
    IF llFound
      EXIT
    ENDIF
    
  ENDSCAN  
ENDIF

SELECT &lcAlias
RETURN lnSizPrice 



*!*************************************************************
*! Name      : lfVendData
*! Developer : Ehab Ismail Hamed (EIH)
*! Date      : 10/19/2005
*! Purpose   : Get vendor data to be printed on page header
*!*************************************************************
*! Called from : POSTYPO.PRG
*!*************************************************************
*! Calls       : gfGetAdr()
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfVendData()
*!*************************************************************
FUNCTION lfVendData
PARA lcDummy
lcAlias = ALIAS()
SELECT APVENDOR
=SEEK(ALLTRIM(&lcPolnTmp..VENDOR))
lcVenName   = APVENDOR.CVenComp
lcVenFax    = APVENDOR.CFAXNO
lcVenPhone  = APVENDOR.CPHONENO
* Get the vendor addresses
laVenAdr[1] = gfGetAdr('APVENDOR' , '' , '' , '' , 1)
laVenAdr[2] = gfGetAdr('APVENDOR' , '' , '' , '' , 2)
laVenAdr[3] = gfGetAdr('APVENDOR' , '' , '' , '' , 3)
laVenAdr[4] = gfGetAdr('APVENDOR' , '' , '' , '' , 4)
laVenAdr[5] = gfGetAdr('APVENDOR' , '' , '' , '' , 5)
laVenAdr[6] = gfGetAdr('APVENDOR' , '' , '' , '' , 6)
laVenAdr[7] = lcVenFax
laVenAdr[8] = lcVenPhone
SELECT &lcAlias
RETURN ''

*-- End of lfVendData.

*!**************************************************************************
*! Name      : lfStPoNote
*! Developer : Ehab Ismail Hamed (EIH)
*! Date      : 10/19/2005
*! Purpose   : Print notepad templete Called from arcstmb.frx. 
*!**************************************************************************
*! Example   : =lfStPoNote()
*!**************************************************************************
FUNCTION lfStPoNote
IF SEEK('TSTYPO','Notepad')
  lcStPoNote = NOTEPAD.MNOTES
ELSE
  lcStPoNote = ''
ENDIF
RETURN lcStPoNote
*-- End of lfStPoNote.

*!**************************************************************************
*! Name      : lfPoNote
*! Developer : Ehab Ismail Hamed (EIH)
*! Date      : 10/19/2005
*! Purpose   : Print Po note Called from arcstmb.frx. 
*!**************************************************************************
*! Example   : =lfPoNote()
*!**************************************************************************
FUNCTION lfPoNote
PARA lcDummy
IF llRpPrtPn AND !EMPTY(lcDummy)
  IF SEEK('P'+ALLTRIM(lcDummy),'Notepad')
    lnMemoLns = MEMLINES(NOTEPAD.MNOTES) 
  ELSE
    lnMemoLns = 0
  ENDIF
  RETURN lnMemoLns
ENDIF

IF llRpPrtPn AND SEEK('P'+&lcPolnTmp..PO,'Notepad')
  lcPoNote = NOTEPAD.MNOTES
  lnMemoLns = MEMLINES(NOTEPAD.MNOTES) 
ELSE
  lcPoNote = ''
  lnMemoLns = 0
ENDIF
RETURN lnMemoLns
*-- End of lfPoNote.


*!**************************************************************************
*! Name      : lfUDFDesc
*! Developer : Ehab Ismail Hamed (EIH)
*! Date      : 10/19/2005
*! Purpose   : Get the description of required user defined field
*!**************************************************************************
*! Example   : =lfUDFDesc()
*!**************************************************************************
FUNCTION lfUDFDesc
PARAMETER lcUDFName,laUDFArr
lnMemoWdth=SET('MEMOWIDTH')
SET MEMOWIDTH TO 255
IF SEEK(UPPER(lcUDFName), 'SYDFIELD')
  lcMvEntres = MLINE(SYDFIELD.MVENTRIES,1)
  IF EMPTY(lcMvEntres)
    RETURN
  ENDIF
  lnPos=AT('~',lcMvEntres)
  lcAllValue = LEFT(lcMvEntres , lnPos)
  lcCodes    = STRTRAN(lcMvEntres,lcAllValue,'')
  lcCodes    = STRTRAN(lcCodes,'@','')
  lcAllValue    = STRTRAN(lcAllValue,'~','')
  lnOccurs = OCCURS('|',lcAllValue)
  lnOccurs = lnOccurs + 2 
  DIMENSION laUDFArr[lnOccurs - 1 ,2]
  STORE '' TO laUDFArr
  IF lnOccurs = 2
    laUDFArr[1,1] = lcCodes
    laUDFArr[1,2] =  lcAllValue
    RETURN laUDFArr
  ELSE
    FOR lnCnt = 1 TO lnOccurs - 1
      lnPos=AT('|',lcCodes)
      laUDFArr[lnCnt,1] = LEFT(lcCodes,lnPos - 1)
      lcCodes    = STRTRAN(lcCodes,laUDFArr[lnCnt,1]+'|','')
      lnPos=AT('|',lcAllValue)
      laUDFArr[lnCnt,2] = LEFT(lcAllValue,lnPos - 1)
      lcAllValue  = STRTRAN(lcAllValue,laUDFArr[lnCnt,2]+'|','')
      IF OCCURS('|',lcAllValue) = 0
        EXIT
      ENDIF
    ENDFOR
    laUDFArr[lnCnt+1,1] = lcCodes
    laUDFArr[lnCnt+1,2] = lcAllValue
  ENDIF
ENDIF  
SET MEMOWIDTH TO lnMemoWdth

RETURN laUDFArr


*-- End of lfUDFDesc.



*!**************************************************************************
*! Name      : lfMailNote
*! Developer : Ehab Ismail Hamed (EIH)
*! Date      : 10/19/2005
*! Purpose   : get Notepad templete for merch and cgtech to get the emial fields. 
*!**************************************************************************
*! Example   : =lfMailNote()
*!**************************************************************************
FUNCTION lfMailNote
PARAMETER lcTarget
IF SEEK('T'+ALLTRIM(UPPER(lcTarget)),'Notepad')
  lcMailNote = SUBSTR(MLINE(NOTEPAD.MNOTES,1),1,45)
ELSE
  lcMailNote = ''
ENDIF
RETURN lcMailNote
*-- End of lfMailNote.

*----EIH C129442 [END]

