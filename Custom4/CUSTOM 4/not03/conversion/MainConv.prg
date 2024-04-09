*:**********************************************************************
*: Program file        : MainConv.PRG
*: Program description : Convert Aims DBF to Aria27
*: Module              : System Manager (SM)
*: Developer           : Ahmed Salah Shalaby - (SSH)
*: Tracking Job Number : 
************************************************************************
*: C130022 SSH 16-Jan-2005 Customer file conversion
*: C130024 SSH 16-Jan-2005 Styles conversion
*: C130025 SSH 16-Jan-2005 Vendors conversion
*: C130026 SSH 16-Jan-2005 Fabrics Conversion
*: C130027 SSH 16-Jan-2005 Sales rep. Conversion
*: C130768 SSH 16-Jan-2005 Conversion for the style cost sheet
*: C130769 SSH 16-Jan-2005 Convert the data for the codes
*:**********************************************************************
PRIVATE lcAimsDbfsPath

SET SAFETY OFF
SET TALK OFF
SET CPDIALOG OFF
SET NOTIFY off
DO FORM .\AimsConv\Main.scx
READ EVENT
CLOSE ALL
