VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "UIOrdChld"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Attribute VB_Ext_KEY = "SavedWithClassBuilder6" ,"Yes"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
Option Explicit

' This constant should define the Object ID from the Object Dictionary
Const OBJECT_CODE As String = ""

' UI Modes
Public Enum UIchildMode
  UIAddMode = 1
  UIEditMode = 2
  UIViewMode = 3
End Enum

Private Enum PropertiesSettings
  SetPropertiesFromRecordSet = 1
  SetPropertiesToRecordSet = 2
  SetPropertiesToDefaults = 3
  SetPropertiesToOldValues = 4
End Enum

Private mstrConParameter As String
Private mbolHeaderOnly As Boolean
Private mintMode As Integer
Private mobjHeaderRecord As ADODB.Recordset


'child properities start

Private mstrOrdType_line As String
Private mstrOrder_line As String
Private mstrAccount_line As String
Private mstrWareCode_line As String
Private mintLine_No_line As Integer
Private mintShipments_line As Integer
Private mstrStore_line As String
Private mstrCustPo_line As String
Private mstrStyle_line As String
Private mstraltStyle_line As String
Private mstrAltColor_line As String
Private mstrSeason_line As String
Private mstrDesc1_line As String
Private mstrScale_line As String
Private mstrGroup_line As String
Private mstrPrePak_line As String
Private mintPpQty_line As Integer
Private msglPrice_line As Single
Private msglSugretpri_line As Single
Private msglGros_Price_line As Single
Private msglDisc_Pcnt_line As Single
Private mdatStart_line As Date
Private mdatComplete_line As Date
Private msglQty1_line As Single
Private msglQty2_line As Single
Private msglQty3_line As Single
Private msglQty4_line As Single
Private msglQty5_line As Single
Private msglQty6_line As Single
Private msglQty7_line As Single
Private msglQty8_line As Single
Private msglTotQty_line As Single
Private msglbook1_line As Single
Private msglbook2_line As Single
Private msglbook3_line As Single
Private msglbook4_line As Single
Private msglbook5_line As Single
Private msglbook6_line As Single
Private msglbook7_line As Single
Private msglbook8_line As Single
Private msglTotBook_line As Single
Private msglPik1_line As Single
Private msglPik2_line As Single
Private msglPik3_line As Single
Private msglPik4_line As Single
Private msglPik5_line As Single
Private msglPik6_line As Single
Private msglPik7_line As Single
Private msglPik8_line As Single
Private msglTotPik_line As Single
Private mstrPikTkT_line As String
Private mdatPikDate_line As Date
Private mbolPicked_line As Boolean
Private mstrInvoice_line As String
Private mdatInvDate_line As Date
Private mstrFlag_line As String
Private mstrCutTKT_line As String
Private mstrDylot_line As String
Private msglAllocated_line As Single
Private msglCut1_line As Single
Private msglCut2_line As Single
Private msglCut3_line As Single
Private msglCut4_line As Single
Private msglCut5_line As Single
Private msglCut6_line As Single
Private msglCut7_line As Single
Private msglCut8_line As Single
Private msglTotCut_line As Single
Private msglpoalo1_line As Single
Private msglpoalo2_line As Single
Private msglpoalo3_line As Single
Private msglpoalo4_line As Single
Private msglpoalo5_line As Single
Private msglpoalo6_line As Single
Private msglpoalo7_line As Single
Private msglpoalo8_line As Single
Private msglTot_Poalo_line As Single
Private msglPck1_line As Single
Private msglPck2_line As Single
Private msglPck3_line As Single
Private msglPck4_line As Single
Private msglPck5_line As Single
Private msglPck6_line As Single
Private msglPck7_line As Single
Private msglPck8_line As Single
Private msglPWght_line As Single
Private mstrNote_mem_line As String
Private msglComm1_line As Single
Private msglComm2_line As Single
Private mstrPO_line As String
Private msglCost_line As Single
Private mstrGL_Sales_line As String
Private mstrGL_Cost_line As String
Private mstrPackId_line As String
Private mintPOLineNo_line As Integer
Private mstrFromOrder_line As String
Private msglBulkLineNo_line As Single
Private mstrClineStat_line As String
Private mstrAdd_User_line As String
Private mstrAdd_time_line As String
Private mdatAdd_date_line As Date
Private mbolLok_Stat_line As Boolean
Private mstrlok_User_line As String
Private mbolLok_Date_line As Boolean
Private mstrLok_Time_line As String
Private mstrOwner_line As String

Public Property Let OrdType_line(ByVal strOrdType_line As String)
  mstrOrdType_line = strOrdType_line
End Property
Public Property Get OrdType_line() As String
  OrdType_line = mstrOrdType_line
End Property

Public Property Let Owner_line(ByVal strOwner_line As String)
  mstrOwner_line = strOwner_line
End Property
Public Property Get Owner_line() As String
  Owner_line = mstrOwner_line
End Property
Public Property Let Lok_Time_line(ByVal strLok_Time_line As String)
  mstrLok_Time_line = strLok_Time_line
End Property
Public Property Get Lok_Time_line() As String
  Lok_Time_line = mstrLok_Time_line
End Property
Public Property Let Lok_Date_line(ByVal bolLok_Date_line As Date)
  mbolLok_Date_line = bolLok_Date_line
End Property
Public Property Get Lok_Date_line() As Date
  Lok_Date_line = mbolLok_Date_line
End Property
Public Property Let Lok_User_line(ByVal strLok_User_line As String)
  mstrlok_User_line = strLok_User_line
End Property
Public Property Get Lok_User_line() As String
  Lok_User_line = mstrlok_User_line
End Property
Public Property Let Lok_Stat_line(ByVal bolLok_Stat_line As Boolean)
  mbolLok_Stat_line = bolLok_Stat_line
End Property
Public Property Get Lok_Stat_line() As Boolean
  Lok_Stat_line = mbolLok_Stat_line
End Property
Public Property Let Add_date_line(ByVal datAdd_date_line As Date)
  mdatAdd_date_line = datAdd_date_line
End Property
Public Property Get Add_date_line() As Date
  Add_date_line = mdatAdd_date_line
End Property
Public Property Let Add_Time_line(ByVal strAdd_Time_line As String)
  mstrAdd_time_line = strAdd_Time_line
End Property
Public Property Get Add_Time_line() As String
  Add_Time_line = mstrAdd_time_line
End Property
Public Property Let Add_User_line(ByVal strAdd_User_line As String)
  mstrAdd_User_line = strAdd_User_line
End Property
Public Property Get Add_User_line() As String
  Add_User_line = mstrAdd_User_line
End Property
Public Property Let ClineStat_line(ByVal strClineStat_line As String)
  mstrClineStat_line = strClineStat_line
End Property
Public Property Get ClineStat_line() As String
  ClineStat_line = mstrClineStat_line
End Property
Public Property Let BulkLine_No_line(ByVal sglBulkLine_No_line As Single)
  msglBulkLineNo_line = sglBulkLine_No_line
End Property
Public Property Get BulkLine_No_line() As Single
  BulkLine_No_line = msglBulkLineNo_line
End Property
'Public Property Let BulkLine_line(ByVal sglBulkLine_line As Single)
'  msglBulkLine_line = sglBulkLine_line
'End Property
'Public Property Get BulkLine_line() As Single
'  BulkLine_line = msglBulkLine_line
'End Property
Public Property Let FromOrder_line(ByVal strFromOrder_line As String)
   mstrFromOrder_line = strFromOrder_line
End Property
Public Property Get FromOrder_line() As String
  FromOrder_line = mstrFromOrder_line
End Property
Public Property Let POLine_No_line(ByVal intPOLine_No_line As Integer)
  mintPOLineNo_line = intPOLine_No_line
End Property
Public Property Get POLine_No_line() As Integer
  POLine_No_line = mintPOLineNo_line
End Property
Public Property Let PackID_line(ByVal strPackID_line As String)
  mstrPackId_line = strPackID_line
End Property
Public Property Get PackID_line() As String
  PackID_line = mstrPackId_line
End Property
Public Property Let GL_Cost(ByVal strGL_Cost As String)
  mstrGL_Cost_line = strGL_Cost
End Property
Public Property Get GL_Cost() As String
  GL_Cost = mstrGL_Cost_line
End Property
Public Property Let GL_Sales(ByVal strGL_Sales As String)
  mstrGL_Sales_line = strGL_Sales
End Property
Public Property Get GL_Sales() As String
  GL_Sales = mstrGL_Sales_line
End Property
Public Property Let Cost_line(ByVal sglCost_line As Single)
  msglCost_line = sglCost_line
End Property
Public Property Get Cost_line() As Single
  Cost_line = msglCost_line
End Property
Public Property Let PO_line(ByVal strPO_line As String)
  mstrPO_line = strPO_line
End Property
Public Property Get PO_line() As String
  PO_line = mstrPO_line
End Property
Public Property Let Comm2_line(ByVal sglComm2_line As Single)
  msglComm2_line = sglComm2_line
End Property
Public Property Get Comm2_line() As Single
  Comm2_line = msglComm2_line
End Property
Public Property Let Comm1_line(ByVal sglComm1_line As Single)
  msglComm1_line = sglComm1_line
End Property
Public Property Get Comm1_line() As Single
  Comm1_line = msglComm1_line
End Property
Public Property Let Note_Mem_line(ByVal strNote_Mem_line As String)
  mstrNote_mem_line = strNote_Mem_line
End Property
Public Property Get Note_Mem_line() As String
  Note_Mem_line = mstrNote_mem_line
End Property
Public Property Let PWght_line(ByVal sglPWght_line As Single)
  msglPWght_line = sglPWght_line
End Property
Public Property Get PWght_line() As Single
  PWght_line = msglPWght_line
End Property
Public Property Let Pck8_line(ByVal sglPck8_line As Single)
  msglPck8_line = sglPck8_line
End Property
Public Property Get Pck8_line() As Single
  Pck8_line = msglPck8_line
End Property
Public Property Let Pck7_line(ByVal sglPck7_line As Single)
  msglPck7_line = sglPck7_line
End Property
Public Property Get Pck7_line() As Single
  Pck7_line = msglPck7_line
End Property
Public Property Let Pck6_line(ByVal sglPck6_line As Single)
  msglPck6_line = sglPck6_line
End Property
Public Property Get Pck6_line() As Single
  Pck6_line = msglPck6_line
End Property
Public Property Let Pck5_line(ByVal sglPck5_line As Single)
  msglPck5_line = sglPck5_line
End Property
Public Property Get Pck5_line() As Single
  Pck5_line = msglPck5_line
End Property
Public Property Let Pck4_line(ByVal sglPck4_line As Single)
  msglPck4_line = sglPck4_line
End Property
Public Property Get Pck4_line() As Single
  Pck4_line = msglPck4_line
End Property
Public Property Let Pck3_line(ByVal sglPck3_line As Single)
  msglPck3_line = sglPck3_line
End Property
Public Property Get Pck3_line() As Single
  Pck3_line = msglPck3_line
End Property
Public Property Let Pck2_line(ByVal sglPck2_line As Single)
  msglPck2_line = sglPck2_line
End Property
Public Property Get Pck2_line() As Single
  Pck2_line = msglPck2_line
End Property
Public Property Let Pck1_line(ByVal sglPck1_line As Single)
  msglPck1_line = sglPck1_line
End Property
Public Property Get Pck1_line() As Single
  Pck1_line = msglPck1_line
End Property
Public Property Let Total_Poalo_line(ByVal sglTotal_Poalo_line As Single)
  msglTot_Poalo_line = Total_Poalo_line
End Property
Public Property Get Total_Poalo_line() As Single
  Total_Poalo_line = msglTot_Poalo_line
End Property
Public Property Let Poalo8_line(ByVal sglPoalo8_line As Single)
  msglpoalo8_line = sglPoalo8_line
End Property
Public Property Get Poalo8_line() As Single
  Poalo8_line = msglpoalo8_line
End Property
Public Property Let Poalo7_line(ByVal sglPoalo7_line As Single)
  msglpoalo7_line = sglPoalo7_line
End Property
Public Property Get Poalo7_line() As Single
  Poalo7_line = msglpoalo7_line
End Property
Public Property Let Poalo6_line(ByVal sglPoalo6_line As Single)
  msglpoalo6_line = sglPoalo6_line
End Property
Public Property Get Poalo6_line() As Single
  Poalo6_line = msglpoalo6_line
End Property
Public Property Let Poalo5_line(ByVal sglPoalo5_line As Single)
  msglpoalo5_line = sglPoalo5_line
End Property
Public Property Get Poalo5_line() As Single
  Poalo5_line = msglpoalo5_line
End Property
Public Property Let Poalo4_line(ByVal sglPoalo4_line As Single)
  msglpoalo4_line = sglPoalo4_line
End Property
Public Property Get Poalo4_line() As Single
  Poalo4_line = msglpoalo4_line
End Property
Public Property Let Poalo3_line(ByVal sglPoalo3_line As Single)
  msglpoalo3_line = sglPoalo3_line
End Property
Public Property Get Poalo3_line() As Single
  Poalo3_line = msglpoalo3_line
End Property
Public Property Let Poalo2_line(ByVal sglPoalo2_line As Single)
  msglpoalo2_line = sglPoalo2_line
End Property
Public Property Get Poalo2_line() As Single
  Poalo2_line = msglpoalo2_line
End Property
Public Property Let Poalo1_line(ByVal sglPoalo1_line As Single)
  msglpoalo1_line = sglPoalo1_line
End Property
Public Property Get Poalo1_line() As Single
  Poalo1_line = msglpoalo1_line
End Property
Public Property Let Total_Cut_line(ByVal sglTotal_Cut_line As Single)
  msglTotCut_line = sglTotal_Cut_line
End Property
Public Property Get Total_Cut_line() As Single
  Total_Cut_line = msglTotCut_line
End Property
Public Property Let Cut8_line(ByVal sglCut8_line As Single)
  msglCut8_line = sglCut8_line
End Property
Public Property Get Cut8_line() As Single
  Cut8_line = msglCut8_line
End Property
Public Property Let Cut7_line(ByVal sglCut7_line As Single)
  msglCut7_line = sglCut7_line
End Property
Public Property Get Cut7_line() As Single
  Cut7_line = msglCut7_line
End Property
Public Property Let Cut6_line(ByVal sglCut6_line As Single)
  msglCut6_line = sglCut6_line
End Property
Public Property Get Cut6_line() As Single
  Cut6_line = msglCut6_line
End Property
Public Property Let Cut5_line(ByVal sglCut5_line As Single)
  msglCut5_line = sglCut5_line
End Property
Public Property Get Cut5_line() As Single
  Cut5_line = msglCut5_line
End Property
Public Property Let Cut4_line(ByVal sglCut4_line As Single)
  msglCut4_line = sglCut4_line
End Property
Public Property Get Cut4_line() As Single
  Cut4_line = msglCut4_line
End Property
Public Property Let Cut3_line(ByVal sglCut3_line As Single)
  msglCut3_line = sglCut3_line
End Property
Public Property Get Cut3_line() As Single
  Cut3_line = msglCut3_line
End Property
Public Property Let Cut2_line(ByVal sglCut2_line As Single)
  msglCut2_line = sglCut2_line
End Property
Public Property Get Cut2_line() As Single
  Cut2_line = msglCut2_line
End Property
Public Property Let Cut1_line(ByVal sglCut1_line As Single)
  msglCut1_line = sglCut1_line
End Property
Public Property Get Cut1_line() As Single
  Cut1_line = msglCut1_line
End Property
Public Property Let Allocated_line(ByVal sglglAllocated_line As String)
  msglAllocated_line = sglglAllocated_line
End Property
Public Property Get Allocated_line() As String
  Allocated_line = msglAllocated_line
End Property
Public Property Let Dylot_line(ByVal strDylot_line As String)
  mstrDylot_line = strDylot_line
End Property
Public Property Get Dylot_line() As String
  Dylot_line = mstrDylot_line
End Property
Public Property Let CutTkt_line(ByVal strCutTkt_line As String)
  mstrCutTKT_line = strCutTkt_line
End Property
Public Property Get CutTkt_line() As String
  CutTkt_line = mstrCutTKT_line
End Property
Public Property Let Flag_line(ByVal strFlag_line As String)
  mstrFlag_line = strFlag_line
End Property
Public Property Get Flag_line() As String
  Flag_line = mstrFlag_line
End Property
Public Property Let InvDate_line(ByVal datInvDate_line As Date)
  mdatInvDate_line = datInvDate_line
End Property
Public Property Get InvDate_line() As Date
  InvDate_line = mdatInvDate_line
End Property
Public Property Let Invoice_line(ByVal strInvoice_line As String)
  mstrInvoice_line = strInvoice_line
End Property
Public Property Get Invoice_line() As String
  Invoice_line = mstrInvoice_line
End Property
Public Property Let Picked_line(ByVal bolPicked_line As Boolean)
  mbolPicked_line = bolPicked_line
End Property
Public Property Get Picked_line() As Boolean
  Picked_line = mbolPicked_line
End Property
Public Property Let Pikdate_line(ByVal datPikDate_line As Date)
  mdatPikDate_line = datPikDate_line
End Property
Public Property Get Pikdate_line() As Date
  Pikdate_line = mdatPikDate_line
End Property
Public Property Let PikTkt_line(ByVal strPikTkt_line As String)
  mstrPikTkT_line = strPikTkt_line
End Property
Public Property Get PikTkt_line() As String
  PikTkt_line = mstrPikTkT_line
End Property
Public Property Let Total_Pik_line(ByVal sglTotal_Pik_line As Single)
  msglTotPik_line = sglTotal_Pik_line
End Property
Public Property Get Total_Pik_line() As Single
  Total_Pik_line = msglTotPik_line
End Property
Public Property Let Pik8_line(ByVal sglPik8_line As Single)
  msglPik8_line = sglPik8_line
End Property
Public Property Get Pik8_line() As Single
  Pik8_line = msglPik8_line
End Property
Public Property Let Pik7_line(ByVal sglPik7_line As Single)
  msglPik7_line = sglPik7_line
End Property
Public Property Get Pik7_line() As Single
  Pik7_line = msglPik7_line
End Property
Public Property Let Pik4_line(ByVal sglPik4_line As Single)
  msglPik4_line = sglPik4_line
End Property
Public Property Get Pik4_line() As Single
  Pik4_line = msglPik4_line
End Property
Public Property Let Pik6_line(ByVal sglPik6_line As Single)
  msglPik6_line = sglPik6_line
End Property
Public Property Get Pik6_line() As Single
  Pik6_line = msglPik6_line
End Property
Public Property Let Pik5_line(ByVal sglPik5_line As Single)
  msglPik5_line = sglPik5_line
End Property
Public Property Get Pik5_line() As Single
  Pik5_line = msglPik5_line
End Property
Public Property Let Pik3_line(ByVal sglPik3_line As Single)
  msglPik3_line = sglPik3_line
End Property
Public Property Get Pik3_line() As Single
  Pik3_line = msglPik3_line
End Property
Public Property Let Pik2_line(ByVal sglPik2_line As Single)
  msglPik2_line = sglPik2_line
End Property
Public Property Get Pik2_line() As Single
  Pik2_line = msglPik2_line
End Property
Public Property Let Pik1_line(ByVal sglPik1_line As Single)
  msglPik1_line = sglPik1_line
End Property
Public Property Get Pik1_line() As Single
  Pik1_line = msglPik1_line
End Property
Public Property Let Total_Book_line(ByVal sglTotal_Book_line As Single)
  msglTotBook_line = sglTotal_Book_line
End Property
Public Property Get Total_Book_line() As Single
  Total_Book_line = msglTotBook_line
End Property
Public Property Let Book8_line(ByVal sglBook8_line As Single)
  msglbook8_line = sglBook8_line
End Property
Public Property Get Book8_line() As Single
  Book8_line = msglbook8_line
End Property
Public Property Let Book7_line(ByVal sglBook7_line As Single)
  msglbook7_line = sglBook7_line
End Property
Public Property Get Book7_line() As Single
  Book7_line = msglbook7_line
End Property
Public Property Let Book5_line(ByVal sglBook5_line As Single)
  msglbook5_line = sglBook5_line
End Property
Public Property Get Book5_line() As Single
  Book5_line = msglbook5_line
End Property
Public Property Let Book6_line(ByVal sglBook6_line As Single)
  msglbook6_line = sglBook6_line
End Property
Public Property Get Book6_line() As Single
  Book6_line = msglbook6_line
End Property
Public Property Let Book4_line(ByVal sglBook4_line As Single)
  msglbook4_line = sglBook4_line
End Property
Public Property Get Book4_line() As Single
  Book4_line = msglbook4_line
End Property
Public Property Let Book3_line(ByVal sglBook3_line As Single)
  msglbook3_line = sglBook3_line
End Property
Public Property Get Book3_line() As Single
  Book3_line = msglbook3_line
End Property
Public Property Let Book2_line(ByVal sglBook2_line As Single)
  msglbook2_line = sglBook2_line
End Property
Public Property Get Book2_line() As Single
  Book2_line = msglbook2_line
End Property
Public Property Let Book1_line(ByVal sglBook1_line As Single)
  msglbook1_line = sglBook1_line
End Property
Public Property Get Book1_line() As Single
  Book1_line = msglbook1_line
End Property
Public Property Let Total_Qty_line(ByVal sglTotal_Qty_line As Single)
  msglTotQty_line = sglTotal_Qty_line
End Property
Public Property Get Total_Qty_line() As Single
  Total_Qty_line = msglTotQty_line
End Property
Public Property Let Qty8_line(ByVal sglQty8_line As Single)
  msglQty8_line = sglQty8_line
End Property
Public Property Get Qty8_line() As Single
  Qty8_line = msglQty8_line
End Property
Public Property Let Qty5_line(ByVal sglQty5_line As Single)
  msglQty5_line = sglQty5_line
End Property
Public Property Get Qty5_line() As Single
  Qty5_line = msglQty5_line
End Property
Public Property Let Qty7_line(ByVal sglQty7_line As Single)
  msglQty7_line = sglQty7_line
End Property
Public Property Get Qty7_line() As Single
  Qty7_line = msglQty7_line
End Property
Public Property Let Qty6_line(ByVal sglQty6_line As Single)
  msglQty6_line = sglQty6_line
End Property
Public Property Get Qty6_line() As Single
  Qty6_line = msglQty6_line
End Property
Public Property Let Qty4_line(ByVal sglQty4_line As Single)
  msglQty4_line = sglQty4_line
End Property
Public Property Get Qty4_line() As Single
  Qty4_line = msglQty4_line
End Property
Public Property Let Qty3_line(ByVal sglQty3_line As Single)
  msglQty3_line = sglQty3_line
End Property
Public Property Get Qty3_line() As Single
  Qty3_line = msglQty3_line
End Property
Public Property Let Qty2_line(ByVal sglQty2_line As Single)
  msglQty2_line = sglQty2_line
End Property
Public Property Get Qty2_line() As Single
  Qty2_line = msglQty2_line
End Property
Public Property Let Qty1_line(ByVal sglQty1_line As Single)
  msglQty1_line = sglQty1_line
End Property
Public Property Get Qty1_line() As Single
  Qty1_line = msglQty1_line
End Property
Public Property Let Complete_line(ByVal datComplete_line As Date)
  mdatComplete_line = datComplete_line
End Property
Public Property Get Complete_line() As Date
  Complete_line = mdatComplete_line
End Property
Public Property Let Start_line(ByVal datStart_line As Date)
  mdatStart_line = datStart_line
End Property
Public Property Get Start_line() As Date
  Start_line = mdatStart_line
End Property
Public Property Let Disc_pcnt_line(ByVal sglDisc_Pcnt_line As Single)
  msglDisc_Pcnt_line = sglDisc_Pcnt_line
End Property
Public Property Get Disc_pcnt_line() As Single
  Disc_pcnt_line = msglDisc_Pcnt_line
End Property
Public Property Let Gros_price_line(ByVal sglGros_price_line As Single)
  msglGros_Price_line = sglGros_price_line
End Property
Public Property Get Gros_price_line() As Single
  Gros_price_line = msglGros_Price_line
End Property
Public Property Let Sugretpri_line(ByVal sglSugretpri_line As Single)
  msglSugretpri_line = sglSugretpri_line
End Property
Public Property Get Sugretpri_line() As Single
  Sugretpri_line = msglSugretpri_line
End Property
Public Property Let Price_line(ByVal sglPrice_line As Single)
  msglPrice_line = sglPrice_line
End Property
Public Property Get Price_line() As Single
  Price_line = msglPrice_line
End Property
Public Property Let pPQty_line(ByVal intpPQty_line As Integer)
  mintPpQty_line = intpPQty_line
End Property
Public Property Get pPQty_line() As Integer
  pPQty_line = mintPpQty_line
End Property
Public Property Let PrePak_line(ByVal strPrePak_line As String)
  mstrPrePak_line = strPrePak_line
End Property
Public Property Get PrePak_line() As String
  PrePak_line = mstrPrePak_line
End Property
Public Property Let Group_line(ByVal strGroup_line As String)
  mstrGroup_line = strGroup_line
End Property
Public Property Get Group_line() As String
  Group_line = mstrGroup_line
End Property
Public Property Let Scale_line(ByVal strScale_line As String)
  mstrScale_line = strScale_line
End Property
Public Property Get Scale_line() As String
  Scale_line = mstrScale_line
End Property
Public Property Let Desc1_line(ByVal strDesc1_line As String)
  mstrDesc1_line = strDesc1_line
End Property
Public Property Get Desc1_line() As String
  Desc1_line = mstrDesc1_line
End Property
Public Property Let Season_line(ByVal strSeason_line As String)
  mstrSeason_line = strSeason_line
End Property
Public Property Get Season_line() As String
  Season_line = mstrSeason_line
End Property
Public Property Let AltColor_line(ByVal strAltColor_line As String)
  mstrAltColor_line = strAltColor_line
End Property
Public Property Get AltColor_line() As String
  AltColor_line = mstrAltColor_line
End Property
Public Property Let AltStyle_line(ByVal strAltStyle As String)
  mstraltStyle_line = strAltStyle
End Property
Public Property Get AltStyle_line() As String
  AltStyle_line = mstraltStyle_line
End Property
Public Property Let Style_line(ByVal strStyle_line As String)
  mstrStyle_line = strStyle_line
End Property
Public Property Get Style_line() As String
  Style_line = mstrStyle_line
End Property
Public Property Let CustPo_line(ByVal strCustPo_line As String)
  mstrCustPo_line = strCustPo_line
End Property
Public Property Get CustPo_line() As String
  CustPo_line = mstrCustPo_line
End Property
Public Property Let Store_line(ByVal strStore_line As String)
  mstrStore_line = strStore_line
End Property
Public Property Get Store_line() As String
  Store_line = mstrStore_line
End Property
Public Property Let shipments_line(ByVal intshipments_line As Integer)
  mintShipments_line = intshipments_line
End Property
Public Property Get shipments_line() As Integer
  shipments_line = mintShipments_line
End Property
Public Property Let Line_No_line(ByVal vData As Integer)
  mintLine_No_line = vData
End Property
Public Property Get Line_No_line() As Integer
  Line_No_line = mintLine_No_line
End Property
Public Property Let WareCode_line(ByVal strWareCode_line As String)
  mstrWareCode_line = strWareCode_line
End Property
Public Property Get WareCode_line() As String
  WareCode_line = mstrWareCode_line
End Property
Public Property Let Account_line(ByVal strAccount_line As String)
  mstrAccount_line = strAccount_line
End Property
Public Property Get Account_line() As String
  Account_line = mstrAccount_line
End Property
Public Property Let Order_line(ByVal strOrder_line As String)
  mstrOrder_line = strOrder_line
End Property
Public Property Get Order_line() As String
  Order_line = mstrOrder_line
End Property
Public Property Let OrderType_line(ByVal strOrderType_line As String)
  mstrOrdType_line = strOrderType_line
End Property
Public Property Get OrderType_line() As String
  OrderType_line = mstrOrdType_line
End Property
Public Property Get Mode() As UIchildMode
  Mode = mintMode
End Property
Public Property Let Mode(ByVal intMode As UIchildMode)
  mintMode = intMode
End Property
Public Property Get ConParameter() As String
  ConParameter = mstrConParameter
End Property
Public Property Let ConParameter(ByVal strConParameter As String)
  mstrConParameter = strConParameter
End Property
Public Property Get HeaderOnly() As Boolean
  HeaderOnly = mbolHeaderOnly
End Property
Public Property Let HeaderOnly(ByVal bolHeaderOnly As Boolean)
  mbolHeaderOnly = bolHeaderOnly
End Property
Public Function Add(Optional ByRef strErrMsg As String = "") As Boolean
  Mode = UIAddMode
  Add = True
End Function
Public Function Load(ByVal strKey As String, _
                     Optional ByRef strErrMsg As String = "") As Boolean
  Mode = UIEditMode
  Load = True
End Function
Public Function Edit(Optional ByRef strErrMsg As String = "") As Boolean
  Mode = UIEditMode
  Edit = True
End Function
Public Function Cancel(Optional ByRef strErrMsg As String = "") As Boolean
  Mode = UIViewMode
  Call SetProperties(SetPropertiesToOldValues)
End Function
Public Function Save(Optional ByRef strErrMsg As String = "") As Boolean
  Mode = UIViewMode
  Save = True
End Function
Public Function Delete(Optional ByVal strKey As String = "", _
                       Optional ByRef strErrMsg As String = "") As Boolean
  Delete = True
End Function
Public Function Move(ByVal strWhere As String, _
                     Optional ByRef strErrMsg As String = "") As Boolean
  Move = True
End Function
Public Function MoveFirst(Optional ByVal strOrderBy As String = "", _
                          Optional ByRef strErrMsg As String = "") As Boolean
  MoveFirst = True
End Function
Public Function MoveLast(Optional ByVal strOrderBy As String = "", _
                         Optional ByRef strErrMsg As String = "") As Boolean
  MoveLast = True
End Function
Public Function MoveNext(Optional ByVal strKey As String = "", _
                         Optional ByVal strOrderBy As String = "", _
                         Optional ByRef strErrMsg As String = "") As Boolean
  MoveNext = True
End Function
Public Function MovePrevious(Optional ByVal strKey As String = "", _
                             Optional ByVal strOrderBy As String = "", _
                             Optional ByRef strErrMsg As String = "") As Boolean
  MovePrevious = True
End Function
Private Sub Class_Initialize()
  Set mobjHeaderRecord = Nothing
End Sub
Private Sub Class_Terminate()
  If Not mobjHeaderRecord Is Nothing Then
    If mobjHeaderRecord.State <> adStateClosed Then
      mobjHeaderRecord.Close
    End If
    Set mobjHeaderRecord = Nothing
  End If
End Sub
Private Sub SetProperties(ByVal intSetTo As PropertiesSettings)
  Select Case intSetTo
    Case SetPropertiesFromRecordSet
    Case SetPropertiesToDefaults
    Case SetPropertiesToOldValues
    Case SetPropertiesToRecordSet
  End Select
End Sub
