VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "UIOrd"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Attribute VB_Ext_KEY = "SavedWithClassBuilder6" ,"Yes"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
Attribute VB_Ext_KEY = "RVB_UniqueId" ,"398578F601C2"
'******************************************************************************************
'* Auther: Ahmed Sayed Ahmed Ali
'* Date  : 8/1/2000
'* Version: 2.7.0
'* Description:
'* To Hold The Parent Child UI Object.
'******************************************************************************************

Option Explicit

' This constant should define the Object ID from the Object Dictionary
Const OBJECT_CODE As String = ""

' Local Variables To Hold The Properties  Values

Private mobjHeaderRecord As ADODB.Recordset
Private mobjOrderLine As ADODB.Recordset

Private mstrConParameter As String
Private mbolHeaderOnly As Boolean
Private mtypMode As UIModes
Private mbolClearBrokenRules As Boolean

' UI Modes
Public Enum UIModes
  UIAddMode = 1
  UIEditMode = 2
  UIViewMode = 3
End Enum


Public Enum OrderChild
  OrderLine = 1
End Enum

Private Enum PropertiesSettings
  SetPropertiesFromRecordSet = 1
  SetPropertiesToRecordSet = 2
  SetPropertiesToDefaults = 3
  SetPropertiesToOldValues = 4
End Enum



Private mstrOrderType As String
Private mstrOrder As String
Private mstrSTATUS As String
Private mstrAccount As String
Private mstrStore As String
Private mstrDept As String
Private mbolMultiPo As Boolean
Private mstrCustPo As String
Private mstrNote1 As String
Private mstrNote2 As String
Private mstrPriority As String
Private mstrOrderCat As String
Private mstrSeason As String
Private mstrDivision As String
Private mstrBulk As String
Private mstrReOrder As String
Private mstrConsol As String
Private mstrMulti As String
Private mstrTermCode As String
Private mstrShipVia As String
Private mstrSpcInst As String
Private mstrStName As String
Private mstrInsurance As String
Private mstrBuyer As String
Private mstrPhone As String
Private mstrFacCode As String
Private mstrFactAcct As String
Private mstrApproval As String
Private mintApprovAmnt As Integer
Private mdatDeclDate As Date
Private mstrDeclCode As String
Private mstrRep1 As String
Private msglCom1 As Single
Private mstrRep2 As String
Private msglCom2 As Single
Private mdatStarted As Date
Private mdatEntered As Date
Private mdatCompleted As Date
Private mdatCanceled As Date
Private msglDiscount As Single
Private mintBook As Integer
Private msglBookAmnt As Single
Private msglCancel As Integer
Private msglCancelAmnt As Single
Private mintShip As Integer
Private msglShipAmnt As Single
Private mintOpenOrd As Integer
Private msglOpenAmnt As Single
Private mintLastLine As Integer
Private mintTotCut As Integer
Private mstrFlag As String
Private mstrLink_Code As String
Private mstrGL_Sales As String
Private mstrInt_Vend As String
Private mstrEventCode As String
Private mstrBillNo As String
Private mstrMerc_Type As String
Private mstrBlankOrder As String
Private mstrDistrb_No As String
Private mstrClass As String
Private mstrMon_Flg As String
Private mintLabels As Integer
Private mbolAlt_ShpTo As Boolean
Private mstrCwareCode As String
Private mstrCancelReason As String
Private mstrAddress1 As String
Private mstrAddress2 As String
Private mstrAddress3 As String
Private mstrAddress4 As String
Private mstraddress5 As String
Private mstrCurrCode As String
Private msglExch_Rate As Single
Private msglCurrUnit As Single
Private mstrFromOrder As String
Private mbolDirect_Inv As Boolean
Private mbolHas_Notes As Boolean
Private mstrAdd_User As String
Private mstrAdd_time As String
Private mdatAdd_date As Date
Private mbolLok_Stat As Boolean
Private mstrlok_User As String
Private mdatLok_date As Date
Private mstrLok_Time As String
Private mbolEDI_Order As Boolean
Private mstrOwner As String
Private mbolFrom_Web As Boolean
'wal_ issue# 128815 add new field in order header[start]
Private mstrContRef  As String
'wal_131300 add new user fileds
Private mstrWebID  As String
Private mstrWebName  As String

Public Property Let ContractID(ByVal strContRef As String)
  mstrContRef = strContRef
End Property
Public Property Get ContractID() As String
  ContractID = mstrContRef
End Property
Public Property Let WebUserID(ByVal strWebID As String)
  mstrWebID = strWebID
End Property
Public Property Get WebUserID() As String
  WebUserID = mstrWebID
End Property
Public Property Let WebUserName(ByVal strWebName As String)
  mstrWebName = strWebName
End Property
Public Property Get WebUserName() As String
  WebUserName = mstrWebName
End Property
Public Property Let From_Web(ByVal bolFromWeb As Boolean)
  mbolFrom_Web = bolFromWeb
End Property
Public Property Get From_Web() As Boolean
  From_Web = mbolFrom_Web
End Property
'wal_ issue# 128815 add new field in order header[end]
Public Property Let Order_type(ByVal strOrderType As String)
  mstrOrderType = strOrderType
End Property
Public Property Get Order_type() As String
  Order_type = mstrOrderType
End Property
Public Property Let Ordered(ByVal strOrder As String)
  mstrOrder = strOrder
End Property
Public Property Get Ordered() As String
   Ordered = mstrOrder
End Property
Public Property Let stats(ByVal strStatus As String)
  mstrSTATUS = strStatus
End Property
Public Property Get stats() As String
  mstrSTATUS = stats
End Property
Public Property Let Acount(ByVal strAccount As String)
  mstrAccount = strAccount
End Property
Public Property Get Acount() As String
  Acount = mstrAccount
End Property
Public Property Let stor(ByVal strstore As String)
  mstrStore = strstore
End Property
Public Property Get stor() As String
  stor = mstrStore
End Property
Public Property Let Department(ByVal strdept As String)
  mstrDept = strdept
End Property
 Public Property Get Department() As String
  Department = mstrDept
End Property
Public Property Let Multi_POrder(ByVal bolmultipo As Boolean)
  mbolMultiPo = bolmultipo
End Property
Public Property Get Multi_POrder() As Boolean
  Multi_POrder = mbolMultiPo
End Property
Public Property Let Cust_po(ByVal strCustPo As String)
  mstrCustPo = strCustPo
End Property
Public Property Get Cust_po() As String
   Cust_po = mstrCustPo
End Property
Public Property Let Note_1(ByVal strnote1 As String)
  mstrNote1 = strnote1
End Property
Public Property Get Note_1() As String
  Note_1 = mstrNote1
End Property
Public Property Let Note_2(ByVal strNote2 As String)
  mstrNote2 = strNote2
End Property
Public Property Get Note_2() As String
  Note_2 = mstrNote2
End Property
Public Property Let Priorities(ByVal strPriorities As String)
  mstrPriority = strPriorities
End Property
Public Property Get Priorities() As String
  Priorities = mstrPriority
End Property
Public Property Let Order_Cat(ByVal strOrderCat As String)
  mstrOrderCat = strOrderCat
End Property
Public Property Get Order_Cat() As String
  Order_Cat = mstrOrderCat
End Property
Public Property Let Seasons(ByVal strSeason As String)
  mstrSeason = strSeason
End Property
Public Property Get Seasons() As String
   Seasons = mstrSeason
End Property
Public Property Let Divisions(ByVal strDivision As String)
  mstrDivision = strDivision
End Property
Public Property Get Divisions() As String
   Divisions = mstrDivision
End Property
Public Property Let Bulk_Order(ByVal strBulk As String)
  mstrBulk = strBulk
End Property
Public Property Get Bulk_Order() As String
   Bulk_Order = mstrBulk
End Property
Public Property Let Re_Order(ByVal strReOrder As String)
  mstrReOrder = strReOrder
End Property
Public Property Get Re_Order() As String
   Re_Order = mstrReOrder
End Property
Public Property Let Consols(ByVal strConsol As String)
  mstrConsol = strConsol
End Property
Public Property Get Consols() As String
   Consols = mstrConsol
End Property
Public Property Let Multi_Store(ByVal strMulti As String)
  mstrMulti = strMulti
End Property
Public Property Get Multi_Store() As String
   Multi_Store = mstrMulti
End Property
Public Property Let Term_Code(ByVal strTermCode As String)
  mstrTermCode = strTermCode
End Property
Public Property Get Term_Code() As String
   Term_Code = mstrTermCode
End Property
Public Property Let Ship_Via(ByVal strShipVia As String)
  mstrShipVia = strShipVia
End Property
Public Property Get Ship_Via() As String
   Ship_Via = mstrShipVia
End Property
Public Property Let Spc_Inst(ByVal strSpcInst As String)
  mstrSpcInst = strSpcInst
End Property
Public Property Get Spc_Inst() As String
   Spc_Inst = mstrSpcInst
End Property
Public Property Let St_Name(ByVal strStName As String)
  mstrStName = strStName
End Property
Public Property Get St_Name() As String
   St_Name = mstrStName
End Property
Public Property Let Do_Insurance(ByVal strInsurance As String)
  mstrInsurance = strInsurance
End Property
Public Property Get Do_Insurance() As String
   Do_Insurance = mstrInsurance
End Property
Public Property Let Buyer_Name(ByVal strBuyer As String)
  mstrBuyer = strBuyer
End Property
Public Property Get Buyer_Name() As String
   Buyer_Name = mstrBuyer
End Property
Public Property Let Phone_No(ByVal strPhone As String)
  mstrPhone = strPhone
End Property
Public Property Get Phone_No() As String
   Phone_No = mstrPhone
End Property
Public Property Let Fac_Code(ByVal strFacCode As String)
  mstrFacCode = strFacCode
End Property
Public Property Get Fac_Code() As String
  Fac_Code = mstrFacCode
End Property
Public Property Let FactAcct(ByVal strFactAcct As String)
  mstrFactAcct = strFactAcct
End Property
Public Property Get FactAcct() As String
  FactAcct = mstrFactAcct
End Property
Public Property Let Do_Approval(ByVal strApproval As String)
  mstrApproval = strApproval
End Property
Public Property Get Do_Approval() As String
  Do_Approval = mstrApproval
End Property
Public Property Let Approval_Amount(ByVal strApprovAmnt As String)
  mintApprovAmnt = strApprovAmnt
End Property
Public Property Get Approval_Amount() As String
  Approval_Amount = mstrApproval
End Property
Public Property Let Decl_Date(ByVal strDeclDate As String)
  mdatDeclDate = strDeclDate
End Property
Public Property Get Decl_Date() As String
  Decl_Date = mdatDeclDate
End Property
Public Property Let Decl_Code(ByVal strDeclCode As String)
  mstrDeclCode = strDeclCode
End Property
Public Property Get Decl_Code() As String
  Decl_Code = mstrDeclCode
End Property
Public Property Let Rep_1(ByVal strRep1 As String)
  mstrRep1 = strRep1
End Property
Public Property Get Rep_1() As String
  Rep_1 = mstrRep1
End Property
Public Property Let Com_1(ByVal sglcom1 As String)
  msglCom1 = sglcom1
End Property
Public Property Get Com_1() As String
  Com_1 = msglCom1
End Property
Public Property Let Rep_2(ByVal strRep2 As String)
  mstrRep2 = strRep2
End Property
Public Property Get Rep_2() As String
  Rep_2 = mstrRep2
End Property
Public Property Let Com_2(ByVal sglcom2 As String)
  msglCom2 = sglcom2
End Property
Public Property Get Com_2() As String
  Com_2 = msglCom2
End Property
Public Property Let Started(ByVal datStarted As Date)
  mdatStarted = datStarted
End Property
Public Property Get Started() As Date
  Started = mdatStarted
End Property
Public Property Let Entered(ByVal datEntered As Date)
  mdatEntered = datEntered
End Property
Public Property Get Entered() As Date
  Entered = mdatEntered
End Property
Public Property Let Completed(ByVal datCompleted As Date)
  mdatCompleted = datCompleted
End Property
Public Property Get Completed() As Date
  Completed = mdatCompleted
End Property
Public Property Let Canceled(ByVal datCanceled As Date)
  mdatCanceled = datCanceled
End Property
Public Property Get Canceled() As Date
  Canceled = mdatCanceled
End Property
Public Property Let Discount(ByVal sglDiscount As Single)
  msglDiscount = sglDiscount
End Property
Public Property Get Discount() As Single
  Discount = msglDiscount
End Property
Public Property Let Book(ByVal intBook As Integer)
  mintBook = intBook
End Property
Public Property Get Book() As Integer
  Book = mintBook
End Property
Public Property Let Book_Amount(ByVal sglBookAmnt As Single)
  msglBookAmnt = sglBookAmnt
End Property
Public Property Get Book_Amount() As Single
  Book_Amount = msglBookAmnt
End Property
Public Property Let sCancel(ByVal sglCancel As Integer)
  msglCancel = sglCancel
End Property
Public Property Get sCancel() As Integer
  sCancel = msglCancel
End Property
Public Property Let CancelAmnt(ByVal sglCancelAmnt As Integer)
  msglCancelAmnt = CancelAmnt
End Property
Public Property Get CancelAmnt() As Integer
  CancelAmnt = msglCancelAmnt
End Property
Public Property Let Ship(ByVal intShip As Integer)
  mintShip = intShip
End Property
Public Property Get Ship() As Integer
  Ship = mintShip
End Property
Public Property Let Ship_Amount(ByVal sglShipAmnt As Single)
  msglShipAmnt = sglShipAmnt
End Property
Public Property Get Ship_Amount() As Single
  Ship_Amount = msglShipAmnt
End Property
Public Property Let OpenOrd(ByVal intOpenOrd As Integer)
  mintOpenOrd = intOpenOrd
End Property
Public Property Get OpenOrd() As Integer
  OpenOrd = mintOpenOrd
End Property
Public Property Let OpenAmnt(ByVal sglOpenAmnt As Single)
  msglOpenAmnt = sglOpenAmnt
End Property
Public Property Get OpenAmnt() As Single
  OpenAmnt = msglOpenAmnt
End Property
Public Property Let LastLine(ByVal intLastLine As Integer)
  mintLastLine = intLastLine
End Property
Public Property Get LastLine() As Integer
  LastLine = mintLastLine
End Property
Public Property Let TotCut(ByVal intTotCut As Integer)
  mintTotCut = intTotCut
End Property
Public Property Get TotCut() As Integer
  TotCut = mintTotCut
End Property
Public Property Let Flag(ByVal strFlag As String)
  mstrFlag = strFlag
End Property
Public Property Get Flag() As String
  Flag = mstrFlag
End Property
Public Property Let Link_Code(ByVal strLink_Code As String)
  mstrLink_Code = strLink_Code
End Property
Public Property Get Link_Code() As String
  Link_Code = mstrLink_Code
End Property
Public Property Let GL_Sales(ByVal strGL_Sales As String)
  mstrGL_Sales = strGL_Sales
End Property
Public Property Get GL_Sales() As String
  GL_Sales = mstrGL_Sales
End Property
Public Property Let Int_Vend(ByVal strInt_Vend As String)
  mstrInt_Vend = strInt_Vend
End Property
Public Property Get Int_Vend() As String
  Int_Vend = mstrInt_Vend
End Property
Public Property Let EventCode(ByVal strEventCode As String)
  mstrEventCode = strEventCode
End Property
Public Property Get EventCode() As String
  EventCode = mstrEventCode
End Property
Public Property Let BillNo(ByVal strBillNo As String)
  mstrBillNo = strBillNo
End Property
Public Property Get BillNo() As String
  BillNo = mstrBillNo
End Property
Public Property Let Merc_Type(ByVal strMerc_Type As String)
  mstrMerc_Type = strMerc_Type
End Property
Public Property Get Merc_Type() As String
  Merc_Type = mstrMerc_Type
End Property
Public Property Let BlankOrder(ByVal strBlankOrder As String)
  mstrBlankOrder = strBlankOrder
End Property
Public Property Get BlankOrder() As String
  BlankOrder = mstrBlankOrder
End Property
Public Property Let Distrb_No(ByVal strDistrb_No As String)
  mstrDistrb_No = strDistrb_No
End Property
Public Property Get Distrb_No() As String
  Distrb_No = mstrDistrb_No
End Property
Public Property Let Class(ByVal strClass As String)
  mstrClass = strClass
End Property
Public Property Get Class() As String
  Class = mstrClass
End Property
Public Property Let Mon_Flag(ByVal strMon_Flag As String)
  mstrMon_Flg = strMon_Flag
End Property
Public Property Get Mon_Flag() As String
  Mon_Flag = mstrMon_Flg
End Property
Public Property Let Labels(ByVal intLabels As Integer)
  mintLabels = intLabels
End Property
Public Property Get Labels() As Integer
  Labels = mintLabels
End Property
Public Property Let Alt_ShpTo(ByVal bolAlt_ShpTo As Boolean)
  mbolAlt_ShpTo = bolAlt_ShpTo
End Property
Public Property Get Alt_ShpTo() As Boolean
  Alt_ShpTo = mbolAlt_ShpTo
End Property
Public Property Let CwareCode(ByVal strcwarecode As String)
  mstrCwareCode = strcwarecode
End Property
Public Property Get CwareCode() As String
  CwareCode = mstrCwareCode
End Property
Public Property Let CancelReason(ByVal strCancelReason As String)
  mstrCancelReason = strCancelReason
End Property
Public Property Get CancelReason() As String
  CancelReason = mstrCancelReason
End Property
Public Property Let Address1(ByVal strAddress1 As String)
  mstrAddress1 = strAddress1
End Property
Public Property Get Address1() As String
  Address1 = mstrAddress1
End Property
Public Property Let Address2(ByVal strAddress2 As String)
  mstrAddress2 = strAddress2
End Property
Public Property Get Address2() As String
  Address2 = mstrAddress2
End Property
Public Property Let Address3(ByVal strAddress3 As String)
  mstrAddress3 = strAddress3
End Property
Public Property Get Address3() As String
  Address3 = mstrAddress3
End Property
Public Property Let Address4(ByVal strAddress4 As String)
  mstrAddress4 = strAddress4
End Property
Public Property Get Address4() As String
  Address4 = mstrAddress4
End Property
Public Property Let address5(ByVal strAddress5 As String)
  mstraddress5 = strAddress5
End Property
Public Property Get address5() As String
  address5 = mstraddress5
End Property
Public Property Let CurrCode(ByVal strCurrCode As String)
  mstrCurrCode = strCurrCode
End Property
Public Property Get CurrCode() As String
  CurrCode = mstrCurrCode
End Property
Public Property Let Exch_Rate(ByVal sglExch_Rate As Single)
  msglExch_Rate = sglExch_Rate
End Property
Public Property Get Exch_Rate() As Single
  Exch_Rate = msglExch_Rate
End Property
Public Property Let CurrUnit(ByVal sglCurrUnit As Single)
  msglCurrUnit = sglCurrUnit
End Property
Public Property Get CurrUnit() As Single
  CurrUnit = msglCurrUnit
End Property
Public Property Let FromOrder(ByVal strFromOrder As String)
  mstrFromOrder = strFromOrder
End Property
Public Property Get FromOrder() As String
  FromOrder = mstrFromOrder
End Property
Public Property Let Direct_Inv(ByVal bolDirect_Inv As Boolean)
  mbolDirect_Inv = bolDirect_Inv
End Property
Public Property Get Direct_Inv() As Boolean
  Direct_Inv = mbolDirect_Inv
End Property
Public Property Let Has_Notes(ByVal bolHas_Notes As Boolean)
  mbolHas_Notes = bolHas_Notes
End Property
Public Property Get Has_Notes() As Boolean
  Has_Notes = mbolHas_Notes
End Property
Public Property Let Add_User(ByVal strAdd_User As String)
  mstrAdd_User = strAdd_User
End Property
Public Property Get Add_User() As String
  Add_User = mstrAdd_User
End Property
Public Property Let Add_Time(ByVal strAdd_Time As String)
  mstrAdd_time = strAdd_Time
End Property
Public Property Get Add_Time() As String
  Add_Time = mstrAdd_time
End Property
Public Property Let Add_Date(ByVal datAdd_date As Date)
  mdatAdd_date = datAdd_date
End Property
Public Property Get Add_Date() As Date
  Add_Date = mdatAdd_date
End Property
Public Property Let Lok_Stat(ByVal bolLok_Stat As Boolean)
  mbolLok_Stat = bolLok_Stat
End Property
Public Property Get Lok_Stat() As Boolean
  Lok_Stat = mbolLok_Stat
End Property
Public Property Let Lok_User(ByVal strLok_User As String)
  mstrlok_User = strLok_User
End Property
Public Property Get Lok_User() As String
  Lok_User = mstrlok_User
End Property
Public Property Let Lok_Date(ByVal datLok_date As Date)
  mdatLok_date = datLok_date
End Property
Public Property Get Lok_Date() As Date
  Lok_Date = mdatLok_date
End Property
Public Property Let Lok_Time(ByVal strLok_time As String)
  mstrLok_Time = strLok_time
End Property
Public Property Get Lok_Time() As String
  Lok_Time = mstrLok_Time
End Property
Public Property Let EDI_Order(ByVal bolEDI_Order As Boolean)
  mbolEDI_Order = bolEDI_Order
End Property
Public Property Get EDI_Order() As Boolean
  EDI_Order = mbolEDI_Order
End Property
Public Property Let Owner(ByVal strOwner As String)
  mstrOwner = strOwner
End Property
Public Property Get Owner() As String
  Owner = mstrOwner
End Property




'*******************************************************************************************
'* Auther: Ahmed Sayed Ahmed Ali
'* Version: 2.7.0
'* Description:
'* To Check If The RecordSet That Hold The Child Object In The BOF OR Not.
'* Parameters:
'* typChildID: The ChildID That We Will Test His Position .
'* Return Value:
'* This Method Return True If The RecordSet That Hold The Child Data In The BOF.
'*******************************************************************************************
Public Function ChildBOF(ByVal typChildID As OrderChild) As Boolean
  
  If Not mobjOrderLine Is Nothing Then
    ChildBOF = mobjOrderLine.BOF
  Else
    ChildBOF = True
  End If
  
End Function
  
'*******************************************************************************************
'* Auther: Ahmed Sayed Ahmed Ali
'* Version: 2.7.0
'* Description:
'* To Check If The RecordSet That Hold The Child Object In The EOF OR Not.
'* Parameters:
'* typChildID: The ChildID That We Will Test His Position .
'* Return Value:
'* This Method Return True If The RecordSet That Hold The Child Data In The EOF.
'*******************************************************************************************
Public Function ChildEOF(ByVal typChildID As OrderChild) As Boolean
  If Not mobjOrderLine Is Nothing Then
    ChildEOF = mobjOrderLine.EOF
  Else
    ChildEOF = True
  End If
  
End Function

Public Property Get Mode() As UIModes
  Mode = mtypMode
End Property

Private Property Let Mode(ByVal intMode As UIModes)
  mtypMode = intMode
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
'*******************************************************************************************
'* Auther: Ahmed Sayed Ahmed Ali
'* Version: 2.7.0
'* Description:
'* To Add Object In The UI
'* Return Value:
'* This Function Return True If The Function Successes.
'*******************************************************************************************
Public Function Add() As Boolean
  Dim objDBOrder As DBOrder.DBOrd
  
  Set objDBOrder = New DBOrder.DBOrd
  Set mobjHeaderRecord = objDBOrder.Add(ConParameter)
  Set mobjOrderLine = mobjHeaderRecord!OrderDetail.Value
  Mode = UIAddMode
  Add = Not mobjHeaderRecord Is Nothing
  Set objDBOrder = Nothing
  Add = True
End Function
'*******************************************************************************************
'* Auther: Ahmed Sayed Ahmed Ali
'* Version: 2.7.0
'* Description:
'* To Add Or Update Child Object In The RecordSet That Hold The Child Data.
'* Parameters:
'* typChildID: The ChildID That We Will Set.
'* objChild : The Child Object That We Will Add Or Update
'* Return Value:
'* This Function Return True If The Function Successes.
'*******************************************************************************************
Public Function ChildSet(ByVal typChildID As OrderChild, _
                         ByVal objChild As UIOrdChld) As Boolean
  Select Case typChildID
    Case OrderLine
      Call SetLineProperties(objChild, SetPropertiesToRecordSet)
      ChildSet = True
  End Select
  
End Function
'*******************************************************************************************
'* Auther: Ahmed Sayed Ahmed Ali
'* Version: 2.7.0
'* Description:
'* To Load The Data Of Given Key.
'* Parameters:
'* strKey: The Key Of The Item That We Will Load.
'* Return Value:
'* This Function Return True If The Function Successes.
'*******************************************************************************************
Public Function Load(ByVal strKey As String) As Boolean
  'set mobjOrderLine = headerrs!child.value
  Mode = UIEditMode
  Load = True
End Function
'*******************************************************************************************
'* Auther: Ahmed Sayed Ahmed Ali
'* Version: 2.7.0
'* Description:
'* To Get a Child Object From The RecordSet That Hold Child Data.
'* Parameters:
'* typChildID: The ChildID That We Will Get.
'* Return Value:
'* This Function Return Object Hold The Child Object.
'*******************************************************************************************
Public Function ChildGet(ByVal typChildID As OrderChild) As Object
'  Select Case typChildID
'    Case Response
'      Dim objLines As UIOrdChld
'      Set objLines = New UIOrdChld
'      Call SetLineProperties(objLines, SetPropertiesFromRecordSet)
'
'  End Select
'  Set ChildGet = objResponse
'  Set objResponse = Nothing
  
End Function
'*******************************************************************************************
'* Auther: Ahmed Sayed Ahmed Ali
'* Version: 2.7.0
'* Description:
'* To Edit Object In The UI
'* Return Value:
'* This Function Return True If The Function Successes.
'*******************************************************************************************
Public Function Edit() As Boolean
  Mode = UIEditMode
  Edit = True
End Function
'*******************************************************************************************
'* Auther: Ahmed Sayed Ahmed Ali
'* Version: 2.7.0
'* Description:
'* To Cancel Update In Object In The UI
'* Return Value:
'* This Function Return True If The Function Successes.
'*******************************************************************************************
Public Function Cancel() As Boolean
  Mode = UIViewMode
  Call SetProperties(SetPropertiesToOldValues)
End Function
'*******************************************************************************************
'* Auther: Ahmed Sayed Ahmed Ali
'* Version: 2.7.0
'* Description:
'* To Save Object In The UI
'* Return Value:
'* This Function Return True If The Function Successes.
'*******************************************************************************************
Public Function Save() As Boolean
  Call SetProperties(SetPropertiesToRecordSet)
  Dim objDB As DBOrder.DBOrd
  Set objDB = New DBOrder.DBOrd
  If objDB.Save(ConParameter, mobjHeaderRecord) Then
    Mode = UIViewMode
    Set objDB = Nothing
    Save = True
  Else
    Save = False
  End If
End Function
'*******************************************************************************************
'* Auther: Ahmed Sayed Ahmed Ali
'* Version: 2.7.0
'* Description:
'* To Delete Object In The UI
'* Parameters:
'* ChildID: [Optional] The Key Of The Item  That We Will Delete From UI.
'* Return Value:
'* This Function Return True If The Function Successes.
'*******************************************************************************************
Public Function Delete(Optional ByVal strKey As String = "") As Boolean
  Delete = True
End Function
'*******************************************************************************************
'* Auther: Ahmed Sayed Ahmed Ali
'* Version: 2.7.0
'* Description:
'* To Delete Child From The RecordSet That Hold The Child Data.
'* Parameters:
'* typChildID: The ChildID That We Will Delete From His RecordSet
'* Return Value:
'* This Method Return True If The Delete Successes.
'*******************************************************************************************
Public Function ChildDelete(ByVal typChildID As OrderChild) As Boolean
  
End Function
'*******************************************************************************************
'* Auther: Ahmed Sayed Ahmed Ali
'* Version: 2.7.0
'* Description:
'* To Move To Specific Object In The UI
'* Parameters:
'* strWhere: The Where Condition Used To Get The Item That We Will Move To Him .
'* Return Value:
'* This Function Return True If The Function Successes.
'*******************************************************************************************
Public Function Move(ByVal strWhere As String) As Boolean
  mobjHeaderRecord.Find strWhere
  Move = Not mobjHeaderRecord.EOF
End Function

Public Function MoveFirst(Optional ByVal strOrderBy As String = "") As Boolean
  mobjHeaderRecord.MoveFirst
  MoveFirst = True
End Function
'*******************************************************************************************
'* Auther: Ahmed Sayed Ahmed Ali
'* Version: 2.7.0
'* Description:
'* To Move To The First Record In The RecordSet That Hold The Child Data.
'* Parameters:
'* typChildID: The ChildID That We Will Move In His RecordSet
'* Return Value:
'* This Method Return True If The Move Successes.
'*******************************************************************************************
Public Function ChildMoveFirst(ByVal typChildID As OrderChild) As Boolean
  If Not mobjOrderLine Is Nothing Then
    mobjOrderLine.MoveFirst
    ChildMoveFirst = True
  Else
    ChildMoveFirst = False
  End If
End Function

Public Function MoveLast(Optional ByVal strOrderBy As String = "") As Boolean
  mobjHeaderRecord.MoveLast
  MoveLast = Not mobjHeaderRecord.EOF
End Function
'*******************************************************************************************
'* Auther: Ahmed Sayed Ahmed Ali
'* Version: 2.7.0
'* Description:
'* To Move To The Last Record In The RecordSet That Hold The Child Data.
'* Parameters:
'* typChildID: The ChildID That We Will Move In His RecordSet
'* Return Value:
'* This Method Return True If The Move Successes.
'*******************************************************************************************
Public Function ChildMoveLast(ByVal typChildID As OrderChild) As Boolean
  If Not mobjOrderLine Is Nothing Then
    mobjOrderLine.MoveLast
    ChildMoveLast = True
  Else
    ChildMoveLast = False
  End If
End Function

Public Function MoveNext(Optional ByVal strKey As String = "", _
                         Optional ByVal strOrderBy As String = "") As Boolean
  MoveNext = True
End Function
'*******************************************************************************************
'* Auther: Ahmed Sayed Ahmed Ali
'* Version: 2.7.0
'* Description:
'* To Move To The Next Record In The RecordSet That Hold The Child Data.
'* Parameters:
'* typChildID: The ChildID That We Will Move In His RecordSet
'* Return Value:
'* This Method Return True If The Move Successes.
'*******************************************************************************************
Public Function ChildMoveNext(ByVal typChildID As OrderChild) As Boolean
  ChildMoveNext = True
End Function

Public Function MovePrevious(Optional ByVal strKey As String = "", _
                             Optional ByVal strOrderBy As String = "") As Boolean
  MovePrevious = True
End Function
'*******************************************************************************************
'* Auther: Ahmed Sayed Ahmed Ali
'* Version: 2.7.0
'* Description:
'* To Move To The Previous Record In The RecordSet That Hold The Child Data.
'* Parameters:
'* typChildID: The ChildID That We Will Move In His RecordSet
'* Return Value:
'* This Method Return True If The Move Successes.
'*******************************************************************************************
Public Function ChildMovePrevious(ByVal typChildID As OrderChild) As Boolean
  ChildMovePrevious = True
End Function

Private Sub Class_Initialize()
  Set mobjHeaderRecord = New ADODB.Recordset
  'Set mobjBrokenRules = New FWBRRule.BrokenRules
End Sub

Private Sub Class_Terminate()
  If Not mobjHeaderRecord Is Nothing Then
    If mobjHeaderRecord.State <> adStateClosed Then
      mobjHeaderRecord.Close
    End If
    Set mobjHeaderRecord = Nothing
  End If
  
  'Set mobjBrokenRules = Nothing
  
End Sub

Private Sub SetProperties(ByVal intSetTo As PropertiesSettings)
  Select Case intSetTo
    Case SetPropertiesFromRecordSet
    
    Case SetPropertiesToDefaults
    
    Case SetPropertiesToOldValues
    
    Case SetPropertiesToRecordSet
      
      mobjHeaderRecord!cordtype = IIf(IsNull(mstrOrderType), "", mstrOrderType)
      mobjHeaderRecord!Order = IIf(IsNull(Ordered), "", Ordered)
      mobjHeaderRecord!Status = IIf(IsNull(mstrSTATUS), "", mstrSTATUS)
      mobjHeaderRecord!account = IIf(IsNull(mstrAccount), "", mstrAccount)
      mobjHeaderRecord!store = IIf(IsNull(mstrStore), "", mstrStore)
      mobjHeaderRecord!Dept = IIf(IsNull(Department), "", Department)
      mobjHeaderRecord!MultiPO = IIf(IsNull(mbolMultiPo), "", mbolMultiPo)
      mobjHeaderRecord!custpo = IIf(IsNull(Cust_po), "", Cust_po)
      mobjHeaderRecord!Note1 = IIf(IsNull(mstrNote1), "", mstrNote1)
      mobjHeaderRecord!Note2 = IIf(IsNull(mstrNote2), "", mstrNote2)
      mobjHeaderRecord!Priority = IIf(IsNull(mstrPriority), "", mstrPriority)
      mobjHeaderRecord!cOrderCat = IIf(IsNull(mstrOrderCat), "", mstrOrderCat)
      mobjHeaderRecord!season = IIf(IsNull(mstrSeason), "", mstrSeason)
      mobjHeaderRecord!cDivision = IIf(IsNull(mstrDivision), "", mstrDivision)
      mobjHeaderRecord!Bulk = IIf(IsNull(mstrBulk), "", mstrBulk)
      mobjHeaderRecord!cReOrder = IIf(IsNull(mstrReOrder), "", mstrReOrder)
      mobjHeaderRecord!Consol = IIf(IsNull(mstrConsol), "", mstrConsol)
      mobjHeaderRecord!Multi = IIf(IsNull(mstrMulti), "", mstrMulti)
      mobjHeaderRecord!cTermCode = IIf(IsNull(mstrTermCode), "", mstrTermCode)
      mobjHeaderRecord!ShipVia = IIf(IsNull(mstrShipVia), "", mstrShipVia)
      mobjHeaderRecord!SpcInst = IIf(IsNull(mstrSpcInst), "", mstrSpcInst)
      mobjHeaderRecord!StName = IIf(IsNull(mstrStName), "", mstrStName)
      mobjHeaderRecord!cInsur = IIf(IsNull(mstrInsurance), "", mstrInsurance)
      mobjHeaderRecord!Buyer = IIf(IsNull(mstrBuyer), "", mstrBuyer)
      mobjHeaderRecord!Phone = IIf(IsNull(mstrPhone), "", mstrPhone)
      mobjHeaderRecord!cFacCode = IIf(IsNull(mstrFacCode), "", mstrFacCode)
      mobjHeaderRecord!FactAcct = IIf(IsNull(mstrFactAcct), "", mstrFactAcct)
      mobjHeaderRecord!Approval = IIf(IsNull(mstrApproval), "", mstrApproval)
      mobjHeaderRecord!ApprAmt = IIf(IsNull(mintApprovAmnt), "", mintApprovAmnt)
      mobjHeaderRecord!Decl_Date = IIf(IsNull(mdatDeclDate), "", mdatDeclDate)
      mobjHeaderRecord!Decl_Code = IIf(IsNull(mstrDeclCode), "", mstrDeclCode)
      mobjHeaderRecord!Rep1 = IIf(IsNull(mstrRep1), "", mstrRep1)
      mobjHeaderRecord!comm1 = IIf(IsNull(msglCom1), "", msglCom1)
      mobjHeaderRecord!Rep2 = IIf(IsNull(mstrRep2), "", mstrRep2)
      mobjHeaderRecord!comm2 = IIf(IsNull(msglCom2), "", msglCom2)
      mobjHeaderRecord!start = IIf(IsNull(mdatStarted), "", mdatStarted)
      mobjHeaderRecord!Entered = IIf(IsNull(mdatEntered), "", mdatEntered)
      mobjHeaderRecord!complete = IIf(IsNull(mdatCompleted), "", mdatCompleted)
      mobjHeaderRecord!Cancelled = IIf(IsNull(mdatCanceled), "", mdatCanceled)
      mobjHeaderRecord!Disc = IIf(IsNull(msglDiscount), "", msglDiscount)
      mobjHeaderRecord!Book = IIf(IsNull(mintBook), "", mintBook)
      mobjHeaderRecord!BookAmt = IIf(IsNull(msglBookAmnt), "", msglBookAmnt)
      mobjHeaderRecord!Cancel = IIf(IsNull(msglCancel), "", msglCancel)
      mobjHeaderRecord!CancelAmt = IIf(IsNull(msglCancelAmnt), "", msglCancelAmnt)
      mobjHeaderRecord!Ship = IIf(IsNull(mintShip), "", mintShip)
      mobjHeaderRecord!ShipAmt = IIf(IsNull(msglShipAmnt), "", msglShipAmnt)
      mobjHeaderRecord!Open = IIf(IsNull(mintOpenOrd), "", mintOpenOrd)
      mobjHeaderRecord!OpenAmt = IIf(IsNull(msglOpenAmnt), "", msglOpenAmnt)
      mobjHeaderRecord!LastLine = IIf(IsNull(mintLastLine), "", mintLastLine)
      mobjHeaderRecord!TotCut = IIf(IsNull(mintTotCut), "", mintTotCut)
      mobjHeaderRecord!Flag = IIf(IsNull(mstrFlag), "", mstrFlag)
      mobjHeaderRecord!Link_Code = IIf(IsNull(mstrLink_Code), "", mstrLink_Code)
      mobjHeaderRecord!GL_Sales = IIf(IsNull(mstrGL_Sales), "", mstrGL_Sales)
      mobjHeaderRecord!Int_Vend = IIf(IsNull(mstrInt_Vend), "", mstrInt_Vend)
      mobjHeaderRecord!Event_Cod = IIf(IsNull(mstrEventCode), "", mstrEventCode)
      mobjHeaderRecord!BillNo = IIf(IsNull(mstrBillNo), "", mstrBillNo)
      mobjHeaderRecord!Merc_Type = IIf(IsNull(mstrMerc_Type), "", mstrMerc_Type)
      mobjHeaderRecord!Blank_Ord = IIf(IsNull(mstrBlankOrder), "", mstrBlankOrder)
      mobjHeaderRecord!Distrb_No = IIf(IsNull(Distrb_No), "", Distrb_No)
      mobjHeaderRecord!cClass = IIf(IsNull(mstrClass), "", mstrClass)
      mobjHeaderRecord!Mon_Flg = IIf(IsNull(mstrMon_Flg), "", mstrMon_Flg)
      mobjHeaderRecord!Alt_ShpTo = IIf(IsNull(mbolAlt_ShpTo), "", mbolAlt_ShpTo)
      mobjHeaderRecord!CwareCode = IIf(IsNull(mstrCwareCode), "", mstrCwareCode)
      mobjHeaderRecord!ccancreson = IIf(IsNull(mstrCancelReason), "", mstrCancelReason)
      mobjHeaderRecord!cAddress1 = IIf(IsNull(mstrAddress1), "", mstrAddress1)
      mobjHeaderRecord!cAddress2 = IIf(IsNull(mstrAddress2), "", mstrAddress2)
      mobjHeaderRecord!cAddress3 = IIf(IsNull(mstrAddress3), "", mstrAddress3)
      mobjHeaderRecord!cAddress4 = IIf(IsNull(mstrAddress4), "", mstrAddress4)
      mobjHeaderRecord!caddress5 = IIf(IsNull(mstraddress5), "", mstraddress5)
      mobjHeaderRecord!cCurrCode = IIf(IsNull(mstrCurrCode), "", mstrCurrCode)
      mobjHeaderRecord!nExRate = IIf(IsNull(msglExch_Rate), "", msglExch_Rate)
      mobjHeaderRecord!nCurrUnit = IIf(IsNull(msglCurrUnit), "", msglCurrUnit)
      mobjHeaderRecord!cfromorder = IIf(IsNull(mstrFromOrder), "", mstrFromOrder)
      mobjHeaderRecord!Direct_Inv = IIf(IsNull(mbolDirect_Inv), "", mbolDirect_Inv)
      mobjHeaderRecord!lHasNotes = IIf(IsNull(mbolHas_Notes), "", mbolHas_Notes)
      mobjHeaderRecord!cadd_user = IIf(IsNull(mstrAdd_User), "", mstrAdd_User)
      mobjHeaderRecord!cadd_time = IIf(IsNull(mstrAdd_time), Time(), mstrAdd_time)
      mobjHeaderRecord!dadd_date = IIf(IsNull(mdatAdd_date), Date, mdatAdd_date)
      mobjHeaderRecord!llok_stat = IIf(IsNull(mbolLok_Stat), "", mbolLok_Stat)
      mobjHeaderRecord!clok_user = IIf(IsNull(mstrlok_User), "", mstrlok_User)
      mobjHeaderRecord!dlok_date = IIf(IsNull(mdatLok_date), "", mdatLok_date)
      mobjHeaderRecord!clok_time = IIf(IsNull(mstrLok_Time), "", mstrLok_Time)
      mobjHeaderRecord!lEDIOrder = IIf(IsNull(mbolEDI_Order), "", mbolEDI_Order)
      mobjHeaderRecord!cowner = IIf(IsNull(mstrOwner), "", mstrOwner)
      mobjHeaderRecord!lFromWeb = IIf(IsNull(mbolFrom_Web), "", mbolFrom_Web)
      'wal_ issue# 128815 add new field in order header[start]
      mobjHeaderRecord!ccontref = IIf(IsNull(mstrContRef), "", mstrContRef)
       mobjHeaderRecord!cwebid = IIf(IsNull(mstrWebID), "", mstrWebID)
      mobjHeaderRecord!cwebname = IIf(IsNull(mstrWebName), "", mstrWebName)
  End Select


 
End Sub

'*******************************************************************************************
'* Auther: Ahmed Sayed Ahmed Ali
'* Version: 2.7.0
'* Description:
'* To Return The Records Count Of the RecordSet That Hold Child Data.
'* Parameters:
'* typChildID: The ChildID That We Will Get His Records Count.
'* Return Value:
'* This Method Return Long Value Represents The Records Count.
'*******************************************************************************************
Public Function ChildRecordCount(ByVal typChildID As OrderChild) As Long

End Function
'*******************************************************************************************
'* Auther: Ahmed Sayed Ahmed Ali
'* Version: 2.7.0
'* Description:
'* To Filter The RecordSet That Hold Child Data.
'* Parameters:
'* typChildID: The ChildID That We Will Get His Records Count.
'* strChildFilterExpression : The Expression That Will Be Use To Filter Data.
'*******************************************************************************************
Public Sub ChildFilter(ByVal typChildID As OrderChild, _
                       ByVal strChildFilterExpression As String)
  
End Sub
'*******************************************************************************************
'* Auther: Ahmed Sayed Ahmed Ali
'* Version: 2.7.0
'* Description:
'* To Find Child Object In The RecordSet That Hold Child Data.
'* Parameters:
'* typChildID: The ChildID That We Will Get His Records Count.
'* strCriteria : Find Criteria.
'*******************************************************************************************
Public Sub ChildFind(ByVal typChildID As OrderChild, _
                     ByVal strCriteria As String)

End Sub

'Public Property Get BrokenRules() As BrokenRules
  'Set BrokenRules = mobjBrokenRules
'End Property

'*******************************************************************************************
'* Auther: Ahmed Sayed Ahmed Ali
'* Version: 2.7.0
'* Description:
'* To Add a New Child Object.
'* Parameters:
'* typChildID: The ChildID That We Will Add.
'* Return Value:
'* This Function Return Object Hold The Child Object.
'*******************************************************************************************
Public Function ChildAddNew(ByVal typChildID As OrderChild) As UIOrdChld
  Dim objChild As UIOrdChld
  Set objChild = New UIOrdChld
  objChild.Mode = UIAddMode
  Set ChildAddNew = objChild
  Set objChild = Nothing
End Function
'Sub setOrdLineProperties()
 ' setpropertiestorecordset if child mode = add then mobjordline.addnew
'End Sub
Private Sub SetLineProperties(ByRef objLines As UIOrdChld, ByVal intSetTo As PropertiesSettings)
  'mobjResponses
  If Not mobjOrderLine Is Nothing Then
    Select Case intSetTo
      
      Case SetPropertiesFromRecordSet
        'objResponse.IssueNo = mobjResponses!IssueNo
        
      Case SetPropertiesToDefaults
      Case SetPropertiesToOldValues
      
      Case SetPropertiesToRecordSet
        If objLines.Mode = UIAddMode Then
          mobjOrderLine.AddNew
        End If
        mobjOrderLine!cordtype = objLines.OrdType_line
        mobjOrderLine!Order = Me.Ordered
        mobjOrderLine!account = objLines.Account_line
        mobjOrderLine!CwareCode = objLines.WareCode_line
        mobjOrderLine!lineno = objLines.Line_No_line
        mobjOrderLine!shipments = objLines.shipments_line
        mobjOrderLine!store = objLines.Store_line
        mobjOrderLine!custpo = objLines.CustPo_line
        mobjOrderLine!Style = objLines.Style_line
        mobjOrderLine!altstyle = objLines.AltStyle_line
        mobjOrderLine!altcolor = objLines.AltColor_line
        mobjOrderLine!season = objLines.Season_line
        mobjOrderLine!desc1 = objLines.Desc1_line
        mobjOrderLine!Scale = objLines.Scale_line
        mobjOrderLine!Group = objLines.Group_line
        mobjOrderLine!prepak = objLines.PrePak_line
        mobjOrderLine!ppqty = objLines.pPQty_line
        mobjOrderLine!price = objLines.Price_line
        mobjOrderLine!nsugretpri = objLines.Sugretpri_line
        mobjOrderLine!gros_price = objLines.Gros_price_line
        mobjOrderLine!disc_pcnt = objLines.Disc_pcnt_line
        mobjOrderLine!start = objLines.Start_line
        mobjOrderLine!complete = objLines.Complete_line
        mobjOrderLine!qty1 = objLines.Qty1_line
        mobjOrderLine!qty2 = objLines.Qty2_line
        mobjOrderLine!qty3 = objLines.Qty3_line
        mobjOrderLine!qty4 = objLines.Qty4_line
        mobjOrderLine!qty5 = objLines.Qty5_line
        mobjOrderLine!qty6 = objLines.Qty6_line
        mobjOrderLine!qty7 = objLines.Qty7_line
        mobjOrderLine!qty8 = objLines.Qty8_line
        mobjOrderLine!totqty = objLines.Total_Qty_line
        mobjOrderLine!book1 = objLines.Book1_line
        mobjOrderLine!book2 = objLines.Book2_line
        mobjOrderLine!book3 = objLines.Book3_line
        mobjOrderLine!book4 = objLines.Book4_line
        mobjOrderLine!book5 = objLines.Book5_line
        mobjOrderLine!book6 = objLines.Book6_line
        mobjOrderLine!book7 = objLines.Book7_line
        mobjOrderLine!book8 = objLines.Book8_line
        mobjOrderLine!totbook = objLines.Total_Book_line
        mobjOrderLine!pik1 = objLines.Pik1_line
        mobjOrderLine!pik2 = objLines.Pik2_line
        mobjOrderLine!pik3 = objLines.Pik3_line
        mobjOrderLine!pik4 = objLines.Pik4_line
        mobjOrderLine!pik5 = objLines.Pik5_line
        mobjOrderLine!pik6 = objLines.Pik6_line
        mobjOrderLine!pik7 = objLines.Pik7_line
        mobjOrderLine!pik8 = objLines.Pik8_line
        mobjOrderLine!totpik = objLines.Total_Pik_line
        mobjOrderLine!piktkt = objLines.PikTkt_line
        mobjOrderLine!pikdate = objLines.Pikdate_line
        mobjOrderLine!picked = objLines.Picked_line
        mobjOrderLine!invoice = objLines.Invoice_line
        mobjOrderLine!invdate = objLines.InvDate_line
        mobjOrderLine!Flag = objLines.Flag_line
        mobjOrderLine!cuttkt = objLines.CutTkt_line
        mobjOrderLine!dyelot = objLines.Dylot_line
        mobjOrderLine!allocated = objLines.Allocated_line
        mobjOrderLine!cut1 = objLines.Cut1_line
        mobjOrderLine!cut2 = objLines.Cut2_line
        mobjOrderLine!cut3 = objLines.Cut3_line
        mobjOrderLine!cut4 = objLines.Cut4_line
        mobjOrderLine!cut5 = objLines.Cut5_line
        mobjOrderLine!cut6 = objLines.Cut6_line
        mobjOrderLine!cut7 = objLines.Cut7_line
        mobjOrderLine!cut8 = objLines.Cut8_line
        mobjOrderLine!TotCut = objLines.Total_Cut_line
        mobjOrderLine!poalo1 = objLines.Poalo1_line
        mobjOrderLine!poalo2 = objLines.Poalo2_line
        mobjOrderLine!poalo3 = objLines.Poalo3_line
        mobjOrderLine!poalo4 = objLines.Poalo4_line
        mobjOrderLine!poalo5 = objLines.Poalo5_line
        mobjOrderLine!poalo6 = objLines.Poalo6_line
        mobjOrderLine!poalo7 = objLines.Poalo7_line
        mobjOrderLine!poalo8 = objLines.Poalo8_line
        mobjOrderLine!tot_poalo = objLines.Total_Poalo_line
        mobjOrderLine!npck1 = objLines.Pck1_line
        mobjOrderLine!npck2 = objLines.Pck2_line
        mobjOrderLine!npck3 = objLines.Pck3_line
        mobjOrderLine!npck4 = objLines.Pck4_line
        mobjOrderLine!npck5 = objLines.Pck5_line
        mobjOrderLine!npck6 = objLines.Pck6_line
        mobjOrderLine!npck7 = objLines.Pck7_line
        mobjOrderLine!npck8 = objLines.Pck8_line
        mobjOrderLine!npwght = objLines.PWght_line
        mobjOrderLine!note_mem = objLines.Note_Mem_line
        mobjOrderLine!comm1 = objLines.Comm1_line
        mobjOrderLine!comm2 = objLines.Comm2_line
        mobjOrderLine!po = objLines.PO_line
        mobjOrderLine!cost = objLines.Cost_line
        mobjOrderLine!GL_Sales = objLines.GL_Sales
        mobjOrderLine!GL_Cost = objLines.GL_Cost
        mobjOrderLine!pack_id = objLines.PackID_line
        mobjOrderLine!npolineno = objLines.POLine_No_line
        mobjOrderLine!cfromorder = objLines.FromOrder_line
        mobjOrderLine!bulklineno = objLines.BulkLine_No_line
        mobjOrderLine!clinestat = objLines.ClineStat_line
        mobjOrderLine!cadd_user = objLines.Add_User_line
        mobjOrderLine!cadd_time = objLines.Add_Time_line
        mobjOrderLine!dadd_date = objLines.Add_date_line
        mobjOrderLine!llok_stat = objLines.Lok_Stat_line
        mobjOrderLine!clok_user = objLines.Lok_User_line
        mobjOrderLine!dlok_date = objLines.Lok_Date_line
        mobjOrderLine!cowner = objLines.Owner_line
        mobjOrderLine!clok_time = objLines.Lok_Time_line
        
    End Select
  End If
End Sub

