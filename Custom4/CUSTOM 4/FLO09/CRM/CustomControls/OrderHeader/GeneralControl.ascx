<%@ Control Language="C#" AutoEventWireup="true" CodeFile="GeneralControl.ascx.cs"
    Inherits="General" %>
<%@ Register Assembly="AjaxControlToolkit" Namespace="AjaxControlToolkit" TagPrefix="cc1" %>
<asp:Panel ID="GeneralPanel" runat="server" Height="70px" Width="780px" Style="left: 0px; top: 0px;">
    <table style=" top:13px; z-index:1">
        <tr>
            <td style="width:6px"></td>
            <td>
                <asp:Label ID="Label1" runat="server" Text="Customer PO#:" Width="96px"></asp:Label>
            </td>
            <td>
                <asp:TextBox ID="CustomerPO1" runat="server" Width="77px" AutoPostBack="True" OnTextChanged="CustomerPO1_TextChanged" MaxLength="15"></asp:TextBox>
            </td>
            <td style="width:5px"></td>
            <td >
                <asp:Label ID="Label2" runat="server" Text="Order Date:"></asp:Label>
            </td>
            <td style="z-index: 0">
                <asp:TextBox ID="EnteredDate" runat="server" Width="65px" style="z-index: 0"  ></asp:TextBox>
            </td>
            <td>
                <asp:ImageButton ID="ImageButton1" runat="server" ImageUrl="~/resources/images/Calendar_scheduleHS.png"/>
            </td>
            <td style="width:5px"></td>
            <td >
                <asp:Label ID="Label3" runat="server" Text="Start Ship Date:"></asp:Label>
            </td>
            <td>
                <asp:TextBox ID="StartDate" runat="server" Width="65px" AutoPostBack="True" OnTextChanged="StartDate_TextChanged"></asp:TextBox>
            </td>
            <td style="width: 19px">                
                <asp:ImageButton ID="ImageButton2" runat="server" ImageUrl="~/resources/images/Calendar_scheduleHS.png"/>
            </td>
            <td style="width:5px"></td>
            <td>
                <asp:Label ID="Label4" runat="server" Text="Cancel Date:"></asp:Label>
            </td>
            <td>
                <asp:TextBox ID="CompleteDate" runat="server" Width="65px"></asp:TextBox>
            </td>
            <td>
                <asp:ImageButton ID="ImageButton3" runat="server" ImageUrl="~/resources/images/Calendar_scheduleHS.png"/>
            </td>
        </tr>
        </table>
        <table style=" top:13px; z-index:0; left: 0px; height: 23px;">
        <tr>
            <td style="width:6px; height: 24px;"></td>
            <td style="height: 24px">
                <asp:Label ID="Label5" runat="server" Text="Season:"></asp:Label>
            </td>
            <td style="height: 24px">
                <asp:DropDownList ID="SeasonCode" runat="server" Width="263px">
                </asp:DropDownList>
            </td>
            <td style="width:120px; height: 24px;">
            </td>
            <td style="height: 24px">
                <asp:Label ID="Label6" runat="server" Text="Division:" Width="49px"></asp:Label>
            </td>
            <td style="height: 24px" >
                <asp:DropDownList ID="Division" runat="server" Width="263px">
                </asp:DropDownList>
            </td>
        </tr>
    </table>
</asp:Panel>
<cc1:RoundedCornersExtender ID="RoundedCornersExtender1" runat="server" BorderColor="LightGray"
    TargetControlID="GeneralPanel">
</cc1:RoundedCornersExtender>
<cc1:CalendarExtender ID="CalendarExtender1" runat="server" Format="M/d/yyyy" TargetControlID="EnteredDate" PopupButtonID="ImageButton1" PopupPosition = "TopRight">
</cc1:CalendarExtender>
<cc1:CalendarExtender ID="CalendarExtender2" runat="server" Format="M/d/yyyy" TargetControlID="StartDate" PopupButtonID="ImageButton2"  PopupPosition = "TopRight">
</cc1:CalendarExtender>
<cc1:CalendarExtender ID="CalendarExtender3" runat="server" Format="M/d/yyyy" TargetControlID="CompleteDate" PopupButtonID="ImageButton3"  PopupPosition = "TopRight">
</cc1:CalendarExtender>
<asp:Panel ID="Panel1" runat="server" Height="3px" Style="left: 35px; position:absolute; 
    top: 305px; background-color: white" Width="52px">
    <asp:Label ID="Label7" runat="server" Text="General" Width="100%" BackColor="white" style="left: 4px;"></asp:Label>
    </asp:Panel>
<asp:Timer ID="Timer1" runat="server" Interval="1" OnTick="Timer1_Tick">
</asp:Timer>
    
