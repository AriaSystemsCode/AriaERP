<%@ Control Language="C#" AutoEventWireup="true" CodeFile="NoteControl.ascx.cs" Inherits="NoteControl" %>
<%@ Register Assembly="AjaxControlToolkit" Namespace="AjaxControlToolkit" TagPrefix="cc1" %>
<asp:Panel ID="NotePanel" runat="server" Height="65px" Width="780px" style="position: relative; top: 5px">
    <asp:TextBox ID="Notes1" runat="server" Width="758px" style="position: relative; left: 7px; top: 10px;"></asp:TextBox>
    <asp:TextBox ID="Notes2" runat="server" Width="758px" style="position: relative; left: 7px; top: 14px;"></asp:TextBox></asp:Panel>
<cc1:RoundedCornersExtender ID="RoundedCornersExtender1" runat="server" BorderColor="LightGray"
    TargetControlID="NotePanel">
</cc1:RoundedCornersExtender>
<asp:Panel ID="Panel1" runat="server" Height="4px" Style="left: 10px; position: relative;
    top: -80px; background-color: white" Width="43px">
    <asp:Label ID="Label1" runat="server" Text="Notes" Width="100%" BackColor="white"></asp:Label>
    </asp:Panel>
