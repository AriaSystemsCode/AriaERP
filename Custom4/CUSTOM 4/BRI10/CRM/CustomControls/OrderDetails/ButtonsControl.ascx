<%@ Control Language="C#" AutoEventWireup="true" CodeFile="ButtonsControl.ascx.cs" Inherits="ButtonsControl" %>
<%@ Register Src="~/CustomControls/OrderDetails/StylesControl.ascx" TagName="StylesControl" TagPrefix="uc7" %>
<%@ Register Src="~/CustomControls/OrderDetails/ProfilesControl.ascx" TagName="ProfilesControl" TagPrefix="uc9" %>
<%@ Register Src="~/CustomControls/OrderDetails/DescriptionControl.ascx" TagName="DescriptionControl" TagPrefix="uc8" %>

<table style="width:700px">
<tr>

<td style=" width: 283px; height: 35px;">
    <asp:Button ID="UpdateToTemplateBtn" runat="server" Text="Update To Template" style="left: 79px; position: relative" Enabled="False" OnClick="UpdateToTemplateBtn_Click" /></td>
   <td style="width: 226px; height: 35px;">
       <asp:Button ID="ViewTemplateBtn" runat="server" Text="View Template" style="left: 57px; position: relative" Enabled="False" OnClick="ViewTemplateBtn_Click" />
    </td>
    <td style="height: 35px; width: 147px;">
    <asp:Button ID="Button1" runat="server" Text="Configure" style="left: 36px; position: relative" OnClientClick = "LoadConfig()"/>
    </td>
</tr>
</table>