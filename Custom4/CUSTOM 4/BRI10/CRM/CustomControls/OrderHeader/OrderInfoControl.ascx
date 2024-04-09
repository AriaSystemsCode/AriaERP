<%@ Control Language="C#" AutoEventWireup="true" CodeFile="OrderInfoControl.ascx.cs"
    Inherits="OrderInfoControl" %>
<%@ Register Assembly="AjaxControlToolkit" Namespace="AjaxControlToolkit" TagPrefix="cc1" %>
<asp:Panel ID="Panel1" runat="server" style="position:relative; top:5px">
    <table >
        <tr>
            <td colspan="3">
                <table>
                    <tr>
                        <td>
                            <asp:CheckBox ID="SwapOrder" runat="server" Text="Swap Order" /></td>
                        <td>
                            <asp:CheckBox ID="FixtureInclude" runat="server" Text="Fixture Include" /></td>
                    </tr>
                    <tr>
                        <td>
                            <asp:CheckBox ID="DiscountedPrice" runat="server" Text="Discounted Price" /></td>
                        <td>
                            <asp:CheckBox ID="GuaranteedSales" runat="server" Text="Guaranteed Sales" /></td>
                    </tr>
                </table>
            </td>
        </tr>
        <tr>
            <td style="width: 126px; height: 41px">
                <asp:Label ID="Label1" runat="server" Text="Order From"></asp:Label></td>
            <td style="width: 83px; height: 41px">
                <asp:DropDownList ID="OrderCategory" runat="server">
                </asp:DropDownList></td>
            <td style="width: 127px; height: 41px">
                <asp:CheckBox ID="SampleOrder" runat="server" Text="Sample Order" /></td>
        </tr>
        <tr>
            <td style="width: 126px">
                <asp:Label ID="Label2" runat="server" Text="Ship Via"></asp:Label></td>
            <td colspan="2">
                <asp:DropDownList ID="ShipVia" runat="server" Width="167px">
                </asp:DropDownList></td>
        </tr>
        <tr>
            <td style="width: 126px; height: 9px">
                <asp:Label ID="Label3" runat="server" Text="Frt. Term"></asp:Label></td>
            <td colspan="2" style="height: 9px">
                <asp:DropDownList ID="SpecialInstructions" runat="server" Width="168px">
                </asp:DropDownList></td>
        </tr>
    </table>
</asp:Panel>
<cc1:RoundedCornersExtender ID="RoundedCornersExtender1" runat="server" BorderColor="LightGray"
    TargetControlID="Panel1">
</cc1:RoundedCornersExtender>
<asp:Panel ID="Panel2" runat="server" Height="3px" Style="left: 9px; position: relative;
    top: -170px; background-color: white" Width="67px">
    <asp:Label ID="Label4" runat="server" Width="100%" Text="Order Info" BackColor="white"></asp:Label>
    </asp:Panel>
