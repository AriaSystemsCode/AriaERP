<%@ Control Language="C#" AutoEventWireup="true" CodeFile="AddressOrderControl.ascx.cs"
    Inherits="AddressOrderControl" %>
<%@ Register Assembly="AjaxControlToolkit" Namespace="AjaxControlToolkit" TagPrefix="cc1" %>

<!-- panel  -->
<asp:Panel ID="OrderAddressPanel" runat="server" Height="150px" Style="z-index: 0;
    position: relative; top: 15px; left: 0px;" Width="461" BackColor="White">
    <asp:CheckBox ID="MultiShipTo1" runat="server" Style="left: 3px; position: relative;
        top: 14px" Text="Multi Ship To" Enabled="False" TabIndex="1" />
    <asp:CheckBox ID="MultiPO1" runat="server" Style="left: 10px; position: relative;
        top: 14px" Text="Multi Customer PO" Enabled="False" TabIndex="2" AutoPostBack="True" OnCheckedChanged="MultiPO1_CheckedChanged" />
    <asp:Label ID="Label2" runat="server" Text="Store:" Style="text-align: center; left: 27px;
        position: relative; top: 12px" Height="20px" Font-Underline="True" Enabled="False" TabIndex="3"></asp:Label>
    <asp:TextBox ID="StoreID1" runat="server" Style="left: 29px; position: relative;
        top: 12px" Width="106px" Enabled="False" TabIndex="4"></asp:TextBox>
    <table style="width: 460px; position: relative; top: 28px; height: 95px; left: 0px;">
        <tr>
            <td style="width: 220px; height: 82px; border: double 2px white;">
                <asp:Panel ID="ShipToAddressPanel" runat="server" Style="position: relative; height: 82px;
                    width: 220px">
                    <asp:TextBox ID="Address12" runat="server" Width="6px" Style="left: 5px; width: 93%;
                        position: relative; top: 10px" Enabled="False" TabIndex="6"></asp:TextBox>
                    <asp:TextBox ID="Address22" runat="server" Style="width: 93%; left: 5px; position: relative;
                        top: 12px;" Enabled="False" TabIndex="7"></asp:TextBox>
                    <asp:TextBox ID="Address32" runat="server" Style="width: 93%; left: 5px; position: relative;
                        top: 13px;" Enabled="False" TabIndex="8"></asp:TextBox></asp:Panel>
            </td>
            <td style="width: 220px; height: 82px; border: double 2px white;">
                <asp:Panel ID="BillToAddressPanel" runat="server" Style="position: relative; height: 82px;
                    width: 220px; left: 0px; top: 0px;">
                    <asp:TextBox ID="ShiptoAddress1" runat="server" Enabled="false" Width="6px" Style="left: 5px; width: 93%;
                        position: relative; top: 10px" TabIndex="10"></asp:TextBox>
                    <asp:TextBox ID="ShiptoAddress2" runat="server" Enabled="false" Style="width: 93%; left: 5px; position: relative;
                        top: 12px;" TabIndex="11"></asp:TextBox>
                    <asp:TextBox ID="ShiptoAddress3" runat="server" Enabled="false" Style="width: 93%; left: 5px; position: relative;
                        top: 13px;" TabIndex="12"></asp:TextBox></asp:Panel>
            </td>
        </tr>
    </table>
</asp:Panel>
&nbsp;&nbsp;<br />
<cc1:RoundedCornersExtender ID="RoundedCornersExtender1" runat="server" TargetControlID="OrderAddressPanel"
    BorderColor="LightGray">
</cc1:RoundedCornersExtender>
<cc1:RoundedCornersExtender ID="RoundedCornersExtender2" runat="server" TargetControlID="ShipToAddressPanel"
    BorderColor="LightGray">
</cc1:RoundedCornersExtender>
<cc1:RoundedCornersExtender ID="RoundedCornersExtender3" runat="server" BorderColor="LightGray"
    TargetControlID="BillToAddressPanel">
</cc1:RoundedCornersExtender>
<asp:Panel ID="Panel1" runat="server" Height="11px" Style="left: 6px; position: relative;
    top: -174px; background-color: white" Width="147px">
    Order Address Control</asp:Panel>
<asp:DropDownList ID="ShipToDropDownList" runat="server" Style="left: 258px; position: absolute;
    top: 122px" Width="210px" AutoPostBack="true" OnSelectedIndexChanged="ShipToDropDownList_SelectedIndexChanged" TabIndex="9">
    <asp:ListItem Text="Ship To Based on Master Setup" Selected="True" Value="0" ></asp:ListItem>
    <asp:ListItem Text="Alternative Address" Value="1" ></asp:ListItem>
</asp:DropDownList>
<asp:Label ID="Label1" runat="server" Text="Bill To Address" Style="left: 36px; position: absolute;
    top: 123px" Width="100px" BackColor="White" TabIndex="5"></asp:Label>