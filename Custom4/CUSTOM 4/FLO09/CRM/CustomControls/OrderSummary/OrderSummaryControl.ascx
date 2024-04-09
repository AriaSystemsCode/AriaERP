<%@ Register TagPrefix="cc1" Namespace="BunnyBear" Assembly="msgBox" %>
<%@ Control Language="C#" AutoEventWireup="true" CodeFile="OrderSummaryControl.ascx.cs"
    Inherits="OrderSummaryControl" %>
<%@ Register Src="~/CustomControls/OrderDetails/StylesControl.ascx" TagName="StyleControl" TagPrefix="uc112" %>
<%@ Register Assembly="AjaxControlToolkit" Namespace="AjaxControlToolkit" TagPrefix="cc1" %>


<table>
    <tr>
        <td style="width:776px; height:350px">
            <asp:Panel ID="Panel1" runat="server" Height="350px" Width="776px" ScrollBars = "Horizontal" >
            
           <asp:GridView ID="LinesGrid" runat="server" Width="774px" AutoGenerateColumns = "False" OnRowCommand="LinesGrid_RowCommand" RowHeaderColumn="Amount" ShowFooter="True" CellPadding="4" ForeColor="#333333" GridLines="Horizontal" style="font-size: 0.9em" AllowPaging="True" PageSize="8" OnPageIndexChanging="LinesGrid_PageIndexChanging" OnSelectedIndexChanged="LinesGrid_SelectedIndexChanged">
                <Columns>
                   <asp:TemplateField>
                        <ItemTemplate>
                            <asp:CheckBox ID="TempLineSelectorChkBx" runat="server" Enabled = "false" />
                        </ItemTemplate>
                   </asp:TemplateField>
                   <asp:TemplateField>
                        <ItemTemplate>
                            <asp:CheckBox ID="LineSelectorChkBx" runat="server" />
                        </ItemTemplate>
                   </asp:TemplateField>
                    <asp:ButtonField DataTextField = "Style" HeaderText = "Style - Color" ShowHeader="True"/>
                    <asp:BoundField DataField = "Description1" HeaderText = "Description" />
                    <asp:BoundField DataField = "ColorDescription" HeaderText = "Color" />
                    <asp:BoundField DataField = "StoreNumber" HeaderText = "Store"/>
                    <asp:BoundField DataField = "TotalBookedquantity" HeaderText = "Booked" />
                    <asp:BoundField DataField = "TotalQuantity" HeaderText = "Open" />
                    <asp:BoundField HeaderText = "ship"  Visible="False" />
                    <asp:BoundField DataField = "Price"  HeaderText = "Price" DataFormatString = "{0:c2}" HtmlEncode = "False" />
                    <asp:BoundField HeaderText = "Pcs/Cs" DataField = "PiecesPerCase" />
                    <asp:BoundField HeaderText = "#Cases" DataField = "NumberOfCases" />
                    <asp:BoundField DataField = "StartDate"  HeaderText = "Ship Date" DataFormatString = "{0:MM/dd/yyyy}"  HtmlEncode = "False"/>
                    <asp:BoundField DataField = "Amount"  HeaderText = "Amount" DataFormatString = "{0:c2}" HtmlEncode = "False" FooterText="999"/>
                </Columns>
               <FooterStyle BackColor="#507CD1" Font-Bold="True" ForeColor="White" />
               <RowStyle BackColor="#EFF3FB" />
               <EmptyDataTemplate>
                   No Data
               </EmptyDataTemplate>
               <EditRowStyle BackColor="#2461BF" />
               <SelectedRowStyle BackColor="#D1DDF1" Font-Bold="True" ForeColor="#333333" />
               <PagerStyle BackColor="#2461BF" ForeColor="White" HorizontalAlign="Center" />
               <HeaderStyle BackColor="#507CD1" Font-Bold="True" ForeColor="White" />
               <AlternatingRowStyle BackColor="White" />
                </asp:GridView>                &nbsp;
                </asp:Panel>
        </td>
        
    </tr>
    <tr>
        <td style="height: 40px;">
            <table cellspacing="0" width="100%" cellpadding="4">
                <tr>
                    <td style="background-color: #5578d4; height: 32px;" align="left">
                        <asp:Button ID="RemoveBtn" runat="server" Text="Remove Selected Line(s)" OnClick="RemoveBtn_Click" />
                      <asp:Button ID="Button1" runat="server" Text="Button" /></td>
                    <td style="background-color: #5578d4; height: 32px;" align="right">
                        <asp:Button ID="SaveBtn" runat="server" Text="Save Order" OnClick="SaveBtn_Click"/>
                    </td>
                </tr>
            </table>
        </td>
    </tr>
</table>
&nbsp;&nbsp;

