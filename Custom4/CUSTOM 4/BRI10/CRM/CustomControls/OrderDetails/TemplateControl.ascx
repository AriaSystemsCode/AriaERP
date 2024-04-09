<%@ Control Language="C#" AutoEventWireup="true" CodeFile="TemplateControl.ascx.cs"
    Inherits="OrderDetails_TemplateControl" %>
<%@ Register Assembly="AjaxControlToolkit" Namespace="AjaxControlToolkit" TagPrefix="cc1" %>

<table>
    <tr>
        <td style="width: 776px; height: 350px;">
            <asp:Panel ID="Panel4" runat="server" ScrollBars="vertical" Height="350px" Width="776px">
                <asp:GridView ID="TemplateLinesGrid" runat="server" Width="756px" AutoGenerateColumns="False"
                    OnRowCommand="TemplateLinesGrid_RowCommand" RowHeaderColumn="Amount" CellPadding="4"
                    ForeColor="#333333" GridLines="None">
                    <Columns>
                        <asp:TemplateField>
                            <ItemTemplate>
                                <asp:CheckBox ID="TempLineSelectorChkBx" runat="server" Enabled="false" />
                            </ItemTemplate>
                        </asp:TemplateField>
                        <asp:TemplateField>
                            <ItemTemplate>
                                <asp:CheckBox ID="TemplateLineSelectorChkBx" runat="server" />
                            </ItemTemplate>
                        </asp:TemplateField>
                        <asp:ButtonField DataTextField="Style" HeaderText="Style - Color" ShowHeader="True" />
                        <asp:BoundField DataField="Description1" HeaderText="Description" />
                        <asp:BoundField DataField="ColorDescription" HeaderText="Color" />
                        <asp:BoundField DataField="Price" HeaderText="Price" DataFormatString="{0:c2}" HtmlEncode="False" />
                        <asp:BoundField DataField="TotalBookedquantity" HeaderText="Qty"/>
                        <asp:BoundField DataField="StartDate" HeaderText="Ship Date" DataFormatString="{0:MM/dd/yyyy}"
                            HtmlEncode="False" />
                        <asp:BoundField DataField="Amount" HeaderText="Amount" DataFormatString="{0:c2}"
                            HtmlEncode="False" FooterText="999" />
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
                </asp:GridView>
                &nbsp;
            </asp:Panel>
        </td>
    </tr>
    <tr>
        <td style="height: 53px; width: 876px;">
            <table cellspacing="0" width="100%" cellpadding="4">
                <tr>
                    <td style="width: 141px">
                      &nbsp; &nbsp;<asp:Button ID="SelectAllBtn" runat="server" Text="Select All" OnClick="SelectAllBtn_Click" />
                    </td>
                    <td style="width: 174px">
                      &nbsp; &nbsp;
                      <asp:Button ID="SelectNoneBtn" runat="server" Text="Select None" OnClick="SelectNoneBtn_Click" />
                    </td>
                    <td style="width: 213px">
                      &nbsp;
                        <asp:Button ID="AssignToStoresBtn" runat="server" Text="Assign to Stores" OnClick="AssignToStoresBtn_Click1"/></td>
                    <td style="width: 136px">
                      &nbsp;
                        <asp:Button ID="RemoveBtn" runat="server" Text="Remove" OnClick="RemoveBtn_Click" />
                    </td>
                    <td>
                      &nbsp; &nbsp;<asp:Button ID="BackBtn" runat="server" Text="Back" Width="60px" OnClick="BackBtn_Click" />
                    </td>
                </tr>
            </table>
        </td>
    </tr>
</table>
<asp:HiddenField ID="HiddenField1" runat="server" />
<asp:HiddenField ID="HiddenField2" runat="server" />
