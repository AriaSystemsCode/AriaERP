<%@ Page Language="C#" AutoEventWireup="true" CodeFile="Configure.aspx.cs" Inherits="WebPages_Configure" EnableEventValidation = "false"  %>

<%@ Register Assembly="AjaxControlToolkit" Namespace="AjaxControlToolkit" TagPrefix="cc1" %>


<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
<head runat="server">
    <base target="_self" />
    <title>Order Line Configuration</title>
    
    <script language = "jscript" type = "text/jscript">
    var lastValue = "";

    function SaveLastValue(value)
    {
      lastValue = value;
    }

    function IsNumber(fld, length)
    {
      try
      {
      var RegExPattern = /^[0-9]+$/;

      if ((fld.value.match(RegExPattern) && fld.value.length <= length) || fld.value == '')
      {
      }
      else
      {
        alert("Invalid number!");
        fld.value = lastValue;
        fld.focus();
        fld.select();
        }
      }
      catch(e)
      {
      }
    }

    function IsDouble(fld)
    {
      try
      {
      var RegExPattern = /^\d{1,9}(\.\d{1,2}){0,1}$/;

      if (fld.value.match(RegExPattern) || fld.value == '')
      {
      }
      else
      {
        alert("Invalid number!");
        fld.value = lastValue;
        fld.focus();
        fld.select();
        }
      }
      catch(e)
      {
      }
    }
    </script>
    
</head>
<body>
    <form id="form1" runat="server">
            <asp:ScriptManager ID="ScriptManager1" runat="server">
                <Services>
                    <asp:ServiceReference Path="~/WebServices/AutoComplete/OrderDetails/Configuration/DesignCodeAutoComplete.asmx" />
                </Services>
            </asp:ScriptManager>
        <div align="center">
            <asp:UpdatePanel ID="UpdatePanel1" runat="server" >
                <ContentTemplate>
                    <table>
                        <tr>
                            <td style="width: 784px">
                                <asp:Panel ID="Panel1" runat="server" Height="220px" Width="800px" ScrollBars = Auto>
                                    <asp:GridView ID="GridView1" runat="server" AutoGenerateColumns="False" CellPadding="0" ForeColor="#333333" GridLines="None" Width="754px" Height="107px" OnRowCommand="GridView1_RowCommand" OnSelectedIndexChanged="GridView1_SelectedIndexChanged">
                                        <Columns>
                                          <asp:TemplateField HeaderText="Edit">
                                            <ItemTemplate>
                                              <asp:LinkButton ID="btnEdit" runat="server">Edit</asp:LinkButton>
                                            </ItemTemplate>
                                          </asp:TemplateField>
                                          <asp:TemplateField HeaderText="Delete">
                                            <ItemTemplate>
                                              <asp:LinkButton ID="btnDelete" runat="server">Delete</asp:LinkButton>
                                            </ItemTemplate>
                                          </asp:TemplateField>
                                          <asp:BoundField HeaderText="Design Code" DataField = "ArtworkDesignCode"/>
                                          <asp:BoundField HeaderText="Description" DataField = "DesignName"/>
                                          <asp:BoundField HeaderText="Placement" DataField = "StylePositionDescription"/>
                                          <asp:BoundField HeaderText="Design Category" DataField = "ArtworDesignCategoryDescription"/>
                                          <asp:BoundField HeaderText="Design Type" DataField = "ArtworkDesignTypeDescription"/>
                                          <asp:BoundField HeaderText="# of imprints" DataField = "NumberofImprints"/>
                                          <asp:BoundField HeaderText="Price" DataField = "Price"/>
                                        </Columns>
                                      <FooterStyle BackColor="#507CD1" Font-Bold="True" ForeColor="White" />
                                      <RowStyle BackColor="#EFF3FB" />
                                      <EditRowStyle BackColor="#2461BF" />
                                      <SelectedRowStyle BackColor="#D1DDF1" Font-Bold="True" ForeColor="#333333" />
                                      <PagerStyle BackColor="#2461BF" ForeColor="White" HorizontalAlign="Center" />
                                      <HeaderStyle BackColor="#507CD1" Font-Bold="True" ForeColor="White" />
                                      <AlternatingRowStyle BackColor="White" />
                                    </asp:GridView>
                                </asp:Panel>
                            </td>
                        </tr>
                        <tr>
                            <td style="height: 353px; width: 784px; text-align: center;">
                              <table>
                                    <tr>
                                        <td>
                                            <asp:Label ID="Label1" runat="server" Text="Design Code:" Width="90px"></asp:Label></td>
                                        <td style="width: 399px">
                                            <asp:TextBox ID="txtDesignCode" runat="server" Width="385px" AutoPostBack="True" OnTextChanged="txtDesignCode_TextChanged"></asp:TextBox></td>
                                    </tr>
                                    <tr>
                                        <td>
                                            <asp:Label ID="Label2" runat="server" Text="Description:" Width="90px"></asp:Label></td>
                                        <td style="width: 399px">
                                            <asp:TextBox ID="txtDescription" runat="server" Width="385px" Enabled="False"></asp:TextBox></td>
                                    </tr>
                                    <tr>
                                        <td colspan="2" style="height: 0px">
                                            <table>
                                                <tr>
                                                    <td>
                                                        <asp:Label ID="Label7" runat="server" Text="Name-Drop:" Width="90px"></asp:Label>
                                                    </td>
                                                    <td>
                                                        <asp:TextBox ID="txtNameDrop" runat="server" Enabled="False"></asp:TextBox>
                                                    </td>
                                                    <td>
                                                    </td>
                                                    <td>
                                                        <asp:Label ID="Label8" runat="server" Text="# of imprints:"></asp:Label>
                                                    </td>
                                                    <td>
                                                        <asp:TextBox ID="txtNoOfImprints" runat="server" Width="136px" MaxLength="6">0</asp:TextBox></td>
                                                </tr>
                                            </table>
                                        </td>
                                    </tr>
                                    <tr>
                                        <td>
                                            <asp:Label ID="Label3" runat="server" Text="Text Line1:" Width="90px"></asp:Label></td>
                                        <td style="width: 399px">
                                            <asp:TextBox ID="txtLine1" runat="server" Width="385px" Enabled="False"></asp:TextBox></td>
                                    </tr>
                                    <tr>
                                        <td>
                                            <asp:Label ID="Label4" runat="server" Text="Text Line2:" Width="90px"></asp:Label></td>
                                        <td style="width: 399px">
                                            <asp:TextBox ID="txtLine2" runat="server" Width="385px" Enabled="False"></asp:TextBox></td>
                                    </tr>
                                    <tr>
                                        <td>
                                            <asp:Label ID="Label5" runat="server" Text="Text Line3:" Width="90px"></asp:Label></td>
                                        <td style="width: 399px">
                                            <asp:TextBox ID="txtLine3" runat="server" Width="385px" Enabled="False"></asp:TextBox></td>
                                    </tr>
                                    <tr>
                                        <td style="height: 26px">
                                            <asp:Label ID="Label6" runat="server" Text="Text Line4:" Width="90px"></asp:Label></td>
                                        <td style="height: 26px; width: 399px;">
                                            <asp:TextBox ID="txtLine4" runat="server" Width="385px" Enabled="False"></asp:TextBox></td>
                                    </tr>
                                    <tr>
                                        <td colspan="2" style="height: 0px">
                                            <table>
                                                <tr>
                                                    <td>
                                                        <asp:Label ID="Label9" runat="server" Text="Design Type:" Width="90px"></asp:Label>
                                                    </td>
                                                    <td>
                                                        <asp:DropDownList ID="ddlDesignType" runat="server" Width="155px" Enabled="False">
                                                        </asp:DropDownList></td>
                                                    <td>
                                                    </td>
                                                    <td>
                                                        <asp:Label ID="Label10" runat="server" Text="Category:"></asp:Label>
                                                    </td>
                                                    <td>
                                                        <asp:DropDownList ID="ddlCategory" runat="server" Width="161px" Enabled="False">
                                                        </asp:DropDownList></td>
                                                </tr>
                                            </table>
                                        </td>
                                    </tr>
                                    <tr>
                                        <td colspan="2" style="height: 0px">
                                            <table>
                                                <tr>
                                                    <td>
                                                        <asp:Label ID="Label11" runat="server" Text="Placement:" Width="90px"></asp:Label>
                                                    </td>
                                                    <td style="width: 154px">
                                                        <asp:DropDownList ID="ddlPlacement" runat="server" Width="150px" Enabled="False">
                                                        </asp:DropDownList></td>
                                                    <td>
                                                    </td>
                                                    <td>
                                                        <asp:Label ID="Label12" runat="server" Text="Price:" Width="65px"></asp:Label>
                                                    </td>
                                                    <td>
                                                        <asp:TextBox ID="txtPrice" runat="server" MaxLength="12">0.00</asp:TextBox></td>
                                                </tr>
                                            </table>
                                        </td>
                                    </tr>
                                </table>
                                <asp:Button ID="btnUpdate" runat="server" Text="Add" OnClick="btnUpdate_Click" Width="98px" />
                              &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;
                                <asp:Button ID="btnFinish" runat="server" Text="Close" Width="110px" OnClientClick = "window.close();"  />&nbsp;
                              <asp:Timer ID="Timer1" runat="server" Interval="1" OnTick="Timer1_Tick">
                              </asp:Timer>
                              </td>
                        </tr>
                    </table>
                    <cc1:AutoCompleteExtender ID="AutoCompleteExtenderDesignCode" runat="server" EnableCaching="false" MinimumPrefixLength="1" UseContextKey="True" TargetControlID="txtDesignCode" ServiceMethod="GetCompletionList" ServicePath="~/WebServices/AutoComplete/OrderDetails/Configuration/DesignCodeAutoComplete.asmx">
                    </cc1:AutoCompleteExtender>
                </ContentTemplate>
            </asp:UpdatePanel>
          &nbsp; &nbsp;
        </div>
    </form>
</body>
</html>
