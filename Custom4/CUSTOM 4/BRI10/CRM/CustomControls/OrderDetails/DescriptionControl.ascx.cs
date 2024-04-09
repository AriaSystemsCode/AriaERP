using System;
using System.Data;
using System.Configuration;
using System.Collections;
using System.Web;
using System.Web.Security;
using System.Web.UI;
using System.Web.UI.WebControls;
using System.Web.UI.WebControls.WebParts;
using System.Web.UI.HtmlControls;
using System.Drawing;
using Aria.Data.BusinessObject;

public partial class DescriptionControl : System.Web.UI.UserControl
{
    private int ConvertToInt(string value)
    {
        try
        {
            return Int32.Parse(value);
        }
        catch (Exception e)
        {
            return 0;
        }
    }

    private double ConvertToDouble(string value)
    {
        try
        {
            return double.Parse(value);
        }
        catch (Exception e)
        {
            return 0;
        }
    }

    public void InitScalesTable(DataTable styleRecords)
    {
        GrossPrice.Text = styleRecords.Rows[0]["PriceA"].ToString();
        NetPrice.Text = styleRecords.Rows[0]["PriceA"].ToString();
        Disc.Text = "0";

        Aria.DataTypes.AriaDataObjectPointer pointer = new Aria.DataTypes.AriaDataObjectPointer();
        string scalecode = styleRecords.Rows[0]["ScaleCode"].ToString();
        pointer.AddKeyField("Scale",scalecode.Trim().PadRight(3));
        AriaBusinessObjectAdapter businessObject = new AriaBusinessObjectAdapter();
        DataTable scaleRecords = businessObject.GetBusinessObject("", "", "99", "CRM.Scale", pointer);

        int NumberOfSizes = ConvertToInt(scaleRecords.Rows[0]["Numberofsizes"].ToString());

        Label[] coloredLabel = new Label[8];
        coloredLabel[0] = OTS1;
        coloredLabel[1] = OTS2;
        coloredLabel[2] = OTS3;
        coloredLabel[3] = OTS4;
        coloredLabel[4] = OTS5;
        coloredLabel[5] = OTS6;
        coloredLabel[6] = OTS7;
        coloredLabel[7] = OTS8;

        Label[] scalesLabel = new Label[8];
        scalesLabel[0] = scale1;
        scalesLabel[1] = scale2;
        scalesLabel[2] = scale3;
        scalesLabel[3] = scale4;
        scalesLabel[4] = scale5;
        scalesLabel[5] = scale6;
        scalesLabel[6] = scale7;
        scalesLabel[7] = scale8;

        TextBox[] qtyTxtBoxes = new TextBox[8];
        qtyTxtBoxes[0] = OrderQty1;
        qtyTxtBoxes[1] = OrderQty2;
        qtyTxtBoxes[2] = OrderQty3;
        qtyTxtBoxes[3] = OrderQty4;
        qtyTxtBoxes[4] = OrderQty5;
        qtyTxtBoxes[5] = OrderQty6;
        qtyTxtBoxes[6] = OrderQty7;
        qtyTxtBoxes[7] = OrderQty8;


        for (int i = 0; i < NumberOfSizes; i++)
        {
            scalesLabel[i].Visible = true;
            coloredLabel[i].Visible = true;
            qtyTxtBoxes[i].Visible = true;
            qtyTxtBoxes[i].Enabled = true;
            qtyTxtBoxes[i].Text = "";


            string OTS_String = styleRecords.Rows[0]["OTS" + (i + 1)].ToString().Trim();
            string Size_String = scaleRecords.Rows[0]["Size" + (i + 1)].ToString().Trim();

            scalesLabel[i].Text = Size_String;

            int OTS_Int = ConvertToInt(OTS_String);
            if (OTS_Int <= 0)
            {
                coloredLabel[i].BackColor = Color.Red;
            }
            else
            {
                coloredLabel[i].BackColor = Color.LightGreen;
            }
            coloredLabel[i].Text = OTS_String;
        }

        for (int i = NumberOfSizes; i < 8; i++)
        {
            scalesLabel[i].Visible = false;
            coloredLabel[i].Visible = false;            
            qtyTxtBoxes[i].Visible = false;
            qtyTxtBoxes[i].Enabled = false;
        }
        
        StyleDescription.Text = styleRecords.Rows[0]["Description1"].ToString().Trim();

        Disc.Enabled = true;
        GrossPrice.Enabled = true;
        NetPrice.Enabled = true;
        Group.Enabled = true;
    }

    public void ClearScalesTable()
    {
        GrossPrice.Text = "0";
        NetPrice.Text = "0";
        Disc.Text = "0";

        Label[] coloredLabel = new Label[8];
        coloredLabel[0] = OTS1;
        coloredLabel[1] = OTS2;
        coloredLabel[2] = OTS3;
        coloredLabel[3] = OTS4;
        coloredLabel[4] = OTS5;
        coloredLabel[5] = OTS6;
        coloredLabel[6] = OTS7;
        coloredLabel[7] = OTS8;

        Label[] scalesLabel = new Label[8];
        scalesLabel[0] = scale1;
        scalesLabel[1] = scale2;
        scalesLabel[2] = scale3;
        scalesLabel[3] = scale4;
        scalesLabel[4] = scale5;
        scalesLabel[5] = scale6;
        scalesLabel[6] = scale7;
        scalesLabel[7] = scale8;

        TextBox[] qtyTxtBoxes = new TextBox[8];
        qtyTxtBoxes[0] = OrderQty1;
        qtyTxtBoxes[1] = OrderQty2;
        qtyTxtBoxes[2] = OrderQty3;
        qtyTxtBoxes[3] = OrderQty4;
        qtyTxtBoxes[4] = OrderQty5;
        qtyTxtBoxes[5] = OrderQty6;
        qtyTxtBoxes[6] = OrderQty7;
        qtyTxtBoxes[7] = OrderQty8;


        for (int i = 0; i < 8; i++)
        {
            scalesLabel[i].Visible = false;
            coloredLabel[i].Visible = false;
            qtyTxtBoxes[i].Visible = true;
            qtyTxtBoxes[i].Enabled = false;
            qtyTxtBoxes[i].Text= "";
        }

        StyleBtn.Enabled = true;
        StyleDescription.Enabled = true;

        StyleDescription.Text = "";

        Group.Text = "";

        GrossPrice.Enabled = false;
        NetPrice.Enabled = false;
        Disc.Enabled = false;
        Group.Enabled = false;

        Total.Text = "0";
    }

    public string GetStyleDescriptionValue()
    {
        return this.StyleDescription.Text;
    }

    public int[] GetAllQtyValues()
    {
        int[] qty = new int[8];
        qty[0] = ConvertToInt(OrderQty1.Text);
        qty[1] = ConvertToInt(OrderQty2.Text);
        qty[2] = ConvertToInt(OrderQty3.Text);
        qty[3] = ConvertToInt(OrderQty4.Text);
        qty[4] = ConvertToInt(OrderQty5.Text);
        qty[5] = ConvertToInt(OrderQty6.Text);
        qty[6] = ConvertToInt(OrderQty7.Text);
        qty[7] = ConvertToInt(OrderQty8.Text);
        return qty;
    }

    public double GetPriceValue()
    {
        return ConvertToDouble(GrossPrice.Text);
    }

    public double GetNetPriceValue()
    {
        return ConvertToDouble(NetPrice.Text);
    }

    public double GetDiscValue()
    {
        return ConvertToDouble(Disc.Text);
    }

    public string GetGroupValue()
    {
        return Group.Text;
    }


    public void FillDescriptionControls(string qty1, string qty2, string qty3, string qty4, string qty5, string qty6, string qty7, string qty8, double discountPercent, string group, double grossPrice, double netPrice, string description)
    {
        OrderQty1.Text = qty1;
        OrderQty2.Text = qty2;
        OrderQty3.Text = qty3;
        OrderQty4.Text = qty4;
        OrderQty5.Text = qty5;
        OrderQty6.Text = qty6;
        OrderQty7.Text = qty7;
        OrderQty8.Text = qty8;

        Total.Text = Convert.ToString((qty1.Trim().Length == 0 ? 0 : Convert.ToInt32(qty1)) +
                                      (qty2.Trim().Length == 0 ? 0 : Convert.ToInt32(qty2)) +
                                      (qty3.Trim().Length == 0 ? 0 : Convert.ToInt32(qty3)) +
                                      (qty4.Trim().Length == 0 ? 0 : Convert.ToInt32(qty4)) +
                                      (qty5.Trim().Length == 0 ? 0 : Convert.ToInt32(qty5)) +
                                      (qty6.Trim().Length == 0 ? 0 : Convert.ToInt32(qty6)) +
                                      (qty7.Trim().Length == 0 ? 0 : Convert.ToInt32(qty7)) +
                                      (qty8.Trim().Length == 0 ? 0 : Convert.ToInt32(qty8)));

        Disc.Text =discountPercent.ToString();
        Group.Text = group;

        GrossPrice.Text = grossPrice.ToString("f");
        NetPrice.Text = netPrice.ToString("f");

        StyleDescription.Text = description;
    }

    protected void StyleDescription_TextChanged(object sender, EventArgs e)
    {
        ScriptManager.RegisterClientScriptBlock(this.Page, typeof(string), "", "LoadingComplete();", true);

        if ((bool)Session["UseExtenedScale"])
        {
            return;
        }

        if (StyleDescription.Text.ToString().Trim() == "No matched records!")
        {
            StyleDescription.Text = "";
        }

        string[] strArray = StyleDescription.Text.Split('|');
        if (strArray.Length == 2)
        {
            Parent.FindControl("Styles1").GetType().GetMethod("AssignStyle").Invoke(Parent.FindControl("Styles1"), new object[] { strArray[0] });
        }

        ScriptManager.GetCurrent(this.Page).SetFocus(StyleDescription.ClientID);
    }

    protected void Page_Load(object sender, EventArgs e)
    {
        if ((bool)Session["UseExtenedScale"])
        {
            AutoCompleteExtender1.Enabled = false;
        }

        OrderQty1.Attributes.Add("onfocusout", "IsNumber(this, 6); jsCalculateTotal();");
        OrderQty1.Attributes.Add("onfocus", "SaveLastValue(this.value)");

        OrderQty2.Attributes.Add("onfocusout", "IsNumber(this, 6); jsCalculateTotal();");
        OrderQty2.Attributes.Add("onfocus", "SaveLastValue(this.value)");

        OrderQty3.Attributes.Add("onfocusout", "IsNumber(this, 6); jsCalculateTotal();");
        OrderQty3.Attributes.Add("onfocus", "SaveLastValue(this.value)");

        OrderQty4.Attributes.Add("onfocusout", "IsNumber(this, 6); jsCalculateTotal();");
        OrderQty4.Attributes.Add("onfocus", "SaveLastValue(this.value)");

        OrderQty5.Attributes.Add("onfocusout", "IsNumber(this, 6); jsCalculateTotal();");
        OrderQty5.Attributes.Add("onfocus", "SaveLastValue(this.value)");

        OrderQty6.Attributes.Add("onfocusout", "IsNumber(this, 6); jsCalculateTotal();");
        OrderQty6.Attributes.Add("onfocus", "SaveLastValue(this.value)");

        OrderQty7.Attributes.Add("onfocusout", "IsNumber(this, 6); jsCalculateTotal();");
        OrderQty7.Attributes.Add("onfocus", "SaveLastValue(this.value)");

        OrderQty8.Attributes.Add("onfocusout", "IsNumber(this, 6); jsCalculateTotal();");
        OrderQty8.Attributes.Add("onfocus", "SaveLastValue(this.value)");

        OrderQty8.Attributes.Add("onfocusout", "IsNumber(this, 6); jsCalculateTotal();");
        OrderQty8.Attributes.Add("onfocus", "SaveLastValue(this.value)");

        GrossPrice.Attributes.Add("onfocusout", "IsCurrency(this); document.getElementById('" + NetPrice.ClientID + "').value = document.getElementById('" + GrossPrice.ClientID + "').value * (1- document.getElementById('" + Disc.ClientID + "').value / 100.00); document.getElementById('" + NetPrice.ClientID + "').value = Math.round(100 * document.getElementById('" + NetPrice.ClientID + "').value) / 100");
        GrossPrice.Attributes.Add("onfocus", "SaveLastValue(this.value)");

        Disc.Attributes.Add("onfocusout", "IsPercent(this); document.getElementById('" + NetPrice.ClientID + "').value = document.getElementById('" + GrossPrice.ClientID + "').value * (1- document.getElementById('" + Disc.ClientID + "').value / 100.00); document.getElementById('" + NetPrice.ClientID + "').value = Math.round(100 * document.getElementById('" + NetPrice.ClientID + "').value) / 100");
        Disc.Attributes.Add("onfocus", "SaveLastValue(this.value);");

        NetPrice.Attributes.Add("onfocusout", "IsCurrency(this); if((document.getElementById('" + GrossPrice.ClientID + "').value - document.getElementById('" + NetPrice.ClientID + "').value) <= 0) { document.getElementById('" + GrossPrice.ClientID + "').value = document.getElementById('" + NetPrice.ClientID + "').value; document.getElementById('" + Disc.ClientID + "').value  = ''; return;}; document.getElementById('" + Disc.ClientID + "').value = (document.getElementById('" + GrossPrice.ClientID + "').value - document.getElementById('" + NetPrice.ClientID + "').value) / document.getElementById('" + GrossPrice.ClientID + "').value * 100.00; document.getElementById('" + NetPrice.ClientID + "').value = Math.round(100 * document.getElementById('" + NetPrice.ClientID + "').value) / 100; document.getElementById('" + Disc.ClientID + "').value = Math.round(100 * document.getElementById('" + Disc.ClientID + "').value) / 100");
        NetPrice.Attributes.Add("onfocus", "SaveLastValue(this.value)");

        StyleDescription.Attributes.Add("onchange", "Loading(' Loading...');");

    }
    protected void StyleBtn_Click(object sender, EventArgs e)
    {
        StyleDescription.Text = "*";
        ScriptManager.GetCurrent(this.Page).SetFocus(StyleDescription.ClientID);
    }
}
