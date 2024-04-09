using System;
using System.Data;
using System.Configuration;
using System.Web;
using System.Web.Security;
using System.Web.UI;
using System.Web.UI.WebControls;
using System.Web.UI.WebControls.WebParts;
using System.Web.UI.HtmlControls;

/// <summary>
/// Summary description for StoreRow
/// </summary>
public class StoreRow
{
    private TableRow row;
    public TextBox TxtStoreId;
    public TextBox TxtPO;
    public TextBox TxtQty;
    public HtmlInputCheckBox checkBox;

	public StoreRow()
	{
        row = new TableRow();
        checkBox = new HtmlInputCheckBox();
        TableCell Check_cell = new TableCell();
        Check_cell.Controls.Add(checkBox);
        

        TxtStoreId = new TextBox();        
        TxtStoreId.ReadOnly = true;        
        TxtStoreId.Width = 150;
        TableCell Store_cell = new TableCell();
        Store_cell.Controls.Add(TxtStoreId);
        TxtPO = new TextBox();

        if (((CheckBox)HttpContext.Current.Session["MultiPOHeader"]).Checked)
        {
            TxtPO.Enabled = true;
        }
        else
        {
            TxtPO.Enabled = false;
            TxtPO.Text = ((TextBox)HttpContext.Current.Session["CustomerPO1"]).Text;
        }
        
        TxtPO.Width = 150;
        TxtPO.MaxLength = 15;
        TableCell PO_cell = new TableCell();
        PO_cell.Controls.Add(TxtPO);

        TxtQty = new TextBox();
        //TxtQty.ReadOnly = true;
        TxtQty.Width = 80;
        TxtQty.MaxLength = 3;
        //TxtQty.Text = "1";
        TableCell Qty_cell = new TableCell();
        Qty_cell.Controls.Add(TxtQty);

        
        row.Cells.Add(Check_cell);
        row.Cells.Add(Store_cell);
        row.Cells.Add(PO_cell);
        row.Cells.Add(Qty_cell);
	}

       
    public void SetStoreValue(string value)
    {
        this.TxtStoreId.Text = value;
    }

    public void SetPOValue(string value)
    {
        this.TxtPO.Text = value;
    }

    public void SetQtyValue(string value)
    {
        this.TxtQty.Text = value;
    }

    public string GetStoreValue()
    {
        return this.TxtStoreId.Text;
    }

    public string GetPOValue()
    {
        return this.TxtPO.Text;
    }

    public string GetQtyValue()
    {
        return this.TxtQty.Text;
    }

    public TableRow GetRow()
    {
        return row;
    }

    public void RegisterJavaScript()
    {
        //checkBox.Attributes.Add("onclick", "javascript:"
        //                            + "document.getElementById('" + TxtPO.ClientID + "').readOnly = !document.getElementById('" + TxtPO.ClientID + "').readOnly;"
        //                            + "document.getElementById('" + TxtQty.ClientID + "').readOnly = !document.getElementById('" + TxtQty.ClientID + "').readOnly;"
        //                       );

        checkBox.Attributes.Add("onclick", "if(document.getElementById('" + checkBox.ClientID + "').checked) { document.getElementById('" + TxtQty.ClientID + "').value = '1';} else { document.getElementById('" + TxtQty.ClientID + "').value = '';}");

        TxtPO.Attributes.Add("onkeypress", "if(!document.getElementById('" + checkBox.ClientID + "').checked) { alert('You have to check current store!'); }; return document.getElementById('" + checkBox.ClientID + "').checked;");

        TxtQty.Attributes.Add("onfocusout", "IsNumber(this, 3);");
        TxtQty.Attributes.Add("onfocus", "SaveLastValue(this.value);");
        TxtQty.Attributes.Add("onkeypress", "if(!document.getElementById('" + checkBox.ClientID + "').checked) { alert('You have to check current store!'); }; return document.getElementById('" + checkBox.ClientID + "').checked;");
    }

    public bool IsDisabled()
    {
        if (!checkBox.Checked || TxtQty.Text.Trim().Length == 0 || Convert.ToInt32(TxtQty.Text) == 0)
            return true;
        return false;
    }
}
