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
using Aria.Data.BusinessObject;

public partial class OrderDetails_TemplateControl : System.Web.UI.UserControl
{
    private TemplateDataTableController templateLines;

    protected void Page_Load(object sender, EventArgs e)
    {
        Session["TemplateControl"] = this;

        templateLines = ((OrderDataTableController)Session["Order"]).templateLines;
        
        Update();
        SaveIDsToHiddenField();
        AssignToStoresBtn.Attributes.Add("onclick", "jsAssignToStore('<%= "+ HiddenField1.ClientID +" %>')");

        SelectAllBtn.Attributes.Add("onclick", "Loading('Selecting...')");
        SelectNoneBtn.Attributes.Add("onclick", "Loading('Unselecting...')");
        RemoveBtn.Attributes.Add("onclick", "Loading('Removing...')");
    }

    private void SaveIDsToHiddenField()
    {
        this.HiddenField1.Value = "";
        int i = 0;
        GridViewRow row = null;
        CheckBox cb = null;
        for (; i < TemplateLinesGrid.Rows.Count-1 ; i++)
        {
            row = TemplateLinesGrid.Rows[i];
            cb = (CheckBox)row.FindControl("TemplateLineSelectorChkBx");
            this.HiddenField1.Value += cb.ClientID+" ";            
        }
        if (TemplateLinesGrid.Rows.Count > 0)
        {
            row = TemplateLinesGrid.Rows[TemplateLinesGrid.Rows.Count-1];
            cb = (CheckBox)row.FindControl("TemplateLineSelectorChkBx");
            this.HiddenField1.Value += cb.ClientID;
        }
    }

    public void Update()
    {
        if (templateLines.select().Rows.Count > 0)
        {
            TemplateLinesGrid.DataSource = templateLines.select();
            TemplateLinesGrid.Columns[0].Visible = false;
            TemplateLinesGrid.Columns[1].Visible = true;
        }
        else
        {
            TemplateDataTableController tempLines = new TemplateDataTableController();
            DataTable tempTable = tempLines.select();
            tempTable.Rows.Add(tempTable.NewRow());
            TemplateLinesGrid.DataSource = tempTable;
            TemplateLinesGrid.Columns[0].Visible = true;
            TemplateLinesGrid.Columns[1].Visible = false;
        }

        TemplateLinesGrid.DataBind();
    }


    protected void TemplateLinesGrid_RowCommand(object sender, GridViewCommandEventArgs e)
    {
        int rowIndex = Int32.Parse(e.CommandArgument.ToString());
        rowIndex = templateLines.GetNotDeletedRowNo(rowIndex);
        DataRow row = templateLines.getLineDataRow(rowIndex);
        //////////////////////////////////////////////////////////////////////////////////////////////
        string style = row["Style"].ToString();
        string styleMajor = style.Substring(0, 12).Trim();
        string color = style.Substring(13).Trim();
        Control containerControl = Parent.Parent.Parent.Parent.Parent.FindControl("OrderDetailsContainerControl1");
        Control styleControl = containerControl.FindControl("Styles1");
        Object[] parameters = new Object[3];
        parameters[0] = style;
        //parameters[1] = color;
        parameters[1] = row["StoreNumber"].ToString();
        parameters[2] = row["CustomerPO"].ToString();
        styleControl.GetType().GetMethod("FillStyleControls").Invoke(styleControl, parameters);
        ((TextBox)styleControl.FindControl("StyleCode")).Enabled = false;
        //////////////////////////////////////////////////////////////////////////////////////////////
        Control DescriptionControl = Parent.Parent.Parent.Parent.Parent.FindControl("OrderDetailsContainerControl1").FindControl("Description1");
        parameters = new Object[13];
        parameters[0] = row["BookedQuantity1"].ToString();
        parameters[1] = row["BookedQuantity2"].ToString();
        parameters[2] = row["BookedQuantity3"].ToString();
        parameters[3] = row["BookedQuantity4"].ToString();
        parameters[4] = row["BookedQuantity5"].ToString();
        parameters[5] = row["BookedQuantity6"].ToString();
        parameters[6] = row["BookedQuantity7"].ToString();
        parameters[7] = row["BookedQuantity8"].ToString();

        parameters[8] = Convert.ToDouble(row["DiscountPercent"]);
        parameters[9] = row["Group"].ToString();
        parameters[10] = Convert.ToDouble(row["GrossPrice"]);
        parameters[11] = Convert.ToDouble(row["Price"]);
        parameters[12] = row["Description1"].ToString();

        DescriptionControl.GetType().GetMethod("FillDescriptionControls").Invoke(DescriptionControl, parameters);

        //////////////////////////////////////////////////////////////////////////////////////////////
        Control ProfileControl = Parent.Parent.Parent.Parent.Parent.FindControl("OrderDetailsContainerControl1").FindControl("Profiles1");
        parameters = new Object[1];
        int lineNo = Convert.ToInt32(templateLines.select().Rows[rowIndex]["LineNumber"]);
        parameters[0] = lineNo;

        ProfileControl.GetType().GetMethod("FillProfileControlsFromTemplate").Invoke(ProfileControl, parameters);
        //////////////////////////////////////////////////////////////////////////////////////////////
        Session["TemplateState"] = lineNo.ToString();


        ((OrderDataTableController)Session["Order"]).tempDesigns = new DesignDataTableController();

        ((OrderDataTableController)Session["Order"]).tempDesigns.Insert(((OrderDataTableController)Session["Order"]).tempDesigns.GetDesignsForLine(lineNo), 0);

        this.Visible = false;
        containerControl.Visible = true;
    }
   
    [System.Web.Services.WebMethodAttribute(), System.Web.Script.Services.ScriptMethodAttribute()]
    public static string GetDynamicContent(string contextKey)
    {
        return default(string);
    }
    
    protected void BackBtn_Click(object sender, EventArgs e)
    {
        this.Visible = false;
        Parent.FindControl("OrderDetailsContainerControl1").Visible = true;
    }

    

    private ArrayList GetSelectedLinesFromTemplate()
    {
        ArrayList list = new ArrayList();
        for (int i = TemplateLinesGrid.Rows.Count - 1; i >= 0; i--)
        {
            GridViewRow row = TemplateLinesGrid.Rows[i];
            CheckBox cb = (CheckBox)row.FindControl("TemplateLineSelectorChkBx");
            if (cb != null && cb.Checked)
            {
                list.Add(i);                
            }
        }       
        return list;
    }

    protected void RemoveBtn_Click(object sender, EventArgs e)
    {
        bool linesSelected = false;

        for (int i = TemplateLinesGrid.Rows.Count - 1; i >= 0; i--)
        {
            GridViewRow row = TemplateLinesGrid.Rows[i];
            CheckBox cb = (CheckBox)row.FindControl("TemplateLineSelectorChkBx");
            if (cb != null && cb.Checked)
            {
                linesSelected = true;

                templateLines.delete(i);
            }
        }

        if (linesSelected)
        {
            ScriptManager.RegisterClientScriptBlock(this.Parent, typeof(string), "", "LoadingComplete()", true);
            Update();
        }
        else
        {
            ScriptManager.RegisterClientScriptBlock(this.Page, typeof(string), "", "alert('No lines selected!')", true);
        }
    }

    protected void SelectAllBtn_Click(object sender, EventArgs e)
    {
        ScriptManager.RegisterClientScriptBlock(this.Parent, typeof(string), "", "LoadingComplete()", true);

        for (int i = TemplateLinesGrid.Rows.Count - 1; i >= 0; i--)
        {
            GridViewRow row = TemplateLinesGrid.Rows[i];
            CheckBox cb = (CheckBox)row.FindControl("TemplateLineSelectorChkBx");
            cb.Checked = true;
        }
    }
    protected void SelectNoneBtn_Click(object sender, EventArgs e)
    {
        ScriptManager.RegisterClientScriptBlock(this.Parent, typeof(string), "", "LoadingComplete()", true);

        for (int i = TemplateLinesGrid.Rows.Count - 1; i >= 0; i--)
        {
            GridViewRow row = TemplateLinesGrid.Rows[i];
            CheckBox cb = (CheckBox)row.FindControl("TemplateLineSelectorChkBx");
            cb.Checked = false;
        }
    }
    protected void AssignToStoresBtn_Click(object sender, EventArgs e)
    {
        ScriptManager.RegisterClientScriptBlock(this.Parent, typeof(string), "", "LoadingComplete()", true);
    }
    protected void AssignToStoresBtn_Click1(object sender, EventArgs e)
    {
        Session["AssignToStore"] = null;
        ScriptManager.RegisterClientScriptBlock(this.Parent, typeof(string), "", "LoadingComplete()", true);

    }
}
