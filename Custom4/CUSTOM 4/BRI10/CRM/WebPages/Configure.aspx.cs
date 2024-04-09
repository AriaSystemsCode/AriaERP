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

public partial class WebPages_Configure : System.Web.UI.Page
{
    protected override void OnInit(EventArgs e)
    {
        base.OnInit(e);

        if (((OrderDataTableController)Session["Order"]).tempDesigns.designsTable == null)
        {
            ((OrderDataTableController)Session["Order"]).tempDesigns = new DesignDataTableController();
        }

        if (!((OrderDataTableController)Session["Order"]).tempDesigns.descriptionLoaded)
        {
            ((OrderDataTableController)Session["Order"]).tempDesigns.LoadDescription();
        }

        AriaBusinessObjectAdapter adapter1 = new AriaBusinessObjectAdapter();
        DataTable table1 = adapter1.GetCode("", "", "99", "CDSGNCTGRY");
        ddlCategory.DataSource = table1;
        ddlCategory.DataTextField = "Description";
        ddlCategory.DataValueField = "Code";
        ddlCategory.DataBind();
        ddlCategory.SelectedValue = adapter1.GetDefaultCode("", "", "99", "CDSGNCTGRY");

        AriaBusinessObjectAdapter adapter2 = new AriaBusinessObjectAdapter();
        DataTable table2 = adapter2.GetCode("", "", "99", "CDSGNTYPE");
        ddlDesignType.DataSource = table2;
        ddlDesignType.DataTextField = "Description";
        ddlDesignType.DataValueField = "Code";
        ddlDesignType.DataBind();
        ddlDesignType.SelectedValue = adapter1.GetDefaultCode("", "", "99", "CDSGNTYPE");

        AriaBusinessObjectAdapter adapter3 = new AriaBusinessObjectAdapter();
        DataTable table3 = adapter3.GetCode("", "", "99", "CSTYLEPOS");
        ddlPlacement.DataSource = table3;
        ddlPlacement.DataTextField = "Description";
        ddlPlacement.DataValueField = "Code";
        ddlPlacement.DataBind();
        ddlPlacement.SelectedValue = adapter1.GetDefaultCode("", "", "99", "CSTYLEPOS");

        txtNoOfImprints.Attributes.Add("onfocusout", "IsNumber(this, 6);");
        txtNoOfImprints.Attributes.Add("onfocus", "SaveLastValue(this.value)");

        txtPrice.Attributes.Add("onfocusout", "IsDouble(this);");
        txtPrice.Attributes.Add("onfocus", "SaveLastValue(this.value)");
    }

    public void ClearAll()
    {
        AriaBusinessObjectAdapter adapter1 = new AriaBusinessObjectAdapter();
        txtDesignCode.Text = "";
        this.txtDescription.Text = "";
        this.txtNameDrop.Text = "";
        this.txtLine1.Text = "";
        this.txtLine2.Text = "";
        this.txtLine3.Text = "";
        this.txtLine4.Text = "";
        this.ddlCategory.SelectedValue = adapter1.GetDefaultCode("", "", "99", "CDSGNCTGRY");
        this.ddlDesignType.SelectedValue = adapter1.GetDefaultCode("", "", "99", "CDSGNTYPE");
        this.ddlPlacement.SelectedValue = adapter1.GetDefaultCode("", "", "99", "CSTYLEPOS");
        this.txtPrice.Text = "0.00";
        this.txtNoOfImprints.Text = "0";
    }

    protected void txtDesignCode_TextChanged(object sender, EventArgs e)
    {
        string[] strArray = txtDesignCode.Text.Split('|');
        if (strArray.Length == 2)
        {
            txtDesignCode.Text = strArray[0].Trim();
        }


        Aria.DataTypes.AriaDataObjectPointer pointer = new Aria.DataTypes.AriaDataObjectPointer();
        pointer.AddKeyField("ArtWorkDesignID", txtDesignCode.Text.Trim());
        AriaBusinessObjectAdapter businessObject = new AriaBusinessObjectAdapter();
        DataTable designFields = businessObject.GetBusinessObject("", "", "99", "CRM.ArtWork", pointer);

        if (designFields.Rows.Count > 0)
        {
            this.txtDescription.Text = designFields.Rows[0]["DesignName"].ToString();
            this.txtNameDrop.Text = designFields.Rows[0]["NameDropId"].ToString();
            this.txtLine1.Text = designFields.Rows[0]["NameDropTextTop"].ToString();
            this.txtLine2.Text = designFields.Rows[0]["NameDropTextBottom"].ToString();
            this.txtLine3.Text = designFields.Rows[0]["NameDropTextLine3"].ToString();
            this.txtLine4.Text = designFields.Rows[0]["NameDropTextLine4"].ToString();

            try
            {
                this.ddlCategory.SelectedValue = designFields.Rows[0]["ArtworDesignCategory"].ToString();
            }
            catch (Exception ex)
            {
                ddlCategory.SelectedValue = businessObject.GetDefaultCode("", "", "99", "CDSGNCTGRY");
            }

            try
            {
                this.ddlDesignType.SelectedValue = designFields.Rows[0]["ArtworkDesignType"].ToString();
            }
            catch (Exception ex)
            {
                ddlDesignType.SelectedValue = businessObject.GetDefaultCode("", "", "99", "CDSGNTYPE");
            }

            try
            {
                this.ddlPlacement.SelectedValue = designFields.Rows[0]["StylePosition"].ToString();
            }
            catch (Exception ex)
            {
                ddlPlacement.SelectedValue = businessObject.GetDefaultCode("", "", "99", "CSTYLEPOS");
            }
        }
        else
        {
            ClearAll();
        }
    }


    private void UpdateGrid()
    {
        DesignDataTableController controller = ((OrderDataTableController)Session["Order"]).tempDesigns;

        if (controller.designsTable.Rows.Count > 0)
        {
            GridView1.DataSource = controller.designsTable;
        }
        else
        {
            DesignDataTableController tempController = new DesignDataTableController();
            DataTable tempTable = tempController.designsTable;
            tempTable.Rows.Add(tempTable.NewRow());
            GridView1.DataSource = tempTable;
        }
        GridView1.DataBind();
    }

    protected void btnUpdate_Click(object sender, EventArgs e)
    {
        if (txtDesignCode.Text.Trim().Length == 0)
        {
            ScriptManager.RegisterClientScriptBlock(this.Page, typeof(string), "", "alert('No design selected!')", true);
            return;
        }

        DesignDataTableController controller = ((OrderDataTableController)Session["Order"]).tempDesigns;
        if (btnUpdate.Text.Equals("Add"))
        {

            DataRow row = controller.designsTable.NewRow();

            row["ArtworkDesignCode"] = txtDesignCode.Text;
            row["DesignName"] = txtDescription.Text;
            row["StylePosition"] = ddlPlacement.SelectedValue;
            row["ArtworDesignCategory"] = ddlCategory.SelectedValue;
            row["ArtworkDesignType"] = ddlDesignType.SelectedValue;
            row["NumberofImprints"] = Convert.ToInt32(txtNoOfImprints.Text);
            row["Price"] = Convert.ToDouble(txtPrice.Text);

            row["StylePositionDescription"] = ddlPlacement.SelectedItem == null ? "" :  ddlPlacement.SelectedItem.Text;
            row["ArtworDesignCategoryDescription"] = ddlCategory.SelectedItem.Text;
            row["ArtworkDesignTypeDescription"] = ddlDesignType.SelectedItem.Text;


            row["DesignName"] = this.txtDescription.Text;
            row["NameDropId"] = this.txtNameDrop.Text;
            row["NameDropTextTop"] = this.txtLine1.Text;
            row["NameDropTextBottom"] = this.txtLine2.Text;
            row["NameDropTextLine3"]= this.txtLine3.Text;
            row["NameDropTextLine4"] = this.txtLine4.Text ;

            controller.Insert(row, 0);
        }
        else if (btnUpdate.Text.Equals("Update"))
        {
            int rowIndex = (int)Session["designEditRowIndex"];

            DataRow row = controller.designsTable.Rows[rowIndex];

            row["ArtworkDesignCode"] = txtDesignCode.Text;
            row["DesignName"] = txtDescription.Text;
            row["StylePosition"] = ddlPlacement.SelectedValue;
            row["ArtworDesignCategory"] = ddlCategory.SelectedValue;
            row["ArtworkDesignType"] = ddlDesignType.SelectedValue;
            row["NumberofImprints"] = Convert.ToInt32(txtNoOfImprints.Text);
            row["Price"] = Convert.ToDouble(txtPrice.Text);

            row["DesignName"] = this.txtDescription.Text;
            row["NameDropId"] = this.txtNameDrop.Text;
            row["NameDropTextTop"] = this.txtLine1.Text;
            row["NameDropTextBottom"] = this.txtLine2.Text;
            row["NameDropTextLine3"] = this.txtLine3.Text;
            row["NameDropTextLine4"] = this.txtLine4.Text;
        }

        GridView1.DataSource = controller.designsTable;

        UpdateGrid();

        btnUpdate.Text = "Add";

        ClearAll();
    }

    protected void Timer1_Tick(object sender, EventArgs e)
    {
        Timer1.Enabled = false;
    }

    protected void btnCancel_Click(object sender, EventArgs e)
    {
        ((OrderDataTableController)Session["Order"]).tempDesigns.designsTable.RejectChanges();
    }

    protected void Page_Load(object sender, EventArgs e)
    {
        Response.Cache.SetNoServerCaching();
        Response.Cache.SetCacheability(System.Web.HttpCacheability.NoCache);
        Response.Cache.SetNoStore();
        Response.Cache.SetExpires(new DateTime(1900, 01, 01, 00, 00, 00, 00));

        UpdateGrid();
    }
    protected void GridView1_SelectedIndexChanged(object sender, EventArgs e)
    {

    }

    protected void GridView1_RowCommand(object sender, GridViewCommandEventArgs e)
    {
        DesignDataTableController controller = ((OrderDataTableController)Session["Order"]).tempDesigns;

        if (controller.designsTable.Rows.Count == 0)
        {
            return;
        }

        if (e.CommandSource is LinkButton)
        {
            int rowNo = Convert.ToInt32(((LinkButton)e.CommandSource).ClientID.Replace("GridView1_ctl", "").Replace("_btnDelete", "").Replace("_btnEdit", "")) - 2;

            if (((LinkButton)e.CommandSource).Text == "Edit")
            {
                btnUpdate.Text = "Update";
                Session["designEditRowIndex"] = rowNo;

                txtDesignCode.Text = controller.designsTable.Rows[rowNo]["ArtworkDesignCode"].ToString();
                this.txtPrice.Text = Convert.ToDouble(controller.designsTable.Rows[rowNo]["Price"]).ToString("f");
                this.txtNoOfImprints.Text = controller.designsTable.Rows[rowNo]["NumberofImprints"].ToString();

                this.txtDescription.Text = controller.designsTable.Rows[rowNo]["DesignName"].ToString();
                this.txtNameDrop.Text = controller.designsTable.Rows[rowNo]["NameDropId"].ToString();
                this.txtLine1.Text = controller.designsTable.Rows[rowNo]["NameDropTextTop"].ToString();
                this.txtLine2.Text = controller.designsTable.Rows[rowNo]["NameDropTextBottom"].ToString();
                this.txtLine3.Text = controller.designsTable.Rows[rowNo]["NameDropTextLine3"].ToString();
                this.txtLine4.Text = controller.designsTable.Rows[rowNo]["NameDropTextLine4"].ToString();
                
                this.ddlCategory.SelectedValue = controller.designsTable.Rows[rowNo]["ArtworDesignCategory"].ToString();
                this.ddlDesignType.SelectedValue = controller.designsTable.Rows[rowNo]["ArtworkDesignType"].ToString();
                this.ddlPlacement.SelectedValue = controller.designsTable.Rows[rowNo]["StylePosition"].ToString();
            }
            else
            {
                int rowIndex = rowNo;
                controller.designsTable.Rows[rowNo].Delete();
                ClearAll();
                UpdateGrid();
            }
        }
    }
}
