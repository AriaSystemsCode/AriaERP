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
using System.Diagnostics;

public partial class ButtonsControl : System.Web.UI.UserControl
{
    private DescriptionControl descriptionControl;
    private ProfilesControl profilesControl;
    private StylesControl stylesControl;

    protected override void OnInit(EventArgs e)
    {
        base.OnInit(e);

        UpdateToTemplateBtn.Attributes.Add("onclick", "Loading(' Updating...');");
    }

    protected void Page_Load(object sender, EventArgs e)
    {
        if (Session["StoreID"].ToString().Equals("Multi"))
        {
            UpdateToTemplateBtn.Enabled = true;
            ViewTemplateBtn.Enabled = true;
        }

        if (!Page.IsPostBack)
            Session.Add("TemplateState", "New");

        descriptionControl = (DescriptionControl)Parent.FindControl("Description1");
        profilesControl = (ProfilesControl)Parent.FindControl("Profiles1");
        stylesControl = (StylesControl)Parent.FindControl("Styles1");
    }

    protected void ViewTemplateBtn_Click(object sender, EventArgs e)
    {
        Parent.Visible = false;
        Parent.Parent.FindControl("TemplateControl1").Visible = true;
    }

    protected void UpdateToTemplateBtn_Click(object sender, EventArgs e)
    {
        try
        {
            if (Session["AddingLine"] != null)
            {
                ScriptManager.RegisterClientScriptBlock(this.Page, typeof(string), "", "LoadingComplete();alert('Please wait!')", true);
                stylesControl.ClearStyleControls();
                Session["AddingLine"] = null;
                return;
            }

            Session["AddingLine"] = "on";

            string styleCode = stylesControl.GetStyleCode();

            if (styleCode.Trim() == "*")
            {
                ScriptManager.RegisterClientScriptBlock(this.Page, typeof(string), "", "LoadingComplete();alert('Please select style.!');", true);
                return;
            }

            string colorDescription = stylesControl.GetColorDescription();
            string descriptionTextValue = descriptionControl.GetStyleDescriptionValue();
            int[] qty = descriptionControl.GetAllQtyValues();

            if ((qty[0] + qty[1] + qty[2] + qty[3] + qty[4] + qty[5] + qty[6] + qty[7]) == 0)
            {
                ScriptManager.RegisterClientScriptBlock(this.Parent, typeof(string), "", "LoadingComplete();alert('No quantity assigned!')", true);
                Session["AddingLine"] = null;
                return;
            }

            if (!profilesControl.IsValid())
            {
                ScriptManager.RegisterClientScriptBlock(this.Parent, typeof(string), "", "LoadingComplete();alert('Please assign all profiles values!')", true);
                Session["AddingLine"] = null;
                return;
            }

            double grossPrice = descriptionControl.GetPriceValue();
            double disc = descriptionControl.GetDiscValue();
            string group = descriptionControl.GetGroupValue();
            TemplateDataTableController templateController = ((OrderDataTableController)Session["Order"]).templateLines;

            int LineNumber = 0;

            if (Session["TemplateState"].ToString().Equals("New"))
            {
                templateController.insert(styleCode, colorDescription, descriptionTextValue, qty[0], qty[1], qty[2], qty[3], qty[4], qty[5], qty[6], qty[7],
                    qty[0], qty[1], qty[2], qty[3], qty[4], qty[5], qty[6], qty[7],
                    grossPrice, disc, Convert.ToDateTime(((TextBox)Session["StartShipDateControl"]).Text), group, descriptionTextValue);

                LineNumber = Convert.ToInt32(Session["TemplateLastLineNumber"]);
            }
            else
            {
                LineNumber = Convert.ToInt32(Session["TemplateState"]);

                templateController.Update(LineNumber, styleCode, colorDescription, descriptionTextValue, qty[0], qty[1], qty[2], qty[3], qty[4], qty[5], qty[6], qty[7],
                    qty[0], qty[1], qty[2], qty[3], qty[4], qty[5], qty[6], qty[7],
                    grossPrice, disc, Convert.ToDateTime(((TextBox)Session["StartShipDateControl"]).Text), group, descriptionTextValue);
            }

            string[] values = profilesControl.GetProfilesValues();
            ProfilesTemplateController profilesTemplateController = ((OrderDataTableController)Session["Order"]).templateProfiles;
            if (Session["TemplateState"].ToString().Equals("New"))
            {
                profilesTemplateController.insert(LineNumber, values);
            }
            else
            {
                profilesTemplateController.update(LineNumber, values);
            }

            DesignDataTableController temp = ((OrderDataTableController)Session["Order"]).tempDesigns;
            if (temp.designsTable.GetChanges() == null || temp.designsTable.GetChanges().Rows.Count > 0)
            {
                if (Session["TemplateState"].ToString().Equals("New"))
                {
                    ((OrderDataTableController)Session["Order"]).templateDesigns.Insert(temp.designsTable.Select(), LineNumber);
                }
                else
                {
                    ((OrderDataTableController)Session["Order"]).templateDesigns.Update(temp.designsTable.Select(), LineNumber);
                }
            }

            ((OrderDataTableController)Session["Order"]).tempDesigns = new DesignDataTableController();

            stylesControl.ClearStyleControls();

            profilesControl.SetInitValues();

            Session["TemplateState"] = "New";

            Control templateControl = Parent.Parent.FindControl("TemplateControl1");
            templateControl.GetType().GetMethod("Update").Invoke(templateControl, null);

            ScriptManager.GetCurrent(this.Page).SetFocus(stylesControl.GetStyleControlClientID());

            ScriptManager.RegisterClientScriptBlock(this.Parent, typeof(string), "", "LoadingComplete();", true);
        }
        catch(Exception ex)
        {
            Session["AddingLine"] = null;

            string errorMessage = "Error: " + ex.Message;
            if (ex.InnerException != null)
            {
                errorMessage +=  "\n\r Error Detail:" + ex.InnerException.Message;
            }
            errorMessage  = errorMessage.Replace("'", "");

            ScriptManager.RegisterClientScriptBlock(this.Parent, typeof(string), "", "LoadingComplete(); alert('" + errorMessage + "')", true);

            EventLog.WriteEntry("Aria.CRM.UpdateTemplate", errorMessage + ":" + ex.StackTrace, EventLogEntryType.Information); 
        }
    }

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
}
