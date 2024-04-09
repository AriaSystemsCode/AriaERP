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
using Aria.DataTypes;
using System.Collections.Generic;

public partial class ProfilesControl : System.Web.UI.UserControl
{
    private int numberOfProfiles;
    private Label[] profilesLables = new Label[12];
    private DropDownList[] profilesLists = new DropDownList[12];
    protected override void OnInit(EventArgs e)
    {
        base.OnInit(e);
        
        profilesLables[0] = Label1;
        profilesLables[1] = Label2;
        profilesLables[2] = Label3;
        profilesLables[3] = Label4;
        profilesLables[4] = Label5;
        profilesLables[5] = Label6;
        profilesLables[6] = Label7;
        profilesLables[7] = Label8;
        profilesLables[8] = Label9;
        profilesLables[9] = Label10;
        profilesLables[10] = Label11;
        profilesLables[11] = Label12;

        
        profilesLists[0] = DropDownList1;
        profilesLists[1] = DropDownList2;
        profilesLists[2] = DropDownList3;
        profilesLists[3] = DropDownList4;
        profilesLists[4] = DropDownList5;
        profilesLists[5] = DropDownList6;
        profilesLists[6] = DropDownList7;
        profilesLists[7] = DropDownList8;
        profilesLists[8] = DropDownList9;
        profilesLists[9] = DropDownList10;
        profilesLists[10] = DropDownList11;
        profilesLists[11] = DropDownList12;


        List<string> codes = new List<string>();
        
        AriaBusinessObjectAdapter businessObject = new AriaBusinessObjectAdapter();
        DataTable profilesNamesDataTable = businessObject.GetProfileCodes("", "", "99", "SO", "ST"); //
        int size = profilesNamesDataTable.Rows.Count;
        numberOfProfiles = size;
        if (size >= 12)
        {
            for (int i = 0; i < 12; i++)
            {
                codes.Add(profilesNamesDataTable.Rows[i]["Code"].ToString());
                profilesLables[i].Text = profilesNamesDataTable.Rows[i]["Description"].ToString()+":";
                DataTable profileListDataTable = businessObject.GetProfileList("", "", "99", profilesNamesDataTable.Rows[i]["Code"].ToString());

                profilesLists[i].DataSource = profileListDataTable;
                profilesLists[i].DataTextField = "Value";
                profilesLists[i].DataValueField = "Value";
                profilesLists[i].DataBind();
            }
        }
        else
        {
            for (int i = 0; i < size; i++)
            {
                codes.Add(profilesNamesDataTable.Rows[i]["Code"].ToString());
                profilesLables[i].Text = profilesNamesDataTable.Rows[i]["Description"].ToString()+":";
                DataTable profileListDataTable = businessObject.GetProfileList("", "", "99", profilesNamesDataTable.Rows[i]["Code"].ToString());
                
                profilesLists[i].DataSource = profileListDataTable;
                profilesLists[i].DataTextField = "Value";
                profilesLists[i].DataValueField = "Value";
                profilesLists[i].DataBind();
            }
            for (int i = size; i < 12; i++)
            {
                profilesLables[i].Visible = false;
                profilesLists[i].Visible = false;
            }
        }
    }

    public void FillProfileControlsFromSummary(int rowIndex)
    {
        ProfilesSummaryController profilesConrtoller = ((OrderDataTableController)Session["Order"]).linesProfiles;
        string[] values = profilesConrtoller.GetValues(rowIndex);

        for (int i = 0; i < values.Length; i++)
        {
            try
            {
                profilesLists[i].SelectedValue = values[i];
            }
            catch(Exception)
            {
                profilesLists[i].SelectedIndex = 0;
            }
        }
    }

    public void FillProfileControlsFromTemplate(int rowIndex)
    {
        ProfilesTemplateController profilesTemplateConrtoller = ((OrderDataTableController)Session["Order"]).templateProfiles;
        string[] values = profilesTemplateConrtoller.GetValues(rowIndex);

        for (int i = 0; i < values.Length; i++)
        {
            profilesLists[i].SelectedValue = values[i];
        }
    }

    public void SetDefaults(string style)
    {
        ProfilesSummaryController controler = ((OrderDataTableController)Session["Order"]).linesProfiles;
        for (int index = 0; index < controler.codes.Count; index++)
        {
            try
            {
                profilesLists[index].SelectedIndex = 0;
            }
            catch (Exception e)
            {
            }

            AriaBusinessObjectAdapter businessObject = new AriaBusinessObjectAdapter();
            DataTable profiles = businessObject.GetProfileValues("", "", "99", "ST", style.PadRight(130), controler.codes[index].Trim());
            if (profiles.Rows.Count > 0)
            {
                try
                {
                    profilesLists[index].SelectedValue = profiles.Rows[0]["Value"].ToString();
                }
                catch (Exception e)
                {
                }
            }
        }
    }

    public void SetInitValues()
    {
        ProfilesSummaryController controler = ((OrderDataTableController)Session["Order"]).linesProfiles;

        for (int index = 0; index < controler.codes.Count; index++)
        {
            profilesLists[index].SelectedIndex = 0;
        }
    }

    public string[] GetProfilesValues()
    {
        string[] values = new string[this.numberOfProfiles];
        for (int i = 0; i < this.numberOfProfiles; i++)
        {
            values[i] = profilesLists[i].SelectedValue;
        }
        return values;
    }

    public bool IsValid()
    {
        for (int i = 0; i < this.numberOfProfiles; i++)
        {
            if (profilesLists[i].SelectedIndex == 0)
            {
                return false;
            }
        }

        return true;
    }
        
}
