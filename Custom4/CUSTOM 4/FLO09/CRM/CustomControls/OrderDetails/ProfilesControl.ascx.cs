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
                // Enhance Profile Performance
                // codes.Add(profilesNamesDataTable.Rows[i]["Code"].ToString());
                profilesLables[i].Text = profilesNamesDataTable.Rows[i]["Description"].ToString()+":";
                // DataTable profileListDataTable = businessObject.GetProfileList("", "", "99", profilesNamesDataTable.Rows[i]["Code"].ToString());
                // Enhance Profile Performance

                // Enhance Profile Performance
                //Session["Profile" + i.ToString().TrimEnd()] = profileListDataTable;

                //profilesLists[i].DataSource = profileListDataTable;
                //profilesLists[i].DataTextField = "Value";
                //profilesLists[i].DataValueField = "Value";
                //profilesLists[i].DataBind();
                profilesLists[i].Items.Clear();
                profilesLists[i].Items.Add("N / A");
                profilesLists[i].SelectedIndex = 0;
                // Enhance Profile Performance
            }
        }
        else
        {
            for (int i = 0; i < size; i++)
            {
                // Enhance Profile Performance
                // codes.Add(profilesNamesDataTable.Rows[i]["Code"].ToString());
                profilesLables[i].Text = profilesNamesDataTable.Rows[i]["Description"].ToString()+":";
                // DataTable profileListDataTable = businessObject.GetProfileList("", "", "99", profilesNamesDataTable.Rows[i]["Code"].ToString());
                // Enhance Profile Performance

                // Enhance Profile Performance
                //Session["Profile" + i.ToString().TrimEnd()] = profileListDataTable;
                
                //profilesLists[i].DataSource = profileListDataTable;
                //profilesLists[i].DataTextField = "Value";
                //profilesLists[i].DataValueField = "Value";
                //profilesLists[i].DataBind();
                profilesLists[i].Items.Clear();
                profilesLists[i].Items.Add("N / A");
                profilesLists[i].SelectedIndex = 0;
                // Enhance Profile Performance
            }
            for (int i = size; i < 12; i++)
            {
                profilesLables[i].Visible = false;
                profilesLists[i].Visible = false;
            }
        }
    }

    // Enhance Profile Performance
    protected override void OnLoad(EventArgs e)
    {
        base.OnLoad(e);

        for (int i = 0; i < 12; i++)
        {
            profilesLists[i].Items.Clear();
            if (Request.Form[profilesLists[i].UniqueID] == null)
            {
                profilesLists[i].Items.Add("N / A");
                profilesLists[i].SelectedIndex = 0;
            }
            else
            {
                profilesLists[i].Items.Add("N / A");
                profilesLists[i].Items.Add(Request.Form[profilesLists[i].UniqueID]);
                profilesLists[i].SelectedValue = Request.Form[profilesLists[i].UniqueID];
            }
        }
    }
    // Enhance Profile Performance

    public void FillProfileControlsFromSummary(int rowIndex)
    {
        ProfilesSummaryController profilesConrtoller = ((OrderDataTableController)Session["Order"]).linesProfiles;
        string[] values = profilesConrtoller.GetValues(rowIndex);

        for (int i = 0; i < values.Length; i++)
        {
            // Enhance Profile Performance
            //try
            //{
            // Enhance Profile Performance

            // Enhance Profile Performance
            profilesLists[i].Items.Clear();
            profilesLists[i].Items.Add("N / A");
            profilesLists[i].Items.Add(values[i]);
            // Enhance Profile Performance
            profilesLists[i].SelectedValue = values[i];

            // Enhance Profile Performance
            // }
            // catch(Exception)
            // {
            //     profilesLists[i].SelectedIndex = 0;
            // }
            // Enhance Profile Performance
        }
    }

    public void FillProfileControlsFromTemplate(int rowIndex)
    {
        ProfilesTemplateController profilesTemplateConrtoller = ((OrderDataTableController)Session["Order"]).templateProfiles;
        string[] values = profilesTemplateConrtoller.GetValues(rowIndex);

        for (int i = 0; i < values.Length; i++)
        {
            // Enhance Profile Performance
            profilesLists[i].Items.Clear();
            profilesLists[i].Items.Add("N / A");
            profilesLists[i].Items.Add(values[i]);
            // Enhance Profile Performance
            profilesLists[i].SelectedValue = values[i];
        }
    }

    public void SetDefaults(string style)
    {
        ProfilesSummaryController controler = ((OrderDataTableController)Session["Order"]).linesProfiles;
        for (int index = 0; index < controler.codes.Count; index++)
        {
            // Enhance Profile Performance
            //try
            //{
            //    profilesLists[index].SelectedIndex = 0;
            //}
            //catch (Exception e)
            //{
            //}
            // Enhance Profile Performance

            AriaBusinessObjectAdapter businessObject = new AriaBusinessObjectAdapter();
            DataTable profiles = businessObject.GetProfileValues("", "", "99", "ST", style.PadRight(130), controler.codes[index].Trim());
            if (profiles.Rows.Count > 0)
            {
                // Enhance Profile Performance
                //try
                //{
                //    profilesLists[index].SelectedValue = profiles.Rows[0]["Value"].ToString();
                //}
                //catch (Exception e)
                //{
                //}
                profilesLists[index].Items.Clear();
                profilesLists[index].Items.Add("N / A");
                profilesLists[index].SelectedIndex = 0;
                if (profiles.Rows[0]["Value"].ToString().TrimEnd().Length > 0)
                {
                    profilesLists[index].Items.Clear();
                    profilesLists[index].Items.Add("N / A");
                    profilesLists[index].Items.Add(profiles.Rows[0]["Value"].ToString());
                    profilesLists[index].SelectedValue = profiles.Rows[0]["Value"].ToString();
                }
                // Enhance Profile Performance
            }
        }
    }

    public void SetInitValues()
    {
        ProfilesSummaryController controler = ((OrderDataTableController)Session["Order"]).linesProfiles;

        for (int index = 0; index < controler.codes.Count; index++)
        {
            // Enhance Profile Performance
            profilesLists[index].Items.Clear();
            profilesLists[index].Items.Add("N / A");
            // Enhance Profile Performance
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
