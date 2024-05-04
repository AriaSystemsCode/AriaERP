using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;
using System.Web.UI;
using System.Web.UI.WebControls;
using DevExpress.Web.ASPxHtmlEditor;
using AriaDevExpress.Module.DataContext;

namespace AriaDevExpress.Web.UserControls.WebSite
{
    public partial class Test : System.Web.UI.UserControl
    {
        public enum contactus
        {
            FirstName,
            LastName,
            Title,
            Phone,
            Email,
            CompanyName,
            Address1,
            City,
            State,
            Country,
            Zip,
            HearAboutUS,
            Comments
        }
        public enum coursereg
        {
            Name,
            Company,
            Title,
            Address1,
            Address2,
            Email,
            City,
            State,
            Zip,
            DateFrom,
            DateTo,
            TimeFrom,
            TimeTo,
            Telephone,
            Fax,
            CourseID,
            Country
        }
        public enum downloads
        {
            FirstName,
            LastName,
            Title,
            Email,
            Phone,
            CompanyName,
            Country,
            CurrentSoftware,
            Links
        }
        public enum bcmPartnr
        {
            FirstName,
            LastName,
            Title,
            Phone,
            Email,
            CompanyName,
            CompanySize,
            Address,
            City,
            State,
            Country,
            Zip,
            URL,
            BusinessType,
            HearAboutUS,
            CompanyType,
            CompSolution,
            BeenInBusiness,
            Comments
        }
        public enum eventreg
        {
            FirstName,
            LastName,
            Title,
            Phone,
            Email,
            CompanyName,
            CompanySize,
            Address1,
            City,
            State,
            Country,
            Zip,
            URL,
            BusinessType,
            HearAboutUS,
            ProductCategory,
            ProductInterest,
            Comments
        }
        public enum freetrail
        {
            FirstName,
            LastName,
            Title,
            Phone,
            Email,
            CompanyName,
            CompanySize,
            Address1,
            City,
            State,
            Country,
            Zip,
            URL,
            BusinessType,
            HearAboutUS,
            ProductCategory,
            ProductInterest,
            Comments
        };
        public enum None { };
        public enum News
        {
            FirstName,
            LastName,
            Title,
            Email,
            Phone,
            CompanyName,
            Country
        };

        AriaOnlineDataContext db = new AriaOnlineDataContext();
        bool saving;
        protected void Page_Load(object sender, EventArgs e)
        {
            string dd = Request.Form["__EVENTTARGET"];

            saving = false;

            if (dd == btnSave.UniqueID)
            {
                saving = true;
                save();
            }
            if (!IsPostBack)
            {
                fillToolBar("None");
            }
           
        }

        public void fillToolBar(string selected)
        {
            ToolbarDropDownMenu menuA = new ToolbarDropDownMenu("email", true, true);
            menuA.Image.Url = "~/Images/email_icon_blue.png";
            menuA.Image.Width = 30;
            menuA.Image.Height = 20;
            ToolbarDropDownMenu menuC = new ToolbarDropDownMenu("email", true, true);
            menuC.Image.Url = "~/Images/email_icon_blue.png";
            menuC.Image.Width = 30;
            menuC.Image.Height = 20;

            if (selected == "contactus")
            {
                foreach (string value in Enum.GetNames(typeof(contactus)))
                {
                    menuA.Items.Add("{" + value + "}", value);
                    menuC.Items.Add("{" + value + "}", value);
                }
            }
            else if (selected == "coursereg")
            {
                foreach (string value in Enum.GetNames(typeof(coursereg)))
                    menuA.Items.Add("{" + value + "}", value);
            }
            else if (selected == "downloads")
            {
                foreach (string value in Enum.GetNames(typeof(downloads)))
                {
                    menuA.Items.Add("{" + value + "}", value);
                    menuC.Items.Add("{" + value + "}", value);
                }
            }
            else if (selected == "freetrail")
            {
                foreach (string value in Enum.GetNames(typeof(freetrail)))
                {
                    menuA.Items.Add("{" + value + "}", value);
                    menuC.Items.Add("{" + value + "}", value);
                }
            }
            else if (selected == "bcmPartnr")
            {
                foreach (string value in Enum.GetNames(typeof(bcmPartnr)))
                {
                    menuA.Items.Add("{" + value + "}", value);
                    menuC.Items.Add("{" + value + "}", value);
                }
            }
            else if (selected == "eventreg")
            {
                foreach (string value in Enum.GetNames(typeof(eventreg)))
                {
                    menuA.Items.Add("{" + value + "}", value);
                    menuC.Items.Add("{" + value + "}", value);
                }
            }
            else if (selected == "None")
            {
                foreach (string value in Enum.GetNames(typeof(None)))
                {
                    menuA.Items.Add("{" + value + "}", value);
                    menuC.Items.Add("{" + value + "}", value);
                }
            }
            else if (selected == "News")
            {
                foreach (string value in Enum.GetNames(typeof(News)))
                {
                    menuA.Items.Add("{" + value + "}", value);
                    menuC.Items.Add("{" + value + "}", value);
                }
            }


            ASPxHtmlEditor1.Toolbars["StandardToolbar2"].Items.Add(menuA);
            ASPxHtmlEditor2.Toolbars["StandardToolbar2"].Items.Add(menuC);

            if (selected != "None" && db.EmailTemplates.Count() > 0)
            {
                var selectA = db.EmailTemplates.First(p => p.ID == selected + "A");// select p).First();
                var selectC = db.EmailTemplates.First(p => p.ID == selected + "C");
                if (!saving)
                {
                    if (selectA != null)
                    {
                        ASPxHtmlEditor1.Html = selectA.Body;
                        txtSubjectA.Text = selectA.Subject;
                        txtToA.Text = selectA.TO;
                    }
                    if (selectC != null)
                    {
                        ASPxHtmlEditor2.Html = selectC.Body;
                        txtSubjectC.Text = selectC.Subject;
                    }
                }
            }
        }
        protected void ddlTemplate_SelectedIndexChanged(object sender, EventArgs e)
        {
            fillToolBar(((DropDownList)sender).SelectedValue);
        }
        protected void btnSave_Click(object sender, EventArgs e)
        {
            if (ddlTemplate.SelectedValue != "None")
            {
                var selectA = (from p in db.EmailTemplates.Where(p => p.ID == ddlTemplate.SelectedValue + "A") select p).First();
                var selectC = (from p in db.EmailTemplates.Where(p => p.ID == ddlTemplate.SelectedValue + "C") select p).First();

                selectA.Body = ASPxHtmlEditor1.Html;
                selectC.Body = ASPxHtmlEditor2.Html;


                selectA.Subject = txtSubjectA.Text;
                selectC.Subject = txtSubjectC.Text;
                selectA.TO = txtToA.Text;

                db.SubmitChanges();
            }

        }
        public void save()
        {
            if (ddlTemplate.SelectedValue != "None")
            {
                var selectA = (from p in db.EmailTemplates.Where(p => p.ID == ddlTemplate.SelectedValue + "A") select p).First();
                var selectC = (from p in db.EmailTemplates.Where(p => p.ID == ddlTemplate.SelectedValue + "C") select p).First();

                selectA.Body = ASPxHtmlEditor1.Html;
                selectC.Body = ASPxHtmlEditor2.Html;


                selectA.Subject = txtSubjectA.Text;
                selectC.Subject = txtSubjectC.Text;
                selectA.TO = txtToA.Text;

                db.SubmitChanges();
            }
        }
    }
}