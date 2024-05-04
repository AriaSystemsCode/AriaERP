using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;
using System.Web.UI;
using System.Web.UI.WebControls;
using System.Web.UI.HtmlControls;
using DevExpress.Data.Filtering;
using System.IO;
using Aria5SystemAdmin.Module.Managers;
using Aria5SystemAdmin.Module.BusinessObjects;
namespace Clioprototype.Web
{
    public partial class ApplyForJobVacancy : System.Web.UI.Page
    {
         const string connectionstr = @"Data Source=SYSTEMADMIN\SQLSERVER;Initial Catalog=Aria5SystemAdmin_Test;User ID=sa;Password=aria_123";
         // const string connectionstr = @"Data Source=u4hfi3e5h9.database.windows.net;Initial Catalog=Aria5SystemAdmin_Staging;User ID=azuresqladmin;Password=aria_123";
        // const string connectionstr = @"Data Source=NSDE_KHALED;Initial Catalog=Aria5SystemAdmin22;User ID=sa;Password=aria_123";
       // const string connectionstr = @"Data Source=NSDE_KHALED;Initial Catalog=Aria5SystemAdmin_testing;User ID=sa;Password=aria_123";
        public List<Aria5SystemAdmin.Module.BusinessObjects.HRExperience> listofexperience
        {
            get
            {
                if (HttpContext.Current.Session["listofExperience"] == null)
                {
                    HttpContext.Current.Session["listofExperience"] = new List<Aria5SystemAdmin.Module.BusinessObjects.HRExperience>();
                }
                return HttpContext.Current.Session["listofExperience"] as List<Aria5SystemAdmin.Module.BusinessObjects.HRExperience>;
                
            }
            set
            {

                Session["listofExperience"] = value;
                
            }
        }
        public List<Aria5SystemAdmin.Module.BusinessObjects.HRSkillInstance> listofeducation
        {
            get
            {
                if (HttpContext.Current.Session["listofeducation"] == null)
                {
                    HttpContext.Current.Session["listofeducation"] = new List<Aria5SystemAdmin.Module.BusinessObjects.HRSkillInstance>();
                }
                return HttpContext.Current.Session["listofeducation"] as List<Aria5SystemAdmin.Module.BusinessObjects.HRSkillInstance>;

            }
            set
            {

                Session["listofeducation"] = value;

            }
        }
         public DevExpress.Xpo.Session sees
         {
             get
             {
                 if (HttpContext.Current.Session["Sess"] == null)
                 {
                     HttpContext.Current.Session["Sess"] = new DevExpress.Xpo.Session();
                 }
                 return HttpContext.Current.Session["Sess"] as DevExpress.Xpo.Session;

             }
             set
             {

                 Session["Sess"] = value;

             }
         }
           
        protected void Page_Load(object sender, EventArgs e)
        {
            if (!IsPostBack)
            {
                sees = new DevExpress.Xpo.Session();
                sees.ConnectionString = connectionstr;
                sees.Connect();
            }
            if (Request.QueryString["jobvacancyName"] != null)
            {
                string Name = Request.QueryString["jobvacancyName"].ToString();
                LbPositionName.Text = Name;
                
            }
            
           
        }

        protected void Button1_Click(object sender, EventArgs e)
        {
            if (string.IsNullOrEmpty(Exp_from.Text) || string.IsNullOrEmpty(Exp_to.Text) || string.IsNullOrEmpty(Exp_com.Text) || string.IsNullOrEmpty(Exp_jpos.Text))
            {
                return;
            }
            Aria5SystemAdmin.Module.BusinessObjects.HRExperience exp = new Aria5SystemAdmin.Module.BusinessObjects.HRExperience(sees);
           if(!string.IsNullOrEmpty(Exp_from.Text))
            exp.From = DateTime.Parse(Exp_from.Text);
           if(!string.IsNullOrEmpty(Exp_to.Text))
            exp.To = DateTime.Parse(Exp_to.Text);
            exp.Company = Exp_com.Text;
            exp.JobPosition = Exp_jpos.Text;
            exp.ReasonOfLeaving = Exp_reasons.Text;
            listofexperience.Add(exp);
            ListBox1.DataSource = listofexperience;
            ListBox1.DataTextField = "JobPosition";
            ListBox1.DataValueField = "JobPosition";
            ListBox1.DataBind();
            Exp_from.Text = "";
            Exp_to.Text = "";
            Exp_com.Text = "";
            Exp_jpos.Text = "";
            Exp_reasons.Text = "";
        }

        protected void BTN_Submit_Click(object sender, EventArgs e)
        {
            if (form2.Page.IsValid == true)
            {
                try
                {
                    if (Request.QueryString["jobvacancy"] == null)
                    {
                        return;
                    }
                    string oid = Request.QueryString["jobvacancy"].ToString();
                    Aria5SystemAdmin.Module.BusinessObjects.HRJobVacancy jv = sees.FindObject<Aria5SystemAdmin.Module.BusinessObjects.HRJobVacancy>(CriteriaOperator.Parse("[Oid] = '" + oid + "'"));
                    Aria5SystemAdmin.Module.BusinessObjects.HREmployee emp = new Aria5SystemAdmin.Module.BusinessObjects.HREmployee(sees);
                    emp.FirstName = Can_fName.Text;
                    emp.LastName = Can_lName.Text;
                    emp.Name = Can_fName.Text + " " + Can_lName.Text;
                    emp.EmploymentStatus = Aria5SystemAdmin.Module.BusinessObjects.Employment_status.Candidate;
                    emp.EMailAddress = Can_email.Text;
                    emp.Nationality = sees.FindObject<DevExpress.Persistent.BaseImpl.Country>(CriteriaOperator.Parse("Name LIKE '" + Can_Nation.Text + "'"));
                    emp.MilitaryStatus = (Aria5SystemAdmin.Module.BusinessObjects.Military_status)Enum.Parse(typeof(Aria5SystemAdmin.Module.BusinessObjects.Military_status), DDl_militarystatus.SelectedValue.ToString());
                    emp.BirthDate = DateTime.Parse(Can_BDate.Text);
                    emp.CurrentSalary = decimal.Parse(Can_currentSalary.Text);
                    emp.ExpectedSalary = Can_expectedsalary.Text;
                    emp.Gender = (Aria5SystemAdmin.Module.BusinessObjects.Gender)Enum.Parse(typeof(Aria5SystemAdmin.Module.BusinessObjects.Gender), DDl_Gender.SelectedValue);
                    if (!string.IsNullOrEmpty(Can_noticeperiod.Text))
                    {
                        emp.NoticePeriod = int.Parse(Can_noticeperiod.Text);
                    }
                    if (listofexperience.Count > 0)
                    {
                        foreach (HRExperience Exper in listofexperience)
                        {
                            //HRExperience existexp = sees.FindObject<HRExperience>(CriteriaOperator.Parse("Oid = '" + Exper.Oid + "'"));
                            HRExperience existexp = new HRExperience(sees);
                            existexp.From = Exper.From;
                            existexp.To = Exper.To;
                            existexp.Company = Exper.Company;
                            existexp.JobPosition = Exper.JobPosition;
                            existexp.ReasonOfLeaving = Exper.ReasonOfLeaving;
                            emp.Experience.Add(existexp);
                        }
                    }
                    else if (!string.IsNullOrEmpty(Exp_from.Text) && !string.IsNullOrEmpty(Exp_to.Text) && !string.IsNullOrEmpty(Exp_com.Text) && !string.IsNullOrEmpty(Exp_jpos.Text))
                    {
                        Aria5SystemAdmin.Module.BusinessObjects.HRExperience exp = new Aria5SystemAdmin.Module.BusinessObjects.HRExperience(sees);
                        if (!string.IsNullOrEmpty(Exp_from.Text))
                            exp.From = DateTime.Parse(Exp_from.Text);
                        if (!string.IsNullOrEmpty(Exp_to.Text))
                            exp.To = DateTime.Parse(Exp_to.Text);
                        exp.Company = Exp_com.Text;
                        exp.JobPosition = Exp_jpos.Text;
                        exp.ReasonOfLeaving = Exp_reasons.Text;
                        emp.Experience.Add(exp);

                    }

                    foreach (HRSkillInstance skInstance in listofeducation)
                    {
                        //  HRSkillInstance existinstan = sees.FindObject<HRSkillInstance>(CriteriaOperator.Parse("Oid = '" + skInstance.Oid + "'"));
                        HRSkillInstance existinstan = new HRSkillInstance(sees);
                        existinstan.Type = skInstance.Type;
                        existinstan.Category = sees.FindObject<Aria5SystemAdmin.Module.BusinessObjects.HRSkillsCategory>(CriteriaOperator.Parse("[Name] = 'B.s.'"));
                        existinstan.Unvirsety = skInstance.Unvirsety;
                        existinstan.Grade = skInstance.Grade;
                        existinstan.Completed = skInstance.Completed;
                        existinstan.CompletionDate = skInstance.CompletionDate;
                        existinstan.Major = skInstance.Major;
                        emp.Qualifications.Add(existinstan);
                    }
                    Aria5SystemAdmin.Module.BusinessObjects.ContactPhone phone = new Aria5SystemAdmin.Module.BusinessObjects.ContactPhone(sees);
                    phone.PhoneNumber = Can_Mob.Text;
                    emp.ContactPhones.Add(phone);
                    Aria5SystemAdmin.Module.BusinessObjects.Attachment att = new Aria5SystemAdmin.Module.BusinessObjects.Attachment(sees);
                    DevExpress.Persistent.BaseImpl.FileData x = new DevExpress.Persistent.BaseImpl.FileData(sees);
                    //ATA this modification because the error that the attached cv in the applicant form is corupted 11/9/2017 [start]
                    // Stream ms = FileUpload2.PostedFile.InputStream;//new MemoryStream(FileUpload2.FileBytes);
                    //x.SaveToStream(FileUpload2.FileContent);

                    x.LoadFromStream(FileUpload2.FileName, FileUpload2.PostedFile.InputStream);
                    //ATA this modification because the error that the attached cv in the applicant form is corupted 11/9/2017 [End]

                    x.FileName = FileUpload2.FileName;
                    att.File = x;
                    att.AttachmentID = x.FileName;
                    att.Name = x.FileName;
                    att.AttachmentType = Aria5SystemAdmin.Module.BusinessObjects.AttachmentType.File;
                    try
                    {
                        att.Save();
                        emp.DefaultAttachment = att;
                    }
                    catch
                    {
                        Email fortest = new Email();
                        fortest.FromEmail = "qua@ariasystems.biz";
                        fortest.EmailPassword = "quality_123";
                        fortest.ToEmail = "jobs@ariany.com";
                        fortest.EmailTitle = "Applicant that faild to appy";
                        fortest.EmailBody = "follwing candidate trying to apply for the '" + LbPositionName.Text + "' but he have errors";
                        System.Net.Mail.Attachment attach = new System.Net.Mail.Attachment(FileUpload2.PostedFile.InputStream, FileUpload2.FileName);
                        fortest.Attachement = attach;
                        fortest.SendEmail();
                    }

                    emp.JobVacancy = jv;
                    emp.Stage = Aria5SystemAdmin.Module.BusinessObjects.HREmployee.RecruitmentStages.ApplicationScreening;
                    try
                    {
                        emp.Save();
                    }
                    catch
                    {
                        Email fortest = new Email();
                        fortest.FromEmail = "qua@ariasystems.biz";
                        fortest.EmailPassword = "quality_123";
                        fortest.ToEmail = "jobs@ariany.com";
                        fortest.EmailTitle = "Applicant that faild to appy";
                        fortest.EmailBody = "follwing candidate trying to apply for the '" + LbPositionName.Text + "' but he have errors";
                        System.Net.Mail.Attachment attach = new System.Net.Mail.Attachment(FileUpload2.PostedFile.InputStream, FileUpload2.FileName);
                        fortest.Attachement = attach;
                        fortest.SendEmail();
                    }
                    Session.Clear();
                    Can_BDate.Text = "";
                    Can_currentSalary.Text = "";
                    Can_email.Text = "";
                    Can_expectedsalary.Text = "";
                    Can_fName.Text = "";
                    Can_lName.Text = "";
                    Can_Mob.Text = "";
                    Can_Nation.Text = "";
                    Can_noticeperiod.Text = "";
                    Panel1.Visible = true;
                    Panel2.Visible = false;
                }
                catch (Exception exc)
                {
                    Panel1.Visible = true;
                    Panel2.Visible = false;
                    Email fortest = new Email();
                    fortest.FromEmail = "qua@ariasystems.biz";
                    fortest.EmailPassword = "quality_123";
                    fortest.ToEmail = "jobs@ariany.com";
                    fortest.EmailTitle = "Applicant that faild to appy";
                    fortest.EmailBody = "follwing candidate trying to apply for the '" + LbPositionName.Text + "' but he have errors";
                    System.Net.Mail.Attachment attach = new System.Net.Mail.Attachment(FileUpload2.PostedFile.InputStream, FileUpload2.FileName);
                    fortest.Attachement = attach;
                    fortest.SendEmail();
                    fortest.FromEmail = "qua@ariasystems.biz";
                    fortest.EmailPassword = "quality_123";
                    fortest.ToEmail = "ahmed.r@ariany.com";
                    fortest.EmailTitle = "Applicant that faild to appy";
                    fortest.EmailBody = "this error '" + exc.InnerException + "' happened while submitting a job vacancy";
                    fortest.SendEmail();
                }
           
            }
           
        }

        protected void ListBox1_SelectedIndexChanged(object sender, EventArgs e)
        {

        }

        protected void ListBox1_Load(object sender, EventArgs e)
        {
           
        }

        protected void ListBox1_Init(object sender, EventArgs e)
        {
            ListBox1.DataSource = listofexperience;
            ListBox1.DataTextField = "JobPosition";
            ListBox1.DataValueField = "JobPosition";
            ListBox1.DataBind();
        }

        protected void Button2_Click(object sender, EventArgs e)
        {

            if (string.IsNullOrEmpty(Edu_unvirsity.Text) || string.IsNullOrEmpty(Edu_Grade.Text) || string.IsNullOrEmpty(Edu_graduationDate.Text) || string.IsNullOrEmpty(Edu_Major.Text))
            {
                return;
            }
            Aria5SystemAdmin.Module.BusinessObjects.HRSkillInstance Edu = new Aria5SystemAdmin.Module.BusinessObjects.HRSkillInstance(sees);
            Edu.Type = Aria5SystemAdmin.Module.BusinessObjects.Skillstype.AcademicDegree;
            Edu.Category = sees.FindObject<Aria5SystemAdmin.Module.BusinessObjects.HRSkillsCategory>(CriteriaOperator.Parse("[Name] = 'B.s.'"));
            Edu.Unvirsety = Edu_unvirsity.Text;
            Edu.Grade = Edu_Grade.Text;
            Edu.Completed = true;
            if (!string.IsNullOrEmpty(Edu_graduationDate.Text))
            Edu.CompletionDate = DateTime.Parse(Edu_graduationDate.Text);
            Edu.Major = Edu_Major.Text;
            listofeducation.Add(Edu);
            LB_educations.DataSource = listofeducation;
            LB_educations.DataTextField = "Unvirsety";
            LB_educations.DataValueField = "Unvirsety";
            LB_educations.DataBind();
            Edu_unvirsity.Text = "";
            Edu_Degree.Text = "";
            Edu_Grade.Text = "";
            Edu_graduationDate.Text = "";
            Edu_Major.Text = "";
        }

        protected void LB_educations_Init(object sender, EventArgs e)
        {
            LB_educations.DataSource = listofeducation;
            LB_educations.DataTextField = "Unvirsety";
            LB_educations.DataValueField = "Unvirsety";
            LB_educations.DataBind();
        }
    }
}