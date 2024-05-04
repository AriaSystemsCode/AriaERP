using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using DevExpress.ExpressApp.DC;
using DevExpress.Persistent.Base;
using DevExpress.Persistent.BaseImpl;
using DevExpress.Xpo;
using DevExpress.Data.Filtering;
namespace Aria5SystemAdmin.Module.BusinessObjects
{
  [DefaultClassOptions]
  [XafDefaultProperty("Name")]
  [RelatedEntity("Aria5-SystemAdmin-Account")]

  [MapInheritance(MapInheritanceType.ParentTable)]
  public partial class Account : Business
  {
    private System.String _accountCode;
    private System.String _dBSchema;
    public Account(DevExpress.Xpo.Session session)
      : base(session)
    {
    }
    protected override void OnSaving()
    {
      if (Session.IsNewObject(this) == true && IsLoading == false)
      {
        // add same record in Sys_Client
        string cid = Id.TrimEnd();
        if (cid.Length > 5)
        {
          cid = cid.Substring(5);
        }
        ;
        string cname = Name.TrimEnd();
        if (cname.Length > 50)
        {
          cname = cname.Substring(50);
        }
        ;
        AccountCode = GenerateAccountCode();
        //SARA.N,1 [Tracking# + Aria5-DevExpress-Account]_Programming, 2-08-2015 [START]
        //if (this != null & this is Account)
        //{
        //  AriaSecuritySystemRole admin = new AriaSecuritySystemRole(Session);
        //  admin.Account = this;
        //  admin.Name = "Administrator";
        //  admin.IsAdministrative = true;
        //  admin.Save();
        //  AriaSecuritySystemUser oneTouchAwaySupportUser = new AriaSecuritySystemUser(Session);
        //  oneTouchAwaySupportUser.Account = this;
        //  oneTouchAwaySupportUser.UserName = AccountCode + "_1TouchAway";
        //  oneTouchAwaySupportUser.SetPassword("1TouchAway");
        //  oneTouchAwaySupportUser.Roles.Add(admin);
        //  oneTouchAwaySupportUser.Save();
        //}
        //SARA.N,1 [Tracking# + Aria5-DevExpress-Account]_Programming, 2-08-2015 [END]
        //Session.ExecuteQuery("Insert into Sys_Client( CClientID, CClientName) values ('" + cid + "','" + cname + "')");
      }
      if (Session.IsNewObject(this) == false && IsLoading == false)
      {
        // edit same record in Sys_Client
        string cid = Id.TrimEnd();
        if (cid.Length > 5)
        {
          cid = cid.Substring(5);
        }
        ;
        string cname = Name.TrimEnd();
        if (cname.Length > 50)
        {
          cname = cname.Substring(50);
        }
        ;
        //Session.ExecuteQuery("Update Sys_Client Set CClientName='" + cname + "' Where CClientID = '" + cid + "'");
      }
      base.OnSaving();
    }
    [DevExpress.Xpo.AssociationAttribute("AccountDevices-Account")]
    public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.AccountDevice> AccountDevices
    {
      get
      {
        return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.AccountDevice>("AccountDevices");
      }
    }
      //ATA 18/5/2016 add tickets to it's account [start]
    [DevExpress.Xpo.AssociationAttribute("TicketAHT-Account")]
    public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.TicketAHT> Tickets
    {
        get
        {
            return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.TicketAHT>("Tickets");
        }
    }
    //ATA 18/5/2016 add tickets to it's account [end]

    [DevExpress.Xpo.AssociationAttribute("ConfigurationItems-Account")]
    public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.ConfigurationItem> ConfigurationItems
    {
      get
      {
        return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.ConfigurationItem>("ConfigurationItems");
      }
    }
    public string GenerateAccountCode()
    {
      // Check if schema name empty
      if (!string.IsNullOrWhiteSpace(Name))
      {
        // initalized account code to name
        string accountCode = Name.ToUpper();
        // remove voiles from accountcode
        //char[] charsToTrim = { ' ', 'i', 'o', 'u', 'e', 'I', 'y', 'O', 'U', 'E', 'Y' };


        // New 1-11-2016 remove Sepcial Charcters 
          //SARA 1/11/2016
        //char[] charsToTrim = { ' ', 'i', 'o', 'u', 'e', 'I', 'y', 'O', 'U', 'E', 'Y' };
        char[] charsToTrim = { ' ', 'i', 'o', 'u', 'e', 'I', 'y', 'O', 'U', 'E', 'Y', '.', '@', '$', '%', '^', '&', '*', '!', '[', ']', '(', ')', '/', '~', '|', '\\' };   
       //SARA 1/11/2016
          foreach (char i in charsToTrim)
        {
          accountCode = accountCode.Replace(i.ToString(), "");
        }
        // If the account code all char is voiles set it to name
        if (accountCode.Length == 0)
        {
          accountCode = Name.ToUpper().Replace(" ", "");
        }
        // If account code greater than 6 strim it to 6 char
        if (accountCode.Length > 6)
        {
          accountCode = accountCode.Substring(0, 6);
        }
        // Get unique account code
        int count = 1;
        while (true)
        {
          // Suggest account code value
          string suggestedAccountCode = accountCode + count.ToString().PadLeft(2, '0');
          // Check if suggest account code value in databadse 
          if (Session.FindObject<Account>(CriteriaOperator.Parse("AccountCode='" + suggestedAccountCode + "'")) == null)
          {
            return suggestedAccountCode;
          }
          else
          {
            // Increment counter by one
            count++;
          }
        }
      }
      else
      {
        return "";
      }
    }
    [DevExpress.Persistent.Base.ImmediatePostDataAttribute]
    public System.String DBSchema
    {
      get
      {
        return _dBSchema;
      }
      set
      {
        SetPropertyValue("DBSchema", ref _dBSchema, value);
        //_accountCode = GenerateAccountCode();
      }
    }
    [DevExpress.Persistent.Base.ImmediatePostDataAttribute]
    public System.String AccountCode
    {
      get
      {
        return _accountCode;
      }
      set
      {
        // _accountCode = GenerateAccountCode();
        SetPropertyValue("AccountCode", ref _accountCode, value);
      }
    }
    [DevExpress.Xpo.AssociationAttribute("ActivationKeies-Account")]
    public XPCollection<Aria5SystemAdmin.Module.BusinessObjects.ActivationKey> ActivationKeies
    {
      get
      {
        return GetCollection<Aria5SystemAdmin.Module.BusinessObjects.ActivationKey>("ActivationKeies");
      }
    }
  }
  public enum Status
  {
    Active = 0,
    Hold = 1,
    Canceled = 2
  }
}
