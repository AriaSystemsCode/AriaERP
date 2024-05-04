using DevExpress.Persistent.Base;
using DevExpress.Xpo;
using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Text;

namespace Aria5SystemAdmin.Module.BusinessObjects
{
    public partial class ConfigurationItem
    {
        public List<Application_T> PreRequesiteAppList;
        [NonPersistent]
        [Browsable(false)]
        // [VisibleInListView(false)]
        //[VisibleInDetailView(false)]
        public bool IsAriaAccount = false;
        //[NonPersistent]
        //[Browsable(false)]
        //public bool IsAriaAccount
        //{
        //    set
        //    {
        //        _isAriaAccount = value;
        //    }
        //    get
        //    {
        //        return _isAriaAccount;
        //    }
        //}
         protected override void OnLoading()
         {
             base.OnLoading();

             IsAriaAccount = false;
             //Mina.B 2015-09-14 Fix bug, check if current account is not equal null [Begin]
             //if (AriaSecuritySystemUser.CurrentAccount.Id.ToUpper().Trim() == "ARIA")
             if (AriaSecuritySystemUser.CurrentAccount != null && AriaSecuritySystemUser.CurrentAccount.Id.ToUpper().Trim() == "ARIA")
             //Mina.B 2015-09-14 Fix bug, check if current account is not equal null [Begin]
             {
                 IsAriaAccount = true;
             }
         }
        protected override void OnLoaded()
        {
            base.OnLoaded();
           
            PreRequesiteAppList = new List<Application_T>();
            List<Application_T> apps = new List<Application_T>();
            foreach (Application_T app in Application.PreRequiste_Applications)
            {
                apps.Add(app);
            }
            GetPreRequisteApplications(apps);

            
        }
       
        private void GetPreRequisteApplications(List<Application_T> apps)
        {
            foreach (Application_T app in apps)
            {
                PreRequesiteAppList.Add(app);
                if (app.PreRequiste_Applications.Count > 0)
                {
                    List<Application_T> preRequiste_Applications = new List<Application_T>();
                    foreach (Application_T preRequiste_App in app.PreRequiste_Applications)
                    {
                        if (!PreRequesiteAppList.Contains(preRequiste_App))
                        {
                            preRequiste_Applications.Add(preRequiste_App);
                        }
                    }
                    if (preRequiste_Applications.Count > 0)
                    {
                        GetPreRequisteApplications(preRequiste_Applications);
                    }
                }
            }
        }
        protected override void OnSaving()
        {
            //Sara.N 2015-18-08 ConfigurationItem - Add division for account [Start]

            //Division division = new Division(Session);
            //if (Account != null)
            //{
            //    division.Guide = Account.Guide;
            //    division.Business = Account;
            //    if (VersionType == Version_Types.Demo)
            //    {
            //        division.Name = Account.Name + "_Demo Data";
            //        //Sara.N 2015-18-08 change default division from trial to demo [Start]
            //        if (!string.IsNullOrWhiteSpace(Account.AccountCode))
            //        {
            //            division.DBSchema = Account.AccountCode + "_Demo";
            //        }
            //        else
            //        {
            //            division.DBSchema = Account.Id + "_Demo";
            //        }


            //        //Sara.N 2015-24-08 change default division from trial to demo [End]

            //    }
            //    else if (VersionType == Version_Types.Trial)
            //    {
            //        division.Name = Account.Name + "_Trial Data";

            //        //Sara.N 2015-18-08 change default division from trial to demo [Start]

            //        if (!string.IsNullOrWhiteSpace(Account.AccountCode))
            //        {
            //            division.DBSchema = Account.AccountCode + "_Demo";
            //        }
            //        else
            //        {
            //            division.DBSchema = Account.Id + "_Demo";
            //        }

            //        //Sara.N 2015-24-08 change default division from trial to demo [End]

            //    }
            //    division.Industry = Account.Industry;
            //    division.Status = Account.Status;
            //    division.StatusId = Account.StatusId;
            //    division.CategoryId = Account.CategoryId;
            //    division.ClassificationId = Account.ClassificationId;
            //    division.EMailAddress = Account.EMailAddress;
            //    division.EntityCategory = Account.EntityCategory;
            //    division.EntityClassification = Account.EntityClassification;
            //    division.EntityType = Account.EntityType;
            //    division.EnteredBy = Account.EnteredBy;
            //    division.EnteredDate = Account.EnteredDate;
            //    division.ExtraData = Account.ExtraData;
            //    division.InternalContactNo = Account.InternalContactNo;
            //    division.IMAGE = Account.IMAGE;
            //    division.Id = Account.Id;
            //    division.Guide = Account.Guide;
            //    division.LanguageCode = Account.LanguageCode;
            //    division.Language = Account.Language;
            //    division.Notes = Account.Notes;
            //    division.Priority = Account.Priority;
            //    division.TypeId = Account.TypeId;
            //    division.WebPageAddress = Account.WebPageAddress;
            //    division.ParentContact = Account.ParentContact;
            //    division.Currency = Account.Currency;
            //    division.CurrencyCode = Account.CurrencyCode;
            //    division.CurrentBalance = Account.CurrentBalance;
            //    division.CreditLimit = division.CreditLimit;
            //    division.Description = Account.Description;





            //    foreach (EntityRelationship relation in Account.News)
            //    {
            //        EntityRelationship newRelation = new EntityRelationship(Session);
            //        newRelation.EntityType = relation.EntityType;
            //        newRelation.RelatedEntity = relation.RelatedEntity;
            //        newRelation.RelatedEntityType = relation.RelatedEntityType;
            //        newRelation.Entity = division;
            //        newRelation.Save();
            //    }
            //    division.Save();
            //    Account.Divisions.Add(division);
            //    Account.Save();
            //}
            ////Sara.N 2015-18-08 ConfigurationItem - Add division for account [End]
            base.OnSaving();
        }



    }
}
