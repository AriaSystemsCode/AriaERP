using System;
using System.ComponentModel;
using System.Collections.Generic;
using System.Diagnostics;
using System.Text;

using DevExpress.ExpressApp;
using DevExpress.ExpressApp.Actions;
using DevExpress.Persistent.Base;
using DevExpress.ExpressApp.ConditionalAppearance;
using DevExpress.Xpo.DB.Helpers;
using AriaDevExpress.Module.BusinessObjects.SysFiles;


namespace AriaDevExpress.Module.Controllers.SysFiles
{

    public partial class BrowseFieldsController : ViewController
    {
        public BrowseFieldsController()
        {
            InitializeComponent();
            RegisterActions(components);
            // this.Active.SetItemValue("Disable", EnableReason);
        }

        //public bool EnableReason
        //{
        //    get
        //    {
        //        bool result = false;
        //        if (this.View != null && this.View is DetailView)
        //            result = ((DetailView)this.View).ViewEditMode == DevExpress.ExpressApp.Editors.ViewEditMode.Edit;
        //        return result;
        //    }
        //}
        File CurrentFile;
        string CurrentBrowseFields;
        private void BrowseFieldsShowAction_CustomizePopupWindowParams(object sender, CustomizePopupWindowParamsEventArgs e)
        {
            IObjectSpace objSpace = Application.CreateObjectSpace();
            BrowseFieldsEmptyClass browseFields = new BrowseFieldsEmptyClass();// objSpace.CreateObject<BrowseFieldsEmptyClass>();
            CurrentFile = View.CurrentObject as File;
            CurrentBrowseFields = CurrentFile.UserBrowseFileds;
            browseFields.SetFile(CurrentFile);
            DetailView targetView = Application.CreateDetailView(objSpace, browseFields);
            e.View = targetView;
        }

        private void BrowseFieldsShowAction_Execute(object sender, PopupWindowShowActionExecuteEventArgs e)
        {
            if (((DetailView)View).ViewEditMode == DevExpress.ExpressApp.Editors.ViewEditMode.View)
            {
                CurrentFile.Save();
                CurrentFile.Session.CommitTransaction();
            }
        }
    }
}
