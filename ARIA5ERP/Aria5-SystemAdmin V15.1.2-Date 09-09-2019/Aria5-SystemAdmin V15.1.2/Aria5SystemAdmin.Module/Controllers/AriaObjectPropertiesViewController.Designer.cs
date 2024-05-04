namespace Aria5SystemAdmin.Module.Controllers
{
    partial class AriaObjectPropertiesViewController
    {
        /// <summary>
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary> 
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Component Designer generated code

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            this.components = new System.ComponentModel.Container();
            this.LinkAriaObjectToAriaObjectsAsProperty = new DevExpress.ExpressApp.Actions.PopupWindowShowAction(this.components);
            // 
            // LinkAriaObjectToAriaObjectsAsProperty
            // 
            this.LinkAriaObjectToAriaObjectsAsProperty.AcceptButtonCaption = null;
            this.LinkAriaObjectToAriaObjectsAsProperty.CancelButtonCaption = null;
            this.LinkAriaObjectToAriaObjectsAsProperty.Caption = "Link Aria Object To Aria Objects As Property";
            this.LinkAriaObjectToAriaObjectsAsProperty.ConfirmationMessage = null;
            this.LinkAriaObjectToAriaObjectsAsProperty.Id = "LinkAriaObjectToAriaObjectsAsProperty";
            this.LinkAriaObjectToAriaObjectsAsProperty.ImageName = null;
            this.LinkAriaObjectToAriaObjectsAsProperty.Shortcut = null;
            this.LinkAriaObjectToAriaObjectsAsProperty.Tag = null;
            this.LinkAriaObjectToAriaObjectsAsProperty.TargetObjectsCriteria = null;
            this.LinkAriaObjectToAriaObjectsAsProperty.TargetViewId = null;
            this.LinkAriaObjectToAriaObjectsAsProperty.ToolTip = null;
            this.LinkAriaObjectToAriaObjectsAsProperty.TypeOfView = null;
            this.LinkAriaObjectToAriaObjectsAsProperty.CustomizePopupWindowParams += new DevExpress.ExpressApp.Actions.CustomizePopupWindowParamsEventHandler(this.ShowNotesAction_CustomizePopupWindowParams);
            this.LinkAriaObjectToAriaObjectsAsProperty.Execute += new DevExpress.ExpressApp.Actions.PopupWindowShowActionExecuteEventHandler(this.AriaObjectProperties_Execute);
            // 
            // AriaObjectPropertiesViewController
            // 
            this.TargetObjectType = typeof(Aria5SystemAdmin.Module.BusinessObjects.AriaObject);
            this.TargetViewType = DevExpress.ExpressApp.ViewType.DetailView;
            this.TypeOfView = typeof(DevExpress.ExpressApp.DetailView);

        }

        #endregion

        private DevExpress.ExpressApp.Actions.PopupWindowShowAction LinkAriaObjectToAriaObjectsAsProperty;
    }
}
