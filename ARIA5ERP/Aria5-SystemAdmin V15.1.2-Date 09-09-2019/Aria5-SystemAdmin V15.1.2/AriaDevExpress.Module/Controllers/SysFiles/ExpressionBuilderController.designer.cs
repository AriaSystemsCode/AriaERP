using AriaDevExpress.Module.BusinessObjects.SysFiles;
namespace AriaDevExpress.Module.Controllers.SysFiles
{
    partial class ExpressionBuilderController
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
            this.ExpressionBuilderShowAction = new DevExpress.ExpressApp.Actions.PopupWindowShowAction(this.components);
            // 
            // ExpressionBuilderShowAction
            // 
            this.ExpressionBuilderShowAction.AcceptButtonCaption = null;
            this.ExpressionBuilderShowAction.ActionMeaning = DevExpress.ExpressApp.Actions.ActionMeaning.Accept;
            this.ExpressionBuilderShowAction.CancelButtonCaption = null;
            this.ExpressionBuilderShowAction.Caption = "Expression Builder";
            this.ExpressionBuilderShowAction.ConfirmationMessage = null;
            this.ExpressionBuilderShowAction.Id = "ExpressionBuilderShowAction";
            this.ExpressionBuilderShowAction.ImageName = null;
            this.ExpressionBuilderShowAction.Shortcut = null;
            this.ExpressionBuilderShowAction.Tag = null;
            this.ExpressionBuilderShowAction.TargetObjectsCriteria = null;
            this.ExpressionBuilderShowAction.TargetObjectType = typeof(Index);
            this.ExpressionBuilderShowAction.TargetViewId = null;
            this.ExpressionBuilderShowAction.TargetViewType = DevExpress.ExpressApp.ViewType.DetailView;
            this.ExpressionBuilderShowAction.ToolTip = null;
            this.ExpressionBuilderShowAction.TypeOfView = typeof(DevExpress.ExpressApp.DetailView);
            this.ExpressionBuilderShowAction.CustomizePopupWindowParams += new DevExpress.ExpressApp.Actions.CustomizePopupWindowParamsEventHandler(this.ExpressionBuilderShowAction_CustomizePopupWindowParams);
            this.ExpressionBuilderShowAction.Execute += new DevExpress.ExpressApp.Actions.PopupWindowShowActionExecuteEventHandler(this.ExpressionBuilderShowAction_Execute);
            // 
            // ExpressionBuilderController
            // 
            this.TargetObjectType = typeof(Index);
            this.TargetViewType = DevExpress.ExpressApp.ViewType.DetailView;
            this.TypeOfView = typeof(DevExpress.ExpressApp.DetailView);
            this.AfterConstruction += new System.EventHandler(this.ExpressionBuilderController_AfterConstruction);

        }

        #endregion

        private DevExpress.ExpressApp.Actions.PopupWindowShowAction ExpressionBuilderShowAction;


    }
}
