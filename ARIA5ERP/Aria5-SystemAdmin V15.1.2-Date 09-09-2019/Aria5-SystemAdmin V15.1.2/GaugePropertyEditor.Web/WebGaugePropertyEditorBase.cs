using System;
using System.Drawing;
using DevExpress.Web.ASPxGauges;
using DevExpress.ExpressApp.Web.Editors;

namespace GaugePropertyEditor.Web {
    public abstract class WebGaugePropertyEditorBase : WebPropertyEditor {
        private object bindableComponentCore;
        private object circulre;
        protected WebGaugePropertyEditorBase(Type objectType, DevExpress.ExpressApp.Model.IModelMemberViewItem info)
            : base(objectType, info) {
        }
        protected override object CreateControlCore() {
            ASPxGaugeControl gaugeControl = new ASPxGaugeControl();
            gaugeControl.BackColor = Color.Transparent;
            SetupGaugeCore(gaugeControl);
            return gaugeControl;
        }
        protected override void OnControlCreated() {
            base.OnControlCreated();
            UpdateEditorState();
        }
        public object BindableComponent {
            get { return bindableComponentCore; }
            set { bindableComponentCore = value; }
        }
        public object Circulre
        {
            get { return circulre; }
            set { circulre = value; }
        }
        public abstract void SetupGaugeCore(ASPxGaugeControl gaugeContainer);
        protected override System.Web.UI.WebControls.WebControl CreateEditModeControlCore() { return null; }
        protected override void ReadEditModeValueCore() { }
    }
}