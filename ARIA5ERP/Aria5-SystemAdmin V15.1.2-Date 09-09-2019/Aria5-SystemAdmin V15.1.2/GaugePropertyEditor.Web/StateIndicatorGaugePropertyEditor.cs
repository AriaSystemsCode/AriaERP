using System;
using System.Drawing;
using DevExpress.Web.ASPxGauges;
using DevExpress.XtraGauges.Base;
using DevExpress.ExpressApp.Editors;
using DevExpress.XtraGauges.Core.Model;
using DevExpress.Web.ASPxGauges.Gauges;
using DevExpress.Web.ASPxGauges.Gauges.State;

namespace GaugePropertyEditor.Web {
    [PropertyEditor(typeof(Enum), "StateIndicatorGaugePropertyEditor", false)]
    public class StateIndicatorGaugePropertyEditor : WebGaugePropertyEditorBase {
        public StateIndicatorGaugePropertyEditor(Type objectType, DevExpress.ExpressApp.Model.IModelMemberViewItem info)
            : base(objectType, info) {
        }
        public override void SetupGaugeCore(ASPxGaugeControl gaugeContainer) {
            StateIndicatorGauge siGauge = (StateIndicatorGauge)gaugeContainer.AddGauge(GaugeType.StateIndicator);
            StateIndicatorComponent stateIndicator = siGauge.AddIndicator();
            stateIndicator.BeginUpdate();
            BindableComponent = stateIndicator;
            StateIndicatorShapeType[] shapes = new StateIndicatorShapeType[] {
                        StateIndicatorShapeType.TrafficLight2,
                        StateIndicatorShapeType.TrafficLight3,
                        StateIndicatorShapeType.TrafficLight4,
                    };
            stateIndicator.States.Clear();
            foreach (StateIndicatorShapeType shape in shapes) {
                IndicatorStateWeb state = new IndicatorStateWeb(shape);
                stateIndicator.States.Add(state);
            }
            stateIndicator.Size = new SizeF(100, 200);
        }
        protected override object GetControlValueCore() {
            return ((StateIndicatorComponent)BindableComponent).StateIndex;
        }
        protected override void ReadValueCore() {
            ((StateIndicatorComponent)BindableComponent).StateIndex = Convert.ToInt32(PropertyValue);
        }
    }
}
