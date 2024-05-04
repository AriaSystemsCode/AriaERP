using System;
using DevExpress.Web.ASPxGauges;
using DevExpress.XtraGauges.Base;
using DevExpress.ExpressApp.Editors;
using DevExpress.XtraGauges.Core.Model;
using DevExpress.Web.ASPxGauges.Gauges.Circular;

namespace GaugePropertyEditor.Web {
    [PropertyEditor(typeof(Double), "CircularGaugePropertyEditor", false)]
    public class CircularGaugePropertyEditor: WebGaugePropertyEditorBase {
        public CircularGaugePropertyEditor(Type objectType, DevExpress.ExpressApp.Model.IModelMemberViewItem info)
            : base(objectType, info) {
        }
        public override void SetupGaugeCore(ASPxGaugeControl gaugeContainer) {
            CircularGauge circularGauge = (CircularGauge)gaugeContainer.AddGauge(GaugeType.Circular);
            // Add the default elements (a scale, background layer, needle and spindle cap).
            circularGauge.AddDefaultElements();
            // Change the background layer's paint style.
            ArcScaleBackgroundLayer backgroundCircular = circularGauge.BackgroundLayers[0];
            backgroundCircular.ShapeType = BackgroundLayerShapeType.CircularFull_Style14;
            
            Circulre = backgroundCircular;
            // Customize the scale's settings.
            ArcScaleComponent scaleCircular = circularGauge.Scales[0];
            BindableComponent = scaleCircular;
            scaleCircular.MinValue = 0;
            scaleCircular.MaxValue = 200;
            scaleCircular.Value = 0;
            scaleCircular.MajorTickCount = 6;
            scaleCircular.MajorTickmark.FormatString = "{0:F0}";
            scaleCircular.MajorTickmark.ShapeType = TickmarkShapeType.Circular_Style21_1;
            scaleCircular.MajorTickmark.ShapeOffset = -9;
            scaleCircular.MajorTickmark.AllowTickOverlap = true;
            scaleCircular.MinorTickCount = 3;
            scaleCircular.MinorTickmark.ShapeType = TickmarkShapeType.Circular_Style20_1;
            // Change the needle's paint style.
            ArcScaleNeedleComponent needle = circularGauge.Needles[0];
            needle.ShapeType = NeedleShapeType.CircularFull_Style3;

            gaugeContainer.Width = 150;
            gaugeContainer.Height = 150;
            gaugeContainer.AutoLayout = true;
        }
        protected override object GetControlValueCore() {
            return ((ArcScaleComponent)BindableComponent).Value;        
        }
        protected override void ReadValueCore() {
            ((ArcScaleComponent)BindableComponent).Value = Convert.ToSingle(PropertyValue);
            if (((ArcScaleComponent)BindableComponent).Value > 100)
            {
                ((ArcScaleBackgroundLayer)Circulre).ShapeType = BackgroundLayerShapeType.CircularFull_Style14;
            
            }
            else
            {
                ((ArcScaleBackgroundLayer)Circulre).ShapeType = BackgroundLayerShapeType.CircularFull_Style1;

            }
  

        }
    }
}
