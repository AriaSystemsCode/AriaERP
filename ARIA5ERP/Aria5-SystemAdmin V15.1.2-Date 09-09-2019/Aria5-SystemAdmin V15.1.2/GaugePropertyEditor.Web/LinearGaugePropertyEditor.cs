using System;
using DevExpress.Web.ASPxGauges;
using DevExpress.XtraGauges.Base;
using DevExpress.ExpressApp.Editors;
using DevExpress.XtraGauges.Core.Base;
using DevExpress.XtraGauges.Core.Model;
using DevExpress.Web.ASPxGauges.Gauges.Linear;

namespace GaugePropertyEditor.Web {
    [PropertyEditor(typeof(int), "LinearGaugePropertyEditor", false)]
    public class LinearGaugePropertyEditor: WebGaugePropertyEditorBase {
        public LinearGaugePropertyEditor(Type objectType, DevExpress.ExpressApp.Model.IModelMemberViewItem info)
            : base(objectType, info) {
        }
        public override void SetupGaugeCore(ASPxGaugeControl gaugeContainer) {
            LinearGauge linearGauge = (LinearGauge)gaugeContainer.AddGauge(GaugeType.Linear);
            // Add the default elements (a scale, background layer and level bar).
            linearGauge.AddDefaultElements();
            // Change the background layer's paint style.
            LinearScaleBackgroundLayer backgroundLinear = linearGauge.BackgroundLayers[0];
            backgroundLinear.ShapeType = BackgroundLayerShapeType.Linear_Style3;
            // Customize the scale's settings.
            LinearScaleComponent scaleLinear = linearGauge.Scales[0];
            BindableComponent = scaleLinear;
            scaleLinear.MinValue = 0;
            scaleLinear.MaxValue = 100;
            scaleLinear.Value = 0;
            scaleLinear.MajorTickCount = 6;
            scaleLinear.MajorTickmark.FormatString = "{0:F0}";
            scaleLinear.MajorTickmark.ShapeType = TickmarkShapeType.Linear_Style6_3;
            scaleLinear.MinorTickCount = 3;
            scaleLinear.MinorTickmark.ShapeType = TickmarkShapeType.Linear_Style5_2;
            // Shift tick marks to the right.
            scaleLinear.MajorTickmark.ShapeOffset = 5f;
            scaleLinear.MinorTickmark.ShapeOffset = 5f;
            // Change the levelBar's paint style.
            LinearScaleLevelComponent levelBar = linearGauge.Levels[0];
            levelBar.ShapeType = LevelShapeSetType.Style3;
            // Shift the background layer up and to the left.
            backgroundLinear.ScaleStartPos = new PointF2D(backgroundLinear.ScaleStartPos.X - 0.005f,
                backgroundLinear.ScaleStartPos.Y - 0.015f);
            backgroundLinear.ScaleEndPos = new PointF2D(backgroundLinear.ScaleEndPos.X - 0.005f,
                backgroundLinear.ScaleEndPos.Y);
        }
        protected override object GetControlValueCore() {
            return ((LinearScaleComponent)BindableComponent).Value;
        }
        protected override void ReadValueCore() {
            ((LinearScaleComponent)BindableComponent).Value = Convert.ToSingle(PropertyValue);
        }
    }
}
