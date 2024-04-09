using System;
using System.CodeDom.Compiler;
using System.ComponentModel;
using System.Diagnostics;
using System.Xml.Serialization;

namespace Fedex.FedexTrack
{
	[DebuggerStepThrough]
	[DesignerCategory("code")]
	[GeneratedCode("System.Xml", "4.0.30319.1")]
	[Serializable]
	[XmlType(Namespace="http://fedex.com/ws/track/v4")]
	public class Weight
	{
		private WeightUnits unitsField;

		private bool unitsFieldSpecified;

		private decimal valueField;

		private bool valueFieldSpecified;

		public WeightUnits Units
		{
			get
			{
				return this.unitsField;
			}
			set
			{
				this.unitsField = value;
			}
		}

		[XmlIgnore]
		public bool UnitsSpecified
		{
			get
			{
				return this.unitsFieldSpecified;
			}
			set
			{
				this.unitsFieldSpecified = value;
			}
		}

		public decimal Value
		{
			get
			{
				return this.valueField;
			}
			set
			{
				this.valueField = value;
			}
		}

		[XmlIgnore]
		public bool ValueSpecified
		{
			get
			{
				return this.valueFieldSpecified;
			}
			set
			{
				this.valueFieldSpecified = value;
			}
		}

		public Weight()
		{
		}
	}
}