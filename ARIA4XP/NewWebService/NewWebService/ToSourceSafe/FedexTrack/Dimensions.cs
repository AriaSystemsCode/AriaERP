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
	public class Dimensions
	{
		private string lengthField;

		private string widthField;

		private string heightField;

		private LinearUnits unitsField;

		private bool unitsFieldSpecified;

		[XmlElement(DataType="nonNegativeInteger")]
		public string Height
		{
			get
			{
				return this.heightField;
			}
			set
			{
				this.heightField = value;
			}
		}

		[XmlElement(DataType="nonNegativeInteger")]
		public string Length
		{
			get
			{
				return this.lengthField;
			}
			set
			{
				this.lengthField = value;
			}
		}

		public LinearUnits Units
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

		[XmlElement(DataType="nonNegativeInteger")]
		public string Width
		{
			get
			{
				return this.widthField;
			}
			set
			{
				this.widthField = value;
			}
		}

		public Dimensions()
		{
		}
	}
}