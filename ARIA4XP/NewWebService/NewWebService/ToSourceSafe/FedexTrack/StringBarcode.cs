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
	public class StringBarcode
	{
		private StringBarcodeType typeField;

		private bool typeFieldSpecified;

		private string valueField;

		public StringBarcodeType Type
		{
			get
			{
				return this.typeField;
			}
			set
			{
				this.typeField = value;
			}
		}

		[XmlIgnore]
		public bool TypeSpecified
		{
			get
			{
				return this.typeFieldSpecified;
			}
			set
			{
				this.typeFieldSpecified = value;
			}
		}

		public string Value
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

		public StringBarcode()
		{
		}
	}
}