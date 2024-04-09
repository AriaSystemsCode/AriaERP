using System;
using System.CodeDom.Compiler;
using System.ComponentModel;
using System.Diagnostics;
using System.Xml.Serialization;

namespace Endicia.WS
{
	[DebuggerStepThrough]
	[DesignerCategory("code")]
	[GeneratedCode("System.Xml", "4.0.30319.1")]
	[Serializable]
	[XmlType(Namespace="www.envmgr.com/LabelService")]
	public class ResponseOptions
	{
		private string postagePriceField;

		[XmlAttribute]
		public string PostagePrice
		{
			get
			{
				return this.postagePriceField;
			}
			set
			{
				this.postagePriceField = value;
			}
		}

		public ResponseOptions()
		{
		}
	}
}