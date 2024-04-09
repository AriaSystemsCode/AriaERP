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
	public class PostageRateResponse
	{
		private int statusField;

		private string errorMessageField;

		private string zoneField;

		private PostageRate[] postageField;

		private PostagePrice[] postagePriceField;

		public string ErrorMessage
		{
			get
			{
				return this.errorMessageField;
			}
			set
			{
				this.errorMessageField = value;
			}
		}

		[XmlElement("Postage")]
		public PostageRate[] Postage
		{
			get
			{
				return this.postageField;
			}
			set
			{
				this.postageField = value;
			}
		}

		[XmlElement("PostagePrice")]
		public PostagePrice[] PostagePrice
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

		public int Status
		{
			get
			{
				return this.statusField;
			}
			set
			{
				this.statusField = value;
			}
		}

		public string Zone
		{
			get
			{
				return this.zoneField;
			}
			set
			{
				this.zoneField = value;
			}
		}

		public PostageRateResponse()
		{
		}
	}
}