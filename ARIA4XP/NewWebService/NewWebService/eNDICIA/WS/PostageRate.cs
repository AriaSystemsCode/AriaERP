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
	public class PostageRate
	{
		private string mailServiceField;

		private decimal rateField;

		public string MailService
		{
			get
			{
				return this.mailServiceField;
			}
			set
			{
				this.mailServiceField = value;
			}
		}

		public decimal Rate
		{
			get
			{
				return this.rateField;
			}
			set
			{
				this.rateField = value;
			}
		}

		public PostageRate()
		{
		}
	}
}