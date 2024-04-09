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
	public class Postage
	{
		private string mailServiceField;

		private string zoneField;

		private string intraBMCField;

		private string pricingField;

		private decimal totalAmountField;

		public string IntraBMC
		{
			get
			{
				return this.intraBMCField;
			}
			set
			{
				this.intraBMCField = value;
			}
		}

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

		public string Pricing
		{
			get
			{
				return this.pricingField;
			}
			set
			{
				this.pricingField = value;
			}
		}

		[XmlAttribute]
		public decimal TotalAmount
		{
			get
			{
				return this.totalAmountField;
			}
			set
			{
				this.totalAmountField = value;
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

		public Postage()
		{
		}
	}
}