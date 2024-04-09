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
	public class PostagePrice
	{
		private Postage postageField;

		private Fees feesField;

		private decimal totalAmountField;

		public Fees Fees
		{
			get
			{
				return this.feesField;
			}
			set
			{
				this.feesField = value;
			}
		}

		public Postage Postage
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

		public PostagePrice()
		{
		}
	}
}