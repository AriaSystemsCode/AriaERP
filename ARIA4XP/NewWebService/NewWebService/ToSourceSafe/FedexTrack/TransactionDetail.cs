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
	public class TransactionDetail
	{
		private string customerTransactionIdField;

		private Localization localizationField;

		public string CustomerTransactionId
		{
			get
			{
				return this.customerTransactionIdField;
			}
			set
			{
				this.customerTransactionIdField = value;
			}
		}

		public Localization Localization
		{
			get
			{
				return this.localizationField;
			}
			set
			{
				this.localizationField = value;
			}
		}

		public TransactionDetail()
		{
		}
	}
}