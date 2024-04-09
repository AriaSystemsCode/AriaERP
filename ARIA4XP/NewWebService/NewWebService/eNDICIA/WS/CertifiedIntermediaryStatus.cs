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
	public class CertifiedIntermediaryStatus
	{
		private string accountIDField;

		private int serialNumberField;

		private decimal postageBalanceField;

		private decimal ascendingBalanceField;

		private string accountStatusField;

		private string deviceIDField;

		public string AccountID
		{
			get
			{
				return this.accountIDField;
			}
			set
			{
				this.accountIDField = value;
			}
		}

		public string AccountStatus
		{
			get
			{
				return this.accountStatusField;
			}
			set
			{
				this.accountStatusField = value;
			}
		}

		public decimal AscendingBalance
		{
			get
			{
				return this.ascendingBalanceField;
			}
			set
			{
				this.ascendingBalanceField = value;
			}
		}

		public string DeviceID
		{
			get
			{
				return this.deviceIDField;
			}
			set
			{
				this.deviceIDField = value;
			}
		}

		public decimal PostageBalance
		{
			get
			{
				return this.postageBalanceField;
			}
			set
			{
				this.postageBalanceField = value;
			}
		}

		public int SerialNumber
		{
			get
			{
				return this.serialNumberField;
			}
			set
			{
				this.serialNumberField = value;
			}
		}

		public CertifiedIntermediaryStatus()
		{
		}
	}
}