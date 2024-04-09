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
	public class LabelRequestResponse
	{
		private int statusField;

		private string errorMessageField;

		private string base64LabelImageField;

		private ImageSet labelField;

		private ImageSet customsFormField;

		private string pICField;

		private string customsNumberField;

		private string trackingNumberField;

		private decimal finalPostageField;

		private int transactionIDField;

		private string transactionDateTimeField;

		private string postmarkDateField;

		private decimal postageBalanceField;

		private PostagePrice postagePriceField;

		public string Base64LabelImage
		{
			get
			{
				return this.base64LabelImageField;
			}
			set
			{
				this.base64LabelImageField = value;
			}
		}

		public ImageSet CustomsForm
		{
			get
			{
				return this.customsFormField;
			}
			set
			{
				this.customsFormField = value;
			}
		}

		public string CustomsNumber
		{
			get
			{
				return this.customsNumberField;
			}
			set
			{
				this.customsNumberField = value;
			}
		}

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

		public decimal FinalPostage
		{
			get
			{
				return this.finalPostageField;
			}
			set
			{
				this.finalPostageField = value;
			}
		}

		public ImageSet Label
		{
			get
			{
				return this.labelField;
			}
			set
			{
				this.labelField = value;
			}
		}

		public string PIC
		{
			get
			{
				return this.pICField;
			}
			set
			{
				this.pICField = value;
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

		public PostagePrice PostagePrice
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

		public string PostmarkDate
		{
			get
			{
				return this.postmarkDateField;
			}
			set
			{
				this.postmarkDateField = value;
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

		public string TrackingNumber
		{
			get
			{
				return this.trackingNumberField;
			}
			set
			{
				this.trackingNumberField = value;
			}
		}

		public string TransactionDateTime
		{
			get
			{
				return this.transactionDateTimeField;
			}
			set
			{
				this.transactionDateTimeField = value;
			}
		}

		public int TransactionID
		{
			get
			{
				return this.transactionIDField;
			}
			set
			{
				this.transactionIDField = value;
			}
		}

		public LabelRequestResponse()
		{
		}
	}
}