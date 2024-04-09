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
	public class SignatureProofOfDeliveryLetterRequest
	{
		private  WebAuthenticationDetail webAuthenticationDetailField;

		private  ClientDetail clientDetailField;

		private  TransactionDetail transactionDetailField;

		private VersionId versionField;

		private  QualifiedTrackingNumber qualifiedTrackingNumberField;

		private string additionalCommentsField;

		private SignatureProofOfDeliveryImageType letterFormatField;

		private bool letterFormatFieldSpecified;

		private ContactAndAddress consigneeField;

		public string AdditionalComments
		{
			get
			{
				return this.additionalCommentsField;
			}
			set
			{
				this.additionalCommentsField = value;
			}
		}

		public  ClientDetail ClientDetail
		{
			get
			{
				return this.clientDetailField;
			}
			set
			{
				this.clientDetailField = value;
			}
		}

		public ContactAndAddress Consignee
		{
			get
			{
				return this.consigneeField;
			}
			set
			{
				this.consigneeField = value;
			}
		}

		public SignatureProofOfDeliveryImageType LetterFormat
		{
			get
			{
				return this.letterFormatField;
			}
			set
			{
				this.letterFormatField = value;
			}
		}

		[XmlIgnore]
		public bool LetterFormatSpecified
		{
			get
			{
				return this.letterFormatFieldSpecified;
			}
			set
			{
				this.letterFormatFieldSpecified = value;
			}
		}

		public  QualifiedTrackingNumber QualifiedTrackingNumber
		{
			get
			{
				return this.qualifiedTrackingNumberField;
			}
			set
			{
				this.qualifiedTrackingNumberField = value;
			}
		}

		public  TransactionDetail TransactionDetail
		{
			get
			{
				return this.transactionDetailField;
			}
			set
			{
				this.transactionDetailField = value;
			}
		}

		public VersionId Version
		{
			get
			{
				return this.versionField;
			}
			set
			{
				this.versionField = value;
			}
		}

		public  WebAuthenticationDetail WebAuthenticationDetail
		{
			get
			{
				return this.webAuthenticationDetailField;
			}
			set
			{
				this.webAuthenticationDetailField = value;
			}
		}

		public SignatureProofOfDeliveryLetterRequest()
		{
		}
	}
}