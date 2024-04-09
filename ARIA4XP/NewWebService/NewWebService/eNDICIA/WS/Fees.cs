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
	public class Fees
	{
		private decimal certificateOfMailingField;

		private decimal certifiedMailField;

		private decimal collectOnDeliveryField;

		private decimal deliveryConfirmationField;

		private decimal electronicReturnReceiptField;

		private decimal insuredMailField;

		private decimal registeredMailField;

		private decimal restrictedDeliveryField;

		private decimal returnReceiptField;

		private decimal returnReceiptForMerchandiseField;

		private decimal signatureConfirmationField;

		private decimal specialHandlingField;

		private decimal totalAmountField;

		public decimal CertificateOfMailing
		{
			get
			{
				return this.certificateOfMailingField;
			}
			set
			{
				this.certificateOfMailingField = value;
			}
		}

		public decimal CertifiedMail
		{
			get
			{
				return this.certifiedMailField;
			}
			set
			{
				this.certifiedMailField = value;
			}
		}

		public decimal CollectOnDelivery
		{
			get
			{
				return this.collectOnDeliveryField;
			}
			set
			{
				this.collectOnDeliveryField = value;
			}
		}

		public decimal DeliveryConfirmation
		{
			get
			{
				return this.deliveryConfirmationField;
			}
			set
			{
				this.deliveryConfirmationField = value;
			}
		}

		public decimal ElectronicReturnReceipt
		{
			get
			{
				return this.electronicReturnReceiptField;
			}
			set
			{
				this.electronicReturnReceiptField = value;
			}
		}

		public decimal InsuredMail
		{
			get
			{
				return this.insuredMailField;
			}
			set
			{
				this.insuredMailField = value;
			}
		}

		public decimal RegisteredMail
		{
			get
			{
				return this.registeredMailField;
			}
			set
			{
				this.registeredMailField = value;
			}
		}

		public decimal RestrictedDelivery
		{
			get
			{
				return this.restrictedDeliveryField;
			}
			set
			{
				this.restrictedDeliveryField = value;
			}
		}

		public decimal ReturnReceipt
		{
			get
			{
				return this.returnReceiptField;
			}
			set
			{
				this.returnReceiptField = value;
			}
		}

		public decimal ReturnReceiptForMerchandise
		{
			get
			{
				return this.returnReceiptForMerchandiseField;
			}
			set
			{
				this.returnReceiptForMerchandiseField = value;
			}
		}

		public decimal SignatureConfirmation
		{
			get
			{
				return this.signatureConfirmationField;
			}
			set
			{
				this.signatureConfirmationField = value;
			}
		}

		public decimal SpecialHandling
		{
			get
			{
				return this.specialHandlingField;
			}
			set
			{
				this.specialHandlingField = value;
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

		public Fees()
		{
		}
	}
}