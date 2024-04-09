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
	public class SpecialServices
	{
		private string certifiedMailField;

		private string cODField;

		private string deliveryConfirmationField;

		private string electronicReturnReceiptField;

		private string insuredMailField;

		private string registeredMailField;

		private string restrictedDeliveryField;

		private string returnReceiptField;

		private string signatureConfirmationField;

		[XmlAttribute]
		public string CertifiedMail
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

		[XmlAttribute]
		public string COD
		{
			get
			{
				return this.cODField;
			}
			set
			{
				this.cODField = value;
			}
		}

		[XmlAttribute]
		public string DeliveryConfirmation
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

		[XmlAttribute]
		public string ElectronicReturnReceipt
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

		[XmlAttribute]
		public string InsuredMail
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

		[XmlAttribute]
		public string RegisteredMail
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

		[XmlAttribute]
		public string RestrictedDelivery
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

		[XmlAttribute]
		public string ReturnReceipt
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

		[XmlAttribute]
		public string SignatureConfirmation
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

		public SpecialServices()
		{
		}
	}
}