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
	public class EMailNotificationRecipient
	{
		private  EMailNotificationRecipientType eMailNotificationRecipientTypeField;

		private bool eMailNotificationRecipientTypeFieldSpecified;

		private string eMailAddressField;

		private bool notifyOnShipmentField;

		private bool notifyOnShipmentFieldSpecified;

		private bool notifyOnExceptionField;

		private bool notifyOnExceptionFieldSpecified;

		private bool notifyOnDeliveryField;

		private bool notifyOnDeliveryFieldSpecified;

		private EMailNotificationFormatType formatField;

		private  Localization localizationField;

		public string EMailAddress
		{
			get
			{
				return this.eMailAddressField;
			}
			set
			{
				this.eMailAddressField = value;
			}
		}

		public  EMailNotificationRecipientType EMailNotificationRecipientType
		{
			get
			{
				return this.eMailNotificationRecipientTypeField;
			}
			set
			{
				this.eMailNotificationRecipientTypeField = value;
			}
		}

		[XmlIgnore]
		public bool EMailNotificationRecipientTypeSpecified
		{
			get
			{
				return this.eMailNotificationRecipientTypeFieldSpecified;
			}
			set
			{
				this.eMailNotificationRecipientTypeFieldSpecified = value;
			}
		}

		public EMailNotificationFormatType Format
		{
			get
			{
				return this.formatField;
			}
			set
			{
				this.formatField = value;
			}
		}

		public  Localization Localization
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

		public bool NotifyOnDelivery
		{
			get
			{
				return this.notifyOnDeliveryField;
			}
			set
			{
				this.notifyOnDeliveryField = value;
			}
		}

		[XmlIgnore]
		public bool NotifyOnDeliverySpecified
		{
			get
			{
				return this.notifyOnDeliveryFieldSpecified;
			}
			set
			{
				this.notifyOnDeliveryFieldSpecified = value;
			}
		}

		public bool NotifyOnException
		{
			get
			{
				return this.notifyOnExceptionField;
			}
			set
			{
				this.notifyOnExceptionField = value;
			}
		}

		[XmlIgnore]
		public bool NotifyOnExceptionSpecified
		{
			get
			{
				return this.notifyOnExceptionFieldSpecified;
			}
			set
			{
				this.notifyOnExceptionFieldSpecified = value;
			}
		}

		public bool NotifyOnShipment
		{
			get
			{
				return this.notifyOnShipmentField;
			}
			set
			{
				this.notifyOnShipmentField = value;
			}
		}

		[XmlIgnore]
		public bool NotifyOnShipmentSpecified
		{
			get
			{
				return this.notifyOnShipmentFieldSpecified;
			}
			set
			{
				this.notifyOnShipmentFieldSpecified = value;
			}
		}

		public EMailNotificationRecipient()
		{
		}
	}
}