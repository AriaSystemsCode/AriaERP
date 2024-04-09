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
	public class TrackNotificationRequest
	{
		private WebAuthenticationDetail webAuthenticationDetailField;

		private ClientDetail clientDetailField;

		private TransactionDetail transactionDetailField;

		private VersionId versionField;

		private string trackingNumberField;

		private bool multiPieceField;

		private bool multiPieceFieldSpecified;

		private string pagingTokenField;

		private string trackingNumberUniqueIdField;

		private DateTime shipDateRangeBeginField;

		private bool shipDateRangeBeginFieldSpecified;

		private DateTime shipDateRangeEndField;

		private bool shipDateRangeEndFieldSpecified;

		private string senderEMailAddressField;

		private string senderContactNameField;

		private EMailNotificationDetail notificationDetailField;

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

		public bool MultiPiece
		{
			get
			{
				return this.multiPieceField;
			}
			set
			{
				this.multiPieceField = value;
			}
		}

		[XmlIgnore]
		public bool MultiPieceSpecified
		{
			get
			{
				return this.multiPieceFieldSpecified;
			}
			set
			{
				this.multiPieceFieldSpecified = value;
			}
		}

		public EMailNotificationDetail NotificationDetail
		{
			get
			{
				return this.notificationDetailField;
			}
			set
			{
				this.notificationDetailField = value;
			}
		}

		public string PagingToken
		{
			get
			{
				return this.pagingTokenField;
			}
			set
			{
				this.pagingTokenField = value;
			}
		}

		public string SenderContactName
		{
			get
			{
				return this.senderContactNameField;
			}
			set
			{
				this.senderContactNameField = value;
			}
		}

		public string SenderEMailAddress
		{
			get
			{
				return this.senderEMailAddressField;
			}
			set
			{
				this.senderEMailAddressField = value;
			}
		}

		[XmlElement(DataType="date")]
		public DateTime ShipDateRangeBegin
		{
			get
			{
				return this.shipDateRangeBeginField;
			}
			set
			{
				this.shipDateRangeBeginField = value;
			}
		}

		[XmlIgnore]
		public bool ShipDateRangeBeginSpecified
		{
			get
			{
				return this.shipDateRangeBeginFieldSpecified;
			}
			set
			{
				this.shipDateRangeBeginFieldSpecified = value;
			}
		}

		[XmlElement(DataType="date")]
		public DateTime ShipDateRangeEnd
		{
			get
			{
				return this.shipDateRangeEndField;
			}
			set
			{
				this.shipDateRangeEndField = value;
			}
		}

		[XmlIgnore]
		public bool ShipDateRangeEndSpecified
		{
			get
			{
				return this.shipDateRangeEndFieldSpecified;
			}
			set
			{
				this.shipDateRangeEndFieldSpecified = value;
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

		public string TrackingNumberUniqueId
		{
			get
			{
				return this.trackingNumberUniqueIdField;
			}
			set
			{
				this.trackingNumberUniqueIdField = value;
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

		public TrackNotificationRequest()
		{
		}
	}
}