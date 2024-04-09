using System;
using System.CodeDom.Compiler;
using System.ComponentModel;
using System.Diagnostics;
using System.Xml.Serialization;
using Fedex.FedexTrack;
namespace Fedex.FedexTrack
{
	[DebuggerStepThrough]
	[DesignerCategory("code")]
	[GeneratedCode("System.Xml", "4.0.30319.1")]
	[Serializable]
	[XmlType(Namespace="http://fedex.com/ws/track/v4")]
	public class TrackDetail
	{
		private Notification notificationField;

		private string trackingNumberField;

		private StringBarcode barcodeField;

		private string trackingNumberUniqueIdentifierField;

		private string statusCodeField;

		private string statusDescriptionField;

		private TrackReconciliation reconciliationField;

		private string serviceCommitMessageField;

		private CarrierCodeType carrierCodeField;

		private bool carrierCodeFieldSpecified;

		private TrackPackageIdentifier[] otherIdentifiersField;

		private string serviceInfoField;

		private  ServiceType serviceTypeField;

		private bool serviceTypeFieldSpecified;

		private Weight packageWeightField;

		private Dimensions packageDimensionsField;

		private Weight packageDimensionalWeightField;

		private Weight shipmentWeightField;

		private string packagingField;

		private  PackagingType packagingTypeField;

		private bool packagingTypeFieldSpecified;

		private string packageSequenceNumberField;

		private string packageCountField;

		private  TrackReturnLabelType trackReturnLabelTypeField;

		private bool trackReturnLabelTypeFieldSpecified;

		private string trackReturnDescriptionField;

		private Address shipperAddressField;

		private Address originLocationAddressField;

		private DateTime estimatedPickupTimestampField;

		private bool estimatedPickupTimestampFieldSpecified;

		private DateTime shipTimestampField;

		private bool shipTimestampFieldSpecified;

		private Distance totalTransitDistanceField;

		private Distance distanceToDestinationField;

		private Address destinationAddressField;

		private Address destinationLocationAddressField;

		private DateTime estimatedDeliveryTimestampField;

		private bool estimatedDeliveryTimestampFieldSpecified;

		private DateTime actualDeliveryTimestampField;

		private bool actualDeliveryTimestampFieldSpecified;

		private Address actualDeliveryAddressField;

		private TrackDeliveryLocationType deliveryLocationTypeField;

		private bool deliveryLocationTypeFieldSpecified;

		private string deliveryLocationDescriptionField;

		private string deliverySignatureNameField;

		private bool signatureProofOfDeliveryAvailableField;

		private bool signatureProofOfDeliveryAvailableFieldSpecified;

		private bool proofOfDeliveryNotificationsAvailableField;

		private bool proofOfDeliveryNotificationsAvailableFieldSpecified;

		private bool exceptionNotificationsAvailableField;

		private bool exceptionNotificationsAvailableFieldSpecified;

		private TrackSplitShipmentPart[] splitShipmentPartsField;

		private RedirectToHoldEligibilityType redirectToHoldEligibilityField;

		private bool redirectToHoldEligibilityFieldSpecified;

		private TrackEvent[] eventsField;

		public Address ActualDeliveryAddress
		{
			get
			{
				return this.actualDeliveryAddressField;
			}
			set
			{
				this.actualDeliveryAddressField = value;
			}
		}

		public DateTime ActualDeliveryTimestamp
		{
			get
			{
				return this.actualDeliveryTimestampField;
			}
			set
			{
				this.actualDeliveryTimestampField = value;
			}
		}

		[XmlIgnore]
		public bool ActualDeliveryTimestampSpecified
		{
			get
			{
				return this.actualDeliveryTimestampFieldSpecified;
			}
			set
			{
				this.actualDeliveryTimestampFieldSpecified = value;
			}
		}

		public StringBarcode Barcode
		{
			get
			{
				return this.barcodeField;
			}
			set
			{
				this.barcodeField = value;
			}
		}

		public CarrierCodeType CarrierCode
		{
			get
			{
				return this.carrierCodeField;
			}
			set
			{
				this.carrierCodeField = value;
			}
		}

		[XmlIgnore]
		public bool CarrierCodeSpecified
		{
			get
			{
				return this.carrierCodeFieldSpecified;
			}
			set
			{
				this.carrierCodeFieldSpecified = value;
			}
		}

		public string DeliveryLocationDescription
		{
			get
			{
				return this.deliveryLocationDescriptionField;
			}
			set
			{
				this.deliveryLocationDescriptionField = value;
			}
		}

		public TrackDeliveryLocationType DeliveryLocationType
		{
			get
			{
				return this.deliveryLocationTypeField;
			}
			set
			{
				this.deliveryLocationTypeField = value;
			}
		}

		[XmlIgnore]
		public bool DeliveryLocationTypeSpecified
		{
			get
			{
				return this.deliveryLocationTypeFieldSpecified;
			}
			set
			{
				this.deliveryLocationTypeFieldSpecified = value;
			}
		}

		public string DeliverySignatureName
		{
			get
			{
				return this.deliverySignatureNameField;
			}
			set
			{
				this.deliverySignatureNameField = value;
			}
		}

		public Address DestinationAddress
		{
			get
			{
				return this.destinationAddressField;
			}
			set
			{
				this.destinationAddressField = value;
			}
		}

		public Address DestinationLocationAddress
		{
			get
			{
				return this.destinationLocationAddressField;
			}
			set
			{
				this.destinationLocationAddressField = value;
			}
		}

		public Distance DistanceToDestination
		{
			get
			{
				return this.distanceToDestinationField;
			}
			set
			{
				this.distanceToDestinationField = value;
			}
		}

		public DateTime EstimatedDeliveryTimestamp
		{
			get
			{
				return this.estimatedDeliveryTimestampField;
			}
			set
			{
				this.estimatedDeliveryTimestampField = value;
			}
		}

		[XmlIgnore]
		public bool EstimatedDeliveryTimestampSpecified
		{
			get
			{
				return this.estimatedDeliveryTimestampFieldSpecified;
			}
			set
			{
				this.estimatedDeliveryTimestampFieldSpecified = value;
			}
		}

		public DateTime EstimatedPickupTimestamp
		{
			get
			{
				return this.estimatedPickupTimestampField;
			}
			set
			{
				this.estimatedPickupTimestampField = value;
			}
		}

		[XmlIgnore]
		public bool EstimatedPickupTimestampSpecified
		{
			get
			{
				return this.estimatedPickupTimestampFieldSpecified;
			}
			set
			{
				this.estimatedPickupTimestampFieldSpecified = value;
			}
		}

		[XmlElement("Events")]
		public TrackEvent[] Events
		{
			get
			{
				return this.eventsField;
			}
			set
			{
				this.eventsField = value;
			}
		}

		public bool ExceptionNotificationsAvailable
		{
			get
			{
				return this.exceptionNotificationsAvailableField;
			}
			set
			{
				this.exceptionNotificationsAvailableField = value;
			}
		}

		[XmlIgnore]
		public bool ExceptionNotificationsAvailableSpecified
		{
			get
			{
				return this.exceptionNotificationsAvailableFieldSpecified;
			}
			set
			{
				this.exceptionNotificationsAvailableFieldSpecified = value;
			}
		}

		public  Notification Notification
		{
			get
			{
				return this.notificationField;
			}
			set
			{
				this.notificationField = value;
			}
		}

		public Address OriginLocationAddress
		{
			get
			{
				return this.originLocationAddressField;
			}
			set
			{
				this.originLocationAddressField = value;
			}
		}

		[XmlElement("OtherIdentifiers")]
		public TrackPackageIdentifier[] OtherIdentifiers
		{
			get
			{
				return this.otherIdentifiersField;
			}
			set
			{
				this.otherIdentifiersField = value;
			}
		}

		[XmlElement(DataType="nonNegativeInteger")]
		public string PackageCount
		{
			get
			{
				return this.packageCountField;
			}
			set
			{
				this.packageCountField = value;
			}
		}

		public Weight PackageDimensionalWeight
		{
			get
			{
				return this.packageDimensionalWeightField;
			}
			set
			{
				this.packageDimensionalWeightField = value;
			}
		}

		public Dimensions PackageDimensions
		{
			get
			{
				return this.packageDimensionsField;
			}
			set
			{
				this.packageDimensionsField = value;
			}
		}

		[XmlElement(DataType="nonNegativeInteger")]
		public string PackageSequenceNumber
		{
			get
			{
				return this.packageSequenceNumberField;
			}
			set
			{
				this.packageSequenceNumberField = value;
			}
		}

		public Weight PackageWeight
		{
			get
			{
				return this.packageWeightField;
			}
			set
			{
				this.packageWeightField = value;
			}
		}

		public string Packaging
		{
			get
			{
				return this.packagingField;
			}
			set
			{
				this.packagingField = value;
			}
		}

		public  PackagingType PackagingType
		{
			get
			{
				return this.packagingTypeField;
			}
			set
			{
				this.packagingTypeField = value;
			}
		}

		[XmlIgnore]
		public bool PackagingTypeSpecified
		{
			get
			{
				return this.packagingTypeFieldSpecified;
			}
			set
			{
				this.packagingTypeFieldSpecified = value;
			}
		}

		public bool ProofOfDeliveryNotificationsAvailable
		{
			get
			{
				return this.proofOfDeliveryNotificationsAvailableField;
			}
			set
			{
				this.proofOfDeliveryNotificationsAvailableField = value;
			}
		}

		[XmlIgnore]
		public bool ProofOfDeliveryNotificationsAvailableSpecified
		{
			get
			{
				return this.proofOfDeliveryNotificationsAvailableFieldSpecified;
			}
			set
			{
				this.proofOfDeliveryNotificationsAvailableFieldSpecified = value;
			}
		}

		public TrackReconciliation Reconciliation
		{
			get
			{
				return this.reconciliationField;
			}
			set
			{
				this.reconciliationField = value;
			}
		}

		public RedirectToHoldEligibilityType RedirectToHoldEligibility
		{
			get
			{
				return this.redirectToHoldEligibilityField;
			}
			set
			{
				this.redirectToHoldEligibilityField = value;
			}
		}

		[XmlIgnore]
		public bool RedirectToHoldEligibilitySpecified
		{
			get
			{
				return this.redirectToHoldEligibilityFieldSpecified;
			}
			set
			{
				this.redirectToHoldEligibilityFieldSpecified = value;
			}
		}

		public string ServiceCommitMessage
		{
			get
			{
				return this.serviceCommitMessageField;
			}
			set
			{
				this.serviceCommitMessageField = value;
			}
		}

		public string ServiceInfo
		{
			get
			{
				return this.serviceInfoField;
			}
			set
			{
				this.serviceInfoField = value;
			}
		}

		public  ServiceType ServiceType
		{
			get
			{
				return this.serviceTypeField;
			}
			set
			{
				this.serviceTypeField = value;
			}
		}

		[XmlIgnore]
		public bool ServiceTypeSpecified
		{
			get
			{
				return this.serviceTypeFieldSpecified;
			}
			set
			{
				this.serviceTypeFieldSpecified = value;
			}
		}

		public Weight ShipmentWeight
		{
			get
			{
				return this.shipmentWeightField;
			}
			set
			{
				this.shipmentWeightField = value;
			}
		}

		public Address ShipperAddress
		{
			get
			{
				return this.shipperAddressField;
			}
			set
			{
				this.shipperAddressField = value;
			}
		}

		public DateTime ShipTimestamp
		{
			get
			{
				return this.shipTimestampField;
			}
			set
			{
				this.shipTimestampField = value;
			}
		}

		[XmlIgnore]
		public bool ShipTimestampSpecified
		{
			get
			{
				return this.shipTimestampFieldSpecified;
			}
			set
			{
				this.shipTimestampFieldSpecified = value;
			}
		}

		public bool SignatureProofOfDeliveryAvailable
		{
			get
			{
				return this.signatureProofOfDeliveryAvailableField;
			}
			set
			{
				this.signatureProofOfDeliveryAvailableField = value;
			}
		}

		[XmlIgnore]
		public bool SignatureProofOfDeliveryAvailableSpecified
		{
			get
			{
				return this.signatureProofOfDeliveryAvailableFieldSpecified;
			}
			set
			{
				this.signatureProofOfDeliveryAvailableFieldSpecified = value;
			}
		}

		[XmlElement("SplitShipmentParts")]
		public TrackSplitShipmentPart[] SplitShipmentParts
		{
			get
			{
				return this.splitShipmentPartsField;
			}
			set
			{
				this.splitShipmentPartsField = value;
			}
		}

		public string StatusCode
		{
			get
			{
				return this.statusCodeField;
			}
			set
			{
				this.statusCodeField = value;
			}
		}

		public string StatusDescription
		{
			get
			{
				return this.statusDescriptionField;
			}
			set
			{
				this.statusDescriptionField = value;
			}
		}

		public Distance TotalTransitDistance
		{
			get
			{
				return this.totalTransitDistanceField;
			}
			set
			{
				this.totalTransitDistanceField = value;
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

		public string TrackingNumberUniqueIdentifier
		{
			get
			{
				return this.trackingNumberUniqueIdentifierField;
			}
			set
			{
				this.trackingNumberUniqueIdentifierField = value;
			}
		}

		public string TrackReturnDescription
		{
			get
			{
				return this.trackReturnDescriptionField;
			}
			set
			{
				this.trackReturnDescriptionField = value;
			}
		}

		public  TrackReturnLabelType TrackReturnLabelType
		{
			get
			{
				return this.trackReturnLabelTypeField;
			}
			set
			{
				this.trackReturnLabelTypeField = value;
			}
		}

		[XmlIgnore]
		public bool TrackReturnLabelTypeSpecified
		{
			get
			{
				return this.trackReturnLabelTypeFieldSpecified;
			}
			set
			{
				this.trackReturnLabelTypeFieldSpecified = value;
			}
		}

		public TrackDetail()
		{
		}
	}
}