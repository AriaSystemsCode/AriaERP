using System;
using System.CodeDom.Compiler;
using System.Xml.Serialization;

namespace Fedex.FedexTrack
{
	[GeneratedCode("System.Xml", "4.0.30319.1")]
	[Serializable]
	[XmlType(Namespace="http://fedex.com/ws/track/v4")]
	public enum TrackIdentifierType
	{
		BILL_OF_LADING,
		COD_RETURN_TRACKING_NUMBER,
		CUSTOMER_AUTHORIZATION_NUMBER,
		CUSTOMER_REFERENCE,
		DEPARTMENT,
		FREE_FORM_REFERENCE,
		GROUND_INTERNATIONAL,
		GROUND_SHIPMENT_ID,
		GROUP_MPS,
		INVOICE,
		PARTNER_CARRIER_NUMBER,
		PART_NUMBER,
		PURCHASE_ORDER,
		RETURNED_TO_SHIPPER_TRACKING_NUMBER,
		RETURN_MATERIALS_AUTHORIZATION,
		TRACKING_NUMBER_OR_DOORTAG,
		TRANSPORTATION_CONTROL_NUMBER,
		SHIPPER_REFERENCE,
		STANDARD_MPS
	}
}