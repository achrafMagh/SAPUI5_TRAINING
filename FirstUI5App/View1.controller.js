sap.ui.define([
	"sap/ui/core/mvc/Controller",
	"sap/ui/model/json/JSONModel",
	"sap/m/MessageToast"
], function(Controller, JSONModel, MessageToast) {
	"use strict";

	return Controller.extend("ZFIRST_APP.controller.View1", {
	onInit: function() {
		
			var oData = {
				recipient: {
					name: "Seidor",
					currency: "EU"
				}
			};
			var oModel = new JSONModel(oData);
			this.getView().setModel(oModel);
			
			var oInvoiceItemsModel = new JSONModel();
			oInvoiceItemsModel.loadData("model/Invoices.json");
			this.getView().setModel(oInvoiceItemsModel, "invoiceItems");
			

		},
	
		
		onPressDisplayTotal: function(){
			this._calculateTotal();
			var fTotal = this.getView().getModel().getProperty("/recipient/total");
			var sCurrency = this.getView().getModel().getProperty("/recipient/currency");
			var sMsg = "Total: " + fTotal + " " + sCurrency;
			MessageToast.show(sMsg);
		},
		
		_calculateTotal: function() {
			var oInvoiceItemsModel = this.getView().getModel("invoiceItems");
			var aInvoices = oInvoiceItemsModel.getProperty("/Invoices");
					var fTotal = 0;
			
					aInvoices.forEach(function (oItem) {
						if (oItem.ExtendedPrice) {
							fTotal += oItem.ExtendedPrice;
						}
					});
			
					// Add total to recipient model
					this.getView().getModel().setProperty("/recipient/total", fTotal.toFixed(2));
		}
});
});