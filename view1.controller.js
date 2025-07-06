sap.ui.define([
	"sap/ui/core/mvc/Controller",
	'sap/ui/model/BindingMode',
	'sap/ui/model/json/JSONModel',
	'sap/viz/ui5/data/FlattenedDataset',
	'sap/viz/ui5/format/ChartFormatter',
	'sap/viz/ui5/api/env/Format',
	"sap/ui/core/BusyIndicator",
	'../formatter/formatter'
], function(Controller, BindingMode, JSONModel, FlattenedDataset, ChartFormatter, Format, formatter, BusyIndicator) {
	"use strict";
	return Controller.extend("ZKPI_NSZKPI_CHARTS.controller.View1", {
		formatter: formatter,
		oVizFrame: null,
		onInit: function() {
			var oChartTypeModel = new sap.ui.model.json.JSONModel({
				selectedChartType: "stacked_column" // default chart type
			});

			$.sap.myVar = [
				[]
			];

			this.getView().setModel(oChartTypeModel, "chartTypeModel");
			
			var kpiModel = new JSONModel({
				kpis: [{
					key: "acc_trav",
					text: "Accidents du travail métier ONEE-BE"
				}, {
					key: "maladie-pro",
					text: "Maladies professionnelles"
				}, {
					key: "acc_trajet",
					text: "Accidents de trajet"
				}, 
				//
				{
					key: "t_acc_mal",
					text: "Nombre de sinistres corporels avec incapacité permanente pour le Personnel"
				}
				
				,{
				 	key: "t_dec",
				 	text: "Nombre de décès"
				 }
				 , {
				  	key: "t_grav",
				  	text: "Taux de gravité"
				  },
				   {
				 	key: "t_j_perd",
				 	text: "Nombre de journées perdues"
				 },
				  {
				  	key: "t_tf",
				  	text: "Taux de Fréquence (avec arrêt)"
				  },
{
				  	key: "t_tf_2",
				  	text: "Taux de Fréquence 2 (sans arrêt)"
				  },
					{
						key: "t_ig",
				 	text: "Indice de gravité"	
					},
					{
						key: "t_if",
						text: "Indice de fréquence"
					},
					
					{
						key: "t_pres",
				 	text: "Accidents prestataires"	
					},
					{
						key: "t_mep",
				 	text: "Mise en place des plans d'actions à l'issue des incidents"	
					},
					{
						key: "t_meppa",
				 	text: "Mise en place des plans d'actions à l'issue de presque accident"	
					},
					{
						key: "t_mepos",
				 	text: "Mise en place d'un plan d'action à l'issue des observations de sécurité"	
					},
					{
						key: "t_ttpi",
				 	text: "Taux de traitement des plans d'action issues des incidents"	
					},
					{
						key: "t_ttppi",
				 	text: "Taux de traitement des plans d'action issues des presques accidents"	
					},
					{
						key: "t_ttppo",
				 	text: "Taux de traitement des plans d'action issus des observations de sécurité"	
					},
					
					
					
					
					//monthly
					{
					 	key: "taux_met",
				 	text: "Taux de clôture des accident de travail ONEE métier"	
					 },
					 //monthly need to add sites in the onee be
					//sites on the first step of select does not exist in the hiearchy..
					 {
					 	key: "taux_clot",
				 	text: "Taux de clôture des accident de travail ONEE trajet"	
					 },
					 
					 {
					 	key: "nbr_pres",
				 	text: "Nombre des accident de travail pretataire"	
					 },
					 {
					 	key: "taux_pre",
				 	text: "Taux de clôture des accident de travail pretataire"	
					 },
					 
					 
					//yearly
					{
						key: "y_tr",
				 	text: "Taux de Réalisation des Tests d'Échantillons"	
					},
					{
						key: "y_ma",
				 	text: "Taux de modifications acceptées"	
					},
					 {
					 	key: "y_mr",
				 	text: "Nombre de demandes de modification refusées"	
					 },
					 {
					 	key: "y_ram",
				 	text: "Taux de réalisation des activités des demandes de modifications"	
					 },
					 {
					 	key: "y_ncmaj",
				 	text: "Nombre des Non-conformités majeures S&ST"	
					 },
					 {
					 	key: "y_ncmin",
				 	text: "Nombre des Non-conformités mineures S&ST"	
					 },
					 {
					 	key: "y_tnc_maj",
				 	text: "Taux de traitement des Non Conformités majeures S&ST"	
					 },
					 {
					 	key: "y_tnc_min",
				 	text: "Taux de traitement des Non Conformités mineures S&ST"	
					 },
					 {
					 	key: "y_tc_acc",
				 	text: "Taux de clôture des accident de travail"	
					 },
					 {
					 	key: "y_a_cs",
				 	text: "Nbre d'audit Audit de certif/Suivi/Renouv/Int/à blanc via SAP EHS"	
					 },
					 {
					 	key: "y_mo_cr",
				 	text: "Mise en oeuvre des contrôles réglementaires"	
					 },
					 {
					 	key: "y_aud_sec",
				 	text: "Nombre des audits de sécurité (Contrôle SST) via SAP EHS "	
					 },
					 {
						key: "t_v_rs",
				 	text: "Nombre de visite innopinée par le Responsable sécurité des sites en exploitation"	
					},
					 //{
					 //	key: "y_nc_dau",
				 //	text: "Nombre total de non-conformités détectées lors des audits"	
					// },
					//yearly
					{
						key: "y_tbr",
				 	text: "Nombre d'incidents liés au bruit"	
					},
					
					//trimestres sans site
					{
						key: "t_ncmaj",
				 	text: "Nombre des Non-conformités majeures S&ST (T)"	
					},
					{
						key: "t_ncmin",
				 	text: "Nombre des Non-conformités mineures S&ST (T)"	
					},
					{
						key: "t_tnc_maj",
				 	text: "Taux de traitement des Non Conformités majeures S&ST (T)"	
					},
					{
						key: "t_tnc_min",
				 	text: "Taux de traitement des Non Conformités mineures S&ST (T)"	
					},
					{
						key: "t_fds",
				 	text: "Taux de disponibilité des FDS dans SAP EHS (T)"	
					},
					
					//YEARLY
					{
						key: "y_nbst",
				 	text: "Nombre des mesures Bruit"	
					},
					{
						key: "y_mep_br",
				 	text: "Mise en place des mesures de protection collective dont les zones ou le bruit dépasse 85 (dB)"	
					},
					{
						key: "y_mep_epi",
				 	text: "Mise en place des mesures de protection dont les zones ou le bruit 85-90 (mise en place des EPI)"	
					},
					{
						key: "y_mep_br2",
				 	text: "Mise en place des mesures de protection dont les zones ou le bruit dépasse 105 dB par x min"	
					},
					{
						key: "y_lum",
				 	text: "Niveaux d'éclairage moyen sur le lieu de travail"	
					},
						{
						key: "y_lum_gaz",
				 	text: "Niveau moyen des gaz toxiques"	
					},
				
					
					
				]
			});
			// this.getView().setModel(directionModel, "directionModel");
			this.getView().setModel(kpiModel, "kpiModel");
			var oODataModel = this.getOwnerComponent().getModel();
			var oChartModel = new JSONModel();
			this.getView().setModel(oChartModel, "chartModel");
			var oVizFrame = this.byId("idVizFrame");
			oVizFrame.setModel(oODataModel);
			oVizFrame.setVizProperties({
				plotArea: {
					dataLabel: {
						visible: true
					}
				},
				valueAxis: {
					title: {
						visible: true,
						text: 'Revenue by site'
					}
				},
				categoryAxis: {
					title: {
						visible: false
					}
				},
				title: {
					visible: false,
					text: 'Revenue by site'
				}
			});
			// Initialize PopOver
			// for popOver
			// var oPopOver = this.getView().byId("idPopOver");
			// oPopOver.connect(oVizFrame.getVizUid());
		},
		myOnClickHandler: function(oEvent) {
			
			// Show the Busy Indicator
			
			
			var data = oEvent.getParameter("data")[0].data;
			var oView = this.getView();
			var oChartModel = oView.getModel("chartModel");
			var oData = oChartModel.getData(); // Récupère toutes les données du modèle
			$.sap.myVar.push(oData);

			var test = oData.filter(item => item.site === data.site && item.period === data.month);
			var id = test[0].id;
	
			var fromDateS = this.dateStringToDate(test[0].fromDate);
			var toDateS = this.dateStringToDate(test[0].toDate);
			
			var formatedID = parseInt(id);
			// data.month = data.month.split(" ").join("");
		
			
			 data.month = this.getDateNumber(test[0].period);
			
			if (data.site != 'Tous les sites') {
				var oBusyIndicator = this.byId("busyIndicator");
			oBusyIndicator.setVisible(true);
				data.year = data.year.split(" ").join("");
			var fromFilterDate;
			var toFilterDate;
			
			if (data.month < 10) {
				fromFilterDate = data.year + "-0" + data.month + "-" + '01';
				toFilterDate = data.year + "-0" + data.month + "-" + '01';
			} else {
				fromFilterDate = data.year + "-" + data.month + "-" + '01';
				toFilterDate = data.year + "-" + data.month + "-" + '01';
			}
			
			var fromDateFinal, toDateFinal;
			const kpiNameS = test[0].kpiName;
			if(kpiNameS.startsWith("t")) {
				fromDateFinal = fromDateS;
				toDateFinal = toDateS;
			}else if (kpiNameS.startsWith("y")){
				fromDateFinal = data.year + "-0" + '1' + "-" + '01';
				toDateFinal = data.year + "-" + "12" + "-" + '30';
			}
			
			else {
				fromDateFinal = fromFilterDate;
				toDateFinal = toFilterDate;
			}
			
			var aFilters = [
				new sap.ui.model.Filter("fromDate", sap.ui.model.FilterOperator.EQ, fromDateFinal),
				new sap.ui.model.Filter("toDate", sap.ui.model.FilterOperator.EQ, toDateFinal),
				new sap.ui.model.Filter("kpiName", sap.ui.model.FilterOperator.EQ, test[0].kpiName),
				new sap.ui.model.Filter("id", sap.ui.model.FilterOperator.EQ, formatedID.toString())
			];
			var monthName;
			var oODataModel = oView.getModel();
			oODataModel.read("/KPISet", {
				filters: aFilters,
				success: function(oData) {
					oBusyIndicator.setVisible(false)
					if (oData.results.length > 0) {
						oData.results.forEach(function(entry) {
							// entry.period.split(" ").join("");

								 //monthName = this.getMonthName(entry.period);
						
							  if (entry.period == "1 ") {
							  	entry.period = "Jan";
							  } else if (entry.period == "2 ") {
							  	entry.period = "Fév";
							  } else if (entry.period == "3 ") {
							  	entry.period = "Mar";
							  } else if (entry.period == "4 ") {
							  	entry.period = "Avr";
							  } else if (entry.period == "5 ") {
							 	entry.period = "Mai";
							 } else if (entry.period == "6 ") {
						  entry.period = "Juin";
							  } else if (entry.period == "7 ") {
							  	entry.period = "Juil";
							  } else if (entry.period == "8 ") {
							  	entry.period = "Août";
							  } else if (entry.period == "9 ") {
							 	entry.period = "Sep";
							  } else if (entry.period == "10 ") {
							  	entry.period = "Oct";
							  } else if (entry.period == "11 ") {
							  	entry.period = "Nov";
							  } else if (entry.period == "12 ") {
							  	entry.period = "Déc";
							  }
						});

						oChartModel.setData(oData.results);
						sap.m.MessageToast.show("Filtres appliqués");
					} else {
						sap.m.MessageToast.show("Pas de données à afficher");
						oChartModel.setData([]);
					}
				},
				error: function() {
					oBusyIndicator.setVisible(false)
					sap.m.MessageToast.show("Erreur");
				}
			});
			}

			

		},
		formatSiteValue: function(value) {
			return value ? value : null;
		},
		onChartTypeChange: function(oEvent) {
			var selectedType = oEvent.getParameter("selectedItem").getKey();
			var oChartTypeModel = this.getView().getModel("chartTypeModel");
			oChartTypeModel.setProperty("/selectedChartType", selectedType);
		},
		onApplyFilters: function() {
			var oView = this.getView();
			var oODataModel = oView.getModel(); // OData model
			var oChartModel = oView.getModel("chartModel");
			var sDateBegin = oView.byId("dateBegin").getDateValue();
			var sDateEnd = oView.byId("dateEnd").getDateValue();
			var sKpi = oView.byId("kpiComboBox").getSelectedKey();
			var sKpiText = oView.byId("kpiComboBox").getSelectedItem().getText();
			var sSelectedType = this.getView().byId("chartTypeComboBox").getSelectedKey();
			var oChartTypeModel = this.getView().getModel("chartTypeModel");
			oChartTypeModel.setProperty("/selectedChartType", sSelectedType);
			// Show the Busy Indicator
			var oBusyIndicator = this.byId("busyIndicator");
			oBusyIndicator.setVisible(true);

			// function formatLocalDate(oDate) {
			// 	var year = oDate.getFullYear();
			// 	var month = String(oDate.getMonth() + 1).padStart(2, '0');
			// 	var day = String(oDate.getDate()).padStart(2, '0');
			// 	return year + "-" + month + "-" + day;
			// }
			if (sDateBegin && sDateEnd) {
				var sDateBeginFormatted = this.formatLocalDate(sDateBegin);
				var sDateEndFormatted = this.formatLocalDate(sDateEnd);
				var aFilters = [
					new sap.ui.model.Filter("fromDate", sap.ui.model.FilterOperator.EQ, sDateBeginFormatted),
					new sap.ui.model.Filter("toDate", sap.ui.model.FilterOperator.EQ, sDateEndFormatted),
					new sap.ui.model.Filter("kpiName", sap.ui.model.FilterOperator.EQ, sKpi),
					new sap.ui.model.Filter("id", sap.ui.model.FilterOperator.EQ, " ")
				];
				var oVizFrame = this.byId("idVizFrame");
				oVizFrame.setVizProperties({
					valueAxis: {
						title: {
							visible: true,
							text: sKpiText
						}
					},
					// legend: {
					//     visible: true
					// },
					plotArea: {
						colorPalette: d3.scale.category20().range(),
						dataLabel: {
							showTotal: true
						}
					}
				});
				oODataModel.read("/KPISet", {
					filters: aFilters,
					success: function(oData) {
						oBusyIndicator.setVisible(false);
						if (oData.results.length > 0) {
							// oData.results.forEach(function(entry) {
							// 	entry.yearMonth = entry.year + "-" + entry.month;
							// });
							oData.results.forEach(function(entry) {
								// 	entry.yearMonth = entry.year + "-" + entry.month;
								if (entry.period == "1 ") {
									entry.period = "Jan";
								} else if (entry.period == "2 ") {
									entry.period = "Fév";
								} else if (entry.period == "3 ") {
									entry.period = "Mar";
								} else if (entry.period == "4 ") {
									entry.period = "Avr";
								} else if (entry.period == "5 ") {
									entry.period = "Mai";
								} else if (entry.period == "6 ") {
									entry.period = "Juin";
								} else if (entry.period == "7 ") {
									entry.period = "Juil";
								} else if (entry.period == "8 ") {
									entry.period = "Août";
								} else if (entry.period == "9 ") {
									entry.period = "Sep";
								} else if (entry.period == "10 ") {
									entry.period = "Oct";
								} else if (entry.period == "11 ") {
									entry.period = "Nov";
								} else if (entry.period == "12 ") {
									entry.period = "Déc";
								}

							});
							//$.sap.myVar.push.apply($.sap.myVar,oData);
							var temp_table = oData.results;
							$.sap.myVar.push(temp_table);
							oChartModel.setData(oData.results);
							sap.m.MessageToast.show("Filtres appliqués");
						} else {
							sap.m.MessageToast.show("Pas de données à afficher");
							oChartModel.setData([]);
						}
					},
					error: function() {
						oBusyIndicator.setVisible(false);
						sap.m.MessageToast.show("Erreur");
					}
				});
			} else {
				sap.m.MessageToast.show("tous les champs sont obligatoires.");
			}
		},
		onBack: function() {
			var oView = this.getView();
			var oODataModel = oView.getModel(); // OData model
			var oChartModel = oView.getModel("chartModel");
			var sDateBegin = oView.byId("dateBegin").getDateValue();
			var sDateEnd = oView.byId("dateEnd").getDateValue();
			var sKpi = oView.byId("kpiComboBox").getSelectedKey();
			var sKpiText = oView.byId("kpiComboBox").getSelectedItem().getText();
			var sSelectedType = this.getView().byId("chartTypeComboBox").getSelectedKey();
			var oChartTypeModel = this.getView().getModel("chartTypeModel");
			oChartTypeModel.setProperty("/selectedChartType", sSelectedType);
			var last_index = $.sap.myVar.length - 1
			if ($.sap.myVar.length > 1) {
				oChartModel.setData($.sap.myVar[last_index]);
				$.sap.myVar.pop();
			}

		},
		dateStringToDate(dateString) {
			const date = new Date(dateString);
			const year = date.getFullYear();
			const month = (date.getMonth() + 1).toString().padStart(2, '0'); // Months are zero-based
			const day = date.getDate().toString().padStart(2, '0');
				return `${year}-${month}-01`;
		},
		getDateNumber(month) {
			if (month === "Jan") {
				return "1";
			} else if (month === "Fév") {
				return "2";
			} else if (month === "Mar") {
				return "3 ";
			} else if (month === "Avr") {
				return "4";
			} else if (month === "Mai") {
				return  "5";
			} else if (month === "Juin") {
				return  "6";
			} else if (month === "Juil") {
				return  "7";
			} else if (month === "Août") {
				return  "8";
			} else if (month === "Sep") {
				return  "9";
			} else if (month === "Oct") {
				return  "10";
			} else if (month === "Nov") {
				return "11";
			} else if (month === "Déc") {
				return "12";
			}
		}
		// ,
		// getMonthName(monthNumber) {
		//      if (monthNumber === "1 ") {
		// 					 return "Jan";
		// 					 } else if (monthNumber == "2 ") {
		// 					 	return "Fév";
		// 					 } else if (monthNumberd == "3 ") {
		// 						 return "Mar";
		// 					 } else if (monthNumber == "4 ") {
		// 					 	return  "Avr";
		// 					 } else if (monthNumber == "5 ") {
		// 						return "Mai";
		// 					} else if (monthNumber == "6 ") {
		// 					 	 return "Juin";
		// 					 } else if (monthNumber == "7 ") {
		// 					 	return "Juil";
		// 					 } else if (monthNumber == "8 ") {
		// 					 	return "Août";
		// 					 } else if (monthNumber == "9 ") {
		// 						return "Sep";
		// 					 } else if (monthNumber == "10 ") {
		// 					 	return "Oct";
		// 					 } else if (monthNumber == "11 ") {
		// 					 	return "Nov";
		// 					 } else if (monthNumber == "12 ") {
		// 					 	return "Déc";
		// 					 }
		// }
		,formatLocalDate(oDate) {
				var year = oDate.getFullYear();
				var month = String(oDate.getMonth() + 1).padStart(2, '0');
				var day = String(oDate.getDate()).padStart(2, '0');
				return year + "-" + month + "-" + day;
			}

	});
});