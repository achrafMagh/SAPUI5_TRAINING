METHOD kpiset_get_entityset.

  CASE lv_kpiname.
    WHEN 'acc_trav' OR 'maladie-pro' OR 'acc_trajet' OR 'taux_clot' OR 'taux_pre' OR 'nbr_pres' OR 'taux_met'. "For monthli kpis ! 
      DO lv_months_diff + 1 TIMES. 
        "KPIS MENSUELS
      ENDDO.
        "KPI ANNUELS

    WHEN OTHERS.
        "KPI TRIMESTRIELS
  ENDCASE.




*BREAK-POINT.
ENDMETHOD.