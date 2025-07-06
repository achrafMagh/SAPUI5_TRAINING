METHOD kpiset_get_entityset.

  CASE lv_kpiname.
    WHEN 'acc_trav' OR 'maladie-pro' OR 'acc_trajet' OR 'taux_clot' OR 'taux_pre' OR 'nbr_pres' OR 'taux_met'. "For monthli kpis ! 
      DO lv_months_diff + 1 TIMES. 
        "KPIS MENSUELS
        WHEN 'acc_trav'
      ENDDO.
    WHEN 'y_exemple1'
    "KPIS ANNUELS
    WHEN 'y_exemple2'
    WHEN OTHERS.
        "KPIS TRIMESTRIELS
        WHEN 't_example1'
  ENDCASE.




*BREAK-POINT.
ENDMETHOD.