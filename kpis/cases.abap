METHOD kpiset_get_entityset.

  CASE lv_kpiname.
    WHEN 'acc_trav' OR 'maladie-pro' OR 'acc_trajet' OR 'taux_clot' OR 'taux_pre' OR 'nbr_pres' OR 'taux_met'. "For monthli kpis ! 
      DO lv_months_diff + 1 TIMES. 
        IF lv_currm < 10.
          lv_currM_s = |0{ lv_currm }|. 
        ELSEIF lv_currm >= 10.
          lv_currM_s = |{ lv_currm }|.
        ENDIF.

        LOOP AT lt_children ASSIGNING FIELD-SYMBOL(<fs_site_temp>).
          IF <fs_site_temp>-children IS NOT INITIAL.
*            IF lv_currm < 10.
            CASE lv_kpiName .
                "Accidents du travail métier
              WHEN 'acc_trav'.
                SELECT COUNT(*)
                FROM i_incidentbasicinfo
                WHERE ehslocationuuid IN @<fs_site_temp>-children "Does not accept IN !
                  AND substring( CAST( incidentdate AS CHAR ), 5, 2 ) = @lv_currM_s
                  AND substring( CAST( incidentdate AS CHAR ), 1 , 4 ) =  @lv_curry
                INTO @lv_kpi_single.
            ENDCASE.

          ENDIF.

          <fs_site_temp>-kpi = lv_kpi_single.

          SELECT SINGLE ehslocationid
              FROM c_curehslocationinclroothier
              WHERE ehslocationuuid = @<fs_site_temp>-parent
              INTO @lv_result-id.
          lv_result-site = <fs_site_temp>-name.
          lv_result-kpivalue = <fs_site_temp>-kpi.
          lv_result-period = lv_currm.  
          lv_result-year = lv_curry.
          lv_result-fromdate = lv_from_date.
          lv_result-todate = lv_to_date.
          lv_result-kpiname = lv_kpiname.
          APPEND lv_result TO et_entityset.
          CLEAR lv_result.
          CLEAR lv_kpi_single.
*          ENDIF.
        ENDLOOP.
        lv_currM = lv_currM + 1.
        IF lv_currM > 12.
          lv_currM = 1.
          lv_currY = lv_currY + 1.
        ENDIF.
      ENDDO.



    WHEN 'y_tr'.
      lv_tempY = lv_to_date(4) - lv_from_date(4).
      DO lv_tempY + 1 TIMES.
        .
        SELECT COUNT(*)
          FROM Ehhss_Sampling_Campaign_Allres
          WHERE status = '03'
          AND substring( CAST( start_date AS CHAR ), 1 , 4 ) =  @lv_curry
          INTO @DATA(lv_allres).

        SELECT COUNT(*)
          FROM Ehhss_Sampling_Campaign_Allres
          WHERE substring( CAST( start_date AS CHAR ), 1 , 4 ) =  @lv_curry
          INTO @DATA(lv_total_allres).

        IF lv_total_allres <> 0.
          lv_kpi_f_y = lv_allres / lv_total_allres.
        ELSE.
          lv_kpi_f_y = 0.
        ENDIF.

        lv_result-site = 'Tous les sites'.
        lv_result-kpivalue = lv_kpi_f_y * 100.
        lv_result-period = ''.
        lv_result-year = lv_curry.
        lv_result-fromdate = lv_from_date.
        lv_result-todate = lv_to_date.
        lv_result-kpiname = lv_kpiname.
        APPEND lv_result TO et_entityset.
        lv_curry = lv_curry + 1.

      ENDDO.





          "nbr of entriesof VALUE  from Ehhss_Sampling_Campaign_Allres when AGENT_KEY_REF = Lumière /
          "Nbr of entries in Ehhss_Sampling_Campaign_Allres when AGENT_KEY_REF = Gaz Toxique
          WHEN 'y_lum_gaz'.
              lv_tempY = lv_to_date(4) - lv_from_date(4).
              DO lv_tempY + 1 TIMES.
          SELECT COUNT(*) FROM Ehhss_Sampling_Campaign_Allres
                  WHERE AGENT_KEY_REF = '2FE3F76090A11EDF80E4E6D147C04A6C' "Change id of 'lumiere'
                  AND substring( CAST( START_DATE AS CHAR ), 1 , 4 ) =  @lv_curry
                  INTO @DATA(lv_lum).

          SELECT COUNT(*) FROM Ehhss_Sampling_Campaign_Allres
                  WHERE AGENT_KEY_REF = '2FE3F76090A11EDF80E4E6D147C04A6C' "Change id of 'lumiere'
                  AND substring( CAST( START_DATE AS CHAR ), 1 , 4 ) =  @lv_curry
                  INTO @DATA(lv_gaz).

              IF lv_total_allres <> 0.
                lv_kpi_f_y = lv_lum / lv_gaz.
              ELSE.
                lv_kpi_f_y = 0.
              ENDIF.

                lv_result-site = 'Tous les sites'.
                lv_result-kpivalue = lv_kpi_f_y.
                lv_result-period = ''.
                lv_result-year = lv_curry.
                lv_result-fromdate = lv_from_date.
                lv_result-todate = lv_to_date.
                lv_result-kpiname = lv_kpiname.
                APPEND lv_result TO et_entityset.
                lv_curry = lv_curry + 1.
              enddo.


      

    WHEN OTHERS.
      lv_trim_start = lv_from_date.

      lv_nbr_tri = ( lv_months_diff ) / 3.
      IF ( lv_months_diff ) MOD 3 <> 0.
        lv_nbr_tri = lv_nbr_tri + 1.
      ENDIF.
      lv_year =  lv_to_date(4) .
      lv_trim_no = 1.

      DO lv_nbr_tri  TIMES.
        lv_trim_end = lv_trim_start.
        CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
          EXPORTING
            date      = lv_trim_start
            days      = 0
            months    = 3
            years     = 0      
            signum    = '+'
          IMPORTING
            calc_date = lv_trim_end.

        IF lv_year <> lv_trim_start(4).
          lv_trim_no = 1.
          lv_year = lv_trim_start(4).
        ELSE.
          lv_trim_no = lv_trim_no + 1.
        ENDIF.
        LOOP AT lt_children ASSIGNING <fs_site_temp>.
          IF <fs_site_temp>-children IS NOT INITIAL.
            CASE lv_kpiname.
              WHEN 't_acc_mal'.
                SELECT COUNT(*)
                  FROM i_incidenttimedata
                   INNER JOIN i_incidentbasicinfo
                      ON i_incidenttimedata~incidentuuid = i_incidentbasicinfo~incidentuuid
                  WHERE i_incidentbasicinfo~ehslocationuuid IN @<fs_site_temp>-children 
                  AND CaseHasPermanentAbsences = 'X'
                  AND incidentdate >= @lv_trim_start
                  AND incidentdate <= @lv_trim_end
                  INTO @lv_kpi_single.
            ENDCASE.
            <fs_site_temp>-kpi = lv_kpi_single.

            SELECT SINGLE ehslocationid
                FROM c_curehslocationinclroothier
                WHERE ehslocationuuid = @<fs_site_temp>-parent
                INTO @lv_result-id.



*            IF lv_kpiname = 't_v_rs'.
*              lv_result-site = 'Tous les sites'.
*            ELSE.
            lv_result-site = <fs_site_temp>-name.
*            ENDIF.
            lv_result-kpivalue = <fs_site_temp>-kpi.
            lv_result-period =  |T{ lv_trim_no }|.
            lv_result-year = lv_trim_start(4).
            lv_result-fromdate = lv_trim_start.
            lv_result-todate = lv_trim_end.
            lv_result-kpiname = lv_kpiname.
            APPEND lv_result TO et_entityset.
            CLEAR lv_result.
            CLEAR lv_kpi_single.
            CLEAR lv_kpi_f.
            CLEAR lv_worked_h.
          ENDIF .
        ENDLOOP .
        lv_trim_start = lv_trim_end.
        "EXIT LOOP IF end date is reached
        IF lv_trim_start > lv_to_date.
          EXIT.
        ENDIF.
*          CLEAR: lt_kpi_temp.
      ENDDO.
      
      
      
      

  ENDCASE.




*BREAK-POINT.
ENDMETHOD.