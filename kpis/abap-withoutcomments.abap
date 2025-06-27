METHOD kpiset_get_entityset.
**TRY.
*CALL METHOD SUPER->KPISET_GET_ENTITYSET
*  EXPORTING
*    IV_ENTITY_NAME           =
*    IV_ENTITY_SET_NAME       =
*    IV_SOURCE_NAME           =
*    IT_FILTER_SELECT_OPTIONS =
*    IS_PAGING                =
*    IT_KEY_TAB               =
*    IT_NAVIGATION_PATH       =
*    IT_ORDER                 =
*    IV_FILTER_STRING         =
*    IV_SEARCH_STRING         =
**    io_tech_request_context  =
**  IMPORTING
**    et_entityset             =
**    es_response_context      =
*    .
**  CATCH /iwbep/cx_mgw_busi_exception.
**  CATCH /iwbep/cx_mgw_tech_exception.
**ENDTRY.

  TYPES: BEGIN OF ty_site_lvl, 
           Level_1  TYPE ehfnd_location_uuid_ref,
           Level_2  TYPE ehfnd_location_uuid_ref,
           Level_3  TYPE ehfnd_location_uuid_ref,
           Level_4  TYPE ehfnd_location_uuid_ref,
           Level_5  TYPE ehfnd_location_uuid_ref,
           Level_6  TYPE ehfnd_location_uuid_ref,
           Level_7  TYPE ehfnd_location_uuid_ref,
           Level_8  TYPE ehfnd_location_uuid_ref,
           Level_9  TYPE ehfnd_location_uuid_ref,
           Level_10 TYPE ehfnd_location_uuid_ref,
         END OF ty_site_lvl.

  DATA: lt_all_sites TYPE TABLE OF ty_site_lvl. 

  DATA: lv_kpi_f      TYPE f,  
        lv_kpi_single TYPE int8, 
        lv_total_sini TYPE int8,
        lv_total_f    TYPE p DECIMALS 2,
        lv_kpi_f_y    TYPE p DECIMALS 2,
        lv_sinis_clot TYPE int8. 
 

  TYPES: BEGIN OF ty_site,
           id   TYPE ehfnd_location_uuid_ref,
           name TYPE string,
           kpi  TYPE i,
         END OF ty_site,
         tty_sites TYPE RANGE OF ehfnd_location_uuid_ref. 
  TYPES: BEGIN OF ty_parent,
           parent   TYPE ehfnd_location_uuid_ref,
           name     TYPE string,
           kpi      TYPE i,
           children TYPE tty_sites,
         END OF ty_parent,
         tty_parent TYPE TABLE OF ty_parent. 

  "Filtres
  DATA: lt_sites      TYPE RANGE OF ernam, "RANGE Has 4 fields: SIGN, OPTION, LOW, HIGH (same structure in oData api when using filter)
        lt_from_date  TYPE RANGE OF ernam, "ernam data element type string
        lt_to_date    TYPE RANGE OF ernam, "we could put this type which is DATA TEST TYPE /IWBEP/S_COD_SELECT_OPTION "which is the type of return of filter"
        lt_kpiName    TYPE RANGE OF ernam,
        lt_directions TYPE RANGE OF ernam,
        lt_ids        TYPE RANGE OF ernam,
        lv_from_date  TYPE d,
        lv_to_date    TYPE d,
        lv_kpiName    TYPE string,
        lv_direction  TYPE string,
        lv_id         TYPE string,
        lv_root       TYPE string.
  DATA: lv_months_diff      TYPE i,                    " Total months difference
        lv_nbr_tri          TYPE i,                    " Total number of quarters
        lv_years_diff       TYPE i,
        lv_remaining_months TYPE i,
        lv_trim_start       TYPE d,                    " Start of the trimester
        lv_trim_end         TYPE d,                    " End of the trimester
        lv_trim_no          TYPE i,
        lv_currM            TYPE i,    " Changed from STRING to INT
        lv_currY            TYPE i,
        lv_tempY            TYPE i.

  DATA: lt_children TYPE tty_parent, " Declare the table variable
        lt_temp     TYPE tty_parent,
        lv_result   LIKE LINE OF et_entityset.

  DATA: lv_kpi_site TYPE i VALUE 0,
        lv_currM_s  TYPE string.

  DATA: lv_worked_h TYPE i VALUE 0.


 

  //get all Filtres
  //You are looping over a table (it_filter_select_options) that contains multiple filter conditions 

    LOOP AT it_filter_select_options ASSIGNING FIELD-SYMBOL(<ls_select>).
    CASE <ls_select>-property.
      WHEN 'kpiName'.
        lt_kpiName = VALUE #( FOR ls_kpiName IN <ls_select>-select_options    "value #() This is the value constructor expression.
                                                                               "It constructs a value (in this case, an internal table) dynamically.
                             ( sign = ls_kpiName-sign                         "for each entry in select_options, it creates a structure with these fields
                               option = ls_kpiName-option
                               low = ls_kpiName-low
                               high = ls_kpiName-high ) ).
      WHEN 'fromDate'.
        lt_from_date = VALUE #( FOR ls_from_date IN <ls_select>-select_options
                            ( sign = ls_from_date-sign
                            low = ls_from_date-low
                            option = ls_from_date-option
                            high = ls_from_date-high )
                           )
        .
      WHEN 'toDate'.
        lt_to_date = VALUE #( FOR ls_to_date IN <ls_select>-select_options
                            ( sign = ls_to_date-sign
                            low = ls_to_date-low
                            option = ls_to_date-option
                            high = ls_to_date-high )
                           )
        .
      WHEN 'id'.
        lt_ids = VALUE #( FOR ls_to_date IN <ls_select>-select_options
                            ( sign = ls_to_date-sign
                            low = ls_to_date-low
                            option = ls_to_date-option
                            high = ls_to_date-high )
                           )
        .
      WHEN OTHERS.
    ENDCASE.
  ENDLOOP.

  //It could be multiple values
  //table in abap starts with index 1

  lv_kpiName = lt_kpiName[ 1 ]-low.
  lv_from_date = lt_from_date[ 1 ]-low.
  lv_to_date = lt_to_date[ 1 ]-low.
  lv_id = lt_ids[ 1 ]-low.



  lv_years_diff = ( lv_to_date(4) - lv_from_date(4) ) * 12. 
  lv_remaining_months = ( lv_to_date+4(2) - lv_from_date+4(2) ).  
  lv_months_diff = lv_years_diff + lv_remaining_months. 
  lv_currM = lv_from_date+4(2).
  lv_currY = lv_from_date(4).
  DATA(lv_years_number) = ( lv_to_date(4) - lv_from_date(4) ).




  DATA: id        TYPE string,
        lv_parent TYPE ehfnd_location_uuid_ref.
  id = lv_id.

  id = |{ lv_id WIDTH = 20 ALIGN = RIGHT PAD = '0' }|. 
  SELECT SINGLE ehslocationuuid
    FROM c_curehslocationinclroothier
    WHERE ehslocationid = @id
    INTO @lv_parent.

  SELECT ehslocationuuid AS parent,  ehslocationname AS name
    FROM c_curehslocationinclroothier
    WHERE ehsparentlocationuuid = @lv_parent
    INTO CORRESPONDING FIELDS OF TABLE @lt_temp.


  "Get All children of all levels
  LOOP AT lt_temp ASSIGNING FIELD-SYMBOL(<fs_parent>).
    SELECT
    a~ehslocationuuid  AS Level_1,
    b~ehslocationuuid AS Level_2,
    c~ehslocationuuid AS Level_3,
    d~ehslocationuuid AS Level_4,
    e~ehslocationuuid AS Level_5,
    f~ehslocationuuid AS Level_6,
    g~ehslocationuuid AS Level_7,
    h~ehslocationuuid AS Level_8,
    i~ehslocationuuid AS Level_9,
    j~ehslocationuuid AS Level_10
  INTO TABLE @lt_all_sites
  FROM c_curehslocationinclroothier AS a
  LEFT JOIN c_curehslocationinclroothier AS b ON a~ehslocationuuid = b~ehsparentlocationuuid
  LEFT JOIN c_curehslocationinclroothier AS c ON b~ehslocationuuid = c~ehsparentlocationuuid
  LEFT JOIN c_curehslocationinclroothier AS d ON c~ehslocationuuid = d~ehsparentlocationuuid
  LEFT JOIN c_curehslocationinclroothier AS e ON d~ehslocationuuid = e~ehsparentlocationuuid
  LEFT JOIN c_curehslocationinclroothier AS f ON e~ehslocationuuid = f~ehsparentlocationuuid
  LEFT JOIN c_curehslocationinclroothier AS g ON f~ehslocationuuid = g~ehsparentlocationuuid
  LEFT JOIN c_curehslocationinclroothier AS h ON g~ehslocationuuid = h~ehsparentlocationuuid
  LEFT JOIN c_curehslocationinclroothier AS i ON h~ehslocationuuid = i~ehsparentlocationuuid
  LEFT JOIN c_curehslocationinclroothier AS j ON i~ehslocationuuid = j~ehsparentlocationuuid
  WHERE a~ehslocationuuid = @<fs_parent>-parent.


    "get all sites from all levels
    "get all sites for a specific parent !!! for exampl
    LOOP AT lt_all_sites ASSIGNING FIELD-SYMBOL(<fs_site_lvl>).
      IF <fs_site_lvl>-Level_1 =  <fs_parent>-parent.
        IF <fs_site_lvl>-Level_1 IS NOT INITIAL.
          APPEND VALUE #( sign = 'I' option = 'EQ' low = <fs_site_lvl>-Level_1 ) TO <fs_parent>-children.

        ENDIF.
        IF <fs_site_lvl>-Level_2 IS NOT INITIAL.
          APPEND VALUE #( sign = 'I' option = 'EQ' low = <fs_site_lvl>-Level_2 ) TO <fs_parent>-children.
        ENDIF.
        IF <fs_site_lvl>-Level_3 IS NOT INITIAL.
          APPEND VALUE #( sign = 'I' option = 'EQ' low = <fs_site_lvl>-Level_3 ) TO <fs_parent>-children.
        ENDIF.
        IF <fs_site_lvl>-Level_4 IS NOT INITIAL.
          APPEND VALUE #( sign = 'I' option = 'EQ' low = <fs_site_lvl>-Level_4 ) TO <fs_parent>-children.
        ENDIF.
        IF <fs_site_lvl>-Level_5 IS NOT INITIAL.
          APPEND VALUE #( sign = 'I' option = 'EQ' low = <fs_site_lvl>-Level_5 ) TO <fs_parent>-children.
        ENDIF.
        IF <fs_site_lvl>-Level_6 IS NOT INITIAL.
          APPEND VALUE #( sign = 'I' option = 'EQ' low = <fs_site_lvl>-Level_6 ) TO <fs_parent>-children.
        ENDIF.
        IF <fs_site_lvl>-Level_7 IS NOT INITIAL.
          APPEND VALUE #( sign = 'I' option = 'EQ' low = <fs_site_lvl>-Level_7 ) TO <fs_parent>-children.
        ENDIF.
        IF <fs_site_lvl>-Level_8 IS NOT INITIAL.
          APPEND VALUE #( sign = 'I' option = 'EQ' low = <fs_site_lvl>-Level_8 ) TO <fs_parent>-children.
        ENDIF.
        IF <fs_site_lvl>-Level_9 IS NOT INITIAL.
          APPEND VALUE #( sign = 'I' option = 'EQ' low = <fs_site_lvl>-Level_9 ) TO <fs_parent>-children.
        ENDIF.
        IF <fs_site_lvl>-Level_10 IS NOT INITIAL.
          APPEND VALUE #( sign = 'I' option = 'EQ' low = <fs_site_lvl>-Level_10 ) TO <fs_parent>-children.
        ENDIF.
      ENDIF.

    ENDLOOP.

    CLEAR lt_all_sites.

  ENDLOOP.

 
  LOOP AT lt_temp ASSIGNING FIELD-SYMBOL(<fs>).
    SORT <fs>-children BY low.
    DELETE ADJACENT DUPLICATES FROM <fs>-children COMPARING low.
  ENDLOOP.


  APPEND LINES OF lt_temp TO lt_children.

  "datetime
  DATA: lv_start_hhmmss TYPE string VALUE '081500'.
  DATA: lv_end_hhmmss TYPE string VALUE '081500'.
  "Monthly kpis
  CASE lv_kpiname.
    WHEN 'acc_trav' OR 'maladie-pro' OR 'acc_trajet' OR 'taux_clot' OR 'taux_pre' OR 'nbr_pres' OR 'taux_met'. "For monthli kpis !
      DO lv_months_diff + 1 TIMES. "we added 1 to get the kpi of last month
        IF lv_currm < 10.
          lv_currM_s = |0{ lv_currm }|. "we need this in sql to compare ! 
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
              WHEN 'maladie-pro'.
                SELECT COUNT( i_incidentbasicinfo~incidentuuid )
                     FROM i_incidentbasicinfo
                      INNER JOIN I_IncidentInjuryIllness
                     ON i_incidentbasicinfo~incidentuuid = I_IncidentInjuryIllness~incidentuuid
                     WHERE i_incidentbasicinfo~ehslocationuuid IN @<fs_site_temp>-children
                         AND substring( CAST( i_incidentbasicinfo~incidentdate AS CHAR ), 5, 2 ) = @lv_currM_s
                         AND substring( CAST( i_incidentbasicinfo~incidentdate AS CHAR ), 1 , 4 ) =  @lv_curry
                        AND ( I_IncidentInjuryIllness~injuryillnessclassification = 'EHHSS_ILLC_HEAR'
                          OR I_IncidentInjuryIllness~injuryillnessclassification = 'EHHSS_ILLC_OTHER_ILL'
                          OR I_IncidentInjuryIllness~injuryillnessclassification = 'EHHSS_ILLC_RESP' )
*                     GROUP BY
*                        incident~ehslocationuuid, injuryillnessclassification
                     INTO @lv_kpi_single.
              WHEN 'acc_trajet'.
                SELECT COUNT(*)
                      FROM i_incidentinjuredpersoninfo AS inc_info
                        INNER JOIN i_incidentbasicinfo AS incident
                        ON inc_info~incidentuuid = incident~incidentuuid
                      WHERE ehslocationuuid IN @<fs_site_temp>-children
                        AND substring( CAST( incidentdate AS CHAR ), 5, 2 ) = @lv_currM_s
                        AND substring( CAST( incidentdate AS CHAR ), 1 , 4 ) =  @lv_curry
                        AND incidenttype = 'EHHSS_OIT_ACC_ON_WAY'
                     INTO @lv_kpi_single.

                "Taux de clôture des accident de travail ONEE métier
                "(Nbre d'entrées CDS R_EHSIncidentTP when status =3/Nbre d'entrée InjuryIllnessUUID dans CDS: I_IncidentInjuryIllness)*100
              WHEN 'taux_met'.
                SELECT COUNT(*)
                       FROM i_incidentinjuredpersoninfo AS inc_info
                         INNER JOIN i_incidentbasicinfo AS incident
                         ON inc_info~incidentuuid = incident~incidentuuid
*                        INNER JOIN R_EHSIncidentTP AS r_ehs
*                        ON inc_info~incidentuuid = r_ehs~incidentuuid
                       WHERE ehslocationuuid IN @<fs_site_temp>-children
                         AND substring( CAST( incidentdate AS CHAR ), 5, 2 ) = @lv_currM_s
                         AND substring( CAST( incidentdate AS CHAR ), 1 , 4 ) =  @lv_curry
                         AND incidenttype = 'ZEHHSS_OIT_ACC_PRE'
                      INTO @lv_sinis_clot.


                SELECT COUNT(*)
                    FROM  I_IncidentInjuryIllness AS inj
                      INNER JOIN i_incidentbasicinfo AS incident
                      ON inj~incidentuuid = incident~incidentuuid
                    WHERE ehslocationuuid IN @<fs_site_temp>-children
                      AND substring( CAST( incidentdate AS CHAR ), 5, 2 ) = @lv_currM_s
                      AND substring( CAST( incidentdate AS CHAR ), 1 , 4 ) =  @lv_curry
                   INTO @DATA(lv_total_inj).



                IF lv_total_inj <> 0.
                  lv_total_f = lv_sinis_clot / lv_total_inj. "check formule plz
                  lv_kpi_single = lv_total_f * 100.

                ELSE.
                  lv_kpi_single =  0.
                ENDIF.


                "Taux de clôture des accident de travail ONEE trajet
              WHEN 'taux_clot'.
                "(Step 1 : Select INCIDENTUUID from I_IncidentInjuredPersonInfo où EHHSS_OCC_INC_TYPE_CODE_NCONV when INCIDENTTYPE =EHHSS_OIT_ACC_ON_WAY
                "Step 2 :  when I_IncidentInjuredPersonInfo-INCIDENTUUID = R_EHSIncidentTP-INCIDENTUUID ,
                "Select from R_EHSIncidentTPINCIDENTSTATUS= 3 /Nbre d'entrées dans CDS I_IncidentInjuredPersonInfo où EHHSS_OCC_INC_TYPE_CODE_NCONV= EHHSS_OIT_ACC_ON_WAY)*100
                SELECT COUNT(*)
                       FROM i_incidentinjuredpersoninfo AS inc_info
                         INNER JOIN i_incidentbasicinfo AS incident
                         ON inc_info~incidentuuid = incident~incidentuuid
*                        INNER JOIN R_EHSIncidentTP AS r_ehs
*                        ON inc_info~incidentuuid = r_ehs~incidentuuid
                       WHERE ehslocationuuid IN @<fs_site_temp>-children
                         AND substring( CAST( incidentdate AS CHAR ), 5, 2 ) = @lv_currM_s
                         AND substring( CAST( incidentdate AS CHAR ), 1 , 4 ) =  @lv_curry
                         AND incidenttype = 'EHHSS_OIT_ACC_ON_WAY'
                      INTO @lv_sinis_clot.


                SELECT COUNT(*)
                    FROM i_incidentinjuredpersoninfo AS inc_info
                      INNER JOIN i_incidentbasicinfo AS incident
                      ON inc_info~incidentuuid = incident~incidentuuid
                      INNER JOIN R_EHSIncidentTP AS r_ehs
                      ON incident~incidentuuid = r_ehs~incidentuuid
                    WHERE ehslocationuuid IN @<fs_site_temp>-children
                    AND incidentstatus = '03'
                      AND substring( CAST( incidentdate AS CHAR ), 5, 2 ) = @lv_currM_s
                      AND substring( CAST( incidentdate AS CHAR ), 1 , 4 ) =  @lv_curry
                   INTO @lv_total_sini.

                IF lv_total_sini <> 0.
                  lv_total_f = lv_sinis_clot / lv_total_sini.
                  lv_kpi_single =  lv_total_f * 100.
                ELSE.
                  lv_kpi_single =  0.
                ENDIF.

                "Nombre des accident de travail pretataire
                "Nbre d'entrées CDS R_EHSIncidentTP when status =3 /Nbre d'entrées dans CDS I_IncidentInjuredPersonInfo où EHHSS_OCC_INC_TYPE_CODE_NCONV= Prestataire
              WHEN 'nbr_pres'.
                SELECT COUNT(*)
                       FROM i_incidentinjuredpersoninfo AS inc_info
                         INNER JOIN i_incidentbasicinfo AS incident
                         ON inc_info~incidentuuid = incident~incidentuuid
*                        INNER JOIN R_EHSIncidentTP AS r_ehs
*                        ON inc_info~incidentuuid = r_ehs~incidentuuid
                       WHERE ehslocationuuid IN @<fs_site_temp>-children
                         AND substring( CAST( incidentdate AS CHAR ), 5, 2 ) = @lv_currM_s
                         AND substring( CAST( incidentdate AS CHAR ), 1 , 4 ) =  @lv_curry
                         AND incidenttype = 'ZEHHSS_OIT_ACC_PRE'
                      INTO @lv_sinis_clot.


                SELECT COUNT(*)
                    FROM i_incidentinjuredpersoninfo AS inc_info
                      INNER JOIN i_incidentbasicinfo AS incident
                      ON inc_info~incidentuuid = incident~incidentuuid
                      INNER JOIN R_EHSIncidentTP AS r_ehs
                      ON incident~incidentuuid = r_ehs~incidentuuid
                    WHERE ehslocationuuid IN @<fs_site_temp>-children
                    AND incidentstatus = '03'
                      AND substring( CAST( incidentdate AS CHAR ), 5, 2 ) = @lv_currM_s
                      AND substring( CAST( incidentdate AS CHAR ), 1 , 4 ) =  @lv_curry
                   INTO @lv_total_sini.

                IF lv_total_sini <> 0.
                  lv_total_f = lv_sinis_clot / lv_total_sini. "check formule plz
                  lv_kpi_single = lv_total_f.

                ELSE.
                  lv_kpi_single =  0.
                ENDIF.

                "Taux de clôture des accident de travail pretataire
                "(Nombre de sinistres prestataire clôturés / Nombre total de sinistres prestataire) x 100
              WHEN 'taux_pre'.
                "(Step 1 :  Select INCIDENTUUID from I_IncidentInjuredPersonInfo où EHHSS_OCC_INC_TYPE_CODE_NCONV when INCIDENTTYPE =ZEHHSS_OIT_ACC_PRE
                "Step 2 :  when I_IncidentInjuredPersonInfo-INCIDENTUUID = R_EHSIncidentTP-INCIDENTUUID ,
                "Select from R_EHSIncidentTPINCIDENTSTATUS= 3 /Nbre d'entrées dans CDS I_IncidentInjuredPersonInfo où EHHSS_OCC_INC_TYPE_CODE_NCONV= ZEHHSS_OIT_ACC_PRE)*100
                SELECT COUNT(*)
                       FROM i_incidentinjuredpersoninfo AS inc_info
                         INNER JOIN i_incidentbasicinfo AS incident
                         ON inc_info~incidentuuid = incident~incidentuuid
*                        INNER JOIN R_EHSIncidentTP AS r_ehs
*                        ON inc_info~incidentuuid = r_ehs~incidentuuid
                       WHERE ehslocationuuid IN @<fs_site_temp>-children
                         AND substring( CAST( incidentdate AS CHAR ), 5, 2 ) = @lv_currM_s
                         AND substring( CAST( incidentdate AS CHAR ), 1 , 4 ) =  @lv_curry
                         AND incidenttype = 'ZEHHSS_OIT_ACC_PRE'
                      INTO @lv_sinis_clot.


                SELECT COUNT(*)
                    FROM i_incidentinjuredpersoninfo AS inc_info
                      INNER JOIN i_incidentbasicinfo AS incident
                      ON inc_info~incidentuuid = incident~incidentuuid
                      INNER JOIN R_EHSIncidentTP AS r_ehs
                      ON incident~incidentuuid = r_ehs~incidentuuid
                    WHERE ehslocationuuid IN @<fs_site_temp>-children
                    AND incidentstatus = '03'
                      AND substring( CAST( incidentdate AS CHAR ), 5, 2 ) = @lv_currM_s
                      AND substring( CAST( incidentdate AS CHAR ), 1 , 4 ) =  @lv_curry
                   INTO @lv_total_sini.

                IF lv_total_sini <> 0.
                  lv_total_f = lv_sinis_clot / lv_total_sini.
                  lv_kpi_single =  lv_total_f * 100.
                ELSE.
                  lv_kpi_single =  0.
                ENDIF.


            ENDCASE.

          ENDIF.

          <fs_site_temp>-kpi = lv_kpi_single.

          SELECT SINGLE ehslocationid
              FROM c_curehslocationinclroothier
              WHERE ehslocationuuid = @<fs_site_temp>-parent
              INTO @lv_result-id.
          lv_result-site = <fs_site_temp>-name.
          lv_result-kpivalue = <fs_site_temp>-kpi.
          lv_result-period = lv_currm.  "filter should not be null
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



      "Yearly kpi

      ""Taux de Réalisation des Tests d'Échantillons
      ""(Nbre of entries from Ehhss_Sampling_Campaign_Allres when STATUS =3 / Nbre of entries from Ehhss_Sampling_Campaign_Allres)*100
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

      "Taux de modification acceptés
      "(Nombre de modifications acceptés / Nombre total des demandes de modifications ) x 100
      "(Nbre of entries from C_CHGMGMTREQWORKLIST when CHGMGMTREQSTATUS = 3)/Nbre of entries from C_CHGMGMTREQWORKLIST)*100

    WHEN 'y_ma'.
      lv_tempY = lv_to_date(4) - lv_from_date(4).
      DO lv_tempY + 1 TIMES.
        .
        SELECT COUNT(*)
          FROM c_chgmgmtreqworklist
          WHERE chgmgmtreqstatus = '03'
          AND substring( CAST( chgmgmtreqcreationdate AS CHAR ), 1 , 4 ) =  @lv_curry " Or CHGMGMTREQREPORTEDDATE
          INTO @DATA(lv_modif_accepted).

        SELECT COUNT(*)
          FROM c_chgmgmtreqworklist
          WHERE substring( CAST( chgmgmtreqcreationdate AS CHAR ), 1 , 4 ) =  @lv_curry " Or CHGMGMTREQREPORTEDDATE
          INTO @DATA(lv_total_modif).


        IF lv_total_modif <> 0.
          lv_kpi_f_y =  lv_modif_accepted / lv_total_modif.
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

      "Nombre de demande de modification refusé
      "Nbre of entries from C_CHGMGMTREQWORKLIST when CHGMGMTREQSTATUS = 7
    WHEN 'y_mr'.
      lv_tempY = lv_to_date(4) - lv_from_date(4).
      DO lv_tempY + 1 TIMES.
        SELECT COUNT(*)
          FROM c_chgmgmtreqworklist
          WHERE chgmgmtreqstatus = '07' "Pas de status 07 dans la bd, il faut l'ajouter pour tester
          AND substring( CAST( chgmgmtreqcreationdate AS CHAR ), 1 , 4 ) =  @lv_curry
          INTO @DATA(lv_modif_ref).


        lv_result-site = 'Tous les sites'.
        lv_result-kpivalue = lv_modif_ref.
        lv_result-period = ''.
        lv_result-year = lv_curry.
        lv_result-fromdate = lv_from_date.
        lv_result-todate = lv_to_date.
        lv_result-kpiname = lv_kpiname.
        APPEND lv_result TO et_entityset.
        lv_curry = lv_curry + 1.

      ENDDO.


      "Nbre d'audit Audit de certif/Suivi/Renouv/Int/à blanc via SAP EHS
      "Nbre Of entries from PLMM_AUDITT when AUDIT_TYPE = 11
    WHEN 'y_a_cs'.
      lv_tempY = lv_to_date(4) - lv_from_date(4).
      DO lv_tempY + 1 TIMES.
        SELECT COUNT(*)
          FROM plmm_audit
          WHERE audit_type = '11'
          AND substring( CAST( released_on AS CHAR ), 1 , 4 ) =  @lv_curry
          INTO @DATA(lv_release).


        lv_result-site = 'Tous les sites'.
        lv_result-kpivalue = lv_release.
        lv_result-period = ''.
        lv_result-year = lv_curry.
        lv_result-fromdate = lv_from_date.
        lv_result-todate = lv_to_date.
        lv_result-kpiname = lv_kpiname.
        APPEND lv_result TO et_entityset.
        lv_curry = lv_curry + 1.

      ENDDO.


      "Taux de réalisation des activités des demandes de modifications
      "(Nombre des activités des demandes de modifications Terminées / Nombre des activités des demandes de modifications) x 100
      "(Nbre of entries from C_ChgMgmtActyOverviewQuery when CHGMGMTREQSTATUS = 8 /Nbre of entries from C_ChgMgmtActyOverviewQuery)*100

    WHEN 'y_ram'.
      lv_tempY = lv_to_date(4) - lv_from_date(4).
      DO lv_tempY + 1 TIMES.
        SELECT COUNT(*)
         FROM c_chgmgmtreqworklist
         WHERE chgmgmtreqstatus = '08'
          AND substring( CAST( chgmgmtreqcreationdate AS CHAR ), 1 , 4 ) =  @lv_curry
         INTO @DATA(lv_modif_done).

        SELECT COUNT(*)
          FROM c_chgmgmtreqworklist
          WHERE substring( CAST( chgmgmtreqcreationdate AS CHAR ), 1 , 4 ) =  @lv_curry
          INTO @lv_total_modif.


        IF lv_total_modif <> 0.
          lv_kpi_f_y =  lv_modif_done / lv_total_modif.
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

      "Nombre des Non-conformités majeures S&ST
      "Nbre Of entries from PLMM_QUEST_RES when ASSESSMENT_RES = 0
    WHEN 'y_ncmaj'.
      lv_tempY = lv_to_date(4) - lv_from_date(4).
      DO lv_tempY + 1 TIMES.
        SELECT COUNT(*)
          FROM plmm_quest_res
          WHERE assessment_res = '0'
          AND substring( CAST( changeddatetime AS CHAR ), 1 , 4 ) =  @lv_curry "Didn't find another date in this table
          INTO @DATA(lv_ncm).


        lv_result-site = 'Tous les sites'.
        lv_result-kpivalue = lv_ncm.
        lv_result-period = ''.
        lv_result-year = lv_curry.
        lv_result-fromdate = lv_from_date.
        lv_result-todate = lv_to_date.
        lv_result-kpiname = lv_kpiname.
        APPEND lv_result TO et_entityset.
        lv_curry = lv_curry + 1.

      ENDDO.

      "Nombre des Non-conformités mineures S&ST
      "Nbre Of entries from PLMM_QUEST_RES when ASSESSMENT_RES = 4
    WHEN 'y_ncmin'.
      lv_tempY = lv_to_date(4) - lv_from_date(4).
      DO lv_tempY + 1 TIMES.
        SELECT COUNT(*)
          FROM plmm_quest_res
          WHERE assessment_res = '4'
          AND substring( CAST( changeddatetime AS CHAR ), 1 , 4 ) =  @lv_curry "Didn't find another date in this table
          INTO @lv_ncm.


        lv_result-site = 'Tous les sites'.
        lv_result-kpivalue = lv_ncm.
        lv_result-period = ''.
        lv_result-year = lv_curry.
        lv_result-fromdate = lv_from_date.
        lv_result-todate = lv_to_date.
        lv_result-kpiname = lv_kpiname.
        APPEND lv_result TO et_entityset.
        lv_curry = lv_curry + 1.
      ENDDO.


      "Taux de traitement des Non Conformités majeures S&ST
      "(Nombre de NC Maj auxquels une mesure est affectée/Nombre total de NC Maj)*100
      "(Nbre Of entries from PLMM_QUEST_RES when ASSESSMENT_RES = 0 and CORR_REQUIRED = X / Nbre Of entries from PLMM_QUEST_RES when ASSESSMENT_RES = 0) *100
    WHEN 'y_tnc_maj'.
      lv_tempY = lv_to_date(4) - lv_from_date(4).
      DO lv_tempY + 1 TIMES.
        SELECT COUNT(*)
      FROM plmm_quest_res
      WHERE assessment_res = '0'
      AND substring( CAST( changeddatetime AS CHAR ), 1 , 4 ) =  @lv_curry "Didn't find another date in this table
      AND corr_required = 'X'
      INTO @DATA(lv_nc_maj_aff).

        SELECT COUNT(*)
         FROM plmm_quest_res
         WHERE assessment_res = '0'
          AND substring( CAST( changeddatetime AS CHAR ), 1 , 4 ) =  @lv_curry "Didn't find another date in this table
         INTO @DATA(lv_nc_maj_total).

        IF lv_nc_maj_total <> 0.
          lv_kpi_f_y =  lv_nc_maj_aff / lv_nc_maj_total.
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


      "Taux de traitement des Non Conformités mineures S&ST
      "(Nombre de NC Min auxquels une mesure est affectée/Nombre total de NC Min)*100
      "(Nbre Of entries from PLMM_QUEST_RES when ASSESSMENT_RES = 4 and CORR_REQUIRED = X / Nbre Of entries from PLMM_QUEST_RES when ASSESSMENT_RES = 4) *100
    WHEN 'y_tnc_min'.
      lv_tempY = lv_to_date(4) - lv_from_date(4).
      DO lv_tempY + 1 TIMES.
        SELECT COUNT(*)
      FROM plmm_quest_res
      WHERE assessment_res = '4'
      AND substring( CAST( changeddatetime AS CHAR ), 1 , 4 ) =  @lv_curry "Didn't find another date in this table
      AND corr_required = 'X'
      INTO @DATA(lv_nc_min_aff).

        SELECT COUNT(*)
         FROM plmm_quest_res
         WHERE assessment_res = '4'
          AND substring( CAST( changeddatetime AS CHAR ), 1 , 4 ) =  @lv_curry "Didn't find another date in this table
         INTO @DATA(lv_nc_min_total).

        IF lv_nc_min_total <> 0.
          lv_kpi_f_y =  lv_nc_min_aff / lv_nc_min_total.
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

      "Taux de clôture des accident de travail ONEE métier
      "(Nombre de sinistres ONEE métier clôturés / Nombre total de sinistres ONEE métier) x 100
      "(Nbre d'entrées CDS R_EHSIncidentTP when status =3/Nbre d'entrée InjuryIllnessUUID dans CDS: I_IncidentInjuryIllness)*100


    WHEN 'y_tc_acc'.
      lv_tempY = lv_to_date(4) - lv_from_date(4).
      DO lv_tempY + 1 TIMES.
        SELECT COUNT(*)
         FROM R_EHSIncidentTP
         WHERE incidentstatus = '03'
         AND substring( CAST( creationdatetime AS CHAR ), 1 , 4 ) =  @lv_curry " CREATIONDATETIME or LASTCHANGEDATETIME
         INTO @DATA(lv_sin_clot).

        SELECT COUNT( InjuryIllnessUUID )
          FROM I_IncidentInjuryIllness
          WHERE substring( CAST( creationdatetime AS CHAR ), 1 , 4 ) =  @lv_curry
          INTO @DATA(lv_total_sin).

        IF lv_total_sin <> 0.
          lv_kpi_f_y =  lv_sin_clot / lv_total_sin.
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


      "Mise en oeuvre des contrôles réglementaires
      "Nbre Of entries from PLMM_AUDITT when AUDIT_TYPE = 18
    WHEN 'y_mo_cr'.
      lv_tempY = lv_to_date(4) - lv_from_date(4).
      DO lv_tempY + 1 TIMES.




        SELECT COUNT(*)
          FROM plmm_audit
          WHERE audit_type = '18'
          AND substring( CAST( changeddatetime AS CHAR ), 1 , 4 ) =  @lv_curry
          INTO @DATA(lv_cr).


        lv_result-site = 'Tous les sites'.
        lv_result-kpivalue = lv_cr.
        lv_result-period = ''.
        lv_result-year = lv_curry.
        lv_result-fromdate = lv_from_date.
        lv_result-todate = lv_to_date.
        lv_result-kpiname = lv_kpiname.
        APPEND lv_result TO et_entityset.
        lv_curry = lv_curry + 1.

      ENDDO.



      "Nombre des audits de sécurité (Contrôle SST) via SAP EHS
      "Nombre total de non-conformités détectées lors des audits
      "Nbre Of entries from PLMM_AUDITT when AUDIT_TYPE = 90
    WHEN 'y_aud_sec'.
      lv_tempY = lv_to_date(4) - lv_from_date(4).
      DO lv_tempY + 1 TIMES.
        SELECT COUNT(*)
          FROM plmm_audit
          WHERE audit_type = '90'
          AND substring( CAST( released_on AS CHAR ), 1 , 4 ) =  @lv_curry
          INTO @DATA(lv_audit_sec).


        lv_result-site = 'Tous les sites'.
        lv_result-kpivalue = lv_cr.
        lv_result-period = ''.
        lv_result-year = lv_curry.
        lv_result-fromdate = lv_from_date.
        lv_result-todate = lv_to_date.
        lv_result-kpiname = lv_kpiname.
        APPEND lv_result TO et_entityset.
        lv_curry = lv_curry + 1.

      ENDDO.


      "Nbr Of entries in  I_IncidentInjuryIllness with InjuryIllnessClassificationt = Perte auditive
      "Nombre d'incidents liés au bruit
      "Nombre total d'incidents concernant le bruit.

    WHEN 'y_tbr'.
      lv_tempY = lv_to_date(4) - lv_from_date(4).
      DO lv_tempY + 1 TIMES.
        LOOP AT lt_children ASSIGNING FIELD-SYMBOL(<fs_site_y_temp>).
          IF <fs_site_y_temp>-children IS NOT INITIAL.


            SELECT COUNT( i_incidentbasicinfo~incidentuuid )
                 FROM i_incidentbasicinfo
                  INNER JOIN I_IncidentInjuryIllness
                 ON i_incidentbasicinfo~incidentuuid = I_IncidentInjuryIllness~incidentuuid
                 WHERE i_incidentbasicinfo~ehslocationuuid IN @<fs_site_y_temp>-children
                     AND substring( CAST( i_incidentbasicinfo~incidentdate AS CHAR ), 1, 4 ) = @lv_curry
                    AND I_IncidentInjuryIllness~injuryillnessclassification = 'EHHSS_ILLC_HEAR'
                 INTO @lv_cr.

            SELECT SINGLE ehslocationid
          FROM c_curehslocationinclroothier
          WHERE ehslocationuuid = @<fs_site_y_temp>-parent
          INTO @lv_result-id.

            lv_result-site = <fs_site_y_temp>-name.
            lv_result-kpivalue = lv_cr.
            lv_result-period = ''.
            lv_result-year = lv_curry.
            lv_result-fromdate = lv_from_date.
            lv_result-todate = lv_to_date.
            lv_result-kpiname = lv_kpiname.
            APPEND lv_result TO et_entityset.
          ENDIF.
        ENDLOOP.

        lv_curry = lv_curry + 1.
        DATA(lv_last_y) = lv_to_date(4).
        IF lv_curry > lv_last_y.
          EXIT.
        ENDIF.

      ENDDO.


      ""Niveaux de bruit sur le site de travail
      "Nombre des mesures Bruit "
      "Nbr Of entries from Ehhss_Sampling_Campaign_Allres when VALUEUNIT = db(A)
      WHEN 'y_nbst'.
     lv_tempY = lv_to_date(4) - lv_from_date(4).
      DO lv_tempY + 1 TIMES.

          SELECT COUNT(*) FROM Ehhss_Sampling_Campaign_Allres
          WHERE VALUEUNIT = 'DBA'
          AND substring( CAST( START_DATE AS CHAR ), 1 , 4 ) =  @lv_curry
          INTO @DATA(lv_br).

        lv_result-site = 'Tous les sites'.
        lv_result-kpivalue = lv_br.
        lv_result-period = ''.
        lv_result-year = lv_curry.
        lv_result-fromdate = lv_from_date.
        lv_result-todate = lv_to_date.
        lv_result-kpiname = lv_kpiname.
        APPEND lv_result TO et_entityset.
        lv_curry = lv_curry + 1.
      enddo.

      "mise en place des mesures de protection collective dont les zones ou le bruit dépasse 85 (dB)
      "Nbr Of entries from Ehhss_Sampling_Campaign_Allres when VALUEUNIT = db(A) and VALUE > 85

      WHEN 'y_mep_br'.
     lv_tempY = lv_to_date(4) - lv_from_date(4).
      DO lv_tempY + 1 TIMES.

          SELECT COUNT(*) FROM Ehhss_Sampling_Campaign_Allres
          WHERE VALUEUNIT = 'DBA'
            AND VALUE > 85
          AND substring( CAST( START_DATE AS CHAR ), 1 , 4 ) =  @lv_curry
          INTO @lv_br.

        lv_result-site = 'Tous les sites'.
        lv_result-kpivalue = lv_br.
        lv_result-period = ''.
        lv_result-year = lv_curry.
        lv_result-fromdate = lv_from_date.
        lv_result-todate = lv_to_date.
        lv_result-kpiname = lv_kpiname.
        APPEND lv_result TO et_entityset.
        lv_curry = lv_curry + 1.
      enddo.

      "Nbr Of entries from Ehhss_Sampling_Campaign_Allres when VALUEUNIT = db(A) and VALUE from 85db(A) to 90 bd(A)
      "Mise en place des mesures de protection dont les zones ou le bruit 85-90 (mise en place des EPI)
      WHEN 'y_mep_epi'.
     lv_tempY = lv_to_date(4) - lv_from_date(4).
      DO lv_tempY + 1 TIMES.

          SELECT COUNT(*) FROM Ehhss_Sampling_Campaign_Allres
          WHERE VALUEUNIT = 'DBA'
            AND VALUE >= 85
            AND vALUE <= 90
          AND substring( CAST( START_DATE AS CHAR ), 1 , 4 ) =  @lv_curry
          INTO @lv_br.

        lv_result-site = 'Tous les sites'.
        lv_result-kpivalue = lv_br.
        lv_result-period = ''.
        lv_result-year = lv_curry.
        lv_result-fromdate = lv_from_date.
        lv_result-todate = lv_to_date.
        lv_result-kpiname = lv_kpiname.
        APPEND lv_result TO et_entityset.
        lv_curry = lv_curry + 1.
      enddo.

      "Nbr Of entries from Ehhss_Sampling_Campaign_Allres when VALUEUNIT = db(A) and VALUE >105
      "Mise en place des mesures de protection dont les zones ou le bruit dépasse 105 dB par x min
      WHEN 'y_mep_br2'.
     lv_tempY = lv_to_date(4) - lv_from_date(4).
      DO lv_tempY + 1 TIMES.

          SELECT COUNT(*) FROM Ehhss_Sampling_Campaign_Allres
          WHERE VALUEUNIT = 'DBA'
            AND VALUE > 105
          AND substring( CAST( START_DATE AS CHAR ), 1 , 4 ) =  @lv_curry
          INTO @lv_br.

        lv_result-site = 'Tous les sites'.
        lv_result-kpivalue = lv_br.
        lv_result-period = ''.
        lv_result-year = lv_curry.
        lv_result-fromdate = lv_from_date.
        lv_result-todate = lv_to_date.
        lv_result-kpiname = lv_kpiname.
        APPEND lv_result TO et_entityset.
        lv_curry = lv_curry + 1.
      enddo.

      "Niveaux d'éclairage moyen sur le lieu de travail
   "Nbr Of entries from Ehhss_Sampling_Campaign_Allres when VALUEUNIT = db(A) and VALUE >105
      WHEN 'y_ecl'.
     lv_tempY = lv_to_date(4) - lv_from_date(4).
      DO lv_tempY + 1 TIMES.

          SELECT COUNT(*) FROM Ehhss_Sampling_Campaign_Allres
          WHERE VALUEUNIT = 'DBA'
            AND VALUE > 105
          AND substring( CAST( START_DATE AS CHAR ), 1 , 4 ) =  @lv_curry
          INTO @lv_br.

        lv_result-site = 'Tous les sites'.
        lv_result-kpivalue = lv_br.
        lv_result-period = ''.
        lv_result-year = lv_curry.
        lv_result-fromdate = lv_from_date.
        lv_result-todate = lv_to_date.
        lv_result-kpiname = lv_kpiname.
        APPEND lv_result TO et_entityset.
        lv_curry = lv_curry + 1.
      enddo.

      " new new new
      "Niveaux d'éclairage moyen sur le lieu de travail
      "Mesure moyenne des niveaux d'éclairage (lux) dans différentes zones de travail.

      "Sum of VALUE  from Ehhss_Sampling_Campaign_Allres when AGENT_KEY_REF = Lumière
      "/

      WHEN 'y_lum'.
        lv_tempY = lv_to_date(4) - lv_from_date(4).
        DO lv_tempY + 1 TIMES.

            SELECT COUNT(*) FROM Ehhss_Sampling_Campaign_Allres
             WHERE AGENT_KEY_REF = '2FE3F76090A11EDF80E4E6D147C04A6C' "Change id of 'lumiere'
            AND substring( CAST( START_DATE AS CHAR ), 1 , 4 ) =  @lv_curry
            INTO @lv_br.

          lv_result-site = 'Tous les sites'.
          lv_result-kpivalue = lv_br.
          lv_result-period = ''.
          lv_result-year = lv_curry.
          lv_result-fromdate = lv_from_date.
          lv_result-todate = lv_to_date.
          lv_result-kpiname = lv_kpiname.
          APPEND lv_result TO et_entityset.
          lv_curry = lv_curry + 1.
        enddo.



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


      "SUM of what ?? ( I need a field)
    WHEN 'y_ec_moy'.
      lv_tempY = lv_to_date(4) - lv_from_date(4).

      DO lv_tempY + 1 TIMES.
        .
*        SELECT COUNT(*)
*          FROM Ehhss_Sampling_Campaign_Allres
*          WHERE AGENT_KEY_REF = 'ZEHHSS_HZC_AMBL_GIDEN'
*          AND substring( CAST( start_date AS CHAR ), 1 , 4 ) =  @lv_curry
*          INTO @DATA(lv_allres).

*        SELECT COUNT(*)
*          FROM Ehhss_Sampling_Campaign_Allres
*          WHERE substring( CAST( start_date AS CHAR ), 1 , 4 ) =  @lv_curry
*          INTO @DATA(lv_total_allres).

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

      "trimestrielle sans sites
    WHEN 't_ncmaj' OR 't_ncmin' OR 't_tnc_maj' OR 't_tnc_min' OR 't_fds'.


      lv_trim_start = lv_from_date.

      " Calculate number of quarters
      lv_nbr_tri = ( lv_months_diff ) / 3.
      IF ( lv_months_diff ) MOD 3 <> 0.
        lv_nbr_tri = lv_nbr_tri + 1.
      ENDIF.


      DATA(lv_year) =  lv_to_date(4) .
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

        DATA(lv_start_time) = |{ lv_trim_start }071500|.
        DATA(lv_end_time) = |{ lv_trim_end }190000|.

        "Nombre des Non-conformités majeures S&ST (trimestre)
        "Nbre Of entries from PLMM_QUEST_RES when ASSESSMENT_RES = 0
        CASE lv_kpiname.
          WHEN 't_ncmaj'.
            SELECT COUNT(*)
              FROM plmm_quest_res
              WHERE assessment_res = '0'
               AND changeddatetime >= @lv_start_time
              AND changeddatetime <= @lv_end_time
*                  AND substring( CAST( changeddatetime AS CHAR ), 1 , 4 ) =  @lv_curry "Didn't find another date in this table
              INTO @DATA(lv_ncmaj).

            "Nombre des Non-conformités mineures S&ST (trimestre)
            "Nbre Of entries from PLMM_QUEST_RES when ASSESSMENT_RES = 4
          WHEN 't_ncmin'.
            SELECT COUNT(*)
            FROM plmm_quest_res
            WHERE assessment_res = '4'
              AND changeddatetime >= @lv_start_time
                    AND changeddatetime <= @lv_end_time
          INTO @lv_ncmaj.


            ""Taux de traitement des Non Conformités majeures S&ST(trimestre)
            "(Nombre de NC Maj auxquels une mesure est affectée/Nombre total de NC Maj)*100
            "(Nbre Of entries from PLMM_QUEST_RES when ASSESSMENT_RES = 0 and CORR_REQUIRED = X
            " /
            " Nbre Of entries from PLMM_QUEST_RES when ASSESSMENT_RES = 0) *100
          WHEN 't_tnc_maj'.
            SELECT COUNT(*)
              FROM plmm_quest_res
              WHERE assessment_res = '0'
              AND corr_required = 'X'
              AND changeddatetime >= @lv_start_time
               AND changeddatetime <= @lv_end_time
              INTO @DATA(lv_tnc_maj_aff).

            SELECT COUNT(*)
             FROM plmm_quest_res
             WHERE assessment_res = '0'
              AND changeddatetime >= @lv_start_time
              AND changeddatetime <= @lv_end_time
             INTO @DATA(lv_tnc_maj_total).

            IF lv_tnc_maj_total <> 0.
              lv_kpi_f_y = lv_tnc_maj_aff / lv_tnc_maj_total.
              lv_ncmaj =  lv_kpi_f_y * 100.
            ELSE.
              lv_ncmaj = 0.
            ENDIF.

            "Taux de traitement des Non Conformités mineures S&ST (trimestre)
            "(Nombre de NC Min auxquels une mesure est affectée/Nombre total de NC Min)*100
            "(Nbre Of entries from PLMM_QUEST_RES when ASSESSMENT_RES = 4 and CORR_REQUIRED = X
            "/
            " Nbre Of entries from PLMM_QUEST_RES when ASSESSMENT_RES = 4) *100
          WHEN 't_tnc_min'.
            SELECT COUNT(*)
              FROM plmm_quest_res
              WHERE assessment_res = '4'
              AND corr_required = 'X'
              AND changeddatetime >= @lv_start_time
               AND changeddatetime <= @lv_end_time
              INTO @lv_tnc_maj_aff.

            SELECT COUNT(*)
             FROM plmm_quest_res
             WHERE assessment_res = '4'
              AND changeddatetime >= @lv_start_time
              AND changeddatetime <= @lv_end_time
             INTO @lv_tnc_maj_total.

            IF lv_tnc_maj_total <> 0.
              lv_kpi_f_y = lv_tnc_maj_aff / lv_tnc_maj_total.
              lv_ncmaj =  lv_kpi_f_y * 100.
            ELSE.
              lv_ncmaj = 0.
            ENDIF.

            "Nbr of entries in C_MYCHEMICALAPPROVALSLIST with METHODOFUSE=EHHSS_MOU_AvecFDS
            "Taux de disponibilité des FDS dans SAP EHS
          WHEN 't_fds'.
            SELECT COUNT(*)
           FROM c_mychemicalapprovalslist
           WHERE methodofuse = 'EHHSS_MOU_AvecFDS'
            AND requestdate >= @lv_trim_start
            AND requestdate <= @lv_trim_end
           INTO @lv_ncmaj.
              
          

        ENDCASE.



        lv_result-site = 'Tous les sites'.

        lv_result-kpivalue = lv_ncmaj.
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


        lv_trim_start = lv_trim_end.
        "EXIT LOOP IF end date is reached
        IF lv_trim_start > lv_to_date.
          EXIT.
        ENDIF.
*          CLEAR: lt_kpi_temp.
      ENDDO.
      
      "Semestrielle
      "Taux de réalisation du plan d’actions issues des Contrôles Réglementaires
      ""STEP1 : Select from PLMM_AUDIT Audit when AUDIT_TYPE = 18 
      "STEP2: Nbre Of entries in PLMM_QUEST_RES of audit selected in step 1 with 'X' in CORR_REQUIRED"

      when 's_tr_acr'.
      

    WHEN OTHERS.
      "trimestrielle

      lv_trim_start = lv_from_date.

      " Calculate number of quarters
      lv_nbr_tri = ( lv_months_diff ) / 3.
      IF ( lv_months_diff ) MOD 3 <> 0.
        lv_nbr_tri = lv_nbr_tri + 1.
      ENDIF.


      lv_year =  lv_to_date(4) .
      lv_trim_no = 1.

      DO lv_nbr_tri  TIMES.
*        lv_trim_no = sy-index. " Current trimester number
*        lv_trim_end = lv_trim_start + 3 * 30 - 1. " Approximation of trimester end date
*        IF lv_trim_end > lv_to_date.
*          lv_trim_end = lv_to_date.
*        ENDIF.
        lv_trim_end = lv_trim_start.
        CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
          EXPORTING
            date      = lv_trim_start
            days      = 0
            months    = 3
            years     = 0      " Added missing parameter
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
                  WHERE i_incidentbasicinfo~ehslocationuuid IN @<fs_site_temp>-children  "Location <> null
                  AND CaseHasPermanentAbsences = 'X'
*                  AND CASEHASPERMANENTRESTRICTIONS = 'X'
                  AND incidentdate >= @lv_trim_start
                  AND incidentdate <= @lv_trim_end
                  INTO @lv_kpi_single.

                "Nombre de journées perdues par incapacité temporaire pour le personnel.
                "Nombre de journées perdues
              WHEN 't_j_perd'.
                SELECT SUM( nmbrofcalendardaysawayfromwork ) "or nmbrofrstrcdandtransfcaldays
                   FROM i_incidenttimedata AS inc_meta
                    INNER JOIN i_incidentbasicinfo AS incident
                   ON inc_meta~incidentuuid = incident~incidentuuid
                   WHERE ehslocationuuid IN @<fs_site_temp>-children
                   AND  nmbrofcalendardaysawayfromwork >= 0 " or  nmbrofrstrcdandtransfcaldays
                  AND incidentdate >= @lv_trim_start
                    AND incidentdate <= @lv_trim_end
                  INTO @lv_kpi_single.

                "Nombre d' Accident mortel survenu sur le Site (S&ST)
                ""Nombre de décès(Accidents survenus sur les Sites)"

              WHEN 't_dec'.
                SELECT COUNT(*)
                  FROM I_IncidentInjuredPersonInfo
                   INNER JOIN i_incidentbasicinfo
                      ON I_IncidentInjuredPersonInfo~incidentuuid = i_incidentbasicinfo~incidentuuid
                  WHERE i_incidentbasicinfo~ehslocationuuid IN @<fs_site_temp>-children  "Location <> null
                  AND injuryillnessisfatal = 'X'
                  AND incidentdate >= @lv_trim_start
                  AND incidentdate <= @lv_trim_end
                  INTO @lv_kpi_single.

                "Taux de Gravité (TG)
                "(Nombre des journées perdues par incapacité temporaire/heures travaillées) x 1 000
              WHEN 't_grav'.
*
                SELECT SUM( NmbrOfCalendarDaysAwayFromWork )
                FROM i_incidenttimedata AS inc_meta

                 INNER JOIN i_incidentbasicinfo AS incident
                    ON inc_meta~incidentuuid = incident~incidentuuid
                WHERE incident~ehslocationuuid IN @<fs_site_temp>-children
*                  AND casehaspermanentabsences = 'X'
               AND incidentdate >= @lv_trim_start
                 AND incidentdate <= @lv_trim_end
                  INTO @lv_kpi_single
                .

                SELECT SINGLE ehsworkedtimeinhours
                  FROM i_ehshourworked AS ehsworked
                  INNER JOIN i_incidentbasicinfo AS incident
                    ON ehsworked~ehslocationuuid = incident~ehslocationuuid
                  WHERE ehsworked~ehslocationuuid = @<fs_site_temp>-parent
                  AND incidentdate >= @lv_trim_start
                 AND incidentdate <= @lv_trim_end
                  INTO @lv_worked_h.
                IF lv_worked_h <> 0.

                  lv_kpi_f = lv_kpi_single / lv_worked_h.
                  lv_kpi_single = lv_kpi_f  * 1000.
                ELSE.
                  lv_kpi_single = 0. "or lv_kpi_single
                ENDIF.

              WHEN 't_tf'.
*
                SELECT COUNT(*)
                FROM i_incidenttimedata AS inc_meta
                 INNER JOIN i_incidentbasicinfo AS incident
                    ON inc_meta~incidentuuid = incident~incidentuuid
                WHERE incident~ehslocationuuid IN @<fs_site_temp>-children
                AND nmbrofcalendardaysawayfromwork > 0 " ou bien nmbrofrstrcdandtransfcaldays
               AND incidentdate >= @lv_trim_start
                 AND incidentdate <= @lv_trim_end
                  INTO @lv_kpi_single
                .

                SELECT SINGLE ehshourworkedpersoncount
                  FROM i_ehshourworked AS ehsworked
                  INNER JOIN i_incidentbasicinfo AS incident
                    ON ehsworked~ehslocationuuid = incident~ehslocationuuid
                  WHERE ehsworked~ehslocationuuid = @<fs_site_temp>-parent
                  AND incidentdate >= @lv_trim_start
                 AND incidentdate <= @lv_trim_end
                  INTO @lv_worked_h.

                IF lv_worked_h <> 0.
                  lv_kpi_f = lv_kpi_single / lv_worked_h.
                  lv_kpi_single = lv_kpi_f * 1000000.
                ELSE.
                  lv_kpi_single = 0.
                ENDIF.
*                    lv_kpi_single = lv_kpi_single / lv_worked_h.

              WHEN 't_tf_2'.
*
                SELECT COUNT(*)
                FROM i_incidenttimedata AS inc_meta
                 INNER JOIN i_incidentbasicinfo AS incident
                    ON inc_meta~incidentuuid = incident~incidentuuid
                WHERE incident~ehslocationuuid IN @<fs_site_temp>-children
                AND nmbrofcalendardaysawayfromwork = 0 " ou bien nmbrofrstrcdandtransfcaldays
               AND incidentdate >= @lv_trim_start
                 AND incidentdate <= @lv_trim_end
                  INTO @lv_kpi_single
                .

                SELECT SINGLE ehshourworkedpersoncount
                  FROM i_ehshourworked AS ehsworked
                  INNER JOIN i_incidentbasicinfo AS incident
                    ON ehsworked~ehslocationuuid = incident~ehslocationuuid
                  WHERE ehsworked~ehslocationuuid = @<fs_site_temp>-parent
                  AND incidentdate >= @lv_trim_start
                 AND incidentdate <= @lv_trim_end
                  INTO @lv_worked_h.

                IF lv_worked_h <> 0.
                  lv_kpi_f = lv_kpi_single / lv_worked_h.
                  lv_kpi_single = lv_kpi_f * 1000000.
                ELSE.
                  lv_kpi_single = 0.
                ENDIF.

                "Indice de gravité
              WHEN 't_ig'.
                SELECT SUM( NmbrOfCalendarDaysAwayFromWork )
                FROM i_incidenttimedata AS inc_meta
                 INNER JOIN i_incidentbasicinfo AS incident
                    ON inc_meta~incidentuuid = incident~incidentuuid
                WHERE incident~ehslocationuuid IN @<fs_site_temp>-children
*                    AND NMBROFCALENDARDAYSAWAYFROMWORK = 0 " ou bien nmbrofrstrcdandtransfcaldays
               AND incidentdate >= @lv_trim_start
                 AND incidentdate <= @lv_trim_end
                  INTO @lv_kpi_single
                .


                SELECT SINGLE ehshourworkedpersoncount
                  FROM i_ehshourworked AS ehsworked
                  INNER JOIN i_incidentbasicinfo AS incident
                    ON ehsworked~ehslocationuuid = incident~ehslocationuuid
                  WHERE ehsworked~ehslocationuuid = @<fs_site_temp>-parent
                  AND incidentdate >= @lv_trim_start
                 AND incidentdate <= @lv_trim_end
                  INTO @lv_worked_h.

                IF lv_worked_h <> 0.
                  lv_kpi_f = lv_kpi_single / lv_worked_h.
                  lv_kpi_single = lv_kpi_f * 1000.
                ELSE.
                  lv_kpi_single = 0.
                ENDIF.

                "indice de fréquence
              WHEN 't_if'.
*
*                   SELECT COUNT( i_incidentbasicinfo~incidentuuid )
*                     FROM i_incidentbasicinfo
*                      INNER JOIN I_IncidentInjuryIllness
*                     ON i_incidentbasicinfo~incidentuuid = I_IncidentInjuryIllness~incidentuuid
*                  WHERE ehslocationuuid IN @<fs_site_temp>-children
*                 AND incidentdate >= @lv_trim_start
*                   AND incidentdate <= @lv_trim_end
*                    INTO @lv_kpi_single
*                  .

                SELECT COUNT(*)
           FROM i_incidentbasicinfo
           WHERE ehslocationuuid IN @<fs_site_temp>-children "Does not accept IN !
            AND incidentdate >= @lv_trim_start
              AND incidentdate <= @lv_trim_end
           INTO @lv_kpi_single.

                SELECT SINGLE ehshourworkedpersoncount
                 FROM i_ehshourworked AS ehsworked
                 INNER JOIN i_incidentbasicinfo AS incident
                   ON ehsworked~ehslocationuuid = incident~ehslocationuuid
                 WHERE ehsworked~ehslocationuuid = @<fs_site_temp>-parent
                 AND incidentdate >= @lv_trim_start
                AND incidentdate <= @lv_trim_end
                 INTO @lv_worked_h.

                IF lv_worked_h <> 0.
                  lv_kpi_f = lv_kpi_single / lv_worked_h.
                  lv_kpi_single = lv_kpi_f * 1000.
                ELSE.
                  lv_kpi_single = 0.
                ENDIF.


                "Nombre des accidents prestataire
              WHEN 't_pres'.
                SELECT COUNT(*)
                 FROM i_incidentinjuredpersoninfo AS inc_info
                   INNER JOIN i_incidentbasicinfo AS incident
                   ON inc_info~incidentuuid = incident~incidentuuid
                WHERE ehslocationuuid IN @<fs_site_temp>-children
                   AND incidentdate >= @lv_trim_start
                  AND incidentdate <= @lv_trim_end
                  AND incidenttype = 'ZEHHSS_OIT_ACC_PRE'
                INTO @lv_kpi_single.

                "(Step 1  Select INCIDENTUUID from table C_IncidentGroupQuery when INCIDENTCATEGORY =1
                "Step 2: Select Nbre of entries from C_IncidentTaskDefinition For Selected INCIDENTUUID (Of Step 1)/Nbre Of entries C_IncidentGroupQuery when INCIDENTCATEGORY =1)*100
                "Mise en place des plans d'actions à l'issue des incidents
              WHEN 't_mep'.
                SELECT COUNT(*) "Need to be added to the hierar
                   FROM
                    i_incidentgroupcube AS incident
                     INNER JOIN c_incidentrelatedtask AS task_def
                     ON task_def~incidentuuid = incident~incidentuuid
                  WHERE incident~ehslocationuuid IN @<fs_site_temp>-children
                     AND incident~incidentdate >= @lv_trim_start
                    AND incident~incidentdate <= @lv_trim_end
                    AND incidentcategory = '001'
                  INTO @DATA(lv_total_inc).


                SELECT COUNT(*)
                 FROM  i_incidentgroupcube AS incident
                WHERE ehslocationuuid IN @<fs_site_temp>-children
                   AND incidentdate >= @lv_trim_start
                  AND incidentdate <= @lv_trim_end
                  AND incidentcategory = '001'
                INTO @DATA(lv_plan_act).


                IF lv_total_inc <> 0.
                  lv_kpi_f_y =  lv_plan_act / lv_total_inc.
                  lv_kpi_single = lv_kpi_f_y * 100.
                ELSE.
                  lv_kpi_single = 0.
                ENDIF.

                "Mise en place des plans d'actions à l'issue de presque accident
                "(Nombre des plans d'actions mis en place à l'issue de presque accident/Nombre des presques accidents déclaré)*100
                "Step 1  Select INCIDENTUUID from table C_IncidentGroupQuery when INCIDENTCATEGORY =2
                "Step 2: Select Nbre of entries from C_IncidentTaskDefinition For Selected INCIDENTUUID (Of Step 1)/Nbre Of entries C_IncidentGroupQuery when INCIDENTCATEGORY =2)*100
              WHEN 't_meppa'.
                SELECT COUNT(*) "Need to be added to the hierar
                   FROM
                  i_incidentgroupcube AS incident
                     INNER JOIN c_incidentrelatedtask AS task_def
                     ON task_def~incidentuuid = incident~incidentuuid
                  WHERE incident~ehslocationuuid IN @<fs_site_temp>-children
                     AND incident~incidentdate >= @lv_trim_start
                    AND incident~incidentdate <= @lv_trim_end
                    AND incidentcategory = '002'
                  INTO @lv_total_inc.

                SELECT COUNT(*)
                 FROM  i_incidentgroupcube AS incident
                WHERE ehslocationuuid IN @<fs_site_temp>-children
                   AND incidentdate >= @lv_trim_start
                  AND incidentdate <= @lv_trim_end
                  AND incidentcategory = '002'
                INTO @lv_plan_act.


                IF lv_total_inc <> 0.
                  lv_kpi_f_y =  lv_plan_act / lv_total_inc.
                  lv_kpi_single = lv_kpi_f_y * 100.
                ELSE.
                  lv_kpi_single = 0.
                ENDIF.

                "Mise en place d'un plan d'action à l'issue des observations de sécurité
                "(Nombre des plans d'actions mis en place à l'issue des observations de sécurité/Nombre des observations de sécurité déclaré)*100
                "Step 1  Select INCIDENTUUID from table C_IncidentGroupQuery when INCIDENTCATEGORY =3
                "Step 2: Select Nbre of entries from C_IncidentTaskDefinition For Selected INCIDENTUUID (Of Step 1)
                "/Nbre Of entries C_IncidentGroupQuery when INCIDENTCATEGORY =3)*100
              WHEN 't_mepos'.
                SELECT COUNT(*) "Need to be added to the hierar
                   FROM
                  i_incidentgroupcube AS incident
                     INNER JOIN c_incidentrelatedtask AS task_def
                     ON task_def~incidentuuid = incident~incidentuuid
                  WHERE incident~ehslocationuuid IN @<fs_site_temp>-children
                     AND incident~incidentdate >= @lv_trim_start
                    AND incident~incidentdate <= @lv_trim_end
                    AND incidentcategory = '003'
                  INTO @lv_total_inc.



                SELECT COUNT(*)
              FROM  i_incidentgroupcube AS incident
             WHERE ehslocationuuid IN @<fs_site_temp>-children
                AND incidentdate >= @lv_trim_start
               AND incidentdate <= @lv_trim_end
               AND incidentcategory = '003'
             INTO @lv_plan_act.


                IF lv_total_inc <> 0.
                  lv_kpi_f_y =  lv_plan_act / lv_total_inc.
                  lv_kpi_single = lv_kpi_f_y * 100.
                ELSE.
                  lv_kpi_single = 0.
                ENDIF.


                "Taux de traitement des plans d'action issues des incidents
                ""(Step 1  Select INCIDENTUUID from table C_IncidentGroupQuery when INCIDENTCATEGORY =1
                "Step 2: Select Nbre of entries from C_IncidentTaskDefinition For Selected INCIDENTUUID (Of Step 1) when EHSTASKDEFINITIONSTATUS = 3 / Nbr of entries Step 1  Select INCIDENTUUID from table C_IncidentGroupQuery when INCIDENTCATEGORY =1
                "Step 2: Select Nbre of entries from C_IncidentTaskDefinition For Selected INCIDENTUUID (Of Step 1))*100"
              WHEN 't_ttpi'.
                SELECT COUNT(*)
                   FROM  i_incidentgroupcube AS incident
                  WHERE ehslocationuuid IN @<fs_site_temp>-children
                     AND incidentdate >= @lv_trim_start
                    AND incidentdate <= @lv_trim_end
                    AND incidentcategory = '001'
                  INTO @lv_total_inc.

                SELECT COUNT(*)
                  FROM i_incidentbasicinfo AS incident
              INNER JOIN  c_incidenttaskdefandinstance AS task_def
              ON task_def~incidentuuid = incident~incidentuuid
             WHERE ehslocationuuid IN @<fs_site_temp>-children
                AND incidentdate >= @lv_trim_start
               AND incidentdate <= @lv_trim_end
               AND ehstaskdefinitionstatus = '03'
             INTO @lv_plan_act.


                IF lv_total_inc <> 0.
                  lv_kpi_f_y =  lv_plan_act / lv_total_inc.
                  lv_kpi_single = lv_kpi_f_y * 100.
                ELSE.
                  lv_kpi_single = 0.
                ENDIF.



                "Taux de traitement des plans d'action issues des presques accidents
                ""(Step 1  Select INCIDENTUUID from table C_IncidentGroupQuery when INCIDENTCATEGORY =2
                " Step 2: Select Nbre of entries from C_IncidentTaskDefinition For Selected INCIDENTUUID (Of Step 1) when EHSTASKDEFINITIONSTATUS = 3 / Nbr of entries Step 1  Select INCIDENTUUID from table C_IncidentGroupQuery when INCIDENTCATEGORY =2
                "Step 2: Select Nbre of entries from C_IncidentTaskDefinition For Selected INCIDENTUUID (Of Step 1))*100"
              WHEN 't_ttppi'.
                SELECT COUNT(*) "Need to be added to the hierar
                    FROM
                   i_incidentgroupcube AS incident
                      INNER JOIN c_incidentrelatedtask AS task_def
                      ON task_def~incidentuuid = incident~incidentuuid
                   WHERE incident~ehslocationuuid IN @<fs_site_temp>-children
                      AND incident~incidentdate >= @lv_trim_start
                     AND incident~incidentdate <= @lv_trim_end
                     AND incidentcategory = '002'
                   INTO @lv_plan_act.

                SELECT COUNT(*)
                  FROM i_incidentbasicinfo AS incident
              INNER JOIN  c_incidenttaskdefandinstance AS task_def
              ON task_def~incidentuuid = incident~incidentuuid
             WHERE ehslocationuuid IN @<fs_site_temp>-children
                AND incidentdate >= @lv_trim_start
               AND incidentdate <= @lv_trim_end
               AND ehstaskdefinitionstatus = '03'
             INTO @lv_total_inc.


                IF lv_total_inc <> 0.
                  lv_kpi_f_y =  lv_plan_act / lv_total_inc.
                  lv_kpi_single = lv_kpi_f_y * 100.
                ELSE.
                  lv_kpi_single = 0.
                ENDIF.


                "Taux de traitement des plans d'action issus des observations de sécurité
                ""(Step 1  Select INCIDENTUUID from table C_IncidentGroupQuery when INCIDENTCATEGORY =3
                " Step 2: Select Nbre of entries from C_IncidentTaskDefinition For Selected INCIDENTUUID (Of Step 1) when EHSTASKDEFINITIONSTATUS = 3
                "/ Nbr of entries Step 1  Select INCIDENTUUID from table C_IncidentGroupQuery when INCIDENTCATEGORY =3
                "Step 2: Select Nbre of entries from C_IncidentTaskDefinition For Selected INCIDENTUUID (Of Step 1))*100"
              WHEN 't_ttppo'.
                SELECT COUNT(*) "Need to be added to the hierar
                    FROM
                   i_incidentgroupcube AS incident
                      INNER JOIN c_incidentrelatedtask AS task_def
                      ON task_def~incidentuuid = incident~incidentuuid
                   WHERE incident~ehslocationuuid IN @<fs_site_temp>-children
                      AND incident~incidentdate >= @lv_trim_start
                     AND incident~incidentdate <= @lv_trim_end
                     AND incidentcategory = '003'
                   INTO @lv_plan_act.

                SELECT COUNT(*)
                  FROM i_incidentbasicinfo AS incident
              INNER JOIN  c_incidenttaskdefandinstance AS task_def
              ON task_def~incidentuuid = incident~incidentuuid
             WHERE ehslocationuuid IN @<fs_site_temp>-children
                AND incidentdate >= @lv_trim_start
               AND incidentdate <= @lv_trim_end
               AND ehstaskdefinitionstatus = '03'
             INTO @lv_total_inc.


                IF lv_total_inc <> 0.
                  lv_kpi_f_y =  lv_plan_act / lv_total_inc.
                  lv_kpi_single = lv_kpi_f_y * 100.
                ELSE.
                  lv_kpi_single = 0.
                ENDIF.


                "Nombre de visite innopinée par le Responsable sécurité des sites en exploitation
                "AUDIT_TYPENbre Of entries from PLMM_AUDITT when AUDIT_TYPE = 13
              WHEN 't_v_rs'.
                "DATA: lv_start_hhmmss TYPE string VALUE '081500'.
                " DATA: lv_end_hhmmss TYPE string VALUE '081500'.
                lv_start_time = |{ lv_trim_start }071500|.
                lv_end_time = |{ lv_trim_end }190000|.
                SELECT COUNT(*)
                  FROM plmm_audit
                  WHERE audit_type = '13'
                  AND changeddatetime >= @lv_start_time
                  AND changeddatetime <= @lv_end_time
                  INTO @lv_kpi_single.

*            lv_result-site = 'Tous les sites'.

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