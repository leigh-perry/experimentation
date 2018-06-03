package exp

object TempTool {
  def main(args: Array[String]): Unit = {

    println()

    // val aliasToTable = Map(joins.map { j => j.toTableAlias -> j.toTable }: _*)

    // DONE
    selects
      .foreach {
        s =>
          val i = s.indexOf(" AS ")
          val (from, to) =
            if (i == -1) {
              val noPrefix = s.substring(s.indexOf(".") + 1)
              (s, noPrefix)
            } else {
              (s.substring(0, i), s.substring((i + 4)))
            }

          //println(from -> to)
          if (from.startsWith("CASE WHEN")) {
            println(s"// $from")
          } else {
            val cs = from.split("\\.")
            if (cs.length == 2) {
              val alias = cs(0)
              val col = cs(1)

              if (alias == "inc") {
                println(s"b.set${snakeToClass(to)}(vIn.get${snakeToClass(col)})")
              } else {
                println(s"b.set${snakeToClass(to)}(j.${alias}.map(_.get${snakeToClass(col)}).orNull)")
              }
            } else {
              println(s"// mystery $from")
            }
          }
      }
    println()

    // DONE
    //    println("private case class Joined(")
    //    println("  streamValue: VInput,")
    //    joins.foreach {
    //      j => println(s"  ${j.toTableAlias}: Option[sor.sor_cc_ci.${j.toTable}.Value] = None,")
    //    }
    //    println(")")
    //    println()
    //
    // DONE
    //    joins.foreach {
    //      j =>
    //        println(s"val gkt_${j.toTable} =")
    //        println(
    //          "   globalKTable[sor.sor_cc_ci." + j.toTable + ".Key, sor.sor_cc_ci." + j
    //            .toTable + ".Value](builder, c, \"sor" +
    //            "." + s"sor_cc_ci.${j.toTable}" + "\")"
    //        )
    //    }
    //    println()
    //

    // DONE
    //    joins.zipWithIndex
    //      .foreach {
    //        case (j, i) =>
    //          println(
    //            s"// LEFT JOIN sor_cc_ci.${j.toTable} ${j.toTableAlias} ON" +
    //              s" ${j.toTableAlias}.${j.toCol} = inc.${j.fromCol}"
    //          )
    //
    //          val getter = "get" + snakeToClass(j.fromCol)
    //          if (i == 0) {
    //
    //            println(s"val j$i: KStream[KInput, Joined] =")
    //            println("  stream")
    //            println(s"    .leftJoin(")
    //            println(s"      gkt_${j.toTable},")
    //            println(s"      (key: KInput, streamValue: VInput) => {")
    //            println(s"        Option(streamValue.$getter).map(new sor.sor_cc_ci.${j.toTable}.Key(_)).orNull")
    //            println(s"      },")
    //            println(s"      (streamValue: VInput, gktValue: sor.sor_cc_ci.${j.toTable}.Value) => {")
    //            println(s"        Joined(streamValue, ${j.toTableAlias} = Option(gktValue))")
    //            println(s"      }")
    //            println(s"    )")
    //            println()
    //          } else {
    //
    //            println(s"val j$i: KStream[KInput, Joined] =")
    //            println(s"  j${(i - 1)}")
    //            println(s"    .leftJoin(")
    //            println(s"      gkt_${j.toTable},")
    //            println(s"      (key: KInput, value: Joined) => {")
    //            println(
    //              s"        Option(value.streamValue.$getter)." +
    //                s"map(new sor.sor_cc_ci.${j.toTable}.Key(_)).orNull"
    //            )
    //            println(s"      },")
    //            println(s"      (j: Joined, gktValue: sor.sor_cc_ci.${j.toTable}.Value) => {")
    //            println(s"        j.copy(${j.toTableAlias} = Option(gktValue))")
    //            println(s"      }")
    //            println(s"    )")
    //            println()
    //          }
    //
    //      }
    //    println()
  }

  def snakeToCamel(s: String): String = {
    var camel = s
    while (camel.contains("_")) {
      camel =
        camel
          .replaceFirst("_[a-z0-9]", String.valueOf(Character.toUpperCase(camel.charAt(camel.indexOf("_") + 1))))
    }
    camel
  }

  def snakeToClass(s: String): String = {
    val camel = snakeToCamel(s)
    s"${camel.substring(0, 1).toUpperCase}${camel.substring(1)}"
  }

  /*
                            toTable        toTableAlias   toTableAlias toCol  fromCol
   LEFT JOIN sor_cc_ci.cctl_losspartytype inclossparty ON inclossparty.id = inc.cgu_lossparty
   LEFT JOIN sor_cc_ci.cctl_cgu_usage usage ON usage.id = inc.cgu_usage
   LEFT JOIN sor_cc_ci.cctl_alcoholreading_ext alcoholreading ON alcoholreading.id = inc.cgu_alcoholreading
   LEFT JOIN sor_cc_ci.cctl_cgu_druguse druguse ON druguse.id = inc.cgu_druguse
   LEFT JOIN sor_cc_ci.cctl_cgu_alcoholdrugsconsumed alcoholdrugsconsumed ON alcoholdrugsconsumed.id = inc.cgu_alcoholdrugsconsumed
   LEFT JOIN sor_cc_ci.cctl_relationtoinsured_ext relationtoinsured ON relationtoinsured.id = inc.driverrelation
   LEFT JOIN sor_cc_ci.cctl_severitytype severitytype ON severitytype.id = inc.severity
   LEFT JOIN sor_cc_ci.cctl_incident incidentsubtype ON incidentsubtype.id = inc.subtype
   LEFT JOIN sor_cc_ci.cctl_losspartytype vehlossparty ON vehlossparty.id = inc.vehiclelossparty
   LEFT JOIN sor_cc_ci.cctl_vehicletype vehicletype ON vehicletype.id = inc.vehicletype
   LEFT JOIN sor_cc_ci.cctl_reasonforuse reasonforuse ON reasonforuse.id = inc.vehicleusereason;
   */
  case class Joinery(toTable: String, toTableAlias: String, toCol: String, fromCol: String)


  val joins =
    Array(
      Joinery("cctl_losspartytype", "inclossparty", "id", "cgu_lossparty"),
      Joinery("cctl_cgu_usage", "usage", "id", "cgu_usage"),
      Joinery("cctl_alcoholreading_ext", "alcoholreading", "id", "cgu_alcoholreading"),
      Joinery("cctl_cgu_druguse", "druguse", "id", "cgu_druguse"),
      Joinery("cctl_cgu_alcoholdrugsconsumed", "alcoholdrugsconsumed", "id", "cgu_alcoholdrugsconsumed"),
      Joinery("cctl_relationtoinsured_ext", "relationtoinsured", "id", "driverrelation"),
      Joinery("cctl_severitytype", "severitytype", "id", "severity"),
      Joinery("cctl_incident", "incidentsubtype", "id", "subtype"),
      Joinery("cctl_losspartytype", "vehlossparty", "id", "vehiclelossparty"),
      Joinery("cctl_vehicletype", "vehicletype", "id", "vehicletype"),
      Joinery("cctl_reasonforuse", "reasonforuse", "id", "vehicleusereason")
    )

  val selects =
    Array(
      "'CC_CI' AS src_sys",
      "inc.id AS incident_id",
      "inc.claimid AS claim_id",
      "inc.cgu_riskunitid AS risk_unit_id",
      "inc.vehicleid AS vehicle_id",
      "inc.cgu_incidentaddressid AS incident_address_id",
      "CASE WHEN inc.cgu_nodamage::text = 1::text THEN 'Yes'::text ELSE 'No'::text END AS vehicle_damage_area_no_damage_flag",
      "CASE WHEN inc.cgu_codeone::text = 1::text THEN 'Yes'::text ELSE 'No'::text END AS vehicle_damage_area_1_front_left_flag",
      "CASE WHEN inc.cgu_codetwo::text = 1::text THEN 'Yes'::text ELSE 'No'::text END AS vehicle_damage_area_2_front_right_flag",
      "CASE WHEN inc.cgu_codethree::text = 1::text THEN 'Yes'::text ELSE 'No'::text END AS vehicle_damage_area_3_side_right_flag",
      "CASE WHEN inc.cgu_codefour::text = 1::text THEN 'Yes'::text ELSE 'No'::text END AS vehicle_damage_area_4_rear_right_flag",
      "CASE WHEN inc.cgu_codefive::text = 1::text THEN 'Yes'::text ELSE 'No'::text END AS vehicle_damage_area_5_rear_left_flag",
      "CASE WHEN inc.cgu_codesix::text = 1::text THEN 'Yes'::text ELSE 'No'::text END AS vehicle_damage_area_6_side_left_flag",
      "CASE WHEN inc.cgu_codeseven::text = 1::text THEN 'Yes'::text ELSE 'No'::text END AS vehicle_damage_area_7_engine_flag",
      "CASE WHEN inc.cgu_codeeight::text = 1::text THEN 'Yes'::text ELSE 'No'::text END AS vehicle_damage_area_8_roof_flag",
      "CASE WHEN inc.cgu_codenine::text = 1::text THEN 'Yes'::text ELSE 'No'::text END AS vehicle_damage_area_9_interior_flag",
      "CASE WHEN inc.cgu_windscreenonly::text = 1::text THEN 'Yes'::text ELSE 'No'::text END AS vehicle_damage_area_windscreen_only",
      "inc.cgu_lossparty AS incident_loss_party_id",
      "inclossparty.typecode AS incident_loss_party_code",
      "inclossparty.name AS incident_loss_party_name",
      "inc.cgu_usage AS property_usage_type_id",
      "usage.typecode AS property_usage_code",
      "usage.name AS property_usage_name",
      "inc.cgu_alcoholreading AS alcohol_reading_id",
      "alcoholreading.typecode AS alcohol_reading_code",
      "alcoholreading.name AS alcohol_reading_name",
      "inc.cgu_druguse AS drug_use_id",
      "druguse.typecode AS drug_use_code",
      "druguse.name AS drug_use_name",
      "inc.cgu_alcoholdrugsconsumed AS alcohol_drugs_consumed_id",
      "alcoholdrugsconsumed.typecode AS alcohol_drugs_consumed_code",
      "alcoholdrugsconsumed.name AS alcohol_drugs_consumed_name",
      "inc.driverrelation AS driver_relation_to_insured_id",
      "relationtoinsured.typecode AS driver_relation_to_insured_code",
      "relationtoinsured.name AS driver_relation_to_insured_name",
      "inc.severity AS incident_severity_id",
      "severitytype.typecode AS incident_severity_code",
      "severitytype.name AS incident_severity_name",
      "inc.subtype AS incident_subtype_id",
      "incidentsubtype.typecode AS incident_subtype_code",
      "incidentsubtype.name AS incident_subtype_name",
      "incidentsubtype.description AS incident_subtype_description",
      "inc.vehiclelossparty AS incident_vehicle_loss_party_id",
      "vehlossparty.typecode AS incident_vehicle_loss_party_code",
      "vehlossparty.name AS incident_vehicle_loss_party_name",
      "inc.vehicletype AS incident_vehicle_type_id",
      "vehicletype.typecode AS incident_vehicle_type_code",
      "vehicletype.name AS incident_vehicle_type_name",
      "inc.vehicleusereason AS incident_vehicle_use_id",
      "reasonforuse.typecode AS incident_vehicle_use_code",
      "reasonforuse.name AS incident_vehicle_use_name",
      "inc.retired",
      "inc.createtime AS create_time",
      "inc.createuserid AS create_userid",
      "inc.updatetime AS update_time",
      "inc.updateuserid AS update_userid",
      "inc.ctl_ins_ts",
      "inc.ctl_upd_ts",
      "inc.archivepartition AS archive_partition",
      "inc.beanversion AS incident_bean_version",
      "inc.loadcommandid AS load_command_id",
      "inc.affdvcmplind AS affdv_cmpl_ind",
      "inc.airbagsdeployed AS incident_air_bags_deployed_flag_name",
      "inc.airbagsmissing AS airbags_missing",
      "inc.alarmtype AS alarm_type",
      "inc.alreadyrepaired AS already_repaired",
      "inc.ambulanceused AS ambulance_used",
      "inc.antithftind AS anti_theft_flag_id",
      "inc.appraisal",
      "inc.appraisalfirstappointment AS appraisal_first_appointment",
      "inc.assessmentclosedate AS assessment_close_date",
      "inc.assessmentcomment AS assessment_comment",
      "inc.assessmentdate_cgu AS assessment_create_date",
      "inc.assessmentname AS assessment_name",
      "inc.assessmentstatus AS assessment_status_id",
      "inc.assessmenttargetclosedate AS assessment_target_close_date",
      "inc.assessmenttype AS assessment_type",
      "inc.baggagemissingfrom AS baggage_missing_from",
      "inc.baggagerecoveredon AS baggage_recovered_on",
      "inc.baggagetype AS baggage_type",
      "inc.bodyshopselected AS body_shop_selected",
      "inc.carriercompensated AS carrier_compensated",
      "inc.carriercompensatedamount AS carrier_compensated_amount",
      "inc.cgu_accommrequired AS accommodation_required_desc",
      "inc.cgu_activitiespriorincident AS activities_prior_incident",
      "inc.cgu_anyadditnalassmtreqd AS additional_assesment_required",
      "inc.cgu_anyurgactionreqd AS incident_any_urgent_action_req_flag_id",
      "inc.cgu_assmtasc AS assessment_scope_of_work_asc",
      "inc.cgu_assmtassessor AS assessment_scope_of_work_assessor",
      "inc.cgu_assmtcmc AS assessment_scope_of_work_cmc",
      "inc.cgu_assmtcustomer AS assessment_scope_of_work_customer",
      "inc.cgu_assmtdiscussions AS assessment_scope_of_work_discussions",
      "inc.cgu_assmtexcess AS assessment_excess_id",
      "inc.cgu_assmtobservations AS assessment_scope_of_work_observations",
      "inc.cgu_assmtphotostaken AS photos_taken",
      "inc.cgu_assmtrecovery AS assessment_recovery_id",
      "inc.cgu_assmtsettlement AS assessment_settlement_type_id",
      "inc.cgu_authorisation AS authorisation",
      "inc.cgu_bodyfunction AS body_function",
      "inc.cgu_buildingestimate AS building_estimate",
      "inc.cgu_buildinglossparty AS building_loss_party",
      "inc.cgu_canbusinesstrade AS can_your_business_trade_if_applicable",
      "inc.cgu_carriername AS carrier_name",
      "inc.cgu_claimassmtacceptance AS claim_assessment_acceptance_id",
      "inc.cgu_clientcostcentre AS client_cost_centre",
      "inc.cgu_comment AS incident_comment",
      "inc.cgu_commentstorepairer AS commentsto_repairer",
      "inc.cgu_contactid AS contact_associated_with_the_official",
      "inc.cgu_costofrepairs AS cost_of_repairs",
      "inc.cgu_daystolossdate AS unoccupied_days_to_loss_date",
      "inc.cgu_doelectricity AS do_you_have_electricity",
      "inc.cgu_doesrequireegrepairs AS does_the_property_require_emergency_repairs",
      "inc.cgu_dorunningwater AS do_you_have_running_water",
      "inc.cgu_employmentcapacity AS employment_capacity",
      "inc.cgu_encumbranceamount AS amount_owing_to_lender_marine",
      "inc.cgu_enddate AS enddate",
      "inc.cgu_expertreportreqd AS expert_report_required",
      "inc.cgu_exposedfrom AS exposed_from",
      "inc.cgu_exposedto AS exposed_to",
      "inc.cgu_financiallosstype AS financial_loss_type",
      "inc.cgu_firedamage AS incident_fire_damage_flag_id",
      "inc.cgu_firstdrinktime AS time_of_first_drink",
      "inc.cgu_floodfreshwater AS flood_occured_in_fresh_water",
      "inc.cgu_hazardouswaste AS hazardous_waste_id",
      "inc.cgu_hirecardailyrate AS hire_car_daily_rate",
      "inc.cgu_hirecareentitlement AS hire_care_entitlement",
      "inc.cgu_hirecarmaxdays AS hire_car_maximum_days",
      "inc.cgu_hirecarmaxlimit AS hire_car_maximum_limit",
      "inc.cgu_insuredrelation AS incident_insured_relation_id",
      "inc.cgu_insuredrelation_other AS incident_insured_relation_other",
      "inc.cgu_internaluserid AS incident_internal_userid",
      "inc.cgu_isclaimantminor AS is_the_claimant_a_minor",
      "inc.cgu_ishabitable AS is_the_house_habitable",
      "inc.cgu_isinsureddriveratlosssence AS is_insured_driverat_loss_sence",
      "inc.cgu_ispropertyaccessible AS is_this_property_accessible",
      "inc.cgu_isunderothercover AS covered_under_any_other_policy_or_scheme",
      "inc.cgu_kilometers200k AS kilometres_200k_flag_id",
      "inc.cgu_lastdrinktime AS last_drink_time",
      "inc.cgu_locationalcoholconsumed AS location_alcohol_consumed",
      "inc.cgu_makesaferequired AS make_safe_required_id",
      "inc.cgu_nonliability AS non_liability_reason_id",
      "inc.cgu_nonliabsavings AS non_liability_savings",
      "inc.cgu_numberofoccupants AS number_occupants",
      "inc.cgu_numpassengers AS number_of_passengers_marine",
      "inc.cgu_obtaindoccertificate AS obtain_doctors_certificate_for_lost_wages",
      "inc.cgu_otheramountcompensated AS amount_compensated",
      "inc.cgu_othercoverdetails AS other_cover_details",
      "inc.cgu_otherhazwastedetail AS other_hazard_waste_detail_desc",
      "inc.cgu_ownerspermission AS owners_permission_flag_id",
      "inc.cgu_propertyincustody AS incident_property_in_custody_id",
      "inc.cgu_purpose AS incident_purpose_text",
      "inc.cgu_refertoinvestigations AS refer_to_investigation_desc",
      "inc.cgu_refertorecoveries AS refer_to_recoveries",
      "inc.cgu_refertounderwriting AS refer_to_underwriting",
      "inc.cgu_salvageid AS salvage_id",
      "inc.cgu_startdate AS startdate",
      "inc.cgu_statereason AS state_reason",
      "inc.cgu_taxibenefit AS taxi_benefit",
      "inc.cgu_taxifarelimit AS taxi_fare_limit",
      "inc.cgu_taxijourneylimit AS taxi_journey_limit",
      "inc.cgu_thirdpartyriskunitvehicle AS third_party_risk_unit_vehicle",
      "inc.cgu_totaldaysunoccpd AS total_days_unoccupied",
      "inc.cgu_totaldrinks AS total_drinks",
      "inc.cgu_towingdatetime AS incident_towing_date",
      "inc.cgu_treatment_paid AS treatment_paid",
      "inc.cgu_typealcoholconsumed AS type_alcohol_consumed",
      "inc.cgu_unoccupiedfrom AS unoccupied_from_date",
      "inc.cgu_unoccupiedto AS unoccupied_to_date",
      "inc.cgu_valuepreaccident AS value_pre_accident",
      "inc.cgu_vehiclerequirestowing AS vehicle_requires_towing",
      "inc.cgu_vehiclerequiretow AS does_the_vehicle_require_a_tow",
      "inc.citationissued AS citation_issued",
      "inc.claimincident AS claim_incident",
      "inc.classtype AS class_type_id",
      "inc.collision",
      "inc.collisionpoint AS collision_point",
      "inc.commencing_date",
      "inc.componentsmissing AS components_missing",
      "inc.damagedareasize AS damaged_area_size",
      "inc.datesalvageassigned AS date_salvage_assigned",
      "inc.datevehiclerecovered AS unrequired",
      "inc.datevehiclesold AS date_vehicle_sold",
      "inc.debrisremovalind AS debris_removal_ind",
      "inc.delayonly AS delay_only",
      "inc.delivery_date",
      "inc.descother AS desc_other",
      "inc.description AS incident_description",
      "inc.detailedinjurytype AS detailed_injury_type",
      "inc.disabledduetoaccident AS disabled_due_to_accident",
      "inc.driverreltoowner AS driver_rel_to_owner",
      "inc.emsind AS ems_ind",
      "inc.equipmentfailure AS incident_equipment_failure_flag",
      "inc.estdamagetype AS est_damage_type",
      "inc.estimatesreceived AS estimates_received",
      "inc.estrepaircost AS est_repair_cost",
      "inc.estrepairtime AS est_repair_time",
      "inc.extdamagetxt AS ext_damagetxt",
      "inc.extrication AS incident_extrication_flag_id",
      "inc.extwallmat AS exterior_wall_material_id",
      "inc.fencesdamaged AS fences_damaged",
      "inc.fireburndash AS fire_burn_dash",
      "inc.fireburnengine AS fire_burn_engine",
      "inc.fireburnwindshield AS fire_burn_windshield",
      "inc.fireprotdetails AS fire_prot_details",
      "inc.fireprotectionavailable AS fire_protection_available",
      "inc.floodsaltwater AS flood_salt_water",
      "inc.generalinjurytype AS general_injury_type",
      "inc.goods_description",
      "inc.goods_from",
      "inc.goods_to",
      "inc.hazardinvolved AS hazard_involved_flag_id",
      "inc.hitandrun AS hit_and_run",
      "inc.impairment",
      "inc.includecontentlineitems AS include_content_line_items",
      "inc.includelineitems AS include_line_items",
      "inc.inspectionrequired AS inspection_required",
      "inc.interiormissing AS interior_missing",
      "inc.internaluserid AS internal_user_id",
      "inc.locationaddress AS location_address",
      "inc.locationind AS location_ind",
      "inc.lossarea AS loss_area",
      "inc.lossdesc AS loss_desc",
      "inc.lossestimate AS loss_estimate",
      "inc.lossoccured AS loss_occured",
      "inc.lossofuse AS lossof_use",
      "inc.lostwages AS lost_wages",
      "inc.lotnumber AS lot_number",
      "inc.materialsdamaged AS materials_damaged",
      "inc.mealsdays AS meals_days",
      "inc.mealspeople AS meals_people",
      "inc.mealsrate AS meals_rate",
      "inc.medicaltreatmenttype AS medical_treatment_type",
      "inc.mileage100k AS mileage100_k",
      "inc.minoronpolicy AS minor_on_policy",
      "inc.moldinvolved AS mold_involved",
      "inc.movepermission AS move_permission",
      "inc.numberofpeopleonpolicy AS number_of_people_on_policy",
      "inc.numsprinkler AS num_sprinkler",
      "inc.numsprinkoper AS num_sprink_oper",
      "inc.numstories AS num_stories",
      "inc.occupancytype AS occupancy_type",
      "inc.odomread AS odometer_read",
      "inc.otherinsurer_ext AS incident_other_insurer_id",
      "inc.ownerretainingsalvage AS owner_retaining_salvage",
      "inc.ownerspermission AS incident_owners_permission_flag_id",
      "inc.percentagedrivenbyminor AS percentage_driven_by_minor",
      "inc.phantomvehicle AS phantom_vehicle",
      "inc.propertydesc AS property_desc",
      "inc.propertyid AS property_id",
      "inc.propertysize AS property_size_metre_sqm",
      "inc.publicid AS public_id",
      "inc.recovclasstype AS recov_class_type",
      "inc.recovcondtype AS recov_cond_type",
      "inc.recovdate AS recov_date",
      "inc.recoverylocationid AS recovery_location_id",
      "inc.recovind AS incident_recovered_flag_id",
      "inc.recovstate AS recov_state",
      "inc.relatedtripruid AS related_trip_r_u_id",
      "inc.rentalagency AS rental_agency",
      "inc.rentalbegindate AS rental_begin_date",
      "inc.rentaldailyrate AS rental_daily_rate",
      "inc.rentalenddate AS rental_end_date",
      "inc.rentalrequired AS rental_required",
      "inc.rentalreserveno AS rental_reserve_no",
      "inc.repwheredisind AS rep_where_dis_ind",
      "inc.returntomodworkactual AS return_to_mod_work_actual",
      "inc.returntomodworkdate AS return_to_mod_work_date",
      "inc.returntomodworkvalid AS return_to_mod_work_valid",
      "inc.returntoworkactual AS return_to_work_actual",
      "inc.returntoworkdate AS return_to_work_date",
      "inc.returntoworkvalid AS return_to_work_valid",
      "inc.roofmaterial AS roof_material_type_id",
      "inc.salvagecompany AS salvage_company",
      "inc.salvagenet AS salvage_net",
      "inc.salvageprep AS salvage_prep",
      "inc.salvageproceeds AS salvage_proceeds",
      "inc.salvagestorage AS salvage_storage",
      "inc.salvagetitle AS salvage_title",
      "inc.salvagetow AS salvage_tow_fees",
      "inc.shipping_method",
      "inc.speed",
      "inc.sprinklertype AS sprinkler_type",
      "inc.sprinkretserv AS sprink_ret_serv",
      "inc.startdate AS start_date",
      "inc.storageaccrind AS storage_accr_ind",
      "inc.storagefclty AS storage_fclty",
      "inc.storagefeeamt AS storage_fee_amt",
      "inc.totalloss AS incident_total_loss_flag_id",
      "inc.totallosspoints AS incident_total_loss_points",
      "inc.trafficviolation AS traffic_violation",
      "inc.tripruid AS trip_ru_id",
      "inc.vehcondtype AS veh_cond_type",
      "inc.vehicleacv AS vehicle_acv",
      "inc.vehicleage10years AS incident_vehicle_age_10_years_flag_id",
      "inc.vehicleage5years AS vehicle_age5_years",
      "inc.vehicledirection AS vehicle_direction",
      "inc.vehicledriveable AS vehicle_driveable",
      "inc.vehiclelocation AS vehicle_location",
      "inc.vehicleoperable AS incident_vehicle_operable_flag_name",
      "inc.vehicleparked AS incident_vehicle_parked_flag_id",
      "inc.vehiclepolstatus AS vehicle_pol_status",
      "inc.vehiclerollover AS incident_vehicle_rollover_flag_id",
      "inc.vehiclesubmerged AS vehicle_submerged_flag_id",
      "inc.vehicletitlerecvd AS vehicle_title_recvd",
      "inc.vehicletitlereqd AS vehicle_title_reqd",
      "inc.vehlockind AS vehicle_lock_flag_id",
      "inc.vehstolenind AS vehicle_stolen_flag_id",
      "inc.vehtowedind AS incident_vehicle_towed_flag_id",
      "inc.vesseltype AS vessel_type",
      "inc.waterleveldash AS water_level_dash",
      "inc.waterlevelseats AS water_level_seats",
      "inc.whentoview AS when_to_view",
      "inc.yearbuilt AS year_built",
      "inc.yearsinhome AS years_in_home",
    )


  //  def stuff0() = {
  //    println()
  //    println()
  //
  //    println("case class CcIncident(")
  //    colsSorToCtx.foreach {
  //      case (colSor, colCtx) =>
  //        if (!colSor.contains(".")) {
  //          val className = classNameSorOf(colSor)
  //          println(s"  $colSor: Option[$className],")
  //        }
  //    }
  //    println(")")
  //    println("// TODO LP REMOVE OPTION for ID")
  //    println()
  //
  //    println("case class CcIncidentView(")
  //    colsSorToCtx.foreach {
  //      case (colSor, colCtx) =>
  //        val className = classNameCtxOf(colCtx)
  //        println(s"  $colCtx: Option[$className],")
  //    }
  //    println(")")
  //    println("// TODO LP REMOVE OPTION for ID")
  //    println()
  //
  //    println("val genCcIncident: Gen[CcIncident] =")
  //    println("  for {")
  //    colsSorToCtx.foreach {
  //      case (colSor, colCtx) =>
  //        if (!colSor.contains(".")) {
  //          val gen = genOf(colSor)
  //          println(s"    $colSor <- $gen")
  //        }
  //    }
  //    println("  } yield CcIncident(")
  //    colsSorToCtx.foreach {
  //      case (colSor, colCtx) =>
  //        if (!colSor.contains(".")) {
  //          println(s"    $colSor,")
  //        }
  //    }
  //    println("  )")
  //    println()
  //
  //    println("INSERT INTO sor_cc_ci.cc_incident (")
  //    colsSorToCtx.foreach {
  //      case (colSor, colCtx) =>
  //        if (!colSor.contains(".")) {
  //          println(s"  $colSor,")
  //        }
  //    }
  //    println(") VALUES (")
  //    colsSorToCtx.foreach {
  //      case (colSor, colCtx) =>
  //        if (!colSor.contains(".")) {
  //          println(s"  $${e.$colSor},")
  //        }
  //    }
  //    println(")")
  //    println("// TODO LP REMOVE COMMAS")
  //    println()
  //
  //    println("SELECT")
  //    colsSorToCtx.foreach {
  //      case (colSor, colCtx) =>
  //        println(s"  $colCtx,")
  //    }
  //    println(" FROM")
  //    println("   sor_view.cc_ci_incident")
  //    println("// TODO LP REMOVE COMMA")
  //    println()
  //  }
  //
  //  private def classNameSorOf(col: String): String = {
  //    classNameOf(colTypesSor(col))
  //  }
  //
  //  private def classNameCtxOf(col: String): String = {
  //    classNameOf(colTypesCtx(col))
  //  }
  //
  //  private def classNameOf(s: String) = {
  //    if (s.contains("character") || s.contains("text")) {
  //      "String"
  //    } else if (s.contains("bigint")) {
  //      "Long"
  //    } else {
  //      s"//// Nothing: $s ////"
  //    }
  //  }
  //
  //  private def genOf(col: String): String = {
  //    val s = colTypesSor(col)
  //    if (s.contains("character")) {
  //      val i = s.indexOf('(')
  //      if (i == -1) {
  //        "Gen.option(stringGen)"
  //      } else {
  //        val s2 = s.substring(i + 1)
  //        val s3 = s2.substring(0, s2.length)
  //        s"Gen.option(stringGen(maxSize = $s3)"
  //      }
  //    } else if (s.contains("bigint")) {
  //      "genOptLong"
  //    } else {
  //      s"//// Nothing: $s ////"
  //    }
  //  }
  //
  //  val colsSorToCtx =
  //    Map(
  //      "id" -> "incident_id",
  //      "alcoholdrugsconsumed.name" -> "alcohol_drugs_consumed_name",
  //      "alcoholdrugsconsumed.typecode" -> "alcohol_drugs_consumed_code",
  //      "alcoholreading.name" -> "alcohol_reading_name",
  //      "alcoholreading.typecode" -> "alcohol_reading_code",
  //      "cgu_codeeight" -> "vehicle_damage_area_8_roof_flag",
  //      "cgu_codefive" -> "vehicle_damage_area_5_rear_left_flag",
  //      "cgu_codefour" -> "vehicle_damage_area_4_rear_right_flag",
  //      "cgu_codenine" -> "vehicle_damage_area_9_interior_flag",
  //      "cgu_codeone" -> "vehicle_damage_area_1_front_left_flag",
  //      "cgu_codeseven" -> "vehicle_damage_area_7_engine_flag",
  //      "cgu_codesix" -> "vehicle_damage_area_6_side_left_flag",
  //      "cgu_codethree" -> "vehicle_damage_area_3_side_right_flag",
  //      "cgu_codetwo" -> "vehicle_damage_area_2_front_right_flag",
  //      "cgu_nodamage" -> "vehicle_damage_area_no_damage_flag",
  //      "cgu_windscreenonly" -> "vehicle_damage_area_windscreen_only",
  //      "cgu_activitiespriorincident" -> "activities_prior_incident",
  //      "cgu_alcoholdrugsconsumed" -> "alcohol_drugs_consumed_id",
  //      "cgu_alcoholreading" -> "alcohol_reading_id",
  //      "cgu_druguse" -> "drug_use_id",
  //      "cgu_incidentaddressid" -> "incident_address_id",
  //      "cgu_usage" -> "property_usage_type_id",
  //      "driverrelation" -> "driver_relation_to_insured_id",
  //      "severity" -> "incident_severity_id",
  //      "sprinklertype" -> "sprinkler_type",
  //      "subtype" -> "incident_subtype_id",
  //      "vehiclelocation" -> "vehicle_location",
  //      "vehiclelossparty" -> "incident_vehicle_loss_party_id",
  //      "vehicletitlereqd" -> "vehicle_title_reqd",
  //      "vehicletype" -> "incident_vehicle_type_id",
  //      "vehicleusereason" -> "incident_vehicle_use_id",
  //      "yearsinhome" -> "years_in_home",
  //      "incidentsubtype.description" -> "incident_subtype_description",
  //      "incidentsubtype.name" -> "incident_subtype_name",
  //      "incidentsubtype.typecode" -> "incident_subtype_code",
  //      "inclossparty.name" -> "incident_loss_party_name",
  //      "inclossparty.typecode" -> "incident_loss_party_code",
  //      "reasonforuse.name" -> "incident_vehicle_use_name",
  //      "reasonforuse.typecode" -> "incident_vehicle_use_code",
  //      "relationtoinsured.name" -> "driver_relation_to_insured_name",
  //      "relationtoinsured.typecode" -> "driver_relation_to_insured_code",
  //      "severitytype.name" -> "incident_severity_name",
  //      "severitytype.typecode" -> "incident_severity_code",
  //      "usage.name" -> "property_usage_name",
  //      "usage.typecode" -> "property_usage_code",
  //      "vehicletype.name" -> "incident_vehicle_type_name",
  //      "vehicletype.typecode" -> "incident_vehicle_type_code",
  //      "vehlossparty.name" -> "incident_vehicle_loss_party_name",
  //      "vehlossparty.typecode" -> "incident_vehicle_loss_party_code",
  //    )
  //  val colTypesSor =
  //    Map(
  //      "estrepairtime" -> "character varying(60)",
  //      "damagedareasize" -> "bigint",
  //      "numsprinkoper" -> "bigint",
  //      "recovind" -> "bigint",
  //      "createtime" -> "timestamp(6) without time zone",
  //      "numstories" -> "bigint",
  //      "airbagsmissing" -> "character(1)",
  //      "returntomodworkvalid" -> "character(1)",
  //      "carriercompensated" -> "character(1)",
  //      "vehcondtype" -> "bigint",
  //      "claimid" -> "bigint",
  //      "vehiclerollover" -> "character(1)",
  //      "cgu_refertoinvestigations" -> "character varying(250)",
  //      "yearbuilt" -> "timestamp(6) without time zone",
  //      "cgu_lastdrinktime" -> "timestamp(6) without time zone",
  //      "moldinvolved" -> "bigint",
  //      "floodsaltwater" -> "character(1)",
  //      "mileage100k" -> "character(1)",
  //      "propertydesc" -> "character varying(255)",
  //      "speed" -> "bigint",
  //      "cgu_buildingestimate" -> "numeric(18,2)",
  //      "totallosspoints" -> "bigint",
  //      "salvageproceeds" -> "numeric(18,2)",
  //      "cgu_unoccupiedfrom" -> "timestamp(6) without time zone",
  //      "cgu_assmtrecovery" -> "bigint",
  //      "ownerspermission" -> "character(1)",
  //      "cgu_usage" -> "bigint",
  //      "cgu_codeeight" -> "character(1)",
  //      "vehicleparked" -> "character(1)",
  //      "cgu_commentstorepairer" -> "character varying(250)",
  //      "cgu_financiallosstype" -> "bigint",
  //      "vehicledriveable" -> "character(1)",
  //      "generalinjurytype" -> "bigint",
  //      "roofmaterial" -> "bigint",
  //      "recovclasstype" -> "bigint",
  //      "equipmentfailure" -> "character(1)",
  //      "cgu_totaldaysunoccpd" -> "bigint",
  //      "cgu_ownerspermission" -> "character(1)",
  //      "cgu_unoccupiedto" -> "timestamp(6) without time zone",
  //      "percentagedrivenbyminor" -> "bigint",
  //      "detailedinjurytype" -> "bigint",
  //      "waterlevelseats" -> "character(1)",
  //      "internaluserid" -> "bigint",
  //      "returntoworkactual" -> "character(1)",
  //      "debrisremovalind" -> "character(1)",
  //      "cgu_totaldrinks" -> "bigint",
  //      "vehicletitlereqd" -> "bigint",
  //      "updateuserid" -> "bigint",
  //      "salvageprep" -> "numeric(18,2)",
  //      "salvagetow" -> "numeric(18,2)",
  //      "includecontentlineitems" -> "character(1)",
  //      "antithftind" -> "character(1)",
  //      "cgu_assmtcmc" -> "character varying(250)",
  //      "locationaddress" -> "bigint",
  //      "cgu_clientcostcentre" -> "character varying(256)",
  //      "driverrelation" -> "bigint",
  //      "vehicletype" -> "bigint",
  //      "totalloss" -> "character(1)",
  //      "archivepartition" -> "bigint",
  //      "cgu_propertyincustody" -> "bigint",
  //      "rentalagency" -> "character varying(25)",
  //      "extrication" -> "character(1)",
  //      "cgu_codenine" -> "character(1)",
  //      "cgu_codefive" -> "character(1)",
  //      "cgu_towingdatetime" -> "timestamp(6) without time zone",
  //      "lossarea" -> "bigint",
  //      "cgu_dorunningwater" -> "bigint",
  //      "cgu_daystolossdate" -> "bigint",
  //      "cgu_comment" -> "character varying(255)",
  //      "cgu_assmtdiscussions" -> "character varying(250)",
  //      "lossdesc" -> "character varying(30)",
  //      "returntomodworkactual" -> "character(1)",
  //      "fireburndash" -> "character(1)",
  //      "cgu_assmtcustomer" -> "character varying(250)",
  //      "storagefclty" -> "character varying(30)",
  //      "baggagetype" -> "bigint",
  //      "recovstate" -> "bigint",
  //      "airbagsdeployed" -> "character(1)",
  //      "impairment" -> "numeric(4,1)",
  //      "includelineitems" -> "character(1)",
  //      "hitandrun" -> "character(1)",
  //      "driverreltoowner" -> "bigint",
  //      "recoverylocationid" -> "bigint",
  //      "cgu_thirdpartyriskunitvehicle" -> "bigint",
  //      "assessmentname" -> "character varying(128)",
  //      "cgu_assmtexcess" -> "bigint",
  //      "cgu_refertounderwriting" -> "character varying(250)",
  //      "citationissued" -> "bigint",
  //      "bodyshopselected" -> "character(1)",
  //      "returntomodworkdate" -> "timestamp(6) without time zone",
  //      "fireburnwindshield" -> "character(1)",
  //      "salvagenet" -> "numeric(18,2)",
  //      "cgu_alcoholdrugsconsumed" -> "bigint",
  //      "storagefeeamt" -> "numeric(18,2)",
  //      "movepermission" -> "character(1)",
  //      "cgu_anyadditnalassmtreqd" -> "character varying(250)",
  //      "fireprotdetails" -> "character varying(1)",
  //      "recovdate" -> "timestamp(6) without time zone",
  //      "minoronpolicy" -> "bigint",
  //      "cgu_authorisation" -> "character varying(3)",
  //      "classtype" -> "bigint",
  //      "vehicleage5years" -> "character(1)",
  //      "rentalreserveno" -> "character varying(15)",
  //      "cgu_nodamage" -> "character(1)",
  //      "cgu_buildinglossparty" -> "bigint",
  //      "appraisal" -> "character(1)",
  //      "cgu_numberofoccupants" -> "bigint",
  //      "cgu_bodyfunction" -> "bigint",
  //      "lossoccured" -> "bigint",
  //      "odomread" -> "bigint",
  //      "carriercompensatedamount" -> "numeric(18,2)",
  //      "lossofuse" -> "character(1)",
  //      "cgu_expertreportreqd" -> "character varying(250)",
  //      "cgu_incidentaddressid" -> "bigint",
  //      "ownerretainingsalvage" -> "character(1)",
  //      "descother" -> "character varying(255)",
  //      "numberofpeopleonpolicy" -> "bigint",
  //      "cgu_codefour" -> "character(1)",
  //      "medicaltreatmenttype" -> "bigint",
  //      "cgu_assmtassessor" -> "character varying(250)",
  //      "vehiclelocation" -> "character varying(255)",
  //      "sprinkretserv" -> "bigint",
  //      "datesalvageassigned" -> "timestamp(6) without time zone",
  //      "beanversion" -> "bigint",
  //      "cgu_otherhazwastedetail" -> "character varying(50)",
  //      "baggagemissingfrom" -> "timestamp(6) without time zone",
  //      "vehiclepolstatus" -> "bigint",
  //      "recovcondtype" -> "bigint",
  //      "salvagestorage" -> "numeric(18,2)",
  //      "salvagecompany" -> "character varying(30)",
  //      "yearsinhome" -> "bigint",
  //      "vehlockind" -> "character(1)",
  //      "vehicleusereason" -> "bigint",
  //      "estrepaircost" -> "numeric(18,2)",
  //      "baggagerecoveredon" -> "timestamp(6) without time zone",
  //      "rentalrequired" -> "character(1)",
  //      "assessmentclosedate" -> "timestamp(6) without time zone",
  //      "occupancytype" -> "bigint",
  //      "createuserid" -> "bigint",
  //      "alarmtype" -> "bigint",
  //      "vehicleid" -> "bigint",
  //      "claimincident" -> "character(1)",
  //      "startdate" -> "timestamp(6) without time zone",
  //      "salvagetitle" -> "numeric(18,2)",
  //      "collision" -> "character(1)",
  //      "relatedtripruid" -> "bigint",
  //      "rentalbegindate" -> "timestamp(6) without time zone",
  //      "cgu_insuredrelation" -> "bigint",
  //      "id" -> "bigint NOT NULL",
  //      "assessmentcomment" -> "character varying(255)",
  //      "severity" -> "bigint",
  //      "cgu_riskunitid" -> "bigint",
  //      "datevehiclerecovered" -> "timestamp(6) without time zone",
  //      "materialsdamaged" -> "character varying(250)",
  //      "cgu_insuredrelation_other" -> "character varying(1333)",
  //      "otherinsurer_ext" -> "bigint",
  //      "estimatesreceived" -> "bigint",
  //      "cgu_purpose" -> "character varying(255)",
  //      "cgu_codetwo" -> "character(1)",
  //      "cgu_vehiclerequirestowing" -> "character(1)",
  //      "storageaccrind" -> "bigint",
  //      "emsind" -> "character(1)",
  //      "cgu_floodfreshwater" -> "character(1)",
  //      "assessmenttype" -> "bigint",
  //      "loadcommandid" -> "bigint",
  //      "cgu_codethree" -> "character(1)",
  //      "cgu_assmtsettlement" -> "bigint",
  //      "alreadyrepaired" -> "character(1)",
  //      "vehicleage10years" -> "character(1)",
  //      "rentalenddate" -> "timestamp(6) without time zone",
  //      "cgu_statereason" -> "character varying(30)",
  //      "tripruid" -> "bigint",
  //      "cgu_codeone" -> "character(1)",
  //      "cgu_alcoholreading" -> "bigint",
  //      "cgu_exposedto" -> "timestamp(6) without time zone",
  //      "estdamagetype" -> "bigint",
  //      "componentsmissing" -> "character(1)",
  //      "inspectionrequired" -> "character(1)",
  //      "cgu_internaluserid" -> "bigint",
  //      "updatetime" -> "timestamp(6) without time zone",
  //      "ambulanceused" -> "character(1)",
  //      "subtype" -> "bigint",
  //      "cgu_assmtobservations" -> "character varying(250)",
  //      "assessmentdate_cgu" -> "timestamp(6) without time zone",
  //      "cgu_ispropertyaccessible" -> "bigint",
  //      "publicid" -> "character varying(20)",
  //      "cgu_doesrequireegrepairs" -> "bigint",
  //      "lostwages" -> "character(1)",
  //      "mealsdays" -> "bigint",
  //      "retired" -> "bigint",
  //      "returntoworkdate" -> "timestamp(6) without time zone",
  //      "cgu_isclaimantminor" -> "character(1)",
  //      "datevehiclesold" -> "timestamp(6) without time zone",
  //      "assessmentstatus" -> "bigint",
  //      "interiormissing" -> "character(1)",
  //      "cgu_doelectricity" -> "bigint",
  //      "repwheredisind" -> "character(1)",
  //      "vehtowedind" -> "character(1)",
  //      "phantomvehicle" -> "character(1)",
  //      "disabledduetoaccident" -> "bigint",
  //      "affdvcmplind" -> "bigint",
  //      "cgu_makesaferequired" -> "bigint",
  //      "cgu_anyurgactionreqd" -> "character(1)",
  //      "lossestimate" -> "numeric(18,2)",
  //      "cgu_hazardouswaste" -> "bigint",
  //      "propertysize" -> "bigint",
  //      "cgu_assmtasc" -> "bigint",
  //      "hazardinvolved" -> "bigint",
  //      "cgu_accommrequired" -> "character varying(250)",
  //      "collisionpoint" -> "bigint",
  //      "mealspeople" -> "bigint",
  //      "extdamagetxt" -> "character varying(289)",
  //      "sprinklertype" -> "bigint",
  //      "numsprinkler" -> "bigint",
  //      "delayonly" -> "character(1)",
  //      "cgu_claimassmtacceptance" -> "bigint",
  //      "cgu_codeseven" -> "character(1)",
  //      "cgu_kilometers200k" -> "character(1)",
  //      "appraisalfirstappointment" -> "timestamp(6) without time zone",
  //      "rentaldailyrate" -> "numeric(18,2)",
  //      "cgu_salvageid" -> "bigint",
  //      "fireprotectionavailable" -> "character(1)",
  //      "whentoview" -> "character varying(255)",
  //      "vehstolenind" -> "character(1)",
  //      "returntoworkvalid" -> "character(1)",
  //      "vehiclesubmerged" -> "character(1)",
  //      "cgu_canbusinesstrade" -> "bigint",
  //      "cgu_contactid" -> "bigint",
  //      "cgu_codesix" -> "character(1)",
  //      "trafficviolation" -> "bigint",
  //      "locationind" -> "character(1)",
  //      "cgu_valuepreaccident" -> "numeric(18,2)",
  //      "vehicledirection" -> "bigint",
  //      "vehicleoperable" -> "character(1)",
  //      "cgu_ishabitable" -> "character(1)",
  //      "vehiclelossparty" -> "bigint",
  //      "cgu_druguse" -> "bigint",
  //      "lotnumber" -> "character varying(20)",
  //      "description" -> "character varying(1333)",
  //      "cgu_refertorecoveries" -> "character varying(250)",
  //      "fireburnengine" -> "character(1)",
  //      "vehicleacv" -> "numeric(18,2)",
  //      "cgu_assmtphotostaken" -> "character(1)",
  //      "cgu_windscreenonly" -> "character(1)",
  //      "cgu_nonliability" -> "bigint",
  //      "mealsrate" -> "numeric(18,2)",
  //      "vehicletitlerecvd" -> "bigint",
  //      "fencesdamaged" -> "character(1)",
  //      "assessmenttargetclosedate" -> "timestamp(6) without time zone",
  //      "cgu_firedamage" -> "character(1)",
  //      "cgu_costofrepairs" -> "numeric(18,2)",
  //      "extwallmat" -> "bigint",
  //      "propertyid" -> "bigint",
  //      "cgu_exposedfrom" -> "timestamp(6) without time zone",
  //      "waterleveldash" -> "character(1)",
  //      "cgu_nonliabsavings" -> "numeric(18,2)",
  //      "cgu_employmentcapacity" -> "bigint",
  //      "cgu_firstdrinktime" -> "timestamp(6) without time zone",
  //      "cgu_locationalcoholconsumed" -> "character varying(255)",
  //      "cgu_activitiespriorincident" -> "character varying(255)",
  //      "cgu_typealcoholconsumed" -> "character varying(255)",
  //      "cgu_isinsureddriveratlosssence" -> "bigint",
  //      "cgu_obtaindoccertificate" -> "bigint",
  //      "cgu_isunderothercover" -> "bigint",
  //      "cgu_carriername" -> "character varying(255)",
  //      "cgu_startdate" -> "timestamp(6) without time zone",
  //      "cgu_otheramountcompensated" -> "numeric(18,2)",
  //      "cgu_enddate" -> "timestamp(6) without time zone",
  //      "cgu_othercoverdetails" -> "character varying(255)",
  //      "cgu_treatment_paid" -> "character(1)",
  //      "goods_to" -> "bigint",
  //      "commencing_date" -> "timestamp(6) without time zone",
  //      "vesseltype" -> "bigint",
  //      "cgu_encumbranceamount" -> "numeric(18,2)",
  //      "shipping_method" -> "bigint",
  //      "goods_description" -> "character varying(1333)",
  //      "goods_from" -> "bigint",
  //      "delivery_date" -> "timestamp(6) without time zone",
  //      "cgu_numpassengers" -> "bigint",
  //      "cgu_hirecardailyrate" -> "numeric(18,2)",
  //      "cgu_taxibenefit" -> "bigint",
  //      "cgu_taxifarelimit" -> "character varying(255)",
  //      "cgu_hirecarmaxdays" -> "bigint",
  //      "cgu_taxijourneylimit" -> "character varying(255)",
  //      "cgu_hirecareentitlement" -> "bigint",
  //      "cgu_hirecarmaxlimit" -> "numeric(18,2)",
  //      "cgu_vehiclerequiretow" -> "character(1)",
  //      "ctl_ins_ts" -> "timestamp without time zone",
  //      "ctl_upd_ts" -> "timestamp without time zone",
  //      "cgu_lossparty" -> "bigint",
  //    )
  //
  //  val colTypesCtx =
  //    Map(
  //      "src_sys" -> "character varying(10)",
  //      "incident_id" -> "bigint",
  //      "claim_id" -> "bigint",
  //      "risk_unit_id" -> "bigint",
  //      "vehicle_id" -> "bigint",
  //      "incident_address_id" -> "bigint",
  //      "vehicle_damage_area_no_damage_flag" -> "text",
  //      "vehicle_damage_area_1_front_left_flag" -> "text",
  //      "vehicle_damage_area_2_front_right_flag" -> "text",
  //      "vehicle_damage_area_3_side_right_flag" -> "text",
  //      "vehicle_damage_area_4_rear_right_flag" -> "text",
  //      "vehicle_damage_area_5_rear_left_flag" -> "text",
  //      "vehicle_damage_area_6_side_left_flag" -> "text",
  //      "vehicle_damage_area_7_engine_flag" -> "text",
  //      "vehicle_damage_area_8_roof_flag" -> "text",
  //      "vehicle_damage_area_9_interior_flag" -> "text",
  //      "vehicle_damage_area_windscreen_only" -> "text",
  //      "incident_loss_party_id" -> "bigint",
  //      "incident_loss_party_code" -> "character varying(50)",
  //      "incident_loss_party_name" -> "character varying(256)",
  //      "property_usage_type_id" -> "bigint",
  //      "property_usage_code" -> "character varying(50)",
  //      "property_usage_name" -> "character varying(256)",
  //      "alcohol_reading_id" -> "bigint",
  //      "alcohol_reading_code" -> "character varying(50)",
  //      "alcohol_reading_name" -> "character varying(256)",
  //      "drug_use_id" -> "bigint",
  //      "drug_use_code" -> "character varying(50)",
  //      "drug_use_name" -> "character varying(256)",
  //      "alcohol_drugs_consumed_id" -> "bigint",
  //      "alcohol_drugs_consumed_code" -> "character varying(50)",
  //      "alcohol_drugs_consumed_name" -> "character varying(256)",
  //      "driver_relation_to_insured_id" -> "bigint",
  //      "driver_relation_to_insured_code" -> "character varying(50)",
  //      "driver_relation_to_insured_name" -> "character varying(256)",
  //      "incident_severity_id" -> "bigint",
  //      "incident_severity_code" -> "character varying(50)",
  //      "incident_severity_name" -> "character varying(256)",
  //      "incident_subtype_id" -> "bigint",
  //      "incident_subtype_code" -> "character varying(50)",
  //      "incident_subtype_name" -> "character varying(256)",
  //      "incident_subtype_description" -> "character varying(512)",
  //      "incident_vehicle_loss_party_id" -> "bigint",
  //      "incident_vehicle_loss_party_code" -> "character varying(50)",
  //      "incident_vehicle_loss_party_name" -> "character varying(256)",
  //      "incident_vehicle_type_id" -> "bigint",
  //      "incident_vehicle_type_code" -> "character varying(50)",
  //      "incident_vehicle_type_name" -> "character varying(256)",
  //      "incident_vehicle_use_id" -> "bigint",
  //      "incident_vehicle_use_code" -> "character varying(50)",
  //      "incident_vehicle_use_name" -> "character varying(256)",
  //      "retired" -> "bigint",
  //      "create_time" -> "timestamp(6) without time zone",
  //      "create_userid" -> "bigint",
  //      "update_time" -> "timestamp(6) without time zone",
  //      "update_userid" -> "bigint",
  //      "ctl_ins_ts" -> "timestamp without time zone",
  //      "ctl_upd_ts" -> "timestamp without time zone",
  //      "archive_partition" -> "bigint",
  //      "incident_bean_version" -> "bigint",
  //      "load_command_id" -> "bigint",
  //      "affdv_cmpl_ind" -> "bigint",
  //      "incident_air_bags_deployed_flag_name" -> "character(1)",
  //      "airbags_missing" -> "character(1)",
  //      "alarm_type" -> "bigint",
  //      "already_repaired" -> "character(1)",
  //      "ambulance_used" -> "character(1)",
  //      "anti_theft_flag_id" -> "character(1)",
  //      "appraisal" -> "character(1)",
  //      "appraisal_first_appointment" -> "timestamp(6) without time zone",
  //      "assessment_close_date" -> "timestamp(6) without time zone",
  //      "assessment_comment" -> "character varying(255)",
  //      "assessment_create_date" -> "timestamp(6) without time zone",
  //      "assessment_name" -> "character varying(128)",
  //      "assessment_status_id" -> "bigint",
  //      "assessment_target_close_date" -> "timestamp(6) without time zone",
  //      "assessment_type" -> "bigint",
  //      "baggage_missing_from" -> "timestamp(6) without time zone",
  //      "baggage_recovered_on" -> "timestamp(6) without time zone",
  //      "baggage_type" -> "bigint",
  //      "body_shop_selected" -> "character(1)",
  //      "carrier_compensated" -> "character(1)",
  //      "carrier_compensated_amount" -> "numeric(18,2)",
  //      "accommodation_required_desc" -> "character varying(250)",
  //      "activities_prior_incident" -> "character varying(255)",
  //      "additional_assesment_required" -> "character varying(250)",
  //      "incident_any_urgent_action_req_flag_id" -> "character(1)",
  //      "assessment_scope_of_work_asc" -> "bigint",
  //      "assessment_scope_of_work_assessor" -> "character varying(250)",
  //      "assessment_scope_of_work_cmc" -> "character varying(250)",
  //      "assessment_scope_of_work_customer" -> "character varying(250)",
  //      "assessment_scope_of_work_discussions" -> "character varying(250)",
  //      "assessment_excess_id" -> "bigint",
  //      "assessment_scope_of_work_observations" -> "character varying(250)",
  //      "photos_taken" -> "character(1)",
  //      "assessment_recovery_id" -> "bigint",
  //      "assessment_settlement_type_id" -> "bigint",
  //      "authorisation" -> "character varying(3)",
  //      "body_function" -> "bigint",
  //      "building_estimate" -> "numeric(18,2)",
  //      "building_loss_party" -> "bigint",
  //      "can_your_business_trade_if_applicable" -> "bigint",
  //      "carrier_name" -> "character varying(255)",
  //      "claim_assessment_acceptance_id" -> "bigint",
  //      "client_cost_centre" -> "character varying(256)",
  //      "incident_comment" -> "character varying(255)",
  //      "commentsto_repairer" -> "character varying(250)",
  //      "contact_associated_with_the_official" -> "bigint",
  //      "cost_of_repairs" -> "numeric(18,2)",
  //      "unoccupied_days_to_loss_date" -> "bigint",
  //      "do_you_have_electricity" -> "bigint",
  //      "does_the_property_require_emergency_repairs" -> "bigint",
  //      "do_you_have_running_water" -> "bigint",
  //      "employment_capacity" -> "bigint",
  //      "amount_owing_to_lender_marine" -> "numeric(18,2)",
  //      "enddate" -> "timestamp(6) without time zone",
  //      "expert_report_required" -> "character varying(250)",
  //      "exposed_from" -> "timestamp(6) without time zone",
  //      "exposed_to" -> "timestamp(6) without time zone",
  //      "financial_loss_type" -> "bigint",
  //      "incident_fire_damage_flag_id" -> "character(1)",
  //      "time_of_first_drink" -> "timestamp(6) without time zone",
  //      "flood_occured_in_fresh_water" -> "character(1)",
  //      "hazardous_waste_id" -> "bigint",
  //      "hire_car_daily_rate" -> "numeric(18,2)",
  //      "hire_care_entitlement" -> "bigint",
  //      "hire_car_maximum_days" -> "bigint",
  //      "hire_car_maximum_limit" -> "numeric(18,2)",
  //      "incident_insured_relation_id" -> "bigint",
  //      "incident_insured_relation_other" -> "character varying(1333)",
  //      "incident_internal_userid" -> "bigint",
  //      "is_the_claimant_a_minor" -> "character(1)",
  //      "is_the_house_habitable" -> "character(1)",
  //      "is_insured_driverat_loss_sence" -> "bigint",
  //      "is_this_property_accessible" -> "bigint",
  //      "covered_under_any_other_policy_or_scheme" -> "bigint",
  //      "kilometres_200k_flag_id" -> "character(1)",
  //      "last_drink_time" -> "timestamp(6) without time zone",
  //      "location_alcohol_consumed" -> "character varying(255)",
  //      "make_safe_required_id" -> "bigint",
  //      "non_liability_reason_id" -> "bigint",
  //      "non_liability_savings" -> "numeric(18,2)",
  //      "number_occupants" -> "bigint",
  //      "number_of_passengers_marine" -> "bigint",
  //      "obtain_doctors_certificate_for_lost_wages" -> "bigint",
  //      "amount_compensated" -> "numeric(18,2)",
  //      "other_cover_details" -> "character varying(255)",
  //      "other_hazard_waste_detail_desc" -> "character varying(50)",
  //      "owners_permission_flag_id" -> "character(1)",
  //      "incident_property_in_custody_id" -> "bigint",
  //      "incident_purpose_text" -> "character varying(255)",
  //      "refer_to_investigation_desc" -> "character varying(250)",
  //      "refer_to_recoveries" -> "character varying(250)",
  //      "refer_to_underwriting" -> "character varying(250)",
  //      "salvage_id" -> "bigint",
  //      "startdate" -> "timestamp(6) without time zone",
  //      "state_reason" -> "character varying(30)",
  //      "taxi_benefit" -> "bigint",
  //      "taxi_fare_limit" -> "character varying(255)",
  //      "taxi_journey_limit" -> "character varying(255)",
  //      "third_party_risk_unit_vehicle" -> "bigint",
  //      "total_days_unoccupied" -> "bigint",
  //      "total_drinks" -> "bigint",
  //      "incident_towing_date" -> "timestamp(6) without time zone",
  //      "treatment_paid" -> "character(1)",
  //      "type_alcohol_consumed" -> "character varying(255)",
  //      "unoccupied_from_date" -> "timestamp(6) without time zone",
  //      "unoccupied_to_date" -> "timestamp(6) without time zone",
  //      "value_pre_accident" -> "numeric(18,2)",
  //      "vehicle_requires_towing" -> "character(1)",
  //      "does_the_vehicle_require_a_tow" -> "character(1)",
  //      "citation_issued" -> "bigint",
  //      "claim_incident" -> "character(1)",
  //      "class_type_id" -> "bigint",
  //      "collision" -> "character(1)",
  //      "collision_point" -> "bigint",
  //      "commencing_date" -> "timestamp(6) without time zone",
  //      "components_missing" -> "character(1)",
  //      "damaged_area_size" -> "bigint",
  //      "date_salvage_assigned" -> "timestamp(6) without time zone",
  //      "unrequired" -> "timestamp(6) without time zone",
  //      "date_vehicle_sold" -> "timestamp(6) without time zone",
  //      "debris_removal_ind" -> "character(1)",
  //      "delay_only" -> "character(1)",
  //      "delivery_date" -> "timestamp(6) without time zone",
  //      "desc_other" -> "character varying(255)",
  //      "incident_description" -> "character varying(1333)",
  //      "detailed_injury_type" -> "bigint",
  //      "disabled_due_to_accident" -> "bigint",
  //      "driver_rel_to_owner" -> "bigint",
  //      "ems_ind" -> "character(1)",
  //      "incident_equipment_failure_flag" -> "character(1)",
  //      "est_damage_type" -> "bigint",
  //      "estimates_received" -> "bigint",
  //      "est_repair_cost" -> "numeric(18,2)",
  //      "est_repair_time" -> "character varying(60)",
  //      "ext_damagetxt" -> "character varying(289)",
  //      "incident_extrication_flag_id" -> "character(1)",
  //      "exterior_wall_material_id" -> "bigint",
  //      "fences_damaged" -> "character(1)",
  //      "fire_burn_dash" -> "character(1)",
  //      "fire_burn_engine" -> "character(1)",
  //      "fire_burn_windshield" -> "character(1)",
  //      "fire_prot_details" -> "character varying(1)",
  //      "fire_protection_available" -> "character(1)",
  //      "flood_salt_water" -> "character(1)",
  //      "general_injury_type" -> "bigint",
  //      "goods_description" -> "character varying(1333)",
  //      "goods_from" -> "bigint",
  //      "goods_to" -> "bigint",
  //      "hazard_involved_flag_id" -> "bigint",
  //      "hit_and_run" -> "character(1)",
  //      "impairment" -> "numeric(4,1)",
  //      "include_content_line_items" -> "character(1)",
  //      "include_line_items" -> "character(1)",
  //      "inspection_required" -> "character(1)",
  //      "interior_missing" -> "character(1)",
  //      "internal_user_id" -> "bigint",
  //      "location_address" -> "bigint",
  //      "location_ind" -> "character(1)",
  //      "loss_area" -> "bigint",
  //      "loss_desc" -> "character varying(30)",
  //      "loss_estimate" -> "numeric(18,2)",
  //      "loss_occured" -> "bigint",
  //      "lossof_use" -> "character(1)",
  //      "lost_wages" -> "character(1)",
  //      "lot_number" -> "character varying(20)",
  //      "materials_damaged" -> "character varying(250)",
  //      "meals_days" -> "bigint",
  //      "meals_people" -> "bigint",
  //      "meals_rate" -> "numeric(18,2)",
  //      "medical_treatment_type" -> "bigint",
  //      "mileage100_k" -> "character(1)",
  //      "minor_on_policy" -> "bigint",
  //      "mold_involved" -> "bigint",
  //      "move_permission" -> "character(1)",
  //      "number_of_people_on_policy" -> "bigint",
  //      "num_sprinkler" -> "bigint",
  //      "num_sprink_oper" -> "bigint",
  //      "num_stories" -> "bigint",
  //      "occupancy_type" -> "bigint",
  //      "odometer_read" -> "bigint",
  //      "incident_other_insurer_id" -> "bigint",
  //      "owner_retaining_salvage" -> "character(1)",
  //      "incident_owners_permission_flag_id" -> "character(1)",
  //      "percentage_driven_by_minor" -> "bigint",
  //      "phantom_vehicle" -> "character(1)",
  //      "property_desc" -> "character varying(255)",
  //      "property_id" -> "bigint",
  //      "property_size_metre_sqm" -> "bigint",
  //      "public_id" -> "character varying(20)",
  //      "recov_class_type" -> "bigint",
  //      "recov_cond_type" -> "bigint",
  //      "recov_date" -> "timestamp(6) without time zone",
  //      "recovery_location_id" -> "bigint",
  //      "incident_recovered_flag_id" -> "bigint",
  //      "recov_state" -> "bigint",
  //      "related_trip_r_u_id" -> "bigint",
  //      "rental_agency" -> "character varying(25)",
  //      "rental_begin_date" -> "timestamp(6) without time zone",
  //      "rental_daily_rate" -> "numeric(18,2)",
  //      "rental_end_date" -> "timestamp(6) without time zone",
  //      "rental_required" -> "character(1)",
  //      "rental_reserve_no" -> "character varying(15)",
  //      "rep_where_dis_ind" -> "character(1)",
  //      "return_to_mod_work_actual" -> "character(1)",
  //      "return_to_mod_work_date" -> "timestamp(6) without time zone",
  //      "return_to_mod_work_valid" -> "character(1)",
  //      "return_to_work_actual" -> "character(1)",
  //      "return_to_work_date" -> "timestamp(6) without time zone",
  //      "return_to_work_valid" -> "character(1)",
  //      "roof_material_type_id" -> "bigint",
  //      "salvage_company" -> "character varying(30)",
  //      "salvage_net" -> "numeric(18,2)",
  //      "salvage_prep" -> "numeric(18,2)",
  //      "salvage_proceeds" -> "numeric(18,2)",
  //      "salvage_storage" -> "numeric(18,2)",
  //      "salvage_title" -> "numeric(18,2)",
  //      "salvage_tow_fees" -> "numeric(18,2)",
  //      "shipping_method" -> "bigint",
  //      "speed" -> "bigint",
  //      "sprinkler_type" -> "bigint",
  //      "sprink_ret_serv" -> "bigint",
  //      "start_date" -> "timestamp(6) without time zone",
  //      "storage_accr_ind" -> "bigint",
  //      "storage_fclty" -> "character varying(30)",
  //      "storage_fee_amt" -> "numeric(18,2)",
  //      "incident_total_loss_flag_id" -> "character(1)",
  //      "incident_total_loss_points" -> "bigint",
  //      "traffic_violation" -> "bigint",
  //      "trip_ru_id" -> "bigint",
  //      "veh_cond_type" -> "bigint",
  //      "vehicle_acv" -> "numeric(18,2)",
  //      "incident_vehicle_age_10_years_flag_id" -> "character(1)",
  //      "vehicle_age5_years" -> "character(1)",
  //      "vehicle_direction" -> "bigint",
  //      "vehicle_driveable" -> "character(1)",
  //      "vehicle_location" -> "character varying(255)",
  //      "incident_vehicle_operable_flag_name" -> "character(1)",
  //      "incident_vehicle_parked_flag_id" -> "character(1)",
  //      "vehicle_pol_status" -> "bigint",
  //      "incident_vehicle_rollover_flag_id" -> "character(1)",
  //      "vehicle_submerged_flag_id" -> "character(1)",
  //      "vehicle_title_recvd" -> "bigint",
  //      "vehicle_title_reqd" -> "bigint",
  //      "vehicle_lock_flag_id" -> "character(1)",
  //      "vehicle_stolen_flag_id" -> "character(1)",
  //      "incident_vehicle_towed_flag_id" -> "character(1)",
  //      "vessel_type" -> "bigint",
  //      "water_level_dash" -> "character(1)",
  //      "water_level_seats" -> "character(1)",
  //      "when_to_view" -> "character varying(255)",
  //      "year_built" -> "timestamp(6) without time zone",
  //      "years_in_home" -> "bigint",
  //    )

}
