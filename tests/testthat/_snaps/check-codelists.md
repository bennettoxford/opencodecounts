# not_selected_usage_found flags linked codes not selected with usage

    Code
      print(check)
    Message
      
      -- Codelist check --------------------------------------------------------------
      SNOMED CT history version 0.1.0 (TRUD release uk_sctqths_test_release)
      OpenCodeCounts SNOMED CT data version 0.2.0 from 01 Aug 2011 to 31 Jul 2013
      ! Skipped 1 codelist not in SNOMED CT.
      v 1 codelist selects all linked codes with usage.

# check_codelists() skips codelists not found on OpenCodelists

    Code
      print(check)
    Message
      
      -- Codelist check --------------------------------------------------------------
      SNOMED CT history version 0.1.0 (TRUD release uk_sctqths_test_release)
      OpenCodeCounts SNOMED CT data version 0.2.0 from 01 Aug 2011 to 31 Jul 2013
      ! Skipped 1 codelist not found on OpenCodelists:
      ! opensafely/test-dmd/def67890
      i 1 codelist does not select some linked codes with usage:
      
        Codelist              Selected codes    Linked codes not selected
        user-test-phq-snomed  3 codes, 85 uses  1 code, 150 uses, 63.8%
      
      Linked codes not selected and their share of each old and new pair:
      
        user-test-phq-snomed
          836541000000100  old  150 uses  75.0% of pair  PHQ-2 score (old)
      

# filter_usage_periods() only accepts period boundaries

    Code
      filter_usage_periods(usage, "2012-01-01", NULL)
    Condition
      Error in `filter_usage_periods()`:
      ! `start_date` "2012-01-01" is not the start of a reporting period.
      i Available start dates: "2011-08-01" and "2012-08-01"

# print() shows the report

    Code
      print(local_test_check())
    Message
      
      -- Codelist check --------------------------------------------------------------
      SNOMED CT history version 0.1.0 (TRUD release uk_sctqths_test_release)
      OpenCodeCounts SNOMED CT data version 0.2.0 from 01 Aug 2011 to 31 Jul 2013
      ! Skipped 1 codelist not in SNOMED CT.
      i 1 codelist does not select some linked codes with usage:
      
        Codelist              Selected codes    Linked codes not selected
        user-test-phq-snomed  3 codes, 85 uses  1 code, 150 uses, 63.8%
      
      Linked codes not selected and their share of each old and new pair:
      
        user-test-phq-snomed
          836541000000100  old  150 uses  75.0% of pair  PHQ-2 score (old)
      

