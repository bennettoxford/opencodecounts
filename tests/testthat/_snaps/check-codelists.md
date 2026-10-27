# missing_usage_found flags missing codes with usage

    Code
      print(check)
    Message
      
      -- Codelist check --------------------------------------------------------------
      SNOMED CT history version 0.1.0
      OpenCodeCounts SNOMED CT data version 0.2.0 from 01 Aug 2011 to 31 Jul 2013
      ! Skipped 1 codelist not in SNOMED CT.
      v 1 codelist has no missing linked codes with usage.

# check_codelists() skips codelists not found on OpenCodelists

    Code
      print(check)
    Message
      
      -- Codelist check --------------------------------------------------------------
      SNOMED CT history version 0.1.0
      OpenCodeCounts SNOMED CT data version 0.2.0 from 01 Aug 2011 to 31 Jul 2013
      ! Skipped 1 codelist not found on OpenCodelists:
      ! opensafely/test-dmd/def67890
      x 1 codelist misses linked codes with usage:
      
        Codelist              Included codes    Linked codes not included
        user-test-phq-snomed  3 codes, 85 uses  1 code, 150 uses, 63.8%
      
      Missing codes and their share of each old and new code pair:
      
        user-test-phq-snomed
          836541000000100  old code  150 uses  75.0% of pair  PHQ-2 score (old)
      

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
      SNOMED CT history version 0.1.0
      OpenCodeCounts SNOMED CT data version 0.2.0 from 01 Aug 2011 to 31 Jul 2013
      ! Skipped 1 codelist not in SNOMED CT.
      x 1 codelist misses linked codes with usage:
      
        Codelist              Included codes    Linked codes not included
        user-test-phq-snomed  3 codes, 85 uses  1 code, 150 uses, 63.8%
      
      Missing codes and their share of each old and new code pair:
      
        user-test-phq-snomed
          836541000000100  old code  150 uses  75.0% of pair  PHQ-2 score (old)
      

