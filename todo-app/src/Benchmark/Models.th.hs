-- /Users/aditya.ranjan/todo-app/todo-app/src/Benchmark/Models.hs:62:3-33: Splicing declarations
instance Utils.Jsonifier.Jsonifier (Data.Time.Calendar.Days.Day) where
  toJsonifier
    = \ value_abMZ
        -> case value_abMZ of {
             Data.Time.Calendar.Days.ModifiedJulianDay toModifiedJulianDay_abMY
               -> Jsonifier.object
                    [("toModifiedJulianDay", toJsonifier toModifiedJulianDay_abMY)] }
-- /Users/aditya.ranjan/todo-app/todo-app/src/Benchmark/Models.hs:63:3-39: Splicing declarations
instance Utils.Jsonifier.Jsonifier (Data.Time.LocalTime.Internal.TimeOfDay.TimeOfDay) where
  toJsonifier
    = \ value_abOU
        -> case value_abOU of {
             Data.Time.LocalTime.Internal.TimeOfDay.TimeOfDay todHour_abOT
                                                              todMin_abOS todSec_abOR
               -> Jsonifier.object
                    [("todHour", toJsonifier todHour_abOT),
                     ("todMin", toJsonifier todMin_abOS),
                     ("todSec", toJsonifier todSec_abOR)] }
-- /Users/aditya.ranjan/todo-app/todo-app/src/Benchmark/Models.hs:64:3-39: Splicing declarations
instance Utils.Jsonifier.Jsonifier (Data.Time.LocalTime.Internal.LocalTime.LocalTime) where
  toJsonifier
    = \ value_abPE
        -> case value_abPE of {
             Data.Time.LocalTime.Internal.LocalTime.LocalTime localDay_abPD
                                                              localTimeOfDay_abPC
               -> Jsonifier.object
                    [("localDay", toJsonifier localDay_abPD),
                     ("localTimeOfDay", toJsonifier localTimeOfDay_abPC)] }
-- /Users/aditya.ranjan/todo-app/todo-app/src/Benchmark/Models.hs:65:3-38: Splicing declarations
instance Utils.Jsonifier.Jsonifier (Benchmark.Models.Metadata) where
  toJsonifier
    = \ value_abQg
        -> case value_abQg of {
             Benchmark.Models.Metadata result_type_abQf
               -> Jsonifier.object
                    [("result_type", toJsonifier result_type_abQf)] }
-- /Users/aditya.ranjan/todo-app/todo-app/src/Benchmark/Models.hs:66:3-33: Splicing declarations
instance Utils.Jsonifier.Jsonifier (Benchmark.Models.Geo) where
  toJsonifier
    = \ value_abQN
        -> case value_abQN of {
             Benchmark.Models.Geo type__abQM coordinates_abQL
               -> Jsonifier.object
                    [("type_", toJsonifier type__abQM),
                     ("coordinates", toJsonifier coordinates_abQL)] }
-- /Users/aditya.ranjan/todo-app/todo-app/src/Benchmark/Models.hs:67:3-35: Splicing declarations
instance Utils.Jsonifier.Jsonifier (Benchmark.Models.Story) where
  toJsonifier
    = \ value_abRC
        -> case value_abRC of {
             Benchmark.Models.Story from_user_id_str_abRB profile_image_url_abRA
                                    created_at_abRz from_user_abRy id_str_abRx metadata_abRw
                                    to_user_id_abRv text_abRu id_abRt from_user_id_abRs geo_abRr
                                    iso_language_code_abRq to_user_id_str_abRp source_abRo
               -> Jsonifier.object
                    [("from_user_id_str", toJsonifier from_user_id_str_abRB),
                     ("profile_image_url", toJsonifier profile_image_url_abRA),
                     ("created_at", toJsonifier created_at_abRz),
                     ("from_user", toJsonifier from_user_abRy),
                     ("id_str", toJsonifier id_str_abRx),
                     ("metadata", toJsonifier metadata_abRw),
                     ("to_user_id", 
                      case to_user_id_abRv of
                        GHC.Maybe.Nothing -> Jsonifier.null
                        GHC.Maybe.Just maybe_abRn -> toJsonifier maybe_abRn),
                     ("text", toJsonifier text_abRu), ("id", toJsonifier id_abRt),
                     ("from_user_id", toJsonifier from_user_id_abRs),
                     ("geo", 
                      case geo_abRr of
                        GHC.Maybe.Nothing -> Jsonifier.null
                        GHC.Maybe.Just maybe_abRn -> toJsonifier maybe_abRn),
                     ("iso_language_code", toJsonifier iso_language_code_abRq),
                     ("to_user_id_str", 
                      case to_user_id_str_abRp of
                        GHC.Maybe.Nothing -> Jsonifier.null
                        GHC.Maybe.Just maybe_abRn -> toJsonifier maybe_abRn),
                     ("source", toJsonifier source_abRo)] }
-- /Users/aditya.ranjan/todo-app/todo-app/src/Benchmark/Models.hs:68:3-36: Splicing declarations
instance Utils.Jsonifier.Jsonifier (Benchmark.Models.Result) where
  toJsonifier
    = \ value_abTE
        -> case value_abTE of {
             Benchmark.Models.Result results_abTD max_id_abTC since_id_abTB
                                     refresh_url_abTA next_page_abTz results_per_page_abTy page_abTx
                                     completed_in_abTw since_id_str_abTv max_id_str_abTu query_abTt
               -> Jsonifier.object
                    [("results", 
                      Jsonifier.array (toJsonifier `GHC.Base.fmap` results_abTD)),
                     ("max_id", toJsonifier max_id_abTC),
                     ("since_id", toJsonifier since_id_abTB),
                     ("refresh_url", toJsonifier refresh_url_abTA),
                     ("next_page", toJsonifier next_page_abTz),
                     ("results_per_page", toJsonifier results_per_page_abTy),
                     ("page", toJsonifier page_abTx),
                     ("completed_in", toJsonifier completed_in_abTw),
                     ("since_id_str", toJsonifier since_id_str_abTv),
                     ("max_id_str", toJsonifier max_id_str_abTu),
                     ("query", toJsonifier query_abTt)] }
