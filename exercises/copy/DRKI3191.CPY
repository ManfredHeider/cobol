      *---------------------------------------------------------------*
      *
      *    ÄNDERUNGSDATEN AUS ERMITTLUNG VERTRAGSTEIL
      *    GEDLEBEN - G20 (PSLIFE)
      *
      *---------------------------------------------------------------*
      *    LOGBUCH
      *****************************************************************
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AKI01534!14.11.17!2.1 ! Neuerstellung             ! Walbert
      ***************************************************************** 
      * AENDERUNGEN:                                                    
      * AUFTRAG ! DATUM  ! AENDERUNG                       ! NAME       
      * --------------------------------------------------------------- 
      * AKI01819!19.01.21! MIA: NEUE FELDER HINZU:         ! T13AC20
      *         !        !      VZUST-D                    !
      *         !        !      VZUST-D-KZN                !
      *         !        ! [PVCS 2.2]                      !
      * DBO8172 !16.03.21! MIA: FELDER ENTFALLEN:          ! T13AC20    
      *         !        !      RZINS                      !
      * --------!--------!---------------------------------!----------- 
      *         !        !                                 !            
      *         !        !                                 !            
      * ENDE-LOGBUCH
      *****************************************************************
      *
           05 VERARB-KZN                         PIC  X(001).
           05 GES                                PIC  X(002).
           05 VNR                                PIC  X(015).
           05 LFDNR                              PIC  9(004).
           05 DATVERTRAEND                       PIC  X(010).
           05 BEZUGLFDNR                         PIC  9(004).
           05 BEZUGLFDNR-KZN                     PIC  X(001).
           05 HVT-KZN                            PIC  X(001).
           05 HVT-KZN-KZN                        PIC  X(001).
           05 TARIFKLASSE                        PIC S9(005).
           05 TARIFKLASSE-KZN                    PIC  X(001).
           05 BEGINN                             PIC  X(010).
           05 BEGINN-KZN                         PIC  X(001).
           05 ABLAUF                             PIC  X(010).
           05 ABLAUF-KZN                         PIC  X(001).
           05 BEITRZUSTID                        PIC S9(002).
           05 BEITRZUSTID-KZN                    PIC  X(001).
           05 VERSSUMME                          PIC S9(011)V99
                                                      PACKED-DECIMAL.
           05 VERSSUMME-KZN                      PIC  X(001).
           05 ABLBZ                              PIC  X(010).
           05 ABLBZ-KZN                          PIC  X(001).
           05 TARIFBEITRAG                       PIC S9(011)V99
                                                      PACKED-DECIMAL.
           05 TARIFBEITRAG-KZN                   PIC  X(001).
           05 VZUST-D                            PIC S9(003).
           05 VZUST-D-KZN                        PIC  X(001).
