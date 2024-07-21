      *---------------------------------------------------------------*
      *
      *    AKTUALISIERUNGSDATEN AUS ERMITTLUNG RECHENWERTE
      *    GEDLEBEN - G20 (PSLIFE)
      *
      *---------------------------------------------------------------*
      *    LOGBUCH
      *****************************************************************
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AKI01534!13.11.17!2.1 ! Neuerstellung             ! von Helden
      * AKI01742!15.01.20!2.2 ! Neue Felder für BU/RFN    ! BLO
      ***************************************************************** 
      * AENDERUNGEN:                                                    
      * AUFTRAG ! DATUM  ! AENDERUNG                       ! NAME       
      * --------------------------------------------------------------- 
      * DBO8172 !18.03.21! MIA: FELDER ENTFALLEN,          ! T13AC20    
      *         !        !      NEUE FELDER HINZU          !
      * --------!--------!---------------------------------!----------- 
      *         !        !                                 !            
      *         !        !                                 !            
      * ENDE-LOGBUCH
      *****************************************************************
      *
           05 SCHLUESSEL.
              10 SA-VERTRAGSID.
                 15 SA                           PIC  X(003).
                 15 VERTRAGSID                   PIC  X(013).
              10 GES                             PIC  X(002).
              10 VNR                             PIC  X(015).
              10 BERECH-KZN                      PIC  X(001).
           05 LPR.
              10 ZAHLBTG                         PIC S9(011)V99
                                                      PACKED-DECIMAL.
              10 SUMDK                           PIC S9(011)V99
                                                      PACKED-DECIMAL.
              10 BEITRAG-VUE                     PIC S9(011)V99
                                                      PACKED-DECIMAL.
              10 SUMME-VUE                       PIC S9(011)V99
                                                      PACKED-DECIMAL.
              10 GUTHABEN-VUE                    PIC S9(011)V99
                                                      PACKED-DECIMAL.
              10 JAHRBTG                         PIC S9(011)V99
                                                      PACKED-DECIMAL.
      *
           05 VER REDEFINES LPR.
              10 GRTBTG                          PIC S9(011)V99
                                                      PACKED-DECIMAL.
      *
           05 LPT REDEFINES LPR.
              10 LFDNR                           PIC 9(4).
              10 RTBTGVT                         PIC S9(011)V99
                                                      PACKED-DECIMAL.
              10 ZAHLBTGVT                       PIC S9(011)V99
                                                      PACKED-DECIMAL.
