      *---------------------------------------------------------------*
      *
      *    DTA RECHENWERTE
      *    GEDLEBEN - G20 (PSLIFE)
      *
      *---------------------------------------------------------------*
      *    LOGBUCH
      *****************************************************************
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AKI01534!13.11.17!2.1 ! Neuerstellung             ! von Helden
      * AKI01742!22.11.19!2.2 ! BTGVTKI - FORENDITE hinzu ! Lorson
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
      *    HAUPTSATZ SA 'C'
           05 HAUPTSATZ.
              10 SA                              PIC  X(001).
              10 VNR                             PIC  X(015).
              10 BERECH-KZN                      PIC  X(001).
              10 GRTBTG                          PIC S9(011)V99.
              10 ZAHLBTG                         PIC S9(011)V99.
              10 BTGVTKI                OCCURS 10 TIMES.
                 15 LFDNR                        PIC 9(4).
                 15 RTBTGVT                      PIC S9(11)V99.
                 15 ZAHLBTGVT                    PIC S9(11)V99.
              10 SUMDK                           PIC S9(11)V99.
              10 BEITRAG-VUE                     PIC S9(11)V99.
              10 SUMME-VUE                       PIC S9(11)V99.
              10 GUTHABEN-VUE                    PIC S9(11)V99.
              10 JAHRBTG                         PIC S9(11)V99.
      *
      *    VORSATZ   SA 'A'
           05 VORSATZ  REDEFINES HAUPTSATZ.
              10 V-SA                            PIC  X(001).
              10 V-ABSENDER                      PIC  X(003).
              10 V-ERSTELLDAT                    PIC  X(010).
              10 V-EMPFAENGER                    PIC  X(004).
              10 V-INHALT                        PIC  X(020).
      *
      *    NACHSATZ  SA 'E'
           05 NACHSATZ REDEFINES HAUPTSATZ.
              10 N-SA                            PIC  X(001).
              10 N-ANZSATZ                       PIC  9(009).
