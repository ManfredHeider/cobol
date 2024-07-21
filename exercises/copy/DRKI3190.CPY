      *---------------------------------------------------------------*
      *
      *    DTA VERTRAGSTEIL
      *    GEDLEBEN - G20 (PSLIFE)
      *
      *---------------------------------------------------------------*
      *    LOGBUCH
      *****************************************************************
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AKI01534!14.11.17!2.1 ! Neuerstellung             ! Walbert
      * AKI01534!20.02.18!2.2 ! VERSSUMME und TARIFBEITRAG! SWO
      *         !        !    ! nicht mehr gepackt        !
      * AKI01742!22.11.19!2.3 ! LSTERHPROZ hinzugefügt    ! BLO
      ***************************************************************** 
      * AENDERUNGEN:                                                    
      * AUFTRAG ! DATUM  ! AENDERUNG                       ! NAME       
      * --------------------------------------------------------------- 
      * AKI01819!19.01.21! MIA: FELDER ENTFALLEN:          ! T13AC20
      *         !        !      LSTERHPROZ                 !
      *         !        !      NEUE FELDER HINZU:         !
      *         !        !      VZUST-D                    !
      *         !        ! [PVCS 2.4]                      !
      * DBO8172 !16.03.21! MIA: FELDER ENTFALLEN:          ! T13AC20    
      *         !        !      RZINS                      !
      * --------!--------!---------------------------------!----------- 
      *         !        !                                 !            
      *         !        !                                 !            
      * ENDE-LOGBUCH
      *****************************************************************
      *
      *    HAUPTSATZ SA 'C'
           05  HAUPTSATZ.
               10  SA                            PIC  X(001).
               10  ORDNUNGSBEGRIFF.
                   15  VNR                       PIC  X(015).
                   15  LFDNR                     PIC  9(004).
               10  BEZUGLFDNR                    PIC  9(004).
               10  HVT-KZN                       PIC  X(001).
               10  TARIFKLASSE                   PIC  9(005).
               10  BEGINN                        PIC  X(010).
               10  ABLAUF                        PIC  X(010).
               10  BEITRZUSTID                   PIC S9(002).
               10  VERSSUMME                     PIC S9(011)V99.
               10  ABLBZ                         PIC  X(010).
               10  TARIFBEITRAG                  PIC S9(011)V99.
               10  VZUST-D                       PIC S9(003).
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
