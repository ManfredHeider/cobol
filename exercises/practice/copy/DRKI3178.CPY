      *---------------------------------------------------------------*
      *
      *    DTA VERTRAG
      *    GEDLEBEN - G20 (PSLIFE)
      *
      *
      *    SATZLÄNGE VARIABEL
      *
      *---------------------------------------------------------------*
      *    LOGBUCH
      *****************************************************************
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AKI01534!20.10.17!2.1 ! NEUERSTELLUNG             ! SWO
      * AKI01627!30.08.18!2.2 ! ERWEITERUNG UM TIMETAMP   ! DETMAR
      *         !        !2.4 !                           !
      * AKI01742!22.11.19!2.5 ! DYNLTGRVID,LDYNERHDAT     ! BLO
      *         !        !    ! entfernt, DYN-* neu       !
      ***************************************************************** 
      * AENDERUNGEN:                                                    
      * AUFTRAG ! DATUM  ! AENDERUNG                       ! NAME       
      * --------------------------------------------------------------- 
      * AKI01819!30.11.20! MIA: FELDER ENTFALLEN,          ! T13AC20
      *         !        !      NEUE FELDER HINZU          !
      *         !        ! [PVCS 2.6]                      !
      * DBO8172 !16.03.21! MIA: FELDER ENTFALLEN           ! T13AC20    
      ***************************************************************** 
      * AENDERUNGEN:                                                    
      * AUFTRAG    ! DATUM  ! AENDERUNG                    ! NAME      
      * ---------------------------------------------------------------
      * DBO-10280  !08.08.22! FELD STANDVERTR DURCH FILLER ! T13AC20   
      *            !        ! ERSETZT                      !          
      *            !        ! (GENERALI LIEFERT DAS FELD   !          
      *            !        !  NOCH)                       !          
      * -----------!--------!------------------------------!----------- 
      *            !        !                              !          
      *            !        !                              !          
      * ENDE-LOGBUCH
      *****************************************************************
      *
      *    HAUPTSATZ SA 'C'
           05 HAUPTSATZ.
              10 SA                              PIC  X(001).
              10 MANDANT                         PIC  9(007).
              10 VNR                             PIC  X(015).
              10 VZUST-G                         PIC S9(003).
                 88 :XX:-VERTR-KZN-G-AKTIV       VALUE +00
                                                       +12
                                                       +14.
                 88 :XX:-VERTR-KZN-G-INAKTIV     VALUE +06.
              10 VZUST-D                         PIC S9(003).
                 88 :XX:-VERTR-KZN-D-UNBEKANNT   VALUE -002.
                 88 :XX:-VERTR-KZN-D-AKTIV       VALUE +001
                                                       +002
                                                       +003
                                                       +004
                                                       +005
                                                       +007.
                 88 :XX:-VERTR-KZN-D-INAKTIV     VALUE +101
                                                       +102
                                                       +103
                                                       +104.
              10 VERTRIEBSTAT                    PIC  X(001).
              10 INFSPERR-KZN                    PIC  X(001).
              10 MAKLER-KZN                      PIC  X(001).
              10 RNTBEZUG-KZN                    PIC  X(001).
              10 STORNODAT                       PIC  X(010).
              10 BAV-KZN                         PIC  X(001).
              10 VBNR                            PIC  X(007).
              10 PRODKLASSE                      PIC  9(005).
              10 WERBEBEZEICH                    PIC S9(005).
              10 GSMTTAR-BEZ                     PIC  X(025).
              10 ZAHLWID                         PIC S9(002).
      *       EHEMALS STANDVERTR:
              10 FILLER                          PIC  X(026).
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