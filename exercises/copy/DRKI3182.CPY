      *---------------------------------------------------------------*
      *
      *    AKTUALISIERUNGGSDATEN AUS AUFBEREITUNG LBNPSVERTRAG FÜR
      *    LBNPSVERTRAG
      *    GEDLEBEN - G20 (PSLIFE)
      *
      *---------------------------------------------------------------*
      *    LOGBUCH
      *****************************************************************
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AKI01534!20.10.17!2.1 ! NEUERSTELLUNG             ! SWO
050918* AKI01627!05.09.18!2.2 ! ERWEITERUNG UM TIMESTAMP  ! DETMAR
      *         !        !2.4 !                           !
      * AKI01742!17.01.20!2.5 ! NEUE FELDER FÜR BU/RFN    ! LORSON
      ***************************************************************** 
      * AENDERUNGEN:                                                    
      * AUFTRAG ! DATUM  ! AENDERUNG                       ! NAME       
      * --------------------------------------------------------------- 
      * AKI01819!15.01.21! MIA: FELDER ENTFALLEN,          ! T13AC20
      *         !        !      NEUE FELDER HINZU          !
      *         !        ! [PVCS 2.6]                      !
      * DBO8172 !16.03.21! MIA: FELDER ENTFALLEN           ! T13AC20    
      ***************************************************************** 
      * AENDERUNGEN:                                                    
      * AUFTRAG    ! DATUM  ! AENDERUNG                    ! NAME      
      * ---------------------------------------------------------------
      * DBO-10280  !08.08.22! FELD STANDVERTR GELÖSCHT     ! T13AC20   
      * -----------!--------!------------------------------!----------- 
      *            !        !                              !          
      *            !        !                              !          
      * ENDE-LOGBUCH
      *****************************************************************
      *
           05 VERARB-KZN                         PIC  X(001).
           05 GES                                PIC  X(002).
           05 VNR                                PIC  X(015).
           05 PRODUKTID                          PIC S9(004) BINARY.
           05 PRODUKTID-KZN                      PIC  X(001).
           05 STVD0067-KNR                       PIC S9(015)
                                                        PACKED-DECIMAL.
           05 STVD0067-KNR-KZN                   PIC  X(001).
           05 PRODKLAS-KNR                       PIC S9(015)
                                                        PACKED-DECIMAL.
           05 PRODKLAS-KNR-KZN                   PIC  X(001).
           05 GSMTTAR-BEZ                        PIC  X(025).
           05 GSMTTAR-BEZ-KZN                    PIC  X(001).
           05 WERBEBEZ-KNR                       PIC S9(015)
                                                        PACKED-DECIMAL.
           05 WERBEBEZ-KNR-KZN                   PIC  X(001).
           05 RNTBEZUG-KZN                       PIC  X(001).
           05 RNTBEZUG-KZN-KZN                   PIC  X(001).
           05 BAV-KZN                            PIC  X(001).
           05 BAV-KZN-KZN                        PIC  X(001).
           05 VZUST-D-KNR                        PIC S9(004) BINARY.
           05 VZUST-D-KNR-KZN                    PIC  X(001).
           05 VZUST-D-DAT                        PIC  X(010).
           05 VZUST-D-DAT-KZN                    PIC  X(001).
