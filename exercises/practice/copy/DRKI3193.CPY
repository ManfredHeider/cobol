      *---------------------------------------------------------------*
      *
      *    AKTUALISIERUNGGSDATEN AUS AUFBEREITUNG VERTRAGSTEIL
      *    GEDLEBEN - G20 (PSLIFE)
      *
      *---------------------------------------------------------------*
      ***************************************************************** 
      *    VERSION                                                      
      ***************************************************************** 
      * AENDERUNGEN:                                                    
      * AUFTRAG ! DATUM  ! AENDERUNG                       ! NAME       
      * --------------------------------------------------------------- 
      * AKI01534!20.12.17! Neuerstellung                   ! Walbert
      * --------!--------!---------------------------------!----------- 
      * DBO8172 !09.03.21! MIA: NEUE FELDER HINZU:         ! T13AC20
      *         !        !      VZUST-D-KZN                !
      *         !        !      FELDER UMBENANNT:          !
      *         !        !      VTEILZUSTID  IN VZUST-D    !
      *         !        !      VTEILZUSTDAT IN VZUST-D-DAT!
      * DBO8172 !16.03.21! MIA: FELDER ENTFALLEN:          ! T13AC20    
      *         !        !      RZINS                      !
      * --------!--------!---------------------------------!----------- 
      *         !        !                                 !            
      *                                                                 
      * ENDE-LOGBUCH                                                    
      ***************************************************************** 
      *
           05 VERARB-KZN                         PIC  X(001).
           05 GES                                PIC  X(002).
           05 VNR                                PIC  X(015).
           05 LFDNR                              PIC  9(004).
           05 BEZUGLFDNR                         PIC  9(004).
           05 BEZUGLFDNR-KZN                     PIC  X(001).
           05 HVT-KZN                            PIC  X(001).
           05 HVT-KZN-KZN                        PIC  X(001).
           05 TARKLASSE-KNR                      PIC S9(015)
                                                        PACKED-DECIMAL.
           05 TARKLASSE-KNR-KZN                  PIC  X(001).
           05 BEGINN                             PIC  X(010).
           05 BEGINN-KZN                         PIC  X(001).
           05 ABLAUF                             PIC  X(010).
           05 ABLAUF-KZN                         PIC  X(001).
           05 BEITRZUST-KNR                      PIC S9(015)
                                                        PACKED-DECIMAL.
           05 BEITRZUST-KNR-KZN                  PIC  X(001).
           05 ABLBZ                              PIC  X(010).
           05 ABLBZ-KZN                          PIC  X(001).
           05 TARIFBEITRAG                       PIC S9(011)V99
                                                      PACKED-DECIMAL.
           05 TARIFBEITRAG-KZN                   PIC  X(001).
           05 VERSSUMME                          PIC S9(011)V99
                                                      PACKED-DECIMAL.
           05 VERSSUMME-KZN                      PIC  X(001).
           05 VZUST-D                            PIC S9(003).
           05 VZUST-D-KZN                        PIC  X(001).
           05 VZUST-D-DAT                        PIC  X(010).
           05 VZUST-D-DAT-KZN                    PIC  X(001).