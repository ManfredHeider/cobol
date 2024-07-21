      ******************************************************************
      *                                                                *
      *                                                                *
      *    COPY-MEMBER ZUR FORMALEN AUFBEREITUNG LAUFENDE AKTUALISIER- *
      *    UNG AML                                                     *
      *                - Teilschicht AML -                             *
      *                                                                *
      *    BETROFFENE                                                  *
      *    TABELLEN    VERTRAG                                         *
      *                                                                *
      *    SATZLÄNGE   60                                              *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                         !NAME     !VERS.*
      *AUFTRAG !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *16.08.98!NEUERSTELLUNG                          !S.JOST   ! 01  *
      *AKI00001!                                       !         !     *
      *        !                                       !         !     *
      *        !                                       !         !     *
      ******************************************************************
      * AENDERUNGEN:                                                    
      * AUFTRAG    ! DATUM  ! AENDERUNG                    ! NAME       
      * --------------------------------------------------------------  
      * DBO-9916   !06.04.22! VAEND-DAT AUFGENOMMEN        ! T13AC20    
      * -----------!--------!------------------------------!----------  
      *            !        !                              !            
      *            !        !                              !            
      *                                                                 
      * ENDE-LOGBUCH                                                    
      ******************************************************************
      *
       01  AML-SCHICHT-VERTRAG-SATZ.
           05 V-S-KEY.
              10 V-S-GES                         PIC  9(002).
              10 V-S-VNR                         PIC  9(015).
      *
           05 V-S-VERTRAGSDATEN.
              10 V-S-VZUST                       PIC  9(004).
              10 V-S-AEND-VZUST-KZ               PIC  X(001).
              10 V-S-VZUST-DAT                   PIC  9(008).
              10 V-S-AEND-VZUST-DAT-KZ           PIC  X(001).
              10 V-S-VBEGINN                     PIC  9(008).
              10 V-S-AEND-VBEGINN-KZ             PIC  X(001).
              10 V-S-VABLAUF                     PIC  9(008).
              10 V-S-AEND-VABLAUF-KZ             PIC  X(001).
              10 V-S-VAEND-DAT                   PIC  X(008).
      *
           05 V-S-AKTUAL-KZ                      PIC  X(001).
           05 V-S-RESERVE                        PIC  X(002).