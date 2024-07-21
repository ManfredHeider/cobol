      ******************************************************************
      *                                                                *
      *    COPY-MEMBER ZUR FORMALEN AUFBEREITUNG LAUFENDE AKTUALISIER- *
      *    UNG AML                                                     *
      *                - Teilschicht AML -                             *
      *                                                                *
      *    BETROFFENE                                                  *
      *    TABELLEN    LEBENTEILSCHICHT, LEBENTEILTYP, VERTRAG         *
      *                                                                *
010310*    SATZLÄNGE   150                                             *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                         !NAME     !VERS.*
      *AUFTRAG !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *16.08.98!NEUERSTELLUNG                          !S.JOST   ! 01  *
      *AKI00001!                                       !         !     *
      *--------!---------------------------------------!---------!-----*
010310*01.03.10! Neues Feld TTYP-BEITR und Feldnamen   ! HSE     ! 02  *
      *AKI00920! RATEN-BEITRAG in SCHICHT-BEITR geae.  !         !     *
      *--------!---------------------------------------!---------!-----*
260810*26.08.10! Neues Feld DEPZINSUEBANT              ! HSE     ! 03  *
      *AKI00969!                                       !         !     *
      *--------!---------------------------------------!---------!-----*
020614*02.06.14! Neue Felder: RENTENSCHLUESSEL und     !HSE      ! 04  *
      *AKI01296! RENTENSCHLUESSELDATUM                 !         !     *
      *--------!---------------------------------------!---------!-----*
      *13.02.20! AKI01757 ÄNDERUNG AML-RT 032020       !T13AC20  !2.6  *
      *        !   NEUES FELD: BTGRUEKZNPFL            !         !     *
      *--------!---------------------------------------!---------!-----*
      ****************************************************************
      * AENDERUNGEN:                                                  
      * AUFTRAG    ! DATUM  ! AENDERUNG                    ! NAME     
      * --------------------------------------------------------------
      * DBO-9444   !16.11.21! NEUE FELDER:                 ! T13AC20  
      *            !        !    KLB-ERH-AUSSCHL           !          
      * -----------!--------!------------------------------!----------
      * DBO-11133  !20.04.23! SATZLÄNGER VON 130 AUF 150   ! T13AC20  
      *            !        ! GEÄNDERT                     !          
      *            !        ! NEUE FELDER:                 !          
      *            !        !    RABSTUFE                  !          
      * -----------!--------!------------------------------!----------
      *            !        !                              !          
      *            !        !                              !          
      *                                                               
      * ENDE-LOGBUCH                                                  
      ****************************************************************
      *
       01  AML-TEILSCHICHT-SATZ.
           05  S-KEY.
               10  S-GES                         PIC  9(002).
               10  S-VNR                         PIC  9(015).   
               10  S-TEILTYP                     PIC  9(002).   
               10  S-TEILSCHICHT                 PIC  9(003).   
      *                                                       
           05  S-TEILSCHICHTDATEN.                            
               10  S-DYN-DAT                     PIC  9(008).   
               10  S-AEND-DYN-DAT-KZ             PIC  X(001).   
010310         10  S-SCHICHT-BEITR               PIC  9(011)V99.
010310         10  S-AEND-SCHICHT-BEITR-KZ       PIC  X(001).   
               10  S-VERS-SUMME                  PIC  9(011)V99.
               10  S-AEND-VERS-SUMME-KZ          PIC  X(001).   
               10  S-TARIF-BEZ                   PIC  X(015).   
               10  S-AEND-TARIF-BEZ-KZ           PIC  X(001).   
               10  S-BZUST                       PIC  9(002).   
               10  S-AEND-BZUST-KZ               PIC  X(001).   
               10  S-VZUST                       PIC  9(004).   
               10  S-AEND-VZUST-KZ               PIC  X(001).   
               10  S-VZUST-DAT                   PIC  9(008).   
               10  S-AEND-VZUST-DAT-KZ           PIC  X(001).   
041005         10  S-SCHICHTZUGART               PIC  X(001).   
041005         10  S-AEND-SCHICHTZUGART-KZ       PIC  X(001).   
010310         10  S-TTYP-BEITR                  PIC  9(011)V99.
010310         10  S-AEND-TTYP-BEITR-KZ          PIC  X(001).   
260810         10  S-DEPZINSUEBANT               PIC  9(003)V99.
260810         10  S-AEND-DEPZINSUEBANT-KZ       PIC  X(001).   
020614         10  S-RNTSCHL                     PIC  9(001).   
>>             10  S-AEND-RNTSCHL-KZ             PIC  X(001).   
    <<         10  S-RNTSCHL-DAT                 PIC  9(008).   
020614         10  S-AEND-RNTSCHL-DAT-KZ         PIC  X(001).   
               10  S-BTGRUEKZNPFL                PIC  X(001).   
               10  S-AEND-BTGRUEKZNPFL-KZ        PIC  X(001).   
               10  S-KLB-ERH-AUSSCHL             PIC  X(001).   
               10  S-AEND-KLB-ERH-AUSSCHL-KZ     PIC  X(001).   
               10  S-RABSTUFE                    PIC  X(003).   
               10  S-AEND-RABSTUFE-KZ            PIC  X(001).   
               10                                PIC  X(016).   
      *                                                       
           05  S-AKTUAL-KZ                       PIC  X(001).