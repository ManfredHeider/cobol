      ******************************************************************
      *              #####################                             *
      *     ########    D R K I 0 3 5 4    ########                    *
      *              #####################                             *
      *                                                                *
      *  COPY-MEMBER FUER AUFBEREITUNGSPROGRAMM AML VERTRAGSDATEN      *
      *       LEBENTEILSCHICHT                                         *
      *                                                                *
      *    SATZLƒNGE   150                                             *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ƒNDERUNG                         !NAME     !VERS.*
      *AUFTRAG !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *16.07.98!NEUERSTELLUNG                          !HUEBNER  ! 01  *
      *KI-NEU  !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
010310*01.03.10! Neues Feld TTYP-BEITR und Feldnamen   ! HSE     ! 02  *
      *AKI00920! RATEN-BEITRAG in SCHICHT-BEITR geae.  !         !     *
      *--------!---------------------------------------!---------!-----*
260810*26.08.10! Neues Feld DEPZINSUEBANT              ! HSE     ! 03  *
      *AKI00969! RECL von 111 auf 150 erhoeht          !         !     *
      *--------!---------------------------------------!---------!-----*
020614*02.06.14! Neue Felder: RENTENSCHLUESSEL und     ! HSE     ! 04  *
      *AKI01296! RENTENSCHLUESSELDATUM                 !         !     *
      *--------!---------------------------------------!---------!-----*
      *13.02.20! AKI01757 ƒNDERUNG AML-RT 032020       !T13AC20  !2.6  *
      *        !   NEUES FELD: BTGRUEKZNPFL            !         !     *
      ****************************************************************
      * AENDERUNGEN:                                                  
      * AUFTRAG    ! DATUM  ! AENDERUNG                    ! NAME     
      * --------------------------------------------------------------
      * DBO-9444   !16.11.21! NEUE FELDER:                 ! T13AC20  
      *            !        !    KLB-ERH-AUSSCHL           !          
      * -----------!--------!------------------------------!----------
      * DBO-11133  !20.04.23! NEUE FELDER:                 ! T13AC20  
      *            !        !    RABSTUFE                  !          
      * -----------!--------!------------------------------!----------
      *            !        !                              !          
      *            !        !                              !          
      *                                                               
      * ENDE-LOGBUCH                                                  
      ****************************************************************
       01  DRKI0354.

           02  AML-LEBENTEILSCHICHT.
           05  AML-GES                           PIC  9(002).
           05  AML-VNR                           PIC  9(015).

           05  AML-TEILTYP                       PIC  9(002).
           05  AML-TEILSCHICHT                   PIC  9(003).
      *
           05  AML-DATDYNERHOEH                  PIC  X(010).
           05  AML-DATDYNERHOEH-KZ               PIC  X(001).
      *        J WENN DATDYNERHOEH GE‰NDERT
010310     05  AML-SCHICHTBEITR                  PIC  9(011)V99.
010310     05  AML-SCHICHTBEITR-KZ               PIC  X(001).
      *        J WENN Schicht-Beitrag GE‰NDERT
           05  AML-VERSSUMME                     PIC  9(011)V99.
           05  AML-VERSSUMME-KZ                  PIC  X(001).   
      *        J WENN VERSSUMME GE‰NDERT                       
           05  AML-TARBEZ                        PIC  X(015).   
           05  AML-TARBEZ-KZ                     PIC  X(001).   
      *        J WENN TARBEZ GE‰NDERT                          
           05  AML-BEITRZUST                     PIC  X(002).   
           05  AML-BEITRZUST-KZ                  PIC  X(001).   
      *        J WENN BEITRZUST GE‰NDERT                       
           05  AML-VERTRZUST                     PIC S9(002).   
           05  AML-VERTRZUST-KZ                  PIC  X(001).   
      *        J WENN VERTRZUST GE‰NDERT                       
           05  AML-VERTRZUSTDAT                  PIC  X(010).   
           05  AML-VERTRZUSTDAT-KZ               PIC  X(001).   
      *        J WENN VERTRZUSTDAT GE‰NDERT
041005     05  AML-SCHICHTZUGART                 PIC  X(001).
041005     05  AML-SCHICHTZUGART-KZ              PIC  X(001).
      *        J WENN VERTRZUSTDAT GE‰NDERT
010310     05  AML-TTYPBEITR                     PIC  9(011)V99.
010310     05  AML-TTYPBEITR-KZ                  PIC  X(001).   
      *        J WENN TTYPBEITR GE‰NDERT                       
260810     05  AML-DEPZINSUEBANT                 PIC  9(003)V99.
260810     05  AML-DEPZINSUEBANT-KZ              PIC  X(001).   
      *        J WENN DEPZINSUEBANT GE‰NDERT
020614     05  AML-RNTSCHL                       PIC  9(001).
>>         05  AML-RNTSCHL-KZ                    PIC  X(001).
      *        J WENN RNTSCHL GE‰NDERT                      
           05  AML-RNTSCHLDAT                    PIC  X(010).
    <<     05  AML-RNTSCHLDAT-KZ                 PIC  X(001).
020614*        J WENN RNTSCHLDAT GE‰NDERT                   
           05  AML-BTGRUEKZNPFL                  PIC  X(001).
           05  AML-BTGRUEKZNPFL-KZ               PIC  X(001).
      *        J WENN BTGRUEKZNPFL GE‰NDERT
           05  AML-KLB-ERH-AUSSCHL               PIC  X(001).   
           05  AML-KLB-ERH-AUSSCHL-KZ            PIC  X(001).   
      *        J WENN KLB-ERH-AUSSCHL GE‰NDERT
           05  AML-RABSTUFE                      PIC  X(003).
           05  AML-RABSTUFE-KZ                   PIC  X(001).
      *        J WENN RABSTUFE GE‰NDERT
           05  FILLER                            PIC  X(012).
           05  AML-AKTUAL-KZ                     PIC  X(001).