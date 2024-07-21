      ******************************************************************
      *                                                                *
      *    COPY-MEMBER ZU Tarifdaten AML                               *
      *                                                                *
      *    BETROFFENE                                                  *
      *    TABELLEN    VERSLEBENTARIF                                  *
      *                                                                *
      *    SATZLÄNGE   400                                             *
      *                                                                *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                         !NAME     !VERS.*
      *AUFTRAG !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *16.07.98!NEUERSTELLUNG                          !S.JOST   ! 01  *
      *AKI00001!                                       !         !     *
      *        !                                       !         !     *
      *11.04.03! AKI00337 neue Felder BAV              !P. Hübner!     *
      *--------!---------------------------------------!---------!-----*
      *10.04.08! AKI00711 neue Felder                  !I.Frenzel!     *
      *        !                                       !         !     *
      * AKI00860!19.08.09!2.5 ! Erweiterung um Abtretungs-  !I. Frenzel*
      *         !        !    ! und Rentenbezugs-KZ         !          *
      * AKI00918!17.02.10!2.7 ! Erweiterung um              !I. Frenzel*
      *         !        !    ! BU-Bezugs-KZ                !          *
      * AKI00909!02.03.11!2.8 ! Erweiterung um Rechnungs-   !I. Frenzel*
      *         !        !    ! zins                        !          *
      * AKI01065!10.08.11!2.9 ! Rechnungszins entfernt      !I. Frenzel*
      *         !        !    ! Bonus-KZ und Anteile der    !          *
      *         !        !    ! konventionellen Anlage      !          *
      *         !        !    ! eingefügt                   !          *
      *---------!--------!----!-----------------------------!----------*
250716* AKI01455!25.07.16!2.10! Produktbezeichnung fuer     ! HSE      *
      *         !        !    ! PORTOS aufgenommen          !          *
      *---------!--------!----!-----------------------------!----------*
150217* AKI01492!15.02.17!2.11! neue Felder aufgenommen:    ! SWO      *
      *         !        !    !    URSPRPOLDAT              !          *
      *         !        !    !    LVURSPRBEG               !          *
      *         !        !    !    FOMGNTART                !          *
      *         !        !    !    FOMGNTKWAB               !          *
      *         !        !    !    FOMGNTKWBIS              !          *
      *---------!--------!----!-----------------------------!----------*
170803* AKI01538!03.08.17!2.12! NEUE FELDER AUFGENOMMEN:    ! MBS      *
      *         !        !    !    BU-KT-KZN                !          *
      *         !        !    !    VITALITY-KZN             !          *
      *         !        !    !    BERUFSGRP-KZN            !          *
      *---------!--------!----!-----------------------------!----------*
160818* AKI01618!16.08.18!2.13! NEUES FELD AUFGENOMMEN:     ! DETMAR   *
      *         !        !2.15!    STAND-VERTR              !          *
      *---------!--------!----!-----------------------------!----------*
      * AKI01706!10.09.19!2.16! neue Felder aufgenommen:    ! SWO      *
      *         !        !    !    NEUABSCHLKZN             !          *
      *         !        !    !    BASISZINS-BETR           !          *
      *         !        !    !    ZINSBOOST-SATZ           !          *
      *         !        !    !    ZINSBOOST-BETR           !          *
      *---------!--------!----!-----------------------------!----------*
      * AKI01782!14.05.20!2.17! neue Felder aufgenommen:    ! T13AC20  *
      *         !        !    !    EU-KLAUSEL-KZN           !          *
      *---------!--------!----!-----------------------------!----------*
      * AKI01825!03.02.21!2.18! neue Felder aufgenommen:    ! T13AC20  *
      *         !        !    !    SCHUELER-BU              !          *
      *         !        !    ! altes Feld gelöscht:       :!          *
      *         !        !    !    BERUFSGRP-KZN            !          *
      ****************************************************************
      * AENDERUNGEN:                                                  
      * AUFTRAG    ! DATUM  ! AENDERUNG                    ! NAME     
      * --------------------------------------------------------------
      * DBO-9070   !16.08.21! neue Felder aufgenommen:     ! T13AC20  
      *            !        !    BTG-AG                    !          
      *            !        !    BTG-AN                    !          
      *            !        ! Satzlänge auf 400 erweitert  !          
      * -----------!--------!------------------------------!----------
      * DBO-9444   !16.11.21! neue Felder aufgenommen:     ! T13AC20  
      *            !        !    KLB-ERH-AUSSCHL           !          
      *            !24.11.21! Ausprägung ZINSBOOST-SATZ    ! T13AC20  
      *            !        ! geändert                     !          
      * -----------!--------!------------------------------!----------
      * DBO-9994   !02.05.22! Feld STAND-VERTR             ! U131381  
      *            !        !    ausgebaut                 !
      * -----------!--------!------------------------------!----------
      * DBO-10780  !23.01.23! neue Felder aufgenommen:     ! T13AC20  
      *            !        !    WERTSICHOPTKZN            !          
      * -----------!--------!------------------------------!----------
      *            !        !                              !          
      *            !        !                              !          
      *                                                               
      * ENDE-LOGBUCH                                                  
      ****************************************************************
      *
       01  AML-TARIF-SATZ.
           05  L-KEY.
               10  L-GES                         PIC  9(002).
               10  L-VNR                         PIC  9(015).
      *
           05  L-TARIFDATEN.
               10  L-RUMS                        PIC  X(003).
               10  L-AEND-RUMS-KZ                PIC  X(001).
               10  L-RENTE-ZAHLW                 PIC  X(002).
               10  L-AEND-RENTE-ZAHLW-KZ         PIC  X(001).
               10  L-RENTE-GARANT                PIC  X(008).
               10  L-AEND-RENTE-GARANT-KZ        PIC  X(001).
               10  L-DIREKT-VERS-KZ              PIC  X(002).
               10  L-AEND-DIREKT-VERS-KZ         PIC  X(001).
110403         10  L-BAV-FINART                  PIC  9(002).
110403         10  L-AEND-BAV-FINART-KZ          PIC  X(001).
110403         10  L-BAV-DURCHFWEG               PIC  9(002).
110403         10  L-AEND-BAV-DURCHFWEG-KZ       PIC  X(001).
               10  L-LV-ART                      PIC  9(002).
               10  L-AEND-LV-ART-KZ              PIC  X(001).
               10  L-KAP-RECHT-KZ                PIC  X(001).
               10  L-AEND-KAP-RECHT-KZ           PIC  X(001).
100408         10  L-FONDSPALETTE                PIC  X(008).
>>             10  L-AEND-FONDSPALETTE-KZ        PIC  X(001).
               10  L-FONDSAKTKZN                 PIC  X(001).
               10  L-AEND-FONDSAKTKZN-KZ         PIC  X(001).
               10  L-DEPBETR                     PIC  9(011)V99.
               10  L-AEND-DEPBETR-KZ             PIC  X(001).
               10  L-ZSBISWTGJ                   PIC  9(011)V99.
               10  L-AEND-ZSBISWTGJ-KZ           PIC  X(001).
               10  L-DEPZSSATZ                   PIC  9(003)V99.
               10  L-AEND-DEPZSSATZ-KZ           PIC  X(001).
    <<         10  L-DEPWT                       PIC  9(008).
100408         10  L-AEND-DEPWT-KZ               PIC  X(001).
171208         10  L-BTGVERF                     PIC  X(001).
172108*            Zulagenart
171208         10  L-AEND-BTGVERF-KZ             PIC  X(001).
171208*            J WENN BTGVERF GEAENDERT
171208         10  L-DAUZULANTRKZN               PIC  X(001).
171208*            Dauerzulagekennzeichen
171208         10  L-AEND-DAUZULANTRKZN-KZ       PIC  X(001).
171208*            J wenn Dauerzulagekennzeichen geändert
190809         10  L-ABTRETUNGSKZN               PIC  X(001).
190809*            Abtretungskennzeichen
190809         10  L-AEND-ABTRETUNGSKZN-KZ       PIC  X(001).
190809*            J wenn Abtretungskennzeichen geändert
190809         10  L-RENTENBEZUGSKZN             PIC  9(001).
190809*            Rentenbezugskennzeichen
190809         10  L-AEND-RENTENBEZUGSKZN-KZ     PIC  X(001).
190809*            J wenn Rentenbezugskennzeichen geändert
170210         10  L-BUBEZUGSKZN                 PIC  X(001).
170210*            Rentenbezugskennzeichen
170210         10  L-AEND-BUBEZUGSKZN-KZ         PIC  X(001).
170210*            J wenn BU-Bezugskennzeichen geändert
020311*        10  L-RZINS                       PIC 9(003)V99.
020311*            Rechnungszins
020311*        10  L-AEND-RZINS-KZ               PIC X(001).
020311*            J wenn Rechnungszins geändert
100811         10  L-BONUSKZN                    PIC  X(001).
>>    *            Bonuskennzeichen
               10  L-AEND-BONUSKZN-KZ            PIC  X(001).
      *            J wenn Bonuskennzeichen geändert
               10  L-KONVANLPROZ                 PIC  9(003).
      *            Anteil der konventionellen Anlage in Prozent
               10  L-AEND-KONVANLPROZ-KZ         PIC  X(001).
100811*            J wenn Anteil der konventionellen Anlage geändert
250716         10  L-PRODBEZEICH-SCHL            PIC  9(004).
>>    *            Schluessel der Produktbezeichnung
    <<         10  L-AEND-PRODBEZEICH-SCHL-KZ    PIC  X(001).
250716*            J wenn Schluessel der Produktbezeichnung geändert
150217         10  L-URSPRPOLDAT                 PIC  X(008).
>>    *            Ursprüngliches Policendatum
               10  L-AEND-URSPRPOLDAT-KZ         PIC  X(001).
      *            J wenn ursprüngliches Policendatum geändert
               10  L-LVURSPRBEG                  PIC  X(008).
      *            Ursprünglicher Vertragsbeginn
               10  L-AEND-LVURSPRBEG-KZ          PIC  X(001).
      *            J wenn ursprünglicher Vertragsbeginn geändert
               10  L-FOMGNTART                   PIC  X(001).
      *            Kennzeichen für Fondsmanagement
               10  L-AEND-FOMGNTART-KZ           PIC  X(001).
      *            J wenn Kennzeichen Fondsmanagement geändert
               10  L-FOMGNTKWAB                  PIC  X(008).
      *            Fondsmanagement Kundenwunsch ab
               10  L-AEND-FOMGNTKWAB-KZ          PIC  X(001).
      *            J wenn Fondsmanagement Kundenwunsch ab geändert
               10  L-FOMGNTKWBIS                 PIC  X(008).
      *            Fondsmanagement Kundenwunsch bis
    <<         10  L-AEND-FOMGNTKWBIS-KZ         PIC  X(001).
150217*            J wenn Fondsmanagement Kundenwunsch bis geändert
170803         10  L-BU-KT-KZN                   PIC  X(001).
>>    *            Kennz.BU kombiniert mit Krankentage.
               10  L-AEND-BU-KT-KZN-KZ           PIC  X(001).
      *            Kennz.BU kombiniert mit Krankentage geändert
               10  L-VITALITY-KZN                PIC  9(003).
      *            Kennz.BU kombiniert mit Vitality
               10  L-AEND-VITALITY-KZN-KZ        PIC  X(001).
      *            Kennz.BU kombiniert mit Vitality geändert.
               10  L-NEUABSCHLKZN                PIC  X(001).
      *            Kennzeichen für Neuabschluss
               10  L-AEND-NEUABSCHLKZN-KZ        PIC  X(001).
      *            Kennzeichen Neuabschluss geändert
               10  L-BASISZINS-BETR-ALPHA        PIC  X(014).
               10  L-BASISZINS-BETR REDEFINES L-BASISZINS-BETR-ALPHA
                                                 PIC  9(012)V99.
      *            Betrag zum Basiszins
               10  L-AEND-BASISZINS-BETR-KZ      PIC  X(001).
      *            Kennzeichen Betrag zum Basiszins geändert
               10  L-ZINSBOOST-SATZ-ALPHA        PIC  X(008).
               10  L-ZINSBOOST-SATZ REDEFINES L-ZINSBOOST-SATZ-ALPHA
                                                 PIC  9(005)V999.
      *            Zinssatz des Boosters
               10  L-AEND-ZINSBOOST-SATZ-KZ      PIC  X(001).
      *            Kennzeichen Zinssatz des Boosters geändert
               10  L-ZINSBOOST-BETR-ALPHA        PIC  X(014).
               10  L-ZINSBOOST-BETR REDEFINES L-ZINSBOOST-BETR-ALPHA
                                                 PIC  9(012)V99.
      *            Betrag zum Boosterzins
               10  L-AEND-ZINSBOOST-BETR-KZ      PIC  X(001).
      *            Kennzeichen Betrag zum Boosterzins geändert
               10  L-EU-KLAUSEL-KZN              PIC  X(001).
      *            Kennzeichen für EU-Klausel
               10  L-AEND-EU-KLAUSEL-KZN-KZ      PIC  X(001).
      *            Kennzeichen EU-Klausel geändert
               10  L-SCHUELER-BU                 PIC  X(001).
      *            Kennzeichen für Schüler-BU
               10  L-AEND-SCHUELER-BU-KZ         PIC  X(001).
      *            Kennzeichen Schüler-BU geändert
               10  L-BTG-AG                      PIC  9(011)V99.
      *            Tarifbeitrag Arbeitgeber-Anteil
               10  L-AEND-BTG-AG-KZ              PIC  X(001).
      *            Kennzeichen Tarifbeitrag Arbeitgeber-Anteil geändert
               10  L-BTG-AN                      PIC  9(011)V99.
      *            Tarifbeitrag Arbeitnehmer-Anteil
               10  L-AEND-BTG-AN-KZ              PIC  X(001).
      *            Kennzeichen Tarifbeitrag Arbeitnehmer-Anteil geändert
               10  L-KLB-ERH-AUSSCHL-UEBERLAUF   PIC  X(002).
      *            reserviert und nicht zu benutzen; muss mit
      *            low-value X'0000' belegt werden
               10  L-KLB-ERH-AUSSCHL             PIC  X(001).
      *            KLB-Erhöhungsausschluss
               10  L-AEND-KLB-ERH-AUSSCHL-KZ     PIC  X(001).
      *            Kennzeichen KLB-Erhöhungsausschluss geändert
               10  L-WERTSICHOPTKZN              PIC  X(001).
      *            Wertsicherungsoptionskennzeichen
               10  L-AEND-WERTSICHOPTKZN-KZ      PIC  X(001).
      *            Kennzeichen Wertsicherungsoptionskennzeichen geändert
               10  FILLER                        PIC  X(154).
      *            Reserve
      *
           05  L-AKTUAL-KZ                       PIC  X(001).