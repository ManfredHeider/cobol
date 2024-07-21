      ******************************************************************
      *              #####################                             *
      *     ########    D R K I 0 3 8 1    ########                    *
      *              #####################                             *
      *                                                                *
      *  COPY-MEMBER FUER AUFBEREITUNGSPROGRAMM AML VERTRAGSDATEN      *
      *       VERSLEBENTARIF                                           *
      *                                                                *
      *    SATZLÄNGE   300
      *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                         !NAME     !VERS.*
      *AUFTRAG !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *21.07.98!NEUERSTELLUNG                          !HUEBNER  ! 01  *
      *KI-NEU  !                                       !         !     *
      *10.04.08! AKI00711 neue Felder                  !I.Frenzel!2.1  *
      *09.04.09! AKI00809 Tarifprüfung in LVARTTARIF   !I.Frenzel!2.3  *
      *10.06.09! AKI00832 Mischfinanzierte BAV         !I.Frenzel!2.4  *
      * AKI00860!19.08.09!2.5 ! Erweiterung um Abtretungs- I. Frenzel*
      *         !        !    ! und Rentenbezugs-KZ       !          *
      * AKI00918!17.02.10!2.6 ! Erweiterung um            !I. Frenzel*
      *         !        !    ! BU-Bezugs-KZ              !          *
221110* AKI00966!22.11.10!    ! NETTOGESBEITRAG ergänzt   !W. Misgeld*
      * AKI00909!02.03.11!2.9! Erweiterung um Rechnungs- !I. Frenzel *
      *         !        !    ! zins                      !          *
      * AKI01065!10.08.11!2.10! Rechnungszins entfernt    !I. Frenzel*
      *         !        !    ! Bonus-KZ und Anteile der  !          *
      *         !        !    ! konventionellen Anlage    !          *
      *         !        !    ! eingefügt                 !          *
      *---------!--------!----!---------------------------!----------*
250716* AKI01455!25.07.16!2.11! Produktbezeichnung fuer   ! HSE      *
      *         !        !    ! PORTOS aufgenommen        !          *
      *         !        !    ! FILLER in HAUPTSATZ eingef!          *
      *---------!--------!----!---------------------------!----------*
150217* AKI01492!15.02.17!2.12! neue Felder aufgenommen:  ! SWO      *
      *         !        !    !    URSPRPOLDAT            !          *
      *         !        !    !    LVURSPRBEG             !          *
      *         !        !    !    FOMGNTART              !          *
      *         !        !    !    FOMGNTKWAB             !          *
      *         !        !    !    FOMGNTKWBIS            !          *
      *         !        !    ! Präfix AML in 'XXX'       !          *
      *         !        !    ! geändert                  !          *
      *---------!--------!----!---------------------------!----------*
170803* AKI01538!03.08.17!2.13! NEUE FELDER AUFGENOMMEN:  ! MBS      *
      *         !        !    !    BU-KT-KZN              !          *
      *         !        !    !    VITALITY-KZN           !          *
      *         !        !    !    BERUFSGRP-KZN          !          *
      *---------!--------!----!---------------------------!----------*
160818* AKI01618!16.08.18!2.14! NEUES FELD AUFGENOMMEN:   ! DETMAR   *
      *         !        !2.16!    STAND-VERTR            !          *
      *---------!--------!----!---------------------------!----------*
      * AKI01706!10.09.19!2.17! neue Felder aufgenommen:  ! SWO      *
      *         !        !    !    NEUABSCHLKZN           !          *
      *         !        !    !    BASISZINS-BETR         !          *
      *         !        !    !    ZINSBOOST-SATZ         !          *
      *         !        !    !    ZINSBOOST-BETR         !          *
      *---------!--------!----!---------------------------!----------*
      * AKI01782!14.05.20!2.18! neue Felder aufgenommen:  ! T13AC20  *
      *         !        !    !    EU-KLAUSEL-KZN         !          *
      *---------!--------!----!---------------------------!----------*
      * AKI01825!03.02.21!2.19! neue Felder aufgenommen:  ! T13AC20  *
      *         !        !    !    SCHUELER-BU            !          *
      *         !        !    ! altes Feld gelöscht:     :!          *
      *         !        !    !    BERUFSGRP-KZN          !          *
      ****************************************************************
      * AENDERUNGEN:                                                  
      * AUFTRAG    ! DATUM  ! AENDERUNG                    ! NAME     
      * --------------------------------------------------------------
      * DBO-9070   !16.08.21! neue Felder aufgenommen:     ! T13AC20  
      *            !        !    BTG-AG                    !          
      *            !        !    BTG-AN                    !          
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
       01  DRKI0381.
 
           02  'XXX'-VERSLEBENTARIF.
           05  'XXX'-GES                         PIC  9(002).
           05  'XXX'-VNR                         PIC  9(015).
           05  'XXX'-RUMS                        PIC  X(006).
           05  'XXX'-RUMS-KZ                     PIC  X(001).
      *        J WENN RUMS GEAENDERT
           05  'XXX'-RENTE-ZAHLW                 PIC S9(002).
           05  'XXX'-RENTE-ZAHLW-KZ              PIC  X(001).
      *        J WENN ZAHLW GEAENDERT
           05  'XXX'-RENTEGARZEIT                PIC  X(010).
           05  'XXX'-RENTE-GARZEIT-KZ            PIC  X(001).
      *        J WENN ZEIT GEAENDERT
100609*    05  'XXX'-DIREKTKZ             PIC  X(01).
100609     05  'XXX'-DIREKTKZ                    PIC  9(002).
           05  'XXX'-DIREKTKZ-KZ                 PIC  X(001).
      *        J WENN DIREKTKZ GEAENDERT
           05  'XXX'-LVART                       PIC  9(002).
           05  'XXX'-LVART-KZ                    PIC  X(001).
      *        J WENN LVART GEAENDERT
           05  'XXX'-WRECHTKZ                    PIC  X(001).
           05  'XXX'-WRECHTKZ-KZ                 PIC  X(001).
      *        J WENN WRECHTKZ GEAENDERT
100408     05  'XXX'-FONDSPALETTE                PIC  X(008).
>>         05  'XXX'-FONDSPALETTE-KZ             PIC  X(001).
      *        J WENN FONDSPALETTE GEAENDERT
           05  'XXX'-FONDSAKTKZN                 PIC  X(002).
           05  'XXX'-FONDSAKTKZN-KZ              PIC  X(001).
      *        J WENN FONDSAKTKZN GEAENDERT
           05  'XXX'-DEPBETR                     PIC  9(011)V99.
           05  'XXX'-DEPBETR-KZ                  PIC  X(001).
      *        J WENN DEPBETR GEAENDERT
           05  'XXX'-ZSBISWTGJ                   PIC  9(011)V99.
           05  'XXX'-ZSBISWTGJ-KZ                PIC  X(001).
      *        J WENN ZSBISWTGJ GEAENDERT
           05  'XXX'-DEPZSSATZ                   PIC  9(003)V99.
           05  'XXX'-DEPZSSATZ-KZ                PIC  X(001).
      *        J WENN DEPZSSATZ GEAENDERT
           05  'XXX'-DEPWT                       PIC  X(010).
    <<     05  'XXX'-DEPWT-KZ                    PIC  X(001).
100408*        J WENN DEPWT GEAENDERT
171208     05  'XXX'-BTGVERF                     PIC  9(001).
171208     05  'XXX'-BTGVERF-KZ                  PIC  X(001).
171208*        J WENN DEPZSSATZ GEAENDERT
171208     05  'XXX'-DAUZULANTRKZN               PIC  X(001).
171208     05  'XXX'-DAUZULANTRKZN-KZ            PIC  X(001).
171208*        J WENN DEPWT GEAENDERT
090409     05  'XXX'-STVD0067                    PIC  X(001).
090409     05  'XXX'-STVD0067-KZ                 PIC  X(001).
090409*        J WENN STVD0067 GEAENDERT
190809     05  'XXX'-ABTRETUNGSKZN               PIC  X(001).
190809     05  'XXX'-ABTRETUNGSKZN-KZ            PIC  X(001).
190809*        J WENN ABTRETUNGSKZN GEAENDERT
190809     05  'XXX'-RENTENBEZUGSKZN             PIC  9(001).
190809     05  'XXX'-RENTENBEZUGSKZN-KZ          PIC  X(001).
190809*        J WENN RENTENBEZUGSKZN GEAENDERT
170210     05  'XXX'-BUBEZUGSKZN                 PIC  X(001).
170210     05  'XXX'-BUBEZUGSKZN-KZ              PIC  X(001).
170210*        J WENN BUBEZUGSKZN GEAENDERT
211110     05  'XXX'-DVW-NETTOBEITRAG            PIC  9(011)V99.
211110     05  'XXX'-DVW-NETTOBEITRAG-KZ         PIC  X(001).
211110*        J WENN NETTOBEITRAG GEAENDERT
020311*    05  'XXX'-RZINS                       PIC  9(003)V99.
020311*    05  'XXX'-RZINS-KZ                    PIC  X(001).
020311*        J WENN RZINS GEAENDERT
100811     05  'XXX'-BONUSKZN                    PIC  X(001).
>>         05  'XXX'-BONUSKZN-KZ                 PIC  X(001).
      *            J wenn Bonuskennzeichen geändert
           05  'XXX'-KONVANLPROZ                 PIC  9(003).
<<         05  'XXX'-KONVANLPROZ-KZ              PIC  X(001).
100811*            J wenn Anteil der konventionellen Anlage geändert
250716     05  'XXX'-PRODBEZEICH-SCHL            PIC  9(004).
250716     05  'XXX'-PRODBEZEICH-SCHL-KZ         PIC  X(001).
250716*            J wenn Schluessel der Produktbezeichnung geändert
150217     05  'XXX'-URSPRPOLDAT                 PIC  X(010).
>>    *            Ursprüngliches Policendatum
           05  'XXX'-URSPRPOLDAT-KZ              PIC  X(001).
      *            J wenn ursprüngliches Policendatum geändert
           05  'XXX'-LVURSPRBEG                  PIC  X(010).
      *            Ursprünglicher Vertragsbeginn
           05  'XXX'-LVURSPRBEG-KZ               PIC  X(001).
      *            J wenn ursprünglicher Vertragsbeginn geändert
           05  'XXX'-FOMGNTART                   PIC  X(001).
      *            Kennzeichen für Fondsmanagement
           05  'XXX'-FOMGNTART-KZ                PIC  X(001).
      *            J wenn Kennzeichen Fondsmanagement geändert
           05  'XXX'-FOMGNTKWAB                  PIC  X(010).
      *            Fondsmanagement Kundenwunsch ab
           05  'XXX'-FOMGNTKWAB-KZ               PIC  X(001).
      *            J wenn Fondsmanagement Kundenwunsch ab geändert
           05  'XXX'-FOMGNTKWBIS                 PIC  X(010).
      *            Fondsmanagement Kundenwunsch bis
    <<     05  'XXX'-FOMGNTKWBIS-KZ              PIC  X(001).
150217*            J wenn Fondsmanagement Kundenwunsch bis geändert
170803     05  'XXX'-BU-KT-KZN                   PIC  X(001).
>>    *            Kennz. BU kombiniert mit Krankentage.
           05  'XXX'-BU-KT-KZN-KZ                PIC  X(001).
      *            Kennz. BU kombiniert mit Krankentage geändert
           05  'XXX'-VITALITY-KZN                PIC  9(003).
      *            Kennz. BU kombiniert mit Vitality.
    <<     05  'XXX'-VITALITY-KZN-KZ             PIC  X(001).
170803*            Kennz. BU kombiniert mit Vtality geändert..
           05  'XXX'-NEUABSCHLKZN                PIC  X(001).
      *            Kennzeichen für Neuabschluss
           05  'XXX'-NEUABSCHLKZN-KZ             PIC  X(001).
      *            Kennzeichen Neuabschluss geändert
           05  'XXX'-BASISZINS-BETR-ALPHA        PIC  X(014).
           05  'XXX'-BASISZINS-BETR REDEFINES 'XXX'-BASISZINS-BETR-ALPHA
                                                 PIC  9(012)V99.
      *            Betrag zum Basiszins
           05  'XXX'-BASISZINS-BETR-KZ           PIC  X(001).
      *            Kennzeichen Betrag zum Basiszins geändert
           05  'XXX'-ZINSBOOST-SATZ-ALPHA        PIC  X(008).
           05  'XXX'-ZINSBOOST-SATZ REDEFINES 'XXX'-ZINSBOOST-SATZ-ALPHA
                                                 PIC  9(005)V999.
      *            Zinssatz des Boosters
           05  'XXX'-ZINSBOOST-SATZ-KZ           PIC  X(001).
      *            Kennzeichen Zinssatz des Boosters geändert
           05  'XXX'-ZINSBOOST-BETR-ALPHA        PIC  X(014).
           05  'XXX'-ZINSBOOST-BETR REDEFINES 'XXX'-ZINSBOOST-BETR-ALPHA
                                                 PIC  9(012)V99.
      *            Betrag zum Boosterzins
           05  'XXX'-ZINSBOOST-BETR-KZ           PIC  X(001).
      *            Kennzeichen Betrag zum Boosterzins geändert
           05  'XXX'-EU-KLAUSEL-KZN              PIC  X(001).
      *            Kennzeichen für Neuabschluss
           05  'XXX'-EU-KLAUSEL-KZN-KZ           PIC  X(001).
      *            Kennzeichen Neuabschluss geändert
           05  'XXX'-SCHUELER-BU                 PIC  X(001).
      *            Kennzeichen für Schüler-BU
           05  'XXX'-SCHUELER-BU-KZ              PIC  X(001).
      *            Kennzeichen Schüler-BU geändert
           05  'XXX'-BTG-AG                      PIC  9(011)V99.
      *            Tarifbeitrag Arbeitgeber-Anteil
           05  'XXX'-BTG-AG-KZ                   PIC  X(001).
      *            Kennzeichen Tarifbeitrag Arbeitgeber-Anteil geändert
           05  'XXX'-BTG-AN                      PIC  9(011)V99.
      *            Tarifbeitrag Arbeitnehmer-Anteil
           05  'XXX'-BTG-AN-KZ                   PIC  X(001).
      *            Kennzeichen Tarifbeitrag Arbeitnehmer-Anteil geändert
           05  'XXX'-KLB-ERH-AUSSCHL             PIC  X(001).
      *            KLB-Erhöhungsausschluss
           05  'XXX'-KLB-ERH-AUSSCHL-KZ          PIC  X(001).
      *            Kennzeichen KLB-Erhöhungsausschluss geändert
           05  'XXX'-WERTSICHOPTKZN              PIC  X(001).
      *            Wertsicherungsoptionskennzeichen
           05  'XXX'-WERTSICHOPTKZN-KZ           PIC  X(001).
      *            Kennzeichen Wertsicherungsoptionskennzeichen geändert
           05  FILLER                            PIC  X(030).
      *            Reserve
 
           05  'XXX'-AKTUAL-KZ                   PIC  X(001).
      ******************************************************************

