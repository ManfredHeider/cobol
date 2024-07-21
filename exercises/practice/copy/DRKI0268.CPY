      *----------------------------------------------------------------*
      *                                                                *
      *    COPY-MEMBER FÜR KFZ-DATEN AM-VERS VERÄNDERUNGSBESTAND       *
      *    -----------------------------------------------------       *
      *                                                                *
      *    DATEITYP  = QSAM                                            *
      *    SATZLÄNGE   1465                                            *
030306*    Satzlänge    765                                           *
AKI553*    Satzlänge   2200                                           *
150908*    Satzlänge   2550                                            *
300910*    Satzlänge   3000
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                          !NAME     !VER-*
      *AUFTRAG !                                        !         !SION*
      *--------!----------------------------------------!--------_!----*
      *21.08.98!NEUERSTELLUNG                           !B.GOLÜCKE! 01 *
      *        !                                        !         !    *
030306*AKI00338!Änderungen, da Beitragsnachlässe        !P. Pohl  !    *
030306*06032003!über eine eigene Datei gehandelt werden !         !    *
030306*        !Tabellen wg. Beitragsnachlass entfallen !         !    *
030306*        !75-stelliger Filler eingefügt           !         !    *
051004*05.10.04!Neues Feld Datum Erstzulassung VN       !W.Morfeld! 02 *
051004*        !Änderung Filler Angabe                  !         !    *
AKI553*AKI00553 Neue Felder f. Bestandsdatenerwei-      !W.Misgeld!    *
      *        !terung Kfz-Daten                        !         !    *
      *        !                                        !         !    *
200807*AAN00753! PIN-Nr. Erweit. auf 9 Stellen          !Hübner   !    *
      *        !                                        !         !    *
150908*AKI00731! Bestandsdatenerw. Kfz-Tarifierung      !W.Misgeld!    *
      *        !                                        !         !    *
260309*AKI00805!VERS-KFZ-VBEDING in 2 Felder aufgeteilt !WMI      !    *
      *        !                                        !         !    *
300910*AKI00995! Dynamische Anzeige 3 neue Felder       !Hübner   !    *
      *        !                                        !         !    *
120711*AKI01041! Erweiterung d. Leistungskennzeichen    !WMI      !    *
      *        !                                        !         !    *
      *AKI01131! Neue Felder f. SFR, Halter, begl.      !BST      !    *
      *        ! Fahren, Umbau Datensatz                !         !    *
      *        !                                        !         !    *
300915*AKI01381! Neue Felder f. Moped Paket-Typ KNR     !HHU      !    *
      *        ! Fahren, Umbau Datensatz                !         !    *
210416*AKI01427! Neue Felder f. NGT 2016                !tfl      !    *
      *        !                                        !         !    *
250816*AKI01465! VORT-Felder entfernen                  !DDY      !    *
      *        !                                        !         !    *
030517*AKI01517! DTA überarbeiten                       !HHU      !    *
      *        !                                        !         !    *
271118*AKI01641! Gesellschaft eingebaut jetzt   G10 G11 !HHU      !    *
      *        !                                        !         !
      *AKI01779! NGT 2020 E-FZG-kz FIRMEN-NR neu        !HHU      !
      ****************************************************************
      * AENDERUNGEN:                                                  
      * AUFTRAG    ! DATUM  ! AENDERUNG                    ! NAME     
      * --------------------------------------------------------------
      * DBO-10064  !30.05.22! neue Felder aufgenommen:     ! T13AC20  
      *            !        !    VERS-KFZ-GEBDAT-VN        !          
      *            !        !    VERS-KFZ-GEBDAT-VN-AENDKZ !          
      *            !        ! alte Felder gelöscht         !          
      *            !        !    VERS-KFZ-FALTERMAX        !          
      *            !        !    VERS-KFZ-FALTERMAX-AENDKZ !          
      * -----------!--------!------------------------------!----------
      *            !        !                              !          
      *            !        !                              !          
      *                                                               
      * ENDE-LOGBUCH                                                  
      ****************************************************************
      *
       01  DRKI0268.
 
           05  VERS-VERTR-ALLGEMEIN.
 
               10  VERS-VALL-VERARB-KZ                 PIC X(01).
      *            Verarbeitungskennzeichen
      *            I = Insert U = Update
 
               10  VERS-VALL-AEND                      PIC 9(02).
               10  VERS-VALL-VERTRNR                   PIC 9(15).
 
200807         10  VERS-VALL-INT-VERTRNR               PIC 9(09).
               10  VERS-VALL-INT-VERTRNR-AENDKZ        PIC X(01).
      *            J = Änderung interne Vertragenummer
 
               10  VERS-VALL-PRUEF                     PIC 9(02).
               10  VERS-VALL-PRUEF-AENDKZ              PIC X(01).
      *            J = Änderung Prüfziffer f. interne Vertragsnummer
 
               10  VERS-VALL-VBEGINN                   PIC 9(08).
               10  VERS-VALL-VBEGINN-AENDKZ            PIC X(01).
      *            J = Änderung Datum Vertragsbeginn
 
               10  VERS-VALL-VABLAUF                   PIC 9(08).
               10  VERS-VALL-VABLAUF-AENDKZ            PIC X(01).
      *            J = Änderung Datum Vertragsablauf
 
               10  VERS-VALL-ZUSTAND                   PIC X(01).
               10  VERS-VALL-ZUSTAND-AENDKZ            PIC X(01).
      *            J = Änderung Zustand
 
               10  VERS-VALL-ZAHLUNGSW                 PIC X(01).
               10  VERS-VALL-ZAHLUNGSW-AENDKZ          PIC X(01).
      *            J = Änderung Zahlungsweise
 
               10  VERS-VALL-BENDBEITRAG               PIC S9(08)V99.
               10  VERS-VALL-BENDBEITRAG-AENDKZ        PIC X(01).
      *            J = Änderung Brutto Endbetrag
 
               10  VERS-VALL-ABSCHLVERM                PIC 9(07).
               10  VERS-VALL-ABSCHLVERM-AENDKZ         PIC X(01).
      *            J = Änderung Abschlussvermittler
 
               10  VERS-VALL-AENDDATUM                 PIC 9(08).
               10  VERS-VALL-AENDDATUM-AENDKZ          PIC X(01).
      *            J = Änderung Vertragsänderungsdatum
 
               10  VERS-VALL-LETZTEAENDD               PIC 9(08).
               10  VERS-VALL-LETZTEAENDD-AENDKZ        PIC X(01).
      *            J = Änderung Datum letzte Änderung
 
               10  VERS-VALL-ZAHLUNGSART               PIC X(01).
               10  VERS-VALL-ZAHLUNGSART-AENDKZ        PIC X(01).
      *            J = Änderung Zahlungsart
 
               10  VERS-VALL-STORNODAT                 PIC 9(08).
               10  VERS-VALL-STORNODAT-AENDKZ          PIC X(01).
      *            J = Änderung Stornodatum
 
               10  VERS-VALL-GEB-VERTR                 PIC X(01).
               10  VERS-VALL-GEB-VERTR-AENDKZ          PIC X(01).
      *            J = Änderung Kennzeichen gebündelter Vertrag
 
250816         10  FILLER                              PIC X(87).
 
               10  VERS-VALL-WAEHRUNG-KZ               PIC X(03).
               10  VERS-VALL-WAEHRUNG-KZ-AENDKZ        PIC X(01).
      *            J = Änderung Währung
               10  VERS-VALL-FIRMEN-NR                 PIC X(06).
               10  VERS-VALL-FIRMEN-NR-AENDKZ         PIC X(01).
      *            J = Änderung FIRMENNUMMER
 
           05  VERS-VERTR-KFZ.
               10  VERS-KFZ-VERARB-KZ                  PIC X(01).
      *            Verarbeitungskennzeichen
      *            I = Insert U = Update
 
               10  VERS-KFZ-ZSCHL                      PIC 9(02).
               10  VERS-KFZ-ZTEXT                      PIC X(03).
      ****             Inkassoart LSV Scoring
               10  VERS-KFZ-LSV-SCOR                   PIC X(001).
               10  VERS-KFZ-LSV-SCOR-AENDKZ            PIC X(001).
      *            J = Änderung LSV-Scorin
               10  VERS-KFZ-AMTKZ                      PIC X(12).
               10  VERS-KFZ-AMTKZ-AENDKZ               PIC X(01).
      *            J = Änderung Amtliches Kennzeichen
               10  VERS-KFZ-AKZART-KNR                 PIC 9(003).
               10  VERS-KFZ-AKZART-KNR-AENDKZ          PIC X(001).
               10  VERS-KFZ-SAISON-KZ                  PIC X(01).
               10  VERS-KFZ-SAISON-KZ-AENDKZ           PIC X(01).
      *            J = Änderung KZ Saison-Kennzeichen J/N
               10  VERS-KFZ-KFZ-SAISON-BEG             PIC 9(02).
               10  VERS-KFZ-KFZ-SAISON-BEG-AENDKZ      PIC X(01).
      *            J = Änderung Saison Kennzeichen Beginnmonat
 
               10  VERS-KFZ-KFZ-SAISON-END             PIC 9(02).
               10  VERS-KFZ-KFZ-SAISON-END-AENDKZ      PIC X(01).
      *            J = Änderung Saison-Kennzeichen Endmonat
               10  VERS-KFZ-ZULASSD                    PIC 9(08).
               10  VERS-KFZ-ZULASSD-AENDKZ             PIC X(01).
      *            J = Änderung Zulassungsdatum
051004         10  VERS-KFZ-EZDATVN                    PIC 9(08).
               10  VERS-KFZ-EZDATVN-AENDKZ             PIC X(01).
      *            J = Änderung datum erstzulassung vn
               10  VERS-KFZ-FZGART-KNR                 PIC 9(003).
               10  VERS-KFZ-FZGART-KNR-AENDKZ          PIC X(001).
               10  VERS-KFZ-VERWEND-KNR                PIC 9(003).
               10  VERS-KFZ-VERWEND-KNR-AENDKZ         PIC X(001).
               10  VERS-KFZ-NAEHSPEZ-KNR               PIC 9(003).
               10  VERS-KFZ-NAEHSPEZ-KNR-AENDKZ        PIC X(001).
               10  VERS-KFZ-WAGNISKZ                   PIC 9(03).
               10  VERS-KFZ-WAGNISKZ-AENDKZ            PIC X(01).
      *            J = Änderung Wagniskennzeichen
               10  VERS-KFZ-KFZ-WKZ-TEXT               PIC X(25).
               10  VERS-KFZ-KFZ-WKZ-TEXT-AENDKZ        PIC X(01).
      *            J = Änderung
               10  VERS-KFZ-HERST-NR                   PIC X(04).
               10  VERS-KFZ-HERST-NR-AENDKZ            PIC X(01).
      *            J = Änderung Herstellerschlüssel
               10  VERS-KFZ-HERSTELLTEXT               PIC X(025).
               10  VERS-KFZ-HERSTELLTEXT-AENDKZ        PIC X(001).
               10  VERS-KFZ-TYP-MOD-TEXT               PIC X(50).
               10  VERS-KFZ-TYP-MOD-TEXT-AENDKZ        PIC X(01).
      *            J = Änderung Fahrzeugmodell Klartext
               10  VERS-KFZ-KZ-KFZ-TYP                 PIC X(03).
               10  VERS-KFZ-KZ-KFZ-TYP-AENDKZ          PIC X(01).
      *            J = Änderung Typenschlüssel
               10  VERS-KFZ-FAHRZ-ID                   PIC X(20).
               10  VERS-KFZ-FAHRZ-ID-AENDKZ            PIC X(01).
      *            J = Änderung Fahrgestellnummer
               10  VERS-KFZ-KZ-LEIST                   PIC X(01).
               10  VERS-KFZ-KZ-LEIST-AENDKZ            PIC X(01).
      *            J = Änderung KZ Leistung
               10  VERS-KFZ-LEISTUNG                   PIC S9(05)V99.
               10  VERS-KFZ-LEISTUNG-AENDKZ            PIC X(01).
      *            J = Änderung Leistung in KW
               10  VERS-KFZ-PLATZ                      PIC S9(003).
               10  VERS-KFZ-PLATZ-AENDKZ               PIC  X(001).
      *            J = Änderung Anzahl Plätze
               10  VERS-KFZ-GESGEW                     PIC S9(007).
               10  VERS-KFZ-GESGEW-AENDKZ              PIC  X(001).
      *            J = Änderung Gesamtgewicht
               10  VERS-KFZ-KW                         PIC S9(004).
               10  VERS-KFZ-KW-AENDKZ                  PIC  X(001).
      *            J = Änderung KW-Leistung
               10  VERS-KFZ-NUTZ                       PIC S9(007).
    <<         10  VERS-KFZ-NUTZ-AENDKZ                PIC  X(001).
120711*            J = Änderung Nutzlast
               10  VERS-KFZ-KFZ-NEUW                   PIC 9(07).
               10  VERS-KFZ-KFZ-NEUW-AENDKZ            PIC X(01).
      *            J = Änderung Gesamtneuwert des Fahrzeugs
               10  VERS-KFZ-JAHR-FAHRLEIST             PIC  9(007).
               10  VERS-KFZ-JAHR-FAHRLEIST-AENDKZ      PIC  X(001).
      *            J = Änderung jährliche Fahrleistung
300910         10  VERS-KFZ-KM-STAND-AKT          PIC S9(7).
               10  VERS-KFZ-KM-STAND-AKT-AENDKZ   PIC  X(001).
      *            J = Änderung KM-Stand aktuell
               10  VERS-KFZ-KM-STAND-AKT-DATUM    PIC 9(8).
               10  VERS-KFZ-KM-STAND-DATUM-AENDKZ PIC  X(001).
      *            J = Änderung KM-Stand aktuell Datum
               10  VERS-KFZ-FAHRER                     PIC  X(010).
               10  VERS-KFZ-FAHRER-AENDKZ              PIC  X(001).
      *            J = Änderung Fahrer
      ****             Geburtsdaten
               10  VERS-KFZ-GEBDAT-FAHRER              PIC  X(010).
               10  VERS-KFZ-GEBDAT-FAHRER-AENDKZ       PIC  X(001).
      *            J = Änderung Geburtsdatum Fahrer
      *        10  VERS-KFZ-GEBDAT-FAHRERW             PIC  X(010).
      *        10  VERS-KFZ-GEBDAT-FAHRERW-AENDKZ      PIC  X(001).
      *            J = Änderung Geburtsdatum Fahrerin
               10  VERS-KFZ-ALTERSGRP-KNR              PIC S9(004).
               10  VERS-KFZ-ALTERSGRP-KNR-AENDKZ       PIC  X(001).
               10  VERS-KFZ-BEGL-FAHRER                PIC S9(002).
               10  VERS-KFZ-BEGL-FAHRER-AENDKZ         PIC  X(001).
               10  VERS-KFZ-BEGL-FAHRER-KT             PIC  X(010).
               10  VERS-KFZ-BEGL-FAHRER-KT-AENDKZ      PIC  X(001).
               10  VERS-KFZ-FZG-HALTER                 PIC S9(002).
               10  VERS-KFZ-FZG-HALTER-AENDKZ          PIC  X(001).
               10  VERS-KFZ-FZG-HALTER-KTX             PIC  X(010).
               10  VERS-KFZ-FZG-HALTER-KTX-AENDKZ      PIC  X(001).
      ****             abweichender Halter-Name
               10  VERS-KFZ-HALT-NNAME                 PIC  X(040).
               10  VERS-KFZ-HALT-NNAME-AENDKZ          PIC  X(001).
      *            J = Änderung Halter-Nachname
               10  VERS-KFZ-HALT-VNAME                 PIC  X(040).
               10  VERS-KFZ-HALT-VNAME-AENDKZ          PIC  X(001).
      *            J = Änderung Halter-Vorname
      ****             sonstige Vertragsangaben
               10  VERS-KFZ-FZG-NUTZ                   PIC  X(010).
               10  VERS-KFZ-FZG-NUTZ-AENDKZ            PIC  X(001).
      *            J = Änderung Fahrzeug-Nutzung
               10  VERS-KFZ-FZG-ABSTELL                PIC  X(010).
               10  VERS-KFZ-FZG-ABSTELL-AENDKZ         PIC  X(001).
      *            J = Änderung Abstellplatz
               10  VERS-KFZ-AUFBAUART-KNR              PIC S9(004).
               10  VERS-KFZ-AUFBAUART-KNR-AENDKZ       PIC  X(001).
      ****             Kenntexte
               10  VERS-KFZ-FAHRGEBIET                 PIC  X(010).
               10  VERS-KFZ-FAHRGEBIET-AENDKZ          PIC  X(001).
      *            J = Änderung Fahrgebiet
               10  VERS-KFZ-FUHRSCH-ERWERB             PIC  X(010) .
               10  VERS-KFZ-FUHRSCH-ERWERB-AENDKZ      PIC  X(001) .
      *            J = Änderung Führerschein-Erwerb
               10  VERS-KFZ-WOHNEIGENT                 PIC  X(010).
               10  VERS-KFZ-WOHNEIGENT-AENDKZ          PIC  X(001).
      *            J = Änderung Wohneigentum
               10  VERS-KFZ-AG-BRANCHE                 PIC  X(060).
               10  VERS-KFZ-AG-BRANCHE-AENDKZ          PIC  X(001).
      *            J = Änderung Arbeitgeber-Branche
 
               10  VERS-KFZ-SONDERAUSSTATT-TAB.
                   15  VERS-KFZ-SONDERAUSSTATT            OCCURS 20.
                       20 VERS-KFZ-SONTEILNR-KNR       PIC S9(003).
                       20 VERS-KFZ-SONTEIL-KNR-AENDKZ  PIC  X(001).
                       20 VERS-KFZ-SONTEILEWERT        PIC S9(007)V99.
                       20 VERS-KFZ-SONTEILEWER-AENDKZ  PIC  X(001).
               10  VERS-KFZ-TARIFAUSWAHL               PIC  X(001).
               10  VERS-KFZ-TARIFAUSWAHL-AENDKZ        PIC  X(001).
               10  VERS-KFZ-WERKSTBINDKZ               PIC  X(001).
               10  VERS-KFZ-WERKSTBINDKZ-AENDKZ        PIC  X(001).
               10  VERS-KFZ-DA-KH                      PIC 9(02).
               10  VERS-KFZ-DA-KH-AENDKZ               PIC X(01).
      *            J = Änderung Deckungsschlüssel für KFZ-Haftpflicht
               10  VERS-KFZ-RKL-KH                     PIC X(04).
               10  VERS-KFZ-RKL-KH-AENDKZ              PIC X(01).
      *            J = Änderung Haftpflicht Regionalklasse
               10  VERS-KFZ-TYPKL-KH                   PIC 9(02).
               10  VERS-KFZ-TYPKL-KH-AENDKZ            PIC X(01).
      *            J = Änderung Haftpflicht Typenklasse
               10  VERS-KFZ-BEITRAG-ZW-KH              PIC S9(08)V99.
               10  VERS-KFZ-BEITRAG-ZW-KH-AENDKZ       PIC X(01).
      *            J = Änderung Haftpflicht Brutto-Beitrag lt. Zahlweise
               10  VERS-KFZ-DA-KF                      PIC 9(02).
               10  VERS-KFZ-DA-KF-AENDKZ               PIC X(01).
      *            J = Änderung Deckungsschlüssel Fahrzeug
               10  VERS-KFZ-RKL-KF                     PIC X(04).
               10  VERS-KFZ-RKL-KF-AENDKZ              PIC X(01).
      *            J = Änderung Regionalklasse Teil- Vollkasko
               10  VERS-KFZ-TYPKL-KF                   PIC 9(02).
               10  VERS-KFZ-TYPKL-KF-AENDKZ            PIC X(01).
      *            J = Änderung Typklasse Teil- Vollkasko
               10  VERS-KFZ-SB-VK                      PIC S9(08)V99.
               10  VERS-KFZ-SB-VK-AENDKZ               PIC X(01).
      *            J = Änderung Beitrag Selbstbeteiligung Vollkasko
 
               10  VERS-KFZ-SB-TK                      PIC S9(08)V99.
               10  VERS-KFZ-SB-TK-AENDKZ               PIC X(01).
      *            J = Änderung Beitrag Selbstbeteiligung Teilkasko
               10  VERS-KFZ-BEITR-ZW-KF                PIC S9(08)V99.
               10  VERS-KFZ-BEITR-ZW-KF-AENDKZ         PIC X(01).
      *            J = Änderung Teil- Vollkasko Brutto lt. Zahlweise
               10  VERS-KFZ-LEASING                    PIC S9(001).
150908         10  VERS-KFZ-LEASING-AENDKZ             PIC  X(001).
               10  VERS-KFZ-SCHUTZBRIEF                PIC X(10).
               10  VERS-KFZ-SCHUTZBRIEF-AENDKZ         PIC X(01).
      *            J = Änderung Schutzbrief
 
               10  VERS-KFZ-SFR.
120711            15  VERS-KFZ-SFR-FREI-KH             PIC  X(001).
                  15  VERS-KFZ-SFR-FREI-KH-AENDKZ      PIC  X(001).
                  15  VERS-KFZ-SFANZKH                 PIC  9(003).
                  15  VERS-KFZ-SFANZKH-AENDKZ          PIC  X(001).
      *            J = Änderung SFR-FREI-Haftpflicht
                  15  VERS-KFZ-PSFANZKH                PIC S9(002).
                  15  VERS-KFZ-PSFANZKH-AENDKZ         PIC  X(001).
                  15  VERS-KFZ-BEITRAGKH               PIC 9(03).
                  15  VERS-KFZ-BEITRAGKH-AENDKZ        PIC X(01).
      *            J = Änderung Beitragssatz KFZ-Haftpflicht
                  15  VERS-KFZ-BEITRAGKL-KH            PIC X(06).
                  15  VERS-KFZ-BEITRAGKL-KH-AENDKZ     PIC X(01).
      *            J = Änderung Haftpflicht Schadensfreiheitsklasse
                  15  VERS-KFZ-SFR-FREI-VK             PIC  X(001).
                  15  VERS-KFZ-SFR-FREI-VK-AENDKZ      PIC  X(001).
      *            J = Änderung SFR-FREI-Vollkasko
                  15  VERS-KFZ-SFANZVK                 PIC  9(003).
                  15  VERS-KFZ-SFANZVK-AENDKZ          PIC  X(001).
                  15  VERS-KFZ-PSFANZVK                PIC S9(002).
                  15  VERS-KFZ-PSFANZVK-AENDKZ         PIC  X(001).
                  15  VERS-KFZ-BEITRAGKF               PIC 9(03).
                  15  VERS-KFZ-BEITRAGKF-AENDKZ        PIC X(01).
      *            J = Änderung Beitragsatz Fahrzeug
                  15  VERS-KFZ-BKL-VK.
                    20  VERS-KFZ-BKL-VK1               PIC X(03).
                    20  VERS-KFZ-BKL-VK2               PIC X(03).
                  15  VERS-KFZ-BKL-VK-AENDKZ           PIC X(01).
      *            J = Änderung Vollkasko Schadenfreiheitsrabatt
 
      ******** Tabelle Bedingungen (Komplettbestand)
      *
               10  VERS-KFZ-BED-T-ALT-ANZAHL           PIC 9(01).
      *            Anzahl Tabelleneinträge
 
               10  VERS-KFZ-BED-T-ALT-VERARBKZ         PIC X(01).
      *            Dieses Vearbeitungskennzeichen wird hier nur einmal
      *            für die gesamte Tabelle gesetzt, da pro Tabellen-
      *            eintrag nur ein D (Delete) möglich ist. Wie oft ein
      *            Delete erfolgt steht im Feld ANZAHL s.o
 
               10  VERS-KFZ-BED-T-ALT                      OCCURS 3.
260309*            20  VERS-KFZ-VBEDING-ALT            PIC X(21).
260309             20  VERS-KFZ-VBEDING-ALT.
260309                 25  VERS-KFZ-VBEDING-ALT-KEY    PIC X(02).
260309                 25  VERS-KFZ-VBEDING-ALT-TXT    PIC X(19).
      *
      ******** Tabellenende
      *
      ******** Tabelle Bedingungen (Änderungssatz)
      *
               10  VERS-KFZ-BED-T-NEU-ANZAHL           PIC 9(01).
      *            Anzahl Tabelleneinträge
 
               10  VERS-KFZ-BED-T-NEU-VERARBKZ         PIC X(01).
      *            Dieses Vearbeitungskennzeichen wird hier nur einmal
      *            für die gesamte Tabelle gesetzt, da pro Tabellen-
      *            eintrag nur ein I (Insert) möglich ist. Wie oft ein
      *            Insert erfolgt steht im Feld ANZAHL s.o
 
               10  VERS-KFZ-BED-T-NEU                      OCCURS 3.
260309*            20  VERS-KFZ-VBEDING-NEU            PIC X(21).
260309             20  VERS-KFZ-VBEDING-NEU.
260309                 25  VERS-KFZ-VBEDING-NEU-KEY    PIC X(02).
260309                 25  VERS-KFZ-VBEDING-NEU-TXT    PIC X(19).
      *
      ******** Tabellenende
      *
      ******** Tabelle Tarifbezeichnung (Komplettbestand)
      *
               10  VERS-KFZ-TARIFB-T-ALT-ANZAHL       PIC 9(01).
      *            Anzahl Tabelleneinträge
 
               10  VERS-KFZ-TARIFB-T-ALT-VERARBKZ     PIC X(01).
      *            Dieses Vearbeitungskennzeichen wird hier nur einmal
      *            für die gesamte Tabelle gesetzt, da pro Tabellen-
      *            eintrag nur ein D (Delete) möglich ist. Wie oft ein
      *            Delete erfolgt steht im Feld ANZAHL s.o
 
               10  VERS-KFZ-TARIFB-T-ALT                   OCCURS 3.
                   20  VERS-KFZ-TARIFB-ALT             PIC X(07).
      *
      ******** Tabellenende
      *
      ******** Tabelle Tarifbezeichnung (Änderungssatz)
      *
               10  VERS-KFZ-TARIFB-T-NEU-ANZAHL         PIC 9(01).
      *            Anzahl Tabelleneinträge
      *
               10  VERS-KFZ-TARIFB-T-NEU-VERARBKZ       PIC X(01).
      *            Dieses Vearbeitungskennzeichen wird hier nur einmal
      *            für die gesamte Tabelle gesetzt, da pro Tabellen-
      *            eintrag nur ein I (Insert) möglich ist. Wie oft ein
      *            Insert erfolgt steht im Feld ANZAHL s.o
 
               10  VERS-KFZ-TARIFB-T-NEU                   OCCURS 3.
                   20  VERS-KFZ-TARIFB-NEU             PIC X(07).
      *
      ******** Tabellenende
 
      ******** Tabelle Tarifbestimmung (Komplettbestand)
      *
               10  VERS-KFZ-KSMVERTRAG-TAB.
                   15  VERS-KFZ-KSMVERTRAG         OCCURS 4.
                       20  VERS-KFZ-KSMVERTRAG-DAT.
                           25  VERS-KFZ-TAR-BST-SPART      PIC  X(006).
                           25  VERS-KFZ-TAR-BST-KTX        PIC  X(010).
                           25  VERS-KFZ-TAR-BST-TXT        PIC  X(050).
                           25  VERS-KFZ-TAR-BST-GDVKZ      PIC  X(050).
                       20  VERS-KFZ-KSMVERT-AENDKZ         PIC  X(001).
      *            J = Änderung Tarifbestimmung
 
      ******** Tabellenende
 
      ******** Tabelle besond. Vereinbarung (Komplettbestand)
      *
               10  VERS-KFZ-BES-VEREINBARUNG-TAB.
                   15  VERS-KFZ-BES-VEREINBARUNG   OCCURS 15.
                       20  VERS-KFZ-BESVEREINB         PIC  X(050).
                       20  VERS-KFZ-BESVEREI-AENDKZ    PIC  X(001).
      *            J = Änderung besondere Vereinbarung
 
300915         10  VERS-KFZ-MOPED-TYP-KNR              PIC  9(004).
300915         10  VERS-KFZ-MOPED-TYP-KNR-AENDKZ       PIC  X(001).
210416         10  VERS-KFZ-TELEMATIK-ID               PIC  9(014).
210416         10  VERS-KFZ-TELEMATIK-ID-AENDKZ        PIC  X(001).
210416         10  VERS-KFZ-SCOREWERT                  PIC  9(003).
210416         10  VERS-KFZ-SCOREWERT-AENDKZ           PIC  X(001).
210416         10  VERS-KFZ-SCORE-DATUM                PIC  9(008).
210416         10  VERS-KFZ-SCORE-DATUM-AENDKZ         PIC  X(001).
210416         10  VERS-KFZ-FALTERMAX-KNR              PIC  9(002).
210416         10  VERS-KFZ-FALTERMAX-KNR-AENDKZ       PIC  X(001).
               10  VERS-KFZ-E-FZG-KZ                   PIC  X(01).
               10  VERS-KFZ-E-FZG-KZ-AENDKZ            PIC  X(01).
               10  VERS-KFZ-GEBDAT-VN                  PIC  X(010).
               10  VERS-KFZ-GEBDAT-VN-AENDKZ           PIC  X(001).
               10  FILLER                              PIC  X(410).
271118         10  VERS-KFZ-GES                        PIC  99.
 
      ******************************************************************
      *          E n d e    Copy-Member DRKI0268                       *
      ******************************************************************