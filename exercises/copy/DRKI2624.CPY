      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  20.06.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2624.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2624.CPv  $
      *
      *   Rev 2.1   Aug 13 2012 08:53:24   t13ex15
      *AKI01141: DTA-Erweiterung Fondsdepot Bank "WpHG" Programme u
      *nd Copy
      *
      *   Rev 2.0   Jun 20 2012 07:48:54   t13ex15
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *                                                                *
      *    COPY-MEMBER zum Ermittlungsprogramm                         *
      *       WpHG(Bogen-)Daten FodB bzw. DeuBa                        *
      *       mit einem Feld fuer das Aenderungskennzeichen je Attribut*
      *                                                                *
      *    LOGBUCH                                                     *
      *--------!-------------------------------------------!-----------*
      *AUFTRAG !Datum   !Text                              !Name       *
      *--------!-------------------------------------------!-----------*
      *AKI01141!21.06.12!1.0 ! Neu                         !SWZ        *
      *        !        !    !                             !           *
      *        !        !    !                             !           *
      * ENDE-LOGBUCH                                                   *
      ******************************************************************
      *
       01  'XX'-SATZ.
      *
               15  'XX'-VERARB-KNZ          PIC X(001).
      *        Verarbeitungskennzeichen
      *        I = Insert U = Update D = Delete

               15  'XX'-SA                          PIC  X(001).
      *            SATZART "C"
               15  'XX'-GES                         PIC  X(002).
      *            GESELLSCHAFT
               15  'XX'-KEY.
                   20  'XX'-VNR.
                      25 'XX'-KONTONUMMER           PIC  9(013).
      *
               15  'XX'-BERUF                       PIC  X(001).
      *        A = Kaufm. Angestellte in einem Finanzdienst.l.unt.
      *        B = Mitglied der Geschäftsleitung
      *        C = Handwerker
      *        D = Sonstige kaufm. Angestellte
      *        E = Wirtschaftsprüfer/Steuerberater
      *        F = Andere Berufe
      *        G = Technische Angestellte
      *        H = Andere Freiberufler
               15  'XX'-BERUF-UPD                           PIC  X(001).
      *        'J' = Aenderung
      *
               15  'XX'-GEWUENSCHT-ANLAGEZIEL               PIC  X(001).
      *        1 = Sicherheit
      *        2 = Einkommen
      *        3 = Balance
      *        4 = Wachstum
      *        5 = Dynamik
               15  'XX'-GEWUENSCHT-ANLAGEZIEL-UPD           PIC  X(001).
      *        'J' = Aenderung
      *
               15  'XX'-GEWUENSCHT-ANLAGEDAUER              PIC  X(001).
      *        1 = bis 4 Jahre
      *        2 = 4 bis 6 Jahre
      *        3 = 6 bis 10 Jahre
      *        4 = Mehr als 10 Jahre
               15  'XX'-GEWUENSCHT-ANLAGEDAUER-UPD          PIC  X(001).
      *        'J' = Aenderung
      *
               15  'XX'-ANZ-WP-KAEUFE-VERG          PIC  X(001).
      *        1 = 0 bis 2
      *        2 = 3 bis 5
      *        3 = 6 bis 10
      *        4 = Mehr als 10
               15  'XX'-ANZ-WP-KAEUFE-VERG-UPD      PIC  X(001).
      *        'J' = Aenderung
      *
               15  'XX'-DURCHSCHN-ORDERGR           PIC  X(001).
      *        1 = 0 bis 4.999 Euro
      *        2 = 5.000 bis 24.999 Euro
      *        3 = 25.000 bis 49.999 Euro
      *        4 = 50.000 Euro und mehr
               15  'XX'-DURCHSCHN-ORDERGR-UPD       PIC  X(001).
      *        'J' = Aenderung
      *
               15  'XX'-GESCH-AUF-KREDITBASIS       PIC  X(001).
      *        Geschäfte auf Kreditbasis gem. WpHG-Bogen
      *        J/N
               15  'XX'-GESCH-AUF-KREDITBASIS-UPD       PIC  X(001).
      *        'J' = Aenderung
      *
               15  'XX'-HOEHE-KREDITRAHMEN          PIC  S9(006)V99.
      *        Höhe des Kreditrahmens gem. WpHG-Bogen
               15  'XX'-HOEHE-KREDITRAHMEN-UPD       PIC  X(001).
      *        'J' = Aenderung

      *        -------------------------------------------------------
      *        In Anspruch genommene Wertpapierdienstleistungen
      *        -------------------------------------------------------
               15  'XX'-WP-DIENST.
      *        In Anspruch genommene Wertpapierdienstleistungen
      *
                  20  'XX'-WP-DIENST-BERATUNGSFREI     PIC  X(001).
      *           "Beratungsfreie Anlagegeschäfte"
                  20  'XX'-WP-DIENST-BERFREI-UPD PIC  X(001).
      *           'J' = Aenderung
      *
                  20  'XX'-WP-DIENST-ANLAGEBERART      PIC  X(001).
      *           "Anlageberatung"
                  20  'XX'-WP-DIENST-ANLAGEBERART-UPD PIC  X(001).
      *           'J' = Aenderung
      *
                  20  'XX'-WP-DIENST-VERM-VERWALT      PIC  X(001).
      *           "Vermögensverwaltung"
                  20  'XX'-WP-DIENST-VERM-VERWALT-UPD PIC  X(001).
      *           'J' = Aenderung
      *
      *        -------------------------------------------------------
      *        Anlagestrategie gem. WpHG-Bogen
      *        -------------------------------------------------------
               15  'XX'-ANL-STRAT.
      *        Anlagestrategie gem. WpHG-Bogen
      *
                  20  'XX'-ANL-STRAT-PRIV-VERM-AUFB      PIC  X(001).
      *           Privater Vermögensaufbau
                  20  'XX'-ANL-STRAT-P-VERM-AUFB-UPD PIC  X(001).
      *           'J' = Aenderung
      *
                  20  'XX'-ANL-STRAT-RENT-VORS           PIC  X(001).
      *           Rentenvorsorge
                  20  'XX'-ANL-STRAT-RENT-VORS-UPD       PIC  X(001).
      *           'J' = Aenderung
      *
                  20  'XX'-ANL-STRAT-GESRI-VORS          PIC  X(001).
      *           Gesundheits-und Risikovorsorge
                  20  'XX'-ANL-STRAT-GESRI-VORS-UPD       PIC  X(001).
      *           'J' = Aenderung
      *
                  20  'XX'-ANL-STRAT-WOHN-EIGENT         PIC  X(001).
      *           Wohneigentum
                  20  'XX'-ANL-STRAT-WOHN-EIGENT-UPD       PIC  X(001).
      *           'J' = Aenderung
      *
                  20  'XX'-ANL-STRAT-KEIN-ZIEL            PIC  X(001).
      *           Kein konkretes Ziel
                  20  'XX'-ANL-STRAT-KEIN-ZIEL-UPD       PIC  X(001).
      *           'J' = Aenderung

      *        -------------------------------------------------------
      *        Finanzielle Situation gem. WpHG-Bogen
      *        -------------------------------------------------------
      *
               15  'XX'-FIN-SIT.
      *        Finanzielle Situation gem. WpHG-Bogen
      *
                  20  'XX'-FIN-SIT-V.
      *           ----------------------------------------------------
      *           Vermögen
      *           ----------------------------------------------------
      *
                    25  'XX'-FIN-SIT-V-KTO-GUTH          PIC  X(001).
      *             Vermögen > Kontoguthaben
      *             1 = 0 bis 10 T€
      *             2 = 10 bis 50 T€
      *             3 = > 50 T
                    25  'XX'-FIN-SIT-V-KTO-GUTH-UPD       PIC  X(001).
      *             'J' = Aenderung
      *
                    25  'XX'-FIN-SIT-V-WP-VERM           PIC  X(001).
      *             Vermögen > Wertpapiervermögen
      *             1 = 0 bis 10 T€
      *             2 = 10 bis 50 T€
      *             3 = > 50 T
                    25  'XX'-FIN-SIT-V-WP-VERM-UPD       PIC  X(001).
      *             'J' = Aenderung
      *
                    25  'XX'-FIN-SIT-V-IMMO-VERM         PIC  X(001).
      *             Vermögen > Immobilienvermögen
      *             1 = 0 bis 100 T€
      *             2 = 100 bis 500 T€
      *             3 = > 500 T€
                    25  'XX'-FIN-SIT-V-IMMO-VERM-UPD       PIC  X(001).
      *             'J' = Aenderung
      *
                    25  'XX'-FIN-SIT-V-SONST-VERM        PIC  X(001).
      *             Vermögen > Sonstiges Vermögen
      *             1 = 0 bis 10 T€
      *             2 = 10 bis 50 T€
      *             3 = > 50 T
                    25  'XX'-FIN-SIT-V-SONST-VERM-UPD       PIC  X(001).
      *             'J' = Aenderung
      *
                  20  'XX'-FIN-SIT-VERBINDLICHK          PIC  X(001).
      *           Verbindlichkeiten
      *           1 = 0 bis 10 T€
      *           2 = 10 bis 50 T€
      *           3 = > 50 T
                  20  'XX'-FIN-SIT-VERBINDLICHK-UPD       PIC  X(001).
      *           'J' = Aenderung
      *
                  20  'XX'-FIN-SIT-NETTOVERM             PIC  X(001).
      *           Nettovermögen
      *           1 = < 0
      *           2 = 0 bis 50 T€
      *           3 = > 50 T€
                  20  'XX'-FIN-SIT-NETTOVERM-UPD       PIC  X(001).
      *           'J' = Aenderung
      *
                  20  'XX'-FIN-SIT-MTL-EINK.
      *           ----------------------------------------------------
      *           Mtl. Einkünfte
      *           ----------------------------------------------------
      *
                    25  'XX'-FIN-SIT-M-NICH-SELBST       PIC  X(001).
      *             Mtl. Einkünfte > Aus nichtselbstständiger Arbeit
      *             1 = 0 bis 2 T€
      *             2 = 2 bis 5 T€
      *             3 = > 5 T€
                    25  'XX'-FIN-SIT-M-NICH-SELBST-UPD     PIC  X(001).
      *             'J' = Aenderung
      *
                    25  'XX'-FIN-SIT-M-SELBST            PIC  X(001).
      *             Mtl. Einkünfte > Aus selbstständiger Arbeit
      *             1 = 0 bis 2 T€
      *             2 = 2 bis 5 T€
      *             3 = > 5 T€
                    25  'XX'-FIN-SIT-M-SELBST-UPD     PIC  X(001).
      *             'J' = Aenderung
      *
                    25  'XX'-FIN-SIT-M-KAP-VERM          PIC  X(001).
      *             Mtl. Einkünfte > Kapitalvermögen
      *             1 = 0 bis 2 T€
      *             2 = 2 bis 5 T€
      *             3 = > 5 T€
                    25  'XX'-FIN-SIT-M-KAP-VERM-UPD     PIC  X(001).
      *             'J' = Aenderung
      *
                    25  'XX'-FIN-SIT-M-WEITERE           PIC  X(001).
      *             Mtl. Einkünfte > Weitere
      *             1 = 0 bis 2 T€
      *             2 = 2 bis 5 T€
      *             3 = > 5 T€
                    25  'XX'-FIN-SIT-M-WEITERE-UPD     PIC  X(001).
      *             'J' = Aenderung
      *
                  20  'XX'-FIN-SIT-M-AUSGABEN            PIC  X(001).
      *           Mtl. Ausgaben
      *           1 = 0 bis 2 T€
      *           2 = 2 bis 5 T€
      *           3 = > 5 T€
                  20  'XX'-FIN-SIT-M-AUSGABEN-UPD     PIC  X(001).
      *           'J' = Aenderung
      *
                  20  'XX'-FIN-SIT-FREIE-M-LIQ           PIC  X(001).
      *           Freie mtl. Liquidität
      *           1 = 0 bis 2 T€
      *           2 = 2 bis 5 T€
      *           3 = > 5 T€
                  20  'XX'-FIN-SIT-FREIE-M-LIQ-UPD     PIC  X(001).
      *           'J' = Aenderung

      *        -------------------------------------------------------
      *        Kenntnisse und Erfahrungen gem. WpHG-Bogen
      *        -------------------------------------------------------
      *
               15  'XX'-KERF.
      *        Kenntnisse und Erfahrungen gem. WpHG-Bogen
      *
                  20  'XX'-KERF-R1.
      *           ----------------------------------------------------
      *           Risikoklasse 1 >
      *           ----------------------------------------------------
      *
                    25  'XX'-KERF-R1-RENTENFONDS         PIC  X(001).
      *             Risikoklasse 1 > Rentenfonds
      *             J/N
                    25  'XX'-KERF-R1-RENTENFONDS-UPD     PIC  X(001).
      *             'J' = Aenderung
      *
                    25  'XX'-KERF-R1-DAUER               PIC  X(001).
      *             Risikoklasse 1 > Dauer
      *             1 = Keine bis 1 Jahr
      *             2 = Über 1 bis 3 Jahre
      *             3 = Mehr als 3 Jahre
                    25  'XX'-KERF-R1-DAUER-UPD     PIC  X(001).
      *             'J' = Aenderung

                  20  'XX'-KERF-R2.
      *           ----------------------------------------------------
      *           Risikoklasse 2 >
      *           ----------------------------------------------------
      *
                    25  'XX'-KERF-R2-RENTENFONDS         PIC  X(001).
      *             Risikoklasse 2 > Rentenfonds
      *             J/N
                    25  'XX'-KERF-R2-RENTENFONDS-UPD     PIC  X(001).
      *             'J' = Aenderung
      *
                    25  'XX'-KERF-R2-STRUKT-WP           PIC  X(001).
      *             Risikoklasse 2 > Srukturierte Wertpapiere
      *             J/N
                    25  'XX'-KERF-R2-STRUKT-WP-UPD     PIC  X(001).
      *             'J' = Aenderung
      *
                    25  'XX'-KERF-R2-DAUER               PIC  X(001).
      *             Risikoklasse 2 > Dauer
      *             1 = Keine bis 1 Jahr
      *             2 = Über 1 bis 3 Jahre
      *             3 = Mehr als 3 Jahre
                    25  'XX'-KERF-R2-DAUER-UPD     PIC  X(001).
      *             'J' = Aenderung

                  20  'XX'-KERF-R3.
      *           ----------------------------------------------------
      *           Risikoklasse 3 >
      *           ----------------------------------------------------
      *
                    25  'XX'-KERF-R3-RENTENFONDS         PIC  X(001).
      *             Risikoklasse 3 > Rentenfonds
      *             J/N
                    25  'XX'-KERF-R3-RENTENFONDS-UPD     PIC  X(001).
      *             'J' = Aenderung
      *
                    25  'XX'-KERF-R3-AKTIENFONDS         PIC  X(001).
      *             Risikoklasse 3 > Aktienfonds
      *             J/N
                    25  'XX'-KERF-R3-AKTIENFONDS-UPD     PIC  X(001).
      *             'J' = Aenderung
      *
                    25  'XX'-KERF-R3-STRUKT-WP           PIC  X(001).
      *             Risikoklasse 3 > Srukturierte Wertpapiere
      *             J/N
                    25  'XX'-KERF-R3-STRUKT-WP-UPD     PIC  X(001).
      *             'J' = Aenderung
      *
                    25  'XX'-KERF-R3-ALT-INV             PIC  X(001).
      *             Risikoklasse 3 > Alternative Investments
      *             J/N
                    25  'XX'-KERF-R3-ALT-INV-UPD     PIC  X(001).
      *             'J' = Aenderung
      *
                    25  'XX'-KERF-R3-DAUER               PIC  X(001).
      *             Risikoklasse 3 > Dauer
      *             1 = Keine bis 1 Jahr
      *             2 = Über 1 bis 3 Jahre
      *             3 = Mehr als 3 Jahre
                    25  'XX'-KERF-R3-DAUER-UPD     PIC  X(001).
      *             'J' = Aenderung

                  20  'XX'-KERF-R4.
      *           ----------------------------------------------------
      *           Risikoklasse 4 >
      *           ----------------------------------------------------
      *
                    25  'XX'-KERF-R4-RENTENFONDS         PIC  X(001).
      *             Risikoklasse 4 > Rentenfonds
      *             J/N
                    25  'XX'-KERF-R4-RENTENFONDS-UPD     PIC  X(001).
      *             'J' = Aenderung
      *
                    25  'XX'-KERF-R4-AKTIENFONDS         PIC  X(001).
      *             Risikoklasse 4 > Aktienfonds
      *             J/N
                    25  'XX'-KERF-R4-AKTIENFONDS-UPD     PIC  X(001).
      *             'J' = Aenderung
      *
                    25  'XX'-KERF-R4-STRUKT-WP           PIC  X(001).
      *             Risikoklasse 4 > Srukturierte Wertpapiere
      *             J/N
                    25  'XX'-KERF-R4-STRUKT-WP-UPD     PIC  X(001).
      *             'J' = Aenderung
      *
                    25  'XX'-KERF-R4-ALT-INV             PIC  X(001).
      *             Risikoklasse 4 > Alternative Investments
      *             J/N
                    25  'XX'-KERF-R4-ALT-INV-UPD     PIC  X(001).
      *             'J' = Aenderung
      *
                    25  'XX'-KERF-R4-DAUER               PIC  X(001).
      *             Risikoklasse 4 > Dauer
      *             1 = Keine bis 1 Jahr
      *             2 = Über 1 bis 3 Jahre
      *             3 = Mehr als 3 Jahre
                    25  'XX'-KERF-R4-DAUER-UPD     PIC  X(001).
      *             'J' = Aenderung

                  20  'XX'-KERF-R5.
      *           ----------------------------------------------------
      *           Risikoklasse 5 >
      *           ----------------------------------------------------
      *
                    25  'XX'-KERF-R5-RENTENFONDS         PIC  X(001).
      *             Risikoklasse 5 > Rentenfonds
      *             J/N
                    25  'XX'-KERF-R5-RENTENFONDS-UPD     PIC  X(001).
      *             'J' = Aenderung
      *
                    25  'XX'-KERF-R5-AKTIENFONDS         PIC  X(001).
      *             Risikoklasse 5 > Aktienfonds
      *             J/N
                    25  'XX'-KERF-R5-AKTIENFONDS-UPD     PIC  X(001).
      *             'J' = Aenderung
      *
                    25  'XX'-KERF-R5-STRUKT-WP           PIC  X(001).
      *             Risikoklasse 5 > Srukturierte Wertpapiere
      *             J/N
                    25  'XX'-KERF-R5-STRUKT-WP-UPD     PIC  X(001).
      *             'J' = Aenderung
      *
                    25  'XX'-KERF-R5-AI-TERMIN           PIC  X(001).
      *             Risikoklasse 5 > AI und Termingeschäfte
      *             J/N
                    25  'XX'-KERF-R5-AI-TERMIN-UPD     PIC  X(001).
      *             'J' = Aenderung
      *
                    25  'XX'-KERF-R5-DAUER               PIC  X(001).
      *             Risikoklasse 5 > Dauer
      *             1 = Keine bis 1 Jahr
      *             2 = Über 1 bis 3 Jahre
      *             3 = Mehr als 3 Jahre
                    25  'XX'-KERF-R5-DAUER-UPD     PIC  X(001).
      *             'J' = Aenderung

      *        -------------------------------------------------------
      *        Weitere Angaben
      *        -------------------------------------------------------
      *
               15  'XX'-GESPRAECHS-PARTNER            PIC  X(001).
      *        Gesprächspartner  gem. WpHG-Bogen war
      *        1 = Kontoinhaber
      *        2 = Gesetzlicher Vertreter
      *        3 = Bevollmächtigter
      *        4 = Interessent
               15  'XX'-GESPRAECHS-PARTNER-UPD     PIC  X(001).
      *        'J' = Aenderung
      *
               15  'XX'-DATUM-UNTERSCHR               PIC  9(008).
      *        Datum gem. WpHG-Bogens (Unterschriftenfeld)
      *        Zur Befüllung des Beratungsprotokoll
               15  'XX'-DATUM-UNTERSCHR-UPD     PIC  X(001).
      *        'J' = Aenderung
      *
170712*        15  'XX'-DATUM-ERFASSUNG               PIC  9(008).
      *        Erfassungsdatum des WpHG-Bogen beim Produktpartner
      *        Zur Darstellung in den Vertragsdaten
      *        15  'XX'-DATUM-ERFASSUNG-UPD     PIC  X(001).

170712         15  'XX'-KEINE-IN-ANPR-GEN-LEIST       PIC  X(001).
      *        In Anspruch genommene Wertpapierdienstleistungen
      *        gem. WpHG-Bogen war "Keine"
               15  'XX'-KEINE-IN-A-G-LEIST-UPD  PIC  X(001).


               15  'XX'-RESERVE                     PIC  X(169).
      *

