      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  09.08.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2657.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/DRKI2657.CPv  $
      *
      *   Rev 2.6   Oct 11 2017 11:18:56   t13ex78
      *AKI01532: Programme & Copies und TWS 
      *
      *   Rev 2.4   Mar 27 2017 09:30:08   t13ae31
      *FKI01544: FVD Teil 1 alten Code entfernen COPY
      *
      *   Rev 2.3   May 06 2013 09:55:36   t13ex20
      *FKI01424: G48 WPHG-DeuBa: Bereinigung Datumsabweichungen KSD
      *S/DB
      *
      *   Rev 2.2   Aug 30 2012 11:10:16   t13ex20
      *AKI01140: DTA-Erweiterung Deutsche Bank "WpHG" Programme und
      * Copys
      *
      *   Rev 2.1   Aug 28 2012 15:25:34   t13ex20
      *AKI01140: DTA-Erweiterung Deutsche Bank "WpHG" Programme und
      * Copys
      *
      *   Rev 2.0   Aug 09 2012 14:46:16   T13EX20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *                                                                *
      *    COPY-MEMBER für die Ausgabedatei Änderungsbestand           *
      *         Ermittlung WpHG(Bogen-)Daten DeuBa mit Änderungs-KZ
      *
      *                                                                *
      *    SATZLÄNGE 400                                               *
      *                                                                *
      *    LOGBUCH                                                     *
      *--------!-------------------------------------------!-----------*
      *AUFTRAG !Datum   !Text                              !Name       *
      *--------!-------------------------------------------!-----------*
      *AKI01140!09.08.12!1.0 ! Neu                         !UZI        *
      *--------!-------------------------------------------!-----------*
      *FKI01544!24.03.17!    ! FVD entfernt                !HHU        *
      *--------!-------------------------------------------!-----------*
      *AKI01532!11.09.17!    ! Neue Datenfelder MiFID2     !tfl        *
      *        !        !    ! Satzlänge von 300 auf 400   !           *
      *        !        !    !                             !           *
      * ENDE-LOGBUCH                                                   *
      ******************************************************************
      *
       01  'XX'-SATZ.
      *
      ******************************************************************
      *                                                                *
 6    *         HAUPTSATZ                                              *
      *                                                                *
      ******************************************************************
      *
           10  'XX'-AENDERUNG-SATZ.
      *
               15  'XX'-VERARB-KZ                   PIC X(001).
      *        Verarbeitungskennzeichen
      *        I = Insert U = Update D = Delete
 
               15  'XX'-SA                          PIC  X(002).
      *            SATZART "21"
               15  'XX'-GES                         PIC  X(002).
      *            GESELLSCHAFT = '48'
               15  'XX'-KEY.
                   20  'XX'-VNR.
                      25 'XX'-VERTRAGSNUMMER        PIC  9(010).
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
               15  'XX'-BERUF-UPD                   PIC  X(001).
      *        'J' = Aenderung*
 
               15  'XX'-GEWUENSCHT-ANLAGEZIEL       PIC  X(001).
      *        1 = Sicherheit
      *        2 = Einkommen
      *        3 = Balance
      *        4 = Wachstum
      *        5 = Dynamik
               15  'XX'-GEWUENSCHT-ANLAGEZIEL-UPD   PIC  X(001).
      *
               15  'XX'-GEWUENSCHT-ANLAGEDAUER      PIC  X(001).
      *        1 = bis 4 Jahre
      *        2 = 4 bis 6 Jahre
      *        3 = 6 bis 10 Jahre
      *        4 = Mehr als 10 Jahre
               15  'XX'-GEWUENSCHT-ANLAGEDAUER-UPD  PIC  X(001).
      *
               15  'XX'-ANZ-WP-KAEUFE-VERG          PIC  X(001).
      *        1 = 0 bis 2
      *        2 = 3 bis 5
      *        3 = 6 bis 10
      *        4 = Mehr als 10
               15  'XX'-ANZ-WP-KAEUFE-VERG-UPD      PIC  X(001).
      *
               15  'XX'-DURCHSCHN-ORDERGR           PIC  X(001).
      *        1 = 0 bis 4.999 Euro
      *        2 = 5.000 bis 24.999 Euro
      *        3 = 25.000 bis 49.999 Euro
      *        4 = 50.000 Euro und mehr
               15  'XX'-DURCHSCHN-ORDERGR-UPD       PIC  X(001).
      *
               15  'XX'-GESCH-AUF-KREDITBASIS       PIC  X(001).
      *        Geschäfte auf Kreditbasis gem. WpHG-Bogen
      *        J/N
               15  'XX'-GESCH-AUF-KREDITBASIS-UPD   PIC  X(001).
      *
               15  'XX'-HOEHE-KREDITRAHMEN          PIC  S9(006)V99.
      *        Höhe des Kreditrahmens gem. WpHG-Bogen
               15  'XX'-HOEHE-KREDITRAHMEN-UPD      PIC  x(001).
 
      *        -------------------------------------------------------
      *        In Anspruch genommene Wertpapierdienstleistungen
      *        -------------------------------------------------------
               15  'XX'-WP-DIENST.
      *        In Anspruch genommene Wertpapierdienstleistungen
      *
                  20  'XX'-WP-DIENST-BERATUNGSFREI     PIC  X(001).
      *           "Beratungsfreie Anlagegeschäfte"
                  20  'XX'-WP-DIENST-BERFREI-UPD       PIC  X(001).
      *
                  20  'XX'-WP-DIENST-ANLAGEBERART      PIC  X(001).
      *           "Anlageberatung"
                  20  'XX'-WP-DIENST-ANLAGEBERART-UPD  PIC  X(001).
      *
                  20  'XX'-WP-DIENST-VERM-VERWALT      PIC  X(001).
      *           "Vermögensverwaltung"
                  20  'XX'-WP-DIENST-VERM-VERWALT-UPD  PIC  X(001).
      *
      *        -------------------------------------------------------
      *        Anlagestrategie gem. WpHG-Bogen
      *        -------------------------------------------------------
               15  'XX'-ANL-STRAT.
      *        Anlagestrategie gem. WpHG-Bogen
      *
                  20  'XX'-ANL-STRAT-PRIV-VERM-AUFB      PIC  X(001).
      *           Privater Vermögensaufbau
                  20  'XX'-ANL-STRAT-P-VERM-AUFB-UPD     PIC  X(001).
      *
                  20  'XX'-ANL-STRAT-RENT-VORS           PIC  X(001).
      *           Rentenvorsorge
                  20  'XX'-ANL-STRAT-RENT-VORS-UPD       PIC  X(001).
      *
                  20  'XX'-ANL-STRAT-GESRI-VORS          PIC  X(001).
      *           Gesundheits-und Risikovorsorge
                  20  'XX'-ANL-STRAT-GESRI-VORS-UPD      PIC  X(001).
      *
                  20  'XX'-ANL-STRAT-WOHN-EIGENT         PIC  X(001).
      *           Wohneigentum
                  20  'XX'-ANL-STRAT-WOHN-EIGENT-UPD     PIC  X(001).
      *
                  20  'XX'-ANL-STRAT-KEIN-ZIEL           PIC  X(001).
      *           Kein konkretes Ziel
                  20  'XX'-ANL-STRAT-KEIN-ZIEL-UPD       PIC  X(001).
 
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
      *             1 = 0 bis 10 T 
      *             2 = 10 bis 50 T 
      *             3 = > 50 T
                    25  'XX'-FIN-SIT-V-KTO-GUTH-UPD      PIC  X(001).
      *
                    25  'XX'-FIN-SIT-V-WP-VERM           PIC  X(001).
      *             Vermögen > Wertpapiervermögen
      *             1 = 0 bis 10 T 
      *             2 = 10 bis 50 T 
      *             3 = > 50 T
                    25  'XX'-FIN-SIT-V-WP-VERM-UPD       PIC  X(001).
      *
                    25  'XX'-FIN-SIT-V-IMMO-VERM         PIC  X(001).
      *             Vermögen > Immobilienvermögen
      *             1 = 0 bis 100 T 
      *             2 = 100 bis 500 T 
      *             3 = > 500 T 
                    25  'XX'-FIN-SIT-V-IMMO-VERM-UPD     PIC  X(001).
      *
                    25  'XX'-FIN-SIT-V-SONST-VERM        PIC  X(001).
      *             Vermögen > Sonstiges Vermögen
      *             1 = 0 bis 10 T 
      *             2 = 10 bis 50 T 
      *             3 = > 50 T
                    25  'XX'-FIN-SIT-V-SONST-VERM-UPD    PIC  X(001).
      *
                  20  'XX'-FIN-SIT-VERBINDLICHK          PIC  X(001).
      *           Verbindlichkeiten
      *           1 = 0 bis 10 T 
      *           2 = 10 bis 50 T 
      *           3 = > 50 T
                  20  'XX'-FIN-SIT-VERBINDLICHK-UPD      PIC  X(001).
      *
                  20  'XX'-FIN-SIT-NETTOVERM             PIC  X(001).
      *           Nettovermögen
      *           1 = < 0
      *           2 = 0 bis 50 T 
      *           3 = > 50 T 
                  20  'XX'-FIN-SIT-NETTOVERM-UPD         PIC  X(001).
      *
                  20  'XX'-FIN-SIT-MTL-EINK.
      *           ----------------------------------------------------
      *           Mtl. Einkünfte
      *           ----------------------------------------------------
      *
                    25  'XX'-FIN-SIT-M-NICH-SELBST       PIC  X(001).
      *             Mtl. Einkünfte > Aus nichtselbstständiger Arbeit
      *             1 = 0 bis 2 T 
      *             2 = 2 bis 5 T 
      *             3 = > 5 T 
                    25  'XX'-FIN-SIT-M-NICH-SELBST-UPD   PIC  X(001).
      *
                    25  'XX'-FIN-SIT-M-SELBST            PIC  X(001).
      *             Mtl. Einkünfte > Aus selbstständiger Arbeit
      *             1 = 0 bis 2 T 
      *             2 = 2 bis 5 T 
      *             3 = > 5 T 
                    25  'XX'-FIN-SIT-M-SELBST-UPD        PIC  X(001).
      *
                    25  'XX'-FIN-SIT-M-KAP-VERM          PIC  X(001).
      *             Mtl. Einkünfte > Kapitalvermögen
      *             1 = 0 bis 2 T 
      *             2 = 2 bis 5 T 
      *             3 = > 5 T 
                    25  'XX'-FIN-SIT-M-KAP-VERM-UPD      PIC  X(001).
      *
                    25  'XX'-FIN-SIT-M-WEITERE           PIC  X(001).
      *             Mtl. Einkünfte > Weitere
      *             1 = 0 bis 2 T 
      *             2 = 2 bis 5 T 
      *             3 = > 5 T 
                    25  'XX'-FIN-SIT-M-WEITERE-UPD       PIC  X(001).
      *
                  20  'XX'-FIN-SIT-M-AUSGABEN            PIC  X(001).
      *           Mtl. Ausgaben
      *           1 = 0 bis 2 T 
      *           2 = 2 bis 5 T 
      *           3 = > 5 T 
                  20  'XX'-FIN-SIT-M-AUSGABEN-UPD        PIC  X(001).
      *
                  20  'XX'-FIN-SIT-FREIE-M-LIQ           PIC  X(001).
      *           Freie mtl. Liquidität
      *           1 = 0 bis 2 T 
      *           2 = 2 bis 5 T 
      *           3 = > 5 T 
                  20  'XX'-FIN-SIT-FREIE-M-LIQ-UPD       PIC  X(001).
 
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
      *
                    25  'XX'-KERF-R1-DAUER               PIC  X(001).
      *             Risikoklasse 1 > Dauer
      *             1 = Keine bis 1 Jahr
      *             2 = Über 1 bis 3 Jahre
      *             3 = Mehr als 3 Jahre
                    25  'XX'-KERF-R1-DAUER-UPD           PIC  X(001).
 
                  20  'XX'-KERF-R2.
      *           ----------------------------------------------------
      *           Risikoklasse 2 >
      *           ----------------------------------------------------
      *
                    25  'XX'-KERF-R2-RENTENFONDS         PIC  X(001).
      *             Risikoklasse 2 > Rentenfonds
      *             J/N
                    25  'XX'-KERF-R2-RENTENFONDS-UPD     PIC  X(001).
      *
                    25  'XX'-KERF-R2-STRUKT-WP           PIC  X(001).
      *             Risikoklasse 2 > Srukturierte Wertpapiere
      *             J/N
                    25  'XX'-KERF-R2-STRUKT-WP-UPD       PIC  X(001).
      *
                    25  'XX'-KERF-R2-DAUER               PIC  X(001).
      *             Risikoklasse 2 > Dauer
      *             1 = Keine bis 1 Jahr
      *             2 = Über 1 bis 3 Jahre
      *             3 = Mehr als 3 Jahre
                    25  'XX'-KERF-R2-DAUER-UPD           PIC  X(001).
 
                  20  'XX'-KERF-R3.
      *           ----------------------------------------------------
      *           Risikoklasse 3 >
      *           ----------------------------------------------------
      *
                    25  'XX'-KERF-R3-RENTENFONDS         PIC  X(001).
      *             Risikoklasse 3 > Rentenfonds
      *             J/N
                    25  'XX'-KERF-R3-RENTENFONDS-UPD     PIC  X(001).
      *
                    25  'XX'-KERF-R3-AKTIENFONDS         PIC  X(001).
      *             Risikoklasse 3 > Aktienfonds
      *             J/N
                    25  'XX'-KERF-R3-AKTIENFONDS-UPD     PIC  X(001).
      *
                    25  'XX'-KERF-R3-STRUKT-WP           PIC  X(001).
      *             Risikoklasse 3 > Srukturierte Wertpapiere
      *             J/N
                    25  'XX'-KERF-R3-STRUKT-WP-UPD       PIC  X(001).
      *
                    25  'XX'-KERF-R3-ALT-INV             PIC  X(001).
      *             Risikoklasse 3 > Alternative Investments
      *             J/N
                    25  'XX'-KERF-R3-ALT-INV-UPD         PIC  X(001).
      *
                    25  'XX'-KERF-R3-DAUER               PIC  X(001).
      *             Risikoklasse 3 > Dauer
      *             1 = Keine bis 1 Jahr
      *             2 = Über 1 bis 3 Jahre
      *             3 = Mehr als 3 Jahre
                    25  'XX'-KERF-R3-DAUER-UPD           PIC  X(001).
 
                  20  'XX'-KERF-R4.
      *           ----------------------------------------------------
      *           Risikoklasse 4 >
      *           ----------------------------------------------------
      *
                    25  'XX'-KERF-R4-RENTENFONDS         PIC  X(001).
      *             Risikoklasse 4 > Rentenfonds
      *             J/N
                    25  'XX'-KERF-R4-RENTENFONDS-UPD     PIC  X(001).
      *
                    25  'XX'-KERF-R4-AKTIENFONDS         PIC  X(001).
      *             Risikoklasse 4 > Aktienfonds
      *             J/N
                    25  'XX'-KERF-R4-AKTIENFONDS-UPD     PIC  X(001).
      *
                    25  'XX'-KERF-R4-STRUKT-WP           PIC  X(001).
      *             Risikoklasse 4 > Srukturierte Wertpapiere
      *             J/N
                    25  'XX'-KERF-R4-STRUKT-WP-UPD       PIC  X(001).
      *
                    25  'XX'-KERF-R4-ALT-INV             PIC  X(001).
      *             Risikoklasse 4 > Alternative Investments
      *             J/N
                    25  'XX'-KERF-R4-ALT-INV-UPD         PIC  X(001).
      *
                    25  'XX'-KERF-R4-DAUER               PIC  X(001).
      *             Risikoklasse 4 > Dauer
      *             1 = Keine bis 1 Jahr
      *             2 = Über 1 bis 3 Jahre
      *             3 = Mehr als 3 Jahre
                    25  'XX'-KERF-R4-DAUER-UPD           PIC  X(001).
 
                  20  'XX'-KERF-R5.
      *           ----------------------------------------------------
      *           Risikoklasse 5 >
      *           ----------------------------------------------------
      *
                    25  'XX'-KERF-R5-RENTENFONDS         PIC  X(001).
      *             Risikoklasse 5 > Rentenfonds
      *             J/N
                    25  'XX'-KERF-R5-RENTENFONDS-UPD     PIC  X(001).
      *
                    25  'XX'-KERF-R5-AKTIENFONDS         PIC  X(001).
      *             Risikoklasse 5 > Aktienfonds
      *             J/N
                    25  'XX'-KERF-R5-AKTIENFONDS-UPD     PIC  X(001).
      *
                    25  'XX'-KERF-R5-STRUKT-WP           PIC  X(001).
      *             Risikoklasse 5 > Srukturierte Wertpapiere
      *             J/N
                    25  'XX'-KERF-R5-STRUKT-WP-UPD       PIC  X(001).
      *
                    25  'XX'-KERF-R5-AI-TERMIN           PIC  X(001).
      *             Risikoklasse 5 > AI und Termingeschäfte
      *             J/N
                    25  'XX'-KERF-R5-AI-TERMIN-UPD       PIC  X(001).
      *
                    25  'XX'-KERF-R5-DAUER               PIC  X(001).
      *             Risikoklasse 5 > Dauer
      *             1 = Keine bis 1 Jahr
      *             2 = Über 1 bis 3 Jahre
      *             3 = Mehr als 3 Jahre
                    25  'XX'-KERF-R5-DAUER-UPD           PIC  X(001).
 
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
               15  'XX'-GESPRAECHS-PARTNER-UPD        PIC  X(001).
      *
               15  'XX'-NAME-BEVOLLM                  PIC  X(070).
      *        Name, Vorname des Bevollmächtigten
      *        Nur für die KSDS
               15  'XX'-NAME-BEVOLLM-UPD              PIC  X(070).
      *
               15  'XX'-DATUM-PROT-DRUCK              PIC  9(008).
      *        Datum gem. WpHG-Bogens (Unterschriftenfeld)
      *        Zur Befüllung des Beratungsprotokoll
               15  'XX'-DATUM-PROT-DRUCK-UPD          PIC  X(001).
      *
               15  'XX'-KEINE-IN-ANPR-GEN-LEIST       PIC  X(001).
      *        In Anspruch genommene Wertpapierdienstleistungen
      *        gem. WpHG-Bogen war "Keine"
               15  'XX'-KEINE-IN-A-G-LEIST-UPD       PIC  X(001).
 
               15  'XX'-GESELLSCHAFT                  PIC  X(001).
      *        D = DVAG
      *        A = ALLFINANZ
               15  'XX'-GESELLSCHAFT-UPD              PIC  X(001).
 
110917         15  'XX'-KERF-MIFID2.
  >
      *        -------------------------------------------------------
      *        Kenntnisse und Erfahrungen Risikoklasse 2 - Mifid 2
      *        -------------------------------------------------------
 
                   20 'XX'-KERF-R2-MIFID2.
 
                     25 'XX'-KERF-R2-MISCHFONDS     PIC X(001).
      *             Risikoklasse 2 > def. ausgerichtete Mischfonds
      *             J/N
                     25 'XX'-KERF-R2-MISCHFONDS-UPD PIC X(001).
 
                     25 'XX'-KERF-R2-OIF            PIC X(001).
      *             Risikoklasse 2 > offene Immobilienfonds
      *             J/N
                     25 'XX'-KERF-R2-OIF-UPD        PIC X(001).
 
                     25 'XX'-KERF-R2-IMMOINVEST     PIC X(001).
      *             Risikoklasse 2 > strukt. Investment, Immo-Fonds
      *             J/N
                     25 'XX'-KERF-R2-IMMOINVEST-UPD PIC X(001).
 
                     25 'XX'-KERF-R2-KAPITALG       PIC X(001).
      *             Risikoklasse 2 > zum Laufzeitende kapitalgeschuetzte
      *             Produkte von Emittenten mit RLZ < 7 Jahre
      *             J/N
                     25 'XX'-KERF-R2-KAPITALG-UPD   PIC X(001).
 
      *        -------------------------------------------------------
      *        Kenntnisse und Erfahrungen Risikoklasse 3 - Mifid 2
      *        -------------------------------------------------------
 
                   20 'XX'-KERF-R3-MIFID2.
 
                     25 'XX'-KERF-R3-KAPITALG        PIC X(001).
      *             Risikoklasse 3 > zum Laufzeitende kapitalgeschuetzte
      *             Produkte von Emittenten mit RLZ zw. 7 und 15 Jahre
      *             J/N
                     25 'XX'-KERF-R3-KAPITALG-UPD    PIC X(001).
 
                     25 'XX'-KERF-R3-KAPSCHUTZ90     PIC X(001).
      *             Risikoklasse 3 > mit 90% Kapitalschutz
      *             J/N
                     25 'XX'-KERF-R3-KAPSCHUTZ90-UPD PIC X(001).
 
      *        -------------------------------------------------------
      *        Kenntnisse und Erfahrungen Risikoklasse 4 - Mifid 2
      *        -------------------------------------------------------
 
                   20 'XX'-KERF-R4-MIFID2.
 
                     25 'XX'-KERF-R4-RENTE-ETF       PIC X(001).
      *             Risikoklasse 4 > Rentengfonds / ETFs mit Schwerpunkt
      *             auf spekulative Anleihen
      *             J/N
                     25 'XX'-KERF-R4-RENTE-ETF-UPD   PIC X(001).
 
                     25 'XX'-KERF-R4-GELDMARKT       PIC X(001).
      *             Risikoklasse 4 > Geldmarktfonds in Fremdwährung
      *             J/N
                     25 'XX'-KERF-R4-GELDMARKT-UPD   PIC X(001).
 
                     25 'XX'-KERF-R4-AKTIEN-ETF      PIC X(001).
      *             Risikoklasse 4 > Aktienfonds, Aktien-ETFs
      *             J/N
                     25 'XX'-KERF-R4-AKTIEN-UPD      PIC X(001).
 
                     25 'XX'-KERF-R4-MISCHFONDS      PIC X(001).
      *             Risikoklasse 4 > offensiv ausgerichtete Mischfonds
      *             J/N
                     25 'XX'-KERF-R4-MISCHFONDS-UPD  PIC X(001).
 
                     25 'XX'-KERF-R4-OIF             PIC X(001).
      *             Risikoklasse 4 > offene Immobilienfonds
      *             J/N
                     25 'XX'-KERF-R4-OIF-UPD         PIC X(001).
 
                     25 'XX'-KERF-R4-ROHSTOFF        PIC X(001).
      *             Risikoklasse 4 > diversifizierte Rohstofffonds
      *             J/N
                     25 'XX'-KERF-R4-ROHSTOFF-UPD    PIC X(001).
 
                     25 'XX'-KERF-R4-SYNTH-ETF       PIC X(001).
      *             Risikoklasse 4 > synthetische ETFs
      *             J/N
                     25 'XX'-KERF-R4-SYNTH-ETF-UPD   PIC X(001).
 
                     25 'XX'-KERF-R4-STRUKT-AKTIEN   PIC X(001).
      *             Risikoklasse 4 > Strukturierte Produkte mit
      *             Investment gerade auf Aktienfonds/ETFs
      *             J/N
                     25 'XX'-KERF-R4-STR-AKT-UPD     PIC X(001).
 
                     25 'XX'-KERF-R4-BASIS-RK1-RK4   PIC X(001).
      *             Risikoklasse 4 > Strukturierte Produkte von
      *             Emittenten der RK 1 bis RK4
      *             J/N
                     25 'XX'-KERF-R4-B-RK1-RK4-UPD   PIC X(001).
 
                     25 'XX'-KERF-R4-BASIS-RK5       PIC X(001).
      *             Risikoklasse 4 > Strukturierte Produkte von
      *             Emittenten der RK 5 mit mind. 75% Kapitalschutz
      *             J/N
                     25 'XX'-KERF-R4-BASIS-RK5-UPD   PIC X(001).
 
 
      *        -------------------------------------------------------
      *        Kenntnisse und Erfahrungen Risikoklasse 5 - Mifid 2
      *        -------------------------------------------------------
 
                   20 'XX'-KERF-R5-MIFID2.
 
                     25 'XX'-KERF-R5-ROHSTOFF        PIC  X(001).
      *             Risikoklasse 5 > diversifizierte Rohstofffonds
      *             J/N
                     25 'XX'-KERF-R5-ROHSTOFF-UPD    PIC  X(001).
 
                     25 'XX'-KERF-R5-SYN-ETF-AKT     PIC X(001).
      *             Risikoklasse 5 > synthetische ETFs / Indexfonds
      *             auf Emerging Markets Aktien
      *             J/N
                     25 'XX'-KERF-R5-SYN-AKT-UPD     PIC X(001).
 
                     25 'XX'-KERF-R5-SYN-ETF-ROH     PIC X(001).
      *             Risikoklasse 5 > synthetische ETFs / Indexfonds
      *             auf Rohstoffaktien
      *             J/N
                     25 'XX'-KERF-R5-SYN-ROH-UPD     PIC X(001).
 
                     25 'XX'-KERF-R5-STRUKT-ETF      PIC X(001).
      *             Risikoklasse 5 > Strukturierte Produkte mit
      *             Investment gerade auf Rentenfonds /ETFs
      *             J/N
                     25 'XX'-KERF-R5-STRUKT-ETF-UPD  PIC X(001).
      *
 
      *        -------------------------------------------------------
      *        Kenntnisse und Erfahrungen Risikoklasse 6 - Mifid 2
      *        -------------------------------------------------------
 
                   20 'XX'-KERF-R6-MIFID2.
 
                     25 'XX'-KERF-R6-HEBEL           PIC X(001).
      *             Risikoklasse 6 > Hebelprodukte
      *             J/N
                     25 'XX'-KERF-R6-HEBEL-UPD       PIC X(001).
 
                     25 'XX'-KERF-R6-LONGS           PIC X(001).
      *             Risikoklasse 6 > Börslich und außerbörslich
      *             gehandelte Long-Positionen
      *             J/N
                     25 'XX'-KERF-R6-LONGS-UPD       PIC X(001).
 
                     25 'XX'-KERF-R6-DAUER           PIC X(001).
      *             Risikoklasse 6 > Dauer
      *             0 = Erstaufklärung
      *             1 = Keine bis 1 Jahr
      *             2 = Über 1 bis 3 Jahre
      *             3 = Mehr als 3 Jahre
      *             SPACE = bei Mehrfachnennung
                     25 'XX'-KERF-R6-DAUER-UPD       PIC X(001).
 
      *        -------------------------------------------------------
      *        Kenntnisse und Erfahrungen Risikoklasse 7 - Mifid 2
      *        -------------------------------------------------------
 
                   20 'XX'-KERF-R7-MIFID2.
 
                     25 'XX'-KERF-R7-SHORTS          PIC X(001).
      *             Risikoklasse 7 > Börslich und außerbörslich
      *             gehandelte Short-Positionen in Optionen
      *             J/N
                     25 'XX'-KERF-R7-SHORTS-UPD      PIC X(001).
 
                     25 'XX'-KERF-R7-FUTURES         PIC X(001).
      *             Risikoklasse 7 > Börslich und außerbörslich
      *             gehandelte Futures
      *             J/N
                     25 'XX'-KERF-R7-FUTURES-UPD     PIC X(001).
 
                     25 'XX'-KERF-R7-DERIVATE        PIC X(001).
      *             Risikoklasse 7 > Börslich und außerbörslich
      *             gehandelte Derivate wie z.B. Swaps
      *             J/N
                     25 'XX'-KERF-R7-DERIVATE-UPD    PIC X(001).
 
                     25 'XX'-KERF-R7-DEVISEN         PIC X(001).
      *             Risikoklasse 7 > Devisentermingeschäft
      *             J/N
                     25 'XX'-KERF-R7-DEVISEN-UPD     PIC X(001).
 
                     25 'XX'-KERF-R7-DAUER           PIC X(001).
      *             Risikoklasse 7 > Dauer
      *             0 = Erstaufklärung
      *             1 = Keine bis 1 Jahr
      *             2 = Über 1 bis 3 Jahre
      *             3 = Mehr als 3 Jahre
      *             SPACE = bei Mehrfachnennung
                     25 'XX'-KERF-R7-DAUER-UPD       PIC X(001).
 
110917
  <            15  'XX'-RESERVE                      PIC X(073).
 
 

