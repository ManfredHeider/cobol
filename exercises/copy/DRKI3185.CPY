      *****************************************************************
      *                                                               *
      *    G48 - Zielmarkt und ex ante Daten der Deutschen Bank       *
      *    DTA                                                        *
      *                                                               *
      *    SATZLÄNGE 400                                              *
      *                                                               *
      *    LOGBUCH                                                    *
      *--------!-------------------------------------------!----------*
      *AUFTRAG !Datum   !Text                              !Name      *
      *--------!-------------------------------------------!----------*
      *AKI01552!07.11.17!2.0 ! Neu                         !tfl       *
      *--------!-------------------------------------------!----------*
      *AKI01688!20.06.19!    ! neue Felder                 !T13EX19   *
      *AKI01769!11.03.20!    ! neues Feld Wertpapiergruppe !t13ex78   *
      *        !        !    ! (leider von DeuBa mitten in !          *
      *        !        !    ! die DS gestellt)            !          *
      *AKI01822!28.12.20!2.8 ! neues Feld ETF/ETC Kennz.   !t13ex78   *
      *DBO-8855!19.07.21!    ! neue Felder Gesamtausgabe-  !t13ex78   *
      *         !        !   ! aufschlag                   !          *
      *DBO-10166!13.07.22!   ! neue Felder ESG             !U131661   *
      *         !        !   !                             !          *
      *         !        !   !                             !          *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************

       01  'XX'-DTA-SATZ.

      ******************************************************************
      * Vorsatz
      ******************************************************************

           10  'XX'-VORSATZ.

      * Satzart = '01'
               15  'XX'-V-SA                      PIC X(002).
      * Absender = 'DB AG'
               15  'XX'-V-ABSENDER                PIC X(008).
      * Buchungsdatum = 'JHJJMMTT'
               15  'XX'-V-DATUM                   PIC 9(008).
      * Empfänger = 'DVAG', 'ALLF'
               15  'XX'-V-EMPFAENGER              PIC X(004).
      * 'ZIELMART  ' - tägliches Delta
      * 'ZIELMARK  ' - monatlicher Komplettbestand
               15  'XX'-V-TEXT                    PIC X(008).
      * Währungskennzeichen = 'EUR'
               15  'XX'-V-WKZ                     PIC X(003).
      * Reserve
               15  FILLER                         PIC X(367).

      ******************************************************************
      * Hauptsatz
      ******************************************************************

           10  'XX'-HAUPT-SATZ REDEFINES     'XX'-VORSATZ.

      * Satzart "21"
01             15  'XX'-SA                        PIC  X(002).

      * Gesellschaft = '48'
02             15  'XX'-GES                       PIC  X(002).

      * WKN des Fonds
03             15  'XX'-WKN                       PIC  X(006).

      * ISIN des Fonds
04             15  'XX'-ISIN                      PIC  X(012).

      * Laufende Fondskosten
05             15  'XX'-FONDSKOSTEN               PIC  9(004)V9(002).

      * Datum laufende Fondskosten im Format JHJJMMTT
06             15  'XX'-FONDSKOSTEN-DAT           PIC  9(008).

      * Kennzeichen Nullmeldung Fondskosten
      * 'A' - 'XX'-FONDSKOSTEN enthält echten Betrag, also auch 0,00¤
      * ' ' - 'XX'-FONDSKOSTEN ist in der DB2 zu NULLEN
07             15  'XX'-FONDSKOSTEN-NULLKZ        PIC  X(001).

      * Schätzkennzeichen zu Fondskosten
      * 'X' - Wert ist von Deutsche Bank geschätzt
      * ' ' - Wert ist von WM zugeliefert (Default)
08             15  'XX'-S-KZ-FONDS                PIC  X(001).

      * Transaktionskosten des Fonds
09             15  'XX'-TRANSKOSTEN               PIC  9(004)V9(002).

      * Datum Tranksaktionskosten im Format JHJJMMTT
10             15  'XX'-TRANSKOSTEN-DAT           PIC  9(008).

      * Kennzeichen Nullmeldung Transaktionskosten
      * 'A' - 'XX'-TRANSKOSTEN enthält echten Betrag, also auch 0,00¤
      * ' ' - 'XX'-TRANSKOSTEN ist in der DB2 zu NULLEN
11             15  'XX'-TRANSKOSTEN-NULLKZ        PIC  X(001).

      * Schätzkennzeichen zu Transaktionskosten
      * 'X' - Wert ist von Deutsche Bank geschätzt
      * ' ' - Wert ist von WM zugeliefert (Default)
12             15  'XX'-S-KZ-TRANS                PIC  X(001).

      * Performance Fee / Anlassbezogene Kosten
13             15  'XX'-PERFORMANCEFEE            PIC  9(004)V9(002).

      * Peformanacefee Datum im Format JHJJMMTT
14             15  'XX'-PERFORMANCEFEE-DAT        PIC  9(008).

      * Kennzeichen Nullmeldung Performancefee
      * 'A' - 'XX'-PERFORMANCEFEE enthält echten Betrag, also auch 0,00¤
      * ' ' - 'XX'-PERFORMANCEFEE ist in der DB2 zu NULLEN
15             15  'XX'-PERFORMANCEFEE-NULLKZ     PIC  X(001).

      * Schätzkennzeichen zur Performancefee
      * 'X' - Wert ist von Deutsche Bank geschätzt
      * ' ' - Wert ist von WM zugeliefert (Default)
16             15  'XX'-S-KZ-PERFORMANCE          PIC  X(001).

      * Tatsächliche Rücknahmekosten
17             15  'XX'-RUECKKOSTEN               PIC  9(004)V9(002).

      * Tatsächliche Rücknahmekosten Datum im Format JHJJMMTT
18             15  'XX'-RUECKKOSTEN-DAT           PIC  9(008).

      * Kennzeichen Nullmeldung Rücknahmekosten
      * 'A' - 'XX'-RUECKKOSTEN enthält echten Betrag, also auch 0,00¤
      * ' ' - 'XX'-RUECKKOSTEN ist in der DB2 zu NULLEN
19             15  'XX'-RUECKKOSTEN-NULLKZ        PIC  X(001).

      * Schätzkennzeichen zur Performancefee
      * 'X' - Wert ist von Deutsche Bank geschätzt
      * ' ' - Wert ist von WM zugeliefert (Default)
20             15  'XX'-S-KZ-RUECKKOSTEN          PIC  X(001).

      * Minimaler Ausgabeaufschlaf in %
21             15  'XX'-AUSAUF-MIN                PIC S9(006)V9(008).

      * Maximaler Ausgabeaufschlag in %
22             15  'XX'-AUSAUF-MAX                PIC S9(006)V9(008).

      * Minimaler Rücknahmeabschlag in %
23             15  'XX'-RUECKAB-MIN               PIC S9(006)V9(008).

      * Maximaler Rücknahmeabschlag in %
24             15  'XX'-RUECKAB-MAX               PIC S9(006)V9(008).

      * Schätzkennzeichen zum Rücknahmeabschlag
      * 'X' - Wert ist von Deutsche Bank geschätzt
      * ' ' - Wert ist von WM zugeliefert (Default)
25             15  'XX'-S-KZ-RUECKAB              PIC  X(001).

      * Kundenkategorie Zielmarkt Distributor
      * '1' - Privatkunden
      * '2' - Professioneller Kunde
      * '3' - Geeignete Gegenpartei
26             15  'XX'-KATEGORIE-DIS             PIC  X(001).

      * Anlageziel Distributor
      * '1' - Spezifische Altersvorsorge
      * '2' - Allgemeine Vermögensbildung / Vermögensoptimierung
      * '3' - Überproportionale Teilnahme an Kursveränderungen
      * '4' - Hedging
27             15  'XX'-ANLAGEZIEL-DIS            PIC  X(001).

      * Anlagehorizont Distributor
      * '1' - kurzfristig
      * '2' - mittelfristig
      * '3' - langfristig
      * '4' - ohne Einschränkung / unbestimmt
28             15  'XX'-HORIZONT-DIS              PIC  X(001).

      * Kenntnisse und Erfahrungen Zielmarkt Distributor
      * '1' - Anleger mit geringen Kenntnissen & Erfahrungen
      * '2' - Anleger mit durchschnittlichen Kenntnissen & Erfahrungen
      * '3' - Anleger mit überdurchschnittlichen Erfahrungen & Kenntn.
      * '4' - Kenntnisse und/oder Erfahrungen mit hochspeziellen Fin-
      *       anzprodukten, die eine Nachschusspflicht begr. könnten
29             15  'XX'-KENNTNISSE-DIS            PIC  X(001).

      * Finanzielle Verlusttragfähigkeit Zielmarkt Distributor
      * '1' - Keine bzw. nur geringe Verluste tragbar
      * '2' - Verluste tragbarbis zum vollst. Verlust d. Kapitals
      * '3' - Verluste auch über eingesetztes Kapital tragbar
30             15  'XX'-VERLUSTTF-DIS             PIC  X(001).

      * Vertriebsstrategie Zielmarkt Distributor
      * '1' - Beratungsfrei mit Angemessenheits-Check
      * '2' - wie '1' und Zusatzmaßnahmen
      * '3' - Beratung
      * '4' - Vermögensverwaltung
31             15  'XX'-STRATEGIE-DIS             PIC  X(001).

      * Anleger-Doku für Fonds vorhanden (Deutsche Bank)
      * J, N  oder SPACE
32             15  'XX'-ANLDOKU-DB                PIC  X(001).

      * Anleger-Doku für Fonds vorhanden (WM Europ. Document Hub)
      * J, N  oder SPACE
33             15  'XX'-ANLDOKU-WM                PIC  X(001).

      * Kennzeichen Hebelprodukt
      * 'A' - Hebelprodukt, von Emittenten bestätigt
      * 'B' - Nicht-Hebelprodukt, von Emittenten bestätigt
      * 'C' - Hebelprodukt, von WM aus Assetklasse abgeleitet
      * 'D' - Nicht-Hebelprodukt, von WM aus Assetklasse abgeleitet
34             15  'XX'-HEBELPRODUKT              PIC  X(001).

      * Tickgröße nach MiFID2
      * 'A' - Liquiditätsband 1
      * 'B' - Liquiditätsband 2
      * 'C' - Liquiditätsband 3
      * 'D' - Liquiditätsband 4
      * 'E' - Liquiditätsband 5
      * 'F' - Liquiditätsband 6
35             15  'XX'-TICK-MIFID2               PIC  X(001).

      * Mindestanlagebetrag bei Fonds
36             15  'XX'-MINDESTANLAGE             PIC  9(015)V9(03).

      * Fondskategorie nach UCITS-IV
      * '1' - market fund
      * '2' - absolute return fund
      * '3' - total return fund
      * '4' - life cycle fund
      * '5' - structured fund
      * '6' - other
37             15  'XX'-UCITS-KATEGORIE           PIC  X(001).

      * VL Kennzeichen
      * '1' - Aktienfonds mit VL-Förderung
38             15  'XX'-VL-KZ                     PIC  X(001).

      * Kennzeichen Auslandsinvestment-Gesetz
39             15  'XX'-AUSL-INV                  PIC  X(001).

      * Anlageschwerpunkt Fonds: Kategorie
40             15  'XX'-ANLSP-KATEGORIE           PIC  X(001).

      * Anlageschwerpunkt Fonds: Typ
41             15  'XX'-ANLSP-TYP                 PIC  X(001).

      * Anlageschwerpunkt Fonds: Kategorie Sonderheiten
      * '1' - ohne Kategorie
      * '2' - Spezialfonds
      * '9' - ohne Zuordnung
      * 'A' - Total/absolute return fund
      * 'B' - Total/absolute Dachfonds
42             15  'XX'-ANLSP-SONDER              PIC  X(001).

      * Anlageschwerpunkt Fonds: Garantie
      * '1' - ohne Garantie
      * '2' - mit Garantie
      * '3' - Kapitalschutz
      * '4' - ohne Zuordnung
43             15  'XX'-ANLSP-GARANTIE            PIC  X(001).

      * Anlageschwerpunkt Fonds: Laufzeiten
      * '1' - ohne Laufzeitbegrenzung
      * '2' - Laufzeitfonds
      * '3' - ohne Zuordnung
44             15  'XX'-ANLSP-LAUFZ               PIC  X(001).

      * Anlageschwerpunkt Fonds: Länder, Regionen, Emittentenbereiche
      * Siehe Tabelle der DeuBa....erst mal nicht in KTY/KNR abgebildet
45             15  'XX'-ANLSP-LAND                PIC  X(002).

      * Anlageschwerpunkt Fonds: Währungen
      * '01' - EUR-TN-Währungen
      * '02' - Internationale Währung
      * '03' - Europäische Währung ohne Euro
      * '04' - Europäische Währung mit Euro
      * und vieles mehr gem Tabelle GA7
46             15  'XX'-ANLSP-WAEHRUNG            PIC  X(002).

      * Anlageschwerpunkt Fonds: Branchen, Marktsegmente, Instrumente
      * hier nach Tabelle GA8 :-)
47             15  'XX'-ANLSP-BRANCHE             PIC  X(002).

      * Abwicklungsfähigkeit
      * 'J' - abwicklungsfähig
      * 'N' - nicht abwicklungsfähig
      * ' ' - Grundstellung
48             15  'XX'-ABWICKLUNG                PIC  X(001).

      * Vertriebszulassung für Deutschland
      * '01' - Zulassung im öff. Vertrieb liegt für die Fonds vor
      * '  ' - leer :-)
49             15  'XX'-VERTRIEB-DE               PIC  X(002).

      * Vertriebszulassung für Österreich
      * '01' - Zulassung im öff. Vertrieb liegt für die Fonds vor
      * '  ' - leer :-)
50             15  'XX'-VERTRIEB-AT               PIC  X(002).

      * Produktrisikoklasse
51             15  'XX'-RISIKOKLASSE              PIC  X(001).

      * Wertpapiergruppe - Abbildung über KTY/KNR mit KTY 4478
52             15  'XX'-WPGRUPPE                  PIC  X(002).
      *
      * Kennzeichen ETF oder ETC
      * F305 = ETF - exchange traded fund
      * C311 = ETC - exchange traded commodity
               15  'XX'-ETFETC                    PIC  X(004).
      *
      * diese Felder sind jetzt obsolet und auch
      * im Datenspiegel als FILLER  gekennzeichnet
               15  FILLER                         PIC  X(002).
 
      * die Felder 53 - 69 sind irrelevant
               15  FILLER                         PIC  X(017).

      *Devisenkurs: DeuBa-Abrechnungskurs BRIEF
70             15  'XX'-DEVISEN-KURS-BRIEF        PIC  9(008)V9(004).

      *Devisenkurs: DeuBa-Abrechnungskurs GELD
71             15  'XX'-DEVISEN-KURS-GELD         PIC  9(008)V9(004).

      * diese Felder sind irrelevant
               15  FILLER                         PIC  X(0024).
      *Devisenkurs: DeuBa-Abrechnungskurs MITTELKURS (bezogen auf
      *obigen beiden Felder

74             15  'XX'-ABREC-MITTEL-KURS         PIC  9(08)V9(05).
      *
               15  'XX'-GESAUSAUF                 PIC  9(003)V9(006).
               15  'XX'-GESAUSAUFKN               PIC  X(001).
               
      *Nachhaltigkeitspräferenzen - Auszug aus Datenspiegelbeschreibung:
      * A = Keine Angaben/ Daten
      * O = Keine Angaben/Daten oder als nicht nachhaltig deklariert
      * B = Basic = Finanzinstrument berücksichtigt anerkannten
      *     Transparenz- und Branchenstandard                           
      * E = ESG-Produkt folgt dezidierter ESG-Strategie und Hersteller       
      *     berücksichtigt anerkannten Branchenstandard
      * P = Finanzinstrument Artikel 2 Nr. 7a gemäß MiFID II
      * Q = Finanzinstrument Artikel 2 Nr. 7b gemäß MiFID II
      * R = Finanzinstrument Artikel 2 Nr. 7c gemäß MiFID II  
      * S = Finanzinstrument Artikel 2 Nr. 7a, 7b & 7c gemäß MiFID II,
      *     Kriterien für P, Q und R erfüllt
      * T = Finanzinstrument Artikel 2 Nr. 7a, 7b gemäß MiFID II,
      *     Kriterien für P und Q erfüllt
      * U = Finanzinstrument Artikel 2 Nr. 7b & 7c gemäß MiFID II, 
      *     Kriterien für Q und R erfüllt
      * V = Finanzinstrument Artikel 2 Nr. 7a & 7c gemäß MiFID II,
      *     Kriterien für P und R erfüllt
79             15  'XX'-ZMAESGPRAEFERENZ          PIC  X(001).
      
      *Nachhaltigkeit - Schwerpunkt Auszug aus Datenspiegelbeschreibung:
      * A = kein ESG Schwerpunkt
      * E = Ökologisch
      * S = Sozial
      * G = Good Governance
80             15  'XX'-ZMAESGSCHWERPUNKT         PIC  X(001).

      *Gesamtanteil an nachhaltigen Investitionen (in %)
      * Auszug aus Datenspiegelbeschreibung:
      * Enthält den in der vorvertraglichen Information festgelegten
      * Mindestanteil oder geplanten Anteil der nachhaltigen 
      * Investitionen 
81             15  'XX'-ZMAESGINV                 PIC  9(003)V9(004).
               
      *Datum zu Feld ZMAESGINV (im Format JHJJMMTT)
      * Enthält Veröffentlichungsdatum des vorvertraglichen Dokuments, 
      * in dem der Wert veröffentlicht wurde.
82             15  'XX'-ZMAESGINVDAT              PIC  9(008).
               
      *Gesamtanteil an nachhaltigen Investitionen (in %)
      * Auszug aus Datenspiegelbeschreibung:
      * Enthält letzten tatsächlichen Anteil der nachhaltigen
      * Investition                                                     
83             15  'XX'-ZMAESGLETZTINV            PIC  9(003)V9(004). 
      
      *Datum zu Feld ZMAESGLETZTINV (im Format JHJJMMTT)
      * Enthält Datum, zu dem der letzte tatsächliche Anteil ermittelt 
      * wurde.
84             15  'XX'-ZMAESGLETZTINVDAT         PIC  9(008).     
      
      *Anteil an ökologisch nachhaltigen wirtschaftlichen Aktivitäten 
      *n. Taxonomie (in %)                                                    
      * Enthält in der vorvertraglichen Information festgelegten, 
      * Mindestanteil oder geplanten Anteil der nachhaltigen
      * Investitionen des Finanzinstruments, die taxonomiekonform sind.  
85             15  'XX'-ZMAESGANTOEKO             PIC  9(003)V9(004).   
      
      *Datum zu Feld ZMAESGANTOEKO (im Format JHJJMMTT)
      * Enthält Veröffentlichungsdatum des vorvertraglichen Dokuments, 
      * in dem der Wert veröffentlicht wurde.
86             15  'XX'-ZMAESGANTOEKODAT          PIC  9(008).
               
      *Anteil an ökologisch nachhaltigen wirtschaftlichen Aktivitäten         
      *n. Taxonomie (in %)
      * Enthält letzten tatsächlichen Anteil der nachhaltigen 
      * Investitionen des Finanzinstruments, die taxonomiekonform sind.
      
87             15  'XX'-ZMAESGLZTATOEKO           PIC  9(003)V9(004). 

      *Datum zu Feld ZMAESGLZTATOEKO (im Format JHJJMMTT)
      * Enthält Datum, zu dem der letzte tatsächliche Anteil ermittelt 
      * wurde.
88             15  'XX'-ZMAESGLZTATOEKODAT        PIC  9(008).
      
      *Umweltziele nach Artikel 9 der Taxonomie-VO  
      * hier: 01 = Klimaschutz
89             15  'XX'-ZMAESGUZ01                PIC  X(001).

      *Umweltziele nach Artikel 9 der Taxonomie-VO 
      * hier: 02 = Anpassung an den Klimawandel
90             15  'XX'-ZMAESGUZ02                PIC  X(001).

      *Umweltziele nach Artikel 9 der Taxonomie-VO 
      * hier: 03 = nachhaltige Nutzung und Schutz von Wasser- und 
      * Meeresressourcen
91             15  'XX'-ZMAESGUZ03                PIC  X(001).
      
      *Umweltziele nach Artikel 9 der Taxonomie-VO 
      * hier: 04 = Übergang zu einer Kreislaufwirtschaft
92             15  'XX'-ZMAESGUZ04                PIC  X(001).

      *Umweltziele nach Artikel 9 der Taxonomie-VO 
      * hier: 05 = Verminderung der Umweltverschmutzung
93             15  'XX'-ZMAESGUZ05                PIC  X(001).

      *Umweltziele nach Artikel 9 der Taxonomie-VO 
      * hier: 06 = Schutz und Wiederherstellung der Biodiversität und 
      * der Ökosysteme
94             15  'XX'-ZMAESGUZ06                PIC  X(001).
               
      *Zusatzinformationen zur SFDR 
      * 01 = Produkt nach Art. 6 der Offenlegungs-VO
      * 02 = Produkt nach Art. 8 der Offenlegungs-VO
      * 03 = Produkt nach Art. 9 der Offenlegungs-VO
      * 04 = Produkt unterliegt keinen SFDR-Regeln
95             15  'XX'-ZMAESGZUSINFOSFDR         PIC  X(002).         
      
      *Reserve
               15  FILLER                         PIC  X(043).

      ******************************************************************
      * Nachsatz
      ******************************************************************

           10  'XX'-NACHSATZ   REDEFINES     'XX'-VORSATZ.

               15  'XX'-N-SA                      PIC  X(002).
      * Satzart '99'
               15  'XX'-N-ANZ                     PIC  9(007).
      * Anzahl Sätze OHNE Vor- und Nachsatz
               15  FILLER                         PIC  X(391).
