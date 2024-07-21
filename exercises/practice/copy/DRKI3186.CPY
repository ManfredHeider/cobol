      *****************************************************************
      *                                                               *
      *    G48 - Zielmarkt und ex ante Daten der Deutschen Bank       *
      *    KSDS                                                       *
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
      *AKI01822!28.12.20!2.5 ! neues Feld ETF/ETC Kennz.   !t13ex78   *
      *DBO-8855!19.07.21!    ! neue Felder Gesamtausgabe-  !t13ex78   *
      *        !        !    ! aufschlag                   !          *
      *DBO-10166!13.07.22!   ! neue Felder ESG             !U131661   *
      *         !        !   !                             !          *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
 
       01  'XX'-KSDS-SATZ.
 
 
      * Satzart "21"
01         05  'XX'-SA                        PIC  X(002).
 
      * Gesellschaft = '48'
02         05  'XX'-GES                       PIC  X(002).
 
      * WKN des Fonds
03         05  'XX'-WKN                       PIC  X(006).
 
      * ISIN des Fonds, gleichzeitig Key der KSDS
           05  'XX'-KEY.
04             10  'XX'-ISIN                  PIC  X(012).
 
      * Laufende Fondskosten
05         05  'XX'-FONDSKOSTEN               PIC  9(004)V9(002).
 
      * Datum laufende Fondskosten im Format JHJJMMTT
06         05  'XX'-FONDSKOSTEN-DAT           PIC  9(008).
 
      * Kennzeichen Nullmeldung Fondskosten
      * 'A' - 'XX'-FONDSKOSTEN enthält echten Betrag, also auch 0,00¤
      * ' ' - 'XX'-FONDSKOSTEN ist in der DB2 zu NULLEN
07         05  'XX'-FONDSKOSTEN-NULLKZ        PIC  X(001).
 
      * Schätzkennzeichen zu Fondskosten
      * 'X' - Wert ist von Deutsche Bank geschätzt
      * ' ' - Wert ist von WM zugeliefert (Default)
08         05  'XX'-S-KZ-FONDS                PIC  X(001).
 
      * Transaktionskosten des Fonds
09         05  'XX'-TRANSKOSTEN               PIC  9(004)V9(002).
 
      * Datum Tranksaktionskosten im Format JHJJMMTT
10         05  'XX'-TRANSKOSTEN-DAT           PIC  9(008).
 
      * Kennzeichen Nullmeldung Transaktionskosten
      * 'A' - 'XX'-TRANSKOSTEN enthält echten Betrag, also auch 0,00¤
      * ' ' - 'XX'-TRANSKOSTEN ist in der DB2 zu NULLEN
11         05  'XX'-TRANSKOSTEN-NULLKZ        PIC  X(001).
 
      * Schätzkennzeichen zu Transaktionskosten
      * 'X' - Wert ist von Deutsche Bank geschätzt
      * ' ' - Wert ist von WM zugeliefert (Default)
12         05  'XX'-S-KZ-TRANS                PIC  X(001).
 
      * Performance Fee / Anlassbezogene Kosten
13         05  'XX'-PERFORMANCEFEE            PIC  9(004)V9(002).
 
      * Peformanacefee Datum im Format JHJJMMTT
14         05  'XX'-PERFORMANCEFEE-DAT        PIC  9(008).
 
      * Kennzeichen Nullmeldung Performancefee
      * 'A' - 'XX'-PERFORMANCEFEE enthält echten Betrag, also auch 0,00¤
      * ' ' - 'XX'-PERFORMANCEFEE ist in der DB2 zu NULLEN
15         05  'XX'-PERFORMANCEFEE-NULLKZ     PIC  X(001).
 
      * Schätzkennzeichen zur Performancefee
      * 'X' - Wert ist von Deutsche Bank geschätzt
      * ' ' - Wert ist von WM zugeliefert (Default)
16         05  'XX'-S-KZ-PERFORMANCE          PIC  X(001).
 
      * Tatsächliche Rücknahmekosten
17         05  'XX'-RUECKKOSTEN               PIC  9(004)V9(002).
 
      * Tatsächliche Rücknahekosten Datum im Format JHJJMMTT
18         05  'XX'-RUECKKOSTEN-DAT           PIC  9(008).
 
      * Kennzeichen Nullmeldung Rücknahmekosten
      * 'A' - 'XX'-RUECKKOSTEN enthält echten Betrag, also auch 0,00¤
      * ' ' - 'XX'-RUECKKOSTEN ist in der DB2 zu NULLEN
19         05  'XX'-RUECKKOSTEN-NULLKZ        PIC  X(001).
 
      * Schätzkennzeichen zur Performancefee
      * 'X' - Wert ist von Deutsche Bank geschätzt
      * ' ' - Wert ist von WM zugeliefert (Default)
20         05  'XX'-S-KZ-RUECKKOSTEN          PIC  X(001).
 
      * Minimaler Ausgabeaufschlaf in %
21         05  'XX'-AUSAUF-MIN                PIC S9(006)V9(008).
 
      * Maximaler Ausgabeaufschlag in %
22         05  'XX'-AUSAUF-MAX                PIC S9(006)V9(008).
 
      * Minimaler Rücknahmeabschlag in %
23         05  'XX'-RUECKAB-MIN               PIC S9(006)V9(008).
 
      * Maximaler Rücknahmeabschlag in %
24         05  'XX'-RUECKAB-MAX               PIC S9(006)V9(008).
 
      * Schätzkennzeichen zum ücknahmeabschlag
      * 'X' - Wert ist von Deutsche Bank geschätzt
      * ' ' - Wert ist von WM zugeliefert (Default)
25         05  'XX'-S-KZ-RUECKAB              PIC  X(001).
 
      * Kundenkategorie Zielmarkt Distributor
      * '1' - Privatkunden
      * '2' - Professioneller Kunde
      * '3' - Geeignete Gegenpartei
26         05  'XX'-KATEGORIE-DIS             PIC  X(001).
 
      * Anlageziel Distributor
      * '1' - Spezifische Altersvorsorge
      * '2' - Allgemeine Vermögensbildung / Vermögensoptimierung
      * '3' - Überproportionale Teilnahme an Kursveränderungen
      * '4' - Hedging
27         05  'XX'-ANLAGEZIEL-DIS            PIC  X(001).
 
      * Anlagehorizont Distributor
      * '1' - kurzfristig
      * '2' - mittelfristig
      * '3' - langfristig
      * '4' - ohne Einschränkung / unbestimmt
28         05  'XX'-HORIZONT-DIS              PIC  X(001).
 
      * Kenntnisse und Erfahrungen Zielmarkt Distributor
      * '1' - Anleger mit geringen Kenntnissen & Erfahrungen
      * '2' - Anleger mit durchschnittlichen Kenntnissen & Erfahrungen
      * '3' - Anleger mit überdurchschnittlichen Erfahrungen & Kenntn.
      * '4' - Kenntnisse und/oder Erfahrungen mit hochspeziellen Fin-
      *       anzprodukten, die eine Nachschusspflicht begr. könnten
29         05  'XX'-KENNTNISSE-DIS            PIC  X(001).
 
      * Finanzielle Verlusttragfähigkeit Zielmarkt Distributor
      * '1' - Keine bzw. nur geringe Verluste tragbar
      * '2' - Verluste tragbarbis zum vollst. Verlust d. Kapitals
      * '3' - Verluste auch über eingesetztes Kapital tragbar
30         05  'XX'-VERLUSTTF-DIS             PIC  X(001).
 
      * Vertriebsstrategie Zielmarkt Distributor
      * '1' - Beratungsfrei mit Angemessenheits-Check
      * '2' - wie '1' und Zusatzmaßnahmen
      * '3' - Beratung
      * '4' - Vermögensverwaltung
31         05  'XX'-STRATEGIE-DIS             PIC  X(001).
 
      * Anleger-Doku für Fonds vorhanden (Deutsche Bank)
      * J, N  oder SPACE
32         05  'XX'-ANLDOKU-DB                PIC  X(001).
 
      * Anleger-Doku für Fonds vorhanden (WM Europ. Document Hub)
      * J, N  oder SPACE
33         05  'XX'-ANLDOKU-WM                PIC  X(001).
 
      * Kennzeichen Hebelprodukt
      * 'A' - Hebelprodukt, von Emittenten bestätigt
      * 'B' - Nicht-Hebelprodukt, von Emittenten bestätigt
      * 'C' - Hebelprodukt, von WM aus Assetklasse abgeleitet
      * 'D' - Nicht-Hebelprodukt, von WM aus Assetklasse abgeleitet
34         05  'XX'-HEBELPRODUKT              PIC  X(001).
 
      * Tickgröße nach MiFID2
      * 'A' - Liquiditätsband 1
      * 'B' - Liquiditätsband 2
      * 'C' - Liquiditätsband 3
      * 'D' - Liquiditätsband 4
      * 'E' - Liquiditätsband 5
      * 'F' - Liquiditätsband 6
35         05  'XX'-TICK-MIFID2               PIC  X(001).
 
      * Mindestanlagebetrag bei Fonds
36         05  'XX'-MINDESTANLAGE             PIC  9(015)V9(03).
 
      * Fondskategorie nach UCITS-IV
      * '1' - market fund
      * '2' - absolute return fund
      * '3' - total return fund
      * '4' - life cycle fund
      * '5' - structured fund
      * '6' - other
37         05  'XX'-UCITS-KATEGORIE           PIC  X(001).
 
      * VL Kennzeichen
      * '1' - Aktienfonds mit VL-Förderung
38         05  'XX'-VL-KZ                     PIC  X(001).
 
      * Kennzeichen Auslandsinvestment-Gesetz
39         05  'XX'-AUSL-INV                  PIC  X(001).
 
      * Anlageschwerpunkt Fonds: Kategorie
40         05  'XX'-ANLSP-KATEGORIE           PIC  X(001).
 
      * Anlageschwerpunkt Fonds: Typ
41         05  'XX'-ANLSP-TYP                 PIC  X(001).
 
      * Anlageschwerpunkt Fonds: Kategorie Sonderheiten
      * '1' - ohne Kategorie
      * '2' - Spezialfonds
      * '9' - ohne Zuordnung
      * 'A' - Total/absolute return fund
      * 'B' - Total/absolute Dachfonds
42         05  'XX'-ANLSP-SONDER              PIC  X(001).
 
      * Anlageschwerpunkt Fonds: Garantie
      * '1' - ohne Garantie
      * '2' - mit Garantie
      * '3' - Kapitalschutz
      * '4' - ohne Zuordnung
43         05  'XX'-ANLSP-GARANTIE            PIC  X(001).
 
      * Anlageschwerpunkt Fonds: Laufzeiten
      * '1' - ohne Laufzeitbegrenzung
      * '2' - Laufzeitfonds
      * '3' - ohne Zuordnung
44         05  'XX'-ANLSP-LAUFZ               PIC  X(001).
 
      * Anlageschwerpunkt Fonds: Länder, Regionen, Emittentenbereiche
      * Siehe Tabelle der DeuBa....erst mal nicht in KTY/KNR abgebildet
45         05  'XX'-ANLSP-LAND                PIC  X(002).
 
      * Anlageschwerpunkt Fonds: Währungen
      * '01' - EUR-TN-Währungen
      * '02' - Internationale Währung
      * '03' - Europäische Währung ohne Euro
      * '04' - Europäische Währung mit Euro
      * und vieles mehr gem Tabelle GA7
46         05  'XX'-ANLSP-WAEHRUNG            PIC  X(002).
 
      * Anlageschwerpunkt Fonds: Branchen, Marktsegmente, Instrumente
      * hier nach Tabelle GA8 :-)
47         05  'XX'-ANLSP-BRANCHE             PIC  X(002).
 
      * Abwicklungsfähigkeit
      * 'J' - abwicklungsfähig
      * 'N' - nicht abwicklungsfähig
      * ' ' - Grundstellung
48         05  'XX'-ABWICKLUNG                PIC  X(001).
 
      * Vertriebszulassung für Deutschland
      * '01' - Zulassung im öff. Vertrieb liegt für die Fonds vor
      * '  ' - leer :-)
49         05  'XX'-VERTRIEB-DE               PIC  X(002).
 
      * Vertriebszulassung für Österreich
      * '01' - Zulassung im öff. Vertrieb liegt für die Fonds vor
      * '  ' - leer :-)
50         05  'XX'-VERTRIEB-AT               PIC  X(002).
 
      * Produktrisikoklasse
51         05  'XX'-RISIKOKLASSE              PIC  X(001).
 
      * Wertpapiergruppe - KTY/KNR mit KTY 4478
52         05  'XX'-WPGRUPPE                  PIC  X(002).
      *
      * Kennzeichen ETF oder ETC
      * F305 = ETF - exchange traded fund
      * C311 = ETC - exchange traded commodity
           05  'XX'-ETFETC                    PIC  X(004).
 
           05  FILLER                         PIC  X(002).
      *
      *Devisenkurs: DeuBa-Abrechnungskurs BRIEF
77         05  'XX'-DEVISEN-KURS-BRIEF        PIC  9(08)V9(04).

      *Devisenkurs: DeuBa-Abrechnungskurs GELD
78         05  'XX'-DEVISEN-KURS-GELD         PIC  9(08)V9(04).

      *Devisenkurs: DeuBa-Abrechnungskurs MITTELKURS (bezogen auf
      *obigen beiden Felder

81         05  'XX'-ABREC-MITTEL-KURS         PIC  9(08)V9(05).
           05  'XX'-GESAUSAUF                 PIC  9(003)V9(006).
           05  'XX'-GESAUSAUFKN               PIC  X(001).

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
79         05  'XX'-ZMAESGPRAEFERENZ          PIC  X(001).
      
      *Nachhaltigkeit - Schwerpunkt Auszug aus Datenspiegelbeschreibung:
      * A = kein ESG Schwerpunkt
      * E = Ökologisch
      * S = Sozial
      * G = Good Governance
80         05  'XX'-ZMAESGSCHWERPUNKT         PIC  X(001).

      *Gesamtanteil an nachhaltigen Investitionen (in %)
      * Auszug aus Datenspiegelbeschreibung:
      * Enthält den in der vorvertraglichen Information festgelegten
      * Mindestanteil oder geplanten Anteil der nachhaltigen 
      * Investitionen 
81         05  'XX'-ZMAESGINV                 PIC  9(003)V9(004).
               
      *Datum zu Feld ZMAESGINV (im Format JHJJMMTT)
      * Enthält Veröffentlichungsdatum des vorvertraglichen Dokuments, 
      * in dem der Wert veröffentlicht wurde.
82         05  'XX'-ZMAESGINVDAT              PIC  9(008).
               
      *Gesamtanteil an nachhaltigen Investitionen (in %)
      * Auszug aus Datenspiegelbeschreibung:
      * Enthält letzten tatsächlichen Anteil der nachhaltigen
      * Investition                                                     
83         05  'XX'-ZMAESGLETZTINV            PIC  9(003)V9(004). 
      
      *Datum zu Feld ZMAESGLETZTINV (im Format JHJJMMTT)
      * Enthält Datum, zu dem der letzte tatsächliche Anteil ermittelt 
      * wurde.
84         05  'XX'-ZMAESGLETZTINVDAT         PIC  9(008).     
      
      *Anteil an ökologisch nachhaltigen wirtschaftlichen Aktivitäten 
      *n. Taxonomie (in %)                                                    
      * Enthält in der vorvertraglichen Information festgelegten, 
      * Mindestanteil oder geplanten Anteil der nachhaltigen
      * Investitionen des Finanzinstruments, die taxonomiekonform sind.  
85         05  'XX'-ZMAESGANTOEKO             PIC  9(003)V9(004).   
      
      *Datum zu Feld ZMAESGANTOEKO (im Format JHJJMMTT)
      * Enthält Veröffentlichungsdatum des vorvertraglichen Dokuments, 
      * in dem der Wert veröffentlicht wurde.
86         05  'XX'-ZMAESGANTOEKODAT          PIC  9(008).
               
      *Anteil an ökologisch nachhaltigen wirtschaftlichen Aktivitäten         
      *n. Taxonomie (in %)
      * Enthält letzten tatsächlichen Anteil der nachhaltigen 
      * Investitionen des Finanzinstruments, die taxonomiekonform sind.
      
87         05  'XX'-ZMAESGLZTATOEKO           PIC  9(003)V9(004). 

      *Datum zu Feld ZMAESGLZTATOEKO (im Format JHJJMMTT)
      * Enthält Datum, zu dem der letzte tatsächliche Anteil ermittelt 
      * wurde.
88         05  'XX'-ZMAESGLZTATOEKODAT        PIC  9(008).              
      
      *Umweltziele nach Artikel 9 der Taxonomie-VO  
      * hier: 01 = Klimaschutz
89         05  'XX'-ZMAESGUZ01                PIC  X(001).

      *Umweltziele nach Artikel 9 der Taxonomie-VO 
      * hier: 02 = Anpassung an den Klimawandel
90         05  'XX'-ZMAESGUZ02                PIC  X(001).

      *Umweltziele nach Artikel 9 der Taxonomie-VO 
      * hier: 03 = nachhaltige Nutzung und Schutz von Wasser- und 
      * Meeresressourcen
91         05  'XX'-ZMAESGUZ03                PIC  X(001).
      
      *Umweltziele nach Artikel 9 der Taxonomie-VO 
      * hier: 04 = Übergang zu einer Kreislaufwirtschaft
92         05  'XX'-ZMAESGUZ04                PIC  X(001).

      *Umweltziele nach Artikel 9 der Taxonomie-VO 
      * hier: 05 = Verminderung der Umweltverschmutzung
93         05  'XX'-ZMAESGUZ05                PIC  X(001).

      *Umweltziele nach Artikel 9 der Taxonomie-VO 
      * hier: 06 = Schutz und Wiederherstellung der Biodiversität und 
      * der Ökosysteme
94         05  'XX'-ZMAESGUZ06                PIC  X(001).
               
      *Zusatzinformationen zur SFDR 
      * 01 = Produkt nach Art. 6 der Offenlegungs-VO
      * 02 = Produkt nach Art. 8 der Offenlegungs-VO
      * 03 = Produkt nach Art. 9 der Offenlegungs-VO
      * 04 = Produkt unterliegt keinen SFDR-Regeln
95         05  'XX'-ZMAESGZUSINFOSFDR         PIC  X(002). 

      * Reserve
           05  FILLER                         PIC  X(084).