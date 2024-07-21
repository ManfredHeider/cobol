      *****************************************************************
      *                                                               *
      *    G48 - Zielmarkt und ex ante Daten der Deutschen Bank       *
      *    Eingabe für Aktualisierungsprogramm PMKI1355               *
      *                                                               *
      *    SATZLÄNGE 445     
      *                                                               *
      *    LOGBUCH                                                    *
      *--------!-------------------------------------------!----------*
      *AUFTRAG !Datum   !Text                              !Name      *
      *--------!-------------------------------------------!----------*
      *AKI01552!07.11.17!2.0 ! Neu                         !tfl       *
      *--------!-------------------------------------------!----------*
      *AKI01688!20.06.19!    ! neue Felder                 !T13EX19   *
      *AKI01769!11.03.20!    ! Aufnahme WP-Gruppe und KZN  !t13ex78   *
      *        !        !    ! Investment J/N              !          *
      *AKI01822!28.12.20!2.5 ! Neues Feld ETF/ETC und      !t13ex78   *
      *        !        !    ! Fondsart (abgeleitet aus    !          *
      *        !        !    ! ZMAASFKAT)                  !          *
      *DBO-8855!19.07.21!    ! neue Felder Gesamtausgabe-  !t13ex78   *
      *        !        !    ! aufschlag                   !          *
      *DBO-10166!13.07.22!   ! neue Felder ESG             !U131661   *
      *         !        !   !                             !          * 
      * ENDE-LOGBUCH                                                  * 
      *****************************************************************
 
       01  'XX'-AKTU-SATZ.
 
      * Aktualisierungskennzeichen
      * 'I' - Insert
      * 'U' - Update
      * 'D' - Delete
           05  'XX'-AKTUAL-KZ                 PIC  X(001).
 
      * Fonds-ID - wird im PMKI1354 ermittelt
           05  'XX'-FONDS-ID                  PIC S9(004) COMP.
 
      * WKN und ISIN
           05  'XX'-WKN                       PIC  X(006).
           05  'XX'-ISIN                      PIC  X(012).
 
      * Laufende Fondskosten
           05  'XX'-FONDSKOSTEN               PIC  9(004)V9(002).
           05  'XX'-FONDSKOSTEN-AKN           PIC  X(001).
 
      * Datum laufende Fondskosten im Format JHJJMMTT
           05  'XX'-FONDSKOSTEN-DAT           PIC  X(010).
           05  'XX'-FONDSKOSTEN-DAT-AKN       PIC  X(001).
 
      * Kennzeichen Nullmeldung Fondskosten
           05  'XX'-FONDSKOSTEN-NULLKZ        PIC  X(001).
 
      * Schätzkennzeichen zu Fondskosten
           05  'XX'-S-KZ-FONDS                PIC  X(001).
           05  'XX'-S-KZ-FONDS-AKN            PIC  X(001).
 
      * Transaktionskosten des Fonds
           05  'XX'-TRANSKOSTEN               PIC  9(004)V9(002).
           05  'XX'-TRANSKOSTEN-AKN           PIC  X(001).
 
      * Datum Tranksaktionskosten im Format JHJJMMTT
           05  'XX'-TRANSKOSTEN-DAT           PIC  X(010).
           05  'XX'-TRANSKOSTEN-DAT-AKN       PIC  X(001).
 
      * Kennzeichen Nullmeldung Transaktionskosten
           05  'XX'-TRANSKOSTEN-NULLKZ        PIC  X(001).
 
      * Schätzkennzeichen zu Transaktionskosten
           05  'XX'-S-KZ-TRANS                PIC  X(001).
           05  'XX'-S-KZ-TRANS-AKN            PIC  X(001).
 
      * Performance Fee / Anlassbezogene Kosten
           05  'XX'-PERFORMANCEFEE            PIC  9(004)V9(002).
           05  'XX'-PERFORMANCEFEE-AKN        PIC  X(001).
 
      * Peformanacefee Datum im Format JHJJMMTT
           05  'XX'-PERFORMANCEFEE-DAT        PIC  X(010).
           05  'XX'-PERFORMFEE-DAT-AKN        PIC  X(001).
 
      * Kennzeichen Nullmeldung Performancefee
           05  'XX'-PERFORMANCEFEE-NULLKZ     PIC  X(001).
 
      * Schätzkennzeichen zur Performancefee
           05  'XX'-S-KZ-PERFORMANCE          PIC  X(001).
           05  'XX'-S-KZ-PERFORMANCE-AKN      PIC  X(001).
 
      * Tatsächliche Rücknahmekosten
           05  'XX'-RUECKKOSTEN               PIC  9(004)V9(002).
           05  'XX'-RUECKKOSTEN-AKN           PIC  X(001).
 
      * Tatsächliche Rücknahekosten Datum im Format JHJJMMTT
           05  'XX'-RUECKKOSTEN-DAT           PIC  X(010).
           05  'XX'-RUECKKOSTEN-DAT-AKN       PIC  X(001).
 
      * Kennzeichen Nullmeldung Rücknahmekosten
           05  'XX'-RUECKKOSTEN-NULLKZ        PIC  X(001).
 
      * Schätzkennzeichen zur Performancefee
           05  'XX'-S-KZ-RUECKKOSTEN          PIC  X(001).
           05  'XX'-S-KZ-RUECKKOSTEN-AKN      PIC  X(001).
 
      * Minimaler Ausgabeaufschlaf in %
           05  'XX'-AUSAUF-MIN                PIC S9(006)V9(008).
           05  'XX'-AUSAUF-MIN-AKN            PIC  X(001).
 
      * Maximaler Ausgabeaufschlag in %
           05  'XX'-AUSAUF-MAX                PIC S9(006)V9(008).
           05  'XX'-AUSAUF-MAX-AKN            PIC  X(001).
 
      * Minimaler Rücknahmeabschlag in %
           05  'XX'-RUECKAB-MIN               PIC S9(006)V9(008).
           05  'XX'-RUECKAB-MIN-AKN           PIC  X(001).
 
      * Maximaler Rücknahmeabschlag in %
           05  'XX'-RUECKAB-MAX               PIC S9(006)V9(008).
           05  'XX'-RUECKAB-MAX-AKN           PIC  X(001).
 
      * Schätzkennzeichen zum Rücknahmeabschlag
           05  'XX'-S-KZ-RUECKAB              PIC  X(001).
           05  'XX'-S-KZ-RUECKAB-AKN          PIC  X(001).
 
      * Kundenkategorie Zielmarkt Distributor
           05  'XX'-KATEGORIE-DIS             PIC  X(001).
           05  'XX'-KATEGORIE-DIS-AKN         PIC  X(001).
 
      * Anlageziel Distributor
           05  'XX'-ANLAGEZIEL-DIS            PIC  X(001).
           05  'XX'-ANLAGEZIEL-DIS-AKN        PIC  X(001).
 
      * Anlagehorizont Distributor
           05  'XX'-HORIZONT-DIS              PIC  X(001).
           05  'XX'-HORIZONT-DIS-AKN          PIC  X(001).
 
      * Kenntnisse und Erfahrungen Zielmarkt Distributor
           05  'XX'-KENNTNISSE-DIS            PIC  X(001).
           05  'XX'-KENNTNISSE-DIS-AKN        PIC  X(001).
 
      * Finanzielle Verlusttragfähigkeit Zielmarkt Distributor
           05  'XX'-VERLUSTTF-DIS             PIC  X(001).
           05  'XX'-VERLUSTTF-DIS-AKN         PIC  X(001).
 
      * Vertriebsstrategie Zielmarkt Distributor
           05  'XX'-STRATEGIE-DIS             PIC  X(001).
           05  'XX'-STRATEGIE-DIS-AKN         PIC  X(001).
 
      * Anleger-Doku für Fonds vorhanden (Deutsche Bank)
           05  'XX'-ANLDOKU-DB                PIC  X(001).
           05  'XX'-ANLDOKU-DB-AKN            PIC  X(001).
 
      * Anleger-Doku für Fonds vorhanden (WM Europ. Document Hub)
           05  'XX'-ANLDOKU-WM                PIC  X(001).
           05  'XX'-ANLDOKU-WM-AKN            PIC  X(001).
 
      * Kennzeichen Hebelprodukt
           05  'XX'-HEBELPRODUKT              PIC  X(001).
           05  'XX'-HEBELPRODUKT-AKN          PIC  X(001).
 
      * Tickgröße nach MiFID2
           05  'XX'-TICK-MIFID2               PIC  X(001).
           05  'XX'-TICK-MIFID2-AKN           PIC  X(001).
 
      * Mindestanlagebetrag bei Fonds
           05  'XX'-MINDESTANLAGE             PIC  9(015)V9(03).
           05  'XX'-MINDESTANLAGE-AKN         PIC  X(001).
 
      * Fondskategorie nach UCITS-IV
           05  'XX'-UCITS-KATEGORIE           PIC  X(001).
           05  'XX'-UCITS-KATEGORIE-AKN       PIC  X(001).
 
      * VL Kennzeichen
           05  'XX'-VL-KZ                     PIC  X(001).
           05  'XX'-VL-KZ-AKN                 PIC  X(001).
 
      * Kennzeichen Auslandsinvestment-Gesetz
           05  'XX'-AUSL-INV                  PIC  X(001).
           05  'XX'-AUSL-INV-AKN              PIC  X(001).
 
      * Anlageschwerpunkt Fonds: Kategorie
           05  'XX'-ANLSP-KATEGORIE           PIC  X(001).
           05  'XX'-ANLSP-KATEGORIE-AKN       PIC  X(001).
 
      * Anlageschwerpunkt Fonds: Typ
           05  'XX'-ANLSP-TYP                 PIC  X(001).
           05  'XX'-ANLSP-TYP-AKN             PIC  X(001).
 
      * Anlageschwerpunkt Fonds: Kategorie Sonderheiten
           05  'XX'-ANLSP-SONDER              PIC  X(001).
           05  'XX'-ANLSP-SONDER-AKN          PIC  X(001).
 
      * Anlageschwerpunkt Fonds: Garantie
           05  'XX'-ANLSP-GARANTIE            PIC  X(001).
           05  'XX'-ANLSP-GARANTIE-AKN        PIC  X(001).
 
      * Anlageschwerpunkt Fonds: Laufzeiten
           05  'XX'-ANLSP-LAUFZ               PIC  X(001).
           05  'XX'-ANLSP-LAUFZ-AKN           PIC  X(001).
 
      * Anlageschwerpunkt Fonds: Länder, Regionen, Emittentenbereiche
      * Siehe Tabelle der DeuBa....erst mal nicht in KTY/KNR abgebildet
           05  'XX'-ANLSP-LAND                PIC  X(002).
           05  'XX'-ANLSP-LAND-AKN            PIC  X(001).
 
      * Anlageschwerpunkt Fonds: Währungen
           05  'XX'-ANLSP-WAEHRUNG            PIC  X(002).
           05  'XX'-ANLSP-WAEHRUNG-AKN        PIC  X(001).
 
      * Anlageschwerpunkt Fonds: Branchen, Marktsegmente, Instrumente
           05  'XX'-ANLSP-BRANCHE             PIC  X(002).
           05  'XX'-ANLSP-BRANCHE-AKN         PIC  X(001).
 
      * Abwicklungsfähigkeit
           05  'XX'-ABWICKLUNG                PIC  X(001).
           05  'XX'-ABWICKLUNG-AKN            PIC  X(001).
 
      * Vertriebszulassung für Deutschland
           05  'XX'-VERTRIEB-DE               PIC  X(002).
           05  'XX'-VERTRIEB-DE-AKN           PIC  X(001).
 
      * Vertriebszulassung für Österreich
           05  'XX'-VERTRIEB-AT               PIC  X(002).
           05  'XX'-VERTRIEB-AT-AKN           PIC  X(001).
 
      * Produktrisikoklasse
           05  'XX'-RISIKOKLASSE              PIC  X(001).
           05  'XX'-RISIKOKLASSE-AKN          PIC  X(001).
 
      * Kundenkategorie Zielmarkt Emittent
           05  'XX'-KATEGORIE-EMI             PIC  X(001).
           05  'XX'-KATEGORIE-EMI-AKN         PIC  X(001).
 
      * Anlageziel Emittent
           05  'XX'-ANLAGEZIEL-EMI            PIC  X(001).
           05  'XX'-ANLAGEZIEL-EMI-AKN        PIC  X(001).
 
      * Anlagehorizont Emittent
           05  'XX'-HORIZONT-EMI              PIC  X(001).
           05  'XX'-HORIZONT-EMI-AKN          PIC  X(001).
 
      * Kenntnisse und Erfahrungen Zielmarkt Emittent
           05  'XX'-KENNTNISSE-EMI            PIC  X(001).
           05  'XX'-KENNTNISSE-EMI-AKN        PIC  X(001).
 
      * Finanzielle Verlusttragfähigkeit Zielmarkt Emittent
           05  'XX'-VERLUSTTF-EMI             PIC  X(001).
           05  'XX'-VERLUSTTF-EMI-AKN         PIC  X(001).
 
      * Risiko- und Renditeprofile
           05  'XX'-RISIKO-EMI                PIC  X(001).
           05  'XX'-RISIKO-EMI-AKN            PIC  X(001).
 
      * Vertriebsstrategie Zielmarkt Emittent
           05  'XX'-STRATEGIE-EMI             PIC  X(001).
           05  'XX'-STRATEGIE-EMI-AKN         PIC  X(001).
 
      * Spezielle Anforderungen
           05  'XX'-ANFORDERUNGEN             PIC  X(001).
           05  'XX'-ANFORDERUNGEN-AKN         PIC  X(001).
 
      *Devisenkurs: DeuBa-Abrechnungskurs BRIEF
77         05  'XX'-DEVISEN-KURS-BRIEF        PIC  9(08)V9(04).
           05  'XX'-DEV-BRIEF-AKN             PIC  X(001).

      *Devisenkurs: DeuBa-Abrechnungskurs GELD
78         05  'XX'-DEVISEN-KURS-GELD         PIC  9(08)V9(04).
           05  'XX'-DEV-GELD-AKN              PIC  X(001).

      *Devisenkurs: DeuBa-Abrechnungskurs MITTELKURS (bezogen auf
      *obigen beiden Felder

81         05  'XX'-ABREC-MITTEL-KURS         PIC  9(08)V9(05).
           05  'XX'-ABRECH-KURS-AKN           PIC  X(001).
 
      * Wertpapiergruppe
           05  'XX'-WPGRUPPE                  PIC  X(002).
           05  'XX'-WPGRUPPE-AKN              PIC  X(001).
      *
      * Kennzeichen ob Investmendfonds J oder N
           05  'XX'-ISTINVEST                 PIC  X(001).
           05  'XX'-ISTINVEST-AKN             PIC  X(001).
      *
      * ETF oder ETC Kennzeichen
           05  'XX'-ETFETC                    PIC  X(004).
           05  'XX'-ETFETC-AKN                PIC  X(001).
      *
      * Fondsart
           05  'XX'-FONDSART                  PIC  X(020).
           05  'XX'-FONDSART-AKN              PIC  X(001).
      *
           05  'XX'-GESAUSAUF                 PIC  9(003)V9(006).
           05  'XX'-GESAUSAUF-AKN             PIC  X(001).
           
           05  'XX'-GESAUSAUFKN               PIC  X(001).
           05  'XX'-GESAUSAUFKN-AKN           PIC  X(001).
           
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
           05  'XX'-ZMAESGPRAEFERENZ          PIC  X(001).
           05  'XX'-ZMAESGPRAEFERENZ-AKN      PIC  X(001).
      
      *Nachhaltigkeit - Schwerpunkt Auszug aus Datenspiegelbeschreibung:
      * A = kein ESG Schwerpunkt
      * E = Ökologisch
      * S = Sozial
      * G = Good Governance
           05  'XX'-ZMAESGSCHWERPUNKT         PIC  X(001).
           05  'XX'-ZMAESGSCHWERPUNKT-AKN     PIC  X(001).          

      *Gesamtanteil an nachhaltigen Investitionen (in %)
      * Auszug aus Datenspiegelbeschreibung:
      * Enthält den in der vorvertraglichen Information festgelegten
      * Mindestanteil oder geplanten Anteil der nachhaltigen 
      * Investitionen 
           05  'XX'-ZMAESGINV                 PIC  9(003)V9(004).
           05  'XX'-ZMAESGINV-AKN             PIC  X(001).          
               
      *Datum zu Feld ZMAESGINV (im Format JHJJMMTT)
      * Enthält Veröffentlichungsdatum des vorvertraglichen Dokuments, 
      * in dem der Wert veröffentlicht wurde.
           05  'XX'-ZMAESGINVDAT              PIC  X(010).
           05  'XX'-ZMAESGINVDAT-AKN          PIC  X(001).
               
      *Gesamtanteil an nachhaltigen Investitionen (in %)
      * Auszug aus Datenspiegelbeschreibung:
      * Enthält letzten tatsächlichen Anteil der nachhaltigen
      * Investition                                                     
           05  'XX'-ZMAESGLETZTINV            PIC  9(003)V9(004).
           05  'XX'-ZMAESGLETZTINV-AKN        PIC  X(001).
      
      *Datum zu Feld ZMAESGLETZTINV (im Format JHJJMMTT)
      * Enthält Datum, zu dem der letzte tatsächliche Anteil ermittelt 
      * wurde.
           05  'XX'-ZMAESGLETZTINVDAT         PIC  X(010).
           05  'XX'-ZMAESGLETZTINVDAT-AKN     PIC  X(001).           
      
      *Anteil an ökologisch nachhaltigen wirtschaftlichen Aktivitäten 
      *n. Taxonomie (in %)                                                    
      * Enthält in der vorvertraglichen Information festgelegten, 
      * Mindestanteil oder geplanten Anteil der nachhaltigen
      * Investitionen des Finanzinstruments, die taxonomiekonform sind.  
           05  'XX'-ZMAESGANTOEKO             PIC  9(003)V9(004).
           05  'XX'-ZMAESGANTOEKO-AKN         PIC  X(001).
      
      *Datum zu Feld ZMAESGANTOEKO (im Format JHJJMMTT)
      * Enthält Veröffentlichungsdatum des vorvertraglichen Dokuments, 
      * in dem der Wert veröffentlicht wurde.
           05  'XX'-ZMAESGANTOEKODAT          PIC  X(010).
           05  'XX'-ZMAESGANTOEKODAT-AKN      PIC  X(001).           
               
      *Anteil an ökologisch nachhaltigen wirtschaftlichen Aktivitäten         
      *n. Taxonomie (in %)
      * Enthält letzten tatsächlichen Anteil der nachhaltigen 
      * Investitionen des Finanzinstruments, die taxonomiekonform sind.
      
           05  'XX'-ZMAESGLZTATOEKO           PIC  9(003)V9(004).
           05  'XX'-ZMAESGLZTATOEKO-AKN       PIC  X(001).              

      *Datum zu Feld ZMAESGLZTATOEKO (im Format JHJJMMTT)
      * Enthält Datum, zu dem der letzte tatsächliche Anteil ermittelt 
      * wurde.
           05  'XX'-ZMAESGLZTATOEKODAT        PIC  X(010).
           05  'XX'-ZMAESGLZTATOEKODAT-AKN    PIC  X(001).           
      
      *Umweltziele nach Artikel 9 der Taxonomie-VO  
      * hier: 01 = Klimaschutz
           05  'XX'-ZMAESGUZ01                PIC  X(001).
           05  'XX'-ZMAESGUZ01-AKN            PIC  X(001).

      *Umweltziele nach Artikel 9 der Taxonomie-VO 
      * hier: 02 = Anpassung an den Klimawandel
           05  'XX'-ZMAESGUZ02                PIC  X(001).
           05  'XX'-ZMAESGUZ02-AKN            PIC  X(001).

      *Umweltziele nach Artikel 9 der Taxonomie-VO 
      * hier: 03 = nachhaltige Nutzung und Schutz von Wasser- und 
      * Meeresressourcen
           05  'XX'-ZMAESGUZ03                PIC  X(001).
           05  'XX'-ZMAESGUZ03-AKN            PIC  X(001).
      
      *Umweltziele nach Artikel 9 der Taxonomie-VO 
      * hier: 04 = Übergang zu einer Kreislaufwirtschaft
           05  'XX'-ZMAESGUZ04                PIC  X(001).
           05  'XX'-ZMAESGUZ04-AKN            PIC  X(001).

      *Umweltziele nach Artikel 9 der Taxonomie-VO 
      * hier: 05 = Verminderung der Umweltverschmutzung
           05  'XX'-ZMAESGUZ05                PIC  X(001).
           05  'XX'-ZMAESGUZ05-AKN            PIC  X(001).

      *Umweltziele nach Artikel 9 der Taxonomie-VO 
      * hier: 06 = Schutz und Wiederherstellung der Biodiversität und 
      * der Ökosysteme
           05  'XX'-ZMAESGUZ06                PIC  X(001).
           05  'XX'-ZMAESGUZ06-AKN            PIC  X(001).
               
      *Zusatzinformationen zur SFDR 
      * 01 = Produkt nach Art. 6 der Offenlegungs-VO
      * 02 = Produkt nach Art. 8 der Offenlegungs-VO
      * 03 = Produkt nach Art. 9 der Offenlegungs-VO
      * 04 = Produkt unterliegt keinen SFDR-Regeln
           05  'XX'-ZMAESGZUSINFOSFDR         PIC  X(002).
           05  'XX'-ZMAESGZUSINFOSFDR-AKN     PIC  X(001).
           
      *Reserve
           05  FILLER                         PIC  X(010).