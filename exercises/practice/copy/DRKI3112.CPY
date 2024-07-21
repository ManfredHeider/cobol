      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  02.11.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3112.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI3112.CPv  $
      *
      *   Rev 2.2   Dec 19 2016 15:53:40   t13ac20
      *AKI01466: Ermittlung
      *
      *   Rev 2.1   Dec 07 2016 13:03:10   t13ac20
      *AKI01466: Ermittlung
      *
      *   Rev 2.0   Nov 02 2016 13:40:54   t13ac20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3112                                      *
      *    --------------------------------------------------------   *
      *    ÄNDERUNGSDATEN AUS DER ERMITTLUNG VERTRAG/KUNDE            *
      *    DER BKK-LINDE - G51                                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: SWO                                              *
      *    DATUM:    02.11.2016                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AKI01466!02.11.16!2.1 ! NEU                       ! SWO       *
      * AKI01466!16.12.16!2.2 ! FELDER ENTFERNT:          ! SWO       *
      *         !        !    !   - RAHMENVERTRAG         !           *
      *         !        !    !     RAHMENVERTRAG-KZN     !           *
      *         !        !    !   - ABRECHWIDERRUFDAT     !           *
      *         !        !    !     ABRECHWIDERRUFDAT-KZN !           *
      *         !        !    !   - ABRECHWIDERRUFDAT-VP  !           *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *    Verarbeitungskennzeichen  I = Insert, U = Update, D = Delete
      *                              W = Wiederinkraftsetzung
           05 'XX'-VERARB-KZN                   PIC  X(001).
      *  Gesellschaftsnummer DVAG-intern - immer '51'
           05 'XX'-GES                          PIC  9(002).
      *  Vertragsnummer
           05 'XX'-VERNR                        PIC  X(015).
      *  PIN aus dem Antrag
           05 'XX'-ANTRAGSPIN                   PIC  9(010).
           05 'XX'-ANTRAGSPIN-KZN               PIC  X(001).
      *  Vertragszustand
      *    A = aktiv
      *    Z = zukuenftiger Vertrag
      *    D = abgelaufen/storniert
           05 'XX'-VERTRAGSZUSTAND              PIC  X(001).
           05 'XX'-VERTRAGSZUSTAND-KZN          PIC  X(001).
      *  Versicherungsart V (Ebene Vertrag, freiwillig / pflichtvers.)
           05 'XX'-VERSICHERUNGSARTV            PIC  X(001).
           05 'XX'-VERSICHERUNGSARTV-KZN        PIC  X(001).
      *  Beginndatum des Vertrages im Format JHJJMMTT
           05 'XX'-BEGDATVERTRAG                PIC  X(008).
           05 'XX'-BEGDATVERTRAG-KZN            PIC  X(001).
      *  Endedatum des Vertrages im Format JHJJMMTT
           05 'XX'-ENDDATVERTRAG                PIC  X(008).
           05 'XX'-ENDDATVERTRAG-KZN            PIC  X(001).
      *  VB-Nummer
           05 'XX'-VBNR                         PIC  9(007).
           05 'XX'-VBNR-KZN                     PIC  X(001).
      *  Nachname des Mitgliedes
           05 'XX'-NACHNAME                     PIC  X(030).
           05 'XX'-NACHNAME-KZN                 PIC  X(001).
      *  Vorname des Mitgliedes
           05 'XX'-VORNAME                      PIC  X(030).
           05 'XX'-VORNAME-KZN                  PIC  X(001).
      *  Geburtsdatum des Mitgliedes im Format JJJMMTT
           05 'XX'-GEBDAT                       PIC  X(008).
           05 'XX'-GEBDAT-KZN                   PIC  X(001).
      *  Anredeschlüssel
      *    1 = Herr
      *    2 = Frau
      *    4 = Eheleute
      *    5 = Firma
      *    9 = keine Angabe
           05 'XX'-ANREDE                       PIC  9(001).
           05 'XX'-ANREDE-KZN                   PIC  X(001).
      *  Verschluesselung des Geschlechts
      *    1 = maennlich
      *    2 = weiblich
      *    3 = unbekannt
           05 'XX'-GESCHLECHT                   PIC  9(001).
           05 'XX'-GESCHLECHT-KZN               PIC  X(001).
      *  Straße und Hausnummer
           05 'XX'-STRHNR                       PIC  X(040).
           05 'XX'-STRHNR-KZN                   PIC  X(001).
      *  Postleitzahl
           05 'XX'-PLZ                          PIC  X(009).
           05 'XX'-PLZ-KZN                      PIC  X(001).
      *  Wohnort
           05 'XX'-ORT                          PIC  X(030).
           05 'XX'-ORT-KZN                      PIC  X(001).
      *  Land - ISO-Laendercode (bspw. D fuer Deutschland)
           05 'XX'-LAND                         PIC  X(003).
           05 'XX'-LAND-KZN                     PIC  X(001).
      *  Kooperationsservice
      *    Teilnahme am vereinfachten Abrechnungsverfahren J oder N
           05 'XX'-ABRECHSERVICE                PIC  X(001).
           05 'XX'-ABRECHSERVICE-KZN            PIC  X(001).
      *    Rahmenvertrag Zusatzversicherung J oder N
           05 'XX'-RAHMENZUSATZVERS             PIC  X(001).
           05 'XX'-RAHMENZUSATZVERS-KZN         PIC  X(001).
      *  Vorversicherung Gesellschaft (ueber TBAT0039 mit KTY 4469)
           05 'XX'-VORVERSGES                   PIC  9(005).
           05 'XX'-VORVERSGES-KZN               PIC  X(001).
      *  Vorversicherung Datum zu dem gekuendigt wurde (JJJJMMTT)
           05 'XX'-VORVERSDATKUEND              PIC  X(008).
           05 'XX'-VORVERSDATKUEND-KZN          PIC  X(001).
      *  Vorversicherung Kuendigungsbestaetigung J oder N
           05 'XX'-VORVERSKUENDKZ               PIC  X(001).
           05 'XX'-VORVERSKUENDKZ-KZN           PIC  X(001).
      *  Datum zu dem die Maklervollmacht beim PG vorliegt (JJJJMMTT)
           05 'XX'-MAKLERVOLLMDAT               PIC  X(008).
           05 'XX'-MAKLERVOLLMDAT-KZN           PIC  X(001).
      *  Änderungskennzeichen für mitversicherte Personen J oder N
      *  bei Verarbeitungskennzeichen U = Update
           05 'XX'-AEND-KZN-MVP                 PIC  X(001).
      *  Anzahl versicherte Personen (Tabellenelemente)
           05 'XX'-ANZVERSPERS                  PIC  9(002).
      *  Ab hier max. 30 mitversicherte Personen
           05 'XX'-MITVERSTAB                   OCCURS 30.
      *  laufende Nummer der versicherten Person
              10 'XX'-LFDNR-VP                  PIC  9(002).
      *  Nachname der versicherten Person
              10 'XX'-NACHNAME-VP               PIC  X(030).
      *  Vorname der versicherten Person
              10 'XX'-VORNAME-VP                PIC  X(030).
      *  Geburtsdatum der versicherten Person im Format JHJJMMTT
              10 'XX'-GEBDAT-VP                 PIC  X(008).
      *  Anredeschluessel
      *    1 = Herr
      *    2 = Frau
      *    3 = Firma
      *    9 = keine Angabe
              10 'XX'-ANREDE-VP                 PIC  9(001).
      *  Verschluesselung des Geschlechts
      *    1 = maennlich
      *    2 = weiblich
      *    3 = unbekannt
              10 'XX'-GESCHLECHT-VP             PIC  9(001).
      *  Versicherungsart P (auf Personenebene, mitversichert im
      *                      Rahmen der Familienversicheung J oder N)
              10 'XX'-VERSICHERUNGSART-VP       PIC  X(001).
      *  Beginndatum der Mitversicherung im Format JHJJMMTT
              10 'XX'-BEGDAT-VP                 PIC  X(008).
      *  Endedatum der Mitversicherung im Format JHJJMMTT
              10 'XX'-ENDDAT-VP                 PIC  X(008).
      *  Kooperationsservice
      *    Teilnahme an der vereinfachten Abrechnung Ja/Nein
              10 'XX'-ABRECHSERVICE-VP          PIC  X(001).
      *    Rahmenvertrag Zusatzversicherung J oder N
              10 'XX'-RAHMENZUSATZVERS-VP       PIC  X(001).
      *    Loeschkennzeichen auf Ebene mitversicherte Person
      *      S = Storno (bspw. fehlerhafte Bearbeitung)
      *      D = sonstige Loeschung
              10 'XX'-LOESCHKZ-VP               PIC  X(001).
 

