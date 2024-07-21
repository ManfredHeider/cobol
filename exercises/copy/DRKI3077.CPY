      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  14.07.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3077.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI3077.CPv  $
      *
      *   Rev 2.3   Mar 01 2017 11:52:26   t13ac14
      *AKI01466: G51 - BKK Nachtrag wegen DRKI3077
      *
      *   Rev 2.2   Dec 19 2016 15:51:16   t13ac20
      *AKI01466: maschinelle DTA-Kontrolle
      *
      *   Rev 2.1   Dec 07 2016 12:38:04   t13ac14
      *AKI01466: maschinelle DTA-Kontrolle
      *
      *   Rev 2.0   Jul 14 2016 16:21:08   t13ex78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *                                                               *
      *    Copy-Member für die Bestandsdatenuebermittling mit der     *
      *    BKK Linde - G51                                            *
      *                                                               *
      *---------------------------------------------------------------*
      *    LOGBUCH                                                    *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * AKI01452!13.09.16!2.1 ! Neu                       ! tfl       *
      * AKI01466!24.10.16!    ! Anpassungen               ! von Helden*
      * AKI01466!19.12.16!2.2 ! FELDER ENTFERNT:          ! SWO       *
      *         !        !    !   - RAHMENVERTRAG         !           *
      *         !        !    !   - ABRECHWIDERRUFDAT     !           *
      *         !        !    !   - ABRECHWIDERRUFDAT-VP  !           *
      * AKI01466!01.03.17!2.3 ! C-Satz nach vorne geholt  ! von Helden*
      * --------------------------------------------------------------*
      *         !        !    !                           !           *
      *****************************************************************
 
           05  'XX'-SATZ.
 
      *  'C' für C-Satz
               10  'XX'-SA                       PIC  X(001).
 
      *  Gesellschaftsnummer DVAG-intern - immer '51'
               10  'XX'-GES                      PIC  9(002).
 
      *  Vertragsnummer - macht ein Alphawert Probleme???
               10  'XX'-VERNR                    PIC  X(015).
 
      *  PIN aus dem Antrag
               10  'XX'-ANTRAGSPIN               PIC  9(010).
 
      *  Vertragszustand
      *    A = aktiv
      *    Z = zukuenftiger Vertrag
      *    D = abgelaufen/storniert
               10  'XX'-VERTRAGSZUSTAND          PIC  X(001).
 
      *  Versicherungsart V (Ebene Vertrag, freiwillig / pflichtvers.)
               10  'XX'-VERSICHERUNGSARTV        PIC  X(001).
 
      *  Beginndatum des Vertrages im Format JHJJMMTT
               10  'XX'-BEGDATVERTRAG            PIC  X(008).
 
      *  Endedatum des Vertrages im Format JHJJMMTT
               10  'XX'-ENDDATVERTRAG            PIC  X(008).
 
      *  VB-Nummer
               10  'XX'-VBNR                     PIC  9(007).
 
      *  Nachname des Mitgliedes
               10  'XX'-NACHNAME                 PIC  X(030).
 
      *  Vorname des Mitgliedes
               10  'XX'-VORNAME                  PIC  X(030).
 
      *  Geburtsdatum des Mitgliedes im Format JJJMMTT
               10  'XX'-GEBDAT                   PIC  X(008).
 
      *  Anredeschlüssel
      *    1 = Herr
      *    2 = Frau
      *    4 = Eheleute
      *    5 = Firma
      *    9 = keine Angabe
               10  'XX'-ANREDE                   PIC  9(001).
 
      *  Verschluesselung des Geschlechts
      *    1 = maennlich
      *    2 = weiblich
      *    3 = unbekannt
               10  'XX'-GESCHLECHT               PIC  9(001).
 
      *  Straße und Hausnummer
               10  'XX'-STRHNR                   PIC  X(040).
 
      *  Postleitzahl
               10  'XX'-PLZ                      PIC  X(009).
 
      *  Wohnort
               10  'XX'-ORT                      PIC  X(030).
 
      *  Land - ISO-Laendercode (bspw. D fuer Deutschland)
               10  'XX'-LAND                     PIC  X(003).
 
      *  Kooperationsservice
      *    Teilnahme am vereinfachten Abrechnungsverfahren J oder N
               10  'XX'-ABRECHSERVICE            PIC  X(001).
 
      *    Rahmenvertrag Zusatzversicherung J oder N
               10  'XX'-RAHMENZUSATZVERS         PIC  X(001).
 
      *  Vorversicherung Gesellschaft (ueber TBAT0039 mit KTY 4469)
               10  'XX'-VORVERSGES               PIC  9(005).
 
      *  Vorversicherung Datum zu dem gekuendigt wurde (JJJJMMTT)
               10  'XX'-VORVERSDATKUEND          PIC  X(008).
 
      *  Vorversicherung Kuendigungsbestaetigung J oder N
               10  'XX'-VORVERSKUENDKZ           PIC  X(001).
 
      *  Datum zu dem die Maklervollmacht beim PG vorliegt (JJJJMMTT)
               10  'XX'-MAKLERVOLLMDAT           PIC  X(008).
 
      *  Reserve fuer Vertrag
               10  FILLER                        PIC  X(100).
 
      *  Anzahl versicherte Personen (Tabellenelemente)
               10  'XX'-ANZVERSPERS              PIC  9(002).
 
      *  Ab hier max. 30 mitversicherte Personen
               10  'XX'-MITVERSTAB               OCCURS 30.
 
      *  laufende Nummer der versicherten Person
                   15  'XX'-LFDNR-VP             PIC  9(002).
 
      *  Nachname der versicherten Person
                   15  'XX'-NACHNAME-VP          PIC  X(030).
 
      *  Vorname der versicherten Person
                   15  'XX'-VORNAME-VP           PIC  X(030).
 
      *  Geburtsdatum der versicherten Person im Format JHJJMMTT
                   15  'XX'-GEBDAT-VP            PIC  X(008).
 
      *  Anredeschluessel
      *    1 = Herr
      *    2 = Frau
      *    3 = Firma
      *    9 = keine Angabe
                   15  'XX'-ANREDE-VP            PIC  9(001).
 
      *  Verschluesselung des Geschlechts
      *    1 = maennlich
      *    2 = weiblich
      *    3 = unbekannt
                   15  'XX'-GESCHLECHT-VP        PIC  9(001).
 
      *  Versicherungsart P (auf Personenebene, mitversichert im
      *                      Rahmen der Familienversicheung J oder N)
                   15  'XX'-VERSICHERUNGSART-VP  PIC  X(001).
 
      *  Beginndatum der Mitversicherung im Format JHJJMMTT
                   15  'XX'-BEGDAT-VP            PIC  X(008).
 
      *  Endedatum der Mitversicherung im Format JHJJMMTT
                   15  'XX'-ENDDAT-VP            PIC  X(008).
 
      *  Kooperationsservice
      *    Teilnahme an der vereinfachten Abrechnung Ja/Nein
                   15  'XX'-ABRECHSERVICE-VP     PIC  X(001).
 
      *    Rahmenvertrag Zusatzversicherung J oder N
                   15  'XX'-RAHMENZUSATZVERS-VP  PIC  X(001).
 
      *    Loeschkennzeichen auf Ebene mitversicherte Person
      *      S = Storno (bspw. fehlerhafte Bearbeitung)
      *      D = sonstige Loeschung
                   15  'XX'-LOESCHKZ-VP          PIC  X(001).
 
      *  Reserve fuer versicherte Person
                   15 FILLER                     PIC  X(050).
 
 
           05  'XX'-VORSATZ REDEFINES  'XX'-SATZ.
 
      *  'A' fuer Vorsatz (A-Satz)
               10  'XX'-V-SA                     PIC  X(001).
 
      *  Text, welcher den Absender des DTA identifiziert (BKKLINDE)
               10  'XX'-V-ABSENDER               PIC  X(008).
 
      *  Datum der Erstellung des DTA im Format JHJJMMTT
               10  'XX'-V-DATUM                  PIC  9(008).
 
      *  je nach Mandant 'DVAG' oder 'ALLF'
               10  'XX'-V-EMPFAENGER             PIC  X(004).
 
      *  'K' fuer Komplettbestand; 'D' für Deltalieferung
               10  'XX'-LIEFERUNG                PIC  X(001).
 
 
           05  'XX'-NACHSATZ REDEFINES  'XX'-VORSATZ.
 
      *  'E' für Nachsatz (E-Satz)
               10  'XX'-N-SA                     PIC  X(001).
 
      *  Anzahl der gelieferten C-Sätze
               10  'XX'-N-ANZSATZ                PIC  9(008).
 
      *  Anzahl der gelieferten Personen
               10  'XX'-N-ANZPERS                PIC  9(008).
 
      *****************************************************************
      *---------------------------------------------------------------*

