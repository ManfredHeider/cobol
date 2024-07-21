      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  06.09.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2780.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/mf/CPY/DRKI2780.CPv  $
      *
      *   Rev 2.7   Mar 27 2017 09:30:32   t13ae31
      *FKI01544: FVD Teil 1 alten Code entfernen COPY
      *
      *   Rev 2.6   Mar 17 2015 14:39:14   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank - Co
      *pies
      *
      *   Rev 2.5   Jan 13 2015 13:15:54   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank MDTA
      *
      *   Rev 2.4   Sep 26 2013 15:34:48   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.3   Sep 26 2013 15:12:02   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.2   Sep 18 2013 08:10:08   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.1   Sep 13 2013 09:19:44   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.0   Sep 06 2013 10:36:58   t13ex78
      *Archiverstellung
      ******************************************************************
      *              #####################                             *
      *     ########    D R K I 2 7 8 0    ########                    *
      *              #####################                             *
      *                                                                *
      *  COPY-MEMBER FUER BANK-DTA Vertragsdaten Baufi-Daten           *
      *                                                                *
      *    SATZLÄNGE   500 Bytes                                       *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                         !NAME     !VERS.*
      *AUFTRAG !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *06.09.13!NEUERSTELLUNG                          !T.Flierl ! 01  *
      *--------!---------------------------------------!---------!-----*
      *24.03.17! FVD entfernt                          ! HHU     !     *
      ******************************************************************
      *
      *01  DRKI2780.
 
           05  'XX'-VORSATZ.
               10  'XX'-V-SA            PIC  X(001).
      *
      *  A-SATZ
 
               10  'XX'-V-ABSENDER      PIC  X(008).
      *
      *  Text, welcher den Absender des DTA identifiziert
 
               10  'XX'-V-DATUM         PIC  9(008).
      *
      *  Datum der Erstellung des DTA im Format JHJJMMTT
 
               10  'XX'-V-EMPFAENGER    PIC  X(004).
      *
      *  je nach Mandant 'DVAG', 'ALLF'
 
               10  'XX'-V-TEXT          PIC  X(008).
      *
      *  Text, der den Inhalt des DTA identifiziert
      *  'KUNDEN  ', 'VERTRAG ' oder 'BAUFI   '
 
               10  'XX'-LIEFERUNG       PIC  X(001).
 
      *  'K' für Komplettbestand, 'D' für Deltalieferung
 
               10  FILLER               PIC  X(470).
 
 
 
           05  'XX'-SATZ REDEFINES 'XX'-VORSATZ.
               10  'XX'-SA              PIC  X(001).
      *
      *  C-SATZ
               10  'XX'-GES             PIC  9(002).
      *
      *  Gesellschaftsnummer DVAG-intern
 
               10  'XX'-VERNR           PIC  9(015).
               10  'XX'-LFDNR           PIC  9(002).
 
      *  Laufende Nummer
 
               10  'XX'-PRODDAT-ANF     PIC  9(008).
      *
      *  Datum der Produkteröffnung im Format JHJJMMTT
 
               10  'XX'-PRODDAT-END     PIC  9(008).
      *
      *  Datum der Produktschließung im Format JHJJMMTT
 
               10  'XX'-DLSUMME         PIC  9(012)V9(002).
      *
      *  Ursprungskreditbetrag / Darlehenssummer
 
               10  'XX'-DAT-ZINS-END    PIC  9(008).
      *
      *  Datum Ende Zinsfestschreibung im Format JHJJMMTT
 
               10  'XX'-DL-AKT          PIC  9(012)V9(002).
      *
      *  Aktueller Darlehensbetrag
 
               10  'XX'-TILG-ANF        PIC  9(008).
      *
      *  Datum Tilgungsbeginn bzw. sich daraus ergebender Zeitraum
      *  ohne Tilgung im Format JHJJMMTT
 
               10  'XX'-TILG-ART        PIC  X(001).
      *
      *  Tilgungsart
      *  'A' -> annuitätisch
      *  'E' -> endfällig
 
               10  'XX'-TILG-PROZ       PIC  9(002)V9(004).
      *
      *  Prozentsatz Tilgung
 
      *        10  'XX'-EFFZI           PIC  9(002)V9(004).
      *
      *  Effektivzins in %
 
               10  'XX'-NOMZI           PIC  9(002)V9(004).
      *
      *  Nominalzins in %
 
               10  'XX'-ZINS-MODUS      PIC  9(001).
      *
      *  Kennzeichen Zahlungsweise der Zinsen
      *  0 -> Zahlungsweise fehlt
      *  1 -> jährlich
      *  2 -> halbjährlich
      *  4 -> vierteljährlich
      *  5 -> 2-monatlich
      *  6 -> monatlich
      *  8 -> Einmalzahlung
 
               10  'XX'-RATE            PIC  9(012)V9(002).
      *
      *  Rate bzw. Annuität
 
               10  'XX'-DAT-ZUSAGE      PIC  9(008).
      *
      *  Datum der Kreditzusage im Format JHJJMMTT
               10  'XX'-DAT-AUSZAHL     PIC  9(008).
      *
      *  Datum der Kreditauszahlung im Format JHJJMMTT
 
               10  'XX'-DL-ART          PIC  X(001).
      *
      *  Darlehensart, beispielsweise sind dies
      *  'A' -> KfW-Darlehen
      *  'B' -> Zwischenfinanzierung
      *  'C' -> Forward-Darlehen
      *  'D' -> Immobiliendarlehen
 
               10  'XX'-IBAN            PIC  X(034).
               10  'XX'-BIC             PIC  X(011).
               10  'XX'-MANDAT-REF      PIC  X(035).
               10  'XX'-ABW-RZ          PIC  X(001).
      *
      *  Kennzeichen, ob abweichender Ratenzahler vorhanden
      *  'J' -> abweichender Ratenzahler vorhanden
      *  'N' -> kein abweichender Ratenzahler vorhanden
      *  Ist kein abweichender Ratenzahler verschlüsselt werden die
      *  folgenden Felder allesamt mit SPACES geliefert
 
               10  'XX'-ABW-NAME        PIC  X(030).
               10  'XX'-ABW-VORNAME     PIC  X(030).
               10  'XX'-ABW-GEBDAT      PIC  9(008).
               10  'XX'-ABW-ANREDE      PIC  9(001).
               10  'XX'-ABW-STRHNR      PIC  X(040).
               10  'XX'-ABW-PLZ         PIC  X(009).
               10  'XX'-ABW-ORT         PIC  X(030).
               10  'XX'-ABW-LAND        PIC  X(003).
      *
      *  ISO Ländercode
               10  FILLER               PIC  X(143).
 
 
           05  'XX'-NACHSATZ REDEFINES  'XX'-VORSATZ.
               10  'XX'-N-SA            PIC  X(001).
      *
      *  'E' für Nachsatz (E-Satz)
 
               10  'XX'-N-ANZ           PIC  9(008).
      *
      *  Anzahl der gelieferten C-Sätze
 
               10  FILLER               PIC  X(491).
 
      ******************************************************************
      *
      *---------------------------------------------------------------*
 
 

