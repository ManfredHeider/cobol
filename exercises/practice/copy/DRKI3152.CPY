      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  31.05.2017
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3152.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI3152.CPv  $
      *
      *   Rev 2.4   Sep 23 2017 17:49:52   t13ex78
      *AKI01500: Teleson Ablauf
      *
      *   Rev 2.3   Sep 22 2017 09:33:04   t13ex78
      *AKI01500: Teleson Ablauf
      *
      *   Rev 2.2   Jun 23 2017 15:19:18   t13ex23
      *AKI01500: Teleson Ablauf - Korrektur
      *
      *   Rev 2.1   Jun 21 2017 08:16:12   t13ex23
      *AKI01500: Teleson Ablauf Nachbesserung
      *
      *   Rev 2.0   May 31 2017 15:17:58   t13ex23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *                                                               *
      *    Copy-Member für die Bestandsdatenuebermittling mit der     *
      *    TeleSon - G94                                              *
      *                                                               *
      *---------------------------------------------------------------*
      *    LOGBUCH                                                    *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * AKI01500!31.05.17!1.0 ! Neu                       ! MKE       *
      * --------------------------------------------------------------*
      * AKI01500!23.06.17!1.1 ! Korrektur                 ! MKE       *
      * --------------------------------------------------------------*
      * AKI01500!22.09.17!    ! nochmalige Korrektur      ! tfl       *
      * --------------------------------------------------------------*
      * AKI01500!23.09.17!    ! Fehler w/ pers. Blödh.    ! tfl       *
      *         !        !    !                           !           *
      *****************************************************************
           05  'XX'-SATZ.
 
      *  'C' für C-Satz
               10  'XX'-SA                       PIC  X(001).
 
      *  Gesellschaftsnummer DVAG-intern - immer '94'
               10  'XX'-GES                      PIC  9(002).
 
      *  Vertragsnummer
               10  'XX'-VERNR                    PIC  X(015).
 
      *  Vertragszustand
      *    A = aktiv
      *    Z = zukuenftiger Vertrag
               10  'XX'-VERTRAGSZUSTAND          PIC  X(001).
 
      *  Vertragsart
      *    G = Gas
      *    S = Strom
               10  'XX'-VERTRAGSART              PIC  X(001).
 
      *  Versorger/Anbieter
               10  'XX'-VERSORGER                PIC  X(020).
 
      *  Vermittelter Tarif/Produkt
               10  'XX'-TARIF                    PIC  X(050).
 
220917*  Produktart-ID
220917         10  'XX'-PRODUKTID                PIC  X(004).
 
      *  Beginndatum des Vertrages im Format JHJJMMTT
               10  'XX'-BEGDATVERTRAG            PIC  X(008).
 
      *  Endedatum des Vertrages im Format JHJJMMTT
               10  'XX'-ENDDATVERTRAG            PIC  X(008).
 
      *  VB-Nummer
               10  'XX'-VBNR                     PIC  9(007).
 
      *  Zahlbetrag
               10  'XX'-ZAHLBETRAG               PIC  9(009)V99.
 
      *  Zahlungsweise
               10  'XX'-ZAHLUNGSWEISE            PIC  X(002).
 
      *  Nachname des Kunden
               10  'XX'-NACHNAME                 PIC  X(030).
 
      *  Vorname des Kunden
               10  'XX'-VORNAME                  PIC  X(030).
 
      *  Geburtsdatum des Kunden im Format JJJMMTT
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
      *  Filler
               10  FILLER                        PIC  X(019).
 
           05  'XX'-VORSATZ REDEFINES  'XX'-SATZ.
 
      *  'A' fuer Vorsatz (A-Satz)
               10  'XX'-V-SA                     PIC  X(001).
 
      *  Text, welcher den Absender des DTA identifiziert (TELESON)
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
      *****************************************************************
      *---------------------------------------------------------------*
 
 

