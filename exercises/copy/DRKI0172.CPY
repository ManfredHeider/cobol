      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 2002
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2002 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0172.CPY  $
      *    $Revision:   2.10  $
      *
      *    $Log:   L:/mf/CPY/DRKI0172.CPv  $
      *
      *   Rev 2.10   Aug 08 2013 07:44:20   t13ex15
      *AKI01228: OnlineKI+HOST: Datenfelder wegen Liquidation OIF AGI
      *
      *   Rev 2.9   Jul 11 2011 16:15:36   t13ex18
      *AKI01040: Erw. Deuba- inkl. DWS-Daten / Ausgabe in KI-Vertrag
      *
      *   Rev 2.9   Apr 15 2011 14:00:00   T13EX18
      *AKI01040: Neues Attribut Barauschüttung
      *
      *   Rev 2.8   Sep 10 2010 13:52:54   t13ex32
      *AKI00979: Neue Depotansicht  AGI-FondsdepotBank
      *
      *   Rev 2.7   Sep 09 2010 16:11:24   t13ex32
      *AKI00979: Neue Depotansicht  AGI-FondsdepotBank
      *
      *   Rev 2.6   Feb 23 2010 09:16:18   i13az25
      *AKI00779: Masch. DTA-Kontrolle neu auf DB2 - AGI-Performance
      *
      *   Rev 2.5   Jun 29 2009 12:18:26   t13ex39
      *AKI00740: AllianzGI/FodB: Verarbeitung Unterkonten-Systematik 
      *
      *   Rev 2.4   Jun 23 2009 10:33:16   t13ex39
      *AKI00740: AllianzGI/FodB: Verarbeitung Unterkonten-Systematik 
      *
      *   Rev 2.3   Oct 16 2008 13:43:22   t13ex39
      *AKI00735: Bruttoperfomanceberechnung von AllianzGI in der KI
      *
      *   Rev 2.2   Oct 16 2008 13:20:10   t13ex39
      *AKI00735: Bruttoperfomanceberechnung von AllianzGI in der KI
      *
      *   Rev 2.1   Oct 13 2004 15:25:24   t13ex48
      *AKI00386: Akt. AGI-Performance, kum. Ein- und Auszahlungen
      *
      *   Rev 2.0   Nov 14 2002 15:22:12   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *              #####################                             *
      *     ########    D R K I 0 1 7 2    ########                    *
      *              #####################                             *
      *                                                                *
      *  Datei von AGI beinhaltet:                                     *
      *    kumulierte Ein- und Auszahlungen                            *
      *    Performance in %                                            *
      *                                                                *
      *    SATZLÄNGE = 200 bytes                                       *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                         !NAME     !VERS.*
      *AUFTRAG !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *26.05.04!NEUERSTELLUNG                          !H.Sebbach! 01  *
      *19.08.04!2 ZUSAETZLICHE PERFORMANCE-FELDER      !         !     *
      *AKI00386!                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *19.08.08! NEUE STRUKTUR                         !WHO      ! 02  *
      *AKI00735!                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *05.08.09! NEUE STRUKTUR wegen Performanceberech.!HBO      ! 03  *
      *AKI00979!                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *15.04.11! NEUES ATTRIBUT Barausschüttung        !MDI      ! 04  *
      *AKI01040!                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *02.08.13! NEUES ATTRIBUT Barausschüttung OIF    !SWZ      ! 05  *
      *AKI01228!                                       !         !     *
      *----------------------------------------------------------------*




      ******************************************************************
       01  DRKI0172.
           10  AGI-VORSATZ.
      *
               20  AGI-V-SA            PIC X(001).
      *            SATZART = 'A'
               20  AGI-V-ABSENDER      PIC X(008).
      *            ABSENDER = 'AGI,HOF'
               20  AGI-V-DATUM         PIC 9(008).
      *            ERFASSUNGS-/AENDERUNGSDATUM = 'JJJJMMTT'
               20  AGI-V-EMPFAENGER    PIC X(005).
      *            EMPFAENGER = 'DVAG'
               20  AGI-V-TEXT          PIC X(010).
      *            INHALTSANGABE:
      *            Tägliche Lieferung                   'PERFDE'
      *            Komplettbestand                      'PERFDEKP'

               20  AGI-V-INHALT        PIC X(001).
      *            DATENSÄTZE ÜBERTRAGEN: JA = SPACE   NEIN = 'K'

               20  AGI-V-WKZ           PIC X(003).
      *            Währungs-Kennz. =  'EUR'
               20  FILLER              PIC X(164).
      *            RESERVE

      *
           10  AGI-SATZ     REDEFINES      AGI-VORSATZ.
      *
               20  AGI-SA                   PIC  X(001).
      *            SATZART        = 'C'
               20  AGI-GES                  PIC  9(002).
      *            GESELLSCHAFT   = '40'
               20  AGI-VNR.
                   25  AGI-DEPOT-NR         PIC  X(013).
      *            DEPOT-NUMMER MIT FÜHRENDEN NULLEN (DEPOTNUMMER)
                   25  AGI-DEPOT-UNTER-NR   PIC  X(002).
      *            UNTER-DEPOT-NUMMER
               20  AGI-UNTER-KTO-ART        PIC  X(005).
      *            UNTER-KONTO-ART
               20  AGI-ISIN                 PIC  X(012).
      *            Internationale WERTPAPIERKENN-NUMMER
               20  AGI-WP-KENN-NR           PIC  X(006).
      *            WERTPAPIERKENN-NUMMER
               20  AGI-EINZ-KUM             PIC 9(011)V99.
      *            KUMULIERTE EINZAHLUNGEN SEIT DEPOTERÖFFNUNG
               20  AGI-AUSZ-KUM             PIC 9(011)V99.
      *            KUMULIERTE AUSZAHLUNGEN SEIT DEPOTERÖFFNUNG
               20  AGI-AUSS-KUM             PIC 9(011)V99.
      *            KUMULIERTE AUSSCHÜTTUNGEN SEIT DEPOTERÖFFNUNG
050810         20  AGI-AUSS-KUM-BIS-ENDE-VJ PIC 9(011)V99.
>>    *            KUMULIERTE AUSSCHÜTTUNGEN SEIT DEPOTERÖFFNUNG
      *            Bis 31.12 Vorjahr
               20  AGI-VORZEICHEN-KUM-UEB   PIC  X(001).
      *            VORZEICHEN + o. - der Kum Überträge seit Dep. Eroef.
               20  AGI-KUM-UEB              PIC 9(011)V99.
      *            Kumulierte Überträge seit Dep. Eroeffnung.
               20  AGI-INV-KAPITAL-AKTUELL  PIC 9(011)V99.
      *            Inv Kapital seit Dep. Eroeffnung.
               20  AGI-INV-KAPITAL-VORJAHR  PIC 9(011)V99.
      *            Inv Kapital seit Dep. Eroeffnung. - 31.12. Vorjahr
               20  AGI-DURCH-EINSTANDSKURS  PIC 9(005)V99.
      *            DURCHSCHNITTLICHER Einstandskurs in Euro
               20  AGI-VORZEICHEN-REAL-GW   PIC  X(001).
      *            VORZEICHEN + o. - des realisierten Gewinns
               20  AGI-REAL-GW              PIC 9(011)V99.
<<    *            Realisierter Gewinn seit Dep. Eroeffnung.
150411         20  AGI-KUM-BRUTTO-ERTRAGSGUTSCHR PIC 9(011)V99.
150411*            kum. Bruttobetrag Ertragsausschüttung
020813         20  AGI-KUM-BRUTTO-BETRAG-VJ PIC 9(011)V99.
020813*            kum. Bruttobetrag Ertragsausschüttung VJ

020813         20  FILLER                   PIC X(007).
150411*        20  FILLER                   PIC X(033).
050810*        20  FILLER                   PIC X(046).

           10  AGI-NACHSATZ    REDEFINES     AGI-VORSATZ.
      *
               20  AGI-N-SA                 PIC X(001).
      *            SATZART             'E'
               20  AGI-N-ANZ                PIC 9(009).
      *            ANZAHL SAETZE OHNE VOR- UND NACHSATZ
               20  FILLER                   PIC X(190).
      *            RESERVE


      ******************************************************************
