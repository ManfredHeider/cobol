      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 2002
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2002 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0176.CPY  $
      *    $Revision:   2.12  $
      *
      *    $Log:   L:/mf/CPY/DRKI0176.CPv  $
      *
      *   Rev 2.12   Aug 19 2013 12:59:18   t13ex15
      *AKI01228: OnlineKI+HOST: Datenfelder wegen Liquidation OIF A
      *GI DRKI0176 Recomile
      *
      *   Rev 2.11   Aug 19 2013 12:55:14   t13ex15
      *AKI01228: OnlineKI+HOST: Datenfelder wegen Liquidation OIF A
      *GI DRKI0176 Recomile
      *
      *   Rev 2.10   Aug 08 2013 07:44:30   t13ex15
      *AKI01228: OnlineKI+HOST: Datenfelder wegen Liquidation OIF AGI
      *
      *   Rev 2.9   Jul 11 2011 16:15:42   t13ex18
      *AKI01040: Erw. Deuba- inkl. DWS-Daten / Ausgabe in KI-Vertrag
      *
      *   Rev 2.9   Apr 15 2011 14:00:00   T13EX18
      *AKI01040: Neues Attribut: Barauschüttung

      *   Rev 2.8   Sep 10 2010 13:53:00   t13ex32
      *AKI00979: Neue Depotansicht  AGI-FondsdepotBank
      *
      *   Rev 2.7   Sep 09 2010 16:11:32   t13ex32
      *AKI00979: Neue Depotansicht  AGI-FondsdepotBank
      *
      *   Rev 2.6   Jun 29 2009 12:18:32   t13ex39
      *AKI00740: AllianzGI/FodB: Verarbeitung Unterkonten-Systematik 
      *
      *   Rev 2.5   Jun 23 2009 10:33:24   t13ex39
      *AKI00740: AllianzGI/FodB: Verarbeitung Unterkonten-Systematik 
      *
      *   Rev 2.4   Jan 26 2009 09:32:00   t13ex39
      *AKI00735: Bruttoperfomanceberechnung von AllianzGI in der KI
      *
      *   Rev 2.3   Oct 16 2008 13:44:34   t13ex39
      *AKI00735: Bruttoperfomanceberechnung von AllianzGI in der KI
      *
      *   Rev 2.2   Oct 16 2008 13:18:46   t13ex39
      *AKI00735: Bruttoperfomanceberechnung von AllianzGI in der KI
      *
      *   Rev 2.1   Oct 13 2004 15:25:42   t13ex48
      *AKI00386: Akt. KSDS-Performance, kum. Ein- und Auszahlungen
      *
      *   Rev 2.0   Nov 14 2002 15:22:36   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *              #####################                             *
      *     ########    D R K I 0 1 7 6    ########                    *
      *              #####################                             *
      *                                                                *
      *  Satzaufbau für Komplettbestand KSDS
      *  Performance (in %) und kumulierte Ein- und Auszahlungen       *
      *                                                                *
      *    DATEITYP  = KSDS                                            *
      *    SATZLÄNGE   200                                             *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                         !NAME     !VERS.*
      *AUFTRAG !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *28.06.04!NEUERSTELLUNG                          !Sebbach  ! 01  *
      *19.08.04!2 ZUSAETZLICHE PERFORMANCE-FELDER      !         !     *
      *AKI00386!                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *19.08.08! NEUE STRUKTUR                         !WHO      ! 02  *
      *AKI00735!                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *06.08.19! NEUE STRUKTUR wegen Performanceberech.!HBO      ! 03  *
      *AKI00979!                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *15.04.11! Neues Attribut Barausschüttung        !MDI      ! 04  *
      *AKI01040!                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *02.08.13! NEUES ATTRIBUT Barausschüttung OIF    !SWZ      ! 06  *
      *AKI01228!                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *19.08.13! Filler berichtigt                     !SWZ      ! 07  *
      *AKI01228!                                       !         !     *
      *----------------------------------------------------------------*
       01  DRKI0176.
               20  KSDS-SA                     PIC  X(001).
      *            SATZART        = 'C'
               20  KSDS-GES                    PIC  9(002).
      *            GESELLSCHAFT   = '40'
               20 KSDS-PERF-KEY.
                25 KSDS-VNR.
                    30  KSDS-DEPOT-NR        PIC  X(013).
      *              DEPOT-NUMMER MIT FÜHRENDEN NULLEN (DEPOTNUMMER)
                    30  KSDS-DEPOT-UNTER-NR  PIC  X(002).
      *              UNTER-DEPOT-NUMMER
                25 KSDS-UNTER-KTO-ART        PIC  X(005).
      *             UNTER-KONTO-ART
                25 KSDS-ISIN                 PIC  X(012).
      *            Internationale WERTPAPIERKENN-NUMMER
               20  KSDS-WP-KENN-NR           PIC  X(006).
      *            WERTPAPIERKENN-NUMMER
               20  KSDS-EINZ-KUM             PIC 9(011)V99.
      *            KUMULIERTE EINZAHLUNGEN SEIT DEPOTERÖFFNUNG
               20  KSDS-AUSZ-KUM             PIC 9(011)V99.
      *            KUMULIERTE AUSZAHLUNGEN SEIT DEPOTERÖFFNUNG
               20  KSDS-AUSS-KUM             PIC 9(011)V99.
      *            KUMULIERTE AUSSCHÜTTUNGEN SEIT DEPOTERÖFFNUNG
060810         20  KSDS-AUSS-KUM-BIS-ENDE-VJ PIC 9(011)V99.
>>    *            KUMULIERTE AUSSCHÜTTUNGEN SEIT DEPOTERÖFFNUNG
      *            bis Ende Vorjahr
               20  KSDS-VORZEICHEN-KUM-UEB   PIC  X(001).
      *            VORZEICHEN + o. - der Kum Überträge seit Dep. Eroef.
               20  KSDS-KUM-UEB              PIC 9(011)V99.
      *            Kumulierte Überträge seit Dep. Eroeffnung.
               20  KSDS-INV-KAPITAL-AKTUELL  PIC 9(011)V99.
      *            Inv Kapital seit Dep. Eroeffnung.
               20  KSDS-INV-KAPITAL-VORJAHR  PIC 9(011)V99.
      *            Inv Kapital seit Dep. Eroeffnung. - 31.12. Vorjahr
               20  KSDS-DURCH-EINSTANDSKURS  PIC 9(005)V99.
      *            DURCHSCHNITTLICHER Einstandskurs in Euro
               20  KSDS-VORZEICHEN-REAL-GW   PIC  X(001).
      *            VORZEICHEN + o. - des realisierten Gewinns
               20  KSDS-REAL-GW              PIC 9(011)V99.
<<    *            Realisierter Gewinn seit Dep. Eroeffnung.
150411         20  KSDS-KUM-BRUTTO-ERTRAGSGUTSCHR  PIC 9(011)V99.
150411*            kum. Bruttobetrag Ertragsausschüttungg
020813         20  KSDS-KUM-BRUTTO-BETRAG-VJ PIC 9(011)V99.
      *            kum. Bruttobetrag Ertragsausschüttung VJ
020813*
190813         20  FILLER                    PIC X(020).
020813*        20  FILLER                    PIC X(007).
150411*        20  FILLER                    PIC X(033).
060810*        20  FILLER                    PIC X(046).

      ******************************************************************
