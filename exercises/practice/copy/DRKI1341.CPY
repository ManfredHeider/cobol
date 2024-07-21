      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1341.CPY  $
      *    $Revision:   2.10  $
      *
      *    $Log:   L:/mf/CPY/DRKI1341.CPv  $
      *
      *   Rev 2.10   Aug 08 2013 07:44:36   t13ex15
      *AKI01228: OnlineKI+HOST: Datenfelder wegen Liquidation OIF AGI
      *
      *   Rev 2.9   Jul 11 2011 16:15:54   t13ex18
      *AKI01040: Erw. Deuba- inkl. DWS-Daten / Ausgabe in KI-Vertrag
      *
      *   Rev 2.9   Apr 15 2011 14:00:00   T13EX18
      *AKI00979: Neues Attribut Barauschüttung
      *
      *   Rev 2.8   Sep 10 2010 13:53:08   t13ex32
      *AKI00979: Neue Depotansicht  AGI-FondsdepotBank
      *
      *   Rev 2.7   Sep 09 2010 16:11:40   t13ex32
      *AKI00979: Neue Depotansicht  AGI-FondsdepotBank
      *
      *   Rev 2.6   Jun 29 2009 12:19:04   t13ex39
      *AKI00740: AllianzGI/FodB: Verarbeitung Unterkonten-Systematik 
      *
      *   Rev 2.5   Jun 23 2009 14:04:28   t13ex39
      *AKI00740: AllianzGI/FodB: Verarbeitung Unterkonten-Systematik 
      *
      *   Rev 2.4   Oct 16 2008 13:43:32   t13ex39
      *AKI00735: Bruttoperfomanceberechnung von AllianzGI in der KI
      *
      *   Rev 2.3   14 May 2007 15:50:10   t13ex43
      *AKI00610: Außendienst KI Depotausdruck
      *
      *   Rev 2.2   Oct 13 2004 15:25:50   t13ex48
      *AKI00386: Akt. DIT-Performance, kum. Ein- und Auszahlungen
      *
      *   Rev 2.1   Jul 10 2003 13:43:26   t13ex39
      *AKI00327.UE005 DB-PRODUKTE
      *
      *   Rev 2.0   Dec 14 2001 09:21:16   p13sp20
      *Archiverstellung
      *
      *----------------------------------------------------------------*
      *                                                                *
      *    COPY-MEMBER FÜR PERFORMANCE DATEI DEUTSCHE BANK + DIT       *
      *    -----------------------------------------------------       *
      *                                                                *
      *    DATEITYP  = QSAM                                            *
      *    Länge = 100 bytes                                           *
      *                                                                *
      *----------------------------------------------------------------*
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00162!14.11.02!01.2! NEU                       !WHO       *
      * AKI00386!17.09.04!01.2! Verarb. DIT Performance   !HSE       *
      *         !        !    ! Erweiterung um VERTRAGS-ID!          *
      * AKI00610!07.05.07!2.3 ! PERFORMANCE-DATUM HINZU   !KLEEMEIER *
      *---------!-----------------------------------------!----------*
      * AKI00735!19.08.08!2.4 ! NEUE STRUKTUR             !WHO       *
      *---------!-----------------------------------------!----------*
      * AKI00979 16.08.10!2.5 ! NEUE STRUKTUR             ! HBO      *
      *                         wegen Performanceberech.  !          *
      *---------!-----------------------------------------!----------*
      * AKI01040 15.04.11!2.6 ! Neues Attribut            ! MDI      *
      *         !        !    ! Barauschüttung            !          *
      *---------!-------------!---------------------------!----------*
      * AKI01228 02.08.13!2.9 ! Neues Attribut            ! SWZ      *
      *         !        !    ! Barauschüttung VJ         !          *
      *---------!-------------!---------------------------!----------*
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  DRKI1341.
      *
               20  BNK-SA                  PIC  X(01).
      *            SATZART        = '8'
               20  BNK-GES                 PIC  9(02).
      *            GESELLSCHAFT
               20  BNK-VNR                 PIC  9(13).

               20  BNK-GHP-LFDNR           PIC  9(04).
      *            VERTRAGSNUMMER MIT FÜHRENDEN NULLEN (DEPOTNUMMER)

               20  BNK-ANTEIL-LFDNR        PIC  9(02).
      *            LFDNR VON ANTEIL = VERTRAGSUNTERNUMMER

               20  BNK-ISIN                PIC  X(12).
      *            International Securities Identification Number

               20  BNK-WKN                       PIC  X(06).
      *            WERTPAPIERKENNNUMMER

               20  BNK-EINZ-KUM                  PIC  S9(11)V99.
      *            KUMULIERTE EINZAHLUNGEN

               20  BNK-AUSZ-KUM                  PIC  S9(11)V99.
      *            KUMULIERTE AUSZAHLUNGEN

               20  BNK-AUSS-KUM                  PIC  S9(11)V99.
      *            KUMULIERTE AUSschüttung

               20  BNK-AUSS-KUM-BIS-ENDE-VJ      PIC S9(011)V99.
      *            Kumulierte Auschüttungen seit Dep. Eroeffnung.
      *            bis Ende Vorjahr

               20  BNK-KUM-UEB                   PIC S9(011)V99.
      *            Kumulierte Überträge seit Dep. Eroeffnung.

               20  BNK-INV-KAPITAL-AKTUELL       PIC 9(011)V99.
      *            Inv Kapital seit Dep. Eroeffnung.

               20  BNK-INV-KAPITAL-VORJAHR       PIC 9(011)V99.
      *            Inv Kapital seit Dep. Eroeffnung. - 31.12. Vorjahr

               20  BNK-DURCH-EINSTANDSKURS       PIC 9(005)V99.
      *            DURCHSCHNITTLICHER Einstandskurs in Euro

               20  BNK-REAL-GW                   PIC S9(011)V99.
      *            Realisierter Gewinn seit Dep. Eroeffnung.

               20  BNK-GHP-VL-KZ                 PIC  X(01).

               20  BNK-VTG-ID                    PIC X(13).
      *            Vertrags-ID für Performance-Optimierung in Akt.-PGM
               20  BNK-PERFDAT                   PIC  9(08).

               20  BNK-KUM-BRUTTO-ERTRAGSGUTSCHR PIC 9(011)V99.
      *            kum. Bruttobetrag Ertragsausschüttung

               20  BNK-KUM-BRUTTO-BETRAG-VJ      PIC 9(011)V99.
      *            kum. Bruttobetrag Ertragsausschüttung VJ
      
               20  FILLER                        PIC  X(01).
      *        20  FILLER                        PIC  X(14).
      *        20  FILLER                        PIC  X(27).

      *            RESERVE

