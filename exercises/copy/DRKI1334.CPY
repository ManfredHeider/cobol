      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1334.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI1334.CPv  $
      *
      *   Rev 2.3   Jul 11 2011 16:15:48   t13ex18
      *AKI01040: Erw. Deuba- inkl. DWS-Daten / Ausgabe in KI-Vertrag
      *
      *   Rev 2.2   Nov 26 2002 14:55:54   t13ex39
      *AKI00315 KUM EIN/AUS
      *
      *   Rev 2.1   Jul 30 2002 16:33:46   t13ex39
      *AKI00274 DB/24
      *
      *   Rev 2.0   Dec 14 2001 09:21:16   p13sp20
      *Archiverstellung
      *
      *----------------------------------------------------------------*
      *                                                                *
      *    COPY-MEMBER FÜR ÄNDERUNGSBESTAND DWS DEPOTBESATND           *
      *    ----------------------------------------------------------  *
      *                                                                *
      *    Länge = 150 bytes                                           *
      *                                                                *
      *----------------------------------------------------------------*
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00274!        !01  ! NEU                       !W.Hornung *
      * -------------------------------------------------------------*
      * AKI00315!05.11.02!02  ! NEUE FELDER KUM EIN/AUS   !Hornung   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  DRKI1334.
      *
           10  DB-VORSATZ.
      *
               20  DB-V-SA            PIC X(001).
      *            SATZART = '1'
               20  DB-V-ABSENDER      PIC X(008).
      *            ABSENDER = 'DB AG'
               20  DB-V-DATUM         PIC 9(008).
      *            BUCHUNGSDATUM = 'JJJJMMTT'
               20  DB-V-EMPFAENGER    PIC X(004).
      *            EMPFAENGER = 'DVAG'
               20  DB-V-TEXT          PIC X(008).
      *            INHALTSANGABE:
      *            DEPOTBESTAND     TÄGLICH   ‘BESTANDT'
      *            DEPOTBESTAND     KOMPLETT  ‘BESTANDK'
               20  DB-V-WKZ           PIC X(003).
      *            Währungs-Kennz. =  'EUR'
               20  FILLER             PIC X(118).
      *            RESERVE

      *
      *
           10  DB-SATZ     REDEFINES      DB-VORSATZ.
      *
               20  DB-SA                  PIC  X(001).
      *            SATZART        = '5'
               20  DB-GES                 PIC  9(002).
      *            GESELLSCHAFT   = '48'
               20  DB-VNR.
                   25  DB-FILIAL-NR       PIC 9(03).
                   25  DB-KDNR            PIC 9(07).
                   25  DB-UNTER-DEP-NR    PIC 9(02).
                   25  DB-WKZ             PIC 9(03).
      *            VERTRAGSNUMMER MIT FÜHRENDEN NULLEN (DEPOTNUMMER)
               20  DB-DAT-DEPOT-EROEFF    PIC X(08).
      *            DATUM DEPOTERÖFFNUNG
               20  DB-DAT-DEPOT-SCHLIES   PIC X(08).
      *            DATUM DEPOTSCHLIEßUNG
               20  DB-RISIKOKLASSE        PIC X(01).
      *            RISIKOKLASSE
               20  DB-ANLAGEZIEL          PIC X(01).
      *            ANLAGEZIEL
               20  DB-WP-KENN-NR          PIC X(06).
      *            WERTPAPIERKENN-NUMMER
               20  DB-WP-KENN-ISIN        PIC X(12).
      *            WERTPAPIERKENN-NUMMER ISIN
               20  DB-ART                 PIC 9(01).
      *            ART DES PAPIERS
      *            1 = STÜCKE
      *            2 = NOMINALWERTE
               20  DB-STUECKE-ODER-WERT   PIC  9(12)V999.
      *            STÜCKANZAHL ODER NOMINALWERT IN ORIGINALWÄHRUNG
               20  DB-STUECKE-ODER-WERT-R REDEFINES DB-STUECKE-ODER-WERT
                                          PIC X(15).
               20  DB-ORG-WKZ             PIC  X(03).
      *            ORIGINAL-WÄHRUNG DES PAPIERES
               20  DB-KURSWERT-EUR        PIC  9(10)V99.
      *            KURSWERT IN EUR
               20  DB-KURSWERT-ORG-WG     PIC  9(12)V99.
      *            KURSWERT IN ORG-WÄHRUNG
               20  DB-KUM-EIN             PIC  9(11)V99.
      *            Kumulierte Einzahlung
               20  DB-KUM-AUS             PIC  9(11)V99.
      *            Kumulierte Auszahlung
               20  FILLER                 PIC  X(25).
      *            RESERVE

           10  DB-NACHSATZ    REDEFINES     DB-VORSATZ.
      *
               20  DB-N-SA            PIC X(001).
      *            SATZART             '9'
               20  DB-N-ANZ           PIC 9(007).
      *            ANZAHL SAETZE INCL. VOR- UND NACHSATZ
               20  FILLER              PIC X(142).
      *            RESERVE


