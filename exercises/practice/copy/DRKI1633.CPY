      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1633.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI1633.CPv  $
      *
      *   Rev 2.1   Feb 18 2004 16:08:40   t13ex39
      *AKI00391 Fonds aktualisieren
      *
      *   Rev 2.0   Dec 14 2001 09:21:16   p13sp20
      *Archiverstellung
      *
      *----------------------------------------------------------------*
      *                                                                *
      *    COPY-MEMBER FÜR DEPOTTRANSAKTION KOMPLETTBESTAND DB24       *
      *    ----------------------------------------------------------  *
      *                                                                *
      *    DATEITYP  = QSAM                                            *
      *    Länge = 160 bytes                                           *
      *                                                                *
      *----------------------------------------------------------------*
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00391!08.02.04!01  ! NEU                       !W.Hornung *
      * -------------------------------------------------------------*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  DRKI1633.
           10  IT-VORSATZ.
      *
               20  IT-V-SA            PIC X(001).
      *            SATZART = 'A'
               20  IT-V-ABSENDER      PIC X(008).
      *            ABSENDER = 'ITFUTURE'
               20  IT-V-DATUM-VON     PIC 9(008).
      *            Anfang des Zeitraums
               20  IT-V-DATUM-BIS     PIC 9(008).
      *            Ende des Zeitraums
               20  IT-V-EMPFAENGER    PIC X(004).
      *            EMPFAENGER = 'DVAG'
               20  IT-V-TEXT          PIC X(008).
      *            INHALTSANGABE: 'FONDS'
               20  FILLER             PIC X(063).
      *            RESERVE

      *
           10  IT-SATZ     REDEFINES      IT-VORSATZ.
      *
               20  IT-SA                  PIC  X(001).
      *            SATZART        = 'C'
               20  IT-WP-KENN-NR          PIC  X(06).
      *            WERTPAPIERKENN-NUMMER
               20  IT-WP-KENN-ISIN        PIC  X(12).
      *            WERTPAPIERKENN-NUMMER ISIN
               20  IT-GES                 PIC  9(03).
      *            Partnergesellschaft
               20  IT-DVAG-PROD-SCHL      PIC  9(02).
      *            DVAG-Produktschl.
               20  IT-VL-KZ               PIC  X(01).
      *            VL-Kennz.
               20  IT-BEZ                 PIC  X(40).
      *            Bezeichnung
               20  IT-TYP                 PIC  X(20).
      *            TYP
               20  FILLER                  PIC X(15).

      *
           10  IT-NACHSATZ    REDEFINES     IT-VORSATZ.
      *
               20  IT-N-SA             PIC X(001).
      *            SATZART             'E'
               20  IT-N-ANZ            PIC 9(007).
      *            ANZAHL SAETZE INCL. VOR- UND NACHSATZ
               20  IT-N-ANZ-GEF        PIC 9(007).
      *            ANZAHL SAETZE GEFUNDEN
               20  IT-N-ANZ-NICHT-GEF  PIC 9(007).
      *            ANZAHL SAETZE NICHT GEFUNDEN
               20  FILLER              PIC X(078).
      *            RESERVE
