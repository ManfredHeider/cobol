      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  13.11.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2701.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI2701.CPv  $
      *
      *   Rev 2.4   Mar 03 2015 12:10:22   t13ex39
      *FKI01484: Div.Änd bei Fonds und Preisen PMKI0723 / PMKI0573
      *
      *   Rev 2.3   Nov 19 2012 13:21:02   t13ex15
      *AKI01185: Rücknahmepreise Vereinheitlichung wegen 0-Preis
      *
      *   Rev 2.1   Nov 19 2012 12:52:34   t13ex15
      *AKI01185: Rücknahmepreise Vereinheitlichung wegen 0-Preis
      *
      *   Rev 2.0   Nov 13 2012 08:36:40   t13ex15
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *                                                                *
      *    COPY-MEMBER FÜR NEUE RUECKNAHMEPREISE Allgemein (DTA)       *
      *    ----------------------------------------------------------  *
      *                                                                *
      *    DATEINAME = KIDITPR                                         *
      *    LAENGE    = 40 BYTES
      *    LAENGE    = 80 BYTES
      *                                                                *
      *    DIE DATEI KOMMT PER ISDN-LEITUNG VON DIT TÄGLICH ZU UNS     *
      *    UND ENTHÄLT DIE RÜCKNAHMEPREISE DER EINZELNEN FONDS.        *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                          !NAME     !VER-*
      *AUFTRAG !                                        !         !SION*
      *--------!----------------------------------------!--------_!----*
      *13.11.12!NEUERSTELLUNG                           !SWZ      ! 01 *
      *AKI01185!                                        !         !    *
      *--------!--------!-------------------------------!----------*
280115*FKI01484!28.01.15! diverse Fehler bei Preisen +  !WHO       *
      *        !        ! Fonds                         !          *
      *        !        ! Endasatzzähler für DWS + ITFu-!          *
      *        !        ! ture verkleinert              !          *
      *--------!----------------------------------------!----------*
      *
CCC009*    SKIP1
       01  'XX'GABE-SATZ.
           10  'XX'-VORSATZ.
      *
               20  'XX'-V-SA            PIC X(001).
      *            SATZART = 'A'
               20  'XX'-V-ABSENDER      PIC X(008).
      *            ABSENDER = 'DWS    ' (symbolisch für alle)
               20  'XX'-V-DATUM         PIC 9(008).
      *            BUCHUNGSDATUM = 'JJJJMMTT'
               20  'XX'-V-EMPFAENGER    PIC X(004).
      *            EMPFAENGER = 'DVAG'
               20  'XX'-V-TEXT          PIC X(010).
      *            INHALTSANGABE = 'PREISE    '
               20  FILLER              PIC X(049).

      *
           10  'XX'-SATZ     REDEFINES      'XX'-VORSATZ.
      *
               20  'XX'-SA                  PIC  X(001).
      *            SATZART        = 'C'
               20  'XX'-GES                 PIC  9(003).
      *            GESELLSCHAFT
               20  'XX'-WP-KENN-NR          PIC  X(006).
      *            WERTPAPIERKENN-NUMMER
               20  'XX'-ISIN-NR             PIC  X(012).
      *            Internationale Wertpapierkennr.
               20  'XX'-RUECK-PREIS         PIC  9(007)V99.
      *            RUECKNAHMEPREIS
               20  'XX'-AUS-PREIS           PIC  9(007)V99.
      *            AUSGABEPREIS
               20  'XX'-WERT-ENTW           PIC S9(003)V99.
      *            WERTENTWICKLUNG
      *        20  'XX'-WERT-ENTW-VORZ      PIC  X(001).
      *            Vorzeichen WERTENTWICKLUNG
      *        20  'XX'-WERT-ENTW-DAT       PIC  9(008).
      *            DATUM WERTENTWICKLUNG
               20  'XX'-PREIS-DAT           PIC  9(008).
      *            DATUM der Preisen

               20  'XX'-WAEHRUNGS-KZ        PIC  X(003).
      *            Währungs-Kennz. = 'EUR'
               20  'XX'-WERT-ENTW-DAT       PIC  9(008).
      *            DATUM WERTENTWICKLUNG
               20  FILLER                  PIC  X(016).
      *            RESERVE

      *
           10  'XX'-NACHSATZ    REDEFINES     'XX'-VORSATZ.
      *
               20  'XX'-N-SA            PIC X(001).
      *            SATZART             'E'
               20  'XX'-N-ANZ           PIC 9(009).
               20  FILLER REDEFINES 'XX'-N-ANZ.
                 25  'XX'-N-ANZ-DWS-ITF PIC 9(007).
                 25  FILLER             PIC X(002).
      *            ANZAHL SAETZE INCL. VOR- UND NACHSATZ
               20  FILLER              PIC X(070).
      *            RESERVE


