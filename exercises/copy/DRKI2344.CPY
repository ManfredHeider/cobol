      *
      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  07.08.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2344.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRKI2344.CPv  $
      *
      *   Rev 2.5   Mar 09 2016 14:44:32   t13ex73
      *AAT00049: Fehlerverfahren mit Prozentkriterien - G48 PGMs
      *
      *   Rev 2.4   Nov 02 2009 10:19:56   t13ex39
      *AKI00844: DWS-Datenspiegel D E P O T
      *
      *   Rev 2.3   Oct 27 2009 12:38:08   t13ex39
      *AKI00844: DWS-Datenspiegel D E P O T
      *
      *   Rev 2.2   Oct 27 2009 12:04:52   t13ex39
      *AKI00844: DWS-Datenspiegel D E P O T
      *
      *   Rev 2.1   Aug 17 2009 16:48:10   t13ex39
      *AKI00844: DWS-Datenspiegel D E P O T
      *
      *   Rev 2.0   Aug 07 2009 11:05:12   t13ex39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *----------------------------------------------------------------*
      *                                                                *
      *    COPY-MEMBER FÜR ÄNDERUNGSBESTAND DWS DEPOTBESATND           *
      *    ----------------------------------------------------------  *
      *                                                                *
      *    Länge = 200 bytes                                           *
      *                                                                *
      *----------------------------------------------------------------*
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00844!01.08.08!01  ! NEU                       !W.Hornung *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  DRKI2344.
      *
      *
           10  'XX'-SATZ.
      *
               20  'XX'-SA                     PIC  X(002).
      *            SATZART        = '18'
               20  'XX'-GES                    PIC  9(002).
      *            GESELLSCHAFT   = '48'
               20  'XX'-KEY.
                   25  'XX'-VNR.
                       35  'XX'-FILIAL-NR      PIC 9(03).
                       35  'XX'-KDNR           PIC 9(07).
                       35  'XX'-UNTER-DEP-NR   PIC 9(02).
                   25  FILLER   REDEFINES 'XX'-VNR.
                       35  'XX'-VNR-1          PIC 9(10).
                       35  FILLER             PIC X(02).
                   25  'XX'-PORTFOLIO-NR       PIC X(10).
      *            PORTFOLIONMUMMER
      *            VERTRAGSNUMMER MIT FÜHRENDEN NULLEN (DEPOTNUMMER)
                   25  'XX'-WP-KENN-NR         PIC X(06).
      *            WERTPAPIERKENN-NUMMER
               20  'XX'-WP-KENN-ISIN           PIC X(12).
      *            WERTPAPIERKENN-NUMMER ISIN
               20  'XX'-ART                    PIC 9(01).
      *            ART DES PAPIERS
      *            1 = STÜCKE
      *            2 = NOMINALWERTE
               20  'XX'-STUECKE-ODER-WERT      PIC  9(12)V9999.
      *            STÜCKANZAHL ODER NOMINALWERT IN ORIGINALWÄHRUNG
               20  'XX'-ORG-WKZ                PIC  X(03).
      *            ORIGINAL-WÄHRUNG DES PAPIERES
               20  'XX'-KURSWERT-EUR           PIC  9(10)V99.
      *            KURSWERT IN EUR
               20  'XX'-KURSWERT-ORG-WG        PIC  9(12)V99.
      *            KURSWERT IN ORG-WÄHRUNG
               20  'XX'-KUM-EIN                PIC  9(11)V99.
      *            Kumulierte Einzahlung
               20  'XX'-KUM-AUS                PIC  9(11)V99.
      *            Kumulierte Auszahlung
      *        20  'XX'-KUM-AUT-WIEDER-ANLAGE  PIC  9(11)V99.
      *            Kumulierte automatische Wiederanlage
               20  'XX'-DEL-KZ                 PIC  X(01).
      *            Lösch-Kennzeichen
               20  FILLER                     PIC  X(83).
      *            RESERVE
 
           10  'XX'-VORSATZ REDEFINES 'XX'-SATZ.
      *
               20  'XX'-V-SA           PIC X(002).
      *            SATZART = '00'
               20  'XX'-V-ABSENDER     PIC X(008).
      *            ABSENDER = 'DWS  '
               20  'XX'-V-DATUM        PIC 9(008).
      *            BUCHUNGSDATUM = 'JJJJMMTT'
               20  'XX'-V-EMPFAENGER   PIC X(004).
      *            EMPFAENGER = 'DVAG'
               20  'XX'-V-TEXT         PIC X(008).
      *            INHALTSANGABE:
      *            DEPOTBESTAND     TÄGLICH   ŸBESTANDT'
      *            DEPOTBESTAND     KOMPLETT  ŸBESTANDK'
               20  'XX'-V-WKZ          PIC X(003).
      *            Währungs-Kennz. =  'EUR'
               20  FILLER             PIC X(167).
      *            RESERVE
 
      *
           10  'XX'-NACHSATZ REDEFINES 'XX'-SATZ.
      *
               20  'XX'-N-SA           PIC X(002).
      *            SATZART             '99'
               20  'XX'-N-ANZ          PIC 9(007).
      *            ANZAHL SAETZE INCL. VOR- UND NACHSATZ
               20  FILLER              PIC X(191).
      *            RESERVE
 
 

