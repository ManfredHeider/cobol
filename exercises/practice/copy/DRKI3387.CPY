      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  22.08.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3387.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI3387.CPv  $
      *
      *   Rev 2.2   Nov 01 2018 09:42:28   t13ae52
      *AKI01628: Depottransaktionen - DTA / Copys
      *
      *   Rev 2.1   Oct 30 2018 11:22:44   t13ae52
      *AKI01628: Depottransaktionen - DTA / Copys
      *
      *   Rev 2.0   Aug 22 2018 15:22:58   t13ex39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *                                                                *
      *    COPY-MEMBER FÜR DEPOTRANSAKTIONEN VON GENERALI INVEST       *
      *                                                                *
      *    SATZLÄNGE   100                                             *
      *                                                                *
      *    LOGBUCH                                                     *
      *--------!-------------------------------------------!-----------*
      *AUFTRAG !DATUM   !TEXT                              !NAME       *
      *--------!-------------------------------------------!-----------*
      *AKI01615!22.08.18! NEU                              !WHO        *
      *        !        !                                  !           *
      *        !        !                                  !           *
      *----------------------------------------------------------------*
      * ENDE-LOGBUCH                                                   *
      ******************************************************************
      *
CCC009*    SKIP1
       05  DRKI3387.
 
      ***********************************************************
      *                                                         *
      *                 VORSATZ                                 *
      *                                                         *
      ***********************************************************
 
           10  'XX'-VORSATZ.
      *
               15  'XX'-V-SA              PIC  X(001).
      *            SATZART = 'A'
               15  'XX'-V-ABSENDER        PIC  X(027).
      *            ABSENDER = 'GEI'
               15  'XX'-V-DATUM           PIC  9(008).
      *            DATUM = 'JJJJMMTT' ERSTELLUNG
               15  'XX'-V-DATUM-STAND     PIC  9(008).
      *            DATUM = 'JJJJMMTT' STAND
               15  'XX'-V-EMPFAENGER      PIC  X(027).
      *            EMPFAENGER = 'ALAG'
               15  'XX'-V-TEXT            PIC  X(020).
      *            'UMSATZDATEN'
               15  'XX'-V-ART             PIC  X(001).
      *            D FÜR DELTA
               15  FILLER                 PIC  X(008).
      *            RESERVE
 
           10  'XX'-SATZ REDEFINES 'XX'-VORSATZ.
      *
               20  'XX'-SA                PIC  X(001).
      *            SATZART        = 'C'
               20  'XX'-GES               PIC  9(002).
      *            GESELLSCHAFT   = '42'
               20  'XX'-VNR               PIC  9(015).
      *            VERTRAGSNUMMER MIT FÜHRENDEN NULLEN (DEPOTNUMMER)
               20  'XX'-WP-KENN-NR        PIC  X(006).
      *            WERTPAPIERKENN-NUMMER
               20  'XX'-WP-KENN-ISIN      PIC  X(012).
      *            WERTPAPIERKENN-NUMMER ISIN
               20  'XX'-STUECKE-ODER-WERT PIC  9(12)V9999.
      *            STÜCKANZAHL ODER NOMINALWERT IN ORIGINALWÄHRUNG
               20  'XX'-STUECKE-ODER-WERT-R
                   REDEFINES 'XX'-STUECKE-ODER-WERT PIC X(16).
               20  'XX'-BUCH-BETRAG       PIC  9(010)V99.
      *            BETRAG LETZTE BUCHUNG; BRUTTOUMSATZ
               20  'XX'-BUCH-BETRAG-R REDEFINES 'XX'-BUCH-BETRAG
                                          PIC  X(012).
               20  'XX'-BUCH-DAT          PIC  9(008).
      *            BUCHUNGSDATUM; SCHLUSSTAG  IM FORMAT JJJJMMTT
               20  'XX'-BUCH-SCHL         PIC  X(003).
      *            BUCHUNGSSCHLÜSSEL; ART DES GESCHÄFTS
               20  'XX'-ZASZ-BETRAG       PIC  9(009)V99.
      *            BETRAG ZINSABSCHALGSTEUER
               20  'XX'-ZASZ-BETRAG-R REDEFINES 'XX'-ZASZ-BETRAG
                                          PIC  X(011).
               20  'XX'-STORNO-KZ         PIC  X(001).
      *            STORNO-KENNZEICHEN = 1 FÜR STORNO
               20  FILLER                 PIC  X(013).
      *
           10  'XX'-NACHSATZ REDEFINES 'XX'-SATZ.
      *
               20  'XX'-N-SA              PIC X(001).
      *            SATZART             'E'
               20  'XX'-N-ANZ             PIC 9(007) COMP-3.
      *            ANZAHL SAETZE INCL. VOR- UND NACHSATZ
               20  FILLER                 PIC X(092).
      *            RESERVE
      *
      *
      *
      *
      *
      *
      *
      *
      *
      *
      *
      *
      *
      *
      *

