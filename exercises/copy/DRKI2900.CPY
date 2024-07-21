      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  12.05.2014
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2014 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2900.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2900.CPv  $
      *
      *   Rev 2.1   Jun 24 2014 11:11:02   t13ex77
      *AKI01305: Deuba - Erweiterung OIF
      *
      *   Rev 2.0   May 12 2014 11:54:10   t13ex77
      *Archiverstellung
      *
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
      *                                                               *
      *    COPY-MEMBER FÜR DEUTSCHE BANK KSDS OIF-KUENDIGUNG          *
      *    --------------------------------------------------------   *
      *                                                               *
      *    Länge = 100 Bytes                                          *
      *                                                               *
      *---------------------------------------------------------------*
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * AKI01305!??.06.14! 01 ! Neu                       ! abn       *
      * --------------------------------------------------------------*
      *         !        !    !                           !           *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *01  'XX'-SATZ.
      *
           05  'XX'-SA                              PIC X(002).
      *        SATZART        = '22'
           05  'XX'-GES                             PIC 9(002).
      *        GESELLSCHAFT   = '48'
           05  'XX'-KEY.
               20  'XX'-VNR.
                   25  'XX'-FILIAL-NR               PIC 9(003).
                   25  'XX'-KDNR                    PIC 9(007).
                   25  'XX'-UNTER-DEP-NR            PIC 9(002).
                   25  'XX'-WKZ                     PIC 9(003).
               20  FILLER   REDEFINES 'XX'-VNR.
                   25  'XX'-VNR-1                   PIC 9(010).
                   25  FILLER                       PIC X(005).
      *            VERTRAGSNUMMER
               20  'XX'-ISIN                        PIC X(012).
      *            ISIN
               20  'XX'-DAT-VOR-RUECKZAHL           PIC 9(008).
      *            DATUM VORAUSSICHTLICHER RÜCKZAHLUNGSTERMIN
           05  'XX'-ANTEILE                         PIC 9(013)V9999.
      *        ANZAHL ANTEILE
           05  'XX'-WP-KENN-NR                      PIC X(006).
      *        WERTPAPIERKENNNUMMER
           05  'XX'-DEL-KZ                          PIC X(001).
               88 'XX'-DEL-LOESCH                       VALUE 'D'.
      *        KENNZEICHEN LÖSCHUNG
               88 'XX'-DS-SPERRE                        VALUE 'S'.
      *        KENNZEICHEN INFOSPERRE
           05  FILLER                               PIC X(037).
      *            RESERVE
