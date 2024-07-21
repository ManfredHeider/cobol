      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  24.06.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3689.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3689.CPv  $
      *
      *   Rev 2.1   Aug 28 2019 11:27:28   t13ex78
      *AKI01692: Programme, Copies
      *
      *   Rev 2.0   Jun 24 2019 14:41:56   t13ex78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *****************************************************************
      *                                                               *
      *    COPY-MEMBER FUER DAS AUFBEREITUNGSPROGRAMM BAUFI-DATEN     *
      *                                                               *
      *    SATZLAENGE 200 Bytes                                       *
      *---------------------------------------------------------------*
      *****************************************************************
      *    VERSION
      *****************************************************************
      *  AENDERUNGEN:                                                 *
      *  AUFTRAG !  DATUM   ! VER ! AENDERUNG              ! NAME     *
      * ------------------------------------------------------------- *
      * AKI01692 ! 27.08.19 !  01 ! Neuerstellung          ! tfl      *
      * ------------------------------------------------------------- *
      *
       01 'XX'-AKTUAL-SATZ.
           05  'XX'-AKTUAL-KZ           PIC  X(001) VALUE SPACE.
           05  'XX'-GES                 PIC  9(002) VALUE ZERO.
           05  'XX'-VNR                 PIC  9(015) VALUE ZERO.
           05  'XX'-PRODUKT-ID          PIC S9(004) VALUE ZERO.
           05  'XX'-BUCHDAT             PIC  X(010) VALUE SPACE.
           05  'XX'-UMSATZ              PIC S9(007)V9(002) VALUE ZERO.
           05  'XX'-STUECKZAHL          PIC  9(005) VALUE ZERO.
           05  'XX'-PRODBEZ             PIC  X(030) VALUE SPACE.
           05  'XX'-METALL              PIC  X(001) VALUE SPACE.
           05  'XX'-GRAMM               PIC  9(010)V9(002) VALUE ZERO.
           05  'XX'-EMAIL               PIC  X(040) VALUE SPACE.
           05  'XX'-RABATT              PIC  X(001) VALUE SPACE.
           05  FILLER                   PIC  X(070).


