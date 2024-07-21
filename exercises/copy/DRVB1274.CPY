      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  26.11.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1274.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1274.CPv  $
      *
      *   Rev 2.1   Jan 17 2020 10:12:04   t13ae56
      *AVB01581: PGM - Paket 2 - Dialoge VB31 + VB49
      *
      *   Rev 2.0   Nov 26 2019 13:18:06   t13ae56
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1274                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FUER DAS UPRO UPVB0199                       *
      *    MIT DEN DATEN, DIE EINMAL PRO FUNKTION UEBERGEBEN WERDEN   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    26.11.2019                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * VBL-1198!26.11.19!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHAELT DIE INFORMATIONEN, DIE EINMAL         *
      *    PRO FUNKTION UEBERGEBEN WERDEN --> PI/PO-BEREICH           *
      *                                                               *
      *    FUNKTION DRVB1274 - ERMITTELN FAP-VERSICHERUNGSSUMMEN      *
      *                                                               *
      *****************************************************************
 
       01 PI-DRVB1274.
          05 PI-DRVB1274-DATEN.
             10 PI-DRVB1274-NEU-KZ             PIC  X(01).
             10 PI-DRVB1274-FAP-STF-ALT        PIC  X(01).
             10 PI-DRVB1274-FAP-STF-NEU        PIC  X(01).
             10 PI-DRVB1274-DVAG-STF-ALT       PIC  X(01).
             10 PI-DRVB1274-DVAG-STF-NEU       PIC  X(01).
 
       01 PO-DRVB1274.
          05 PO-DRVB1274-RC                    PIC  X(03).
             88 PO-DRVB1274-RC-OK                   VALUE 'OK '.
             88 PO-DRVB1274-RC-NOK                  VALUE 'NOK'.
 
          05 PO-DRVB1274-DATEN.
             10 PO-DRVB1274-NEU-KZ             PIC  X(01).
             10 PO-DRVB1274-AMLVERS-SUM        PIC S9(09)V99 COMP-3.
             10 PO-DRVB1274-AMLUV-SUM          PIC S9(09)V99 COMP-3.
             10 PO-DRVB1274-DVAGVER-SUM        PIC S9(09)V99 COMP-3.
             10 PO-DRVB1274-DVAGUV-SUM         PIC S9(09)V99 COMP-3.
 
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB1274   --------------------------*
      *---------------------------------------------------------------*

