      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  20.11.2020
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1297.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1297.CPv  $
      *
      *   Rev 2.1   Jan 07 2021 11:50:02   t13ae56
      *AVB01644: PGM - KS2021: Prüfen Bef.-Bed. 2020 in VB22
      *
      *   Rev 2.0   Nov 20 2020 14:26:52   t13ae56
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1297                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FUER DAS LESE-OZP ZWVB0047                   *
      *    MIT DEN DATEN, DIE EINMAL PRO FUNKTION UEBERGEBEN WERDEN   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    27.11.2020                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * VLA-532 !27.11.20!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHAELT DIE INFORMATIONEN, DIE EINMAL         *
      *    PRO FUNKTION UEBERGEBEN WERDEN --> PI/PO-BEREICH           *
      *                                                               *
      *    FUNKTION DRVB1297 - ERMITTELN SATZ AUS TABELLE BVSA (TUPEL)*
      *                                                               *
      *****************************************************************
 
       01 PI-DRVB1297.
          05 PI-DRVB1297-DATEN.
             10 PI-DRVB1297-VS-ZEITRAUM        PIC S9(04)       BINARY.
             10 PI-DRVB1297-VB-NR              PIC S9(09)       BINARY.
 
       01 PO-DRVB1297.
         03 PO-DRVB1297-RC                     PIC  X(03).
             88 PO-DRVB1297-RC-OK                   VALUE 'OK '.
             88 PO-DRVB1297-RC-NOT-FOUND            VALUE 'NFD'.
         03 PO-DRVB1297-DATEN.
          05 PO-DRVB1297-NI-VIVBA047.
             10 NI-DRVB1297-ELE                PIC S9(04)       COMP
                                               OCCURS 57 TIMES.
          05 PO-DRVB1297-DCL-VIVBA047.
             10 PO-DRVB1297-TUPEL-GESAMT       PIC  X(315).
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB1297   --------------------------*
      *---------------------------------------------------------------*

