      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  25.11.2020
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1302.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1302.CPv  $
      *
      *   Rev 2.1   Jan 07 2021 11:52:24   t13ae56
      *AVB01644: PGM - KS2021: Prüfen Bef.-Bed. 2020 in VB22
      *
      *   Rev 2.0   Nov 25 2020 16:03:40   t13ae56
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1302                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FUER DAS LESE-OZP ZWVB0010                   *
      *    MIT DEN DATEN, DIE EINMAL PRO FUNKTION UEBERGEBEN WERDEN   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    25.11.2020                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * VLA-532 !25.11.20!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHAELT DIE INFORMATIONEN, DIE EINMAL         *
      *    PRO FUNKTION UEBERGEBEN WERDEN --> PI/PO-BEREICH           *
      *                                                               *
      *    FUNKTION DRVB1302 - ERMITTELN SATZ AUS TABELLE VBP         *
      *                                                               *
      *****************************************************************
 
       01 PI-DRVB1302.
          05 PI-DRVB1302-DATEN.
             10 PI-DRVB1302-VBP-NR             PIC S9(04)       COMP.
 
       01 PO-DRVB1302.
         03 PO-DRVB1302-RC                     PIC  X(03).
             88 PO-DRVB1302-RC-OK                   VALUE 'OK '.
             88 PO-DRVB1302-RC-NOT-FOUND            VALUE 'NFD'.
         03 PO-DRVB1302-DATEN.
          05 PO-DRVB1302-NI-VIVB0010.
             10 NI-DRVB1302-ELE                PIC S9(04)       COMP
                                               OCCURS 11 TIMES.
          05 PO-DRVB1302-DCL-VIVB0010.
             10 PO-DRVB1302-TUPEL-GESAMT       PIC  X(084).
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB1302   --------------------------*
      *---------------------------------------------------------------*

