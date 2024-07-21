      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Mai 2002
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2002 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0605.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/cpy/DRVB0605.CPv  $
      *
      *   Rev 2.1   Nov 15 2002 16:57:24   t13ae30
      *AVB00643 - Kapitäns-Club
      *
      *   Rev 2.0   Nov 05 2002 11:35:08   p13sp16
      *Archiverstellung
      *
      ****************************************************************
      *    COPY-MEMBER: DRVB0605                                     *
      *    FUNKTION KAPITÄNS-CLUB, KANDIDATEN ERMITTELN              *
      *                                                              *
      *    --------------------------------------------------------- *
      *    ERSTELLT: WILLIG                                          *
      *    DATUM:    06.11.2002                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00643!06.11.02! 01 ! NEUERSTELLUNG             ! WILLIG   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0605                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRVB0605.
          05 PI-DRVB0605-PROGRAMM             PIC  X(008).
          05 PI-DRVB0605-UFKT-CODE            PIC  9(002).
      *
       01 PO-DRVB0605.
          05 PO-DRVB0605-RC                   PIC  X(003).
             88 PO-DRVB0605-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0605-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0605-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0605-RC-ERR                       VALUE 'ERR'.

          05 PO-DRVB0605-DATEN.
             10 PO-DRVB0605-MA-ST-NR          PIC S9(009)  COMP.
             10 PO-DRVB0605-MAST-DAT-VON      PIC  X(026).
             10 PO-DRVB0605-KTY-MAST-TYP      PIC S9(004) USAGE COMP.
             10 PO-DRVB0605-KNR-MAST-TYP      PIC S9(004) USAGE COMP.
             10 PO-DRVB0605-MAST-LAST-UPDATE  PIC  X(026).
             10 PO-DRVB0605-MAST-DAT-BIS      PIC  X(010).
      *
      ******** Ende Copy-Member DRVB0605 *****************************
