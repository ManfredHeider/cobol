      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 2002
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2002 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0622.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/cpy/DRVB0622.CPv  $
      *
      *   Rev 2.1   Dec 17 2002 12:25:54   t13ae30
      *AVB00643 - ENDGÜLTIGE VERSION
      *
      *   Rev 2.0   Dec 04 2002 12:20:52   t13ex15
      *Archiverstellung
      *
      ****************************************************************
      *    COPY-MEMBER: DRVB0622                                     *
      *    FUNKTION KAPITÄNS-CLUB, ADMIRALS-CLUB MITGLIEDER ERMITTELN*
      *                                                              *
      *    --------------------------------------------------------- *
      *    ERSTELLT: WILLIG                                          *
      *    DATUM:    09.12.2002                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00643!09.12.02! 01 ! NEUERSTELLUNG             ! WILLIG   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0622                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRVB0622.
          05 PI-DRVB0622-PROGRAMM             PIC  X(008).
          05 PI-DRVB0622-UFKT-CODE            PIC  9(002).
      *
       01 PO-DRVB0622.
          05 PO-DRVB0622-RC                   PIC  X(003).
             88 PO-DRVB0622-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0622-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0622-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0622-RC-ERR                       VALUE 'ERR'.

          05 PO-DRVB0622-DATEN.
             10 PO-DRVB0622-MA-ST-NR          PIC S9(009)  COMP.
             10 PO-DRVB0622-MAST-DAT-VON      PIC  X(026).
             10 PO-DRVB0622-KTY-MAST-TYP      PIC S9(004) USAGE COMP.
             10 PO-DRVB0622-KNR-MAST-TYP      PIC S9(004) USAGE COMP.
             10 PO-DRVB0622-MAST-LAST-UPDATE  PIC  X(026).
             10 PO-DRVB0622-MAST-DAT-BIS      PIC  X(010).
      *
      ******** Ende Copy-Member DRVB0622 *****************************
