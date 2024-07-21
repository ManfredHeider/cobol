      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2003
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0665.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0665.CPv  $
      *
      *   Rev 2.1   Aug 14 2003 14:10:32   t13ae30
      *AVB00681 - Club "Bes.Leist. im AS-Club"
      *
      *   Rev 2.0   Feb 17 2003 10:58:34   p13sp16
      *Archiverstellung
      *
      ****************************************************************
      *    COPY-MEMBER: DRVB0665                                     *
      *    FUNKTION ERM.MITGLIEDER "BES.LEISTUNGEN IM AS-CLUB"       *
      *                                                              *
      *    --------------------------------------------------------- *
      *    ERSTELLT: WILLIG                                          *
      *    DATUM:    17.07.2003                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00665!17.07.03! 01 ! NEUERSTELLUNG             ! WILLIG   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0665 DES ZJVB0014                    *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRVB0665.
          05 PI-DRVB0665-PROGRAMM             PIC  X(008).
          05 PI-DRVB0665-UFKT-CODE            PIC  9(002).
      *
       01 PO-DRVB0665.
          05 PO-DRVB0665-RC                   PIC  X(003).
             88 PO-DRVB0665-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0665-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0665-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0665-RC-ERR                       VALUE 'ERR'.

          05 PO-DRVB0665-DATEN.
             10 PO-DRVB0665-MA-ST-NR          PIC S9(009)  COMP.
             10 PO-DRVB0665-MAST-DAT-VON      PIC  X(026).
             10 PO-DRVB0665-KTY-MAST-TYP      PIC S9(004) USAGE COMP.
             10 PO-DRVB0665-KNR-MAST-TYP      PIC S9(004) USAGE COMP.
             10 PO-DRVB0665-MAST-LAST-UPDATE  PIC  X(026).
             10 PO-DRVB0665-MAST-DAT-BIS      PIC  X(010).
      *
      ******** Ende Copy-Member DRVB0665 *****************************
