      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0310.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0310.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:58:50   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   15 Sep 1998 17:23:18   T13AE35
      *ASL00022 - Neuerstellung
      *
      *   Rev 1.0   09 Jul 1998 09:57:28   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVB0310  FUNKTION FüR ZWVB0005 RFT          *
      *                                                              *
      *    --------------------------------------------------------- *
      *    ERSTELLT: WALB                                            *
      *    DATUM:    04.08.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ASL00022!04.08.98! 01 ! NEUERSTELLUNG             ! WALB     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0310                                  *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRVB0310.
          05 PI-DRVB0310-DATEN.
             10 PI-DRVB0310-RFT-ST-NR        PIC S9(009)  COMP.
      *
       01 PO-DRVB0310.
          05 PO-DRVB0310-RC                  PIC  X(003).
             88 PO-DRVB0310-RC-OK            VALUE 'OK '.
             88 PO-DRVB0310-RC-NOT-FOUND     VALUE 'NFD'.
          05 PO-DRVB0310-DATEN.
             10 PO-DRVB0310-RFT-DAT-VON      PIC X(10).
             10 PO-DRVB0310-RFT-DAT-BIS      PIC X(10).
             10 PO-DRVB0310-NI-RFT-DAT-BIS   PIC S9(004) COMP.
      *
      ******** Ende Copy-Member **************************************
