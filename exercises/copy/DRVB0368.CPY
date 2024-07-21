      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0368.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0368.CPv  $
      *
      *   Rev 2.0   May 18 1999 10:46:26   T13AE42
      *NT-Version
      *
      *   Rev 1.1   May 17 1999 16:03:00   T13AE42
      *AAS00117 - Delta-Rueckkonvertierung
      *
      *   Rev 1.0   22 Jan 1999 13:41:10   P13SP20
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVB0368                                     *
      *    --------------------------------------------------------- *
      *    ERSTELLT: SCHMIDT, B.                                     *
      *    DATUM:    22.02.1999                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00117!22.02.99! 01 ! NEUERSTELLUNG             ! SCHMIDT  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0368                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRVB0368.
          05 PI-DRVB0368-DATEN.
             10 PI-DRVB0368-ED-ST-NR         PIC S9(009)
                                             BINARY.
      *
       01 PO-DRVB0368.
          05 PO-DRVB0368-RC                  PIC  X(003).
             88 PO-DRVB0368-RC-OK            VALUE 'OK '.
             88 PO-DRVB0368-RC-NFD           VALUE 'NFD'.
          05 PO-DRVB0368-DATEN.
             10 PO-DRVB0368-EDT-DAT-BIS      PIC  X(010).
             10 PO-DRVB0368-NI-EDT-DAT-BIS   PIC S9(004) COMP.
      *
      ******** Ende Copy-Member **************************************
