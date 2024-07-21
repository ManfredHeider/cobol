      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0365.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0365.CPv  $
      *
      *   Rev 2.0   May 18 1999 10:39:46   T13AE42
      *NT-Version
      *
      *   Rev 1.1   May 17 1999 16:02:02   T13AE42
      *AAS00117- Delta-Rueckkonvertierung
      *
      *   Rev 1.0   22 Jan 1999 13:41:00   P13SP20
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
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
      *    FÜR DIE FUNKTION DRVB0365                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRVB0365.
          05 PI-DRVB0365-DATEN.
             10 PI-DRVB0365-EDD-BEZUGS-NR     PIC S9(009) COMP.
      *
       01 PO-DRVB0365.
          05 PO-DRVB0365-RC                  PIC  X(003).
             88 PO-DRVB0365-RC-OK            VALUE 'OK '.
             88 PO-DRVB0365-RC-NFD           VALUE 'NFD'.
          05 PO-DRVB0365-DATEN.
             10 PO-DRVB0365-ED-ST-NR          PIC S9(009) COMP.
             10 PO-DRVB0365-EDD-LAST-UPDATE   PIC  X(026).
      *
      ******** ENDE COPY-MEMBER  DRVB0365 ****************************
