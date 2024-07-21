      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0148.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0148.CPv  $
      *
      *   Rev 2.0   May 18 1999 10:04:08   T13AE35
      *NT-Version
      *
      *   Rev 1.0   Feb 19 1997 09:58:26   P13PVCS
      *Initial revision.
      *
      *--------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVB0148  FUNKTION Rückstellungs-Satz für    *
      *                           ED ermitteln                       *
      *    --------------------------------------------------------- *
      *    ERSTELLT: JÄGER-SOKOLUK                                   *
      *    DATUM:    08.08.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DAISY 1 !08.08.97! 01 ! NEUERSTELLUNG             ! JÄGER    *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0148                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRVB0148.
          05 PI-DRVB0148-PZVP-VERSION        PIC S9(004)  BINARY.
          05 PI-DRVB0148-DATEN.
             10 PI-DRVB0148-ED-ST-NR         PIC S9(009)
                                             BINARY.
      *
       01 PO-DRVB0148.
          05 PO-DRVB0148-RC                  PIC  X(003).
             88 PO-DRVB0148-RC-OK            VALUE 'OK '.
             88 PO-DRVB0148-RC-NFD           VALUE 'NFD'.
          05 PO-DRVB0148-DATEN.
             10 PO-DRVB0148-ED-RCK-STZ       PIC S9(003)V99
                                             PACKED-DECIMAL.
      *
      ******** Ende Copy-Member **************************************
