      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0144.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0144.CPv  $
      *
      *   Rev 2.0   May 05 1999 17:14:30   T13AE30
      *AAS00051 - Stammdaten-DTA ----> Aachen
      *
      *   Rev 1.0   Feb 19 1997 09:58:00   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVB0144  FUNKTION Rückstellungs-Satz für    *
      *                           Mitarbeiter ermitteln              *
      *    --------------------------------------------------------- *
      *    ERSTELLT: JÄGER-SOKOLUK                                   *
      *    DATUM:    06.08.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DAISY 1 !06.08.97! 01 ! NEUERSTELLUNG             ! JÄGER    *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0144                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRVB0144.
          05 PI-DRVB0144-PZVP-VERSION        PIC S9(004)  BINARY.
          05 PI-DRVB0144-DATEN.
             10 PI-DRVB0144-MA-ST-NR         PIC S9(009)
                                             BINARY.
      *
       01 PO-DRVB0144.
          05 PO-DRVB0144-RC                  PIC  X(003).
             88 PO-DRVB0144-RC-OK            VALUE 'OK '.
             88 PO-DRVB0144-RC-NFD           VALUE 'NFD'.
          05 PO-DRVB0144-DATEN.
             10 PO-DRVB0144-MA-RCK-STZ       PIC S9(003)V99
                                             PACKED-DECIMAL.
      *
      ******** Ende Copy-Member **************************************
