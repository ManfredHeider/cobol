      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drvb0360.cpy  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\drvb0360.cpv  $
      *
      *   Rev 2.0   Mar 09 1999 15:48:14   T13AE35
      *AAS00096 neu
      *
      *   Rev 1.0   22 Jan 1999 13:40:48   P13SP20
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVB0360  FUNKTION ZU ZWVB0051               *
      *                                                              *
      *    --------------------------------------------------------- *
      *    ERSTELLT: WALB                                            *
      *    DATUM:    19.02.1999                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00096!19.02.99! 01 ! NEUERSTELLUNG             ! WALB     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0360                                  *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRVB0360.
          05 PI-DRVB0360-DATEN.
             10 PI-DRVB0360-JUR-ST-NR        PIC S9(009)
                                             BINARY.
      *
       01 PO-DRVB0360.
          05 PO-DRVB0360-RC                  PIC  X(003).
             88 PO-DRVB0360-RC-OK            VALUE 'OK '.
             88 PO-DRVB0360-RC-NOT-FOUND     VALUE 'NFD'.
          05 PO-DRVB0360-DATEN.
             10 PO-DRVB0360-JUR-PROV-STZ-GP  PIC S9(003)V99.
             10 PO-DRVB0360-JUR-PROZ-STZ     PIC S9(003)V99.
      *
      ******** Ende Copy-Member **************************************
