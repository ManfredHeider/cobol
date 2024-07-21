      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drvb0359.cpy  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\drvb0359.cpv  $
      *
      *   Rev 2.0   Mar 09 1999 15:47:16   T13AE35
      *AAS00096 neu
      *
      *   Rev 1.0   22 Jan 1999 13:40:44   P13SP20
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVB0359  FUNKTION ZU ZWVB0040               *
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
      *    FÜR DIE FUNKTION DRVB0359                                  *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRVB0359.
          05 PI-DRVB0359-DATEN.
             10 PI-DRVB0359-KL-ST-NR         PIC S9(009)
                                             BINARY.
      *
       01 PO-DRVB0359.
          05 PO-DRVB0359-RC                  PIC  X(003).
             88 PO-DRVB0359-RC-OK            VALUE 'OK '.
             88 PO-DRVB0359-RC-NOT-FOUND     VALUE 'NFD'.
          05 PO-DRVB0359-DATEN.
             10 PO-DRVB0359-KL-PROV-STZ-GP   PIC S9(003)V99.
             10 PO-DRVB0359-KL-PROZ-STZ      PIC S9(003)V99.
      *
      ******** Ende Copy-Member **************************************
