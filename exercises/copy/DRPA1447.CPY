      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1447  Funktion DELETE Standard-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Brenner                                         *
      *    DATUM:    05.01.2001                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00118!05.01.01! 01 ! NEUERSTELLUNG             ! Brenner  *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1447                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  DELETE  <<<       TABELLE      TBPA0133 PMPPH        *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1447.
          05 PI-DRPA1447-PMPPH-LAST-UPDATE     PIC X(026).
          05 PI-DRPA1447-PMPP-KEY.
             10 PI-DRPA1447-PMPP-ID           PIC S9(015) COMP-3.
             10 PI-DRPA1447-PMPG-ID           PIC S9(015) COMP-3.
             10 PI-DRAP0326-PGEB-GES          PIC S9(004) COMP-3.
             10 PI-DRPA1447-PROD-PROD-SCHL    PIC S9(004) COMP-3.
      *
       01 PO-DRPA1447.
          05 PO-DRPA1447-RC                   PIC  X(003).
             88 PO-DRPA1447-RC-OK                         VALUE 'OK '.
             88 PO-DRPA1447-RC-CHANGED                    VALUE 'CHA'.
             88 PO-DRPA1447-RC-DUPLICATE-VALUE            VALUE 'DVA'.
      *
      ****** Ende Copy-Member ****************************************
