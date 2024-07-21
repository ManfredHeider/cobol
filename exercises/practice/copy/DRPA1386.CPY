      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1386.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA1386.CPv  $
      *
      *   Rev 2.1   22 May 2000 17:27:44   T13EX21
      *APA00090 - Neuerstellung
      *
      *   Rev 2.0   Apr 11 2000 16:58:02   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1386 FUNKTION                            *
      *                          CHECK DIALOGBEARBEITUNGSFUNKTION    *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STORCH                                          *
      *    DATUM:    19.05.2000                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00090!19.05.00!2.01! NEUERSTELLUNG             ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1386                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1386.
             10 PI-DRPA1386-PGEB-GES           PIC S9(004) BINARY.
             10 PI-DRPA1386-KNR-DIA-BEARB      OCCURS 20.
                15 PI-DRPA1386-KNR-DIA-BEARB-FKT   PIC S9(004) BINARY.

       01 PO-DRPA1386.
          05 PO-DRPA1386-RC                    PIC  X(003).
             88 PO-DRPA1386-RC-OK                       VALUE 'OK '.

          05 PO-DRPA1386-NPM-FELD-TAB          OCCURS 20.
            10 PO-DRPA1386-KNR-DIA-BEARB-FKT   PIC  S9(004) BINARY.
            10 PO-DRPA1386-KNR-DIA-FKT-ZUL     PIC  S9(004) BINARY.
            10 PO-DRPA1386-KONF-Z-INFO         PIC  S9(004) BINARY.
      *
      ******** ENDE COPY-MEMBER **************************************
