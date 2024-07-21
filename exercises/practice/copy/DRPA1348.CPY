      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1348.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\DRPA1348.CPv  $
      *
      *   Rev 2.2   Mar 22 2000 14:11:06   T13AE18
      *APA00067
      *
      *   Rev 2.1   Mar 16 2000 16:20:34   T13EX13
      *APA00067 Anpassung an Badenia +
      *Umstellung PGDF -> PGKF
      *
      *   Rev 2.0   Mar 01 2000 09:57:58   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1348 FUNKTION CECK FELDER                *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: WILKENS                                         *
      *    DATUM:    14.03.2000                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00067!14.03.00!1.00!NEU                        !WILKENS   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1348                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1348.
             10 PI-DRPA1348-PGEB-GES           PIC S9(004) BINARY.
             10 PI-DRPA1348-KNR-FELD           OCCURS 30.
                15 PI-DRPA1348-KNR-NPM-FELD-NAME   PIC S9(004) BINARY.

       01 PO-DRPA1348.
          05 PO-DRPA1348-RC                    PIC  X(003).
             88 PO-DRPA1348-RC-OK                       VALUE 'OK '.

          05 PO-DRPA1348-NPM-FELD-TAB          OCCURS 30.
            10 PO-DRPA1348-KNR-NPM-FELD-NAME   PIC  S9(004) BINARY.
            10 PO-DRPA1348-KNR-NPM-FELD-ZUL    PIC  S9(004) BINARY.
            10 PO-DRPA1348-KONF-Z-INFO         PIC  S9(004) BINARY.
      ******** ENDE COPY-MEMBER **************************************
