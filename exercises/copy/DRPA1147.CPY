      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1147.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA1147.CPv  $
      *
      *   Rev 2.1   May 25 1999 14:12:04   T13AE18
      *apa00012
      *
      *   Rev 2.0   Mar 25 1999 14:10:14   T13AE18
      *APA00012 Einführung EVS-Umbuchungs-
      *dialog
      *
      *   Rev 1.0   21 Oct 1998 14:00:38   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1147 FUNKTION CECK BEARBEITUNGSFUNKTIONEN*
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    11.12.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!11.12.98! 01 !NEU                        !BOHLINGER *
      *         !15.03.99!1.02!EINFüHRUNG DER BEARBEITUNGS!BOHLINGER *
      *         !        !    !FUNKTIONEN                 !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1147                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1147.
             10 PI-DRPA1147-PRINTE             PIC S9(015)
                                               PACKED-DECIMAL.
             10 PI-DRPA1147-BEARB-FKT-TAB      OCCURS 10.
                15 PI-DRPA1147-BEARB-FKT       PIC S9(004) BINARY.

       01 PO-DRPA1147.
          05 PO-DRPA1147-RC                    PIC  X(003).
             88 PO-DRPA1147-RC-OK                       VALUE 'OK '.

          05 PO-DRPA1147-MELDUNG.
             10 PO-DRPA1147-MELDUNG-TYP        PIC  X(001).
             10 PO-DRPA1147-MELDUNG-NR         PIC S9(004) BINARY.

          05 PO-DRPA1147-BEARB-FKT-TAB         OCCURS 10.
            10 PO-DRPA1147-BEARB-FKT           PIC  S9(004) BINARY.
            10 PO-DRPA1147-S-BEARB-FKT-ZULAES  PIC   X(001).

      *
      ******** Ende Copy-Member **************************************
