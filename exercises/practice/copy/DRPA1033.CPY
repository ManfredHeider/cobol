      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1033.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA1033.CPv  $
      *
      *   Rev 2.1   Jan 18 2000 14:38:00   T13EX22
      *APA00071: SF44 mit TBPA0026-Zugriff
      *
      *   Rev 2.0   Feb 27 1999 14:29:58   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   14 Jul 1998 14:00:04   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1033  FUNKTION DELETE PGV EINTRAG        *
      *                           ZUORDNUNG FÜR UPPA0029             *
      *    -------------------------------------------------------   *
      *    ERSTELLT: MUND                                            *
      *    DATUM:    22.11.1999                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00071!22.11.99! 01 ! NEUERSTELLUNG             ! MUND     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1033                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  DELETE PGV <<<                                       *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRPA1033.
          05 PI-DRPA1033-PGVZ-LAST-UPDATE   PIC  X(026).
          05 PI-DRPA1033-PGVZ-KEY.
             10 PI-DRPA1033-PGEB-GES           PIC S9(004)  BINARY.
             10 PI-DRPA1033-PGVZ-PGV           PIC  X(008).
             10 PI-DRPA1033-PGVZ-VZ-POSITIV    PIC  X(001).
             10 PI-DRPA1033-PGVZ-GUELTIG-AB    PIC  X(010).

       01 PO-DRPA1033.
          05 PO-DRPA1033-RC                    PIC  X(003).
             88 PO-DRPA1033-RC-OK                        VALUE 'OK '.
             88 PO-DRPA1033-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRPA1033-MELDUNG.
             10 PO-DRPA1033-MELDUNG-TYP        PIC  X(001).
             10 PO-DRPA1033-MELDUNG-NR         PIC S9(004) BINARY.
      *
