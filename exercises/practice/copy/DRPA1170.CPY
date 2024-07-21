      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1170.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA1170.CPv  $
      *
      *   Rev 2.1   May 25 1999 14:26:20   T13AE18
      *apa00012
      *
      *   Rev 2.0   Mar 25 1999 14:10:24   T13AE18
      *APA00012 Einführung EVS-Umbuchungs-
      *dialog
      *
      *   Rev 1.0   21 Oct 1998 14:03:00   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1170 FUNKTION UPDATE VERTRAGSDATEN       *
      *                                                              *
      *                          FüR VERTRAGSDATENMODUL UGPA0001     *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    19.03.1999                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!19.03.99! 01  !NEU                       !BOHLINGER *
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1170                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1170.
             10 FILLER                         PIC  X(001).

       01 PO-DRPA1170.
          05 PO-DRPA1170-RC                    PIC  X(003).
             88 PO-DRPA1170-RC-OK                       VALUE 'OK '.
             88 PO-DRPA1170-RC-UNZULAESSIG              VALUE 'UNZ'.

          05 PO-DRPA1170-MELDUNG.
             10 PO-DRPA1170-MELDUNG-TYP        PIC  X(001).
             10 PO-DRPA1170-MELDUNG-NR         PIC S9(004) BINARY.
      *
      ******** Ende Copy-Member **************************************
