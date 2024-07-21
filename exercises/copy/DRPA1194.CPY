      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1194.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA1194.CPv  $
      *
      *   Rev 2.0   Mar 25 1999 14:10:48   T13AE18
      *APA00012 Einführung EVS-Umbuchungs-
      *dialog
      *
      *   Rev 1.0   21 Oct 1998 14:05:10   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1194 FUNKTION UPDATE VORVERTRAGSDATEN    *
      *                                                              *
      *                          FüR VORVERTRAGSDATENMODUL UGPA0003  *
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
      *    FÜR DIE FUNKTION DRPA1194                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1194.
             10 FILLER                         PIC  X(001).

       01 PO-DRPA1194.
          05 PO-DRPA1194-RC                    PIC  X(003).
             88 PO-DRPA1194-RC-OK                       VALUE 'OK '.
             88 PO-DRPA1194-RC-UNZULAESSIG              VALUE 'UNZ'.

          05 PO-DRPA1194-MELDUNG.
             10 PO-DRPA1194-MELDUNG-TYP        PIC  X(001).
             10 PO-DRPA1194-MELDUNG-NR         PIC S9(004) BINARY.
      *
      ******** Ende Copy-Member **************************************
