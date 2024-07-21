      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1196.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1196.CPv  $
      *
      *   Rev 2.1   Sep 02 2019 09:28:46   t13ex67
      *APA02796: Neuer Dialog für Korrekturen PRO-RATA Faktoren
      *
      *   Rev 2.0   Mar 25 1999 14:10:54   T13AE18
      *APA00012 Einführung EVS-Umbuchungs-
      *dialog
      *
      *   Rev 1.0   21 Oct 1998 14:05:22   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1196 FUNKTION UPDATE DIALOGMODULE        *
      *                          KOORDINIEREN                        *
      *                          FüR DIALOGMODUL UGPA0004            *
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
      * APA02796!12.07.19!2.01 !SCHALTER, OB PVER-UPDATE  ! STORCH   *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1196                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1196.
             10 FILLER                         PIC  X(001).
             10 PI-DRPA1196-S-PVER-UPDATE      PIC  X(001).

       01 PO-DRPA1196.
          05 PO-DRPA1196-RC                    PIC  X(003).
             88 PO-DRPA1196-RC-OK                       VALUE 'OK '.
             88 PO-DRPA1196-RC-UNZULAESSIG              VALUE 'UNZ'.

          05 PO-DRPA1196-MELDUNG.
             10 PO-DRPA1196-MELDUNG-TYP        PIC  X(001).
             10 PO-DRPA1196-MELDUNG-NR         PIC S9(004) BINARY.
      *
      ******** Ende Copy-Member **************************************

