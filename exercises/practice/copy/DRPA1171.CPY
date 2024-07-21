      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1171.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA1171.CPv  $
      *
      *   Rev 2.1   May 25 1999 14:26:24   T13AE18
      *apa00012
      *
      *   Rev 2.0   Mar 25 1999 14:10:24   T13AE18
      *APA00012 Einführung EVS-Umbuchungs-
      *dialog
      *
      *   Rev 1.0   21 Oct 1998 14:03:06   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1171 FUNKTION SET AUSGANGS-PE-STRUKTUR   *
      *                                                              *
      *                          FüR VORVERTRAGSDATENMODUL UGPA0003  *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    24.02.1999                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!24.02.99! 01  !NEU                       !BOHLINGER *
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1171                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1171.
             10 PI-DRPA1171-PRINTE             PIC S9(015)
                                               PACKED-DECIMAL.
             10 PI-DRPA1171-PEST-ID            PIC S9(015)
                                               PACKED-DECIMAL.

       01 PO-DRPA1171.
          05 PO-DRPA1171-RC                    PIC  X(003).
             88 PO-DRPA1171-RC-OK                       VALUE 'OK '.

      ******** Ende Copy-Member **************************************
