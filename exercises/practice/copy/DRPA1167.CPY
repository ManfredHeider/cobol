      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1167.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA1167.CPv  $
      *
      *   Rev 2.1   May 25 1999 14:19:02   T13AE18
      *apa00012
      *
      *   Rev 2.0   Mar 25 1999 14:10:18   T13AE18
      *APA00012 Einführung EVS-Umbuchungs-
      *dialog
      *
      *   Rev 1.0   21 Oct 1998 14:02:44   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1167 FUNKTION set ee-entscheidung        *
      *                          FüR PRODUKTTEIL                     *
      *                          FüR PROVISIONSDATENMODUL UGPA0001   *
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
      *    FÜR DIE FUNKTION DRPA1167                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1167.
             10 PI-DRPA1167-PVPT-ID            PIC S9(015)
                                               PACKED-DECIMAL.
             10 PI-DRPA1167-PVPT-VERSION       PIC S9(004)   BINARY.
             10 PI-DRPA1167-EE-ENTSCHEIDUNG    PIC  X(001).
             10 PI-DRPA1167-KNR-EVS-GRUND      PIC S9(004)   BINARY.
             10 PI-DRPA1167-PVPT-PIN           PIC S9(009)   BINARY.

       01 PO-DRPA1167.
          05 PO-DRPA1167-RC                    PIC  X(003).
             88 PO-DRPA1167-RC-OK                       VALUE 'OK '.

      ******** Ende Copy-Member **************************************
