      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1149.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA1149.CPv  $
      *
      *   Rev 2.0   May 25 1999 14:14:20   T13AE18
      *apa00012
      *
      *   Rev 1.0   21 Oct 1998 14:00:48   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1149 FUNKTION Get Ausgangsdaten          *
      *                          für EVS-Vertrag                     *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Bohlinger                                       *
      *    DATUM:    22.12.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!22.12.98! 01 !NEU                        ! Bohlinger*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1149                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1149.
          05 PI-DRPA1149-PRINTE                PIC S9(015)
                                               PACKED-DECIMAL.
      *
       01 PO-DRPA1149.
          05 PO-DRPA1149-RC                    PIC  X(003).
             88 PO-DRPA1149-RC-OK                   VALUE 'OK '.
             88 PO-DRPA1149-RC-NOK                  VALUE 'NOK'.

          05 PO-DRPA1149-MELDUNG.
             10 PO-DRPA1149-MELDUNG-TYP        PIC  X(001).
             10 PO-DRPA1149-MELDUNG-NR         PIC S9(004) BINARY.

          05 PO-DRPA1149-DATEN.
             COPY DRPA1150.
