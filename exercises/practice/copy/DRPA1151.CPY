      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1151.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\DRPA1151.CPv  $
      *
      *   Rev 2.2   Jul 13 1999 10:26:24   T13AE18
      *apa00012
      *
      *   Rev 2.1   May 25 1999 14:16:10   T13AE18
      *apa00012
      *
      *   Rev 2.0   Mar 25 1999 14:10:18   T13AE18
      *APA00012 Einführung EVS-Umbuchungs-
      *dialog
      *
      *   Rev 1.0   21 Oct 1998 14:01:02   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1151 FUNKTION GET EVS-AUSGANGSDATEN      *
      *                          FüR UGPA0002                        *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Bohlinger                                       *
      *    DATUM:    01.01.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!05.01.98! 01 !NEU                        ! BOHLINGER*
      * APA00012!05.07.99!2.02!PMBE-V-GUELTIG-AB IM PO-   ! BOHLINGER*
      *         !        !    !BEREICH                    !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1151                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1151.
          05 PI-DRPA1151-PRINTE             PIC S9(015)
                                            PACKED-DECIMAL.
      *
       01 PO-DRPA1151.
          05 PO-DRPA1151-RC                 PIC  X(003).
             88 PO-DRPA1151-RC-OK                VALUE 'OK '.
             88 PO-DRPA1151-RC-NOK               VALUE 'NOK'.

          05 PO-DRPA1151-MELDUNG.
             10 PO-DRPA1151-MELDUNG-TYP     PIC  X(001).
             10 PO-DRPA1151-MELDUNG-NR      PIC S9(004) BINARY.

          05 PO-DRPA1151-DATEN.
             10 PMBE-ID                     PIC S9(015) PACKED-DECIMAL.
             10 PMBE-VERSION                PIC S9(004) BINARY.
             10 PMBE-PGFAELLIG-VON          PIC  X(010).
             10 PMBE-PGFAELLIG-BIS          PIC  X(010).
050799       10 PMBE-V-GUELTIG-AB           PIC  X(026).
             10 VM                          PIC S9(009) BINARY.
             10 AVERM                       PIC S9(009) BINARY.
             10 AVERM-PRZ                   PIC  X(001).
