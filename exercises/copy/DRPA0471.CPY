      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0471.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA0471.CPv  $
      *
      *   Rev 2.1   25 May 2000 11:17:08   T13EX21
      * 
      *
      *   Rev 2.0   May 27 1999 11:03:10   T01JM2S
      *NT-Umstellung
      *
      *   Rev 1.0   23 Jul 1997 12:41:00   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0471                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<PVW GEVO RÜCKBELAST.>>                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    15.01.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!15.01.98! 01 ! NEU                       ! WOLFF    *
      * APA00012!07.12.98! 02 ! BEARBEITUNGSMODUS ENTFERNT! MUND     *
      * APA00090!25.05.00! 2.1! PI-DRPA0471-PIN-VNR-AKZ   ! STORCH   *
      *         !        !    ! EINGEFÜGT                 !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0471                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0471.
          05 PI-DRPA0471-DATEN.
071298*      10 PI-DRPA0471-KNR-B-MODUS             PIC S9(004) BINARY.
071298*      10 FILLER                              PIC  X(001).
250500       10 PI-DRPA0471-PIN-VNR-AKZ             PIC S9(004) BINARY.
      *
       01 PO-DRPA0471.
          05 PO-DRPA0471-RC                         PIC  X(003).
             88 PO-DRPA0471-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0471-RC-MELDUNGSFEHLER             VALUE 'MFE'.
      *
      ******* ENDE COPY-MEMBER **************************************
