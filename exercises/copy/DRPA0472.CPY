      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0472.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA0472.CPv  $
      *
      *   Rev 2.1   25 May 2000 11:17:12   T13EX21
      * 
      *
      *   Rev 2.0   May 27 1999 11:07:14   T01JM2S
      *NT-Umstellung
      *
      *   Rev 1.0   23 Jul 1997 12:41:06   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0472                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<PVW GEVO WIK        >>                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    30.01.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!30.01.98! 01 ! NEU                       ! WOLFF    *
      * APA00012!07.12.98! 02 ! BEARBEITUNGSMODUS ENTFERNT! MUND     *
      * APA00090!25.05.00! 2.1! PI-DRPA0472-PIN-VNR-AKZ   ! STORCH   *
      *         !        !    ! EINGEFÜGT                 !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0472                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0472.
          05 PI-DRPA0472-DATEN.
071298*      10 PI-DRPA0472-KNR-B-MODUS             PIC S9(004) BINARY.
071298*      10 FILLER                              PIC  X(001).
250500       10 PI-DRPA0472-PIN-VNR-AKZ             PIC S9(004) BINARY.
      *
       01 PO-DRPA0472.
          05 PO-DRPA0472-RC                         PIC  X(003).
             88 PO-DRPA0472-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0472-RC-MELDUNGSFEHLER             VALUE 'MFE'.
      *
      ******* ENDE COPY-MEMBER **************************************
