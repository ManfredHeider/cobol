      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0469.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA0469.CPv  $
      *
      *   Rev 2.1   25 May 2000 11:15:52   T13EX21
      *APA00090 - PIN-VNR-AKZ im PI-Bereich 
      *eingefügt
      *
      *   Rev 2.0   May 27 1999 10:59:58   T01JM2S
      *NT-Umstellung
      *
      *   Rev 1.0   23 Jul 1997 12:40:46   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0469                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<PVW GEVO NEUZUGANG  >>                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: PRIGGEN                                          *
      *    DATUM:    03.12.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!03.12.97! 01 ! NEU                       ! PRIGGEN  *
      * APA00012!07.12.98! 02 ! BEARBEITUNGSMODUS ENTFERNT! MUND     *
      * APA00090!25.05.00! 2.1! PI-DRPA0469-PIN-VNR-AKZ   ! STORCH   *
      *         !        !    ! EINGEFÜGT                 !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0469                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0469.
          05 PI-DRPA0469-DATEN.
071298*      10 PI-DRPA0469-KNR-B-MODUS             PIC S9(004) BINARY.
071298*      10 FILLER                              PIC  X(001).
250500       10 PI-DRPA0469-PIN-VNR-AKZ             PIC S9(004) BINARY.
      *
       01 PO-DRPA0469.
          05 PO-DRPA0469-RC                         PIC  X(003).
             88 PO-DRPA0469-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0469-RC-MELDUNGSFEHLER             VALUE 'MFE'.
          05 PO-DRPA0469-DATEN.
             10 PO-DRPA0469-PVER.
                COPY DRPA0653.
             10 PO-DRPA0469-PVOR.
                COPY DRPA0614.
             10 PO-DRPA0469-PMDS.
                COPY DRPA0639.
             10 PO-DRPA0469-MELD-DS.
                COPY DRPA0615.
      *
      ******* ENDE COPY-MEMBER **************************************
