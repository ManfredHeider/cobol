      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0569.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA0569.CPv  $
      *
      *   Rev 2.1   25 May 2000 11:17:14   T13EX21
      * 
      *
      *   Rev 2.0   May 27 1999 14:05:46   T01JM2S
      *APA00042
      *
      *   Rev 1.0   22 Oct 1997 11:56:14   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0569                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<PVW GEVO EVS-STO-WIK>>                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    29.01.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!29.01.98! 01 ! NEU                       ! WOLFF    *
      * APA00012!07.12.98! 02 ! BEARBEITUNGSMODUS ENTFERNT! MUND     *
      * APA00090!25.05.00! 2.1! PI-DRPA0569-PIN-VNR-AKZ   ! STORCH   *
      *         !        !    ! EINGEFÜGT                 !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0569                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0569.
          05 PI-DRPA0569-DATEN.
071298*      10 PI-DRPA0569-KNR-B-MODUS             PIC S9(004) BINARY.
071298*      10 FILLER                              PIC  X(001).
250500       10 PI-DRPA0569-PIN-VNR-AKZ             PIC S9(004) BINARY.
      *
       01 PO-DRPA0569.
          05 PO-DRPA0569-RC                         PIC  X(003).
             88 PO-DRPA0569-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0569-RC-MELDUNGSFEHLER             VALUE 'MFE'.
             88 PO-DRPA0569-RC-KEINE-EVS-BEARB            VALUE 'KEB'.
          05 PO-DRPA0569-DATEN.
             10 PO-DRPA0569-PVER.
                COPY DRPA0653.
             10 PO-DRPA0569-PMDS.
                COPY DRPA0639.
             10 PO-DRPA0569-MELD-DS.
                COPY DRPA0615.
      *
      ******* ENDE COPY-MEMBER **************************************
