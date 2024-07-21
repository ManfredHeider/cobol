      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0951.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:\MF\CPY\DRPA0951.CPv  $
      *
      *   Rev 2.3   May 31 1999 07:51:44   T01JM2S
      *APA00012 - NEUE RC'S UPPA0100
      *
      *   Rev 2.2   May 28 1999 07:38:52   T01JM2S
      *APA00042
      *
      *   Rev 2.1   May 17 1999 18:29:08   T01JM2S
      *APA00012 - NEUE FUNKTIONEN DRPA1237, DRPA1238
      *
      *   Rev 2.0   May 17 1999 11:36:28   T01JM2S
      *NT-Umstellung
      *
      *   Rev 1.2   May 17 1999 11:33:32   T01JM2S
      *NT-Umstellung
      *
      *   Rev 1.1   11 Nov 1998 15:52:16   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   01 Apr 1998 14:17:46   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0951                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<ERM. PVW/BATCH  EVS >>                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    08.05.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!08.05.98! 01 ! NEU                       ! WOLFF    *
      *         !        !    !                           !          *
      * AFPA0012!07.01.99! 02 ! ÜBERARBEITET              ! WOLFF    *
      *         !        !    !                           !          *
      * APA00012!17.05.99!2.1 ! ANPASSUNG AN DRPA1238     ! WOLFF    *
      *         !        !    !                           !          *
      * APA00012!26.05.99!2.3 ! NEU PO-DRPA0951-RC-NOK    ! WOLFF    *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0951                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0951.
          05 PI-DRPA0951-DATEN.
             10 PI-DRPA0951-NPM-TEIL.
                COPY DRPA0940.
             10 PI-DRPA0951-MELD-TEIL.
                COPY DRPA0941.
             10 PI-DRPA0951-MAX-USED-MELD-DS        PIC S9(004) BINARY.
             10 PI-DRPA0951-MELD-DS-TEIL            OCCURS 10
                                                 INDEXED BY IX-MELD-DS.
                COPY DRPA0941.
      *
       01 PO-DRPA0951.
          05 PO-DRPA0951-RC                         PIC  X(003).
             88 PO-DRPA0951-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0951-RC-NICHT-BUCHEN               VALUE 'NBU'.
             88 PO-DRPA0951-RC-NOK                        VALUE 'NOK'.
          05 PO-DRPA0951-DATEN.
             10 PO-DRPA0951-MAX-USED-KOFV-TEIL      PIC S9(004) BINARY.
             10 PO-DRPA0951-KOFV-TEIL               OCCURS 2.
                COPY DRPA0847.
      *
      ******* ENDE COPY-MEMBER **************************************
