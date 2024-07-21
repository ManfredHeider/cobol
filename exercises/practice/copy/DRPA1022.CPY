      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1022.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRPA1022.CPv  $
      *
      *   Rev 2.4   Jan 30 2019 09:13:38   t13ex67
      *APA02548: Nachfolgeregelung Direktion Familie
      *
      *   Rev 2.3   Jan 13 2014 07:22:14   t13ex67
      *APA01539: Kalypso TP5: Anpassungen Datenmodell: TBPA0023-TBP
      *A0033
      *
      *   Rev 2.2   Nov 07 2006 16:07:46   t13ex25
      *APA00452: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.1   24 Sep 2001 12:30:04   t13ex21
      *FPA00292 - Erweiterung PI-Bereich
      *
      *   Rev 2.0   May 26 1999 13:42:20   T13EX21
      *APA00012 - Neuerstellung
      *
      *   REV 1.0   14 JUL 1998 13:59:26   P13SP19
      *INITIAL REVISION.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1022                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<GEGENBUCHUNG ERSTELLEN>>                 *
      *    --------------------------------------------------------   *
      *    ERSTELLT: FOLTZ                                            *
      *    DATUM:    20.08.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!20.08.98! 01 ! NEU                       ! FOLTZ    *
      * FPA00292!18.09.01!02.1! ERWEITERUNG PI-BEREICH    ! STORCH   *
      * APA00452!07.11.06!02.2! UMSTELLUNG 3-STELL. PROD. ! JÄGER    *
      * APA01539!04.10.13!02.3! PI-BEREICH ERWEITERT      ! STORCH   *
      * APA02548!23.01.19!02.4! DSGVO:                    ! STORCH   *
      *         !        !    ! PO-BEREICH ERWEITERT      !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1022                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01  PI-DRPA1022.
           05 PI-DRPA1022-BUCHUNG.
              COPY DRPA0451.

           05 PI-DRPA1022-KNR-BUCHUNGS-GRUND         PIC S9(004) BINARY.
           05 PI-DRPA1022-PGEB-GES                   PIC S9(004) BINARY.
           05 PI-DRPA1022-PROD-PROD-SCHL             PIC S9(004) BINARY.
           05 PI-DRPA1022-PRODART                    PIC S9(004) BINARY.
           05 PI-DRPA1022-LAUFZEIT                   PIC S9(004) BINARY.
           05 PI-DRPA1022-STICHTAG                   PIC  X(010).
041013     05 PI-DRPA1022-TARIFSCHL-GRP              PIC S9(009) BINARY.
041013     05 PI-DRPA1022-TARIFSCHL                  PIC S9(009) BINARY.
041013     05 PI-DRPA1022-KNR-TARIF-KZ               PIC S9(004) BINARY.
041013     05 PI-DRPA1022-VERM-GES                   PIC S9(004) BINARY.
041013     05 PI-DRPA1022-KNR-MANDANT                PIC S9(004) BINARY.
041013     05 PI-DRPA1022-SAM-NR                     PIC S9(009) BINARY.
041013     05 PI-DRPA1022-KNR-RAB-ST                 PIC S9(004) BINARY.
041013     05 PI-DRPA1022-ZAHLW                      PIC S9(004) BINARY.
041013     05 PI-DRPA1022-URSPR-POL-DAT              PIC X(010).
041013     05 PI-DRPA1022-FAELLIG-VON                PIC X(010).

230119     05 PI-DRPA1022-BA-PEST-PEDB.
230119       COPY DRPA0433.
      *
       01  PO-DRPA1022.
           05 PO-DRPA1022-RC                         PIC  X(003).
              88 PO-DRPA1022-RC-GBU-ERSTELLT                VALUE 'GER'.
              88 PO-DRPA1022-RC-GBU-NOT-ERSTBAR             VALUE 'GNE'.
              88 PO-DRPA1022-RC-GBU-NOT-RELEV               VALUE 'GNR'.
           05 PO-DRPA1022-GEGENBUCHUNG.
              COPY DRPA0451.
230119     05 PO-DRPA1022-KZ-GB-PEST-NEU             PIC  X(001).
230119     05 PO-DRPA1022-GB-PEST-PEDB.
230119        COPY DRPA0433.

      ******* ENDE COPY-MEMBER **************************************

