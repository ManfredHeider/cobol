      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0547.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRPA0547.CPv  $
      *
      *   Rev 2.4   Sep 21 2018 12:16:44   t13ex44
      *APA02548: Nachfolgeregelung Direktion Familie
      *
      *   Rev 2.3   Apr 07 2016 10:11:36   T13EX67
      *APA02220: 4 Vor- und Nachkommastellen zur Konditionsermittlu
      *ng - TBPA0023/MSS
      *
      *   Rev 2.2   Dec 19 2013 11:12:56   t13ex72
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g): Kollektivverträge
      *
      *   Rev 2.1   Apr 03 2000 11:25:40   T13EX25
      *APA00089, Provision-Verteil-KZ neu
      *
      *   Rev 2.0   Feb 27 1999 14:29:14   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   06 Aug 1998 10:33:00   T13EX25
      *APAA0032 - PEW-Release 2.0
      *
      *   Rev 1.0   01 Oct 1997 11:29:20   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0547                                      *
      *    --------------------------------------------------------   *
      *    DATEN FÜR KONDITION ZUR GRUNDPROVISION                     *
      *    --------------------------------------------------------   *
      *    ERSTELLT: JÄGER-SOKOLUK                                    *
      *    DATUM:    13.10.97                                         *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!13.10.97! 01 ! NEU                       ! JÄGER    *
      * APA00089!31.03.00! 02 ! NEUE ATTRIBUTE AUS KONDI- ! JÄGER    *
      *         !        !    ! TIONEN FÜR GRUNDPROVISION !          *
      * APA01526!24.10.13!2.1 ! BEGR.SÄTZE KOLLEKTIV      ! BÖTTCHER *
      * APA02220!10.03.16!2.3 ! ÄNDERUNG FORMAT %-SÄTZE   ! STORCH   *
      * APA02548!26.07.18!2.4 ! RED-PROVSATZ RESTSTRUKTUR ! SCHEIDEL *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE DATEN FÜR DIE KONDITION ZUR       *
      *    GRUNDPROVISION.                                           *
      *                                                              *
      ****************************************************************
      *
             30 KOND-DATEN-GRP.
                35 PRKO-KOND-NR                PIC S9(004) BINARY.
                35 KNR-KOND-REGEL-GRP          PIC S9(004) BINARY.
100316*         35 PRKO-GRP-STZ                PIC S9(003)V999
100316          35 PRKO-GRP-STZ                PIC S9(004)V9999
                                                   PACKED-DECIMAL.
100316*         35 PRKO-VWF-STZ                PIC S9(003)V999
100316          35 PRKO-VWF-STZ                PIC S9(004)V9999
                                                   PACKED-DECIMAL.
100316*         35 PRKO-VTF-STZ                PIC S9(003)V999
100316          35 PRKO-VTF-STZ                PIC S9(004)V9999
                                                   PACKED-DECIMAL.
                35 PRKO-RCK-WERT               PIC S9(009) BINARY.
                35 KNR-KOND-WAEHRUNG           PIC S9(009) BINARY.
                35 PVT-KZ                      PIC S9(009) BINARY.
241013          35 BEGRSATZ-AP                 PIC S9(009)V9(2) COMP-3.
241013          35 BEGRSATZ-PRPFLSU            PIC S9(009)V9(2) COMP-3.
241013          35 EH-SATZ-ALLF                PIC S9(009)V9(2) COMP-3.
241013          35 KUERZ-SATZ                  PIC S9(009)V9(6) COMP-3.
260718          35 RED-PROV-STZ-RESTSTRUK      PIC S9(004)V9(04)
260718                                             PACKED-DECIMAL.
      *
      ******* ENDE COPY-MEMBER **************************************

