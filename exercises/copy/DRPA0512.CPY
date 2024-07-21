      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0512.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRPA0512.CPv  $
      *
      *   Rev 2.3   Apr 07 2016 10:11:18   T13EX67
      *APA02220: 4 Vor- und Nachkommastellen zur Konditionsermittlu
      *ng - TBPA0023/MSS
      *
      *   Rev 2.2   Jan 10 2014 12:48:20   t13ex72
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g): GeVo Leben (CBO)
      *
      *   Rev 2.1   Apr 03 2000 11:23:56   T13EX25
      *APA00089, Summe im PI-Bereich neu
      *
      *
      *   Rev 2.0   May 27 1999 08:20:32   T13EX25
      *APA00042, Erfassung V2.0
      *
      *   Rev 1.0   12 Sep 1997 16:59:06   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0512  FUNKTION VWF PROVISION BERECHNEN   *
      *                           (UPPA0044)                         *
      *    --------------------------------------------------------- *
      *    ERSTELLT: JÄGER-SOKOLUK                                   *
      *    DATUM:    06.08.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DAISY 1 !16.09.97! 01 ! NEUERSTELLUNG             ! JÄGER    *
      * APA00089!23.03.00! 02 ! SUMME HINZUGEFÜGT/OHNE ZVP! JÄGER    *
      * APA01526!24.10.13! 2.2! BEGR.SÄTZE KOLLEKTIV      ! BÖTTCHER *
      * APA02220!10.03.16! 2.3! ÄNDERUNG FORMAT %-SÄTZE   ! STORCH   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0512                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0512.
          05 PI-DRPA0512-DATEN.
             10 PI-DRPA0512-KNR-REGEL       PIC S9(004)  BINARY.
100316*      10 PI-DRPA0512-VWF-PROV-STZ    PIC S9(003)V999
100316       10 PI-DRPA0512-VWF-PROV-STZ    PIC S9(004)V9999
                                             PACKED-DECIMAL.
             10 PI-DRPA0512-BEITRAG         PIC S9(009)V99
                                             PACKED-DECIMAL.
             10 PI-DRPA0512-SUMME           PIC S9(009)V99
                                             PACKED-DECIMAL.
241013       10 PI-DRPA0512-BEGRSATZ-AP      PIC S9(009)V9(2) COMP-3.
241013       10 PI-DRPA0512-BEGRSATZ-PRPFLSU PIC S9(009)V9(2) COMP-3.
      *
       01 PO-DRPA0512.
          05 PO-DRPA0512-RC                 PIC  X(003).
             88 PO-DRPA0512-RC-OK                VALUE 'OK '.
             88 PO-DRPA0512-RC-REGEL-FALSCH      VALUE 'REF'.
             88 PO-DRPA0512-RC-VSTAT-GEFUELLT    VALUE 'VSG'.
          05 PO-DRPA0512-DATEN.
             10 PO-DRPA0512-VWF-PROV-BTRG   PIC S9(009)V99
                                             PACKED-DECIMAL.
      *
      ******** ENDE COPY-MEMBER **************************************

