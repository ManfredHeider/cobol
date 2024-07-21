      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0384.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/mf/CPY/DRPA0384.CPv  $
      *
      *   Rev 2.7   Sep 11 2019 13:16:34   t13ex44
      *APA02814: Familieninterne DL-Nachfolge-Regelung: Übertragung
      * Praxisbestand
      *
      *   Rev 2.6   Mar 12 2019 15:05:38   t13ex44
      *APA02814: Familieninterne DL-Nachfolge-Regelung: Übertragung
      * Praxisbestand
      *
      *   Rev 2.5   Mar 28 2013 09:13:52   t13ex25
      *APA01561: DVAG Wien: gewerblicher Kundenbetreuer
      *
      *   Rev 2.5   Mar 26 2013 15:41:44   t13ex25
      *APA01561: DVAG Wien: gewerblicher Kundenbetreuer
      *
      *   Rev 2.4   Jun 19 2012 09:23:24   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.3   May 21 2012 08:48:22   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.2   Dec 10 2007 09:16:22   t13ex21
      *APA00622: Programmänderungen Martina
      *
      *   Rev 2.1   Jul 17 2006 12:28:32   t13ex44
      *APA00409: Kundenbetreuungsvereinbarung
      *
      *   Rev 2.0   May 27 1999 12:16:04   T13EX25
      *APA00042, Neuerstellung V2.0
      *
      *   Rev 1.0   23 Jul 1997 12:29:56   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0384  FUNKTION VWF PROVISION BERECHNEN   *
      *    --------------------------------------------------------- *
      *    ERSTELLT: JÄGER-SOKOLUK                                   *
      *    DATUM:    23.07.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DAISY 1 !21.07.97! 01 ! NEUERSTELLUNG             ! JÄGER    *
      * APA00409!14.03.06!2.1 ! KD-BETR-KZ HINZU          ! SCHEIDEL *
      * APA00622!21.06.07!2.2 ! NEU: PI-KNR-VGES          ! STORCH   *
      * APA01101!29.02.12!2.3 ! VGES DURCH MANDANT ERSETZT! STORCH   *
      * APA01561!26.03.13!2.4 ! VBAS Wien                 ! Jäger    *
      * APA02824!08.03.19!2.6 ! VNR HINZU                 ! SCHEIDEL *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0384                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0384.
          05 PI-DRPA0384-PZVP-VERSION        PIC S9(004)  BINARY.
          05 PI-DRPA0384-DATEN.
             10 PI-DRPA0384-KD-BETR          PIC  9(007).
             10 PI-DRPA0384-PROV-TERMIN      PIC  X(010).
             10 PI-DRPA0384-KOND-KDB.
                COPY DRPA0554.
140306       10 PI-DRPA0384-KD-BETR-KZ       PIC  X(001).
210607       10 PI-DRPA0384-KNR-MANDANT      PIC S9(004) BINARY.
260313       10 PI-DRPA0384-KD-BETR-ALT      PIC  9(007).
260313       10 PI-DRPA0384-PGEB-GES         PIC S9(004) BINARY.
080319       10 PI-DRPA0384-VNR              PIC  X(015).
      *
       01 PO-DRPA0384.
          05 PO-DRPA0384-RC                  PIC  X(003).
             88 PO-DRPA0384-RC-OK                 VALUE 'OK '.
             88 PO-DRPA0384-RC-VSTAT-GEFUELLT     VALUE 'VSG'.
          05 PO-DRPA0384-DATEN.
             10 PO-DRPA0384-STRK-KZ          PIC  X(001).
      *
      ******** Ende Copy-Member **************************************


