      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1308.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/DRPA1308.CPv  $
      *
      *   Rev 2.6   May 24 2018 11:50:16   t13ex67
      *APA02579: FriWi: Provisionsverteilung- und Abrechnung - MSS
      *
      *   Rev 2.5   Mar 03 2017 07:59:56   t13ex67
      *APA02275: DAISY-Erweiterungen zu FlexN - Dialoge MSS
      *
      *   Rev 2.4   Jul 16 2015 15:12:14   t13ex67
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System: Dialog
      *e MSS
      *
      *   Rev 2.3   Mar 22 2000 14:08:28   T13AE18
      *APA00067
      *
      *   Rev 2.2   Mar 16 2000 16:20:08   T13EX13
      *APA00067 Anpassung an Badenia +
      *Umstellung PGDF -> PGKF
      *
      *   Rev 2.1   Feb 22 2000 08:42:48   T13AE18
      *APA00080
      *
      *   Rev 2.0   Nov 29 1999 13:55:12   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1308 FUNKTION CECK DIALOGFELDER          *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    03.12.1999                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00068!03.12.99!2.01!NEU                        !BOHLINGER *
      * APA00067!14.03.00!2.03!UMSTELLUNG PGDF -> PGKF    !WILKENS   *
      * APA01932!01.06.15!2.04!ERWEITERUNG OCCURS-STRUKTUR!STORCH    *
      * APA02275!02.03.17!2.05!ERWEITERUNG OCCURS-STRUKTUR!STORCH    *
      * APA02579!24.05.18!2.06!ERWEITERUNG OCCURS-STRUKTUR!STORCH    *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1308                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1308.
             10 PI-DRPA1308-PGEB-GES           PIC S9(004) BINARY.
140300*      10 PI-DRPA1308-KNR-DIAL-FELD      OCCURS 21.
010615*      10 PI-DRPA1308-KNR-DIAL-FELD      OCCURS 25.
020317*      10 PI-DRPA1308-KNR-DIAL-FELD      OCCURS 30.
020317       10 PI-DRPA1308-KNR-DIAL-FELD      OCCURS 50.
140300**        15 PI-DRPA1308-KNR-DIA-FELD-NAME   PIC S9(004) BINARY.
140300          15 PI-DRPA1308-KNR-NPM-FELD-NAME   PIC S9(004) BINARY.

       01 PO-DRPA1308.
          05 PO-DRPA1308-RC                    PIC  X(003).
             88 PO-DRPA1308-RC-OK                       VALUE 'OK '.

140300*   05 PO-DRPA1308-NPM-FELD-TAB          OCCURS 21.
010615*   05 PO-DRPA1308-NPM-FELD-TAB          OCCURS 25.
020317    05 PO-DRPA1308-NPM-FELD-TAB          OCCURS 30.
140300      10 PO-DRPA1308-KNR-NPM-FELD-NAME   PIC  S9(004) BINARY.
140300      10 PO-DRPA1308-KNR-NPM-FELD-ZUL    PIC  S9(004) BINARY.
140300      10 PO-DRPA1308-KONF-Z-INFO         PIC  S9(4) USAGE COMP.
140300**    10 PO-DRPA1308-KNR-DIA-FELD-NAME   PIC  S9(004) BINARY.
140300**    10 PO-DRPA1308-KNR-DIA-FELD-BEARB  PIC  S9(004) BINARY.

      *
      ******** Ende Copy-Member **************************************



