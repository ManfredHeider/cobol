      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0180.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAT0180.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:30   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Nov 1998 17:52:54   T13EX26
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   05 Sep 1997 09:21:20   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0180 FUNKTION ERSTELLEN NOTIZFUNKTION    *
      *                          zu einer art von objekt             *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    08.11.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!08.11.97! 01 !NEU                        !BOHLINGER *
      *         !        !    !                           !          *
      * AFPA0012!22.06.98!1.1 !NEU: PI-DRAT0180-TAB       ! WOLFF    *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAT0180                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRAT0180.
           05  PI-DRAT0180-UFKT                PIC  X(02).
      *   KANN BELEGT WERDEN MIT 1=write-first  2=write-next
          05 PI-DRAT0180-AUSWAHL-KRITERIEN.
             10 PI-DRAT0180-KNR-NOTIZ-ART      PIC S9(004) BINARY.
             10 PI-DRAT0180-KNR-OBJEKT-HKF-TAB PIC S9(004) BINARY.
             10 PI-DRAT0180-NTZ-PTR-ID       PIC S9(015) PACKED-DECIMAL.
             10 PI-DRAT0180-NTZ-PTR-ST-NR      PIC S9(009) BINARY.
             10 PI-DRAT0180-NTZ-PTR-PIN        PIC S9(009) BINARY.
             10 PI-DRAT0180-NTZ-PTR-VNR        PIC  X(015).
          05 PI-DRAT0180-DATEN.
             10 PI-DRAT0180-ANZAHL           PIC S9(004)  BINARY.
220698*      10 PI-DRAT0180-TABELLE OCCURS 10.
220698*         15 PI-DRAT0180-NTZZ-ZEILE    PIC  X(080).
220698       10 PI-DRAT0180-TAB.
220698          15 PI-DRAT0180-TABELLE         OCCURS 10.
220698             20 PI-DRAT0180-NTZZ-ZEILE   PIC  X(080).
      *
       01 PO-DRAT0180.
          05 PO-DRAT0180-RC                  PIC  X(003).
             88 PO-DRAT0180-RC-OK                       VALUE 'OK '.
      *
      ******** Ende Copy-Member **************************************
