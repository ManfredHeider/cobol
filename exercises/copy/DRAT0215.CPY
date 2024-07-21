      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0215.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAT0215.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:36   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Nov 1998 17:54:14   T13EX26
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   05 Sep 1997 09:21:34   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0215 FUNKTION ERMITTELN EINER NOTIZ      *
      *                                   ZU EINER ART VON OBJEKT    *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    29.12.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!29.12.97! 01 !NEU                        !BOHLINGER *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAT0215                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    FUNKTION: ERMITTELN EINER NOTIZ ZU EINER ART VON OBJEKT   *
      *                                                              *
      ****************************************************************
       01 PI-DRAT0215.
           05  PI-DRAT0215-UFKT                   PIC  X(02).
      *   KANN BELEGT WERDEN MIT 1=LIST-FIRST, 2=LIST-NEXT, 3=QUIT
          05 PI-DRAT0215-KEY.
             10 PI-DRAT0215-NTZ-ID                 PIC S9(015)
                                                         PACKED-DECIMAL.
      *
       01 PO-DRAT0215.
          05 PO-DRAT0215-RC                        PIC  X(003).
             88 PO-DRAT0215-RC-OK                        VALUE 'OK '.
             88 PO-DRAT0215-RC-LEER                      VALUE 'LER'.
             88 PO-DRAT0215-RC-EOF                       VALUE 'EOF'.
          05 PO-DRAT0215-ERGEBNISZEILE.
             10 PO-DRAT0215-NTZ-DATEN.
                15 PO-DRAT0215-KNR-OBJEKT-HKF-TAB  PIC S9(004) BINARY.
                15 PO-DRAT0215-KNR-NOTIZ-ART       PIC S9(004) BINARY.
                15 PO-DRAT0215-NTZ-KENNUNG         PIC  X(008).
                15 PO-DRAT0215-NTZ-LAST-UPDATE     PIC  X(026).
             10 PO-DRAT0215-NTZZ-DATEN.
                15 PO-DRAT0215-ANZAHL              PIC S9(004) BINARY.
                15 PO-DRAT0215-NTZZ-TABELLE        OCCURS 10.
                   20 PO-DRAT0215-NTZZ-ZEILE       PIC  X(080).
      *
      ******** Ende Copy-Member **************************************
