      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0196.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAT0196.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:32   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Nov 1998 17:53:56   T13EX26
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   05 Sep 1997 09:22:28   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0196  FUNKTION NOTIZEN ERMITTELN         *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    27.11.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!27.11.97! 01 ! NEUERSTELLUNG             ! BOHLINGER*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAT0196                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRAT0196.
           05  PI-DRAT0196-UFKT                PIC  X(02).
      *   KANN BELEGT WERDEN MIT 1=LIST-FIRST, 2=LIST-NEXT, 3=QUIT
          05 PI-DRAT0196-AUSWAHL-KRITERIEN.
             10 PI-DRAT0196-KNR-OBJEKT-HKF-TAB PIC S9(004) BINARY.
             10 PI-DRAT0196-NTZ-PTR-ID       PIC S9(015) PACKED-DECIMAL.
             10 PI-DRAT0196-S-NTZ-PTR-ID     PIC  X(001).
             10 PI-DRAT0196-NTZ-PTR-ST-NR    PIC S9(009) BINARY.
             10 PI-DRAT0196-S-NTZ-PTR-ST-NR  PIC  X(001).
             10 PI-DRAT0196-NTZ-PTR-PIN      PIC S9(009) BINARY.
             10 PI-DRAT0196-S-NTZ-PTR-PIN    PIC  X(001).
             10 PI-DRAT0196-NTZ-PTR-VNR      PIC  X(015).
             10 PI-DRAT0196-S-NTZ-PTR-VNR    PIC  X(001).
      *
       01 PO-DRAT0196.
          05 PO-DRAT0196-RC                   PIC  X(003).
             88 PO-DRAT0196-RC-OK                       VALUE 'OK '.
             88 PO-DRAT0196-RC-EOF                      VALUE 'EOF'.
             88 PO-DRAT0196-RC-LEER                     VALUE 'LER'.
          05 PO-DRAT0196-NTZ-DATEN.
             10 PO-DRAT0196-NTZ-ID           PIC S9(015) PACKED-DECIMAL.
             10 PO-DRAT0196-KNR-NOTIZ-ART    PIC S9(004) BINARY.
             10 PO-DRAT0196-NTZ-KENNUNG      PIC  X(008).
             10 PO-DRAT0196-NTZ-LAST-UPDATE  PIC  X(026).
             10 PO-DRAT0196-S-NOTIZ-NEU      PIC  X(001).
          05 PO-DRAT0196-NTZZ-DATEN.
             10 PO-DRAT0196-ANZAHL            PIC S9(004) BINARY.
             10 PO-DRAT0196-NTZZ-TABELLE      OCCURS 10.
                15 PO-DRAT0196-NTZZ-ZEILE     PIC  X(080).
      *
      ******** Ende Copy-Member **************************************
