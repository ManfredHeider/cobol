      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0216.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAT0216.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:36   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Nov 1998 17:54:18   T13EX26
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   05 Sep 1997 09:21:34   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0216 FUNKTION ERMITTELN ALLER NOTIZEN    *
      *                                   ZU EINEM OBJEKT            *
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
      *    FÜR DIE FUNKTION DRAT0216                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    FUNKTION: ERMITTELN ALLER NOTIZEINTRäGE ZU EINEM OBJEKT   *
      *    << JE NOTIZ WIRD NUR DIE ERSTE NOTIZZEILE AUSGEGEBEN >>   *
      ****************************************************************
       01 PI-DRAT0216.
           05  PI-DRAT0216-UFKT                PIC  X(02).
      *   KANN BELEGT WERDEN MIT 1=LIST-FIRST, 2=LIST-NEXT, 3=QUIT
          05 PI-DRAT0216-AUSWAHL-KRITERIEN.
             10 PI-DRAT0216-KNR-OBJEKT-HKF-TAB PIC S9(004) BINARY.
             10 PI-DRAT0216-NTZ-PTR-ID          PIC S9(015)
                                                         PACKED-DECIMAL.
             10 PI-DRAT0216-NTZ-PTR-ST-NR       PIC S9(009) BINARY.
             10 PI-DRAT0216-NTZ-PTR-PIN         PIC S9(009) BINARY.
             10 PI-DRAT0216-NTZ-PTR-VNR         PIC  X(015).
      *
       01 PO-DRAT0216.
          05 PO-DRAT0216-RC                        PIC  X(003).
             88 PO-DRAT0216-RC-OK                        VALUE 'OK '.
             88 PO-DRAT0216-RC-LEER                      VALUE 'LER'.
             88 PO-DRAT0216-RC-EOF                       VALUE 'EOF'.
          05 PO-DRAT0216-DATEN.
             10 PO-DRAT0216-ANZAHL                 PIC S9(004) BINARY.
             10 PO-DRAT0216-TABELLE                OCCURS 10.
                15 PO-DRAT0216-ERGEBNISZEILE.
                   20 PO-DRAT0216-NTZ-ID           PIC S9(015)
                                                   PACKED-DECIMAL.
                   20 PO-DRAT0216-KNR-NOTIZ-ART    PIC S9(004) BINARY.
                   20 PO-DRAT0216-NTZ-KENNUNG      PIC  X(008).
                   20 PO-DRAT0216-NTZ-LAST-UPDATE  PIC  X(026).
                   20 PO-DRAT0216-NTZZ-ZEILE-1     PIC  X(045).
      *
      ******** Ende Copy-Member **************************************
