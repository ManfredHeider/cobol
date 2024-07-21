      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0183.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAT0183.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:30   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Nov 1998 17:53:04   T13EX26
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   05 Sep 1997 09:21:34   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0183 FUNKTION ERMITTELN ALLER NOTIZEN    *
      *                                   ZU EINEM OBJEKT            *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    27.11.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!27.11.97! 01 !NEU                        !BOHLINGER *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAT0183                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    FUNKTION: ERMITTELN ALLER NOTIZEN ZU EINEM OBJEKT,        *
      *              UNABHäNGIG VON DER KNR-NOTIZ-ART                *
      *                                                              *
      ****************************************************************
       01 PI-DRAT0183.
           05  PI-DRAT0183-UFKT                PIC  X(02).
      *   KANN BELEGT WERDEN MIT 1=LIST-FIRST, 2=LIST-NEXT, 3=QUIT
          05 PI-DRAT0183-AUSWAHL-KRITERIEN.
             10 PI-DRAT0183-KNR-OBJEKT-HKF-TAB PIC S9(004) BINARY.
             10 PI-DRAT0183-NTZ-PTR-ID          PIC S9(015)
                                                         PACKED-DECIMAL.
             10 PI-DRAT0183-NTZ-PTR-ST-NR       PIC S9(009) BINARY.
             10 PI-DRAT0183-NTZ-PTR-PIN         PIC S9(009) BINARY.
             10 PI-DRAT0183-NTZ-PTR-VNR         PIC  X(015).
      *
       01 PO-DRAT0183.
          05 PO-DRAT0183-RC                     PIC  X(003).
             88 PO-DRAT0183-RC-OK                        VALUE 'OK '.
             88 PO-DRAT0183-RC-LEER                      VALUE 'LER'.
             88 PO-DRAT0183-RC-EOF                       VALUE 'EOF'.
          05 PO-DRAT0183-NTZ-DATEN.
             10 PO-DRAT0183-NTZ-ID              PIC S9(015)
                                                         PACKED-DECIMAL.
             10 PO-DRAT0183-KNR-NOTIZ-ART       PIC S9(004) BINARY.
             10 PO-DRAT0183-KENNUNG             PIC  X(008).
             10 PO-DRAT0183-LAST-UPDATE         PIC  X(026).
             10 PO-DRAT0183-S-NOTIZ-NEU         PIC  X(001).
          05 PO-DRAT0183-NTZZ-DATEN.
             10 PO-DRAT0183-ANZAHL              PIC S9(004) BINARY.
             10 PO-DRAT0183-NTZZ-TABELLE        OCCURS 10.
                15 PO-DRAT0183-NTZZ-ZEILE       PIC  X(080).
      *
      ******** Ende Copy-Member **************************************
