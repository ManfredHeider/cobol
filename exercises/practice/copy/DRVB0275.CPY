      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0275.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/MF/CPY/DRVB0275.CPv  $
      *
      *   Rev 2.2   May 06 2002 10:11:22   t13ae23
      *AVB00600 - Anzeigen Ortsnamen -
      *
      *   Rev 2.1   16 Jun 2000 11:27:00   T13AE34
      *AVB00483-Dialog VB51 
      *
      *   Rev 2.0   Feb 27 1999 15:58:40   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   23 Jul 1997 12:25:16   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *   COPY-MEMBER DRVB0275 :                                     *
      *   FUNKTION DRVB0275 - LIST VB NACH PLZ                       *
      *   -------------------------------------------------------    *
      *   - BENUTZT BISHER ALS SCHNITTSTELLE ZWISCHEN DEM BUFFER-OZP *
      *     ZYVB0001 ( DIALOG VB51 -SUCHE VB NACH PLZ ) UND          *
      *     ZJVB0016                                                 *
      *   -------------------------------------------------------    *
      *   ERSTELLT: RAHNER                                           *
      *   DATUM:    22.03.2000                                       *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00485!22.03.00!2.1 !NEU                        ! RAHNER   *
      * AVB00600!06.05.02!2.2 !ANZEIGEN ORTSNAMEN         ! RAHNER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0275                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN ( IN DRVB289  )                *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************

       01 PI-DRVB0275.
          05 PI-DRVB0275-PROGRAMM                PIC  X(008).
          05 PI-DRVB0275-UFKT                    PIC  9(002).

          05 PI-DRVB0275-AUSWAHL-KRITERIEN.
             10 PI-DRVB0275-PLZ                  PIC X(005).
             10 PI-DRVB0275-STUFE-VON            PIC S9(4) USAGE COMP.
             10 PI-DRVB0275-STUFE-BIS            PIC S9(4) USAGE COMP.
             10 PI-DRVB0275-EINHEITEN            PIC 9(008).
             10 PI-DRVB0275-DIRNR                PIC X(003).
             10 PI-DRVB0275-KENNZ                PIC X(001).

       01 PO-DRVB0275.
          05  PO-DRVB0275-RC                     PIC X(003).
              88 PO-DRVB0275-RC-OK               VALUE 'OK '.
              88 PO-DRVB0275-RC-EOF              VALUE 'EOF'.
              88 PO-DRVB0275-RC-LEER             VALUE 'LER'.

          05 PO-DRVB0275-ERGEBNISZEILE.
             10 PO-DRVB0275-PLZ                  PIC X(005).
060502       10 PO-DRVB0275-ORTSNAME             PIC X(015).
             10 PO-DRVB0275-VBNR                 PIC 9(007).
             10 PO-DRVB0275-KNR-AUSL             PIC S9(004) COMP.
             10 PO-DRVB0275-STUFENR              PIC S9(004) COMP.

