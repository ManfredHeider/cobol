      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0140.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRPA0140.CPv  $
      *
      *   Rev 2.4   23 Aug 2007 09:52:16   t13ae46
      *APA00639: Erweiterung der PIN auf 9 Stellen
      *
      *   Rev 2.3   Nov 08 2006 09:11:48   t13ex44
      *APA00452: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.2   Feb 23 2000 13:58:20   T13EX21
      *APA00081 - Erweiterung um PGEB-GES
      *
      *   Rev 2.1   Oct 12 1999 10:05:42   T13EX21
      *APA00052 - Neuerstellung
      *
      *   Rev 2.0   Feb 27 1999 14:27:58   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Feb 19 1997 10:19:06   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0140                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF LIST ZU PVER UND PVPT                      *
      *    --------------------------------------------------------   *
      *    ERSTELLT: M. STORCH                                        *
      *    DATUM:    21.09.1999                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00052!21.09.99!    ! NEU                       ! STORCH   *
      * APA00081!03.02.00!    ! PI-PIN HINZUGEFÜGT        ! STORCH   *
      * APA00452!01.11.06! 2.3! DREIST. PRODUKTSCHLUESSEL ! SCHEIDEL *
      * APA00639!27.06.07! 2.4! ERW. DER PIN AUF 9 STELLEN! WANG     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0140                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0140.
          05 PI-DRPA0140-PROGRAMM            PIC  X(008).
          05 PI-DRPA0140-UFKT                PIC  9(002).
          05 PI-DRPA0140-SCHALTER            PIC  9.
          05 PI-DRPA0140-AUSWAHLKRITERIEN.
             10 PI-DRPA0140-PGEB-GES         PIC 9(002).
             10 PI-DRPA0140-NAME             PIC X(025).
             10 PI-DRPA0140-S-NAME           PIC X(001).
             10 PI-DRPA0140-VORNAME          PIC X(020).
             10 PI-DRPA0140-S-VORNAME        PIC X(001).
011106*      10 PI-DRPA0140-PRODUKT          PIC 9(002).
011106       10 PI-DRPA0140-PROD-PROD-SCHL   PIC S9(004) BINARY.
             10 PI-DRPA0140-S-PRODUKT        PIC X(001).
             10 PI-DRPA0140-ZEITRAUM-VON     PIC X(010).
             10 PI-DRPA0140-ZEITRAUM-BIS     PIC X(010).
             10 PI-DRPA0140-S-ZEITRAUM       PIC X(001).
             10 PI-DRPA0140-AVERM            PIC 9(007).
             10 PI-DRPA0140-S-AVERM          PIC X(001).
030200       10 PI-DRPA0140-PIN              PIC S9(009) USAGE COMP.
030200       10 PI-DRPA0140-S-PIN            PIC X(001).
      *
       01 PO-DRPA0140.
          05 PO-DRPA0140-RC                  PIC X(003).
             88 PO-DRPA0140-RC-OK                VALUE 'OK '.
             88 PO-DRPA0140-RC-EOF               VALUE 'EOF'.
             88 PO-DRPA0140-RC-LEER              VALUE 'LER'.
          05 PO-DRPA0140-ANZAHL-ERG-ZEILEN   PIC  9(05).

          05 PO-DRPA0140-ERGEBNISZEILE.
               10 PO-DRPA0140-VNR            PIC X(015).
270607*        10 PO-DRPA0140-PIN            PIC 9(007).
270607         10 PO-DRPA0140-PIN            PIC 9(009).
               10 PO-DRPA0140-NAME           PIC X(025).
               10 PO-DRPA0140-VNAME          PIC X(020).
               10 PO-DRPA0140-AVERM          PIC 9(007) COMP.
011106*        10 PO-DRPA0140-PRODSCHL       PIC S9(04) COMP.
011106         10 PO-DRPA0140-PROD-PROD-SCHL PIC S9(04) BINARY.
               10 PO-DRPA0140-VERTR-BEG      PIC X(010).
               10 PO-DRPA0140-PRINTE         PIC S9(015)V USAGE COMP-3.
