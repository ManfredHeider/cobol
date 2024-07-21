      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  01.07.2005
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2005 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1652.CPY  $
      *    $Revision:   2.8  $
      *
      *    $Log:   L:/mf/CPY/DRPA1652.CPv  $
      *
      *   Rev 2.8   Sep 21 2018 12:19:20   t13ex44
      *APA02548: Nachfolgeregelung Direktion Familie
      *
      *   Rev 2.7   Apr 07 2016 10:13:42   T13EX67
      *APA02220: 4 Vor- und Nachkommastellen zur Konditionsermittlu
      *ng - TBPA0023/MSS
      *
      *   Rev 2.6   Jul 15 2005 11:45:38   t13ae23
      *AAN00305: DIT-Aufbauplan
      *
      *   Rev 2.5   Jul 15 2005 10:18:40   t13ae23
      *AAN00305: DIT-Aufbauplan
      *
      *   Rev 2.4   Jul 01 2005 17:14:36   t13ae34
      *AAN00305: DIT-Aufbauplan
      *
      *   Rev 2.3   Jul 01 2005 17:05:44   t13ae34
      *AAN00305: DIT-Aufbauplan
      *
      *   Rev 2.2   Jul 01 2005 16:57:00   t13ae34
      *AAN00305: DIT-Aufbauplan
      *
      *   Rev 2.1   Jul 01 2005 16:36:08   t13ae34
      *AAN00305: DIT-Aufbauplan
      *
      *   Rev 2.0   Jul 01 2005 13:33:46   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1652  FUNKTION LIST-OZP FÜR TABELLE PRKO *
      *                           TBPA0023                           *
      *    PRODUKTGEBER, PRODUKTSCHLÜSSEL, STICHTAG, KTY-KOND-REGEL  *
      *    --------------------------------------------------------  *
      *    ERSTELLT: HEIDER                                          *
      *    DATUM:    01.07.2005                                      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00582!01.07.05!2.1 ! NEUERSTELLUNG             !HEIDER    *
      * APA02220!09.03.16!2.02! ÄNDERUNG FORMAT %-SÄTZE   ! STORCH   *
      * APA02548!26.07.18!2.3 ! ERW AUF 10 SÄTZE UND WERTE! SCHEIDEL *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1652                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1652.
          05 PI-DRPA1652-PROGRAMM            PIC  X(008).
          05 PI-DRPA1652-UFKT-CODE           PIC  9(002).
           05  PI-DRPA1652-AUSWAHL-KRITERIEN.
               10 PI-DRPA1652-PGEB-GES       PIC S9(004)      COMP.
               10 PI-DRPA1652-PROD-PROD-SCHL PIC S9(004)      COMP.
               10 PI-DRPA1652-STICHTAG       PIC  X(010).
               10 PI-DRPA1652-KTY-KOND-REGEL PIC S9(004)      COMP.
      *
       01 PO-DRPA1652.
          05 PO-DRPA1652-RC                  PIC  X(003).
             88 PO-DRPA1652-RC-OK                        VALUE 'OK '.
             88 PO-DRPA1652-RC-LEER                      VALUE 'LER'.
             88 PO-DRPA1652-RC-EOF                       VALUE 'EOF'.
          05 PO-DRPA1652-DATEN.
             10 PO-DRPA1652-KNR-KOND-ART      PIC S9(004)     COMP.
             10 PO-DRPA1652-KTY-KOND-ART      PIC S9(004)     COMP.
             10 PO-DRPA1652-PRKO-KOND-NR      PIC S9(004)     COMP.
             10 PO-DRPA1652-PRKO-GUELTIG      PIC  X(001).
             10 PO-DRPA1652-PRKO-GUELTIG-AB   PIC  X(010).
             10 PO-DRPA1652-PRKO-GUELTIG-BIS  PIC  X(010).
             10 PO-DRPA1652-PRKO-SELKRIT-PA   PIC  X(001).
             10 PO-DRPA1652-PRKO-SELKRIT-LZ   PIC  X(001).
             10 PO-DRPA1652-PRKO-PA-VON       PIC S9(004)     COMP.
             10 PO-DRPA1652-PRKO-PA-BIS       PIC S9(004)     COMP.
             10 PO-DRPA1652-PRKO-LZ-VON       PIC S9(004)     COMP.
             10 PO-DRPA1652-PRKO-LZ-BIS       PIC S9(004)     COMP.
             10 PO-DRPA1652-KNR-KOND-REGEL    PIC S9(004)     COMP.
090316       10 PO-DRPA1652-PRKO-SATZ-1       PIC S9(004)V9999 COMP-3.
090316       10 PO-DRPA1652-PRKO-SATZ-2       PIC S9(004)V9999 COMP-3.
090316       10 PO-DRPA1652-PRKO-SATZ-3       PIC S9(004)V9999 COMP-3.
090316       10 PO-DRPA1652-PRKO-SATZ-4       PIC S9(004)V9999 COMP-3.
090316       10 PO-DRPA1652-PRKO-SATZ-5       PIC S9(004)V9999 COMP-3.
090316       10 PO-DRPA1652-PRKO-SATZ-6       PIC S9(004)V9999 COMP-3.
260718       10 PO-DRPA1652-PRKO-SATZ-7       PIC S9(004)V9999 COMP-3.
260718       10 PO-DRPA1652-PRKO-SATZ-8       PIC S9(004)V9999 COMP-3.
260718       10 PO-DRPA1652-PRKO-SATZ-9       PIC S9(004)V9999 COMP-3.
260718       10 PO-DRPA1652-PRKO-SATZ-10      PIC S9(004)V9999 COMP-3.
             10 PO-DRPA1652-PRKO-WERT-1       PIC S9(009)     COMP.
             10 PO-DRPA1652-PRKO-WERT-2       PIC S9(009)     COMP.
             10 PO-DRPA1652-PRKO-WERT-3       PIC S9(009)     COMP.
             10 PO-DRPA1652-PRKO-WERT-4       PIC S9(009)     COMP.
             10 PO-DRPA1652-PRKO-WERT-5       PIC S9(009)     COMP.
             10 PO-DRPA1652-PRKO-WERT-6       PIC S9(009)     COMP.
260718       10 PO-DRPA1652-PRKO-WERT-7       PIC S9(009)     COMP.
260718       10 PO-DRPA1652-PRKO-WERT-8       PIC S9(009)     COMP.
260718       10 PO-DRPA1652-PRKO-WERT-9       PIC S9(009)     COMP.
260718       10 PO-DRPA1652-PRKO-WERT-10      PIC S9(009)     COMP.
      *
      ***** ENDE-COPY-MEMBER *****************************************
 

