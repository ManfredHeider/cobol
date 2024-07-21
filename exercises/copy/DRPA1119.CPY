      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1119.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRPA1119.CPv  $
      *
      *   Rev 2.3   Sep 21 2018 12:18:42   t13ex44
      *APA02548: Nachfolgeregelung Direktion Familie
      *
      *   Rev 2.2   Apr 07 2016 10:12:46   T13EX67
      *APA02220: 4 Vor- und Nachkommastellen zur Konditionsermittlu
      *ng - TBPA0023/MSS
      *
      *   Rev 2.1   Apr 03 2000 11:26:34   T13EX25
      *APA00089, Wert-2 bis Wert-6 neu
      *
      *   Rev 2.0   May 25 1999 14:06:04   T13AE18
      *apa00012
      *
      *   Rev 1.0   21 Oct 1998 13:58:02   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1119  FUNKTION LIST-OZP FÜR TABELLE PRKO *
      *                                                 TBPA0023     *
      *        ERMITTELN DER KONDITIONEN FüR ALLE GESELLSCHAFTEN     *
      *    --------------------------------------------------------  *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    05.11.1998                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0020!05.11.98! 01 ! NEUERSTELLUNG             !BOHLINGER *
      * APA00089!31.03.00! 02 ! WERT-2 BIS WERT-6 NEU     !JÄGER     *
      * APA02220!09.03.16!2.02! ÄNDERUNG FORMAT %-SÄTZE   ! STORCH   *
      * APA02548!26.07.18!2.3 ! ERW AUF 10 SÄTZE U. WERTE ! SCHEIDEL *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1119                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1119.
          05 PI-DRPA1119-PROGRAMM            PIC  X(008).
          05 PI-DRPA1119-UFKT                PIC  9(002).
      *
       01 PO-DRPA1119.
          05 PO-DRPA1119-RC                  PIC  X(003).
             88 PO-DRPA1119-RC-OK                        VALUE 'OK '.
             88 PO-DRPA1119-RC-LEER                      VALUE 'LER'.
             88 PO-DRPA1119-RC-EOF                       VALUE 'EOF'.
          05 PO-DRPA1119-DATEN.
             10 PO-DRPA1119-PGEB-GES          PIC S9(004)     COMP.
             10 PO-DRPA1119-PROD-PROD-SCHL    PIC S9(004)     COMP.
             10 PO-DRPA1119-KNR-KOND-ART      PIC S9(004)     COMP.
             10 PO-DRPA1119-KTY-KOND-ART      PIC S9(004)     COMP.
             10 PO-DRPA1119-PRKO-KOND-NR      PIC S9(004)     COMP.
             10 PO-DRPA1119-PRKO-GUELTIG      PIC  X(001).
             10 PO-DRPA1119-PRKO-GUELTIG-AB   PIC  X(010).
             10 PO-DRPA1119-PRKO-GUELTIG-BIS  PIC  X(010).
             10 PO-DRPA1119-PRKO-SELKRIT-PA   PIC  X(001).
             10 PO-DRPA1119-PRKO-SELKRIT-LZ   PIC  X(001).
             10 PO-DRPA1119-PRKO-PA-VON       PIC S9(004)     COMP.
             10 PO-DRPA1119-PRKO-PA-BIS       PIC S9(004)     COMP.
             10 PO-DRPA1119-PRKO-LZ-VON       PIC S9(004)     COMP.
             10 PO-DRPA1119-PRKO-LZ-BIS       PIC S9(004)     COMP.
             10 PO-DRPA1119-KNR-KOND-REGEL    PIC S9(004)     COMP.
             10 PO-DRPA1119-KTY-KOND-REGEL    PIC S9(004)     COMP.
090316       10 PO-DRPA1119-PRKO-SATZ-1       PIC S9(004)V9999 COMP-3.
090316       10 PO-DRPA1119-PRKO-SATZ-2       PIC S9(004)V9999 COMP-3.
090316       10 PO-DRPA1119-PRKO-SATZ-3       PIC S9(004)V9999 COMP-3.
090316       10 PO-DRPA1119-PRKO-SATZ-4       PIC S9(004)V9999 COMP-3.
090316       10 PO-DRPA1119-PRKO-SATZ-5       PIC S9(004)V9999 COMP-3.
090316       10 PO-DRPA1119-PRKO-SATZ-6       PIC S9(004)V9999 COMP-3.
260718       10 PO-DRPA1119-PRKO-SATZ-7       PIC S9(004)V9999 COMP-3.
260718       10 PO-DRPA1119-PRKO-SATZ-8       PIC S9(004)V9999 COMP-3.
260718       10 PO-DRPA1119-PRKO-SATZ-9       PIC S9(004)V9999 COMP-3.
260718       10 PO-DRPA1119-PRKO-SATZ-10      PIC S9(004)V9999 COMP-3.
             10 PO-DRPA1119-PRKO-WERT-1       PIC S9(009)     COMP.
             10 PO-DRPA1119-PRKO-WERT-2       PIC S9(009)     COMP.
             10 PO-DRPA1119-PRKO-WERT-3       PIC S9(009)     COMP.
             10 PO-DRPA1119-PRKO-WERT-4       PIC S9(009)     COMP.
             10 PO-DRPA1119-PRKO-WERT-5       PIC S9(009)     COMP.
             10 PO-DRPA1119-PRKO-WERT-6       PIC S9(009)     COMP.
260718       10 PO-DRPA1119-PRKO-WERT-7       PIC S9(009)     COMP.
260718       10 PO-DRPA1119-PRKO-WERT-8       PIC S9(009)     COMP.
260718       10 PO-DRPA1119-PRKO-WERT-9       PIC S9(009)     COMP.
260718       10 PO-DRPA1119-PRKO-WERT-10      PIC S9(009)     COMP.
      *
      ***** Ende-Copy-Member *****************************************
 

