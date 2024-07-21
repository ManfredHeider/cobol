      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1116.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRPA1116.CPv  $
      *
      *   Rev 2.3   Sep 21 2018 12:18:02   t13ex44
      *APA02548: Nachfolgeregelung Direktion Familie
      *
      *   Rev 2.2   Apr 07 2016 10:12:36   T13EX67
      *APA02220: 4 Vor- und Nachkommastellen zur Konditionsermittlu
      *ng - TBPA0023/MSS
      *
      *   Rev 2.1   Apr 03 2000 11:26:14   T13EX25
      *APA00089, Wert-2 bis Wert-6 neu
      *
      *   Rev 2.0   May 26 1999 13:51:52   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   21 Oct 1998 13:57:52   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1116                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA1116                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                        *
      *    DATUM:    05.11.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0020!05.11.98! 01 ! NEU                       ! BOHLINGER*
      * APA00089!31.03.00! 02 ! WERT-2 BIS WERT-6 NEU     ! JÄGER    *
      * APA02220!09.03.16!2.02! ÄNDERUNG FORMAT %-SÄTZE   ! STORCH   *
      * APA02548!26.07.18!2.3 ! NACHF. DIREKTION FAMILIE  ! SCHEIDEL *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA1116
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    FUNKTION DRPA1116 DES BUFFER-OZP ZYPA0027 ERMITTELT
      *    DIE ERGEBNISMENGE FüR PRODUKTUPRO TABELLE PRKO
      *
      ****************************************************************
      *
       01  PI-DRPA1116.
           05  PI-DRPA1116-UFKT                PIC  9(02).
           05  PI-DRPA1116-LOG-SATZ-NR         PIC  9(05).
      *
       01  PO-DRPA1116.
           05  PO-DRPA1116-RC                  PIC  X(03).
               88 PO-DRPA1116-RC-OK                        VALUE 'OK '.
               88 PO-DRPA1116-RC-EOF                       VALUE 'EOF'.
               88 PO-DRPA1116-RC-LEER                      VALUE 'LER'.
           05  PO-DRPA1116-ANZAHL-ERG-ZEILEN   PIC  9(05).
 
           05  PO-DRPA1116-ERGEBNISZEILE.
               10 PO-DRPA1116-PGEB-GES          PIC S9(004)     COMP.
               10 PO-DRPA1116-PROD-PROD-SCHL    PIC S9(004)     COMP.
               10 PO-DRPA1116-KNR-KOND-ART      PIC S9(004)     COMP.
               10 PO-DRPA1116-KTY-KOND-ART      PIC S9(004)     COMP.
               10 PO-DRPA1116-PRKO-KOND-NR      PIC S9(004)     COMP.
               10 PO-DRPA1116-PRKO-GUELTIG      PIC  X(001).
               10 PO-DRPA1116-PRKO-GUELTIG-AB   PIC  X(010).
               10 PO-DRPA1116-PRKO-GUELTIG-BIS  PIC  X(010).
               10 PO-DRPA1116-PRKO-SELKRIT-PA   PIC  X(001).
               10 PO-DRPA1116-PRKO-SELKRIT-LZ   PIC  X(001).
               10 PO-DRPA1116-PRKO-PA-VON       PIC S9(004)     COMP.
               10 PO-DRPA1116-PRKO-PA-BIS       PIC S9(004)     COMP.
               10 PO-DRPA1116-PRKO-LZ-VON       PIC S9(004)     COMP.
               10 PO-DRPA1116-PRKO-LZ-BIS       PIC S9(004)     COMP.
               10 PO-DRPA1116-KNR-KOND-REGEL    PIC S9(004)     COMP.
               10 PO-DRPA1116-KTY-KOND-REGEL    PIC S9(004)     COMP.
090316         10 PO-DRPA1116-PRKO-SATZ-1       PIC S9(004)V9999 COMP-3.
090316         10 PO-DRPA1116-PRKO-SATZ-2       PIC S9(004)V9999 COMP-3.
090316         10 PO-DRPA1116-PRKO-SATZ-3       PIC S9(004)V9999 COMP-3.
090316         10 PO-DRPA1116-PRKO-SATZ-4       PIC S9(004)V9999 COMP-3.
090316         10 PO-DRPA1116-PRKO-SATZ-5       PIC S9(004)V9999 COMP-3.
090316         10 PO-DRPA1116-PRKO-SATZ-6       PIC S9(004)V9999 COMP-3.
260718         10 PO-DRPA1116-PRKO-SATZ-7       PIC S9(004)V9999 COMP-3.
260718         10 PO-DRPA1116-PRKO-SATZ-8       PIC S9(004)V9999 COMP-3.
260718         10 PO-DRPA1116-PRKO-SATZ-9       PIC S9(004)V9999 COMP-3.
260718         10 PO-DRPA1116-PRKO-SATZ-10      PIC S9(004)V9999 COMP-3.
               10 PO-DRPA1116-PRKO-WERT-1       PIC S9(009)     COMP.
               10 PO-DRPA1116-PRKO-WERT-2       PIC S9(009)     COMP.
               10 PO-DRPA1116-PRKO-WERT-3       PIC S9(009)     COMP.
               10 PO-DRPA1116-PRKO-WERT-4       PIC S9(009)     COMP.
               10 PO-DRPA1116-PRKO-WERT-5       PIC S9(009)     COMP.
               10 PO-DRPA1116-PRKO-WERT-6       PIC S9(009)     COMP.
260718         10 PO-DRPA1116-PRKO-WERT-7       PIC S9(009)     COMP.
260718         10 PO-DRPA1116-PRKO-WERT-8       PIC S9(009)     COMP.
260718         10 PO-DRPA1116-PRKO-WERT-9       PIC S9(009)     COMP.
260718         10 PO-DRPA1116-PRKO-WERT-10      PIC S9(009)     COMP.
      *
      *   BEI ERWEITERUNG TSQ-LÄNGE IN ZYPA0027 ANPASSEN
      ******* ENDE COPY-MEMBER **************************************
 

