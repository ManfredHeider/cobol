      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1074.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/mf/CPY/DRPA1074.CPv  $
      *
      *   Rev 2.7   Apr 07 2016 10:12:28   T13EX67
      *APA02220: 4 Vor- und Nachkommastellen zur Konditionsermittlu
      *ng - TBPA0023/MSS
      *
      *   Rev 2.6   Jun 19 2012 09:28:12   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.5   May 21 2012 08:53:02   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.4   Dec 10 2007 12:21:04   t13ex21
      *APA00622: Programmänderungen Martina
      *
      *   Rev 2.3   Aug 06 2003 09:29:34   t13ex21
      *APA00245 - Erweiterung PI/PO-Bereich
      *
      *   Rev 2.2   May 07 2003 14:23:08   t13ex21
      *APA00234 - Erweiterung PI-Bereich
      *
      *   Rev 2.1   23 Oct 2000 12:18:44   T13EX21
      *APA00105 - PI-Bereich erweitert
      *
      *   Rev 2.0   May 26 1999 15:52:24   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   20 Jul 1998 10:20:12   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1074 FUNKTION PE-STRUKTUR PRÜFEN (DAISY) *
      *                          FÜR UWPA0005                        *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STORCH                                          *
      *    DATUM:    21.09.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  ! VER ! AENDERUNG                 ! NAME    *
      * -------------------------------------------------------------*
      * APA00012!21.09.98! 01  ! NEU                       ! STORCH  *
      *         !03.12.98!     ! ERWEITERUNG PI-BEREICH    ! STORCH  *
      * APA00105!23.10.00!2.01 ! ERWEITERUNG UM KOND-REGEL ! STORCH  *
      * APA00234!05.05.03!2.02 ! ERWEITERUNG UM PI-DGV     ! STORCH  *
      * APA00245!28.07.03!2.03 ! ERWEITERUNG PI/PO-BEREICH ! STORCH  *
      * APA00622!12.06.07!2.04 ! ERWEITERUNG PI-BEREICH    ! STORCH  *
      * APA01101!29.02.12!2.05 ! VGES DURCH MANDANT ERSETZT! STORCH   *
      * APA02220!10.03.16!2.06 ! %-SATZ GEÄNDERT          !  STORCH  *
      *         !        !     !                           !         *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1074                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1074.
          05 PI-DRPA1074-PZVP-VERSION          PIC S9(004) BINARY.
          05 PI-DRPA1074-NUTZUNGS-BED.
100316*      10 KDBETR-DIFF-PROV-STZ           PIC S9(003)V999
100316       10 KDBETR-DIFF-PROV-STZ           PIC S9(004)V9999
                                                  PACKED-DECIMAL.
100316*      10 KDBETR-DIFF-PROV-STZ-2         PIC S9(003)V999
100316       10 KDBETR-DIFF-PROV-STZ-2         PIC S9(004)V9999
                                                  PACKED-DECIMAL.
             10 PVPT-KDBETR-REDUZ              PIC  X(001).
             10 KNR-KOND-REGEL-KDB             PIC S9(004) BINARY.
             10 PE-STRUKTUR-ERF-DAT            PIC  X(010).
             10 S-PE-STRUKTUR-AP-FP            PIC  X(002).
             10 S-PE-STRUKTUR-ABGELEHNTES-EVS  PIC  X(001).
             10 S-PE-STRUKTUR-N-DURCHST-PROV   PIC  X(001).
             10 STICHTAG-FAELLIG-VON           PIC  X(010).
             10 S-PE-STRUKTUR-DGV              PIC S9(004) BINARY.
             10 KNR-MANDANT                    PIC S9(004) BINARY.

       01 PO-DRPA1074.
          05 PO-DRPA1074-RC                    PIC  X(003).
             88 PO-DRPA1074-RC-OK                       VALUE 'OK '.
             88 PO-DRPA1074-RC-NOT-OK                   VALUE 'NOK'.
          05 PO-DRPA1074-DATEN.
             10 PO-DRPA1074-PVPT-KDBETR-REDUZ   PIC  X(001).
             10 PO-DRPA1074-VTAB-MAX            PIC  9(002).
             10 PO-DRPA1074-VSTAT-TABELLE.
                15 PO-DRPA1074-VSTAT-TAB       OCCURS 20
                                  INDEXED BY PO-DRPA1074-VTAB-IND.
                   20 PO-DRPA1074-VTAB-VSTAT       PIC  X(004).
                   20 PO-DRPA1074-VTAB-VSTAT-UNR   PIC S9(004) BINARY.
                   20 PO-DRPA1074-VTAB-ORT         PIC  X(030).
                   20 PO-DRPA1074-VTAB-BEREICH     PIC  X(010).

      ******** ENDE COPY-MEMBER **************************************

