      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1057.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRPA1057.CPv  $
      *
      *   Rev 2.4   Apr 07 2016 10:12:18   T13EX67
      *APA02220: 4 Vor- und Nachkommastellen zur Konditionsermittlu
      *ng - TBPA0023/MSS
      *
      *   Rev 2.3   Aug 12 2003 10:00:42   t13ex21
      *APA00245 - Erweiterung PO-Bereich
      *
      *   Rev 2.2   May 07 2003 14:22:50   t13ex21
      *APA00234 - Erweiterung PI-Bereich
      *
      *   Rev 2.1   23 Oct 2000 12:18:16   T13EX21
      *APA00105 - PO-Bereich erweitert
      *
      *   Rev 2.0   Mar 25 1999 14:09:58   T13AE18
      *APA00012 Einführung EVS-Umbuchungs-
      *dialog
      *
      *   Rev 1.0   14 Jul 1998 14:01:24   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1057 FUNKTION GET PE-SRUKTUR             *
      *                          MITTELS PEST-ID                     *
      *                          FÜR PROVISIONSDATENMODUL UGPA0002   *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    07.09.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!07.09.98! 01  !NEU                       !BOHLINGER *
      *         !06.10.98!01.02!PRZ-TABELLE EINGEFÜGT     !BOHLINGER *
      *         !16.10.98!01.03!S-PDPT-ID   EINGEFÜGT     !BOHLINGER *
      *         !27.10.98!01.04!STRUKTUR-ERF-DAT AUSGE-   !BOHLINGER *
      *         !        !     !STERNT, DA IN PEST ENTHALTEN         *
      *         !03.12.98!01.05!STICHTAG EINGEFÜGT        !BOHLINGER *
      *         !        !     !                          !          *
      * APA00105!23.10.00!2.01 ! ERWEITERUNG UM KOND-REGEL!  STORCH  *
      * APA00234!05.05.03!2.02 ! ERWEITERUNG UM PO-DGV    !  STORCH  *
      * APA00245!28.07.03!2.03 ! ERWEITERUNG UM           !  STORCH  *
      *         !        !     ! PO-KDBETR-DIFF-PROV-STZ2 !          *
      * APA02220!10.03.16!2.04 ! %-SATZ GEÄNDERT          !  STORCH  *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1057                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1057.
             10 PI-DRPA1057-PRINTE             PIC S9(015)
                                               PACKED-DECIMAL.
             10 PI-DRPA1057-PEST-ID            PIC S9(015)
                                               PACKED-DECIMAL.
             10 PI-DRPA1057-PDPT-ID            PIC S9(015)
                                               PACKED-DECIMAL.
             10 PI-DRPA1057-S-PDPT-ID          PIC  X(001).

       01 PO-DRPA1057.
          05 PO-DRPA1057-RC                    PIC  X(003).
             88 PO-DRPA1057-RC-OK                       VALUE 'OK '.
             88 PO-DRPA1057-RC-NOT-FOUND                VALUE 'NFD'.

          05 PO-DRPA1057-MELDUNG.
             10 PO-DRPA1057-MELDUNG-TYP        PIC  X(001).
             10 PO-DRPA1057-MELDUNG-NR         PIC S9(004) BINARY.

          05 PO-DRPA1057-DATEN.
             COPY DRPA0433.

          05 PO-DRPA1057-PRZ-TABELLE           OCCURS 20.
             10 ST-NR-PRZ                      PIC X(001).

          05 PO-DRPA1057-NUTZUNGS-BED.
             10 KDBETR-DIFF-PROV-STZ           PIC S9(003)V999
                                                  PACKED-DECIMAL.
100316*      10 KDBETR-DIFF-PROV-STZ-2         PIC S9(003)V999
100316       10 KDBETR-DIFF-PROV-STZ-2         PIC S9(004)V9999
                                                  PACKED-DECIMAL.
             10 KNR-KOND-REGEL-KDB             PIC S9(004) BINARY.
             10 STICHTAG                       PIC  X(010).
             10 S-PE-STRUKTUR-AP-FP            PIC  X(002).
             10 S-PE-STRUKTUR-ABGELEHNTES-EVS  PIC  X(001).
             10 S-PE-STRUKTUR-N-DURCHST-PROV   PIC  X(001).
             10 S-PE-STRUKTUR-DGV              PIC S9(004) BINARY.
             10 PVPT-KDBETR-REDUZ              PIC  X(001).
      *
      ******** Ende Copy-Member **************************************

