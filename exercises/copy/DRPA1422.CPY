      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1422.CPY  $
      *    $Revision:   2.20  $
      *
      *    $Log:   L:/mf/CPY/DRPA1422.CPv  $
      *
      *   Rev 2.20   Sep 06 2017 11:09:00   t13ex24
      *APA02504: Ges 48- Vorbewegungsermittlung nach T-Nummern
      *
      *   Rev 2.19   Mar 30 2017 10:44:24   t13ae37
      *APA02353: Neuer Produktpartner TeleSon
      *
      *   Rev 2.18   Apr 07 2016 10:12:54   T13EX67
      *APA02220: 4 Vor- und Nachkommastellen zur Konditionsermittlu
      *ng - TBPA0023/MSS
      *
      *   Rev 2.17   Dec 17 2015 08:28:28   t13ex24
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System
      *
      *   Rev 2.16   Jul 01 2015 11:38:38   t13ex24
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System
      *PVW-An
      *passungen für neue PGV's 1052, 4052
      *
      *   Rev 2.15   May 15 2015 11:39:16   t13ex44
      *APA02038: 20-VEP: laufende Provision
      *
      *   Rev 2.14   Jan 29 2015 13:30:08   t13ex44
      *APA01963: Ges 20- Änderung der Basistabelle und Vorbewegungs
      *ermittlung
      *
      *   Rev 2.13   Jan 14 2015 10:57:30   t13ex44
      *APA01963: Ges 20- Änderung der Basistabelle und Vorbewegungs
      *ermittlung
      *
      *   Rev 2.12   Mar 25 2014 13:06:18   t13ex72
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g):
      *
      *   Rev 2.11   Jan 10 2014 13:19:48   t13ex72
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g): GeVo Leben (CBO)
      *
      *   Rev 2.10   Jun 13 2012 07:42:52   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.9   Jun 05 2012 08:49:00   t13ae37
      *FPA01271: Ermittlung des PGV-KZ bei Vorbewegung korrigieren
      *
      *   Rev 2.8   May 21 2012 08:55:44   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.7   Mar 30 2012 12:45:24   t13ae37
      *APA01189: vertriebsbezogene Vorbewegungsermittlung bei Belas
      *tungen
      *
      *   Rev 2.6   May 24 2011 14:10:04   t13ae37
      *APA01189: vertriebsbezogene Vorbewegungsermittlung bei Belas
      *tungen
      *
      *   Rev 2.5   Feb 25 2003 16:14:22   t13ex25
      *FPA00453; Suchfunktion erweitert
      *
      *   Rev 2.4   Aug 29 2002 12:36:36   t13ex22
      *APA00206: Haftungsdauerberechnung für Generali Leben
      *
      *   Rev 2.3   Feb 05 2002 17:37:46   t13ex43
      *APA00165 : GeVo WIK als Folgezugang
      *
      *   Rev 2.2   26 Sep 2000 11:04:52   T13EX21
      *APA000105 - Erweiterung Zwischenstufe
      *
      *   Rev 2.1   25 Sep 2000 12:15:44   T13EX21
      *APA00105 - Neuerstellung
      *
      *   Rev 2.0   12 Jul 2000 12:32:50   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1422 ERMITTELN DER PE-STRUKTUR UND DGV    *
      *                          DER VORBEWEGUNG                      *
      *    -----------------------------------------------------------*
      *    ERSTELLT: STORCH                                           *
      *    DATUM:    22.09.2000                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00105!22.09.00!2.01! NEUERSTELLUNG             ! STORCH   *
      * APA00165!31.01.02!2.03! PG-GSMT-PROV HINZU        ! KLEEMEIER*
      * APA00206!23.08.02!2.04! EH-ERMITTLUNG ERGÄNZT     ! MUND     *
      * FPA00453!25.02.03!2.05! SUCHFUNKTION ERWEITERT    ! JÄGER    *
      * APA01189!08.03.11!2.6 ! VORBEWEGUNGSERMITTLUNG MIT! BUCH     *
      *         !        !    ! VERTRIEBSGESELLSCHAFT     !          *
      * APA01101!10.04.12!2.7 ! VGES DURCH MANDANT ERSETZT! STORCH   *
      *         !        !    ! *-BEREICHE ENTFERNT       !          *
      * FPA01271!04.06.12!2.9 ! FÜR KORREKTUR VGES NÖTIG  ! BUCH     *
      * APA01101!13.06.12!2.10! STAND 2.8 WIEDER AKTIVIERT! STORCH   *
      * APA01526!24.04.13!2.11! GeVo Leben                ! BÖTTCHER *
      * APA01526!24.10.13!2.12! BEGR.SÄTZE KOLLEKTIV      ! BÖTTCHER *
      * APA01963!05.01.15!2.13! NEUER RC "NSK"            ! SCHEIDEL *
      *         !        !    ! ERW. PGV-VORB-DATEN       !          *
      * APA02038!05.05.15!2.15! ERW. FUER PGV 1941        ! SCHEIDEL *
      * APA01932!19.05.15!2.16! ERWEITERUNG LUCY FÜR NEUE ! KÖPRÜCÜ  *
      *         !        !    ! VORBEWEGUNGSERMITTLUN     !          *
      * APA01932!16.12.15!2.17! ERWEITERUNG PO-BEREICH    ! KÖPRÜCÜ  *
      * APA02220!09.03.16!2.18! ÄNDERUNG FORMAT %-SÄTZE   ! STORCH   *
      * APA02353!30.03.17!2.19! Teleson                   ! Buch     *
      * APA02504!04.09.17!2.20! ERWEITERUNG PI-BEREICH    ! KÖPRÜCÜ  *
      * DSY04126!21.12.21!    ! PO-DRPA1422-PMPR-NEUST-KZ ! BUCH     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1422                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1422.
          05 PI-DRPA1422-SUCHFUNKTION           PIC  9(02).
          05 PI-DRPA1422-GP-ERMITTLUNG          PIC  X(01).
 
          05 PI-DRPA1422-DATEN.
             10 PI-DRPA1422-PGEB-GES            PIC S9(04) COMP.
             10 PI-DRPA1422-PRINTE              PIC S9(15) COMP-3.
             10 PI-DRPA1422-FAELLIG-VON         PIC  X(10).
             10 PI-DRPA1422-PROD-PROD-SCHL      PIC S9(04) COMP.
             10 PI-DRPA1422-PVPT-ID             PIC S9(15) COMP-3.
             10 PI-DRPA1422-PVPT-VERSION        PIC S9(04) COMP.
             10 PI-DRPA1422-PG-GSMT-PROV      PIC S9(09)V9(02) COMP-3.
290212       10 PI-DRPA1422-KNR-MANDANT         PIC S9(04) BINARY.
             10 PI-DRPA1422-PGV-KZ              PIC  X(01).
                88 PI-DRPA1422-PGV-GUTSCHRIFT   VALUE 'G'.
                88 PI-DRPA1422-PGV-BELASTUNG    VALUE 'B'.
240413       10 PI-DRPA1422-UNR                 PIC S9(04) COMP.
240413       10 PI-DRPA1422-SCHICHT-NR          PIC S9(04) COMP.
240413       10 PI-DRPA1422-PGV                 PIC S9(04) COMP.
240413       10 PI-DRPA1422-PMPE-SAM-NR         PIC S9(09) COMP.
050115*      10 PI-DRPA1422-PGV-VORB-DATEN      PIC  X(50).
050115       10 PI-DRPA1422-PGV-VORB-DATEN      PIC  X(200).
241013       10 PI-DRPA1422-URSPR-POL-DAT       PIC  X(10).
050515       10 PI-DRPA1422-HIST-UNR            PIC S9(04) COMP.
050515       10 PI-DRPA1422-KZ-PROD-NEU         PIC  X(01).
300317*      10 PI-DRPA1422-PMBE-PG-RECH-NR     PIC  X(12).
300317       10 PI-DRPA1422-PMBE-PG-RECH-NR     PIC  X(30).
040917       10 PI-DRPA1422-TNR-KZ              PIC  X(01).
040917          88 PI-DRPA1422-TNR-KZ-VORH      VALUE 'T'.
040917          88 PI-DRPA1422-TNR-KZ-VORH-NEIN VALUE ' '.
040917       10 PI-DRPA1422-T-NUMMER            PIC  X(20).
      *
       01 PO-DRPA1422.
          05 PO-DRPA1422-RC                     PIC  X(03).
             88 PO-DRPA1422-RC-OK               VALUE 'OK '.
             88 PO-DRPA1422-RC-NOT-FOUND        VALUE 'NFD'.
240413       88 PO-DRPA1422-RC-UNZULAESSIG      VALUE 'UNZ'.
050115       88 PO-DRPA1422-RC-NSK              VALUE 'NSK'.
190515       88 PO-DRPA1422-RC-RNR              VALUE 'RNR'.
          05 PO-DRPA1422-DATEN.
             10 PO-DRPA1422-PEST-ID             PIC S9(15)V COMP-3.
             10 PO-DRPA1422-DGV                 PIC S9(04) COMP.
             10 PO-DRPA1422-PMPT-FAELLIG-VON    PIC X(10).
             10 PO-DRPA1422-PMPT-GRUND-PROV     PIC S9(9)V99 COMP-3.
             10 PO-DRPA1422-PMPT-GSMT-PROV      PIC S9(9)V99 COMP-3.
             10 PO-DRPA1422-PMPT-EINH           PIC S9(6)V99 COMP-3.
240413*      10 PO-DRPA1422-PMPT-MA-GRD-PR-STZ  PIC S9(03)V999 COMP-3.
100316       10 PO-DRPA1422-PMPT-MA-GRD-PR-STZ  PIC S9(04)V9999 COMP-3.
240413       10 PO-DRPA1422-PMPR-PROD-SCHL      PIC S9(04) COMP.
240413       10 PO-DRPA1422-PMPR-PGV            PIC S9(04) COMP.
240413       10 PO-DRPA1422-PGV-1900-D-I        PIC  X(01).
211221       10 PO-DRPA1422-PMPR-NEUST-KZ       PIC  X(01).
161215    05 PO-DRPA1422-STEUSTAT.
161215       COPY DRPA1843.
      *
      ******* ENDE COPY-MEMBER **************************************
 
 
 
 

