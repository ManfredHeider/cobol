      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1424.CPY  $
      *    $Revision:   2.18  $
      *
      *    $Log:   L:/mf/CPY/DRPA1424.CPv  $
      *
      *   Rev 2.18   Sep 06 2017 11:09:16   t13ex24
      *APA02504: Ges 48- Vorbewegungsermittlung nach T-Nummern
      *
      *   Rev 2.17   Mar 30 2017 10:44:32   t13ae37
      *APA02353: Neuer Produktpartner TeleSon
      *
      *   Rev 2.16   Apr 07 2016 11:33:12   T13ex24
      *APA02220: 4 Vor- und Nachkommastellen zur Konditionsermittlung
      *
      *   Rev 2.15   Dec 17 2015 08:28:20   t13ex24
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System
      *
      *   Rev 2.14   Nov 19 2015 17:32:44   t13ex67
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System
      *
      *   Rev 2.13   May 26 2015 09:43:04   t13ex24
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System
      *PVW-An
      *passungen für neue PGV's 1052, 4052
      *
      *   Rev 2.12   Jan 29 2015 13:30:12   t13ex44
      *APA01963: Ges 20- Änderung der Basistabelle und Vorbewegungs
      *ermittlung
      *
      *   Rev 2.11   Jan 14 2015 10:57:34   t13ex44
      *APA01963: Ges 20- Änderung der Basistabelle und Vorbewegungs
      *ermittlung
      *
      *   Rev 2.10   Mar 25 2014 13:06:32   t13ex72
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g):
      *
      *   Rev 2.9   Jan 10 2014 13:21:12   t13ex72
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g): GeVo Leben (CBO)
      *
      *   Rev 2.8   Jun 19 2012 09:30:56   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.7   May 21 2012 08:55:52   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.6   Mar 30 2012 12:45:18   t13ae37
      *APA01189: vertriebsbezogene Vorbewegungsermittlung bei Belas
      *tungen
      *
      *   Rev 2.5   May 24 2011 14:10:10   t13ae37
      *APA01189: vertriebsbezogene Vorbewegungsermittlung bei Belas
      *tungen
      *
      *   Rev 2.4   Feb 25 2003 16:15:16   t13ex25
      *FPA00453; Suchfunktion erweitert
      *
      *   Rev 2.3   Aug 29 2002 12:36:36   t13ex22
      *APA00206: Haftungsdauerberechnung für Generali Leben
      *
      *   Rev 2.2   Feb 05 2002 17:37:56   t13ex43
      *APA00165 : GeVo WIK als Folgezugang
      *
      *   Rev 2.1   26 Sep 2000 11:05:20   T13EX21
      *APA00105 - Neuerstellung
      *
      *   Rev 2.0   12 Jul 2000 12:32:52   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1424 ERMITTELN DER PE-STRUKTUR UND DGV    *
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
      * APA00165!23.01.02!2.02! PG-GSMT-PROV HINZU        ! KLEEMEIER*
      * APA00206!23.08.02!2.03! EH- UND FÄLLIGKEITSERM.BEI! MUND     *
      *         !        !    ! FKT. DRPA1424             !          *
      * FPA00453!25.03.02!2.04! SUCHFUNKTION ERWEITERT    ! JÄGER    *
      * APA01189!08.03.11!2.5 ! VORBEWEGUNGSERMITTLUNG MIT! BUCH     *
      *         !        !    ! VERTRIEBSGESELLSCHAFT     !          *
      * APA01101!10.04.12!2.7 ! VGES DURCH MANDANT ERSETZT! STORCH   *
      *         !        !    ! *-BEREICHE ENTFERNT       !          *
      * APA01526!24.04.13!2.9 ! GeVo Leben                ! BÖTTCHER *
      * APA01963!05.01.15!2.11! NEUER RC "NSK"            ! SCHEIDEL *
      *         !        !    ! ERW. PGV-VORB-DATEN       !          *
      * APA01932!19.05.15!2.12! LUCY: NEUER PI-PARAMETER  ! KÖPRÜCÜ  *
      *         !19.11.15!2.13! KORREKTUR                 ! STORCH   *
      *         !16.12.15!2.15! LUCY: NEUER PO-PARAMETER  ! KÖPRÜCÜ  *
      * APA02220!07.04.16!2.16! MA-GRD-PR-STZ vergrößert  ! KÖPRÜCÜ  *
      * APA02353!30.03.17!2.17! Teleson                   ! Buch     *
      * APA02504!04.09.17!2.18! ERWEITERUNG PI-BEREICH    ! KÖPRÜCÜ  *
      * DSY04126!21.12.21!    ! PO-DRPA124-PMPR-NEUST-KZ  ! BUCH     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1424                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1424.
          05 PI-DRPA1424-DATEN.
             10 PI-DRPA1424-SUCHFUNKTION        PIC  9(02).
             10 PI-DRPA1424-PGEB-GES            PIC S9(04) COMP.
             10 PI-DRPA1424-PRINTE              PIC S9(15) COMP-3.
             10 PI-DRPA1424-FAELLIG-VON         PIC  X(10).
             10 PI-DRPA1424-PROD-PROD-SCHL      PIC S9(04) COMP.
             10 PI-DRPA1424-PVPT-ID             PIC S9(15) COMP-3.
             10 PI-DRPA1424-PVPT-VERSION        PIC S9(04) COMP.
             10 PI-DRPA1424-PG-GSMT-PROV        PIC S9(09)V9(02) COMP-3.
             10 PI-DRPA1424-GP-ERMITTLUNG       PIC  X(01).
080311       10 PI-DRPA1424-KNR-MANDANT         PIC S9(04) COMP.
240413       10 PI-DRPA1424-UNR                 PIC S9(04) COMP.
240413       10 PI-DRPA1424-SCHICHT-NR          PIC S9(04) COMP.
240413       10 PI-DRPA1424-PGV                 PIC S9(04) COMP.
080311       10 PI-DRPA1424-PGV-KZ              PIC  X(01).
080311          88 PI-DRPA1424-PGV-GUTSCHRIFT   VALUE 'G'.
080311          88 PI-DRPA1424-PGV-BELASTUNG    VALUE 'B'.
050115*      10 PI-DRPA1424-PGV-VORB-DATEN      PIC  X(50).
050115       10 PI-DRPA1424-PGV-VORB-DATEN      PIC  X(200).
300317*      10 PI-DRPA1424-PMBE-PG-RECH-NR     PIC  X(012).
300317       10 PI-DRPA1424-PMBE-PG-RECH-NR     PIC  X(030).
040917       10 PI-DRPA1424-TNR-KZ              PIC  X(01).
040917          88 PI-DRPA1424-TNR-KZ-VORH      VALUE 'T'.
040917          88 PI-DRPA1424-TNR-KZ-VORH-NEIN VALUE ' '.
040917       10 PI-DRPA1424-T-NUMMER            PIC  X(020).
      *
       01 PO-DRPA1424.
          05 PO-DRPA1424-RC                     PIC  X(03).
             88 PO-DRPA1424-RC-OK               VALUE 'OK '.
             88 PO-DRPA1424-RC-NOT-FOUND        VALUE 'NFD'.
240413       88 PO-DRPA1424-RC-UNZULAESSIG      VALUE 'UNZ'.
050115       88 PO-DRPA1424-RC-NSK              VALUE 'NSK'.
190515*      88 PO-DRPA1422-RC-RNR              VALUE 'RNR'.
191115       88 PO-DRPA1424-RC-RNR              VALUE 'RNR'.
          05 PO-DRPA1424-DATEN.
             10 PO-DRPA1424-PEST-ID             PIC S9(15)V COMP-3.
             10 PO-DRPA1424-DGV                 PIC S9(04) COMP.
             10 PO-DRPA1424-PMPT-FAELLIG-VON    PIC  X(10).
             10 PO-DRPA1424-PMPT-GSMT-PROV      PIC S9(09)V99 COMP-3.
             10 PO-DRPA1424-PMPT-GRUND-PROV     PIC S9(09)V99 COMP-3.
             10 PO-DRPA1424-PMPT-EINH           PIC S9(06)V99 COMP-3.
240413*      10 PO-DRPA1424-PMPT-MA-GRD-PR-STZ  PIC S9(03)V999 COMP-3.
070416       10 PO-DRPA1424-PMPT-MA-GRD-PR-STZ  PIC S9(04)V9(04) COMP-3.
240413       10 PO-DRPA1424-PMPR-PROD-SCHL      PIC S9(04) COMP.
240413       10 PO-DRPA1424-PMPR-PGV            PIC S9(04) COMP.
240413       10 PO-DRPA1424-PGV-1900-D-I        PIC  X(01).
161215       10 PO-DRPA1424-PDPT-ID             PIC S9(15)V COMP-3.
211221       10 PO-DRPA1424-PMPR-NEUST-KZ       PIC  X(01).
 
      *
      ******* ENDE COPY-MEMBER **************************************
 
 
 

