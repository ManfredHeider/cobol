      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0470.CPY  $
      *    $Revision:   2.14  $
      *
      *    $Log:   L:/mf/CPY/DRPA0470.CPv  $
      *
      *   Rev 2.14   Jan 09 2020 11:43:54   t13ex44
      *APA02899: Änderung der Abschlussprovision/-einheiten bei Tar
      *ifumstellung im Kfz-Bereich (Teil 2)
      *
      *   Rev 2.13   Mar 24 2017 14:51:40   t13ex24
      *APA02353: Vorbewegungsermittlung TeleSon
      *
      *   Rev 2.12   Dec 22 2015 10:35:00   t13ex44
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System
      *
      *   Rev 2.11   May 26 2015 09:43:10   t13ex24
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System
      *PVW-An
      *passungen für neue PGV's 1052, 4052
      *
      *   Rev 2.10   Jun 13 2012 07:42:38   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.9   Jun 05 2012 08:48:52   t13ae37
      *FPA01271: Ermittlung des PGV-KZ bei Vorbewegung korrigieren
      *
      *   Rev 2.8   May 21 2012 08:48:52   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.7   Mar 30 2012 12:45:36   t13ae37
      *APA01189: vertriebsbezogene Vorbewegungsermittlung bei Belas
      *tungen
      *
      *   Rev 2.6   May 24 2011 14:11:06   t13ae37
      *APA01189: vertriebsbezogene Vorbewegungsermittlung bei Belas
      *tungen
      *
      *   Rev 2.5   16 May 2007 06:43:16   t13ex44
      *APA00604: Anpassungen in Daisy zur Ablösung PEW
      *
      *   Rev 2.4   23 Aug 2001 16:04:56   T13EX25
      *APA00133; PEW-Zustand in PI-Bereich aufnehmen
      *
      *   Rev 2.3   25 May 2000 11:17:06   T13EX21
      *APA00090 - PIN-VNR-AKZ im PI-Bereich
      *eingefügt
      *
      *   Rev 2.2   Mar 10 2000 10:52:22   T13EX21
      *APA00066 - Redesign PVW
      *
      *   Rev 2.1   Mar 12 1999 09:30:08   T13EX22
      *Zähler für AKT-DGV eingefügt
      *
      *   Rev 2.0   Mar 11 1999 11:27:38   T13EX22
      *MFE-UMSTELLUNG
      *
      *   Rev 1.0   23 Jul 1997 12:40:52   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0470  FUNKTION GEVO-FOLGE ( UWPA0028)    *
      *    --------------------------------------------------------  *
      *    ERSTELLT: PARENT                                          *
      *    DATUM:    30.01.1997                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!08.01.98! 01 ! NEUERSTELLUNG             ! PARENT   *
      * APA00012!07.12.98! 02 ! BEARBEITUNGSMODUS ENTFERNT! MUND     *
      * APA00012!11.03.99! 03 ! KENNZEICHEN FÜR DGV NEU   ! MUND     *
      * APA00066!26.11.99! 04 ! PO-DRPA0470-RC-VSTAT-GEF. ! STORCH   *
      *         !        !    ! ENTFERNT                  !          *
      * APA00090!25.05.00! 2.1! PI-DRPA0470-PIN-VNR-AKZ   ! STORCH   *
      *         !        !    ! EINGEFÜGT                 !          *
      * APA00133!23.08.01! 2.2! PI-DRPA0470-PEW-ZUSTAND   ! JÄGER    *
      * APA00604!12.04.07! 2.3! ABSCHALTUNG PEW           ! SCHEIDEL *
      * APA01189!20.05.11! 2.6! VERTRIEBSBEZOGENE VOR-    ! BUCH     *
      *         !        !    ! BEWEGUNGSERMITTLUNG       !          *
      * APA01101!10.04.12! 2.7! VGES DURCH MANDANT ERSETZT! STORCH   *
      *         !        !    ! *-BEREICHE ENTFERNT       !          *
      * FPA01271!04.06.12! 2.9! FÜR KORREKTUR UWPA0028    ! BUCH     *
      *         !        !    ! STAND 2.6 BENÖTIGT        !          *
      * APA01101!13.06.12!2.10! STAND 2.8 WIEDER AKTIVIERT! STORCH   *
      * APA01932!19.05.15!2.11! LUCY: NEUER RETURNCODE RNR! KÖPRÜCÜ  *
      * APA01932!16.12.15!2.12! DURCHREICHEN STEUSTAT     ! SCHEIDEL *
      * APA02899!16.12.19!2.14! KDBETR IN PO-BER. HINZU   ! SCHEIDEL *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0470                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0470.
          05 PI-DRPA0470-DATEN.
             10 PI-DRPA0470-PIN-VNR-AKZ             PIC S9(004) BINARY.
             10 PI-DRPA0470-ANTRAG-ZUSTAND          PIC  9(002).
             10 PI-DRPA0470-KNR-MANDANT             PIC S9(004) BINARY.
      *
       01 PO-DRPA0470.
          05 PO-DRPA0470-RC                         PIC  X(003).
             88 PO-DRPA0470-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0470-RC-RNR                        VALUE 'RNR'.
             88 PO-DRPA0470-RC-MELDUNGSFEHLER             VALUE 'MFE'.
          05 PO-DRPA0470-VNR-ALT                    PIC  X(015).
          05 PO-DRPA0470-DGV-KZ                     PIC  9(001).
             88 PO-DRPA0470-DGV-USED                VALUE 1.
             88 PO-DRPA0470-DGV-NOT-USED            VALUE 2.
          05 PO-DRPA0470-STEUSTAT.
          COPY DRPA1843.
161219    05 PO-DRPA0470-KZ-STRUK-KDBETR            PIC  X(001).
161219    05 PO-DRPA0470-KDBETR                     PIC S9(009) BINARY.
      *
      ******* ENDE COPY-MEMBER **************************************
 
 


