      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0035.CPY  $
      *    $Revision:   2.14  $
      *
      *    $Log:   L:/mf/CPY/DRAT0035.CPv  $
      *
      *   Rev 2.14   Sep 18 2015 11:33:02   t13ae13
      *AVB01363: Neue Ersatzqualifiaktion
      *
      *   Rev 2.13   Aug 14 2014 12:33:20   t13ae13
      *AVB01323: VB22 - Seminar 32 keine Ersatzqualifikation
      *
      *   Rev 2.12   Jun 11 2012 08:43:42   t13ae13
      *AVB01226: Ersatzqualifikation im VB22 hinterlegen
      *
      *   Rev 2.11   May 10 2011 10:45:20   t13ae13
      *AVB01155: Weitere Ersatzqualifikation im Bericht 0098 und 0096
      *
      *   Rev 2.10   Oct 01 2010 12:26:06   t13ae13
      *AVB01104: Weitere Ersatzqualifikation im Bericht 0098 und 0096
      *
      *   Rev 2.9   Jun 07 2010 08:10:12   t13ae13
      *AVB01051: FVD-Prüfung gleichsetzen mit DBBV-VBA/ALP-Prüfung
      *
      *   Rev 2.8   Feb 27 2009 10:21:36   t13ae13
      *AVB00961: FVD Stufe 1 - VB22 Regelbetrieb (swl)
      *
      *   Rev 2.7   Sep 08 2008 10:49:46   t13ae13
      *AVB00992: neue ALP-Prüfung / VB-Dialoge
      *
      *   Rev 2.6   16 May 2007 10:43:14   t13ae13
      *AVB00908: Neuer VB-Vertrag 2007 - VB22
      *
      *   Rev 2.5   16 May 2007 10:06:44   t13ae13
      *AVB00908: Neuer VB-Vertrag 2007 - VB22
      *
      *   Rev 2.4   Mar 20 2006 10:40:02   t13ae13
      *AVB00764: Betreuerwechsel
      *
      *   Rev 2.3   Jul 01 2003 12:28:18   t13ae30
      *AAS00375 - KARRIERE-SEMINAR
      *
      *   Rev 2.2   Jun 20 2002 14:34:28   t13ae13
      *AVB00606: Einführung Seminare für DVAG-Wien
      *
      *   Rev 2.1   Jun 14 1999 10:28:42   T13AE26
      *AVB00446 neue Seminare DBBV-BASIS und
      *                 AUFBAU
      *
      *   Rev 2.0   Feb 27 1999 11:57:08   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   26 Nov 1997 12:17:08   T13AE26
      *AVB00398 - 40Proz.Klausel Teil2
      *
      *   Rev 1.0   Nov 04 1995 08:40:32   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: KNR-KONSTANTEN FUER SEMINARE (DRAT0035)       *
      *    --------------------------------------------------------   *
      *    VERSION   01                                               *
      *    --------------------------------------------------------   *
      *    ERSTELLT: GIESE                                            *
      *    DATUM:    23.04.1993                                       *
      *    STAND:    23.04.1993                                       *
      *****************************************************************
      *   SCHLÜSSELWERTE FÜR ST-TYP  (KTY 37)
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !        !    !                           !          *
      * AFVB0303!02.05.97!    !DAV-Zertifikat und RGS-SEM.!          *
      *         !        !    !eingefügt                  ! T. Moll  *
      * AVB00398!25.11.97! 02 !neue Auftragsnummer        ! T. Moll  *
      * AVB00446!10.06.99!2.1 !Neue Seminare DBBV-Basis u.!          *
      *         !        !    ! -Aufbau                   ! T. Moll  *
      * AVB00606!21.05.02!2.2 !Neue Seminare für DVAG-Wien! S. Walb  *
      * AAS00375!15.01.03!2.3 !NEUES KARRIERE-SEMINAR     ! TH.WILLIG*
      * AVB00734!10.10.05!2.4 !Neue externe Seminare      ! S. Walb  *
      * AVB00908!09.05.07!2.5 !Neue Seminare              ! S. Walb  *
      *         !16.05.07!2.6 !Fehler beim Einchecken     !          *
      * AVB00992!14.08.08!2.7 !Neue Seminare              ! S. Walb  *
      * AVB00987!        !    !Neues Seminar für FVD      !          *
      * AVB00989!        !    !Neues Seminar für Allfinanz!          *
      * AVB00961!05.02.09!2.8 !Neue Seminare für FVD      ! S. Walb  *
      * AVB01051!27.05.10!2.9 !Neues Seminar 56 für FVD   ! S. Walb  *
      * AVB01104!01.10.10!2.10!Neue Seminare              ! S. Walb  *
      * AVB01155!03.05.11!2.11!Neues Seminar 61           ! S. Walb  *
      * AVB01226!06.06.12!2.12!Neues Seminar 27           ! S. Walb  *
      * AVB01323!12.08.14!2.13!Umbenennung Seminar 32;    ! S. Walb  *
      *         !        !    !neue Seminare 62, 63, 68   !          *
      * AVB01363!16.09.15!2.14!Neue Seminare 80 und 88    ! S. Walb  *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *
       01  K-KNR-VB-SEMINAR          PIC  S9(004) COMP VALUE 01.
       01  K-KNR-AL-SEMINAR          PIC  S9(004) COMP VALUE 02.
       01  K-KNR-GABLER-PRUEF        PIC  S9(004) COMP VALUE 03.
       01  K-KNR-BDV-PRUEF           PIC  S9(004) COMP VALUE 04.
       01  K-KNR-RAUT-BERG-1         PIC  S9(004) COMP VALUE 05.
       01  K-KNR-RAUT-BERG-2         PIC  S9(004) COMP VALUE 06.
       01  K-KNR-RAUT-BERG-3         PIC  S9(004) COMP VALUE 07.
       01  K-KNR-RAUT-BERG-4         PIC  S9(004) COMP VALUE 08.
       01  K-KNR-RAUT-BERG-5         PIC  S9(004) COMP VALUE 09.
       01  K-KNR-VB-PRUEF            PIC  S9(004) COMP VALUE 10.
050209*01  K-KNR-CORRELL-1           PIC  S9(004) COMP VALUE 11.
050209 01  K-KNR-DBBV-FVD-ALP-PRUEF  PIC  S9(004) COMP VALUE 11.
       01  K-KNR-CORRELL-2           PIC  S9(004) COMP VALUE 12.
       01  K-KNR-CORRELL-3           PIC  S9(004) COMP VALUE 13.
       01  K-KNR-IHK-PRUEF           PIC  S9(004) COMP VALUE 14.
       01  K-KNR-GABLER-STUD         PIC  S9(004) COMP VALUE 15.
       01  K-KNR-DAV-ZERTIFIKAT      PIC  S9(004) COMP VALUE 16.
       01  K-KNR-RGS-SEMINAR         PIC  S9(004) COMP VALUE 17.
100699 01  K-KNR-DBBV-BASIS          PIC  S9(004) COMP VALUE 18.
100699 01  K-KNR-DBBV-AUFBAU         PIC  S9(004) COMP VALUE 19.
210502 01  K-KNR-AUFBAU-1            PIC  S9(004) COMP VALUE 20.
210502 01  K-KNR-PRUEFUNG-1          PIC  S9(004) COMP VALUE 21.
210502 01  K-KNR-AUFBAU-2            PIC  S9(004) COMP VALUE 22.
210502 01  K-KNR-PRUEFUNG-2          PIC  S9(004) COMP VALUE 23.
150103 01  K-KNR-KARRIERE-SEM        PIC  S9(004) COMP VALUE 24.
150103 01  K-KNR-KARR-SEM-ABSOLV     PIC  S9(004) COMP VALUE 25.
140808 01  K-KNR-AM-SCHAD-REG-VOLLM  PIC  S9(004) COMP VALUE 26.
060612 01  K-KNR-GEPR-KFM-VER-FI-IHK PIC  S9(004) COMP VALUE 27.
140808 01  K-KNR-GEPR-VERSFACHM-IHK  PIC  S9(004) COMP VALUE 28.
140808 01  K-KNR-GEPR-VERSFACH-BWV-A PIC  S9(004) COMP VALUE 29.
101005 01  K-KNR-GEPR-VERSKFM-IHK    PIC  S9(004) COMP VALUE 30.
101005 01  K-KNR-GEPR-BANKKFM-IHK    PIC  S9(004) COMP VALUE 31.
101005*01  K-KNR-GEPR-INVESTKFM-IHK  PIC  S9(004) COMP VALUE 32.
120814 01  K-KNR-GEPR-FINANL-FM-IHK  PIC  S9(004) COMP VALUE 32.
101005 01  K-KNR-GEPR-VERSFACHM-BWV  PIC  S9(004) COMP VALUE 33.
101005 01  K-KNR-GEPR-BSP-FIN-FM-BWB PIC  S9(004) COMP VALUE 34.
101005 01  K-KNR-GEPR-FB-FINANZD-IHK PIC  S9(004) COMP VALUE 35.
101005 01  K-KNR-GEPR-FW-FINANZB-IHK PIC  S9(004) COMP VALUE 36.
101005*01  K-KNR-GEPR-BANKFW-IHK     PIC  S9(004) COMP VALUE 37.
090507 01  K-KNR-SONDERG-DBBV-BASIS  PIC  S9(004) COMP VALUE 37.
090507 01  K-KNR-SONDERG-DBBV-AUFBAU PIC  S9(004) COMP VALUE 38.
090507 01  K-KNR-FUEHRUNG-PARTNER    PIC  S9(004) COMP VALUE 39.
090507 01  K-KNR-FINANZMANAGEMENT    PIC  S9(004) COMP VALUE 40.
090507 01  K-KNR-PERSOENLICHKEIT     PIC  S9(004) COMP VALUE 41.
090507 01  K-KNR-RHETORIK-AUFTRETEN  PIC  S9(004) COMP VALUE 42.
090507 01  K-KNR-ARB-LAUF-QUAL-MANAG PIC  S9(004) COMP VALUE 43.
090507 01  K-KNR-VERANSTALTUNG-MANAG PIC  S9(004) COMP VALUE 44.
090507 01  K-KNR-TRAIN-THE-TRAINER   PIC  S9(004) COMP VALUE 45.
090507 01  K-KNR-RHETOR-AUFTR-PROFIS PIC  S9(004) COMP VALUE 46.
090507 01  K-KNR-ZEITMANAGEMENT      PIC  S9(004) COMP VALUE 47.
090507 01  K-KNR-FUEHRUNG-PARTNER-2  PIC  S9(004) COMP VALUE 48.
090507 01  K-KNR-FUEHRUNG-PARTNER-V  PIC  S9(004) COMP VALUE 49.
090507 01  K-KNR-VBA-PRUEFUNG        PIC  S9(004) COMP VALUE 50.
140808 01  K-KNR-DBBV-VBA-PRUEFUNG   PIC  S9(004) COMP VALUE 51.
140808 01  K-KNR-DBBV-ALP-PRUEFUNG   PIC  S9(004) COMP VALUE 52.
140808 01  K-KNR-EAFP-PRUEFUNG-FVD   PIC  S9(004) COMP VALUE 53.
050209*140808 01  K-KNR-HPTB-VOR-310800-ALL PIC  S9(004) COMP VALUE 54.
050209 01  K-KNR-HPTB-VOR-010800-ALL PIC  S9(004) COMP VALUE 54.
050209 01  K-KNR-HPTB-VOR-010800-FVD PIC  S9(004) COMP VALUE 55.
270510 01  K-KNR-FVD-PRUEFUNG        PIC  S9(004) COMP VALUE 56.
011010 01  K-KNR-GEPR-VERS-FW-IHK    PIC  S9(004) COMP VALUE 57.
011010 01  K-KNR-BERATUNGSCHECK      PIC  S9(004) COMP VALUE 58.
011010 01  K-KNR-BERAT-CHECK-ABGEL   PIC  S9(004) COMP VALUE 59.
011010 01  K-KNR-GEPR-BACHELOR-ARTS  PIC  S9(004) COMP VALUE 60.
030511 01  K-KNR-DIPL-BW-FACHR-VERS  PIC  S9(004) COMP VALUE 61.
120814 01  K-KNR-VB-STARTERSEM-ANSPR PIC  S9(004) COMP VALUE 62.
120814 01  K-KNR-VB-STARTERSEM-ABSOL PIC  S9(004) COMP VALUE 63.
120814 01  K-KNR-GEPR-INVEST-FM-IHK  PIC  S9(004) COMP VALUE 68.
160915 01  K-KNR-RECHNUNGSMANAGM-KMU PIC  S9(004) COMP VALUE 80.
160915 01  K-KNR-KFM-VERS-FIN-VS-IHK PIC  S9(004) COMP VALUE 88.
      *
      ******** ENDE COPY-MEMBER (DRAT0035) ************************

