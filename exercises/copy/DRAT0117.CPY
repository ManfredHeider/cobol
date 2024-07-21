      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *     Datum   :  November 1995
      *     Kopf aus:  N:PROFILEKOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0117.CPY  $
      *    $Revision:   2.9  $
      *
      *    $Log:   L:/mf/CPY/DRAT0117.CPv  $
      *
      *   Rev 2.9   Aug 16 2019 14:04:42   t13ex44
      *APA02814: Familieninterne DL-Nachfolge-Regelung: Übertragung
      * Praxisbestand
      *
      *   Rev 2.8   Mar 12 2019 15:02:34   t13ex44
      *APA02814: Familieninterne DL-Nachfolge-Regelung: Übertragung
      * Praxisbestand
      *
      *   Rev 2.7   Sep 21 2018 12:12:52   t13ex44
      *APA02548: Nachfolgeregelung Direktion Familie
      *
      *   Rev 2.6   Jan 10 2014 11:35:56   t13ex72
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g): GeVo Leben (CBO)
      *
      *   Rev 2.5   Dec 08 2011 10:51:46   t13ex44
      *APA01304: "KÜP auf sich selbst"
      *
      *   Rev 2.4   Sep 16 2010 10:35:28   t13ex44
      *APA01080: Erweiterung AVS-DAISY
      *
      *   Rev 2.3   May 26 2010 07:11:18   t13ex44
      *APA00993: Kundenübernahme-Folgeprovision (KÜF)
      *
      *   Rev 2.2   Aug 25 2008 10:18:30   t13ex44
      *APA00774: Kundenübernahmeprovision
      *
      *   Rev 2.1   Jul 17 2006 12:26:58   t13ex44
      *APA00409: Kundenbetreuungsvereinbarung
      *
      *   Rev 2.0   May 28 1999 18:22:06   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.1   Mar 07 1997 09:18:06   P13PVCS
      *Neuerstellung
      *
      *   Rev 1.0   Feb 19 1997 12:18:44   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0110  KONSTANTEN FÜR EMP-TYP, KTY = 107  *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Prömpeler                                       *
      *    DATUM:    09.09.1996                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DAISY 1 !09.09.96! 01 ! NEUERSTELLUNG             ! Prömpeler*
      * DAISY 1 !21.10.96! 01 ! Ergänzung um Praxisstufen ! Priggen  *
      * DAISY 1 !21.07.97! 01 ! Ergänzung um Averm-alt    ! Jäger    *
      * APA00409!14.03.06!2.1 ! NEU: VB-UE UND VB-UE-ALT  ! SCHEIDEL *
      *         !26.06.08!2.2 ! NEU: KDUE                 ! SCHEIDEL *
      * APA00993!29.04.10!2.3 ! NEU: KUEF                 ! SCHEIDEL *
      * APA01304!17.11.11!2.5 ! NEU: KUEP-QUEREINSTEIGER  ! SCHEIDEL *
      * APA01526!25.03.13!2.6 ! NEU: OHNE-EINHEITEN       ! BÖTTCHER *
      * APA02548!26.07.18!2.7 ! NEU: NFOE                 ! SCHEIDEL *
      * APA02814!18.02.19!2.8 ! NEU: VBUM, VBUM-ALT, NFEG ! SCHEIDEL *
      *         !        !    !      NFGR                 !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  FILLER.
           05 K-KNR-EMP-TYP-UNBEKANNT PIC S9(004) BINARY SYNC VALUE  0.
           05 K-KNR-EMP-TYP-VTF       PIC S9(004) BINARY SYNC VALUE  1.
           05 K-KNR-EMP-TYP-VWF       PIC S9(004) BINARY SYNC VALUE  2.
           05 K-KNR-EMP-TYP-VM        PIC S9(004) BINARY SYNC VALUE  3.
           05 K-KNR-EMP-TYP-KD-BETR   PIC S9(004) BINARY SYNC VALUE  4.
           05 K-KNR-EMP-TYP-BETREUER  PIC S9(004) BINARY SYNC VALUE  5.
           05 K-KNR-EMP-TYP-NG-STRUK  PIC S9(004) BINARY SYNC VALUE  6.
           05 K-KNR-EMP-TYP-AVERM     PIC S9(004) BINARY SYNC VALUE  7.
           05 K-KNR-EMP-TYP-AVERM-O-P PIC S9(004) BINARY SYNC VALUE  8.
           05 K-KNR-EMP-TYP-AVERM-P2  PIC S9(004) BINARY SYNC VALUE  9.
           05 K-KNR-EMP-TYP-AVERM-P3  PIC S9(004) BINARY SYNC VALUE  10.
           05 K-KNR-EMP-TYP-AVERM-P4  PIC S9(004) BINARY SYNC VALUE  11.
           05 K-KNR-EMP-TYP-AVERM-P5  PIC S9(004) BINARY SYNC VALUE  12.
           05 K-KNR-EMP-TYP-AVERM-P6  PIC S9(004) BINARY SYNC VALUE  13.
           05 K-KNR-EMP-TYP-AVERM-P7  PIC S9(004) BINARY SYNC VALUE  14.
           05 K-KNR-EMP-TYP-AVERM-ALT PIC S9(004) BINARY SYNC VALUE  15.
           05 K-KNR-EMP-TYP-VBUE      PIC S9(004) BINARY SYNC VALUE  16.
           05 K-KNR-EMP-TYP-VBUE-ALT  PIC S9(004) BINARY SYNC VALUE  17.
           05 K-KNR-EMP-TYP-KDUE      PIC S9(004) BINARY SYNC VALUE  18.
           05 K-KNR-EMP-TYP-KUEF      PIC S9(004) BINARY SYNC VALUE  19.
           05 K-KNR-EMP-TYP-QUER      PIC S9(004) BINARY SYNC VALUE  20.
           05 K-KNR-EMP-TYP-OAE       PIC S9(004) BINARY SYNC VALUE  21.
           05 K-KNR-EMP-TYP-N1EM      PIC S9(004) BINARY SYNC VALUE  22.
           05 K-KNR-EMP-TYP-N1EM-ALT  PIC S9(004) BINARY SYNC VALUE  23.
           05 K-KNR-EMP-TYP-N1EO      PIC S9(004) BINARY SYNC VALUE  24.
           05 K-KNR-EMP-TYP-N1EO-ALT  PIC S9(004) BINARY SYNC VALUE  25.
           05 K-KNR-EMP-TYP-N2EO      PIC S9(004) BINARY SYNC VALUE  26.
           05 K-KNR-EMP-TYP-N2EO-ALT  PIC S9(004) BINARY SYNC VALUE  27.
           05 K-KNR-EMP-TYP-N1GM      PIC S9(004) BINARY SYNC VALUE  28.
           05 K-KNR-EMP-TYP-N1GO      PIC S9(004) BINARY SYNC VALUE  29.
           05 K-KNR-EMP-TYP-N2GM      PIC S9(004) BINARY SYNC VALUE  30.
           05 K-KNR-EMP-TYP-N2GO      PIC S9(004) BINARY SYNC VALUE  31.
      *    Nummer des grössten oben aufgeführten Wertes
           05 K-KNR-EMP-TYP-MAX       PIC S9(004) BINARY SYNC VALUE  31.
 


