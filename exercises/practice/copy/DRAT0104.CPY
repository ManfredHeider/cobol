      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0104.CPY  $
      *    $Revision:   2.9  $
      *
      *    $Log:   L:/mf/CPY/DRAT0104.CPv  $
      *
      *   Rev 2.9   Jun 30 2016 11:51:40   t13ex71
      *AVB01400: Projekt Lucy2 - Neue Funktion in UPVB0189 aufnehme
      *n (BPR)
      *
      *   Rev 2.8   13 Apr 2007 17:20:44   t13ae13
      *AVB00907: Beförderungsbedingungen Österreich 2007
      *
      *   Rev 2.7   Feb 17 2005 18:29:04   t13ae30
      *AAS00513: Jubiläums-Wettbewerbs/Mittelmeer-Schiffsreise 2005.
      *
      *   Rev 2.6   Feb 17 2005 18:19:42   t13ae30
      *AAS00513: Jubiläums-Wettbewerbs/Mittelmeer-Schiffsreise 2005.
      *
      *   Rev 2.5   Dec 18 2003 12:00:26   t13ex22
      *AAS00443: Erweiterung für Wettbewerb 2004
      *
      *   Rev 2.4   20 Mar 2001 10:11:06   T13AE13
      *AAS00264 - Wettbewerb und Leistungsplan 2001
      *
      *   Rev 2.3   06 Oct 2000 10:38:40   T13AE30
      *ASL00067 - FÖBO2
      *
      *   Rev 2.2   26 Apr 2000 12:15:44   T01JMBQ
      *APA00084 - Redesign SST-MA
      *
      *   Rev 2.1   Jun 10 1999 11:17:26   T13AE34
      *APA00012-SST Daisy
      *
      *   Rev 2.0   Feb 27 1999 11:57:20   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   13 Nov 1998 09:07:00   T13EX26
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   Apr 30 1996 17:16:24   T13EX16
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAT0104   KNR-KONST. KTY-60 STEUER-INFO      *
      *    --------------------------------------------------------   *
      *    ERSTELLT: AUERBACH                                         *
      *    DATUM:    09.04.1996                                       *
      *****************************************************************
      *    VERSION   1    09.04.96 16:59:59
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFVB0244!09.04.96! 01 ! NEU                       ! AUERBACH *
      * APA00012!26.05.98! 02 ! Neue Ausprägung für das   ! Jäger    *
      *         !        !    ! Mitarbeiter-Abrechnungsdat!          *
      * ASL00025!30.10.98! 03 ! NEUE AUSPRäGUNG FüR DAS   ! WALB     *
      *         !        !    ! EINSPIELDATUM D. STATISTIK!          *
      *         !        !    ! IM LETZTEN MONAT IM QUART.!          *
      * APA00012!28.05.99!2.1 ! NEU: ZEITPUNKT LETZTE     ! HEIDER   *
      *         !        !    !      ERST. FEHLERDATEI    !          *
      * APA00012!26.04.00!2.2 ! NEU: ZEITPUNKT SSTMA      ! KRUFF    *
      * ASL00067!20.09.00!2.3 ! NEU: ZEITPUNKT FÖBO2      ! TH.WILLIG*
      *         !        !    !      SST ---> AACHEN      !          *
      * AAS00264!09.03.01!2.4 ! NEU: ZEITPUNKT Wettbewerb ! S. Walb  *
      *         !        !    ! u. Leistungsplan 2001     !          *
      * AAS00513!28.12.04!2.5 ! NEU: ZEITPUNKT WBW2005    ! TH.WILLIG*
      *         !        !    ! NEU: ZEITPUNKT WBW2005-OES!          *
      * AVB00907!08.03.07!2.6 ! KNR = 18 nachgepflegt     ! S. Walb  *
      * AVB01400!01.06.16!2.7 ! KNR = 93 nachgepflegt     ! B.Pretsch*
      *         !        !    !                           !          *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *   SCHLÜSSELWERTE KTY-60   STEUER-INFO
      *
       01  FILLER.
           05  K-KNR-FIBU-DAT-TRANS         PIC S9(004) COMP VALUE 001.
           05  K-KNR-MA-DAT                 PIC S9(004) COMP VALUE 002.
301098     05  K-KNR-STAT-DAT               PIC S9(004) COMP VALUE 003.
280599     05  K-KNR-FEHLER-DAT             PIC S9(004) COMP VALUE 004.
260400     05  K-KNR-SSTMA-DAT              PIC S9(004) COMP VALUE 005.
200900     05  K-KNR-SST-FB2-AC             PIC S9(004) COMP VALUE 006.
090301     05  K-KNR-ZPKT-LPLAN-2001        PIC S9(004) COMP VALUE 007.
090301     05  K-KNR-ZPKT-WETTB-2001        PIC S9(004) COMP VALUE 008.
281204***KONSTANTEN MIT KNR=9 bis KNR=20 MÜSSEN NACHGEPFLEGT WERDEN!!!
080307     05  K-KNR-ZPKT-EQF-LISTE         PIC S9(004) COMP VALUE 018.
281204     05  K-KNR-ZPKT-WETTB-2005        PIC S9(004) COMP VALUE 021.
281204     05  K-KNR-ZPKT-WBW2005-OES       PIC S9(004) COMP VALUE 022.
010616     05  K-KNR-FIBU-SST-DAT           PIC S9(004) COMP VALUE 093.
      *
      ***** ENDE COPY-MEMBER DRAT0104  KNR-KONST. STEUER-INFO     .*****

