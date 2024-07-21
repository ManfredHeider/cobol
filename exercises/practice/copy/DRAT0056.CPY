      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0056.CPY  $
      *    $Revision:   2.23  $
      *
      *    $Log:   L:/mf/CPY/DRAT0056.CPv  $
      *
      *   Rev 2.23   Mar 24 2020 15:25:14   t13ex18
      *AVB01615: IT-Projekt EasyVB Stufe 4 - Stammdatenaktualisieru
      *ng - Erweiterung SST für extZOB (VB-System)
      *
      *   Rev 2.22   Mar 23 2020 16:41:40   t13ex18
      *AVB01615: IT-Projekt EasyVB Stufe 4 - Stammdatenaktualisieru
      *ng - Erweiterung SST für extZOB (VB-System)
      *
      *   Rev 2.21   Mar 20 2020 17:35:00   t13ex18
      *AVB01615: IT-Projekt EasyVB Stufe 4 - Stammdatenaktualisieru
      *ng - Erweiterung SST für extZOB (VB-System)
      *
      *   Rev 2.20   Jan 13 2020 16:39:40   t13ae30
      *AAS01307: 1. Erweiterung Cursor OZPs ZJVB0013 und ZJVB0014
      *
      *   Rev 2.19   Oct 30 2019 12:38:58   t13ae30
      *AAS01307: Anpassung Bedingungen AS-Club mit besonderen Leist
      *ungen und AS-Club (VB-System)
      *
      *   Rev 2.18   Feb 10 2017 15:01:16   T13AE30
      *AAS01186: DVAG-Wien: Admirals-Club
      *
      *   Rev 2.17   Jan 16 2017 11:02:16   t13ae30
      *AAS01186: DVAG-Wien: Admirals-Club
      *
      *   Rev 2.16   Dec 18 2014 11:23:02   t13ae13
      *AVB01321: Integration FVD - Dialoge (SWL)
      *
      *   Rev 2.15   May 06 2014 11:28:38   t13ex13
      *AVB01309: RGT: VB45/VB46
      *
      *   Rev 2.14   Nov 19 2009 10:34:08   t13ae27
      *AAS00847: Clubmitgliedschaften DVAG-Wien: Elite- und Kapitän
      *sclub in Ö
      *
      *   Rev 2.13   Oct 05 2009 16:18:12   t13ae30
      *AAS00847: Clubmitgliedschaften DVAG-Wien: Elite- und Kapitän
      *sclub in Ö
      *
      *   Rev 2.12   Oct 05 2009 16:10:28   t13ae30
      *AAS00847: Clubmitgliedschaften DVAG-Wien: Elite- und Kapitän
      *sclub in Ö
      *
      *   Rev 2.11   Jun 22 2009 14:46:20   t13ex13
      *AVB01041: PGM - VB46 Pflege DG Allfinanz
      *
      *   Rev 2.10   Jan 09 2009 17:03:40   t13ae30
      *AAS00779: Allfinanz "Champions-Club"
      *
      *   Rev 2.9   Mar 13 2008 10:50:40   t13ae30
      *AAS00746: Clubprüfung für ernannte Mitglieder
      *
      *   Rev 2.8   04 Mar 2008 14:41:34   t13ae27
      *AAS00733: EliteClub Unternehmensaufbau
      *
      *   Rev 2.7   15 Jan 2007 15:01:22   t13ex57
      *AVB00899: Änderung Erfolgsprovision der DVAG-Wien ab 01.01.2
      *007
      *
      *   Rev 2.6   22 Jun 2006 13:19:38   T13EX57
      *AVB00852: Anpassung VB-System an "Neue Komm.Strategie" : UBO
      *
      *   Rev 2.5   Sep 04 2003 13:04:28   t13ex22
      *AVB00701:Dialog VB44 - Pflege Insolvenzkonten;
      *Anzeige BV-Schutz in UU11 und VB21
      *
      *   Rev 2.4   Aug 14 2003 14:08:24   t13ae30
      *AVB00681 - Club "Bes.Leist. im AS-Club"
      *
      *   Rev 2.3   Jun 23 2003 11:50:42   t13ae13
      *FVB00526: Fehlerbehebung aus Dim 7ukunft
      *
      *   Rev 2.2   Jan 14 2003 09:20:42   t13ae13
      *AVB00649: Neues Aufstiegssystem für RGS - D
      *
      *   Rev 2.1   Nov 15 2002 16:40:16   t13ae30
      *AVB00643 - Kapitäns-Club
      *
      *   Rev 2.0   Feb 27 1999 11:57:12   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.2   Apr 15 1997 14:37:54   T13EX11
      *Neue KNR=14: AS-CLUB-BDG-1997
      *
      *   Rev 1.1   Dec 29 1995 15:41:02   T13AE26
      *AFVB0251: AL-Probe
      *
      *   Rev 1.0   Nov 04 1995 08:45:04   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: KNR-KONSTANTEN FUER MAST-TYP                  *
      *                 KTY-NR = 42                                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: HECKER                                           *
      *    DATUM:    18.11.1993                                       *
      *****************************************************************
      *    VERSION   4    20.01.95 08:56:40
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFVB0037!25.11.93! 01 ! NEU                       ! HECKER   *
      * AFVB0061!10.01.94!    ! AUN-KONSTANTEN UMBENANNT  ! SCHÖLLH. *
      * AFVB0050!11.01.94!    ! NEU: ADMIRAL-CLUB,AS-CLUB ! SCHÖLLH. *
      *         !20.01.94! 02 ! PROD.UEBERGABE            ! HECKER   *
      * FEVB0135!11.08.94!    ! NEU: VILLA-FLORES-LV/-UV  ! SCHÖLLH. *
      *         !11.08.94! 03 ! PROD.UEBERGABE            ! SCHÖLLH. *
      * AFVB0151!12.09.94!    ! UMBENANNT: ADMIRAL-CLUB,  ! SCHÖLLH. *
      *         !        !    ! NEU: ADM-CLUB-ERFUELLT,   !          *
      *         !        !    !      ADM-CLUB-PREIS-1     !          *
      *         !19.09.94! 04 ! PROD.UEBERGABE            ! SCHÖLLH. *
      * AFVB0160!20.12.94!    ! NEU: UMSATZAKTIV-STATUS-A ! SCHÖLLH. *
      *         !        !    ! NEU: UMSATZAKTIV-LB       !          *
      *         !        !    ! 05-ER STUFEN              !          *
      *         !13.01.95! 05 ! PROD.UEBERGABE            ! SCHÖLLH. *
      * AFVB0177!18.01.95!    ! FÜR WERTE '1' UND '2' NEUE! SCHÖLLH. *
      *         !        !    !  BEZEICHNUNGEN EINGEFÜHRT;!          *
      *         !        !    !  DIE ALTEN SOLLEN NICHT   !          *
      *         !        !    !  MEHR VERWENDET WERDEN    !          *
      *         !19.01.95! 06 ! PROD.UEBERGABE            ! SCHÖLLH. *
      * AFVB0251!10.12.95! 07 ! AL-(Probe/Knade/Dauer)    ! Moll     *
      * AFVB0347!03.04.97! 08 ! NEU: AS-CLUB-BDG-1997     ! SCHÖLLH. *
      *         !        !    !  (KNR=14);                !          *
      *         !        !    ! KNR=4 UMBENANNT;          !          *
      *         !        !    ! KNR-AL-DAUER: Wert korri- !          *
      *         !        !    !  giert (12->13) (falscher !          *
      *         !        !    !  wert hat zu keinen pgm-  !          *
      *         !        !    !  fehlern geführt)         !          *
      * AVB00643!06.11.02! 09 ! NEU: KAPITÄNS-CLUB        ! WILLIG   *
      * AVB00649!25.11.02!2.2 ! NEU: Anlage B /2003       ! Walb     *
      * FVB00526!10.04.03!2.3 ! NEU: Kündigungsbestätigung! Walb     *
      * AVB00681!17.07.03!2.4 ! NEU: BESONDERE LEISTUNGEN ! WILLIG   *
      *         !        !    !      IM AS-CLUB           !          *
      *         !        !    ! NEU: REGIONALBEIRAT       !          *
      *         !        !    !      REGIONALBEIRATSVORS. !          *
      * AVB00701!21.08.03!2.5 ! KNR 25 BV GEGEN BEARB.    ! MUND     *
      *         !        !    ! SCHÜTZEN                  !          *
      * AVB00852!14.06.06!2.6 ! Anpassung VB-System an    ! U. Boll  *
      *         !        !    ! Neue Kommunik. Strategie  !          *
020107* AVB00899!02.01.07!2.7 ! Änderung Erfolgsprovision ! U. Boll  *
      *         !        !    ! für DVAG WIEN             !          *
      * AAS00733!17.01.08!2.8 ! NEU: ELITE-CLUB           ! P. Dürr  *
      * AAS00779!01.09.08!2.9 ! NEU: ALLFI. CHAMPIONS-CLUB! WILLIG   *
      * AVB01041!11.05.09!2.11! NEU: 2. DG - SPRECHER     !GUTFLEISCH*
      * AAS00852!14.08.09!2.12! NEU:                      ! WILLIG   *
      *         !        !    ! KAPITÄNS-CLUB DVAG, WIEN  !          *
      * AAS00847!06.10.09!2.13! NEU: ELITE-CLUB Öst.      ! P. Dürr  *
      * AVB01309!15.04.14!2.14! NEUE NKV                  !GUTFLEISCH*
      * AVB01321!24.09.14!2.16! NEU: FVD2DVAG (KNR 45)    ! S. Walb  *
      *         !        !    ! und KNR 38 - 42           !          *
      * AVB01150!15.09.16!2.17! NEU: ADM.-CLUB, WIEN      ! WILLIG   *
      * VBL-964 !11.09.19!2.18! NEUE CLUBS AB 012020      ! WILLIG   *
110320* AVB01615!11.03.20!2.17! NEU: 0068-0075 aufgenommen! G.Bruder *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *   ERLÄUTERUNGEN:
      *     - ADM-CLUB-PREIS-1 = ANRECHT AUF AUFENTHALT IN VILLA SONOMA
      *                          (STAND 12.09.1994)
      *


       01  FILLER.
      *     * DIE 'AUN'-BEZEICHNUNGEN NICHT MEHR BENUTZEN !!
           05 K-KNR-AUN-MASTF                PIC S9(04) COMP VALUE  1.
           05 K-KNR-AUN-REPR                 PIC S9(04) COMP VALUE  2.
           05 K-KNR-STATUS-A-VB              PIC S9(04) COMP VALUE  1.
           05 K-KNR-STATUS-A-RP              PIC S9(04) COMP VALUE  2.
           05 K-KNR-ADM-CLUB-MITGLIED        PIC S9(04) COMP VALUE  3.
030497*    * AS-CLUB-MITGLIED: NACH BEDINGUNGEN 1994 ODER 1997
030497*    *                   (BEDINGUNGEN 1997: SIEHE KNR=14)
030497     05 K-KNR-AS-CLUB-MITGLIED         PIC S9(04) COMP VALUE  4.
           05 K-KNR-VILLA-FLORES-LV          PIC S9(04) COMP VALUE  5.
           05 K-KNR-VILLA-FLORES-UV          PIC S9(04) COMP VALUE  6.
           05 K-KNR-ADM-CLUB-ERFUELLT        PIC S9(04) COMP VALUE  7.
           05 K-KNR-ADM-CLUB-PREIS-1         PIC S9(04) COMP VALUE  8.
           05 K-KNR-UMSATZAKTIV-STATUS-A     PIC S9(04) COMP VALUE  9.
           05 K-KNR-UMSATZAKTIV-LB           PIC S9(04) COMP VALUE 10.
101295     05 K-KNR-AL-PROBE                 PIC S9(04) COMP VALUE 11.
101295     05 K-KNR-AL-GNADE                 PIC S9(04) COMP VALUE 12.
030497     05 K-KNR-AL-DAUER                 PIC S9(04) COMP VALUE 13.
030497*    * AS-CLUB-BDG-1997: VB HAT DIE AB 1997 GELTENDEN BEDINGUNGEN
030497*    *                   ERFÜLLT (MITGLIEDSCHAFT: SIEHE KNR=4)
030497     05 K-KNR-AS-CLUB-BDG-1997         PIC S9(04) COMP VALUE 14.
061102     05 K-KNR-VB-SCHWEIZ               PIC S9(04) COMP VALUE 15.
061102     05 K-KNR-KAPITAENS-CLUB-MITGLIED  PIC S9(04) COMP VALUE 16.
061102     05 K-KNR-KAPITAENS-CLUB-ERFUELLT  PIC S9(04) COMP VALUE 17.
251102     05 K-KNR-ANLAGE-B-2003            PIC S9(04) COMP VALUE 18.
100403     05 K-KNR-KUENDBESTAETIGUNG        PIC S9(04) COMP VALUE 19.
170703     05 K-KNR-REGIONALBEIRAT           PIC S9(04) COMP VALUE 20.
170703     05 K-KNR-REGBEIRATVORSITZENDER    PIC S9(04) COMP VALUE 21.
170703     05 K-KNR-BL-ASCLUB-MITGLIED       PIC S9(04) COMP VALUE 22.
170703     05 K-KNR-BL-ASCLUB-ERFUELLT       PIC S9(04) COMP VALUE 23.
170703     05 K-KNR-ASCLUB-ERFUELLT          PIC S9(04) COMP VALUE 24.
210803     05 K-KNR-BV-GESCHUETZT            PIC S9(04) COMP VALUE 25.
160606     05 K-KNR-SONDERVEREINBARUNG       PIC S9(04) COMP VALUE 26.
160606     05 K-KNR-SPRECHER-DG              PIC S9(04) COMP VALUE 27.
160606     05 K-KNR-MITGL-VERTR-BER-AUS      PIC S9(04) COMP VALUE 28.
020107     05 K-KNR-NEUE-BEFBED-WIEN         PIC S9(04) COMP VALUE 29.
170108     05 K-KNR-ELITECLUB-MITGLIED       PIC S9(04) COMP VALUE 30.
170108     05 K-KNR-ELITECLUB-ERFUELLT       PIC S9(04) COMP VALUE 31.
010908     05 K-KNR-CHAMPCLUB-MITGLIED       PIC S9(04) COMP VALUE 32.
010908     05 K-KNR-CHAMPCLUB-ERFUELLT       PIC S9(04) COMP VALUE 33.
110509     05 K-KNR-ZWEITER-SPRECHER-DG      PIC S9(04) COMP VALUE 34.
140809     05 K-KNR-KAP-CLUB-MITGL-WIEN      PIC S9(04) COMP VALUE 35.
140809     05 K-KNR-KAP-CLUB-WIEN-ERUELLT    PIC S9(04) COMP VALUE 36.
061009     05 K-KNR-ELITECLUB-MITGLIED-WIEN  PIC S9(04) COMP VALUE 37.
240914     05 K-KNR-CHAMPCLUB-NEUE-PARTNER   PIC S9(04) COMP VALUE 38.
240914     05 K-KNR-EXIST-GRUEND-PHASE-2011  PIC S9(04) COMP VALUE 39.
240914     05 K-KNR-EXIST-GRUEND-24MON-2011  PIC S9(04) COMP VALUE 40.
240914     05 K-KNR-EXIST-GRUEND-12MON-2011  PIC S9(04) COMP VALUE 41.
240914     05 K-KNR-ELITECLUB-WIEN-ERFUELLT  PIC S9(04) COMP VALUE 42.
240914     05 K-KNR-BERUFSSTARTER            PIC S9(04) COMP VALUE 43.
150414     05 K-KNR-BEREICHS-AUSSCHUSS       PIC S9(04) COMP VALUE 44.
240914     05 K-KNR-FVD-2-DVAG               PIC S9(04) COMP VALUE 45.
150916     05 K-KNR-EXISTENGRD-BONUS         PIC S9(04) COMP VALUE 46.
150916     05 K-KNR-ADM-CL-MITGL-WIEN        PIC S9(04) COMP VALUE 47.
150916     05 K-KNR-ADM-CL-WIEN-ERFUELLT     PIC S9(04) COMP VALUE 48.
110919     05 K-KNR-KV-FOERDERBONUS          PIC S9(04) COMP VALUE 49.
110919     05 K-KNR-PD-NACHFOLGEREGELUNG     PIC S9(04) COMP VALUE 50.
110919     05 K-KNR-SD-NACHFOLGEREGELUNG     PIC S9(04) COMP VALUE 51.
110919     05 K-KNR-AS-CL-MITGL-WIEN         PIC S9(04) COMP VALUE 52.
110919     05 K-KNR-AS-CL-WIEN-ERFUELLT      PIC S9(04) COMP VALUE 53.
110919     05 K-KNR-AS-CL-MITGL-ALLF1        PIC S9(04) COMP VALUE 54.
110919     05 K-KNR-AS-CL-ALLF1-ERFUELLT     PIC S9(04) COMP VALUE 55.
110919     05 K-KNR-AS-CL-MITGL-ALLF2        PIC S9(04) COMP VALUE 56.
110919     05 K-KNR-AS-CL-ALLF2-ERFUELLT     PIC S9(04) COMP VALUE 57.
110919     05 K-KNR-BL-ASCL-MITGL-WIEN       PIC S9(04) COMP VALUE 58.
110919     05 K-KNR-BL-ASCL-WIEN-ERFUELLT    PIC S9(04) COMP VALUE 59.
110919     05 K-KNR-BL-ASCL-MITGL-ALLF1      PIC S9(04) COMP VALUE 60.
110919     05 K-KNR-BL-ASCL-ALLF1-ERFUELLT   PIC S9(04) COMP VALUE 61.
110919     05 K-KNR-BL-ASCL-MITGL-ALLF2      PIC S9(04) COMP VALUE 62.
110919     05 K-KNR-BL-ASCL-ALLF2-ERFUELLT   PIC S9(04) COMP VALUE 63.
110919     05 K-KNR-KAP-CLUB-MITGL-ALLF1     PIC S9(04) COMP VALUE 64.
110919     05 K-KNR-KAP-CLUB-ALLF1-ERUELLT   PIC S9(04) COMP VALUE 65.
110919     05 K-KNR-KAP-CLUB-MITGL-ALLF2     PIC S9(04) COMP VALUE 66.
110919     05 K-KNR-KAP-CLUB-ALLF2-ERUELLT   PIC S9(04) COMP VALUE 67.
110320     05 K-KNR-ELITECLUB-MITGL-ALLF1    PIC S9(04) COMP VALUE 68.
110320     05 K-KNR-ELITECLUB-ALLF1-ERUELLT  PIC S9(04) COMP VALUE 69.
110320     05 K-KNR-ELITECLUB-MITGL-ALLF2    PIC S9(04) COMP VALUE 70.
110320     05 K-KNR-ELITECLUB-ALLF2-ERUELLT  PIC S9(04) COMP VALUE 71.
110320     05 K-KNR-Anlage-B2020-akzeptiert  PIC S9(04) COMP VALUE 72.
110320     05 K-KNR-Anlage-B2020-abgelehnt   PIC S9(04) COMP VALUE 73.
110320     05 K-KNR-D-Chancen-Profil-Umsatz  PIC S9(04) COMP VALUE 74.
110320     05 K-KNR-UnsBest-DL-Net-Neu-Ges   PIC S9(04) COMP VALUE 75.
 
      *
      ******** ENDE COPY-MEMBER ***************************************








