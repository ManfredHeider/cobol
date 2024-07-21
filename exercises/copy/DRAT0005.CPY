      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0005.CPY  $
      *    $Revision:   2.12  $
      *
      *    $Log:   L:/mf/CPY/DRAT0005.CPv  $
      *
      *   Rev 2.12   Jul 12 2019 09:59:36   t13ae56
      *AVB01570: PGM - Gutscheincode als Hinweis ausgeben
      *
      *   Rev 2.11   Sep 30 2011 09:02:08   t13ae13
      *AVB01185: Maßnahmenpaket - Teil 1
      *
      *   Rev 2.10   Sep 29 2010 11:10:58   t13ae13
      *AVB01031: ED- flexible Provisionszuordnung - VB29, XA20
      *
      *   Rev 2.9   Oct 29 2009 13:18:36   t13ae13
      *AVB01014: Änderung ED-Regelung ab Alter 60
      *
      *   Rev 2.8   Feb 21 2007 12:30:32   t13ae34
      *ACV00054: Change Request CTV-Service-Funktion
      *
      *   Rev 2.7   Feb 15 2007 10:20:06   t13ae34
      *ACV00054: Anpassung Driver-Rahmen -> über AAN00655
      *
      *   Rev 2.6   Jan 27 2006 11:37:46   t13ae34
      *ACV00054: Anpassung Driver-Rahmen
      *
      *   Rev 2.5   Nov 03 2004 11:52:40   t13ex25
      *APA00295: Daisy-KÜ
      *
      *   Rev 2.4   10 May 2001 13:08:56   T13AE34
      *ACV00015-CTV-Briefe aus Neudialogen
      *
      *   Rev 2.3   07 Dec 2000 17:40:18   T13AE28
      *AVB00508: Ber.Inv. E Stufe 2 im VB22
      *
      *   Rev 2.2   Jun 30 1999 15:20:28   T13EX21
      *APA00012 - K-OP-FEK geändert
      *
      *   Rev 2.1   Mar 09 1999 11:36:36   T13AE41
      *Daten recovert
      *
      *   Rev 2.0   Feb 27 1999 11:57:02   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.6   13 Nov 1998 16:53:20   T13EX26
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.5   26 Nov 1997 12:16:38   T13AE26
      *AVB00398 - 40Proz.Klausel Teil2
      *
      *   Rev 1.4   25 Jun 1997 08:54:20   T13EX16
      *AFPA0010 - neue Operationen eingefuegt
      *
      *   Rev 1.3   May 13 1997 15:43:28   T13EX11
      *FEVB0347: neue OP DPO
      *
      *   Rev 1.2   Mar 20 1997 16:56:44   T13EX16
      *DRIVER-ANPASSUNG DAISY
      *
      *   Rev 1.1   Mar 26 1996 16:56:06   T13EX11
      *AFVB0226  neue OP's: TBA, TBE, DWL
      *
      *   Rev 1.0   Nov 04 1995 08:34:18   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: OPERATIONSCODES (DRAT0005)                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: W. SCHOELLHAMMER (INTEGRATA AG)                  *
      *    DATUM:    23.11.1992                                       *
      ****************************************************************
      *    VERSION   2    09.02.95 16:40:38
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !23.04.93! 01 ! NEU                       ! SCHOELLH.*
      *         !03.05.93! 02 ! NEUE OP'S                 ! SCHOELLH.*
      *         !23.07.93! 03 ! NEU: OP 'FU3'             ! SCHOELLH.*
      *         !        !    ! NEU: OP 'BS3'             ! HECKER   *
      * FEVB0181!24.11.94!    ! NEU: OP'S ASR, ZI1, DWS   ! SCHÖLLH. *
      *         !25.11.94! 04 ! PROD.ÜBERGABE             ! SCHÖLLH. *
      * AFVB0142!14.12.94!    ! NEU: OP 'AHI'             ! GIESE    *
      *         !        ! 05 ! PROD.ÜBERGABE             !          *
      * AFVB0163!16.01.95!    ! NEU: 'HSB' ENTFÄLLT: 'HFT'! SCHÖLLH. *
      *         !08.02.95! 06 ! PROD.ÜBERGABE             ! GIESE    *
      * AFVB0174!06.03.95!    ! NEU: OP 'AHM', 'FI3'      ! GOECKE   *
      * (DAISY) !08.01.96!    ! NEU: OP 'KPT'             ! GIESE    *
      * AFVB0226!02.02.96!    ! NEU: OP 'TBA','TBE','DWL' ! SCHÖLLH. *
      * AFPA0002!24.09.96!    ! NEU: OP 'DRU','ESE','DUD' ! AUERBACH *
      *         !        !    !         'ZUK'             !          *
      *         !20.01.97!    ! NEU: OP 'DPN','DPP'       ! SCHÖLLH. *
      *         !25.01.97!    ! NEU: OP 'BPU','ERD','FUA' ! AUERBACH *
      *         !        !    !         'FUE','AWA'       !          *
      *         !03.02.97!    ! NEU: OP 'RRD'             ! SCHÄFER  *
      * FEVB0374!30.04.97!    ! NEU: OP 'DPO'             ! SCHÖLLH. *
      * AFPA0010!06.06.97!    ! NEU: OP 'FSA', 'FSR'      ! AUERBACH *
      * AFVB0303!19.06.97!    ! NEU: OP 'U2R',            ! T. Moll  *
      * AFVB0303!22.07.97!    ! NEU: OP 'ST1', 'FD3'      ! SCHÖLLH. *
      *         !26.08.97!    ! NEU: OP 'ZI2'             ! D.WILHELM*
      * AVB00398!25.11.97! 07 ! neue Auftragsnummer       ! T. Moll  *
      * APA00020!21.02.98!    ! NEU: OP 'TSE'             ! BOHLINGER*
      * APA00012!21.04.98!    ! NEU: OP 'BPA'             ! STORCH   *
      * APA00012!11.05.98!    ! NEU: OP 'DMI'             ! FOLTZ    *
      * APA00012!11.05.98!    ! NEU: OP 'AND'             ! FOLTZ    *
      * APA00012!11.05.98!    ! NEU: OP 'ANV'             ! FOLTZ    *
      * APA00012!11.05.98!    ! NEU: OP 'AUS'             ! FOLTZ    *
      * APA00012!19.05.98!    ! NEU: OP 'BER'             ! FOLTZ    *
      * APA00012!18.08.98!    ! NEU: OP 'TBH'             ! BOHLINGER*
      * APA00012!14.09.98!    ! NEU: OP 'ZI3'             ! SCHRIEBER*
      * APA00012!15.10.98!    ! NEU: OP 'FD4'             ! STORCH   *
      * APA00012!27.10.98!    ! NEU: OP 'FD5'             ! STORCH   *
      * APA00012!09.11.98!    ! NEU: OP 'FD6'             ! STORCH   *
      * APA00012!04.12.98!    ! NEU: OP 'FEK'             ! LÜTTGES  *
      * APA00012!25.06.99!    ! FEK IN PVW GEÄNDERT       ! STORCH   *
      * AVB00508!20.10.00!    ! NEU: OP 'AM3'             ! WALB     *
      * ACV00015!08.03.01!2.4 ! NEU: OP 'CTV'             ! HEIDER   *
      * APA00295!15.10.04!2.5 ! NEU: OP 'FD7'             ! JÄGER    *
      * ACV00054!24.11.05!2.5 ! NEU: OP 'CT1, 'CT2'       ! HEIDER   *
      * AVB01014!06.07.09!2.9 ! NEU: OP 'FU4','BS4'       ! WALB     *
      * AVB01031!14.05.10!2.10! NEU: OP 'FU5','BS5'       ! WALB     *
      * AVB01085!09.09.11!2.11! NEU: OP 'AM4'             ! WALB     *
040719* VBL-1128!04.07.19!2.12! NEU: OP 'CTN'             ! KLEEMEIER*
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *   HIER SIND ALLE OPERATIONSCODES DEFINIERT, DIE FUER
      *   DIALOGINTERNE UND DRIVER-OPERATIONEN BENOETIGT WERDEN:
      *   DIE OPS SIND IN P:.DVAG.DRA.RAHMEN.KP_OP.SAM
      *   BESCHRIEBEN
      *
      *   1. ALLGEMEIN FESTGELEGTE OPERATIONSCODES
      *
       01  K-OP-ALLGEMEIN.
           05  K-OP-ENTER                PIC  X(003) VALUE 'ENT'.
           05  K-OP-F1                   PIC  X(003) VALUE 'F1 '.
           05  K-OP-F2                   PIC  X(003) VALUE 'F2 '.
           05  K-OP-F3                   PIC  X(003) VALUE 'F3 '.
           05  K-OP-F4                   PIC  X(003) VALUE 'F4 '.
           05  K-OP-F5                   PIC  X(003) VALUE 'F5 '.
           05  K-OP-F6                   PIC  X(003) VALUE 'F6 '.
           05  K-OP-F7                   PIC  X(003) VALUE 'F7 '.
           05  K-OP-F7A                  PIC  X(003) VALUE 'F7A'.
           05  K-OP-F8                   PIC  X(003) VALUE 'F8 '.
           05  K-OP-F8A                  PIC  X(003) VALUE 'F8A'.
           05  K-OP-F9                   PIC  X(003) VALUE 'F9 '.
           05  K-OP-F10                  PIC  X(003) VALUE 'F10'.
           05  K-OP-F11                  PIC  X(003) VALUE 'F11'.
           05  K-OP-F12                  PIC  X(003) VALUE 'F12'.
           05  K-OP-F13                  PIC  X(003) VALUE 'F13'.
           05  K-OP-F14                  PIC  X(003) VALUE 'F14'.
           05  K-OP-F15                  PIC  X(003) VALUE 'F15'.
           05  K-OP-F16                  PIC  X(003) VALUE 'F16'.
           05  K-OP-F17                  PIC  X(003) VALUE 'F17'.
           05  K-OP-F18                  PIC  X(003) VALUE 'F18'.
           05  K-OP-F19                  PIC  X(003) VALUE 'F19'.
           05  K-OP-F20                  PIC  X(003) VALUE 'F20'.
           05  K-OP-F21                  PIC  X(003) VALUE 'F21'.
           05  K-OP-F22                  PIC  X(003) VALUE 'F22'.
           05  K-OP-F23                  PIC  X(003) VALUE 'F23'.
           05  K-OP-F24                  PIC  X(003) VALUE 'F24'.
           05  K-OP-PA1                  PIC  X(003) VALUE 'PA1'.
           05  K-OP-PA2                  PIC  X(003) VALUE 'PA2'.

080301     05  K-OP-CTV                  PIC  X(003) VALUE 'CTV'.
           05  K-OP-DDL                  PIC  X(003) VALUE 'DDL'.
           05  K-OP-DEU                  PIC  X(003) VALUE 'DEU'.
           05  K-OP-DMC                  PIC  X(003) VALUE 'DMC'.
200197     05  K-OP-DPN                  PIC  X(003) VALUE 'DPN'.
300497     05  K-OP-DPO                  PIC  X(003) VALUE 'DPO'.
200197     05  K-OP-DPP                  PIC  X(003) VALUE 'DPP'.
           05  K-OP-DSH                  PIC  X(003) VALUE 'DSH'.
           05  K-OP-DUO                  PIC  X(003) VALUE 'DUO'.
240996     05  K-OP-DUD                  PIC  X(003) VALUE 'DUD'.
           05  K-OP-DWD                  PIC  X(003) VALUE 'DWD'.
241194     05  K-OP-DWS                  PIC  X(003) VALUE 'DWS'.
           05  K-OP-DWU                  PIC  X(003) VALUE 'DWU'.
020296     05  K-OP-DWL                  PIC  X(003) VALUE 'DWL'.
           05  K-OP-HFG                  PIC  X(003) VALUE 'HFG'.
160194*     05  K-OP-HFT                  PIC  X(003) VALUE 'HFT'.
160194     05  K-OP-HSB                  PIC  X(003) VALUE 'HSB'.
      *
      *   2. DIALOGABHAENGIGE OPERATIONSCODES
      *
       01  K-OP-DIALOGABH.
141294     05  K-OP-AHI                  PIC  X(003) VALUE 'AHI'.
060395     05  K-OP-AHM                  PIC  X(003) VALUE 'AHM'.
           05  K-OP-AM1                  PIC  X(003) VALUE 'AM1'.
           05  K-OP-AM2                  PIC  X(003) VALUE 'AM2'.
201000     05  K-OP-AM3                  PIC  X(003) VALUE 'AM3'.
090911     05  K-OP-AM4                  PIC  X(003) VALUE 'AM4'.
110598     05  K-OP-AND                  PIC  X(003) VALUE 'AND'.
110598     05  K-OP-ANV                  PIC  X(003) VALUE 'ANV'.
           05  K-OP-ANZ                  PIC  X(003) VALUE 'ANZ'.
241194     05  K-OP-ASR                  PIC  X(003) VALUE 'ASR'.
           05  K-OP-ASW                  PIC  X(003) VALUE 'ASW'.
110598     05  K-OP-AUS                  PIC  X(003) VALUE 'AUS'.
250197     05  K-OP-AWA                  PIC  X(003) VALUE 'AWA'.
190598     05  K-OP-BER                  PIC  X(003) VALUE 'BER'.
210498     05  K-OP-BPA                  PIC  X(003) VALUE 'BPA'.
250197     05  K-OP-BPU                  PIC  X(003) VALUE 'BPU'.
           05  K-OP-BST                  PIC  X(003) VALUE 'BST'.
           05  K-OP-BS1                  PIC  X(003) VALUE 'BS1'.
           05  K-OP-BS2                  PIC  X(003) VALUE 'BS2'.
           05  K-OP-BS3                  PIC  X(003) VALUE 'BS3'.
060709     05  K-OP-BS4                  PIC  X(003) VALUE 'BS4'.
140510     05  K-OP-BS5                  PIC  X(003) VALUE 'BS5'.
241105     05  K-OP-CT1                  PIC  X(003) VALUE 'CT1'.
241105     05  K-OP-CT2                  PIC  X(003) VALUE 'CT2'.
040719     05  K-OP-CTN                  PIC  X(003) VALUE 'CTN'.
           05  K-OP-DEL                  PIC  X(003) VALUE 'DEL'.
110598     05  K-OP-DMI                  PIC  X(003) VALUE 'DMI'.
240996     05  K-OP-DRU                  PIC  X(003) VALUE 'DRU'.
250197     05  K-OP-ERD                  PIC  X(003) VALUE 'ERD'.
240996     05  K-OP-ESE                  PIC  X(003) VALUE 'ESE'.
           05  K-OP-ESP                  PIC  X(003) VALUE 'ESP'.
           05  K-OP-FD1                  PIC  X(003) VALUE 'FD1'.
           05  K-OP-FD2                  PIC  X(003) VALUE 'FD2'.
220797     05  K-OP-FD3                  PIC  X(003) VALUE 'FD3'.
151098     05  K-OP-FD4                  PIC  X(003) VALUE 'FD4'.
271098     05  K-OP-FD5                  PIC  X(003) VALUE 'FD5'.
091198     05  K-OP-FD6                  PIC  X(003) VALUE 'FD6'.
151004     05  K-OP-FD7                  PIC  X(003) VALUE 'FD7'.
041298*    05  K-OP-FEK                  PIC  X(003) VALUE 'FEK'.
250699     05  K-OP-PVW                  PIC  X(003) VALUE 'PVW'.
           05  K-OP-FI1                  PIC  X(003) VALUE 'FI1'.
           05  K-OP-FI2                  PIC  X(003) VALUE 'FI2'.
060395     05  K-OP-FI3                  PIC  X(003) VALUE 'FI3'.
060697     05  K-OP-FSA                  PIC  X(003) VALUE 'FSA'.
060697     05  K-OP-FSR                  PIC  X(003) VALUE 'FSR'.
250197     05  K-OP-FUA                  PIC  X(003) VALUE 'FUA'.
           05  K-OP-FUD                  PIC  X(003) VALUE 'FUD'.
250197     05  K-OP-FUE                  PIC  X(003) VALUE 'FUE'.
           05  K-OP-FU1                  PIC  X(003) VALUE 'FU1'.
           05  K-OP-FU2                  PIC  X(003) VALUE 'FU2'.
           05  K-OP-FU3                  PIC  X(003) VALUE 'FU3'.
060709     05  K-OP-FU4                  PIC  X(003) VALUE 'FU4'.
140510     05  K-OP-FU5                  PIC  X(003) VALUE 'FU5'.
           05  K-OP-INI                  PIC  X(003) VALUE 'INI'.
           05  K-OP-INS                  PIC  X(003) VALUE 'INS'.
           05  K-OP-KEY                  PIC  X(003) VALUE 'KEY'.
080196     05  K-OP-KPT                  PIC  X(003) VALUE 'KPT'.
           05  K-OP-PRF                  PIC  X(003) VALUE 'PRF'.
030297     05  K-OP-RRD                  PIC  X(003) VALUE 'RRD'.
           05  K-OP-SHR                  PIC  X(003) VALUE 'SHR'.
220797     05  K-OP-ST1                  PIC  X(003) VALUE 'ST1'.
020296     05  K-OP-TBA                  PIC  X(003) VALUE 'TBA'.
020296     05  K-OP-TBE                  PIC  X(003) VALUE 'TBE'.
180898     05  K-OP-TBH                  PIC  X(003) VALUE 'TBH'.
           05  K-OP-TSD                  PIC  X(003) VALUE 'TSD'.
210298     05  K-OP-TSE                  PIC  X(003) VALUE 'TSE'.
           05  K-OP-TSU                  PIC  X(003) VALUE 'TSU'.
           05  K-OP-UAR                  PIC  X(003) VALUE 'UAR'.
           05  K-OP-UPD                  PIC  X(003) VALUE 'UPD'.
190697     05  K-OP-U2R                  PIC  X(003) VALUE 'U2R'.
           05  K-OP-WDI                  PIC  X(003) VALUE 'WDI'.
241194     05  K-OP-ZI1                  PIC  X(003) VALUE 'ZI1'.
260897     05  K-OP-ZI2                  PIC  X(003) VALUE 'ZI2'.
140998     05  K-OP-ZI3                  PIC  X(003) VALUE 'ZI3'.
           05  K-OP-ZSP                  PIC  X(003) VALUE 'ZSP'.
240996     05  K-OP-ZUK                  PIC  X(003) VALUE 'ZUK'.
      *
      ******** ENDE COPY-MEMBER OPERATIONSCODES (DRAT0005) ************



