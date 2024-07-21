      *---------------------------------------------------------------*
      *    STANDARD FÜR ALLE OBJEKTE UNTER PVCS
      *            DATUM   :  NOVEMBER 1995
      *            KOPF AUS:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    COPYRIGHT 1995 ATLAS DIENSTLEISTUNGS GMBH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0003.CPY  $
      *    $Revision:   2.11  $
      *
      *    $Log:   L:/mf/CPY/DRAT0003.CPv  $
      *
      *   Rev 2.11   Jan 13 2014 07:21:36   t13ex67
      *APA01539: Kalypso TP5: Anpassungen Datenmodell: TBPA0023-TBP
      *A0033
      *
      *   Rev 2.10   10 May 2001 13:07:58   T13AE34
      *ACV00015-CTV-Briefe aus Neudialogen
      *
      *   Rev 2.9   Mar 16 2000 16:19:42   T13EX13
      *APA00067 Anpassung an Badenia +
      *Umstellung PGDF -> PGKF
      *
      *   Rev 2.8   Mar 13 2000 17:56:12   T13EX13
      *APA00087 Umstellung auf PPGZ 
      *
      *   Rev 2.7   Mar 09 2000 17:30:22   T13EX13
      *APA00087 Umstellung auf PPGZ 
      *
      *   Rev 2.6   Jan 04 2000 08:53:36   T13AE18
      *APA00068
      *
      *   Rev 2.5   Dec 17 1999 16:19:50   T13AE18
      *APA00068
      *
      *   Rev 2.3   Jun 29 1999 10:22:08   T13AE34
      *FRZ00028-PVCS Korrektur
      *
      *   REV 2.2   Jun 29 1999 14:44:34   T13AE34
      *FRZ00028-PVCS Korrektur
      *
      *   REV 2.1   APR 06 1999 14:44:34   T13AE34
      *AAT00005-Euro-Switch
      *
      *   REV 2.0   MAR 01 1999 14:44:34   T13AE34
      *NT-UMSTELLUNG
      *
      *   REV 1.6   13 NOV 1998 16:53:18   T13EX26
      *PEG-PRODUKTIONSÜBERGABE
      *
      *   REV 1.5   27 AUG 1997 17:39:38   T13EX23
      *AFPA0022 - ANPASSUNG RAHMEN\DRIVER
      *
      *   REV 1.4   25 JUL 1997 09:47:18   T13AE34
      *AFAN0146 - DRIVER-ANPASSUNG ALT-NEUDIALOGE
      *
      *   REV 1.3   10 JUL 1997 11:46:14   T13EX11
      *AFPA0019/AFPA0021 - KONST. FUER KOMPETENZ-PRUEFUNG UND UPRO-NAME
      *
      *   REV 1.2   MAY 12 1997 10:58:44   T13EX11
      *NEUE KONSTANTEN FUER UPRO-NAMEN
      *
      *   REV 1.1   MAR 20 1997 17:00:02   T13AE31
      *DRIVER-ANPASSUNG DAISY
      *
      *   REV 1.0   NOV 04 1995 08:33:52   P13SP99
      *INITIAL REVISION.
      *
      *---------------------------------------------------------------*

      *****************************************************************
      *    COPY-MEMBER: KONSTANTEN FUER DIALOGSTEUERUNG (DRAT0003)    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: W. SCHOELLHAMMER (INTEGRATA AG)                  *
      *    DATUM:    30.10.1992                                       *
      *****************************************************************
      *    VERSION   2    02.05.95 16:35:24
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !24.04.93! 01 ! NEU                       ! SCHÖLLH. *
      *         !15.09.93! 02 ! NEU: K-FTAST-OK-FELD      ! SCHÖLLH. *
      * FEVB0218!03.03.95!    ! NEU: K-KLEIN/GROSSBUCHST. ! SCHÖLLH. *
      *         !10.03.95! 03 ! PROD.UEBERGABE            ! SCHÖLLH. *
      * AFVB0182!15.03.95!    ! NEUE KONSTANTEN           ! SCHÖLLH. *
      *         !28.04.95! 04 ! PROD.UEBERGABE            ! SCHÖLLH. *
      * AFPA0002!20.01.97! 05 ! NEUE/GEÄNDERTE KONSTANTEN ! SCHÖLLH. *
      *         !03.02.97!    ! KONSTANTEN FÜR UPRO-NAMEN ! AUERBACH *
      * AFVB0339!16.04.97! 06 ! KONSTANTEN FÜR UPRO-NAMEN ! SCHÖLLH. *
      * AFPA0019!10.06.97! 07 ! KONSTANTEN FÜR KOMP-PRF,  ! SCHÖLLH. *
      *         !        !    ! UPRO-NAMEN                !          *
      * AFPA0021!04.07.97! 08 ! KONSTANTEN FÜR UPRO-NAMEN ! HEIDER   *
      * AFAN    !        ! 09 ! KONSTANTEN FÜR UPRO-NAMEN ! HEIDER   *
      *         !09.06.98! 10 ! K-KLEINB.. WIEDER AUF     !BOHLINGER *
      *         !        !    ! KLEINSCHREIBUNG GESETZT   !          *
      * AFPA0012!17.07.98! 11 ! KONSTANTE FüR DIALOGMODUL !BOHLINGER *
      *         !        !    !                           !          *
      * AFPA0012!11.10.98! 12 ! KONSTANTE FüR TSQ-NAME    !BOHLINGER *
      *         !        !    ! K-TSQ-NAME-PRKO           !          *
      *         !        !    ! K-TSQ-NAME-PPGR           !          *
      * AAT00005!17.02.99!1.7 ! EURO-SWITCH IN NEUDIALOGEN!HEIDER    *
      * AFPA0068!06.12.99!1.8 ! KONSTANTE FÜR TSQ-NAME    !BOHLINGER *
      *         !        !    ! K-TSQ-NAME-PGDF           !          *
      *         !        !    ! K-TSQ-NAME-PGVZ           !          *
      * APA00087!01.03.00!1.9 ! KONSTANTE FÜR TSQ-NAME    !WILKENS   *
      *         !        !    ! K-TSQ-NAME-PPGZ           !          *
      * APA00067!14.03.00!1.10! KONSTANTE FÜR TSQ-NAME    !WILKENS   *
      *         !        !    ! K-TSQ-NAME-PGKF           !          *
      * ACV00015!08.03.01!2.10! CTV-TRANS: 'ZA13'         !HEIDER    *
      * APA01539!01.10.13!2.11! KONSTANTE FÜR TSQ-NAME    ! STORCH   *
      *         !        !    ! K-TSQ-NAME-PRKL           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
       01  K-DIALOGSTEUERUNG-KONSTANTEN.

      *   MAX. INDEX-WERT ZUR OP-TABELLE
           05  K-DI-OP-MAX           PIC S9(004) COMP VALUE 10.

200197*   MAX. INDEX-WERT ZUR CARU-TSQ-INFOS-TAB
200197     05  K-I-TSQI-MAX          PIC S9(004) COMP VALUE 40.

      *   MAX. ANZAHL SITZUNGS-UNTERBRECHUNGEN (TECHNISCH MAX: 1)
           05  K-USITZG-ANZ-MAX      PIC  9(001)      VALUE 1.

200197*   MAX. ANZAHL FOLGEDIALOGE(MIT GEDÄCHTNIS) (TECHNISCH MAX: 31)
200197     05  K-DIALOG-FOLGE-NR-MAX PIC  9(002)      VALUE 31.

      *   MAX. ANZAHL UNTERANWENDUNGEN (TECHNISCH MAX: 7)
           05  K-UANW-MAX            PIC  9(001)      VALUE 7.

200197*   MAX. ANZAHL PAGES (ZUSAMMENGEHÖRENDE MAPS EINES LEVELS, DIE
200197*        AUF MEHRERE PGMS VERTEILT SIND) (TECHNISCH MAX: 3)
200197     05  K-PAGE-MAX            PIC  9(001)      VALUE 3.

      *   KONSTANTEN FUER SCHNITTSTELLE NEU-/ ALT-ANWENDUNGEN
           05  K-TRANS-HAUPTMENUE    PIC  X(004) VALUE 'DVAG'.
250797     05  K-ALT-NEU             PIC  X(001) VALUE 'A'.
           05  K-NEUANW-AUFRUF       PIC  X(001) VALUE 'N'.
           05  K-SYSTEM-PROD         PIC  X(001) VALUE 'P'.
100697     05  K-SYSTEM-TEST         PIC  X(001) VALUE 'T'.

      *   KONSTANTEN FUER TRANSAKTIONSTYPEN
100697     05  K-TRANS-TYP-MENUE     PIC  X(001) VALUE '1'.
100697     05  K-TRANS-TYP-DIALOG    PIC  X(001) VALUE '2'.
100697     05  K-TRANS-TYP-UANW      PIC  X(001) VALUE '3'.
100697     05  K-TRANS-TYP-SUBMENUE  PIC  X(001) VALUE '4'.

      *   TRANSAKTIONEN FUER SONDERDIALOGE (UNTERANWENDUNGEN)
           05  K-TRANS-SCHLUEHILFE   PIC  X(004) VALUE 'ZA02'.
           05  K-TRANS-HILFEDIALOG   PIC  X(004) VALUE 'ZA03'.
080301     05  K-TRANS-CTV           PIC  X(004) VALUE 'ZA13'.

      *   F-TASTEN-ANZEIGE FUER DIALOGUNTERBRECHUNG/-WIEDERAUFNAHME
           05  K-FTAST-UNTERBR       PIC  X(009) VALUE ' F23=UNTB'.
           05  K-FTAST-WIEDERAUFN    PIC  X(009) VALUE ' F24=WAUF'.
           05  K-FTAST-UNTBR-WAUFN   PIC  X(009) VALUE ' F23 F24 '.
           05  K-FTAST-ABBRUCH       PIC  X(009) VALUE ' F12=ABBR'.
           05  K-FTAST-OK-FELD       PIC  X(009) VALUE ' F21=OK-F'.
150395     05  K-FTAST-CTV           PIC  X(009) VALUE ' F22=CTV '.
170299     05  K-EURO-SWITCH-DM
170299                               PIC  X(003) VALUE 'DM '.
170299     05  K-EURO-SWITCH-OES
170299                               PIC  X(003) VALUE 'OES'.
170299     05  K-EURO-SWITCH-EURO
170299                               PIC  X(003) VALUE 'EUR'.

      *   KONSTANTEN FUER DIALOGMASKE KOPF UND FUSS
           05  K-PROD                PIC  X(004) VALUE 'PROD'.
           05  K-TEST                PIC  X(004) VALUE 'TEST'.
           05  K-BEZ-CICS-DATUM      PIC  X(005) VALUE 'CICS:'.
           05  K-CTV                 PIC  X(004) VALUE 'CTV:'.
           05  K-OK                  PIC  X(003) VALUE 'OK:'.
           05  K-J                   PIC  X(001) VALUE 'J'.
           05  K-N                   PIC  X(001) VALUE 'N'.

      *   DEFINITIONEN FUER ALLGEMEINE HINWEISMELDUNGEN
           05  K-AT-PGM-NAME         PIC  X(008) VALUE 'PGAT9999'.

      *   DEFINITIONEN FUER HINWEISMELDUNGEN DER DIALOGMODULE
      *   FüR DIE BESTANDSDIALOGE PO11 BIS PO25
170798     05  K-PO-PGM-NAME         PIC  X(008) VALUE 'PGPO9999'.

      *   KONSTANTE FüR TSQ-NAME FüR DIALOGüBERGREIFENDE TS-QUEUES
      *   DIE VON ALLEN ANWENDERN GENUTZT WERDEN KöNNEN
      ***      TSQ-PRKO FüR BUFFER-OZP ZYPA0027
111098     05  K-TSQ-NAME-PRKO       PIC  X(008) VALUE '9999PRKO'.
      ***      TSQ-PRKL FÜR BUFFER-OZP ZYPA0027
011013     05  K-TSQ-NAME-PRKL       PIC  X(008) VALUE '9999PRKL'.
      ***      TSQ-PPGR-PGRP FÜR BUFFER-OZP ZYPA0028
111098     05  K-TSQ-NAME-PPGR-PGRP  PIC  X(008) VALUE '9999PPGR'.
      ***      TSQ-PGDF 'PG-ABHÄNGIGE DIALOGFELDER' FÜR BUFFER ZYPA0034
061299     05  K-TSQ-NAME-PGKF       PIC  X(008) VALUE '9999PGKF'.
      ***      TSQ-PGVZ 'PGV-ZUORDNUNG' FÜR BUFFER ZYPA0035
061299     05  K-TSQ-NAME-PGVZ       PIC  X(008) VALUE '9999PGVZ'.
      ***      TSQ-PPGZ 'PGV-GEVO-ZUORDNUNG' FÜR BUFFER ZYPA0033
061299     05  K-TSQ-NAME-PPGZ       PIC  X(008) VALUE '9999PPGZ'.

030297*   DEFINITIONEN DER IM RAHMEN VERWENDETEN UPRO'S
030297     05  K-UPAT0001            PIC  X(008) VALUE 'UPAT0001'.
030297     05  K-UPAT0003            PIC  X(008) VALUE 'UPAT0003'.
030297     05  K-UPAT0005            PIC  X(008) VALUE 'UPAT0005'.
030297     05  K-UPAT0007            PIC  X(008) VALUE 'UPAT0007'.
160497     05  K-UPAT0013            PIC  X(008) VALUE 'UPAT0013'.
100697     05  K-UPAT0017            PIC  X(008) VALUE 'UPAT0017'.
040797     05  K-UPAT0018            PIC  X(008) VALUE 'UPAT0018'.
170299     05  K-UPAT0023            PIC  X(008) VALUE 'UPAT0023'.
030297     05  K-ZWPA0015            PIC  X(008) VALUE 'ZWPA0015'.
040797     05  K-ZWAT0008            PIC  X(008) VALUE 'ZWAT0008'.

      *   FEHLERMELDUNG BEI DIREKTEINSTIEG UEBER TAC DES DIALOGS
           05  K-TACERR              PIC  X(076) VALUE
                     '       ***** BITTE MELDEN SIE SICH UEBER DAS KONTR
      -              'OLL-SYSTEM AN *****       '.
           05  K-TACERR-LEN          PIC S9(004) COMP VALUE 76.

      *   KONSTANTEN FUER WERTE VON DI_TYP (IN TAB. DIALOG-INFO)
           05  FILLER.
               10  K-DI-TYP-MENUE    PIC  X(001)     VALUE '1'.
               10  K-DI-TYP-ANW      PIC  X(001)     VALUE '2'.
               10  K-DI-TYP-UANW     PIC  X(001)     VALUE '3'.

150395*   KONSTANTEN FUER DIALOG-STATUS (IN TAB. DIALOG-ZUSTAND)
150395     05  FILLER.
150395         10  K-DIALOG-STATUS-INARBEIT      PIC  9(001) VALUE ZERO.
150395         10  K-DIALOG-STATUS-INI           PIC  9(001) VALUE 1.
150395         10  K-DIALOG-STATUS-ABGESCHLOSSEN PIC  9(001) VALUE 2.

      *   MAP-NAMEN DER RAHMENMASKEN
           05  K-MAP-RAHMEN-KOPF    PIC  X(007) VALUE 'BAT0001'.
           05  K-MAP-RAHMEN-FUSS    PIC  X(007) VALUE 'BAT0002'.
           05  K-MAP-RAHMEN-TRACE   PIC  X(007) VALUE 'BAT0009'.

030395*   ARBEITSFELDER FÜR UMSETZUNG VON KLEIN- IN GROSS-BUCHSTABEN
030395     05  K-KLEINBUCHSTABEN    PIC  X(029)      VALUE
090698               'abcdefghijklmnopqrstuvwxyzäöü'.
030395     05  K-GROSSBUCHSTABEN    PIC  X(029)      VALUE
030395               'ABCDEFGHIJKLMNOPQRSTUVWXYZÄÖÜ'.
      *
      ******** ENDE COPY-MEMBER KONST. DIALOG-STEUERUNG (DRAT0003) ****
