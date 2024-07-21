      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0319.CPY  $
      *    $Revision:   2.16  $
      *
      *    $Log:   L:/mf/CPY/DRAT0319.CPv  $
      *
      *   Rev 2.16   Nov 13 2020 14:06:58   t13ex71
      *ASL00647: Sperrung Geldleistungen bei Entscheidung gegen neu
      *e Anlage B - Copybooks zum PMRW0043 (BPR)
      *
      *   Rev 2.15   Sep 07 2017 14:40:30   t13ex71
      *ASL00567: Lucy2 -> Teilauszahlung wenn Status nicht erklärt
      *- Anpassung an PMRW0043
      *
      *   Rev 2.14   Jun 14 2016 14:48:26   t13ex30
      *AVB01400: Projekt Lucy2 - STAB-Korrekturbuchungen PMRW0044
      *
      *   Rev 2.13   Jun 24 2015 15:19:28   t13ex71
      *ASL00479: Rückstufungen in Nebenberuf bzw. Sollsaldo in PMRW
      *0018
      *
      *   Rev 2.12   Jan 09 2014 15:10:28   t13ex13
      *ASL00504: Columbusbonus
      *
      *   Rev 2.11   Mar 23 2011 09:55:06   t13ex43
      *ASL00376: PGM - Sollsalden prüfen bei Auszahlung von Geldlei
      *stungen
      *
      *   Rev 2.10   Oct 08 2010 10:15:50   t13ex43
      *ASL00362: PGM - Auszahlung Columbus-GSH (neu - abgerechnet)
      *
      *   Rev 2.9   Dec 03 2009 10:03:54   t13ae36
      *FVB00735: Korrekturen im VB-System:KNR-Änderung
      *
      *   Rev 2.8   Sep 29 2009 15:19:48   t13ae36
      *AAS00815: Kein Geldscheinheft für Betreuer gekündigter Partner
      *
      *   Rev 2.7   Jul 11 2005 13:12:40   T13AE27
      *ASL00204: K-Leistungszuschuss
      *
      *   Rev 2.6   Jul 19 2004 09:34:12   t13ae27
      *ASL00183: MAP - Manageraufbauplan - Fibu-SST
      *
      *   Rev 2.5   May 26 2004 09:21:06   t13ex43
      *PGM - Dimension 7 Zukunft Geldscheinheft
      *
      *   Rev 2.4   Nov 11 2003 14:36:32   t13ae35
      *ASL00169 - neue Konstante für BOZ-Anspruch
      *
      *   Rev 2.3   28 Nov 2000 16:27:56   T13AE35
      *ASL00055 - neue KNR für Austritt
      *
      *   Rev 2.2   10 May 2000 14:20:14   T13AE26
      *ASL00046 Förderbonus Auszahlung
      *
      *   Rev 2.1   Apr 18 2000 12:43:10   T13AE26
      *ASL00046 Einführung Förderbonus (Dimension 7)
      *
      *   Rev 2.0   Mar 16 1999 09:57:24   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0319  KONSTANTEN FÜR KTY 69              *
      *                           Anspruchsstatus (Sonderleistung)   *
      *    -------------------------------------------------------   *
      *    ERSTELLT: T. Moll                                         *
      *    DATUM:    30.03.2000                                      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ASL00046!30.03.00!2.1 ! NEU                       ! T. Moll  *
      * ASL00046!04.05.00!2.2 ! KNR für Pfändung          ! S. Walb  *
      * ASL00055!13.11.00!2.3 ! KNR für Austritt          ! S. Walb  *
      * ASL00169!31.10.03!2.4 ! KNR für kein BOZ          ! J.Hering *
      * ASL00182!06.05.04!2.5 ! KNR FÜR FEHLENDE BANKVERB.! KLEEMEIER*
      * ASL00204!15.06.05!2.6 ! KNR für LZB mit KLZ       ! P.Duerr  *
      * AAS00815!30.09.09!2.7 ! KNR für gekündigte Partner! R.Franke *
      * FVB00735!03.12.09!2.8 ! KNR für g.P. geändert     ! R.Franke *
      * ASL00362!30.09.10!2.10! KNR FÜR FEHLENDE VM-ÜBERS.! KLEEMEIER*
      * ASL00376!22.03.11!2.11! KNR FÜR SOLLSALDO         ! KLEEMEIER*
      * ASL00479!05.06.15!2.12! KNR FÜR RÜCKSTUFUNG IN    ! B.PRETSCH*
      *         !        !    ! NEBENBERUF                !          *
      * AVB01400!14.06.15!2.13! KNR FÜR STEUERLICHER      ! H.WÖLFLIK*
      *         !        !    ! STATUS FEHLT              !          *
      * ASL00567!24.08.17!2.14! Teilauszahlung wenn Status! B.Pretsch*
      *         !        !    ! nicht erklärt             !          *
      * VLA-1014!07.09.20!2.15! Keine Auszahlung bei Ab-  ! B.Pretsch*
      *         !        !    ! lehnung Anlage B          !          *
      * VLA-2985!20.07.21!2.16! Auszahlungssperre 1       ! B.Pretsch*
200423* VLA-6436!20.04.23!2.28! Auszahlungssperre 2       ! L.Betz   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  FILLER.
           05 K-KNR-ANSPR-OFFEN           PIC S9(004) BINARY VALUE  1.
           05 K-KNR-ANSPR-AUSGEZAHLT      PIC S9(004) BINARY VALUE  2.
           05 K-KNR-ANSPR-WIDERRUFEN      PIC S9(004) BINARY VALUE  3.
           05 K-KNR-ANSPR-PFAENDUNG       PIC S9(004) BINARY VALUE  4.
           05 K-KNR-ANSPR-AUSTRITT        PIC S9(004) BINARY VALUE  5.
           05 K-KNR-ANSPR-BOZ             PIC S9(004) BINARY VALUE  6.
           05 K-KNR-ANSPR-BV-FEHLT        PIC S9(004) BINARY VALUE  7.
           05 K-KNR-ANSPR-MAP-KEINE-ERKL  PIC S9(004) BINARY VALUE  8.
           05 K-KNR-ANSPR-WIDERRUFEN-KLZ  PIC S9(004) BINARY VALUE  9.
      **   05 K-KNR-ANSPR-AUSTRITT-PARTN  PIC S9(004) BINARY VALUE 10.
           05 K-KNR-ANSPR-AUSTRITT-PARTN  PIC S9(004) BINARY VALUE 11.
           05 K-KNR-ANSPR-FEHL-VM-UEBERS  PIC S9(004) BINARY VALUE 12.
           05 K-KNR-ANSPR-SOLLSALDO       PIC S9(004) BINARY VALUE 13.
           05 K-KNR-ANSPR-KEINE-AUSZ-COB  PIC S9(004) BINARY VALUE 14.
           05 K-KNR-ANSPR-RCKST-IN-NB     PIC S9(004) BINARY VALUE 15.
           05 K-KNR-ANSPR-UST-STAT-FEHLT  PIC S9(004) BINARY VALUE 16.
           05 K-KNR-ANSPR-NUR-FREI-AUSGZ  PIC S9(004) BINARY VALUE 17.
           05 K-KNR-ANSPR-ANL-B-ABGEL     PIC S9(004) BINARY VALUE 18.
           05 K-KNR-ANSPR-SPERRE-AUSZ     PIC S9(004) BINARY VALUE 19.
200423     05 K-KNR-ANSPR-SPERRE-GEST     PIC S9(004) BINARY VALUE 20.
      ****
      **** ENDE COPY DRAT0319 ****************************************
      ****
 
 


