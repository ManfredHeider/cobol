      * T13EX28   Stand: 12/11/98    12:37:32
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0205.CPY  $
      *    $Revision:   2.13  $
      *
      *    $Log:   L:/mf/CPY/DRVB0205.CPv  $
      *
      *   Rev 2.13   Nov 27 2017 13:40:38   t13ex71
      *AVB01347: Lucy - USt.pfl. Provisionen / VB-System - steuerl.
      * Status von Zeitpunkt '2' auf '5' in UPVB0189 und ZWVB0072 (
      *
      *
      *   Rev 2.12   Nov 27 2017 13:22:38   t13ex71
      *AVB01347: Lucy - USt.pfl. Provisionen / VB-System - steuerl.
      * Status von Zeitpunkt '2' auf '5' in UPVB0189 und ZWVB0072 (
      *
      *
      *   Rev 2.11   Jun 30 2016 11:51:34   t13ex71
      *AVB01400: Projekt Lucy2 - Neue Funktion in UPVB0189 aufnehme
      *n (BPR)
      *
      *   Rev 2.10   Jan 26 2016 07:40:46   t13ex67
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System
      *
      *   Rev 2.9   Feb 16 2011 11:25:34   t13ae13
      *AVB01036: Einheitenzählung bei Betreuerwechsel
      *
      *   Rev 2.8   13 Apr 2007 17:26:32   t13ae13
      *AVB00907: Beförderungsbedingungen Österreich 2007
      *
      *   Rev 2.7   Oct 11 2004 12:05:10   t13ae27
      *ASL00183: MAP - Manageraufbauplan Ermittlung Ansprüche für D
      *-Leiter (Fiktiv)
      *
      *   Rev 2.6   Apr 17 2002 14:33:34   t13ae27
      *Neue Erfolgsprovisionsbedingungen ab 04/2002
      *
      *   Rev 2.5   31 May 2001 10:37:56   t13ae27
      *neue Funktion
      *
      *   Rev 2.4   19 Mar 2001 17:20:22   T13AE30
      *AAS00264- LEISTUNGSPLAN / WETTBEWERB 2001
      *
      *   Rev 2.3   06 Oct 2000 10:39:48   T13AE30
      *ASL00067 - FÖBO2
      *
      *   Rev 2.2   Jun 10 1999 11:17:30   T13AE34
      *APA00012-SST Daisy
      *
      *   Rev 2.1   Apr 22 1999 18:42:08   T01JM2E
      *
      *
      *   Rev 1.0   23 Jul 1997 12:15:22   P13SP99
      *Initial revision.
      *
      *--------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVB0205  FUNKTIONEN ZU LESE-OZP TABELLE     *
      *                                            ZPKT TBVB0072     *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: JÄGER-SOKOLUK                                   *
      *    DATUM:    05.01.1998                                      *
      ****************************************************************
      *    VERSION   02
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * DAISY 1 !05.01.98!01.01! NEUERSTELLUNG            ! JÄGER    *
      * AFPA0012!22.10.98!01.02! NEU: DRVB0327            ! WOLFF    *
      * ASL00025!29.10.98!01.03! NEU: DRVB0330            ! WALB     *
      * DAISY   !15.04.99!01.04! NEU: DRVB0377            ! D.WILHELM*
      * APA00012!28.05.99! 2.2 ! NEU: DRVB0398            ! HEIDER   *
      * APA00012!04.06.99! 2.2 ! NEU: DRVB0415            ! HEIDER   *
      * ASL00067!20.09.00! 2.3 ! NEU: DRVB0503/507        ! TH.WILLIG*
      * AAS00264!28.02.01! 2.4 ! NEU: DRVB0524            ! TH.WILLIG*
      * AAS00274!14.05.01! 2.5 ! NEU: DRVB0543            ! P. DUERR *
      * AVB00599!19.03.02! 2.6 ! NEU: DRVB0583            ! P. DUERR *
      * AVB00731!24.08.04! 2.7 ! NEU: DRVB0731            ! P. DUERR *
      * AVB00907!08.03.07! 2.8 ! NEU: DRVB0844            ! S. WALB  *
      * AVB01036!01.12.10! 2.9 ! NEU: DRVB0983            ! S. WALB  *
      * APA01932!25.01.16! 2.10! NEU: DRVB1127            ! STORCH   *
      * AVB01400!24.05.16! 2.11! NEU: DRVB1142            ! B.PRETSCH*
      * AVB01347!09.11.17! 2.12! NEU: DRVB1172            ! B.PRETSCH*
      *         !        !     !                          !          *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-ZWVB0072.
           05 PI-ZWVB0072-FUNKTION            PIC X(008).
              88 PI-ZWVB0072-DRVB0206             VALUE 'DRVB0206'.
              88 PI-ZWVB0072-DRVB0327             VALUE 'DRVB0327'.
              88 PI-ZWVB0072-DRVB0330             VALUE 'DRVB0330'.
              88 PI-ZWVB0072-DRVB0377             VALUE 'DRVB0377'.
              88 PI-ZWVB0072-DRVB0398             VALUE 'DRVB0398'.
              88 PI-ZWVB0072-DRVB0415             VALUE 'DRVB0415'.
              88 PI-ZWVB0072-DRVB0503             VALUE 'DRVB0503'.
              88 PI-ZWVB0072-DRVB0507             VALUE 'DRVB0507'.
              88 PI-ZWVB0072-DRVB0524             VALUE 'DRVB0524'.
              88 PI-ZWVB0072-DRVB0543             VALUE 'DRVB0543'.
              88 PI-ZWVB0072-DRVB0583             VALUE 'DRVB0583'.
              88 PI-ZWVB0072-DRVB0731             VALUE 'DRVB0731'.
              88 PI-ZWVB0072-DRVB0844             VALUE 'DRVB0844'.
              88 PI-ZWVB0072-DRVB0983             VALUE 'DRVB0983'.
              88 PI-ZWVB0072-DRVB1127             VALUE 'DRVB1127'.
              88 PI-ZWVB0072-DRVB1142             VALUE 'DRVB1142'.
              88 PI-ZWVB0072-DRVB1172             VALUE 'DRVB1172'.
      *
 
 

