      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *     Datum   :  November 1995
      *     Kopf aus:  N:PROFILEKOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0116.CPY  $
      *    $Revision:   2.10  $
      *
      *    $Log:   L:/mf/CPY/DRAT0116.CPv  $
      *
      *   Rev 2.10   May 22 2018 09:45:56   t13ae46
      *APA02579: FriWi: Provisionsverteilung- und Abrechnung
      *
      *   Rev 2.9   Sep 08 2015 14:03:40   t13ae37
      *APA01932: ABU / Schnittstellen und Referenz
      *
      *   Rev 2.8   Jun 10 2008 10:03:10   t13ex21
      *APA00712: FVD Stufe 1: Programmänderungen
      *
      *   Rev 2.7   Dec 10 2007 07:55:42   t13ex21
      *APA00622: Programmänderungen Martina
      *
      *   Rev 2.6   Dec 17 2004 10:01:32   t13ex44
      *APA00218: Auslagerung von Provisionsdaten
      *
      *   Rev 2.5   12 Sep 2001 14:37:50   t13ae46
      *APA00144: K-KNR-PROT-ART-PEG-SICH ergaenzt
      *
      *   Rev 2.4   25 Jun 2001 08:04:36   t13ae13
      *AVB00534 - neue Protokollart für Einführung PIN
      *
      *   Rev 2.3   Jun 29 1999 14:10:22   T13EX22
      *K-KNR-PROT-ART-PMPT-ZUKUENFTIG neu
      *
      *   Rev 2.2   Jun 29 1999 10:34:32   T13EX25
      *Neue KNR für PNAP Batch-Verarbeitung
      *
      *   Rev 2.1   May 25 1999 13:15:14   T01JM2S
      *APA00012 - COMPILE-FEHLERBEHEBUNG PMPA0038
      *
      *   Rev 2.0   Apr 21 1999 17:21:46   T01JM2S
      *NT-Umstellung
      *
      *   Rev 1.2   11 Nov 1998 09:29:54   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.1   Mar 07 1997 09:18:00   P13PVCS
      *Neuerstellung
      *
      *   Rev 1.0   Feb 19 1997 12:18:36   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0116  KONSTANTEN FÜR PROT-ART   KTY = 106*
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: PRIGGEN                                         *
      *    DATUM:    09.09.1996                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DAISY 1 !05.09.96! 01 ! NEUERSTELLUNG             ! PRIGGEN  *
      * AFPA00..!30.01.97! 02 !K-KNR-PROT-ART-PEW-EXPORT  !BOHLINGER *
      * AFPA0012!20.06.97! 03 ! K-KNR-PROT-ART-PROVISION  ! WOLFF    *
      *         !        !    ! EINGEFÜGT                 !          *
      * AFPA0012!08.09.98! 04 ! K-KNR-PROT-ART-SU-PRODART ! D.WILHELM*
      *         !        !    ! EINGEFÜGT                 !          *
      * AFPA0012!16.09.98! 05 ! PROTOKOLLARTEN ÜBERARBEITET AUERBACH *
      *         !        !    ! NEUE HINZUGEFÜGT, FÜR     !          *
      *         !        !    ! BESTEHENDE NEUE NUMMERN   !          *
      *         !        !    ! VERGEBEN                  !          *
      * APA00012!25.05.99!2.1 ! K-KNR-PROT-KORR-PROV-     ! WOLFF    *
      *         !        !    ! RUECKSTELLUNG EINGEFÜGT   !          *
      * APA00012!28.06.99!2.2 ! K-KNR-PROT-ART-PNAP-BATCH ! JÄGER    *
      * APA00012!29.06.99!2.3 ! PROT-ART-PMPT-ZUKUENFTIG  ! MUND     *
      * AVB00543!22.06.01!2.4 ! NEUE PROT-ART F.VB-BEREICH! WALB     *
      * APA00144!31.08.01!2.5 ! K-KNR-PROT-ART-PEG-SICH   ! WANG     *
      * APA00218!15.07.04!2.6 ! K-KNR-PROT-ART-AUSLAGERUNG! SCHEIDEL *
      * APA00622!17.09.07!2.7 ! K-KNR-PROT-ART-SST-MAA    ! STORCH   *
      * APA00712!25.03.08!2.8 ! K-KNR-PROT-ART-SST-MA-FVD ! STORCH   *
      * APA01932!08.09.15!2.9 ! K-KNR-PROT-ART-SST-MA-UST ! BUCH     *
      * APA02579!01.03.18!3.0 ! FVD HERAUSGENOMMEN        ! WANG     *
      *         !        !    ! K-KNR-PROT-ART-SST-MA-ALAG!          *
      *         !        !    ! K-KNR-PROT-ART-SU-UST-ALAG!          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  FILLER.
 
 
      ***  0 - 9 SIND FÜR PEW-PROTOKOLLE RESERVIERT
           05 K-KNR-PROT-ART-AN-TDS-UMS     PIC S9(004) BINARY SYNC
                                                        VALUE  1.
           05 K-KNR-PROT-ART-PEW-ERSTEL     PIC S9(004) BINARY SYNC
                                                        VALUE  2.
           05 K-KNR-PROT-ART-PEW-EXPORT     PIC S9(004) BINARY SYNC
                                                        VALUE  3.
 
 
      ***  10 - 19 SIND PEG UND PVW-PROTOKOLLE RESERVIERT
           05 K-KNR-PROT-ART-PEG-BATCH      PIC S9(004) BINARY SYNC
                                                        VALUE  10.
           05 K-KNR-PROT-ART-PVW-BATCH      PIC S9(004) BINARY SYNC
                                                        VALUE  11.
280699     05 K-KNR-PROT-ART-PNAP-BATCH     PIC S9(004) BINARY SYNC
280699                                                  VALUE  12.
290699     05 K-KNR-PROT-ART-PMPT-ZUKUENFTIG PIC S9(004) BINARY SYNC
290699                                                  VALUE  13.
310801     05 K-KNR-PROT-ART-PEG-SICH        PIC S9(004) BINARY SYNC
310801                                                  VALUE  14.
 
 
      ***  20 - 29 SIND FÜR SST-MA UND KONTROLLSYSTEM-PROTOKOLLE RESERV.
           05 K-KNR-PROT-ART-SU-PRODART     PIC S9(004) BINARY SYNC
                                                        VALUE  20.
           05 K-KNR-PROT-ART-SU-SST-MA      PIC S9(004) BINARY SYNC
                                                        VALUE  21.
           05 K-KNR-PROT-ART-SU-AP          PIC S9(004) BINARY SYNC
                                                        VALUE  22.
080915***  DIE PROTOKOLLARTEN 23 UND 24 BLEIBEN UNGENUTZT.
           05 K-KNR-PROT-ART-SU-UST-DVAG    PIC S9(004) BINARY SYNC
                                                        VALUE  23.
           05 K-KNR-PROT-ART-SU-UST-ALLF    PIC S9(004) BINARY SYNC
                                                        VALUE  24.
           05 K-KNR-PROT-ART-SU-BUCH-DAT    PIC S9(004) BINARY SYNC
                                                        VALUE  25.
           05 K-KNR-PROT-ART-DTA-GELDSTAND  PIC S9(004) BINARY SYNC
                                                        VALUE  26.
250599     05 K-KNR-PROT-KORR-PROV-RUECKST  PIC S9(004) BINARY SYNC
250599                                                  VALUE  27.
150704     05 K-KNR-PROT-AUSLAGERUNG        PIC S9(004) BINARY SYNC
150704                                                  VALUE  28.
170907     05 K-KNR-PROT-ART-SU-SST-MAA     PIC S9(004) BINARY SYNC
170907                                                  VALUE  29.
 
220601***  30 - 39 SIND FÜR VB-BEREICH RESERVIERT
220601     05 K-KNR-PROT-ART-PIN            PIC S9(004) BINARY SYNC
220601                                                  VALUE  30.
010318*    05 K-KNR-PROT-ART-SU-SST-MA-FVD  PIC S9(004) BINARY SYNC
010318*                                                 VALUE  31.
010318     05 K-KNR-PROT-ART-SU-SST-MA-ALAG PIC S9(004) BINARY SYNC
010318                                                  VALUE  31.
270218     05 K-KNR-PROT-ART-SU-UST-ALAG    PIC S9(004) BINARY SYNC
270218                                                  VALUE  32.
170907***  DIE PROTOKOLLART 42 BLEIBT UNGENUTZT.
      *
      *** ENDE COPY-STRECKE ******************************************


