      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0155.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/MF/CPY/DRAT0155.CPv  $
      *
      *   Rev 2.3   Jun 05 2002 14:21:52   t13ex21
      *APA00191 - Neue Konditionsregel
      *
      *   Rev 2.2   Apr 03 2000 11:10:00   T13EX25
      *APA00089, neue Regel Stückprovision
      *
      *
      *   Rev 2.1   Jan 20 2000 08:42:22   T13AE18
      *AAT0006
      *
      *   Rev 2.0   Feb 27 1999 11:57:28   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   06 Aug 1998 09:21:06   T13EX16
      *APA00032 - Umsetellung Konditionskonzept
      *
      *   Rev 1.0   18 Aug 1997 10:27:20   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0155  KONSTANTEN FÜR     KTY = 119       *
      *                 Konditions-Regel Grundprovision              *
      *                                                              *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!04.09.97! 01 ! NEUERSTELLUNG             ! AUERBACH *
      * APA00080!20.01.00!2.01! NEUE KNR-K-REGEL-...      ! BOHLINGER*
      * APA00089!23.03.00!2.02! REGEL STÜCKPROVISION NEU  ! JÄGER    *
      * APA00191!15.05.02!2.03! NEUE KNR-K-REGEL 5        ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *  KNR  REGEL                                                  *
      *  ----------------------------------                          *
      *    1  KEINE PROVISION                                        *
      *    2  VERGÜTUNG AUS BEITRAG                                  *
      *    3  VERGÜTUNG AUS SUMME                                    *
      *    4  STÜCKPROVISION                                         *
      *    5  VERGÜTUNG AUS SUMME MIT LAUFZEITABHÄNGIGER KOMPONENTE  *
      ****************************************************************

       01  FILLER.
           05 K-KNR-K-REGEL-GRUND-P-KEINE    PIC S9(004) BINARY VALUE 1.
           05 K-KNR-K-REGEL-GRUND-P-VG-BTG   PIC S9(004) BINARY VALUE 2.
200100     05 K-KNR-K-REGEL-GRUND-P-VG-SUM   PIC S9(004) BINARY VALUE 3.
230300     05 K-KNR-K-REGEL-GRUND-P-VG-STK   PIC S9(004) BINARY VALUE 4.
150502     05 K-KNR-K-REGEL-GRUND-P-SUM-LZ   PIC S9(004) BINARY VALUE 5.
