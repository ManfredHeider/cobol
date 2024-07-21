      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0156.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/MF/CPY/DRAT0156.CPv  $
      *
      *   Rev 2.3   Jun 05 2002 14:21:54   t13ex21
      *APA00191 - Neue Konditionsregel
      *
      *   Rev 2.2   Mar 23 2000 16:20:40   T13EX22
      *APA00067: PEG Anpassungen für Badenia
      *
      *   Rev 2.1   Jan 20 2000 08:42:24   T13AE18
      *AAT0006
      *
      *   Rev 2.0   Feb 27 1999 11:57:28   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   06 Aug 1998 09:21:48   T13EX16
      *APA00032 - Umstellung Konditionskonzept
      *
      *   Rev 1.0   18 Aug 1997 10:27:26   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0156  KONSTANTEN FÜR     KTY = 120       *
      *                 Konditions-Regel Gesamtprovision             *
      *                                                              *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!04.09.97! 01 ! NEUERSTELLUNG             ! AUERBACH *
      *         !28.10.97!    ! ANPASSUNG AN NAMENSKON-   ! MUND     *
      *         !        !    ! VENTIONEN                 !          *
      * APA00080!20.01.00!2.01! NEUE KNR-K-REGEL-...      ! BOHLINGER*
      * APA00064!15.03.00!2.02! NEUE KNR-K-REGEL-...      ! MUND     *
      * APA00191!15.05.02!2.03! NEUE KNR-K-REGEL 5        ! STORCH   *
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
      *                                                              *
      ****************************************************************

       01  FILLER.
      *    05 K-KNR-K-REGEL-GES-P-KEINE     PIC S9(004) BINARY VALUE 1.
281097     05 K-KNR-K-REGEL-GSP-KEINE       PIC S9(004) BINARY VALUE 1.
      *    05 K-KNR-K-REGEL-GES-P-VG-BTG    PIC S9(004) BINARY VALUE 2.
281097     05 K-KNR-K-REGEL-GSP-VG-BTG      PIC S9(004) BINARY VALUE 2.
200100     05 K-KNR-K-REGEL-GSP-VG-SUM      PIC S9(004) BINARY VALUE 3.
150300     05 K-KNR-K-REGEL-GSP-VG-STCK     PIC S9(004) BINARY VALUE 4.
150502     05 K-KNR-K-REGEL-GSP-VG-SUM-LZ   PIC S9(004) BINARY VALUE 5.
