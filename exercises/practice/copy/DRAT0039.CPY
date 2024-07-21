      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0039.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRAT0039.CPv  $
      *
      *   Rev 2.4   Dec 07 2004 16:37:06   t13ex43
      *AVD00001: PGM - VBI-SST : Entladen MS + MSBZ
      *
      *   Rev 2.3   Nov 01 2004 13:07:28   t13ex43
      *AVD00001: PGM - VBI-SST : Entladen MS + MSBZ
      *
      *   Rev 2.2   Sep 20 2004 10:01:00   t13ex43
      *AVD00001: PGM - VBI-SST : Entladen MS + MSBZ
      *
      *   Rev 2.1   Feb 13 2003 15:36:06   t13ae13
      *AVB00648: Dim 7ukunft - Phase 2
      *
      *   Rev 2.0   Feb 27 1999 11:57:08   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 08:41:22   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: KNR-KONSTANTEN FUER STUFE (DRAT0039)          *
      *    --------------------------------------------------------   *
      *    ERSTELLT: GIESE                                            *
      *    DATUM:    23.04.1993                                       *
      *****************************************************************
      *    VERSION   1    20.04.95 11:40:25
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !23.04.93! 01 ! NEU                       ! GIESE    *
      *         !10.08.93! 02 ! K-STF-VM01 VALUE 2 GEÄND. !          *
      *         !        !    !        AUF VALUE 1        ! NAUMANN  *
      * AFVB0203!04.04.95!    ! NEU: K-STF-BD             ! SCHÖLLH. *
      *         !20.04.95! 03 ! PROD.ÜBERGABE             ! SCHÖLLH. *
      * AVB00648!10.02.03!2.1 ! NEU: K-STF-VBAS,K-STF-ALP ! Walb     *
170904* AVD00001!17.09.04!2.2 ! VBI-STUFEN HINZU (TEIL)   ! KLEEMEIER*
011104* AVD00001!01.11.04!2.3 ! AENDERUNG VBI-STUFENNRN.  ! KLEEMEIER*
071204* AVD00001!07.12.04!2.4 ! AENDERUNGEN VM09 + AL     ! KLEEMEIER*
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************

      *
      *   AUSPRÄGUNGEN STUFE
      *
       01  K-STF-VM01               PIC S9(04) COMP VALUE 1.
       01  K-STF-VM02               PIC S9(04) COMP VALUE 2.
       01  K-STF-VM03               PIC S9(04) COMP VALUE 3.
       01  K-STF-VM04               PIC S9(04) COMP VALUE 4.
100203 01  K-STF-VBAS               PIC S9(04) COMP VALUE 13.
       01  K-STF-VBA                PIC S9(04) COMP VALUE 16.
100203 01  K-STF-ALP                PIC S9(04) COMP VALUE 149.
       01  K-STF-AL                 PIC S9(04) COMP VALUE 161.
       01  K-STF-GA                 PIC S9(04) COMP VALUE 262.
       01  K-STF-RGS                PIC S9(04) COMP VALUE 272.
       01  K-STF-GS                 PIC S9(04) COMP VALUE 363.
       01  K-STF-HGS                PIC S9(04) COMP VALUE 464.
       01  K-STF-RD1                PIC S9(04) COMP VALUE 465.
       01  K-STF-RD2                PIC S9(04) COMP VALUE 566.
040495 01  K-STF-BD                 PIC S9(04) COMP VALUE 591.
       01  K-STF-D                  PIC S9(04) COMP VALUE 667.
       01  K-STF-LD                 PIC S9(04) COMP VALUE 858.
      *
      *   AUSPRÄGUNGEN VBI-STUFE
      *
170904*01  K-STF-VBI-VM09           PIC S9(09) COMP VALUE 900000.
170904*01  K-STF-VBI-VBA3           PIC S9(09) COMP VALUE 1600000.
170904*01  K-STF-VBI-VBA2           PIC S9(09) COMP VALUE 1600001.
170904*01  K-STF-VBI-VBA1           PIC S9(09) COMP VALUE 1600002.
170904*01  K-STF-VBI-VBAK           PIC S9(09) COMP VALUE 1600003.
170904*01  K-STF-VBI-AL-D           PIC S9(09) COMP VALUE 16100000.
170904*01  K-STF-VBI-AL-K           PIC S9(09) COMP VALUE 16100001.
170904*01  K-STF-VBI-AL-P           PIC S9(09) COMP VALUE 16100002.
170904*01  K-STF-VBI-ALD            PIC S9(09) COMP VALUE 16100003.
170904*01  K-STF-VBI-AL             PIC S9(09) COMP VALUE 16100004.
      *
      *   NEUE VBI-STUFEN
      *
071204 01  K-STF-VBI-VM09           PIC S9(09) COMP VALUE    90000.
011104 01  K-STF-VBI-VBAK           PIC S9(09) COMP VALUE  1600000.
011104 01  K-STF-VBI-VBA1           PIC S9(09) COMP VALUE  1600001.
011104 01  K-STF-VBI-VBA2           PIC S9(09) COMP VALUE  1600002.
011104 01  K-STF-VBI-VBA3           PIC S9(09) COMP VALUE  1600003.
071204 01  K-STF-VBI-AL-P           PIC S9(09) COMP VALUE 16100000.
071204 01  K-STF-VBI-AL-K           PIC S9(09) COMP VALUE 16100001.
071204 01  K-STF-VBI-AL-D           PIC S9(09) COMP VALUE 16100002.
071204 01  K-STF-VBI-AL             PIC S9(09) COMP VALUE 16100003.
071204 01  K-STF-VBI-ALD            PIC S9(09) COMP VALUE 16100004.
      *
      ******** ENDE COPY-MEMBER KNR-KONSTANTEN STUFE (DRAT0039)********
      *
