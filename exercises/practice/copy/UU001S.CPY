      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   UU001S.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/UU001S.CPv  $
      *
      *   Rev 2.1   Jul 12 2002 10:08:26   t13ae34
      *AAT00023 - KS Host-Kompetenzen verbessern
      *
      *   Rev 2.0   Feb 27 1999 15:15:06   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:44:02   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: TRANSAKTIONS-DATEI (UU001S)                   * 00020000
      *    --------------------------------------------------------   * 00030000
      *    ERSTELLT: H. SCHORN /AGS                                   * 00040000
      *    DATUM:    06.12.1979                                       * 00050000
      ***************************************************************** 00060000
      *    VERSION   4    10.07.95 17:18:42
      ****************************************************************  00062000
      * AENDERUNGEN:                                                 *  00063000
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00064000
      * -------------------------------------------------------------*  00065000
      *         !06.12.79! 00 ! NEU                       ! SCHORN   *  00066000
      *         !25.03.93! 01 ! NEU: K1-NAW-KZ, K1-MENUE, ! LEISTE   *  00068000
      *         !        !    !  K1-TRANS-TYP, K1-PGNAME  !          *  00068000
      *         !        !    ! RESERVE VERGRÖSSERT       !          *  00068000
      * AFVB0182!15.03.95!    ! NEU: K1-CTV-BRIEFMENUE    ! SCHÖLLH. *  00068000
      *         !28.04.95! 02 ! PROD.UEBERGABE            ! SCHÖLLH. *  00136000
      * AFRZ0013!10.07.95! 03 ! NEU: K1-BILDNR-67,        ! WIECHERS *  00091200
      *         !        !    ! K1-NEWS AUF 34 STELLEN    !          *  00092000
      *         !        !    ! VERGRÖSSERT               !          *  00092000
      * AAT00023!12.07.02!2.1 ! K+S HOST-KOMPETENZEN      ! HEIDER   *  00092000
      *         !        !    !                           !          *  00092000
      *                                                              *  00093000
      * ENDE-LOGBUCH                                                 *  00094000
      ****************************************************************  00095000
      *
      *    ----------------------------------------------------------
      *    LÄNGE:     200 BYTE
      *    SCHLÜSSEL: K1-TRANS
      *    ----------------------------------------------------------
      *
CCC009*    SKIP1
       01  UU001S.
           10  K1-SA                       PIC 99.
           10  K1-KEY.
               20  K1-TRANS                PIC 9(6).
      *
      *    BESTANDS-SATZ
      *
           10  K1-SATZ.
               20  K1-TEXT                 PIC X(45).
               20  K1-STATUS               PIC X.
               20  K1-DATEI        OCCURS 50.
                   30  K1-DASCHL           PIC X.
950710*        20  K1-BILDNR               PIC X(5).
950710         20  K1-BILDNR-15            PIC X(5).
               20  K1-TEXTNR               PIC 999.
250393         20  K1-NAW-KZ               PIC X.
250393         20  K1-MENUE                PIC X(4).
250393         20  K1-TRANS-TYP            PIC X.
250393         20  K1-PGNAME               PIC X(8).
150395         20  K1-CTV-BRIEFMENUE       PIC X(16).
950710         20  K1-BILDNR-67            PIC X(02).
120702*            WERTEBEREICH KTY=205
120702         20  K1-SYSTEM-SUBSYSTEM     PIC 9(04)  COMP-3.
120702         20  K1-SYSTEM-SUBSYSTEM-X REDEFINES
120702             K1-SYSTEM-SUBSYSTEM     PIC X(03).
120702*        20  K1-RESERVE              PIC X(56).
120702         20  K1-RESERVE              PIC X(53).
950710*         20  K1-RESERVE              PIC X(58).
150395*         20  K1-RESERVE              PIC X(74).
      *
      *    INFORMATIONS-SATZ
      *
           10  K1-INFO  REDEFINES  K1-SATZ.
950710         20  K1-NEWS                 PIC X(34).
950710*        20  K1-NEWS                 PIC X(28).
      *
      *    DUMMY-SATZ
      *
           10  K1-DUMMY  REDEFINES  K1-SATZ.
               20  K1-ANZAHL               PIC 9(7)    COMP-3.
               20  K1-LADEDAT              PIC 9(7)    COMP-3.
               20  K1-ELADEDAT             PIC 9(7)    COMP-3.

