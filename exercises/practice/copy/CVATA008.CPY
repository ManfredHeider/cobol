      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVATA008.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CVATA008.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:56:46   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 12:04:38   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: CVATA008                                      * 00020002
      *    --------------------------------------------------------   * 00030000
      *    DCLGEN VON VIATA008 (KNR)                                  * 00040002
      *    --------------------------------------------------------   * 00050000
      *    ERSTELLT: BÖFF                                             * 00060002
      *    DATUM:    04.11.1994                                       * 00070002
      ***************************************************************** 00080000
      *    VERSION   1    10.01.95 16:16:23
      ****************************************************************  00100000
      * AENDERUNGEN:                                                 *  00110000
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00120000
      * -------------------------------------------------------------*  00130000
      *         !        !    ! NEU                       ! KUHN     *  00140002
      * AFVB0142!04.11.94!    ! ERWEITERUNG UM FELD       ! BÖFF     *  00140002
      *         !        !    ! KNR-BEZ10                 !          *  00143000
      *         !10.01.95! 01 ! PRODUKTIONSÜBERGABE       ! GIESE    *  00143100
      *         !        !    !                           !          *  00144000
      *         !        !    !                           !          *  00143100
      *         !        !    !                           !          *  00144000
      *         !        !    !                           !          *  00145000
      *                                                              *  00146000
      * ENDE-LOGBUCH                                                 *  00147000
      ****************************************************************  00148000
      ******************************************************************
      * DCLGEN TABLE(VIATA008)                                         *
      *        ACTION(REPLACE)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIATA008 TABLE
CCC004*    ( KTY_NR                         SMALLINT NOT NULL,
CCC004*      KNR_NR                         SMALLINT NOT NULL,
CCC004*      KNR_LAST_UPDATE                TIMESTAMP NOT NULL,
CCC004*      KNR_BEZ                        CHAR(80) NOT NULL,
CCC004*      KNR_BEZ_KURZ                   CHAR(4) NOT NULL,
CCC004*      KNR_HINDERNIS                  CHAR(1) NOT NULL,
CCC004*      KNR_BEZ10                      CHAR(10) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIATA008                           *
      ******************************************************************
       01  VIATA008-GESAMT.
        05  NI-VIATA008-TAB.
           10  NI-VIATA008-ELE     PIC S9(04)  COMP   OCCURS  7  TIMES.
        05  NI-VIATA008            REDEFINES   NI-VIATA008-TAB.
           10 NI-KTY-NR            PIC S9(04)  COMP.
           10 NI-KNR-NR            PIC S9(04)  COMP.
           10 NI-KNR-LAST-UPDATE   PIC S9(04)  COMP.
           10 NI-KNR-BEZ           PIC S9(04)  COMP.
           10 NI-KNR-BEZ-KURZ      PIC S9(04)  COMP.
           10 NI-KNR-HINDERNIS     PIC S9(04)  COMP.
           10 NI-KNR-BEZ10         PIC S9(04)  COMP.
        05  DCL-VIATA008.
           10 KTY-NR               PIC S9(4) USAGE COMP.
           10 KNR-NR               PIC S9(4) USAGE COMP.
           10 KNR-LAST-UPDATE      PIC X(26).
           10 KNR-BEZ              PIC X(80).
           10 KNR-BEZ-KURZ         PIC X(4).
           10 KNR-HINDERNIS        PIC X(1).
           10 KNR-BEZ10            PIC X(10).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 7       *
      ******************************************************************
