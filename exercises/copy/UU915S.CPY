      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   UU915S.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/UU915S.CPv  $
      *
      *   Rev 2.3   Aug 23 2012 11:23:20   t13ae13
      *AVB01237: (Wieder-)Einführung Stufe VM01 (mit 10%Diff.Prov.)
      * - UU915S und Programme
      *
      *   Rev 2.2   Feb 29 2012 15:50:06   t13ex57
      *AVB01206: Umbenennung BD - UBO
      *
      *   Rev 2.1   Mar 05 2003 10:08:54   t13ae13
      *AVB00648: Dim 7ukunft - Neue Stufen 
      *
      *   Rev 2.0   Feb 27 1999 15:15:30   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 10:06:22   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****  VERSION   1    29.05.95 13:08:14
      ******************************************************
      *                                                    *
      *            COPY-MEMBER PROVISIONS-SAETZE           *
      *                                                    *
      * GEAENDERT 06.10.86/BIEHL:  GS1 ==> GS ,GS2 ==> HGS *
      * GEAENDERT 07.10.86/WIECHERS GS ==> GS1,HGS ==> GS2 *
      * GEAENDERT 13.10.86/BIEHL:   GS1 ==> GS,GS2 ==> HGS *
      * GEAENDERT 10.03.88/DOBSLAFF EINFUEHRUNG RGS (272)  *
      * GEAENDERT 19.03.91/NAUMANN:                        *
      * MIT DER EINFUEHRUNG RANGFREI, D.H.:                *
      * STUFE RF-T M. STUF-NR. = 888,                      *
      * STUFE RF-R M. STUF-NR. = 889,                      *
      * STUFE RF-A M. STUF-NR. = 890                       *
      * VON OCC. 34 AUF OCC. 37 ERWEITERT.                 *
      * GEAENDERT 03.04.95/GIESE                           *
      * EINFÜHRUNG EINER NEUEN STUFE                       *
      * STUFE BD   M. STUF-NR. = 591,                      *
      * VON OCC. 37 AUF OCC. 38 ERWEITERT.                 *
      ******************************************************
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00648!10.02.03!2.1 ! Neue Stufen VBAS und ALP  ! Walb     *
      * AVB01206!29.02.12!2.2 ! Umbenennung BD => PD      ! Boll     *
      * AVB01237!02.08.12!2.3 ! VM01 mit 10%              ! Walb     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  UU915S.
           05  PROV.
               10  FILLER          PIC 999     COMP-3  VALUE 001.
               10  FILLER          PIC X(4)            VALUE 'VM01'.
020812*        10  FILLER          PIC 9(4)    COMP-3  VALUE 0300.
020812         10  FILLER          PIC 9(4)    COMP-3  VALUE 0100.
               10  FILLER          PIC 999     COMP-3  VALUE 002.
               10  FILLER          PIC X(4)            VALUE 'VM02'.
               10  FILLER          PIC 9(4)    COMP-3  VALUE 0200.
               10  FILLER          PIC 999     COMP-3  VALUE 003.
               10  FILLER          PIC X(4)            VALUE 'VM03'.
               10  FILLER          PIC 9(4)    COMP-3  VALUE 0300.
               10  FILLER          PIC 999     COMP-3  VALUE 004.
               10  FILLER          PIC X(4)            VALUE 'VM04'.
               10  FILLER          PIC 9(4)    COMP-3  VALUE 0400.
               10  FILLER          PIC 999     COMP-3  VALUE 009.
               10  FILLER          PIC X(4)            VALUE 'VM09'.
               10  FILLER          PIC 9(4)    COMP-3  VALUE 0300.
               10  FILLER          PIC 999     COMP-3  VALUE 010.
               10  FILLER          PIC X(4)            VALUE 'VBN '.
               10  FILLER          PIC 9(4)    COMP-3  VALUE 0600.
100203         10  FILLER          PIC 999     COMP-3  VALUE 013.
100203         10  FILLER          PIC X(4)            VALUE 'VBAS'.
100203         10  FILLER          PIC 9(4)    COMP-3  VALUE 0400.
               10  FILLER          PIC 999     COMP-3  VALUE 016.
               10  FILLER          PIC X(4)            VALUE 'VBA1'.
               10  FILLER          PIC 9(4)    COMP-3  VALUE 0600.
               10  FILLER          PIC 999     COMP-3  VALUE 011.
               10  FILLER          PIC X(4)            VALUE 'VBN+'.
               10  FILLER          PIC 9(4)    COMP-3  VALUE 0800.
               10  FILLER          PIC 999     COMP-3  VALUE 018.
               10  FILLER          PIC X(4)            VALUE 'VBA2'.
               10  FILLER          PIC 9(4)    COMP-3  VALUE 0800.
               10  FILLER          PIC 999     COMP-3  VALUE 121.
               10  FILLER          PIC X(4)            VALUE 'VBH1'.
               10  FILLER          PIC 9(4)    COMP-3  VALUE 0800.
               10  FILLER          PIC 999     COMP-3  VALUE 019.
               10  FILLER          PIC X(4)            VALUE 'VBA3'.
               10  FILLER          PIC 9(4)    COMP-3  VALUE 0900.
100203         10  FILLER          PIC 999     COMP-3  VALUE 149.
100203         10  FILLER          PIC X(4)            VALUE 'ALP '.
100203         10  FILLER          PIC 9(4)    COMP-3  VALUE 0800.
               10  FILLER          PIC 999     COMP-3  VALUE 161.
               10  FILLER          PIC X(4)            VALUE 'AL  '.
               10  FILLER          PIC 9(4)    COMP-3  VALUE 0900.
               10  FILLER          PIC 999     COMP-3  VALUE 222.
               10  FILLER          PIC X(4)            VALUE 'VBH2'.
               10  FILLER          PIC 9(4)    COMP-3  VALUE 1000.
               10  FILLER          PIC 999     COMP-3  VALUE 252.
               10  FILLER          PIC X(4)            VALUE 'GAA '.
               10  FILLER          PIC 9(4)    COMP-3  VALUE 1000.
               10  FILLER          PIC 999     COMP-3  VALUE 262.
               10  FILLER          PIC X(4)            VALUE 'GA  '.
               10  FILLER          PIC 9(4)    COMP-3  VALUE 1000.
               10  FILLER          PIC 999     COMP-3  VALUE 272.
               10  FILLER          PIC X(4)            VALUE 'RGS '.
               10  FILLER          PIC 9(4)    COMP-3  VALUE 1000.
               10  FILLER          PIC 999     COMP-3  VALUE 323.
               10  FILLER          PIC X(4)            VALUE 'VBH3'.
               10  FILLER          PIC 9(4)    COMP-3  VALUE 1100.
               10  FILLER          PIC 999     COMP-3  VALUE 353.
               10  FILLER          PIC X(4)            VALUE 'GS1A'.
               10  FILLER          PIC 9(4)    COMP-3  VALUE 1100.
               10  FILLER          PIC 999     COMP-3  VALUE 363.
               10  FILLER          PIC X(4)            VALUE 'GS  '.
               10  FILLER          PIC 9(4)    COMP-3  VALUE 1100.
               10  FILLER          PIC 999     COMP-3  VALUE 424.
               10  FILLER          PIC X(4)            VALUE 'VBH4'.
               10  FILLER          PIC 9(4)    COMP-3  VALUE 1200.
               10  FILLER          PIC 999     COMP-3  VALUE 454.
               10  FILLER          PIC X(4)            VALUE 'GS2A'.
               10  FILLER          PIC 9(4)    COMP-3  VALUE 1200.
               10  FILLER          PIC 999     COMP-3  VALUE 464.
               10  FILLER          PIC X(4)            VALUE 'HGS '.
               10  FILLER          PIC 9(4)    COMP-3  VALUE 1200.
               10  FILLER          PIC 999     COMP-3  VALUE 465.
               10  FILLER          PIC X(4)            VALUE 'RD1 '.
               10  FILLER          PIC 9(4)    COMP-3  VALUE 1260.
               10  FILLER          PIC 999     COMP-3  VALUE 525.
               10  FILLER          PIC X(4)            VALUE 'VBH5'.
               10  FILLER          PIC 9(4)    COMP-3  VALUE 1320.
               10  FILLER          PIC 999     COMP-3  VALUE 555.
               10  FILLER          PIC X(4)            VALUE 'RDA '.
               10  FILLER          PIC 9(4)    COMP-3  VALUE 1320.
      ******************************************************
               10  FILLER          PIC 999     COMP-3  VALUE 566.
               10  FILLER          PIC X(4)            VALUE 'RD2 '.
               10  FILLER          PIC 9(4)    COMP-3  VALUE 1320.
      ******************************************************
030495         10  FILLER          PIC 999     COMP-3  VALUE 591.
290212*        10  FILLER          PIC X(4)            VALUE 'BD  '.
290212         10  FILLER          PIC X(4)            VALUE 'PD  '.
030495         10  FILLER          PIC 9(4)    COMP-3  VALUE 1360.
      ******************************************************
               10  FILLER          PIC 999     COMP-3  VALUE 626.
               10  FILLER          PIC X(4)            VALUE 'VBH6'.
               10  FILLER          PIC 9(4)    COMP-3  VALUE 1400.
      ******************************************************
               10  FILLER          PIC 999     COMP-3  VALUE 656.
               10  FILLER          PIC X(4)            VALUE 'DA  '.
               10  FILLER          PIC 9(4)    COMP-3  VALUE 1400.
               10  FILLER          PIC 999     COMP-3  VALUE 667.
               10  FILLER          PIC X(4)            VALUE 'D   '.
               10  FILLER          PIC 9(4)    COMP-3  VALUE 1400.
               10  FILLER          PIC 999     COMP-3  VALUE 757.
               10  FILLER          PIC X(4)            VALUE 'EDA '.
               10  FILLER          PIC 9(4)    COMP-3  VALUE 0060.
               10  FILLER          PIC 999     COMP-3  VALUE 768.
               10  FILLER          PIC X(4)            VALUE 'ED  '.
               10  FILLER          PIC 9(4)    COMP-3  VALUE 0060.
               10  FILLER          PIC 999     COMP-3  VALUE 858.
               10  FILLER          PIC X(4)            VALUE 'LD  '.
               10  FILLER          PIC 9(4)    COMP-3  VALUE 1480.
               10  FILLER          PIC 999     COMP-3  VALUE 888.
               10  FILLER          PIC X(4)            VALUE 'RF-T'.
               10  FILLER          PIC 9(4)    COMP-3  VALUE 0.
               10  FILLER          PIC 999     COMP-3  VALUE 889.
               10  FILLER          PIC X(4)            VALUE 'RF-R'.
               10  FILLER          PIC 9(4)    COMP-3  VALUE 0.
               10  FILLER          PIC 999     COMP-3  VALUE 890.
               10  FILLER          PIC X(4)            VALUE 'RF-A'.
               10  FILLER          PIC 9(4)    COMP-3  VALUE 0.
               10  FILLER          PIC 999     COMP-3  VALUE 999.
               10  FILLER          PIC X(4)            VALUE 'CPD '.
               10  FILLER          PIC 9(4)    COMP-3  VALUE 0.
               10  FILLER          PIC 999     COMP-3  VALUE 0.
               10  FILLER          PIC X(4)            VALUE 'ENDE'.
               10  FILLER          PIC 9(4)    COMP-3  VALUE 0.
030495*    05  PROV1       REDEFINES   PROV    OCCURS 37.
100203*030495    05  PROV1       REDEFINES   PROV    OCCURS 38.
100203     05  PROV1       REDEFINES   PROV    OCCURS 40.
               10  PSTUFE          PIC 999     COMP-3.
               10  PTEXT           PIC X(4).
               10  PPROVS          PIC 999V9   COMP-3.

