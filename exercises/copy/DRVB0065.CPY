      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0065.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0065.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:57:56   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:16:06   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************  00010000
      *    COPY-MEMBER: DRVB0065                                     *  00020000
      *    --------------------------------------------------------  *  00030000
      *    SCHNITTSTELLE UPVB0054 HOLEN VERSCH. DATUM-ANGABEN ZUR    *  00040000
      *                           ZULETZT EINGESPIELTEN VB-STATISTIK *  00051000
      *    --------------------------------------------------------  *  00052400
      *    ERSTELLT: WOLFF                                           *  00053000
      *    DATUM:    06.10.1994                                      *  00054002
      ****************************************************************  00056000
      *    VERSION   1    29.10.94 10:56:44
      ****************************************************************  00056200
      * AENDERUNGEN:                                                 *  00056300
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00056400
      * -------------------------------------------------------------*  00056500
      * AFVB0142!06.10.94! 01 ! NEU                       ! WOLFF    *  00056603
      *         !        !    !                           !          *  00056700
      *         !        !    !                           !          *  00056800
      *         !        !    !                           !          *  00056900
      *         !        !    !                           !          *  00057000
      *                                                              *  00057100
      * ENDE-LOGBUCH                                                 *  00057200
      ****************************************************************  00057300
      *                                                                 00057400
      *   DATUM-ANGABEN ZU VB-STATISTIK:                                00058000
      *     - JÜNGSTES STATISTIK-DATUM                                  00059000
      *     - STATISTIK-DATUM VOR 2 MONATEN  (FÜR 3-MONATS-ZEITRÄUME)   00059100
      *     - STATISTIK-DATUM VOR 11 MONATEN (FÜR 12-MONATS-ZEITRÄUME)  00059200
      *     - STATISTIK-DATUM NACH 1 MONAT                              00059301
      *     - STATISTIK-DATUM VOR 12 MONATEN (WIRD ANGEZEIGT)           00059404
      *                                                                 00060000
       01  PO-UPVB0054.                                                 00080000
           05 PO-UPVB0054-VB-STAT-DATUM-ISO     PIC X(10).              00101004
           05 PO-UPVB0054-VB-STAT-2MON-ISO      PIC X(10).              00101104
           05 PO-UPVB0054-VB-STAT-11MON-ISO     PIC X(10).              00102004
           05 PO-UPVB0054-VB-STAT-1MON-ISO      PIC X(10).              00103004
           05 PO-UPVB0054-VB-STAT-12MON-ISO     PIC X(10).              00104005
      *                                                                 00180000
      ******** ENDE COPY-MEMBER (DRVB0065) *******************          00190000
                                                                        01850000
