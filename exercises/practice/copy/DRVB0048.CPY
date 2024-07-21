      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0048.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0048.CPv  $
      *
      *   Rev 2.1   Jan 22 2018 12:35:58   t13ae13
      *ASL00573: VSW 2017 (Abwicklung Versorgungsentgelte 2017) - U
      *PVB0093, UPVB0043, UPVB0117 (swl)
      *
      *   Rev 2.0   Feb 27 1999 15:57:52   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:12:38   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: DRVB0048                                      * 00020000
      *    --------------------------------------------------------   * 00030000
      *    SCHNITTSTELLE UPVB0043 ERM. GESAMTEINH. EIGEN UND GRUPPE   * 00040000
      *    --------------------------------------------------------   * 00050000
      *    ERSTELLT: SCHÖLLHAMMER                                     * 00060000
      *    DATUM:    11.11.1993 (11:11 UHR)                           * 00070000
      ***************************************************************** 00080000
      *    VERSION   02   20.12.93                                   *  00090002
      ****************************************************************  00100000
      * AENDERUNGEN:                                                 *  00110000
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00120000
      * -------------------------------------------------------------*  00130000
      * AFVB0037!25.11.93! 01 ! NEU                       ! SCHÖLLH. *  00140000
      * FEVB0045!15.12.93!    ! PI-WERTE GEÄND. (OHNE TAG)! SCHÖLLH. *  00141002
      *         !20.12.93! 02 ! PROD.ÜBERGABE             ! SCHÖLLH. *  00142002
      * ASL00573!10.01.18!2.1 ! neue Funktion             ! S. Walb  *  00150000
      *         !        !    !                           !          *  00160000
      *         !        !    !                           !          *  00170000
      *                                                              *  00180000
      * ENDE-LOGBUCH                                                 *  00190000
      ****************************************************************  00200000
      *                                                                 00210000
      *   FUNKTIONEN:                                                   00220000
      *   UPVB43-FKT-EH-EIG-GRU-ZEITR  ERMITTL. NETTO-GESAMTEINHEITEN   00230000
      *                                EIGEN UND GRUPPE UND FOLGEGE-    00240000
      *                                SCHÄFT EIGEN UND GRUPPE,         00240100
      *                                VON MONAT-VON BIS MONAT-BIS      00241000
      *                                                                 00260000
      *   PI-UPVB0043-ST-NR          STELLE FÜR DIE ERMITTELT WIRD      00270000
151293*   PI-UPVB0043-DAT-VON-...    ERSTER MONAT DES ZEITRAUMS (INCL.) 00280000
151293*   PI-UPVB0043-DAT-BIS-...    LETZTER MONAT DES ZEITRAUMS (INCL.)00300000
      *                                                                 00310000
      *   PO-UPVB0043-N-EH-EIG       SUMMIERTER NETTO-EIGENUMSATZ       00320000
      *   PO-UPVB0043-N-EH-GRU       SUMMIERTER NETTO-GRUPPENUMSATZ     00330000
      *   PO-UPVB0043-FG-N-EH-EIG    SUMMIERTES NETTO-FOLGEGESCH-EIGEN  00331000
      *   PO-UPVB0043-FG-N-EH-GRU    SUMMIERTER NETTO-FOLGEGESCH-GRUPPE 00332000
      *                                                                 00340000
       01  PI-UPVB0043.                                                 00350000
           05 PI-UPVB0043-FKT                PIC  X(004).               00360000
               88  UPVB43-FKT-EH-EIG-GRU-ZEITR            VALUE 'EGZR'. 00370000
100118         88  UPVB43-FKT-EH-EG-GR-UST-FR             VALUE 'USTF'. 00370000
           05 PI-UPVB0043-ST-NR              PIC S9(009)   COMP.        00380000
151293*     05 PI-UPVB0043-MONAT-VON-ISO      PIC  X(010).              00390000
151293*     05 PI-UPVB0043-MONAT-BIS-ISO      PIC  X(010).              00400000
151293     05 PI-UPVB0043-DAT-VON-JAHR       PIC  9(004).               00401001
151293     05 PI-UPVB0043-DAT-VON-MONAT      PIC  9(002).               00401101
151293     05 PI-UPVB0043-DAT-BIS-JAHR       PIC  9(004).               00401201
151293     05 PI-UPVB0043-DAT-BIS-MONAT      PIC  9(002).               00401301
      *                                                                 00410000
       01  PO-UPVB0043.                                                 00420000
           05  PO-UPVB0043-N-EH-EIG          PIC S9(009)V9 COMP-3.      00430000
           05  PO-UPVB0043-N-EH-GRU          PIC S9(009)V9 COMP-3.      00440000
           05  PO-UPVB0043-FG-N-EH-EIG       PIC S9(009)V9 COMP-3.      00441000
           05  PO-UPVB0043-FG-N-EH-GRU       PIC S9(009)V9 COMP-3.      00442000
      *                                                                 00450000
      ******** ENDE COPY-MEMBER (DRVB0048) *******************          00460000
                                                                        00470000

