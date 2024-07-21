      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DPAT0045.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DPAT0045.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:56:56   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Mar 20 1997 16:56:28   T13EX16
      *DRIVER-ANPASSUNG DAISY
      *
      *   Rev 1.0   Nov 04 1995 08:18:52   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: DPAT0045       DIALOG-RAHMENSTEUERUNG         * 00020000
      *    (NEUE/GEÄNDERTE KOMPONENTEN - GEGENSTÜCK ZU DPAT0042)      * 00020000
      *    (WG. FLIESSENDER ANBINDUNG AN CTV AUS DPAT0001 AUSGELAGERT)* 00020000
      *    --------------------------------------------------------   * 00030000
      *    ERSTELLT: W. SCHÖLLHAMMER (INTEGRATA AG)                   * 00040000
      *    DATUM:    03.03.1995                                       * 00050000
      ***************************************************************** 00060000
      *    VERSION   1    02.05.95 16:37:15
      ****************************************************************  00062000
      * AENDERUNGEN:                                                 *  00063000
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00064000
      * -------------------------------------------------------------*  00065000
      * AFVB0182!15.03.95!    ! NEU                       ! SCHOELLH.*  00066000
      *         !28.04.95! 01 ! PROD.UEBERGABE            ! SCHÖLLH. *  00136000
      * AFPA0002!11.03.97! 02 ! KORREKTUR FEHLERMELDUNG   ! AUERBACH *  00070500
      *         !        !    !                           !          *  00070500
      *         !        !    !                           !          *  00070500
      *         !        !    !                           !          *  00070500
      *         !        !    !                           !          *  00070500
      *         !        !    !                           !          *  00070500
      *                                                              *  00070600
      * ENDE-LOGBUCH                                                 *  00070700
      ****************************************************************  00070800
      *                                                                 00070900
      *------------------------------------------------------------*    00760000
       UR5-INDEX-DZST-POSITIONIEREN SECTION.                            00770001
      *------------------------------------------------------------*    00780000
      * INPUT : S-DIALOG-ZUSTAND, T-DIALOG-ZUSTAND-TAB             *    00790001
      * OUTPUT: I-DZST                                             *    00810000
      * VERARBEITUNG:                                              *    00830000
      *   - INDEX AUF DIE ZEILE IN T-DIALOG-ZUSTAND-TAB POSITIO-   *    00840000
      *     NIEREN, DIE ZUM AKTUELLEN S-DIALOG-ZUSTAND GEHÖRT      *    00850000
      *------------------------------------------------------------*    00890000
       UR5-ANF.                                                         00910000
           MOVE 'UR5  ' TO ERR-ORT-SEC                                  00920000
           PERFORM ZR1-TRACE-KURZ                                       00930000
      *                                                                 00940000
           SET I-DZST                        TO 1                       01001005
           SEARCH T-DIALOG-ZUSTAND-ZEILE                                01001105
              VARYING I-DZST                                            01002005
              AT END                                                    01003000
                 MOVE K-27                   TO ERR-ORT-LFD             01003103
                 MOVE 'DIALOG-ZUSTAND NICHT IN T-DIALOG-ZUSTAN'         01003203
                                             TO ERR-VAR-ZEILE01(01:40)  01003303
                 MOVE 'D-TAB GEFUNDEN                         '         01003403
                                             TO ERR-VAR-ZEILE01(41:40)  01003503
110397           MOVE 'DIALOG-ZUSTAND: '     TO ERR-VAR-ZEILE02(01:16)  01003703
110397           MOVE S-DIALOG-ZUSTAND       TO ERR-VAR-ZEILE02(17:03)  01003803
                 PERFORM UR93-PROG-FEHLER                               01004003
              WHEN T-DIALOG-ZUSTAND(I-DZST) = S-DIALOG-ZUSTAND          01005000
                 CONTINUE                                               01006000
           END-SEARCH                                                   01007000
           .                                                            01010000
       UR5-EXIT.                                                        01020000
           EXIT.                                                        01030000
      *                                                                 21340000
      ******* ENDE COPY-MEMBER DPAT0045 (DIALOG-RAHMENSTEUERUNG) ****** 21350000
                                                                        21360000
