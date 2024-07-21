      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0063.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0063.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:57:54   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Dec 29 1995 15:42:22   T13AE26
      *AFVB0251: AL-Probe
      *
      *   Rev 1.0   Nov 04 1995 09:15:36   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: DRVB0063                                      * 00020000
      *    --------------------------------------------------------   * 00030000
      *    SCHNITTSTELLE UPVB0052  MAST-EINTRÄGE BEENDEN              * 00040000
      *    --------------------------------------------------------   * 00050000
      *    ERSTELLT: SCHÖLLHAMMER                                     * 00060000
      *    DATUM:    03.08.1994                                       * 00070000
      ***************************************************************** 00080000
      *    VERSION   2    10.05.95 17:19:57
      ****************************************************************  00100000
      * AENDERUNGEN:                                                 *  00110000
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00120000
      * -------------------------------------------------------------*  00130000
      * AFVB0117!03.08.94!    ! NEU                       ! SCHÖLLH. *  00140006
      *         !06.09.94! 01 ! PROD.UEBERGABE            ! SCHÖLLH. *  00141006
      * AFVB0160!07.05.95!    ! NUR NOCH EIN EVENT ZU     ! SCHÖLLH. *  00143000
      *         !        !    !   EINEM ZEITPUNKT SETZBAR !          *  00143100
      * AFVB0251!22.12.95! 02 ! EV6 NEU (WG. AL-PROBE)    ! SCHÖLLH. *  00144000
      *         !        !    !                           !          *  00145000
      *         !        !    !                           !          *  00145000
      *                                                              *  00146000
      * ENDE-LOGBUCH                                                 *  00147000
      ****************************************************************  00148000
      *                                                                 00149000
      *   FUNKTIONEN:                                                   00150000
      *   PI-VB52-FKT-MAST-BEEND     MAST-EINTRÄGE BEENDEN ABH. VON     00160000
      *                              EVENT                              00170000
      *                                                                 00171000
080595*   ES KANN NUR EIN EVENT GLEICHZEITIG GESETZT WERDEN.            00180000
      *                                                                 00200102
      *   ABH. VON PI-EVENTS SIND FOLGENDE PI-DATEN MITZUGEBEN:         00201000
      *                                                                 00204000
      *                 ! EV1 ! EV2 ! EV3 ! EV4 ! EV5 ! EV6 !           00240000
      *   --------------+-----+-----+-----+-----+-----+-----+---------- 00241000
      *   MA-ST-NR      !  X  !  X  !  X  !  X  !  X  !  X  !           00250000
      *   MS-NR-ALT     !  X  !     !  X  !     !     !  X  !           00260005
      *   MS-NR-NEU     !  X  !     !  X  !     !     !  X  !           00270005
      *   VBP-NR-ALT    !     !  X  !     !     !     !     !           00280000
      *   VBP-NR-NEU    !     !  X  !     !     !     !     !           00281000
      *   STD-DATEN     !  X  !  X  !  X  !  X  !  X  !  X  !           00281103
      *   ------------------------------------------------------------- 00282000
      *                                                                 00290000
       01  PI-UPVB0052.                                                 00300000
           05 PI-UPVB0052-FKT                PIC  X(002).               00310000
              88 PI-VB52-FKT-MAST-BEEND                   VALUE 'MB'.   00320000
                                                                        00321000
070595     05 PI-UPVB0052-EVENTS             PIC  X(001).                0033000
070595           88 PI-VB52-EV1-RUECKSTUF                 VALUE '1'.     0033020
070595           88 PI-VB52-EV2-VBP-AENDERUNG             VALUE '2'.     0033040
070595           88 PI-VB52-EV3-VBP-ENDE-HOCHSTUF         VALUE '3'.     0033060
070595           88 PI-VB52-EV4-DL-ENDE                   VALUE '4'.     0033080
070595           88 PI-VB52-EV5-MA-AUSTRITT               VALUE '5'.     0033100
221295           88 PI-VB52-EV6-HOCHSTUF                  VALUE '6'.     0033100
                                                                        00332000
070595*     05 PI-UPVB0052-EVENTS.                                       0033000
070595*        10 PI-UPVB0052-EV1             PIC  X(001).               0033010
070595*           88 PI-VB52-EV1-RUECKSTUF                 VALUE '1'.    0033020
070595*        10 PI-UPVB0052-EV2             PIC  X(001).               0033030
070595*           88 PI-VB52-EV2-VBP-AENDERUNG             VALUE '2'.    0033040
070595*        10 PI-UPVB0052-EV3             PIC  X(001).               0033050
070595*           88 PI-VB52-EV3-VBP-ENDE-HOCHSTUF         VALUE '3'.    0033060
070595*        10 PI-UPVB0052-EV4             PIC  X(001).               0033070
070595*           88 PI-VB52-EV4-DL-ENDE                   VALUE '4'.    0033080
070595*        10 PI-UPVB0052-EV5             PIC  X(001).               0033090
070595*           88 PI-VB52-EV5-MA-AUSTRITT               VALUE '5'.    0033100
070595*        10 FILLER                      PIC  X(005).               0033110
                                                                        00332000
           05 PI-UPVB0052-DATEN.                                        00333000
              10 PI-UPVB0052-MA-ST-NR        PIC S9(009)   COMP.        00340000
              10 PI-UPVB0052-MS-NR-ALT       PIC S9(004)   COMP.        00360000
              10 PI-UPVB0052-MS-NR-NEU       PIC S9(004)   COMP.        00361000
              10 PI-UPVB0052-VBP-NR-ALT      PIC S9(004)   COMP.        00362000
              10 PI-UPVB0052-VBP-NR-NEU      PIC S9(004)   COMP.        00363000
              10 FILLER                      PIC  X(040).               00370003
                                                                        00370103
           05 PI-UPVB0052-STD-DATEN.                                    00371003
              10 PI-UPVB0052-PROGRAMM-ID     PIC  X(008).               00380004
              10 PI-UPVB0052-PROGRAMM-KZ     PIC  X(001).               00380104
              10 PI-UPVB0052-USER-ID         PIC  X(007).               00381004
              10 PI-UPVB0052-TERMINAL-ID     PIC  X(004).               00390004
              10 PI-UPVB0052-DAT-UPDATE-ISO  PIC  X(010).               00391004
      *                                                                 00400000
       01  PO-UPVB0052.                                                 00410000
           05 PO-UPVB0052-RC                 PIC  X(001).               00420000
              88 PO-VB52-RC-KEIN-MAST-EINTRAG             VALUE '1'.    00430001
              88 PO-VB52-RC-MAST-EINTR-BEENDET            VALUE '2'.    00441001
      *                                                                 00442000
      ******** ENDE COPY-MEMBER ******************************          00443006
                                                                        00444000
