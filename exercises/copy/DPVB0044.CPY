      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DPVB0044.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DPVB0044.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:57:34   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 08:29:44   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: DPVB0044                                      * 00020000
      *    DIESE SECTION ORDNET DIE DATENFELDER DER BV ZWISCHENFEL-   * 00021000
      *    DERN ZU.                                                   * 00021100
      *    --------------------------------------------------------   * 00021200
      *    ERSTELLT: H. LEISTE        (DVAG)                          * 00021500
      *    DATUM:    19.05.1993                                       * 00021600
      ***************************************************************** 00021800
      *    VERSION   03   01.10.93                                   *  00090101
      ***************************************************************** 00021800
      * AENDERUNGEN:                                                 *  00021900
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00022000
      * -------------------------------------------------------------*  00022100
      *         !19.05.93! 01 ! NEU                       ! LEISTE   *  00022200
      *         !29.07.93! 02 ! S. TESTBERICHT TEST STUFE ! NAUMANN  *  00022300
      *         !        !    ! I, 1. TEST, FEHL-NR 2/1   !          *  00022500
      *         !30.09.93!    ! BERÜCKSICHTIGUNG VON UNGÜLTIGER BANK-*  00022300
      *         !        !    ! VERBINDUNG                ! NAUMANN  *  00022300
      *         !01.10.93! 03 ! PROD.-UEBERGABE           ! NAUMANN  *  00095002
      *         !        !    !                           !          *  00022600
      *         !        !    !                           !          *  00022700
      *         !        !    !                           !          *  00022800
      *         !        !    !                           !          *  00022900
      *                                                              *  00023000
      * ENDE-LOGBUCH                                                 *  00023100
      ****************************************************************  00023200
      *----------------------------------------------------------
       BV-ZUORDNEN SECTION.
      *----------------------------------------------------------
      *----------------------------------------------------------       00023500
      *-INPUT : DCL-TBVB0021                                            00023600
      *-OUTPUT: Z-INSTITUT                                              00023700
      *-        Z-BLZ                                                   00023800
      *-        Z-KTONR                                                 00023900
      *-        Z-KTOINH                                                00024000
      *-        Z-SPERRE                                                00024100
      *----------------------------------------------------------       00025000
       BV-ZUORDNEN-ANF.
           MOVE BV-BANKNAME             TO   Z-INSTITUT
           MOVE BV-KTO-INHABER          TO   Z-KTOINH

290793     MOVE ZERO                    TO   Z-SPACE
290793     INSPECT BV-BLZ TALLYING Z-SPACE
290793             FOR CHARACTERS BEFORE SPACE
290793     IF Z-SPACE  =  ZERO
290793        MOVE ZERO                 TO   Z-BLZ
290793     ELSE
290793        MOVE BV-BLZ(1:Z-SPACE)    TO   Z-BLZ
290793     END-IF

290793     MOVE ZERO                    TO   Z-SPACE
290793     INSPECT BV-KTO-NR TALLYING Z-SPACE
290793             FOR CHARACTERS BEFORE SPACE
290793     IF Z-SPACE  =  ZERO
290793        MOVE ZERO                 TO   Z-KTONR
290793     ELSE
290793        MOVE BV-KTO-NR(1:Z-SPACE) TO   Z-KTONR
290793     END-IF

29079M     IF   KNR-SPERRE       = K-KNR-KEINE-SPERRE OR
290793          KNR-SPERRE       = K-KNR-KEINE-SPERRE-BV-UNG
290793          MOVE ZERO               TO   Z-SPERRE
290793     ELSE
290793          MOVE 1                  TO   Z-SPERRE
290793     END-IF

300993*    BEI UNGÜLTIGER BANKVERBINDUNG WIRD WIE FOLGT VERFAHREN:
300993     IF   KNR-SPERRE       = K-KNR-KEINE-SPERRE-BV-UNG OR
300993          KNR-SPERRE       = K-KNR-SPERRE-BV-UNG
300993          MOVE 1                  TO   Z-BLZ
300993                                       Z-KTONR
300993          MOVE SPACE              TO   Z-KTOINH
300993          MOVE 'O'                TO   Z-INSTITUT
300993     END-IF
           .
       BV-ZUORDNEN-EXIT.
           EXIT.

