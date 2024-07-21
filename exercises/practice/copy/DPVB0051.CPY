      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DPVB0051.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DPVB0051.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:57:36   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 08:31:18   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: DPVB0051                                      * 00020000
      *    DIESE SECTION ORDNET DIE DATENFELDER DER BV ZWISCHENFEL-   * 00021000
      *    DERN ZU.                                                   * 00021100
      *    MOD. VERSION   1    13.10.95 15:15:17
      *    --------------------------------------------------------   * 00021200
      *    ERSTELLT: B. BOHLINGER     (DVAG)                          * 00021500
      *    DATUM:    13.10.1995                                       * 00021600
      ***************************************************************** 00021800
      *    VERSION   03   01.10.93                                   *  00090101
      ***************************************************************** 00021800
      * AENDERUNGEN:                                                 *  00021900
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00022000
      * -------------------------------------------------------------*  00022100
      * FEOP0125!13.09.95! 01 ! NEU                       ! BOHLINGER*  00022200
      *         !        !    !                           !          *  00022600
      *         !        !    !                           !          *  00022700
      *         !        !    !                           !          *  00022800
      *         !        !    !                           !          *  00022900
      *                                                              *  00023000
      * ENDE-LOGBUCH                                                 *  00023100
      ****************************************************************  00023200
      *----------------------------------------------------------
       BV-ZUORDNEN-NEU SECTION.
      *----------------------------------------------------------
      *----------------------------------------------------------       00023500
      *-INPUT : DCL-TBVB0021                                            00023600
      *-OUTPUT: Z-INSTITUT                                              00023700
      *-        Z-BLZ                                                   00023800
      *-        Z-KTONR                                                 00023900
      *-        Z-KTOINH                                                00024000
      *-        Z-SPERRE                                                00024100
      *----------------------------------------------------------       00025000
       BV-ZUORDNEN-NEU-ANF.
           MOVE BV-BANKNAME             TO   Z-INSTITUT
           MOVE BV-KTO-INHABER          TO   Z-KTOINH

           MOVE ZERO                    TO   Z-SPACE
           INSPECT BV-BLZ TALLYING Z-SPACE
                   FOR CHARACTERS BEFORE SPACE
           IF Z-SPACE  =  ZERO
              MOVE ZERO                 TO   Z-BLZ
           ELSE
              MOVE BV-BLZ(1:Z-SPACE)    TO   Z-BLZ
           END-IF

           MOVE ZERO                    TO   Z-SPACE
           INSPECT BV-KTO-NR TALLYING Z-SPACE
                   FOR CHARACTERS BEFORE SPACE
           IF Z-SPACE  =  ZERO
              MOVE ZERO                 TO   Z-KTONR
           ELSE
              MOVE BV-KTO-NR(1:Z-SPACE) TO   Z-KTONR
           END-IF

           MOVE KNR-SPERRE              TO   Z-SPERRE

      *    BEI UNGÜLTIGER BANKVERBINDUNG WIRD WIE FOLGT VERFAHREN:
           IF   KNR-SPERRE       = K-KNR-KEINE-SPERRE-BV-UNG OR
                KNR-SPERRE       = K-KNR-SPERRE-BV-UNG
                MOVE 1                  TO   Z-BLZ
                                             Z-KTONR
                MOVE SPACE              TO   Z-KTOINH
                MOVE 'O'                TO   Z-INSTITUT
           END-IF
           .
       BV-ZUORDNEN-NEU-EXIT.
           EXIT.

