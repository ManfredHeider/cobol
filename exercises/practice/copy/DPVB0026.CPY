      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DPVB0026.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DPVB0026.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:56:56   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 08:26:10   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: DPVB0026                                      * 00020000
      *    DIESE SECTION KORRIGIERT DIE VB-PRAXIS-STUFE               * 00021000
      *    IN ABHÄNGIGKEIT DER MA-STUFE                               * 00021100
      *    DIESE SECTION WIRD IMMER UNMITTELBAR NACH DER ERMITT-      * 00021200
      *    LUNG DER MA-STUFE DURCHLAUFEN (IN TAB-STUFE!!!)            * 00021300
      *    --------------------------------------------------------   * 00021400
      *    ERSTELLT: O. NAUMANN       (DVAG)                          * 00021700
      *    DATUM:    02.04.1993                                       * 00021800
      ***************************************************************** 00022000
      *    VERSION   01   16.09.93                                   *  00022104
      ****************************************************************  00022201
      * AENDERUNGEN:                                                 *  00022301
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00022401
      * -------------------------------------------------------------*  00022501
      *         !27.08.93!    ! VB-PRAXIS-STUFENBERECHNUNG!          *  00022601
      *         !        !    ! NICHT KORREKT. S.TESTBERICHT STUFE II*  00022702
      *         !        !    ! S. 29, FEHLER-NR. 1       !          *  00022801
      *         !        !    ! COPYSTRECKE DPVB0026 GEÄ. ! NAUMANN  *  00022901
      *         !13.09.93!    ! VB-PRAXIS-STUFENBERECHNUNG!          *  00023003
      *         !        !    ! NICHT KORREKT. S.TESTBERICHT STUFE II*  00023103
      *         !        !    ! S. 45,1.B), FEHLER-NR. 4  !          *  00023203
      *         !        !    ! COPYSTRECKE DPVB0026 GEÄ. ! NAUMANN  *  00023303
      *         !16.09.93! 01 ! PROD.UEBERGABE            ! NAUMANN  *  00023404
      *         !        !    !                           !          *  00023501
      *         !        !    !                           !          *  00023601
      *         !        !    !                           !          *  00023701
      *         !        !    !                           !          *  00023801
      *                                                              *  00023901
      * ENDE-LOGBUCH                                                 *  00024001
      ****************************************************************  00024101
       EPROVKZ-NACHBEARB SECTION.                                       00025000
      *------------------------------------------------------------*    00026001
      * EPROVKZ-NACHBEARB                                          *    00027000
      *------------------------------------------------------------*    00028000
      * INPUT : Z-EPROVKZ                                          *    00029000
      *         MS-NR, BS-NR IN DCL-VIVB0012                       *    00030000
      *         WB-VBP-NR    IN WB-VBPZ-ALT                        *    00030101
      *                                                            *    00040000
      * OUTPUT: Z-EPROVKZ                                          *    00050000
      *                                                            *    00090000
      * VERARBEITUNG:                                              *    00100000
      *   -                                                        *    00110000
      *   -                                                        *    00120000
      *   -                                                        *    00130000
      *                                                            *    00140000
      *------------------------------------------------------------*    00150000
       EPROVKZ-NACHBEARB-ANF.                                           00151000
           IF Z-EPROVKZ NOT = SPACE                                     00160000
130993        IF MS-NR IN DCL-VIVB0012 <= K-STF-VBA                     00170003
130993           MOVE SPACE TO Z-EPROVKZ                                00170103
130993        ELSE                                                      00170203
      *          ERMITTLE PROV.-SATZ (MA-STUFE) UND                     00170303
      *                   PROV.-SATZ (VB-PRAXIS)                        00170403
270893*          ES WIRD DER PROV.-SATZ DER AKTUELLEN MA-STUFE ERMITT.  00170503
                                                                        00170603
                 MOVE MS-NR  IN DCL-VIVB0012 TO WK-MS-NR-UNTST          00170703
                 MOVE BS-NR  IN DCL-VIVB0012 TO WK-BS-NR-UNTST          00170803
                 PERFORM  PROVSATZ-STUFE                                00170903
                                                                        00171003
270893*          ES WIRD DER PROV.-SATZ DER ALTEN                       00171103
270893*          VB-PRAXISSTUFE ERMITTELT                               00171203
270893*          UND NICHT DER DER AKTUELLEN VB-PRAXISSTUFE!!!          00171303
270893           MOVE WB-VBP-NR IN WB-VBPZ-ALT TO WK-VBP-NR             00171403
270893*          MOVE VBP-NR IN DCL-VIVB0007   TO WK-VBP-NR             00171503
                 PERFORM  PROVSATZ-VBPRAXIS                             00171603
                                                                        00171703
                 IF  WK-PROV-GP-UNTST NOT < WK-PROV-AP-UNTST            00171803
                     MOVE SPACE TO   Z-EPROVKZ                          00171903
                 ELSE                                                   00172003
                     CONTINUE                                           00172103
                 END-IF                                                 00172203
130993        END-IF                                                    00173003
                                                                        00310100
           ELSE                                                         00310200
              CONTINUE                                                  00310300
           END-IF                                                       00310400
           .                                                            00311000
       EPROVKZ-NACHBEARB-EXIT.                                          00312000
           EXIT.                                                        00313000
