      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DPAT0033.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DPAT0033.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:56:52   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 08:16:30   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************  00000100
      *    COPY-MEMBER: DPAT0033    TITEL VERSORGEN - VERSCH. TITEL  *  00000200
      *    --------------------------------------------------------  *  00000300
      *    ERSTELLT: W. SCHÖLLHAMMER (INTEGRATA AG)                  *  00000600
      *    DATUM:    18.04.1994                                      *  00000700
      ****************************************************************  00000900
      *    VERSION   1    26.04.94 17:21:47
      ****************************************************************  00008000
      * AENDERUNGEN:                                                 *  00009000
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00009100
      * -------------------------------------------------------------*  00009200
      * AFVB0102!22.04.94! 01 ! NEU                       ! SCHÖLLH. *  00009302
      *         !        !    !                           !          *  00010200
      *         !        !    !                           !          *  00010300
      *         !        !    !                           !          *  00010400
      *         !        !    !                           !          *  00010500
      *                                                              *  00010600
      * ENDE-LOGBUCH                                                 *  00010700
      ****************************************************************  00010800
      *                                                                 00100000
      *    DIESE RAHMEN-ROUTINE WIRD VERWENDET FÜR DIE VERSORGUNG VON   00110000
      *    MASKEN-TITELN, DIE WIE FOLGT ERFOLGT:                        00120000
      *    - TITEL 1: KONSTANT AUS K-DIALOG-TITEL1                      00130000
      *    - TITEL 2: ABHÄNGIG VON CARD-DIALOG-SCHRITT-AKT              00131000
      *               ER WIRD AUS DER TABELLE T-TITEL2-TAB GELESEN      00133000
      *    (BEIDES IM JEWEILIGEN PGM-KOPF ZU DEFINIEREN)                00133100
      *                                                                 00135000
      *------------------------------------------------------------*    00136000
      * R711-TITEL-VERSORG                                         *    00137000
      *------------------------------------------------------------*    00138000
      * INPUT : CARD-DIALOG-SCHRITT-AKT;                           *    00139000
      *         K-DIALOG-TITEL1, T-TITEL2-TAB                      *    00140000
      *                                                            *    00150000
      * OUTPUT: GEFUELLTE TITEL-ZEILEN IM BILDSCHIRMKOPF           *    00160000
      *                                                            *    00170000
      * VERARBEITUNG:                                              *    00180000
      *     VERSORGEN DER KOPFFELDER,                              *    00190000
      *     Z.T. ABHAENGIG VON DIALOG-SCHRITT-AKT                  *    00191000
      *                                                            *    00200000
      *------------------------------------------------------------*    00210000
       R711-TITEL-VERSORG SECTION.                                      00220000
       R711-ANF.                                                        00230000
           MOVE 'R711 ' TO ERR-ORT-SEC                                  00240000
           PERFORM ZR1-TRACE-KURZ                                       00250000
      *                                                                 00260000
           MOVE K-DIALOG-TITEL1              TO ARTITL1O                00270000
            MOVE DFHBMASK                    TO ARTITL1A                00280000
                                                                        00310000
           SET I-TITEL2                      TO 1                       00320000
           SEARCH T-T2-ZEILE                                            00330000
                  VARYING I-TITEL2                                      00340000
           AT END                                                       00350000
              MOVE SPACE                     TO ARTITL2O                00360000
           WHEN T-TITEL2-TRANS(I-TITEL2) = CARD-DIALOG-SCHRITT-AKT      00370000
              MOVE T-TITEL2-TEXT(I-TITEL2)   TO ARTITL2O                00380000
           END-SEARCH                                                   00390000
           .                                                            00391001
       R711-EXIT.                                                       00400001
           EXIT.                                                        00410001
      *                                                                 00474900
      ******* ENDE COPY-MEMBER DPAT0033 (TITEL - VERSCH. TITEL) ******* 00475000
      *                                                                 00476000
