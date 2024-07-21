      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DPAT0017.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DPAT0017.CPv  $
      *
      *   Rev 2.1   Dec 06 2017 15:05:28   t13ae23
      *AAT00059: Einführung Cobol 6.2
      *
      *   Rev 2.0   Feb 27 1999 11:56:50   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 08:13:52   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: DPAT0017      TITEL VERSORGEN - MENUEDIALOG   * 00020000
      *    --------------------------------------------------------   * 00030000
      *    VERSION   001                                              * 00040000
      *    --------------------------------------------------------   * 00050000
      *    ERSTELLT: W. SCHÖLLHAMMER (INTEGRATA AG)                   * 00060000
      *    DATUM:    31.03.1993                                       * 00070000
      *    STAND:    21.04.1993                                       * 00080012
      ***************************************************************** 00090000
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !        ! 2.0! NEU                       ! SCHÖLLH. *
      * AAT00059!17.11.17! 2.1! COBOL 6.2 ANPASSUNGEN     ! RAHNER   *
      *         !        !    !                           !          *
      ***************************************************************** 00090000
      *                                                                 00100000
      *    IM MENÜ-DIALOG WERDEN DIE 2 TITELFELDER IM MASKENKOPF WIE    00110002
      *    FOLGT VERSORGT:                                              00120000
      *    - TITEL 1: KONSTANT                                          00130000
      *    - TITEL 2: ABHÄNGIG VON CARR-DIALOG-AKT                      00131000
      *               DER TITEL-2 WIRD AUS DER TRANSAKTIONSDATEI        00132002
      *               GELESEN (K1-TRANS = CARR-DIALOG-AKT)              00133002
      *    (ZUGEHÖRIGES COPY-ELEMENT DER WORKING-STORAGE: DWAT0017)     00170000
      *                                                                 00180000
      *------------------------------------------------------------*    00190000
      * R711-TITEL-VERSORG                                         *    00200000
      *------------------------------------------------------------*    00210000
      * INPUT : CARR-DIALOG-AKT;                                   *    00220002
      *         K-DIALOG-TITEL1                                    *    00230000
      *                                                            *    00240000
      * OUTPUT: GEFUELLTE TITEL-ZEILEN IM BILDSCHIRMKOPF           *    00250000
      *                                                            *    00260000
      * VERARBEITUNG:                                              *    00270000
      *     VERSORGEN DER KOPFFELDER Z.T. ABHAENGIG VON DIALOG-AKT.*    00280000
      *                                                            *    00290000
      *------------------------------------------------------------*    00300000
       R711-TITEL-VERSORG SECTION.                                      00310000
       R711-ANF.                                                        00320000
           MOVE 'R711 ' TO ERR-ORT-SEC                                  00330000
           PERFORM ZR1-TRACE-KURZ                                       00340000
      *                                                                 00350000
           MOVE K-DIALOG-TITEL1              TO ARTITL1O                00360000
            MOVE DFHBMASK                    TO ARTITL1A                00370000
      *!-----------------                                               00380007
      *!   GESTRICHEN, DA TITEL-2 JETZT AUS VSAM GELESEN WIRD           00381007
      *!                                                                00382007
      *!   SET I-TITEL2                      TO 1                       00390001
      *!   SEARCH T-T2-ZEILE                                            00400001
      *!          VARYING I-TITEL2                                      00410001
      *!   AT END                                                       00420001
      *!      MOVE SPACE                     TO ARTITL2O                00430001
      *!   WHEN T-TITEL2-DIALOG(I-TITEL2) = CARR-DIALOG-AKT             00440001
      *!      MOVE T-TITEL2-TEXT(I-TITEL2)   TO ARTITL2O                00450001
      *!   END-SEARCH                                                   00460001
      *!                                                                00460107
      *!------------------                                              00460207
                                                                        00460302
      *    * ERMITTELN UND ZENTRIEREN TITEL-2                           00460402
                                                                        00460511
           INITIALIZE                         ZI-DWAT0018-INPUT,        00460611
                                              ZO-DWAT0018-OUTPUT        00460711
                                              ZIO-DWAT0018-INOUT        00460811
                                                                        00460911
      *    * ZUNÄCHST UMSCHLUESSELN TRANS                               00461011
           MOVE CARR-DIALOG-AKT            TO PI-UPAT0009-TRANS-IN-ANUM 00461111
           SET PI-UPAT0009-UMS-NACH-NUM    TO TRUE                      00461211
171117*    CALL 'UPAT0009' USING BY CONTENT   PI-UPAT0009-EINGABE       00461311
CCCPY1*    CALL K-UPAT0009 USING BY CONTENT   PI-UPAT0009-EINGABE       00461311
CCC004*                          BY REFERENCE PO-UPAT0009-AUSGABE,      00461411
CCC004*                                       ERR-ERROR-BEREICH         00461511
CCC004*    IF ERR-RC-SCHWERER-FEHLER                                    00461612
CCC004*       PERFORM UR99-SCHWERER-FEHLER                              00461711
CCC004*    END-IF                                                       00461811
CCC004*    MOVE PO-UPAT0009-TRANS-OUT-NUM  TO ZIO-DWAT0018-TD-KEY       00463011
CCC004*                                                                 00463111
      *    * ERMITTELN TITEL-2                                          00463211
CCC004*    SET TD-SERVICE-READ             TO TRUE                      00464001
CCC004*    PERFORM UR5-TRANSDAT-SERVICE                                 00465001
CCC004*                                                                 00466001
CCC004*    EVALUATE TRUE                                                00467001
CCC004*    WHEN TD-RC-OK                                                00468001
CCC004*       MOVE ZIO-DWAT0018-TD-DATA    TO TRANS-SATZ                00469008
CCC004*       MOVE K1-TEXT                 TO Z-DWAT0017-F1             00469102
CCC004*                                                                 00469202
      *       * ERMITTELN UND ZENTRIEREN TITEL-2                        00469311
CCC004*       MOVE ZERO                       TO Z-DWAT0017-ANZ-SP      00470702
CCC004*       IF Z-DWAT0017-F1 = SPACE                                  00470802
      *T         CONTINUE                                               00471003
CCC004*          MOVE SPACE                   TO ARTITL2O               00471106
CCC004*          MOVE '-  SPACE  - '          TO ARTITL2O (18:12)       00471207
CCC004*       ELSE                                                      00471502
CCC004*          PERFORM UNTIL Z-DWAT0017-F1-45 NOT = SPACE             00471602
CCC004*             MOVE SPACE                TO Z-DWAT0017-F2          00471702
CCC004*             MOVE Z-DWAT0017-F1        TO Z-DWAT0017-F2-2B45     00471802
CCC004*             MOVE Z-DWAT0017-F2        TO Z-DWAT0017-F1          00471902
CCC004*             ADD 1                     TO Z-DWAT0017-ANZ-SP      00472002
CCC004*          END-PERFORM                                            00472302
CCC004*          COMPUTE Z-DWAT0017-ANZ-SP-H ROUNDED                    00472402
CCC004*                = Z-DWAT0017-ANZ-SP / 2                          00472502
CCC004*          END-COMPUTE                                            00472602
CCC004*          MOVE K1-TEXT              TO Z-DWAT0017-F1             00472702
CCC004*          PERFORM UNTIL Z-DWAT0017-ANZ-SP-H = ZERO               00472802
CCC004*             MOVE SPACE                TO Z-DWAT0017-F2          00472902
CCC004*             MOVE Z-DWAT0017-F1        TO Z-DWAT0017-F2-2B45     00473002
CCC004*             MOVE Z-DWAT0017-F2        TO Z-DWAT0017-F1          00473102
CCC004*             SUBTRACT 1                FROM Z-DWAT0017-ANZ-SP-H  00473202
CCC004*          END-PERFORM                                            00473502
CCC004*          MOVE Z-DWAT0017-F1           TO ARTITL2O               00473602
CCC004*       END-IF                                                    00473702
CCC004*    WHEN OTHER                                                   00473801
CCC004*       MOVE SPACE                      TO ARTITL2O               00474106
CCC004*       MOVE 'NIX GEFUNDEN'             TO ARTITL2O (18:12)       00474207
      *T      MOVE K-1                        TO ERR-ORT-LFD            00474302
      *T      PERFORM UR93-PROG-FEHLER                                  00474402
CCC004*    END-EVALUATE                                                 00474501
CCC004*                                                                 00475000
CCC004*    MOVE DFHBMASK                      TO ARTITL2A               00480002
CCC004*    .                                                            00490000
CCC004*R711-EXIT.                                                       00500000
CCC004*    EXIT.                                                        00510000
      *                                                                 00520000
      ******* ENDE COPY-MEMBER DPAT0017 (TITEL - MENUEDIALOG) ********* 00530000
CCC009*    EJECT                                                        00540000

