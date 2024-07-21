      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DPVB0047.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DPVB0047.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:57:36   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 08:30:24   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: DPVB0047                                      * 00020000
      *    DIESE SECTION ERMITTELT DAS AUSLANDS-KZ FÜR DIE VSAM       * 00021001
      *    DATEIEN VB1 UND VB2                                        * 00021101
      *    --------------------------------------------------------   * 00021200
      *    ERSTELLT: O. NAUMANN       (DVAG)                          * 00021500
      *    DATUM:    08.06.1993                                       * 00021601
      ***************************************************************** 00021800
      *    VERSION   02   06.12.93                                   *  00021904
      ****************************************************************  00022002
      * AENDERUNGEN:                                                 *  00022102
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00022202
      * -------------------------------------------------------------*  00022302
      *         !16.09.93! 01 ! NEU                       ! NAUMANN  *  00022402
      * FEVB0037!06.12.93! 02 ! ERMITTLUNG AUSL-KZ NICHT KORREKT     *  00022503
      *         !        !    !                           ! NAUMANN  *  00022603
      *         !        !    !                           !          *  00022702
      *         !        !    !                           !          *  00022802
      *         !        !    !                           !          *  00022902
      *         !        !    !                           !          *  00023002
      *         !        !    !                           !          *  00023102
      *         !        !    !                           !          *  00023202
      *                                                              *  00023302
      * ENDE-LOGBUCH                                                 *  00023402
      ****************************************************************  00023502
      *---------------------------------------------------------------  00023600
       AUSL-KZ-BELEGEN SECTION.                                         00023701
      *----------------------------------------------------------       00023800
      *-INPUT : KNR-AUSL                                                00023901
061293*-        KTY-AUSL                                                00024004
      *-OUTPUT: Z-AUSL                                                  00024101
      *----------------------------------------------------------       00024200
       AUSL-KZ-BELEGEN-ANF.                                             00024301
      *----------------------------------------------------------       00024400
                                                                        00027604
CCC005*    EXEC SQL                                                     00027704
CCC004*                                                                 00027804
CCC004*       SELECT                                                    00027904
CCC004*              KNR_BEZ_KURZ                                       00029104
CCC004*       INTO                                                      00032104
CCC004*             :DCL-VIAT0008.KNR-BEZ-KURZ                          00033504
CCC004*              :NI-VIAT0008.NI-KNR-BEZ-KURZ                       00033604
CCC004*       FROM                                                      00035704
CCC004*             VIAT0008                                            00035804
CCC004*       WHERE                                                     00035904
CCC004*             KTY_NR          =   :DCL-VIVB0015.KTY-AUSL          00036004
CCC004*         AND KNR_NR          =   :DCL-VIVB0015.KNR-AUSL          00036104
CCC003*    END-EXEC                                                     00036304
061293                                                                  00038204
061293     EVALUATE   SQLCODE                                           00038304
061293     WHEN       KS-OK                                             00038404
061293*    BESTUECKEN DES AUSL-KZ'S                                     00038504
061293        IF KNR-BEZ-KURZ = 'D  '                                   00038604
061293           MOVE SPACE                   TO Z-AUSL                 00038704
061293        ELSE                                                      00038804
061293           MOVE KNR-BEZ-KURZ            TO Z-AUSL                 00038904
061293        END-IF                                                    00039004
061293     WHEN       KS-NOT-FOUND                                      00039104
061293*       ES WIRD DAS AUSL-KZ = DEUTSCHLAND SIMULIERT               00039205
061293        MOVE SPACE                      TO Z-AUSL                 00039305
061293     WHEN       OTHER                                             00039404
061293        SET VERARB-SQL-CODE-EX    TO TRUE                         00039504
061293        MOVE SQLCODE              TO WLR-SQL-CODE                 00039604
061293        IF    SQLERRML       >   ZERO                             00039704
061293            MOVE  SQLERRMC        TO   WLR-FEHLER-TEXT            00039804
061293         ELSE                                                     00039904
061293             MOVE  SPACE          TO   WLR-FEHLER-TEXT            00040004
061293        END-IF                                                    00040104
061293     END-EVALUATE                                                 00040204
061293*    EVALUATE   KNR-AUSL                                          00040304
061293*    WHEN       1                                                 00040404
061293*       MOVE    SPACE          TO Z-AUSL                          00040504
061293*    WHEN       2                                                 00040604
061293*       MOVE    'O  '          TO Z-AUSL                          00040704
061293*    WHEN       3                                                 00040804
061293*       MOVE    'F  '          TO Z-AUSL                          00040904
061293*    WHEN       4                                                 00041004
061293*       MOVE    'PL '          TO Z-AUSL                          00041104
061293*    WHEN       5                                                 00041204
061293*       MOVE    'A  '          TO Z-AUSL                          00041304
061293*    WHEN       6                                                 00041404
061293*       MOVE    'GB '          TO Z-AUSL                          00041504
061293*    WHEN       7                                                 00041604
061293*       MOVE    'NL '          TO Z-AUSL                          00041704
061293*    WHEN       8                                                 00041804
061293*       MOVE    'B  '          TO Z-AUSL                          00041904
061293*    WHEN       9                                                 00042004
061293*       MOVE    'CH '          TO Z-AUSL                          00042104
061293*    WHEN       10                                                00042204
061293*       MOVE    'P  '          TO Z-AUSL                          00042304
061293*    WHEN     OTHER                                               00042404
061293*       CONTINUE                                                  00042504
061293*    END-EVALUATE                                                 00042604
           .                                                            00042700
       AUSL-KZ-BELEGEN-EXIT.                                            00043001
           EXIT.                                                        00050000
