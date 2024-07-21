      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DPVB0045.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DPVB0045.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:57:34   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 08:29:58   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: DPVB0045                                      * 00020000
      *    DIESE SECTION ERMITTELT DIE BANKVERBINDUNG AUS DEM VB1-    * 00021000
      *    SATZBETT                                                   * 00021100
      *    --------------------------------------------------------   * 00021200
      *    ERSTELLT: O. NAUMANN       (DVAG)                          * 00021500
      *    DATUM:    19.05.1993                                       * 00021600
      ***************************************************************** 00021800
      *    VERSION   02   16.09.93                                   *  00021907
      ***************************************************************** 00022007
      * AENDERUNGEN:                                                 *  00022105
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00022205
      * -------------------------------------------------------------*  00022305
      *         !19.05.93! 01 ! NEU                       ! NAUMANN  *  00022405
      *         !29.07.93! 02 ! S. TESTBERICHT TEST STUFE ! NAUMANN  *  00022505
      *         !        !    ! I, 1. TEST, FEHL-NR 3/1   !          *  00022605
      *         !        !    !                           !          *  00022705
      *         !        !    !                           !          *  00022805
      *         !        !    !                           !          *  00022905
      *         !        !    !                           !          *  00023005
      *                                                              *  00023105
      * ENDE-LOGBUCH                                                 *  00023205
      ****************************************************************  00023305
      *---------------------------------------------------------------  00023400
       VB1-BV-VERARB SECTION.                                           00023500
      *----------------------------------------------------------       00023600
      *-INPUT : KEY-ID                                                  00023701
      *-OUTPUT: MINSTITUT                                               00023801
      *-        MBLZ                                                    00023901
      *-        MKTONR                                                  00024001
      *-        MKTOINH                                                 00024101
      *-        MSPERRE                                                 00024201
      *----------------------------------------------------------       00025000
       VB1-BV-VERARB-ANF.                                               00045100
      *----------------------------------------------------------       00045200
           MOVE KEY-ID                 TO WB-ST-NR      IN WB-VIVB0021  00045701
           MOVE 1                      TO WB-KNR-BV-TYP IN WB-VIVB0021  00045801
           PERFORM BV-LESEN                                             00045901
           EVALUATE   SQLCODE                                           00046002
           WHEN       KS-OK                                             00046102
              PERFORM BV-ZUORDNEN                                       00046302
290793        PERFORM VB1-BV-FUELLEN                                    00046405
           WHEN       KS-NOT-FOUND                                      00046502
              CONTINUE                                                  00046604
           WHEN       OTHER                                             00046702
              SET VERARB-SQL-CODE-EX    TO TRUE                         00046802
              MOVE SQLCODE              TO WLR-SQL-CODE                 00046902
              IF    SQLERRML       >   ZERO                             00047002
                  MOVE  SQLERRMC        TO   WLR-FEHLER-TEXT            00047102
               ELSE                                                     00047202
                   MOVE  SPACE          TO   WLR-FEHLER-TEXT            00047302
              END-IF                                                    00047402
           END-EVALUATE                                                 00047502
                                                                        00047605
290793*    PERFORM VB1-BV-FUELLEN                                       00047705
           .                                                            00048001
       VB1-BV-VERARB-EXIT.                                              00051000
           EXIT.                                                        00060000
