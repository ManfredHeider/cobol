      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DPVB0052.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DPVB0052.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:57:36   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 08:31:30   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: DPVB0052                                      * 00020000
      *    DIESE SECTION ERMITTELT DIE BANKVERBINDUNG AUS DEM VB -    * 00021000
      *    SATZBETT                                                   * 00021100
      *    --------------------------------------------------------   * 00021200
      *    ERSTELLT: B. BOHLINGER     (DVAG)                          * 00021500
      *    DATUM:    13.10.1995                                       * 00021603
      ***************************************************************** 00021800
      *    VERSION   1    13.10.95 15:15:34
      ***************************************************************** 00022000
      * AENDERUNGEN:                                                 *  00022100
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00022200
      * -------------------------------------------------------------*  00022300
      * FEOP0125!13.10.95! 01 ! MOD VERSION VON DPVB0045  !BOHLINGER *  00022403
      *         !        !    !                           !          *  00022700
      *         !        !    !                           !          *  00022800
      *         !        !    !                           !          *  00022900
      *         !        !    !                           !          *  00023000
      *                                                              *  00023100
      * ENDE-LOGBUCH                                                 *  00023200
      ****************************************************************  00023300
      *---------------------------------------------------------------  00023400
       VB-BV-VERARB SECTION.                                            00023500
      *----------------------------------------------------------       00023600
      *-INPUT : KEY-ID                                                  00023700
      *-OUTPUT: MINSTITUT                                               00023800
      *-        MBLZ                                                    00023900
      *-        MKTONR                                                  00024000
      *-        MKTOINH                                                 00024100
      *-        MSPERRE                                                 00024200
      *----------------------------------------------------------       00025000
       VB-BV-VERARB-ANF.                                                00045101
      *----------------------------------------------------------       00045200
           MOVE KEY-ID                 TO WB-ST-NR      IN WB-VIVB0021  00045700
           MOVE 1                      TO WB-KNR-BV-TYP IN WB-VIVB0021  00045800
           PERFORM BV-LESEN                                             00045900
           EVALUATE   SQLCODE                                           00046000
           WHEN       KS-OK                                             00046100
              PERFORM BV-ZUORDNEN-NEU                                   00046300
              PERFORM VB1-BV-FUELLEN                                    00046400
           WHEN       KS-NOT-FOUND                                      00046500
              CONTINUE                                                  00046600
           WHEN       OTHER                                             00046700
              SET VERARB-SQL-CODE-EX    TO TRUE                         00046800
              MOVE SQLCODE              TO WLR-SQL-CODE                 00046900
              IF    SQLERRML       >   ZERO                             00047000
                  MOVE  SQLERRMC        TO   WLR-FEHLER-TEXT            00047100
               ELSE                                                     00047200
                   MOVE  SPACE          TO   WLR-FEHLER-TEXT            00047300
              END-IF                                                    00047400
           END-EVALUATE                                                 00047500
                                                                        00047600
           .                                                            00048000
       VB-BV-VERARB-EXIT.                                               00051000
           EXIT.                                                        00060000
