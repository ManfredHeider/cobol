      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DPVB0041.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DPVB0041.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:57:34   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 08:29:06   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: DPVB0041                                      * 00020000
      *    DIESE SECTION BESCHICKT DIE ANSCHRIFTENDATENFELDER AUS     * 00021000
      *    DEM SATZBETT DER VB1-DATEI                                 * 00021100
      *    --------------------------------------------------------   * 00021300
      *    ERSTELLT: O. NAUMANN       (DVAG)                          * 00021600
      *    DATUM:    12.05.1993                                       * 00021700
      ***************************************************************** 00021900
      *    VERSION   01   16.09.93                                   *  00022003
      ****************************************************************  00022103
      * AENDERUNGEN:                                                 *  00022203
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00022303
      * -------------------------------------------------------------*  00022403
      *         !16.09.93! 01 ! NEU                       ! NAUMANN  *  00022503
      *         !        !    !                           !          *  00022603
      *         !        !    !                           !          *  00022703
      *         !        !    !                           !          *  00022803
      *         !        !    !                           !          *  00022903
      *         !        !    !                           !          *  00023003
      *         !        !    !                           !          *  00023103
      *         !        !    !                           !          *  00023203
      *                                                              *  00023303
      * ENDE-LOGBUCH                                                 *  00023403
      ****************************************************************  00023503
      *---------------------------------------------------------------  00023600
       VB1-ANSCHRIFT-FUELLEN SECTION.                                   00023700
      *----------------------------------------------------------       00023800
      *-INPUT : Z-STRASSE                                               00023900
      *-        Z-PLZ                                                   00024000
      *-        Z-AUSL                                                  00024100
      *-        Z-ORT                                                   00024200
      *-        MAAN-TELEFON                                            00024302
      *-        KNR-MAAN-TYP                                            00024402
      *-OUTPUT: MSTRASSE                                                00024500
      *-        MPLZ                                                    00024600
      *-        MAUSL                                                   00024700
      *-        MORT                                                    00024800
      *-        MTELNR                                                  00024900
      *-        MZUSATZKZ                                               00025000
      *----------------------------------------------------------       00025100
       VB1-ANSCHRIFT-FUELLEN-ANF.                                       00025300
      *----------------------------------------------------------       00026000
           MOVE Z-STRASSE       TO MSTRASSE                             00043900
           MOVE Z-PLZ           TO MPLZ                                 00044000
           MOVE Z-AUSL          TO MAUSL                                00044100
           MOVE Z-ORT           TO MORT                                 00044200
                                                                        00044302
           MOVE MAAN-TELEFON    TO MTELNR                               00044502
                                                                        00044800
           EVALUATE KNR-MAAN-TYP                                        00044902
           WHEN    1                                                    00045000
      *    PRIVATANSCHRIFT                                              00045100
              MOVE 4            TO MZUSATZKZ                            00045200
           WHEN    2                                                    00045300
      *    BÜROANSCHRIFT                                                00045400
              MOVE 5            TO MZUSATZKZ                            00045500
           WHEN OTHER                                                   00045600
      *    KEINE ANGABE                                                 00045701
      *       LOG. FEHLER                                               00045801
              SET   VERARB-LOG-FEHLER      TO TRUE                      00045901
              MOVE 'VB1-ANSCHRIFT-FUELLEN' TO WK-PARAGRAPH              00046001
              MOVE  WK-FEHLER-TEXT6        TO WLR-FEHLER-TEXT           00046101
           END-EVALUATE                                                 00046200
           .                                                            00046300
       VB1-ANSCHRIFT-FUELLEN-EXIT.                                      00047000
           EXIT.                                                        00050000
