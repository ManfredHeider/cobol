      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DPVB0028.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DPVB0028.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 16:09:10   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 08:26:42   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: DPVB0028                                      * 00020000
      *    DIESE SECTION ERMITTELT DEN INHALT VON MVARPROV            * 00021000
      *    (VARIABLER PROVISIONSSATZ) FÜR RF-R UND RF-A               * 00021100
      *    --------------------------------------------------------   * 00021200
      *    ERSTELLT: O. NAUMANN       (DVAG)                          * 00021500
      *    DATUM:    02.04.1993                                       * 00021600
      ***************************************************************** 00021800
      *    VERSION   01   16.09.93                                   *  00021902
      ****************************************************************  00022002
      * AENDERUNGEN:                                                 *  00022102
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00022202
      * -------------------------------------------------------------*  00022302
      *         !16.09.93! 01 ! NEU                       ! NAUMANN  *  00022402
      *         !        !    !                           !          *  00022502
      *         !        !    !                           !          *  00022602
      *         !        !    !                           !          *  00022702
      *         !        !    !                           !          *  00022802
      *         !        !    !                           !          *  00022902
      *         !        !    !                           !          *  00023002
      *         !        !    !                           !          *  00023102
      *                                                              *  00023202
      * ENDE-LOGBUCH                                                 *  00023302
      ****************************************************************  00023402
       VARPROV-ERMITT SECTION.                                          00023500
      *----------------------------------------------------------       00023600
      * VARPROV-ERMITT                                             *    00023700
      *------------------------------------------------------------*    00024000
      * INPUT : Z-EPROVKZ                                          *    00025000
      *                                                            *    00026000
      * OUTPUT: Z-VARPROV                                          *    00026100
      *                                                            *    00026500
      * VERARBEITUNG:                                              *    00026600
      *   -                                                        *    00026700
      *   -                                                        *    00026800
      *   -                                                        *    00026900
      *                                                            *    00027000
      *------------------------------------------------------------*    00028000
       VARPROV-ERMITT-ANF.                                              00029001
           IF Z-EPROVKZ NOT = SPACE                                     00030001
      *       ERMITTLE PROV.-SATZ (MA-STUFE) UND                        00040001
      *                PROV.-SATZ (VB-PRAXIS)                           00050001
                                                                        00060001
              PERFORM  PROVSATZ-STUFE                                   00070001
              PERFORM  PROVSATZ-VBPRAXIS                                00080001
                                                                        00090001
              IF  WK-PROV-GP-UNTST NOT < WK-PROV-AP-UNTST               00100001
                  MOVE WK-PROV-GP-UNTST   TO Z-VARPROV                  00110001
              ELSE                                                      00120001
                  MOVE WK-PROV-AP-UNTST   TO Z-VARPROV                  00121001
              END-IF                                                    00122001
           ELSE                                                         00123001
      *       ERMITTLE PROV.-SATZ (MA-STUFE)                            00124001
                                                                        00125001
              PERFORM  PROVSATZ-STUFE                                   00126001
                                                                        00127001
              MOVE WK-PROV-GP-UNTST       TO Z-VARPROV                  00128001
           END-IF                                                       00129001
           .                                                            00130000
       VARPROV-ERMITT-EXIT.                                             00140001
           EXIT.                                                        00150000
