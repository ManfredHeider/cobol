      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DPVB0009.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DPVB0009.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:56:50   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 08:21:48   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: DPVB0009                                      * 00020000
      *    ERMITTELN GRUPPENPROVISIONSSATZ EINES UNTERSTELLTEN AUF-   * 00021000
      *    GRUND DES STELLENTYPES BZW.                                * 00021100
      *    BEIM MA AUFGRUND DER MS-NR UND GGF. DER BS-NR              * 00021200
      *    --------------------------------------------------------   * 00022000
      *    ERSTELLT: O. NAUMANN       (DVAG)                          * 00025000
      *    DATUM:    01.04.1993                                       * 00026000
      ***************************************************************** 00028000
      *    VERSION   01   16.09.93                                   *  00028101
      ****************************************************************  00028201
      * AENDERUNGEN:                                                 *  00028301
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00028401
      * -------------------------------------------------------------*  00028501
      *         !16.09.93! 01 ! NEU                       ! NAUMANN  *  00028601
      *         !        !    !                           !          *  00028701
      *         !        !    !                           !          *  00028801
      *         !        !    !                           !          *  00028901
      *         !        !    !                           !          *  00029001
      *         !        !    !                           !          *  00029101
      *         !        !    !                           !          *  00029201
      *         !        !    !                           !          *  00029301
      *                                                              *  00029401
      * ENDE-LOGBUCH                                                 *  00029501
      ****************************************************************  00029601
      *---------------------------------------------------------------  00029700
       PROVSATZ-UNTST-GP SECTION.                                       00030000
      *---------------------------------------------------------------  00040000
      *-INPUT:  KNR-ST-TYP, WK-MS-NR-UNTST, WK-BS-NR-UNTST              00050000
      *-OUTPUT: WK-PROV-GP-UNTST                                     -  00060000
      *---------------------------------------------------------------  00070000
       PROVSATZ-UNTST-GP-ANF.                                           00080000
                                                                        00090000
           MOVE  ZERO TO WK-PROV-GP-UNTST                               00100000
                                                                        00110000
           EVALUATE KNR-ST-TYP                                          00120000
           WHEN     2                                                   00130000
      *    MA                                                           00140000
               PERFORM PROVSATZ-STUFE                                   00150000
           WHEN     4                                                   00160000
      *    RFT                                                          00170000
               MOVE RFT-PROV-STZ-GP TO WK-PROV-GP-UNTST                 00180000
           WHEN     5                                                   00190000
      *    KL                                                           00200000
               MOVE KL-PROV-STZ-GP  TO WK-PROV-GP-UNTST                 00210000
           WHEN     1                                                   00220000
           WHEN     6                                                   00230000
           WHEN     7                                                   00240000
           WHEN     3                                                   00250000
      *    ED,JUR,SDNR,GK                                               00260000
      *    LOG. FEHLER                                                  00270000
               SET VERARB-LOG-FEHLER        TO TRUE                     00280000
               MOVE 'PROVSATZ-UNTST-GP'     TO WK-PARAGRAPH             00290000
               MOVE  WK-FEHLER-TEXT6        TO WLR-FEHLER-TEXT          00300000
           WHEN     OTHER                                               00310000
      *    LOG. FEHLER                                                  00320000
               SET VERARB-LOG-FEHLER        TO TRUE                     00330000
               MOVE 'PROVSATZ-UNTST-GP'     TO WK-PARAGRAPH             00340000
               MOVE  WK-FEHLER-TEXT6        TO WLR-FEHLER-TEXT          00350000
           END-EVALUATE                                                 00360000
           .                                                            00370000
       PROVSATZ-UNTST-GP-EXIT.                                          00380000
           EXIT.                                                        00390000
