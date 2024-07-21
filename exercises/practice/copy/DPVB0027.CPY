      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DPVB0027.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DPVB0027.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:56:56   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 08:26:26   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: DPVB0027                                      * 00020000
      *    DIESE SECTION ERMITTELT DEN INHALT VON MEPROVKZ            * 00021000
      *    (VB-PRAXISSTUFE IM ALTSYSTEM)                              * 00021100
      *    --------------------------------------------------------   * 00021400
      *    ERSTELLT: O. NAUMANN       (DVAG)                          * 00021700
      *    DATUM:    02.04.1993                                       * 00021800
      ***************************************************************** 00022000
      *    VERSION   01   16.09.93                                   *  00022101
      ****************************************************************  00022201
      * AENDERUNGEN:                                                 *  00022301
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00022401
      * -------------------------------------------------------------*  00022501
      *         !16.09.93! 01 ! NEU                       ! NAUMANN  *  00022601
      *         !        !    !                           !          *  00022701
      *         !        !    !                           !          *  00022801
      *         !        !    !                           !          *  00022901
      *         !        !    !                           !          *  00023001
      *         !        !    !                           !          *  00023101
      *         !        !    !                           !          *  00023201
      *         !        !    !                           !          *  00023301
      *                                                              *  00023401
      * ENDE-LOGBUCH                                                 *  00023501
      ****************************************************************  00023601
       EPROVKZ-ERMITT SECTION.                                          00023700
      *----------------------------------------------------------       00024000
      * EPROVKZ-ERMITT                                             *    00025000
      *------------------------------------------------------------*    00026000
      * INPUT : VBP-NR IN DCL-VIVB0007                             *    00026100
      *                                                            *    00026300
      * OUTPUT: Z-EPROVKZ                                          *    00026400
      *         WLR-RUECK-CODE                                     *    00026500
      *         WLR-FEHLER-TEXT, FALLS KEINE ZUORDNUNG VON VBP-NR  *    00026600
      *         NACH MEPROVKZ MÖGLICH IST.                         *    00026700
      *                                                            *    00026800
      * VERARBEITUNG:                                              *    00060000
      *   -                                                        *    00070000
      *   -                                                        *    00080000
      *   -                                                        *    00090000
      *                                                            *    00100000
      *------------------------------------------------------------*    00110000
       EPROVKZ-ERMITT-ANF.                                              00120000
           EVALUATE VBP-NR IN DCL-VIVB0007                              00130000
           WHEN     2 THRU 10                                           00140000
              COMPUTE Z-VBP-NR-1 = VBP-NR IN DCL-VIVB0007 - 1           00150000
              MOVE    Z-VBP-NR-1   TO Z-EPROVKZ                         00160000
           WHEN     OTHER                                               00170000
      *    LOG. FEHLER                                                  00180000
              SET VERARB-LOG-FEHLER     TO TRUE                         00190000
              MOVE 'EPROVKZ-ERMITT'     TO WK-PARAGRAPH                 00200000
              MOVE  WK-FEHLER-TEXT6     TO WLR-FEHLER-TEXT              00210000
           END-EVALUATE                                                 00220000
           .                                                            00310200
       EPROVKZ-ERMITT-EXIT.                                             00310300
           EXIT.                                                        00310400
