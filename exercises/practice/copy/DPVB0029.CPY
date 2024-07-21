      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DPVB0029.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DPVB0029.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:57:30   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 08:26:56   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: DPVB0029                                      * 00020000
      *    DIESE SECTION ERMITTELT DIE SONDERPROVISION EINES RFT ZU   * 00021000
      *    EINEM KONKRETEN UNTERSTELLTEN                              * 00021100
      *    --------------------------------------------------------   * 00021200
      *    ERSTELLT: O. NAUMANN       (DVAG)                          * 00021500
      *    DATUM:    03.04.1993                                       * 00021600
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
       RFT-BETR-AUSPROV-ERMITT SECTION.                                 00023500
      *----------------------------------------------------------       00023600
      * RFT-BETR-AUSPROV-ERMITT                                    *    00023700
      *------------------------------------------------------------*    00024000
      * INPUT : KNR-ST-TYP, WK-MS-NR-UNTST, WK-BS-NR-UNTST         *    00025000
      *         WB-RFT-PROV-STZ-GP IN WB-RFT-BETR                  *    00025201
      *         WB-RFT-PROZ-STZ    IN WB-RFT-BETR                  *    00025301
      *                                                            *    00026000
      * OUTPUT: Z-AUSPROV                                          *    00026100
      *                                                            *    00026200
      * VERARBEITUNG:                                              *    00026300
      *   -                                                        *    00026400
      *   -                                                        *    00026500
      *   -                                                        *    00026600
      *                                                            *    00026700
      *------------------------------------------------------------*    00026800
       RFT-BETR-AUSPROV-ERMITT-ANF.                                     00026900
      *    ERMITTLE PROVISIONSSATZ GRUPPE FÜR DEN UNTERSTELLTEN         00027000
           PERFORM  PROVSATZ-UNTST-GP                                   00028000
                                                                        00029000
      *    BEREITSTELLEN DES GRUPPENPROVISIONSSATZES                    00030000
      *    UND DES PROZENTSATZES DES BETREUERS                          00040000
           MOVE WB-RFT-PROV-STZ-GP  IN WB-RFT-BETR                      00050000
                                    TO WK-PROV-GP-BETR                  00060000
           MOVE WB-RFT-PROZ-STZ     IN WB-RFT-BETR                      00070000
                                    TO WK-PROZ-STZ                      00080000
                                                                        00090000
           PERFORM SOPROV-ERMITT                                        00100000
           .                                                            00127000
       RFT-BETR-AUSPROV-ERMITT-EXIT.                                    00128000
           EXIT.                                                        00129000
