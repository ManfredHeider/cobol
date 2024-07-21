      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DPVB0030.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DPVB0030.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:57:30   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 08:27:10   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: DPVB0030                                      * 00020000
      *    DIESE SECTION ERMITTELT DIE SONDERPROVISION EINES KL  ZU   * 00021000
      *    EINEM KONKRETEN UNTERSTELLTEN                              * 00021100
      *    --------------------------------------------------------   * 00021200
      *    ERSTELLT: O. NAUMANN       (DVAG)                          * 00021500
      *    DATUM:    03.04.1993                                       * 00021600
      ***************************************************************** 00021800
      *    VERSION   01   16.09.93                                   *  00021905
      ****************************************************************  00022005
      * AENDERUNGEN:                                                 *  00022105
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00022205
      * -------------------------------------------------------------*  00022305
      *         !16.09.93! 01 ! NEU                       ! NAUMANN  *  00022405
      *         !        !    !                           !          *  00022505
      *         !        !    !                           !          *  00022605
      *         !        !    !                           !          *  00022705
      *         !        !    !                           !          *  00022805
      *         !        !    !                           !          *  00022905
      *         !        !    !                           !          *  00023005
      *         !        !    !                           !          *  00023105
      *                                                              *  00023205
      * ENDE-LOGBUCH                                                 *  00023305
      ****************************************************************  00023405
       KL-BETR-AUSPROV-ERMITT SECTION.                                  00023500
      *----------------------------------------------------------       00023600
      * KL-BETR-AUSPROV-ERMITT                                     *    00023700
      *------------------------------------------------------------*    00024000
      * INPUT : KNR-ST-TYP, WK-MS-NR-UNTST, WK-BS-NR-UNTST         *    00025000
      *         WB-KL-PROV-STZ-GP IN WB-KL-BETR                    *    00025101
      *         WB-KL-PROZ-STZ    IN WB-KL-BETR                    *    00025201
      *                                                            *    00026000
      * OUTPUT: Z-AUSPROV                                          *    00026100
      *                                                            *    00026200
      * VERARBEITUNG:                                              *    00026300
      * AB R 3.0.00 WIRD KL NACH MA BZW. RF-R KONVERTIERT.         *    00026402
      * D.H.: DER KL ERSCHEINT IM ALTSYSTEM NICHT ALS KL, SONDERN  *    00026502
      * ALS MA BZW. ALS RF-R!!!                                    *    00026602
      * DAS HEIßT AUCH, DAß NUR BEI BESTIMMTEN KONSTELLATIONEN DES *    00026802
      * KL EINE SONDERPROVISION ERMITTELT WERDEN MUß.              *    00026902
      * DIES RICHTET SICH NACH DEM DATENFELD WB-KL-PROZ-STZ!!!     *    00027002
      * WB-KL-PROZ-STZ =    0       : Z-AUSPROV = 0                *    00027102
      * WB-KL-PROZ-STZ =  100       : Z-AUSPROV = 0                *    00027202
      * WB-KL-PROZ-STZ < 100 UND > 0: ERMITTLE Z-AUSPROV           *    00027302
      *                                                            *    00027400
      *------------------------------------------------------------*    00027500
       KL-BETR-AUSPROV-ERMITT-ANF.                                      00027600
           EVALUATE WB-KL-PROZ-STZ                                      00027702
               WHEN ZERO                                                00027802
               WHEN  100                                                00027902
                  MOVE    ZERO             TO Z-AUSPROV                 00028002
               WHEN  OTHER                                              00028202
      *           ERMITTLE PROVISIONSSATZ GRUPPE FÜR DEN UNTERSTELLTEN  00028303
                  PERFORM  PROVSATZ-UNTST-GP                            00028402
                                                                        00028502
      *           BEREITSTELLEN DES GRUPPENPROVISIONSSATZES             00028603
      *           UND DES PROZENTSATZES DES BETREUERS                   00028703
                  MOVE WB-KL-PROV-STZ-GP   IN WB-KL-BETR                00028802
                                           TO WK-PROV-GP-BETR           00028902
                  MOVE WB-KL-PROZ-STZ      IN WB-KL-BETR                00029002
                                           TO WK-PROZ-STZ               00029102
                                                                        00029202
                  PERFORM SOPROV-ERMITT                                 00029302
           END-EVALUATE                                                 00030002
           .                                                            00110000
       KL-BETR-AUSPROV-ERMITT-EXIT.                                     00120000
           EXIT.                                                        00130000
