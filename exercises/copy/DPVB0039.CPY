      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DPVB0039.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DPVB0039.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:57:32   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 08:28:40   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: DPVB0039                                      * 00020000
      *    DIESE SECTION ORDNET DIE DATENFELDER AUS DER TABLE MAAN    * 00021000
      *    DEN ENTSPRECHENDEN ZWISCHENFELDERN ZU                      * 00021100
      *    --------------------------------------------------------   * 00021200
      *    ERSTELLT: O. NAUMANN       (DVAG)                          * 00021500
      *    DATUM:    12.05.1993                                       * 00021600
      ***************************************************************** 00021800
      *    VERSION   01   16.09.93                                   *  00021900
      ****************************************************************  00022000
      * AENDERUNGEN:                                                 *  00022100
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00022200
      * -------------------------------------------------------------*  00022300
      *         !16.09.93! 01 ! NEU                       ! NAUMANN  *  00022400
      *         !        !    !                           !          *  00022500
      *         !        !    !                           !          *  00022600
      *         !        !    !                           !          *  00022700
      *         !        !    !                           !          *  00022800
      *         !        !    !                           !          *  00022900
      *         !        !    !                           !          *  00023000
      *         !        !    !                           !          *  00023100
      *                                                              *  00023200
      * ENDE-LOGBUCH                                                 *  00023300
      ****************************************************************  00023400
      *---------------------------------------------------------------  00023500
       ANSCHRIFT-UNGUELT-BELEGEN SECTION.                               00023600
      *---------------------------------------------------------------  00023700
      *-INPUT:  KNR-GRND     IN DCL-VIVB0015                         -  00023800
      *-                                                             -  00023900
      *-OUTPUT: Z-STRASSE                                            -  00024000
      *-        Z-PLZ                                                -  00024100
      *-        Z-AUSL                                               -  00024200
      *-        Z-ORT                                                -  00024300
      *---------------------------------------------------------------  00024400
       ANSCHRIFT-UNGUELT-BELEGEN-ANF.                                   00024500
      *----------------------------------------------------------       00025000
           EVALUATE KNR-GRND                                            00026000
           WHEN     K-KNR-UNBEKANNT-VERZOGEN                            00030000
              MOVE 'UNBEKANNT VERZOGEN' TO Z-STRASSE                    00032000
              MOVE  SPACE               TO Z-PLZ                        00033000
                                           Z-ORT                        00034000
                                           Z-AUSL                       00035000
           WHEN     K-KNR-ANNAHME-VERWEIGERT                            00036000
              MOVE 'ANNAHME VERWEIGERT' TO Z-STRASSE                    00037100
              MOVE  SPACE               TO Z-PLZ                        00037200
                                           Z-ORT                        00037300
                                           Z-AUSL                       00037400
           WHEN     K-KNR-ANSCHR-VERSTORBEN                             00037500
              MOVE 'VERSTORBEN'         TO Z-STRASSE                    00037700
              MOVE  SPACE               TO Z-PLZ                        00037800
                                           Z-ORT                        00037900
                                           Z-AUSL                       00038000
170993*    WHEN     K-KNR-ANSCHR-KUENDIGUNG                             00038100
170993*       MOVE 'KUENDIGUNG '        TO Z-STRASSE                    00038300
170993*       MOVE  SPACE               TO Z-PLZ                        00038400
170993*                                    Z-ORT                        00038500
170993*                                    Z-AUSL                       00038600
           WHEN     K-KNR-ANSCHR-BEENDET                                00038700
              MOVE 'ANSCHRIFT BEENDET'  TO Z-STRASSE                    00038900
              MOVE  SPACE               TO Z-PLZ                        00039000
                                           Z-ORT                        00039100
                                           Z-AUSL                       00039200
           WHEN     OTHER                                               00039300
      *    LOG. FEHLER                                                  00039400
              SET   VERARB-LOG-FEHLER          TO TRUE                  00039500
              MOVE 'ANSCHRIFT-UNGUELT-BELEGEN' TO WK-PARAGRAPH          00039600
              MOVE  WK-FEHLER-TEXT6            TO WLR-FEHLER-TEXT       00039700
           END-EVALUATE                                                 00039800
           .                                                            00041400
       ANSCHRIFT-UNGUELT-BELEGEN-EXIT.                                  00042000
           EXIT.                                                        00050000
