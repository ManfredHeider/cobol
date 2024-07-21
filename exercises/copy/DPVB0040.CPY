      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DPVB0040.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DPVB0040.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:57:34   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 08:28:52   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: DPVB0040                                      * 00020000
      *    DIESE SECTION ORDNET DIE DATENFELDER AUS DER TABLE MAAN    * 00021000
      *    DEN ENTSPRECHENDEN ZWISCHENFELDERN ZU                      * 00021100
      *    FÜR KNR-GRND = ZERO (GÜLTIGE ANSCHRIFT)                    * 00021200
      *    --------------------------------------------------------   * 00021300
      *    ERSTELLT: O. NAUMANN       (DVAG)                          * 00021600
      *    DATUM:    12.05.1993                                       * 00021700
      ***************************************************************** 00021900
      *    VERSION   01   16.09.93                                   *  00022005
      ****************************************************************  00022105
      * AENDERUNGEN:                                                 *  00022205
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00022305
      * -------------------------------------------------------------*  00022405
      *         !16.09.93! 01 ! NEU                       ! NAUMANN  *  00022505
      *         !        !    !                           !          *  00022605
      *         !        !    !                           !          *  00022705
      *         !        !    !                           !          *  00022805
      *         !        !    !                           !          *  00022905
      *         !        !    !                           !          *  00023005
      *         !        !    !                           !          *  00023105
      *         !        !    !                           !          *  00023205
      *                                                              *  00023305
      * ENDE-LOGBUCH                                                 *  00023405
      ****************************************************************  00023505
      *---------------------------------------------------------------  00023600
       VB1-ANSCHRIFT-BELEGEN SECTION.                                   00023702
      *---------------------------------------------------------------  00023800
      *-R 3.0.00, PR-NR 59:                                     -       00023900
      *-          AUFGRUND DER PLZ-UMSTELLUNG WURDEN MEHRERE    -       00024000
      *-          ZUSTELLUNGSTYPEN NOTWENDIG.                   -       00024100
      *-          DIE ANSCHRIFT IM ALT-SYSTEM MUß NUN IN AB-    -       00024200
      *-          HÄNGIGKEIT DES ZUSTELLUNGSTYPEN BELEGT WERDEN -       00024300
      *-INPUT: KNR-ZUSTELLUNG                                   -       00024400
      *-       KNR-AUSL                                         -       00024500
      *-       MAAN-ORT                                         -       00024600
      *-       MAAN-STRASSE                                     -       00024700
      *-       MAAN-PLZ-AKTUELL                                 -       00024800
      *-       MAAN-ORT-PF                                      -       00024900
      *-       MAAN-POSTFACH                                    -       00025000
      *-       MAAN-PLZ-PF                                      -       00025100
      *-OUTPUT:                                                 -       00025200
      *-       Z-ORT                                            -       00025300
      *-       Z-STRASSE                                        -       00025400
      *-       Z-PLZ                                            -       00025500
      *-       Z-AUSL                                           -       00025600
      *----------------------------------------------------------       00025700
       VB1-ANSCHRIFT-BELEGEN-ANF.                                       00025802
      *----------------------------------------------------------       00026000
           EVALUATE KNR-ZUSTELLUNG                                      00039300
           WHEN     K-KNR-POST-ABR-ANSCHR                               00039404
           WHEN     K-KNR-ABR-ANSCHR                                    00039504
           WHEN     K-KNR-POST-PF--ABR-ANSCHR                           00039604
              MOVE MAAN-ORT             TO Z-ORT                        00039900
              MOVE MAAN-STRASSE         TO Z-STRASSE                    00040000
              MOVE MAAN-PLZ-AKTUELL     TO Z-PLZ                        00040100
           WHEN     K-KNR-POST-ABR-PF                                   00040204
           WHEN     K-KNR-ABR-PF                                        00040304
           WHEN     K-KNR-POST-ANSCHR--ABR-PF                           00040404
              MOVE MAAN-ORT-PF          TO Z-ORT                        00040700
              MOVE MAAN-POSTFACH        TO Z-STRASSE                    00040800
              MOVE MAAN-PLZ-PF          TO Z-PLZ                        00040900
           END-EVALUATE                                                 00041000
                                                                        00041100
           PERFORM AUSL-KZ-BELEGEN                                      00041902
           .                                                            00043600
       VB1-ANSCHRIFT-BELEGEN-EXIT.                                      00044002
           EXIT.                                                        00050000
