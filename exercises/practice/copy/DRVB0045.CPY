      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0045.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0045.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:57:50   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:11:58   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: DRVB0045                                      * 00020000
      *    --------------------------------------------------------   * 00030000
      *    SCHNITTSTELLE UPVB0042 BETREUERWECHSEL FÜR UNTERSTELLTE    * 00040000
      *    --------------------------------------------------------   * 00041000
      *    ERSTELLT: GIESE                                            * 00042200
      *    DATUM:    06.09.1993                                       * 00042300
      ***************************************************************** 00042500
      *    VERSION   01   16.09.93                                   *  00042605
      ****************************************************************  00042705
      * AENDERUNGEN:                                                 *  00042805
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00042905
      * -------------------------------------------------------------*  00043005
      *         !16.09.93! 01 ! NEU                       ! GIESE    *  00043106
      *         !        !    !                           !          *  00043205
      *         !        !    !                           !          *  00043305
      *         !        !    !                           !          *  00043405
      *         !        !    !                           !          *  00043505
      *         !        !    !                           !          *  00043605
      *         !        !    !                           !          *  00043705
      *         !        !    !                           !          *  00043805
      *                                                              *  00043905
      * ENDE-LOGBUCH                                                 *  00044005
      ****************************************************************  00044105
      *                                                                 00044200
      *   PI-UPVB0042-MA-ST-NR       DER ZURÜCKGESTUFTE MA              00044301
      *                                                                 00044400
      *   PI-UPVB0042-BETR-ST-NR     DIESE BETR-ST-NR WIRD EINGESETZT,  00044501
      *                              WENN DER ZURÜCKGESTUFTE NICHT      00044601
      *                              MEHR BETREUEN DARF                 00044701
      *                                                                 00044801
      *   UPVB42-FKT-PRUEFEN         ES WIRD NUR GEPRÜFT.               00044902
      *   UPVB42-BETREUUNG-OK        ALS RETURN KOMMT, OB BETREUUNG     00045002
      *   UPVB42-BETREUUNG-NOK       OK ODER NOK IST                    00045104
      *                                                                 00045202
      *   UPVB42-FKT-UPDATEN         PRÜFEN UND UPDATEN                 00045302
      *                              WENN ZURÜCKGESTUFTER MA NICHT      00045402
      *                              BETREUEN DARF, FINDET IMPLIZITER   00045502
      *                              BETREUERWECHSEL STATT              00045602
      *                                                                 00045702
      *   PO-UPVB0042-RETCODE        0 --> OK                           00046000
      *                              1 --> PARAMTER FEHLER              00046101
      *                              2 --> FEHLER BEI SCHREIBMODUL      00047101
      *   PO-UPVB0042-PR-PARAMETER   RÜCKGABE-BEREICH VON SCHREIBMODUL  00047200
      *                                                                 00047300
       01  PI-UPVB0042.                                                 00047400
           05 PI-UPVB0042-MA-ST-NR           PIC S9(09) COMP.           00047500
           05 PI-UPVB0042-ST-TYP             PIC S9(04) COMP.           00047600
           05 PI-UPVB0042-BETR-ST-NR         PIC S9(09) COMP.           00047701
           05 PI-UPVB0042-BETR-ST-TYP        PIC S9(04) COMP.           00047801
           05 PI-UPVB0042-MA-MS-NR           PIC S9(04) COMP.           00047901
           05 PI-UPVB0042-MA-BS-NR           PIC S9(04) COMP.           00048001
           05 PI-UPVB0042-MA-DIR-NR          PIC S9(04) COMP.           00048101
           05 PI-UPVB0042-FKT                PIC  X(02).                00048200
               88  UPVB42-FKT-PRUEFEN                      VALUE 'PR'.  00048300
               88  UPVB42-FKT-UPDATEN                      VALUE 'UP'.  00048400
           05 PI-UPVB0042-CICS-TIMESTAMP     PIC  X(26).                00049003
           05 PI-UPVB0042-USER-ID            PIC  X(08).                00049100
           05 PI-UPVB0042-TERMINAL-ID        PIC  X(08).                00049200
      *                                                                 00049300
       01  PO-UPVB0042.                                                 00049400
           05 PI-UPVB0042-RC                 PIC  X(02).                00049501
               88  UPVB42-BETREUUNG-OK                 VALUE 'OK'.      00049601
               88  UPVB42-BETREUUNG-NOK                VALUE 'NK'.      00049701
           05  PO-UPVB0042-RETCODE           PIC  9(03).                00049800
           05  PO-UPVB0042-PR-PARAMETER      PIC  X(82).                00049900
      *                                                                 00050000
      ******** ENDE COPY-MEMBER (DRVB0045) *******************          00060000
