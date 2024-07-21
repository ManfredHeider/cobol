      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0069.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0069.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:57:56   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:17:00   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************  00001000
      *    COPY-MEMBER: DRVB0069, SCHNITTSTELLE UPVB0056             *  00002003
      *                 KOMPETENZPRÜFUNG AUF DIE VBNR                *  00002100
      *                 ERMITTELT OB DER INNENDIENST BERECHTIGT IST, *  00002200
      *                 DIE VBNR UMZUBUCHEN                          *  00002300
      *    --------------------------------------------------------  *  00002400
      *    ERSTELLT: B. BOHLINGER                                    *  00002505
      *    DATUM:    11.01.1995                                      *  00002606
      ****************************************************************  00002700
      *    VERSION   1    12.01.95 17:02:37
      ****************************************************************  00002900
      * AENDERUNGEN:                                                 *  00003000
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00003100
      * -------------------------------------------------------------*  00003200
      * AFOP0042!11.01.95! 01 ! NEU                       ! BOHLINGER*  00004006
      *         !        !    !                           !          *  00005000
      *         !        !    !                           !          *  00006000
      *         !        !    !                           !          *  00007000
      *         !        !    !                           !          *  00008000
      *                                                              *  00009000
      * ENDE-LOGBUCH                                                 *  00010000
      ****************************************************************  00020000
      *                                                                 00030000
      *   BESCHREIBUNG DER SCHNITTSTELLE:                               00040000
      *   - ERLÄUTERUNG DER FUNKTIONEN                                  00041000
      *   - WELCHE EINGABEN (PI) ERFORDERLICH (ABH. VON FKT)            00042000
      *   -  ZUR ÜBERPRÜFUNG DER KOMPETENZ SIND FOLGENDE EINGABEN       00042105
      *      ERFORDELICH:                                               00042205
      *      - SETZEN DER FUNKTION PI-UPVB0056-FKT-KOMPETENZ            00042305
      *      - ÜBERGABE DER ZU ÜBERPRÜFENDE VBNR                        00042405
      *   - WELCHE AUSGABEN (PO) WERDEN GELIEFERT                       00042505
      *        (ABH. VON FKT UND RC)                                    00043000
      *     FUNKTION KOMPETENZ:                                         00043105
      *             -  KEINE SINDERKOMPETENZ ERFORDERLICH               00043205
      *                (RC-MA-NOT-GESPERRT)                             00043305
      *             -  ZUR ÜBERGEBENEN VBNR IST SONDERKOMPETENZ         00043405
      *                ERFORDERLICH (RC-MA-GESPERRT)                    00043505
      *             -  DIE VBNR KONNTE NICHT ÜBERPRÜFT WERDEN           00043605
      *                (RC-MA-NOTFND UND VB56-RC-NOK)                   00043705
      *   - EVTL. ERLÄUTERUNG ANDERER PI- ODER PO-FELDER                00044000
      *                                                                 00048500
                                                                        00048600
      ***************************************************************   00048700
      *                 PARAMETER-INPUT                             *   00048800
      ***************************************************************   00048900
       01  PI-UPVB0056.                                                 00049003
           05  PI-UPVB0056-FKT                PIC  9(02).               00049103
               88 PI-VB56-FKT-KOMPETENZ                  VALUE 1.       00049203
                                                                        00049500
           05  PI-UPVB0056-MA-ST-NR           PIC  X(07).               00049603
                                                                        00052300
      ***************************************************************   00052400
      *                 PARAMETER-OUTPUT                            *   00052500
      ***************************************************************   00052600
       01  PO-UPVB0056.                                                 00052703
           05  PO-UPVB0056-RC                 PIC  X(03).               00052803
               88  PO-VB56-RC-OK                         VALUE 'OK '.   00052903
               88  PO-VB56-RC-NOK                        VALUE 'NOK'.   00054003
                                                                        00054100
           05  PO-UPVB0056-RC-MA              PIC  9(02).               00054203
               88  PO-VB56-RC-MA-NOT-GESPERRT            VALUE 1.       00054303
               88  PO-VB56-RC-MA-GESPERRT                VALUE 2.       00054403
               88  PO-VB56-RC-MA-NOTFND                  VALUE 3.       00054504
                                                                        00390000
      ******** ENDE COPY-MEMBER * DRVB0069 *************************    00400005
