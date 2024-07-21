      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0070.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0070.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:57:56   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:17:14   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************  00001000
      *    COPY-MEMBER: DRVB0070, SCHNITTSTELLE UPVB0055             *  00002001
      *                 ERM LETZTER WECHSEL STUFE + BONUS            *  00002100
      *                 VOR EINEM BEST. ZEITPUNKT                    *  00002200
      *    --------------------------------------------------------  *  00002300
      *    ERSTELLT: W. SCHÖLLHAMMER                                 *  00002400
      *    DATUM:    02.01.1995                                      *  00002500
      ****************************************************************  00002600
      *    VERSION   1    13.01.95 11:20:13
      ****************************************************************  00002800
      * AENDERUNGEN:                                                 *  00002900
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00003000
      * -------------------------------------------------------------*  00003100
      * AFVB0160!02.01.95!    ! NEU                       ! SCHÖLLH. *  00004003
      *         !13.01.95! 01 ! PROD.UEBERGABE            ! SCHÖLLH. *  00004103
      *         !        !    !                           !          *  00005000
      *         !        !    !                           !          *  00006000
      *         !        !    !                           !          *  00007000
      *         !        !    !                           !          *  00008000
      *                                                              *  00009000
      * ENDE-LOGBUCH                                                 *  00010000
      ****************************************************************  00020000
      *                                                                 00030000
      *   BESCHREIBUNG DER SCHNITTSTELLE:                               00040000
      *                                                                 00040100
      *   FUNKTIONEN:                                                   00040200
      *     PI-VB55-FKT-ERM-ZEITP  ERM. VON MITARBEITER- UND BONUS-STUFE00040301
      *                            ZUM ÜBERGEBENEN ZEITPUNKT SOWIE      00040400
      *                            DER DATEN, WANN DIE JEWEILIGE STUFE  00040500
      *                            ERREICHT WURDE                       00040600
      *                                                                 00040700
      *   EINGABE:                                                      00040800
      *     ABH. VON DER FKT SIND FOLGENDE EINGABEFELDER ZU FÜLLEN:     00041000
      *                            +----+------------------------       00041100
      *                            ! ZP !                               00041200
      *     -----------------------+----+------------------------       00041300
      *     PI-UPVB0055-ST-NR      ! X  !                               00041401
      *     PI-UPVB0055-ZEITPKT-ISO! X  !                               00041501
      *     -----------------------+----+------------------------       00041600
      *                                                                 00041700
      *   AUSGABE:                                                      00041800
      *     ABH. VON DER FKT KÖNNEN FOLGENDE RC'S AUFTRETEN:            00042400
      *                            +----+------------------------       00042500
      *                            ! ZP !                               00042600
      *     -----------------------+----+------------------------       00042700
      *     PO-VB55-RC-OK          ! X  !                               00042801
      *     PO-VB55-RC-NOT-FOUND   ! X  !                               00042901
      *     -----------------------+----+------------------------       00043000
      *                                                                 00043100
      *     ABH. VON FKT UND RC WERDEN FOLGENDE WERTE ZURÜCKGEGEBEN:    00043200
      *                            +--------+--------+---------------   00043300
      *                            ! ZP/OK  ! ZP/NFD !                  00043400
      *     -----------------------+--------+--------+---------------   00043500
      *     PO-UPVB0055-MS         !   X    !        !                  00043701
      *     PO-UPVB0055-MS-DATUM   !   X    !        !                  00043801
      *     PO-UPVB0055-BS         !   X    !        !                  00043901
      *     PO-UPVB0055-BS-DATUM   !   X    !        !                  00044001
      *     -----------------------+--------+--------+---------------   00044100
      *                                                                 00044200
      *   ERLÄUTERUNGEN                                                 00044300
      *   -------------                                                 00044400
      *     PO-VB55-RC         WERTE KONNTEN ERMITTELT WERDEN           00044501
      *     PO-VB55-NOT-FOUND  WERTE KONNTEN NICHT ERMITTELT WERDEN,    00044601
      *                          WEIL ZUM ANGEGEBENEN ZEITP. MS UND BS  00044700
      *                          NICHT DEFINIERT SIND                   00044800
                                                                        00045700
      ***************************************************************   00045800
      *                 PARAMETER-INPUT                             *   00045900
      ***************************************************************   00046000
       01  PI-UPVB0055.                                                 00046101
           05  PI-UPVB0055-FKT                PIC  X(002).              00046201
               88 PI-VB55-FKT-ERM-ZEITP                   VALUE 'ZP'.   00046301
                                                                        00047000
           05  PI-UPVB0055-ST-NR              PIC S9(009) COMP.         00047102
           05  PI-UPVB0055-ZEITPKT-ISO        PIC  X(010).              00048001
           05  FILLER                         PIC  X(048).              00049002
                                                                        00050000
      ***************************************************************   00051000
      *                 PARAMETER-OUTPUT                            *   00052000
      ***************************************************************   00052100
       01  PO-UPVB0055.                                                 00052201
           05  PO-UPVB0055-RC                 PIC  X(003).              00052301
               88  PO-VB55-RC-OK                          VALUE 'OK '.  00052401
               88  PO-VB55-RC-NOT-FOUND                   VALUE 'NFD'.  00052601
                                                                        00052700
           05  PO-UPVB0055-MS                 PIC S9(004) COMP.         00052801
           05  PO-UPVB0055-MS-DATUM           PIC  X(026).              00052901
           05  PO-UPVB0055-BS                 PIC S9(004) COMP.         00053001
           05  PO-UPVB0055-BS-DATUM           PIC  X(026).              00053101
           05  FILLER                         PIC  X(133).              00053200
                                                                        00053300
      ******** ENDE COPY-MEMBER ************************************    00055000
                                                                        00056000
