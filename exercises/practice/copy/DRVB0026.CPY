      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0026.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0026.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:57:46   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:07:58   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************  00011001
      *    COPY-MEMBER: DRVB0026 - SCHNITTSTELLE FÜR UPVB0030        *  00020001
      *    --------------------------------------------------------  *  00030001
      *    ERSTELLT: H. LEISTE                                       *  00060001
      *    DATUM:    24.04.1993                                      *  00070001
      ****************************************************************  00081101
      *    VERSION   1    08.03.94 17:00:15
      ****************************************************************  00081301
      * AENDERUNGEN:                                                 *  00081401
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00081501
      * -------------------------------------------------------------*  00081601
      *         !24.04.93! 01 ! NEU                       ! LEISTE   *  00081701
      * FEVB0080!17.02.94!    ! NEU: PI..-FKT /-MA-RCK-STZ! SCHÖLLH. *  00082201
      *         !07.03.94! 02 ! PROD.UEBERGABE            ! SCHÖLLH. *  00082302
      *         !        !    !                           !          *  00082401
      *         !        !    !                           !          *  00082501
      *         !        !    !                           !          *  00082601
      *         !        !    !                           !          *  00082701
      *         !        !    !                           !          *  00082801
      *                                                              *  00082901
      * ENDE-LOGBUCH                                                 *  00083001
      ****************************************************************  00083101
      *  EINGABE:                                                    *  00083201
      *    FKT      -->  KUENDIGUNG MA DURCHFÜHREN                   *  00083301
      *                                                              *  00083401
      *  AUSGABE:                                                    *  00083501
      *    RC-OK    -->  ALLES OK                                    *  00083601
      *    BETR-ED  -->  NEUER BETREUER WÄRE EIN ED                  *  00084001
      *    RC-NFND  -->  SATZ NICHT GEFUNDEN                         *  00085001
      *    RC-DB2S  -->  FEHLER BEIM SCHREIBMODUL                    *  00086001
      *                  - PR-PARAMTER WIRD GEFÜLLT MIT              *  00087001
      *                    PARAMETER-RETURN AUS SCHREIBMODUL         *  00088001
      ****************************************************************  00090001
      *                                                                 00100000
       01  PI-UPVB0030-EINGABE.                                         00110001
170294     05  PI-UPVB0030-FKT            PIC  X(002).                  00120001
170294         88 PI-UPVB0030-FKT-KUEND                     VALUE 'KU'. 00120101
170294     05  PI-UPVB0030-MA-RCK-STZ     PIC S9(003)V99 COMP-3.        00120201
           05  PI-UPVB0030-ST-NR          PIC S9(009)    COMP.          00121001
           05  PI-UPVB0030-DAT-UPDATE     PIC  X(010).                  00130001
           05  PI-UPVB0030-PROGRAMM-ID    PIC  X(008).                  00140001
           05  PI-UPVB0030-PROGRAMM-KZ    PIC  X(001).                  00150001
           05  PI-UPVB0030-USER-ID        PIC  X(008).                  00160001
           05  PI-UPVB0030-TERMINAL-ID    PIC  X(008).                  00170001
                                                                        00171001
       01  PO-UPVB0030-AUSGABE.                                         00180001
           05  PO-UPVB0030-RETURN-CODE    PIC  9.                       00190001
               88  PO-UPVB0030-RC-OK                        VALUE ZERO. 00200001
               88  PO-UPVB0030-RC-BETR-ED                   VALUE  1.   00210001
               88  PO-UPVB0030-RC-NFND                      VALUE  2.   00220001
               88  PO-UPVB0030-RC-DB2S                      VALUE  3.   00230001
           05  PO-UPVB0030-PR-PARAMETER   PIC  X(082).                  00240001
      *                                                                 00780001
      ******** ENDE COPY-MEMBER (DRVB0026) *******************          00790001
                                                                        00800001
