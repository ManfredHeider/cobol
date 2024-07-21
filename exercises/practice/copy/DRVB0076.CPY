      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0076.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0076.CPv  $
      *
      *   Rev 2.1   Oct 13 2005 09:05:52   t13ex43
      *ACV00072: PGM - Manageraufbauplan - BB als CTV-Brief (2. Sei
      *te)
      *
      *   Rev 2.0   Feb 27 1999 15:57:58   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Feb 18 1997 10:10:50   T13AE24
      *AFVB0338, TBAU0003 auf 1000 Byte vergrößert
      *
      *   Rev 1.0   Nov 04 1995 09:29:02   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVB0076, ALLGEMEINE SCHNITTSTELLE           *
      *                 FÜR CTV-BATCH UPRO-CALL                      *
      *    --------------------------------------------------------  *
      *    ERSTELLT: P. SCHMIDT                                      *
      *    DATUM:    14.02.1995                                      *
      ****************************************************************
      *    VERSION   1    10.03.95 09:59:13
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFVB0183!14.02.95!    ! NEU                       ! SCHMIDT  *
      *         !10.03.95! 01 ! PROD.UEBERGABE            ! SCHÖLLH. *
      * AFVB0338!18.02.97! 02 ! TBAU0003 AUF 1000 BYTE    !          *
      *         !        !    ! VERGRößERT                ! SCHMIDT  *
010905* ACV00072!01.09.05!2.1 ! TBAU0003 AUF 4000 ERHÖHT  ! KLEEMEIER*
      *         !        !    !                           !          *
      *                                                              *
      ****************************************************************
      *
      *   BESCHREIBUNG DER SCHNITTSTELLE:
      *
      *   EINGABE:
      *
      *     PI-DRVB0076-ST-NR
      *     PI-DRVB0076-DATUM-EUR       SYSTEM-DATUM IM EURO-FORMAT
      *
      *   AUSGABE:
      *
      *     PO-DRVB0076-RC
      *     PO-DRVB0076-TBAU0003        BAUSTEIN 3
      *
      ***************************************************************
      *                 PARAMETER-INPUT                             *
      ***************************************************************
       01  PI-DRVB0076.
           05  PI-DRVB0076-ST-NR              PIC S9(009) COMP.
           05  PI-DRVB0076-DATUM-EUR          PIC  X(010).

      ***************************************************************
      *                 PARAMETER-OUTPUT                            *
      ***************************************************************
       01  PO-DRVB0076.
           05  PO-DRVB0076-RC                 PIC  X(003).
               88  PO-DRVB0076-RC-OK                      VALUE 'OK '.
               88  PO-DRVB0076-RC-NOK                     VALUE 'NOK'.
               88  PO-DRVB0076-RC-NOT-FOUND               VALUE 'NFD'.

180297*    05  PO-DRVB0076-TBAU0003           PIC  X(250).
010905**** 05  PO-DRVB0076-TBAU0003           PIC  X(1000).
010905     05  PO-DRVB0076-TBAU0003           PIC  X(4000).

      ******** ENDE COPY-MEMBER ************************************

