      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0104.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0104.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:58:06   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Feb 12 1997 09:11:10   T13EX11
      *AFVB0299: neue Fkt 'DKL' (neue Dir ermitteln
      *          zu übergebener KL-Nr)
      *
      *   Rev 1.0   Mar 27 1996 13:40:34   T13EX16
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVB0104, SCHNITTSTELLE UPVB0088             *
      *                 ERMITTELT VB-NUMMER DER NEUDIREKTION (ND)    *
      *                 ZU EINER ED-ST-NR                            *
      *    --------------------------------------------------------  *
      *    ERSTELLT: AUERBACH                                        *
      *    DATUM:    29.02.1996                                      *
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFVB0226!29.02.96!    ! NEU                       ! AUERBACH *
      * AFVB0299!18.12.96! 02 ! neue Fkt 'DKL': neue Dir  ! Schöllh. *
      *         !        !    !  ermitteln zu einer über- !          *
      *         !        !    !  gebenen KL-NR (Anlass:   !          *
      *         !        !    !  KL mit Typ 'Sonderkonto')!          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *   PI-UPVB0088-FKT
      *      PI-VB88-FKT-ERM-VB-NR
      *         ERMITTELN DER VB-NUMMER (NEUDIREKTION) ZUR VORGE-
      *         GEBENEN ED-ST-NR (ED-KONTO).
181296*         EIN KL-KONTO VOM TYP 'SONDERKONTO' WIRD BEHANDELT WIE
181296*         EIN ED-KONTO (D. H. ES WIRD DER UNTERSTELLTE ERM.)
      *
181296*      PI-VB88-FKT-ERM-ND-ZU-KL
181296*         ERMITTELN DER VB-NUMMER (NEUDIREKTION) ZUR VORGE-
181296*         GEBENEN KL-ST-NR (KL-KONTO).
181296*         DABEI WIRD DAVON AUSGEGANGEN, DASS DER KL NUR EINEN
181296*         UNTERSTELLTEN HAT (WIE BEIM KL-TYP 'SONDERKONTO')
      *
      *   PI-UPVB0088-ED-ST-NR
      *         ED-KONTO FÜR DAS DIE URSPRÜNGLICHE DIREKTION
      *         ERMITTELT WERDEN SOLL
      *
      ***************************************************************
      *                 PARAMETER-INPUT                             *
      ***************************************************************
       01  PI-UPVB0088.
           05  PI-UPVB0088-FKT                PIC  X(003).
181296*         88 PI-VB88-FKT-ERM-VB-NR                 VALUE 'EVN'.
181296         88 PI-VB88-FKT-ERM-ND-ZU-ED              VALUE 'DED'.
181296         88 PI-VB88-FKT-ERM-ND-ZU-KL              VALUE 'DKL'.

           05  PI-UPVB0088-ED-ST-NR           PIC  9(007).
181296     05  PI-UPVB0088-KL-ST-NR REDEFINES PI-UPVB0088-ED-ST-NR
181296                                        PIC  9(007).
      *
      ***************************************************************
      *                 PARAMETER-OUTPUT                            *
      ***************************************************************
       01  PO-UPVB0088.

      *    * ERGEBNIS:
           05  PO-UPVB0088-RC                 PIC  9(001).
               88  PO-VB88-RC-OK                           VALUE 1.
               88  PO-VB88-RC-KEINE-DATEN                  VALUE 2.
181296*         88  PO-VB88-RC-FKT-UNGUELT                  VALUE 3.
181296         88  PO-VB88-RC-VB-NR-NICHT-UNIQUE           VALUE 4.

181296*    *       'MA' ODER 'KL' ÜBER DEM DAS ED-KONTO STEHT
181296*    * ODER: 'MA'           ÜBER DEM DAS KL-KONTO STEHT
           05  PO-UPVB0088-ST-NR              PIC  9(007).

      *    * KENNZEICHNET DIE ST-NR ALS 'MA' ODER 'KL'
           05  PO-UPVB0088-KNR-BEZUGS-TYP     PIC S9(004) BINARY.

      *    * ANZAHL DER EBENEN ZWISCHEN ED-KONTO UND ST-NR DES D
           05  PO-UPVB0088-ANZ-GENERATION     PIC  9(001).

      ******** ENDE COPY-MEMBER   DRVB0104   ***********************

