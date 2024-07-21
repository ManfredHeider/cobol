      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0121.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAT0121.CPv  $
      *
      *   Rev 2.1   Sep 30 2008 10:59:56   t13ae13
      *FVB00725: Rückstufung für Allfinanz-VB mit Verrechnungskonten
      *
      *   Rev 2.0   Feb 27 1999 11:57:22   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Dec 18 1996 12:59:26   P13SP17
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: KNR-KONSTANTEN FUER KL-TYP                    *
      *                 KTY-NR = 67                                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: T. Moll                                          *
      *    DATUM:    26.11.1996                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFVB0299!26.11.96! 01 ! NEU                       ! T. Moll  *
      * FVB00725!30.09.08!2.1 ! neue KL-Typen             ! S. Walb  *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************

       01  K-KNR-KL-INNENDIENST            PIC S9(04) BINARY VALUE 1.
       01  K-KNR-KL-AUSSENDIENST           PIC S9(04) BINARY VALUE 2.
       01  K-KNR-KL-SONDERKONTO            PIC S9(04) BINARY VALUE 3.
300908 01  K-KNR-KL-AM-VERRECHNUNGSKONTO   PIC S9(04) BINARY VALUE 4.
300908 01  K-KNR-KL-UMSCHL-NA-BEST-ALLFI   PIC S9(04) BINARY VALUE 5.

      ******** ENDE COPY-MEMBER ***************************************
