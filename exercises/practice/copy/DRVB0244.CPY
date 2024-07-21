      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0244.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRVB0244.CPv  $
      *
      *   Rev 2.1   Oct 21 1999 14:54:38   T13EX50
      *AKI0003 - Produktionsübergabe :
      *                Neues Bestellverfahren 
      *                "Der Vermögensberater"
      *
      *   Rev 2.0   Jul 01 1999 10:35:46   T13AE42
      *AKI00003 - Check bestellberechtigung
      *
      *   Rev 1.1   Jul 01 1999 10:30:24   T13AE42
      *AKI00003 -  Check Bestellberechtigung
      *
      *   Rev 1.0   23 Jul 1997 12:21:12   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0244                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0049                        *
      *    PRÜFEN OB VBNR FÜR DAS BESTELLVERFAHREN DER ZEITSCHRIFT    *
      *    "DER VERMÖGENSBERATER" BESTELLBERECHTIGT IST               *
      *    --------------------------------------------------------   *
      *    ERSTELLT: B. SCHMIDT                                       *
      *    DATUM:    01.07.1999                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00003!01.07.99! 01 ! NEU                       ! SCHMIDT  *
      *         !13.07.99!    ! AUSGABE MAT-DAT-BIS       ! HERWEG   *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      *
      *---------------------------------------------------------------*
      ***************************************************************
      *                 PARAMETER-INPUT                             *
      ***************************************************************
       01  PI-DRVB0244.
          05 PI-DRVB0244-DATEN.
             10 PI-DRVB0244-MA-ST-NR              PIC S9(009) COMP.

      ***************************************************************
      *                 PARAMETER-OUTPUT                            *
      ***************************************************************
       01  PO-DRVB0244.
           05  PO-DRVB0244-RC                 PIC  X(03).
               88  PO-DRVB0244-RC-OK           VALUE 'OK '.
               88  PO-DRVB0244-RC-NFD          VALUE 'NFD'.
130799     05  P0-DRVB0244-MAT-DAT-BIS         PIC  X(10).
      *
      ******** ENDE COPY-MEMBER (DRVB0244) *******************

