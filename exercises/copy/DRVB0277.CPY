      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0277.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\DRVB0277.CPv  $
      *
      *   Rev 2.2   06 Oct 2000 10:40:06   T13AE30
      *ASL00067 - FÖBO2
      *
      *   Rev 2.1   Jun 10 1999 11:17:32   T13AE34
      *APA00012-SST Daisy
      *
      *   Rev 2.0   Feb 27 1999 15:58:40   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   26 Nov 1998 10:00:26   T13AE35
      *ARW00016 - Neu, Funktionen fuer ZTVB0072
      *
      *   Rev 1.0   23 Jul 1997 12:25:30   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0277                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS STANDARD-OZP ZTVB0072                *
      *    --------------------------------------------------------   *
      *    ERSTELLT: D.Wilhelm                                        *
      *    DATUM:    19.06.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !19.06.98! 01 ! NEU                       ! D.Wilhelm*
      *         !        !    !                           !          *
      * APA00012!28.05.99!2.1 ! NEU: DRVB0397 INSERT      ! HEIDER   *
      *         !        !    !                           !          *
      * ASL00067!20.09.00!2.2 ! NEU: DRVB0508 UPDATE      ! TH.WILLIG*
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE INFORMATION, DIE EINMAL
      *    PRO UPRO ÜBERGEBEN WERDEN
      *    ALSO NICHT: DATEN, DIE PRO FUNKTION ÜBERGEBEN WERDEN
      *                (IN SEPARATEN COPYS PRO FUNKTION)
      *
      ****************************************************************
      *
       01  PI-ZTVB0072.
           05  PI-ZTVB0072-FUNKTION            PIC  X(008).
               88 PI-ZTVB0072-DRVB0278-UPD          VALUE 'DRVB0278'.
               88 PI-ZTVB0072-DRVB0283-SEL-ALL      VALUE 'DRVB0283'.
               88 PI-ZTVB0072-DRVB0319-DEL          VALUE 'DRVB0319'.
280599         88 PI-ZTVB0072-DRVB0397-INS          VALUE 'DRVB0397'.
200900         88 PI-ZTVB0072-DRVB0508-UPD          VALUE 'DRVB0508'.
      *
      ******* ENDE COPY-MEMBER **************************************
