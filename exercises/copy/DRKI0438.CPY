      *---------------------------------------------------------------*
      *    Standard f™r alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0438.CPY  $
      *    $Revision:   2.8  $
      *
      *    $Log:   L:/mf/CPY/DRKI0438.CPv  $
      *
      *   Rev 2.8   Dec 09 2019 13:52:22   t13ae31
      *AKI01734: G81 Advocard Nettopreise Stornodatum UPKI0050 OZP
      *
      *   Rev 2.7   Jan 11 2016 14:56:14   t13ex78
      *AKI01404: AdvoCard - Kombirabatt und Kundenbonus trennen - Z
      *TKI0353
      *
      *   Rev 2.6   Aug 28 2013 13:18:58   t13ex19
      *AKI01249: AdvoCard - Sofortrabatt
      *
      *   Rev 2.5   May 09 2006 10:26:02   t13ex39
      *AKI00215: Umstellung aller Fremdverträge aud EURO
      *
      *   Rev 2.4   May 09 2006 10:10:34   t13ex39
      *AKI00215: Umstellung aller Fremdverträge aud EURO
      *
      *   Rev 1.0   28 Jul 1998 09:53:46   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI0438                                     *
      *    ----------------------------------------------------------*
      *    SCHNITTSTELLE FÜR DAS UPRO ZTKI0353                       *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN       *
      *    ----------------------------------------------------------*
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         12.11.2019 / 10:05:38 / T13AE31
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL
      *    PRO UPRO ÜBERGEBEN WERDEN.
      *    ALSO NICHT: DATEN, DIE PRO FUNKTION ÜBERGEBEN WERDEN
      *                (IN SEPARATEN COPYS PRO FUNKTION)
      *
      ****************************************************************
      *
       01  PI-ZTKI0353.
           05  PI-ZTKI0353-FUNKTION              PIC  X(008).


             88   PI-ZTKI0353-DRKI0392     VALUE 'DRKI0392'.
             88   PI-ZTKI0353-DRKI0393     VALUE 'DRKI0393'.
             88   PI-ZTKI0353-DRKI0403     VALUE 'DRKI0403'.
             88   PI-ZTKI0353-DRKI0413     VALUE 'DRKI0413'.

      *
      ******* ENDE COPY-MEMBER **************************************

