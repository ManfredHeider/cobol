      *---------------------------------------------------------------*
      *    Standard fÜr alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0441.CPY  $
 
      *    $Revision:   2.13  $
      *
      *    $Log:   L:/mf/CPY/DRKI0441.CPv  $
      *
      *   Rev 2.13   Jun 28 2016 12:12:46   t13ex78
      *AKI01427: Korrektur wegen Null-Indikatoren
      *
      *   Rev 2.12   Jun 28 2016 11:46:26   t13ex78
      *AKI01427: Korrektur wegen Null-Indikatoren
      *
      *   Rev 2.11   Jun 28 2016 10:40:10   t13ex78
      *AKI01427: Korrektur wegen Null-Indikatoren
      *
      *   Rev 2.10   Apr 28 2016 08:05:24   t13ex78
      *AKI01427: OZP's & DCLGEN's
      *
      *   Rev 2.9   Nov 21 2013 10:14:10   t13ex52
      *FKI01434: Ersetzen der ObjektId durch die VertragsId in KFZ-
      *Tabellen
      *
      *   Rev 2.8   Nov 24 2010 11:33:54   t13ae31
      *AKI00995: KFZ dynamische Vertragsanzeige
      *
      *   Rev 2.7   Oct 31 2008 09:39:20   t13ex14
      *AKI00731: Bestandsdatenlieferung Kfz Tarifierung; OZPs
      *
      *   Rev 1.0   28 Jul 1998 09:54:06   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI0441                                     *
      *    ----------------------------------------------------------*
      *    SCHNITTSTELLE FÜR DAS UPRO ZTKI0318                       *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN       *
      *    ----------------------------------------------------------*
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         28.06.2016 / 12:10:38 / T13EX78
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
       01  PI-ZTKI0318.
           05  PI-ZTKI0318-FUNKTION              PIC  X(008).
 
 
             88   PI-ZTKI0318-DRKI0440     VALUE 'DRKI0440'.
             88   PI-ZTKI0318-DRKI0442     VALUE 'DRKI0442'.
             88   PI-ZTKI0318-DRKI0449     VALUE 'DRKI0449'.
             88   PI-ZTKI0318-DRKI0783     VALUE 'DRKI0783'.
             88   PI-ZTKI0318-DRKI1984     VALUE 'DRKI1984'.
             88   PI-ZTKI0318-DRKI2277     VALUE 'DRKI2277'.
 
      *
      ******* ENDE COPY-MEMBER **************************************

