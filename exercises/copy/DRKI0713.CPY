      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0713.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/DRKI0713.CPv  $
      *
      *   Rev 2.6   Mar 12 2015 09:11:28   t13ex78
      *FKI01494: BDSG Sperre bei mehrfachbetreuten Altkunden - UPKI
      *0244 anpassen
      *
      *   Rev 2.5   Mar 11 2015 08:56:30   t13ex78
      *FKI01494: BDSG Sperre bei mehrfachbetreuten Altkunden - UPKI
      *0244 anpassen
      *
      *   Rev 2.4   Sep 18 2012 15:13:16   t13ex27
      *AKI00886: Host - neues Design Kundenaktualisierung-KVB
      *
      *   Rev 2.3   Feb 27 2007 12:48:00   t13ex27
      *AKI00557: Ticketverfahren HOST-DB <-> Online-DB - OZPs
      *
      *   Rev 2.2   Apr 07 2004 14:21:14   t13ex27
      *Persönliche Anrede für mehrfachbetreute Kunden
      *
      *   Rev 2.1   Jun 10 1999 12:33:36   T13EX39
      *FKI00050 delete
      *
      *   Rev 2.0   Apr 29 1999 13:57:02   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI0713                                     *
      *    ----------------------------------------------------------*
      *    SCHNITTSTELLE FÜR DAS UPRO ZTKI0067                       *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN       *
      *    ----------------------------------------------------------*
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         11.03.2015 / 08:45:49 / T13EX78
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
       01  PI-ZTKI0067.
           05  PI-ZTKI0067-FUNKTION              PIC  X(008).
 
 
             88   PI-ZTKI0067-DRKI0868     VALUE 'DRKI0868'.
             88   PI-ZTKI0067-DRKI1652     VALUE 'DRKI1652'.
             88   PI-ZTKI0067-DRKI1895     VALUE 'DRKI1895'.
             88   PI-ZTKI0067-DRKI1896     VALUE 'DRKI1896'.
             88   PI-ZTKI0067-DRKI2985     VALUE 'DRKI2985'.
 
      *
      ******* ENDE COPY-MEMBER **************************************
 

