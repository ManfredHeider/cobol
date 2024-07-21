      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  08.05.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2014 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2607.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI2607.CPv  $
      *
      *   Rev 2.3   May 16 2014 13:09:44   t13ex78
      *AKI01180: Speicherfristen bei Interessenten und Altkunden - 
      *ZTKI0000
      *
      *   Rev 2.2   May 15 2014 11:55:34   t13ex78
      *AKI01180: Speicherfristen bei Interessenten und Altkunden -
      *Neugenerien des ZTKI0000 wegen NULL-Indikator
      *
      *   Rev 2.1   Jul 20 2012 09:54:26   t13ex27
      *AKI01135: Kundendaten physisch löschen-OZPs und NKDA-Copys
      *
      *   Rev 2.0   May 8 2012 11:18:10 T13EX27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI2607                                     *
      *    ----------------------------------------------------------*
      *    SCHNITTSTELLE FÜR DAS UPRO ZTKI0000                       *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN       *
      *    ----------------------------------------------------------*
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         16.05.2014 / 11:15:56 / T13EX78
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
       01  PI-ZTKI0000.
           05  PI-ZTKI0000-FUNKTION              PIC  X(008).


             88   PI-ZTKI0000-DRKI2608     VALUE 'DRKI2608'.
             88   PI-ZTKI0000-DRKI2609     VALUE 'DRKI2609'.
             88   PI-ZTKI0000-DRKI2851     VALUE 'DRKI2851'.

      *
      ******* ENDE COPY-MEMBER **************************************
