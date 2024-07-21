      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  30.11.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3526.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3526.CPv  $
      *
      *   Rev 2.1   Dec 11 2018 17:09:18   t13ae52
      *AKI01649: Generali - Löschung Kunden- und Vertragsdaten aus 
      *Temporal Tables - Sourcen/COPY
      *
      *   Rev 2.0   Nov 30 2018 11:22:30   t13ae52
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3526
      *    ----------------------------------------------------------
      *    SCHNITTSTELLE FÜR DAS UPRO UPKI0273
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN
      *    ----------------------------------------------------------
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME
      * -------------------------------------------------------------
      * AKI01649!30.11.18!2.1 ! NEUERSTELLUNG             !SCHUMACHER
      *         !        !    !                           !
      *         !        !    !                           !
      *
      * ENDE-LOGBUCH
      *****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL
      *    PRO UPRO ÜBERGEBEN WERDEN.
      *    ALSO NICHT: DATEN, DIE PRO FUNKTION ÜBERGEBEN WERDEN
      *                (IN SEPARATEN COPYS PRO FUNKTION)
      *
      *****************************************************************
      *
       01  PI-UPKI0273.
           05  PI-UPKI0273-FUNKTION              PIC  X(008).
             88   PI-UPKI0273-DRKI3527           VALUE 'DRKI3527'.
      *
      ******* ENDE COPY-MEMBER ****************************************
