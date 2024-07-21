      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  13.03.2014
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2014 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1788.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1788.CPv  $
      *
      *   Rev 2.1   Mar 26 2014 10:55:18   t13ae39
      *APA01766: GESAMTABRECHNUNGSPROTOKOLL DAISY: zus. Kontrolliste
      *
      *   Rev 2.0   Mar 13 2014 15:00:46   t13ae39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *                                                              *
      *   PARAMETERBESCHREIBUNG                                      *
      *   IN     : PI-ZTPA1788                                       *
      *            PI-EINGABE                                        *
      *            STANDARD-STATUS-BEREICH                           *
      *   OUT    : PO-RUECKGABE                                      *
      *   IN/OUT : ERR-ERROR-BEREICH                                 *
      *                                                              *
      *   VERWENDETE UEBERGABEBEREICHE:                              *
      *                                                              *
      *   COPY : DRPA1788 - Z. B. FUNKTIONSNAME                      *
      *   IN   : PI-ZTPA0034                                         *
      *                                                              *
      *   COPY : DRPA1789 - UEBERGABEBEREICH FUER INSERT             *
      *   IN   : PI-DRPA1789                                         *
      *   OUT  : PO-DRPA1789                                         *
      *                                                              *
      *   COPY : DRPA1790 - UEBERGABEBEREICH FUER SELECT ALL         *
      *   IN   : PI-DRPA1790                                         *
      *   OUT  : PO-DRPA1790                                         *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * APA01766!13.03.14!2.0 ! NEUERSTELLUNG             ! STRAUSS   *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL
      *    PRO UPRO ÜBERGEBEN WERDEN.
      *    ALSO NICHT: DATEN, DIE PRO FUNKTION ÜBERGEBEN WERDEN
      *                (IN SEPARATEN COPYS PRO FUNKTION)
      *
      ****************************************************************
       01  PI-ZTPA0034.
           05 PI-ZTPA0034-FUNKTION            PIC X(008).
              88 PI-ZTPA0034-DRPA1789-INS         VALUE 'DRPA1789'.
              88 PI-ZTPA0034-DRPA1790-SEL         VALUE 'DRPA1790'.
      ******* ENDE COPY-MEMBER **************************************
      *---------------------------------------------------------------*
