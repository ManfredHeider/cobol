      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.03.2014
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2014 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1791.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRPA1791.CPv  $
      *
      *   Rev 2.3   May 21 2014 12:42:24   t13ae37
      *APA01766: Kontrollsummen DAISY: zus. Funktion in ZWPA0034
      *
      *   Rev 2.2   May 20 2014 11:37:50   t13ae37
      *APA01766: Kontrollsummen DAISY: zus. Funktion in ZWPA0034
      *
      *   Rev 2.1   Mar 21 2014 09:13:38   t13ae37
      *APA01766: GESAMTABRECHNUNGSPROTOKOLL DAISY: zus. Kontrolliste
      *
      *   Rev 2.0   Mar 19 2014 10:01:34   t13ae37
      *Archiverstellung
      *
      ****************************************************************
      *                                                              *
      *   PARAMETERBESCHREIBUNG                                      *
      *   IN     : PI-ZTPA1763                                       *
      *            PI-EINGABE                                        *
      *            STANDARD-STATUS-BEREICH                           *
      *   OUT    : PO-RUECKGABE                                      *
      *   IN/OUT : ERR-ERROR-BEREICH                                 *
      *                                                              *
      *   VERWENDETE UEBERGABEBEREICHE:                              *
      *                                                              *
      *   COPY : DRPA1791 - Z. B. FUNKTIONSNAME                      *
      *   IN   : PI-ZXPA0034                                         *
      *                                                              *
      *   COPY : DRPA1792 - UEBERGABEBEREICH FUER SUMME JE PG        *
      *   IN   : PI-DRPA1792                                         *
      *   OUT  : PO-DRPA1792                                         *
      *                                                              *
      *   COPY : DRPA1793 - UEBERGABEBEREICH FUER AKTUELLE SUMME VGES*
      *   IN   : PI-DRPA1793                                         *
      *   OUT  : PO-DRPA1793                                         *
      *                                                              *
      *   COPY : DRPA1794 - UEBERGABEBEREICH FUER GESAMTSUMME VGES   *
      *   IN   : PI-DRPA1794                                         *
      *   OUT  : PO-DRPA1794                                         *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * APA01766!19.03.14! 2.1! NEUERSTELLUNG             ! BUCH      *
      * APA01766!20.05.14! 2.2! DRPA1795 ERGÄNZT          ! BUCH      *
      * APA01766!21.05.14! 2.3! DRPA1796 ERGÄNZT          ! BUCH      *
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
       01  PI-ZXPA0034.
           05 PI-ZXPA0034-FUNKTION            PIC X(008).
              88 PI-ZXPA0034-DRPA1792             VALUE 'DRPA1792'.
              88 PI-ZXPA0034-DRPA1793             VALUE 'DRPA1793'.
              88 PI-ZXPA0034-DRPA1794             VALUE 'DRPA1794'.
              88 PI-ZXPA0034-DRPA1795             VALUE 'DRPA1795'.
              88 PI-ZXPA0034-DRPA1796             VALUE 'DRPA1796'.
      ******* ENDE COPY-MEMBER **************************************
