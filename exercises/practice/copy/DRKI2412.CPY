      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.06.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2412.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/mf/CPY/DRKI2412.CPv  $
      *
      *   Rev 2.7   Jul 24 2017 13:55:44   t13ex27
      *AKI01506: Deuba KK aus GHP und DBP nach BANKPRODUKT überführ
      *en-Abgleich
      *
      *   Rev 2.6   Jun 20 2017 14:01:46   t13ex27
      *AKI01506: Deuba KK aus GHP und DBP nach BANKPRODUKT überführ
      *en-Abgleich
      *
      *   Rev 2.5   Feb 24 2016 15:00:00   t13ex27
      *AKI01409: Abgleich-Kundenübertragungsart berücksichtigen
      *
      *   Rev 2.4   Feb 17 2016 15:54:16   t13ex27
      *AKI01409: Abgleich-Kundenübertragungsart berücksichtigen
      *
      *   Rev 2.3   Nov 24 2010 10:32:02   t13ex27
      *AKI00966: Datenbestandsverarbeitung  DVAG Österreich-Abgleic
      *h-Programm
      *
      *   Rev 2.2   Oct 26 2010 10:50:46   t13ae49
      *AKI00966: Datenbestandsverarbeitung  DVAG Österreich-Abgleic
      *h-Programm
      *
      *   Rev 2.1   Oct 01 2010 11:58:44   t13ex27
      *AKI00966: Datenbestandsverarbeitung  DVAG Österreich-Abgleic
      *h-Programm
      *
      *   Rev 2.0   Jun 28 2010 10:58:04   t13ex14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI2412  FUNKTIONEN LIST-OZP                *
      *                                                              *
      *                                                              *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00966!13.08.10!2.1 ! NEUERSTELLUNG             !I. Frenzel*
      * AKI01409!28.01.16!2.4 ! neue Funktion             !I. Frenzel*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************

       01  PI-ZJKI0035.
           05 PI-ZJKI0035-FUNKTION             PIC X(008).
              88 PI-ZJKI0035-DRKI2413             VALUE 'DRKI2413'.
280116        88 PI-ZJKI0035-DRKI3059             VALUE 'DRKI3059'.
280116        88 PI-ZJKI0035-DRKI3151             VALUE 'DRKI3151'.

      *---------------------------------------------------------------*




