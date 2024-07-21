      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  22.05.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2320.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2320.CPv  $
      *
      *   Rev 2.1   Jul 21 2009 13:33:34   t13ex14
      *AKI00817: Ausweis von Zusatzinformationen zu Advocard; OZP-O
      *bjekte
      *
      *   Rev 2.0   May 22 2009 08:45:22   t13ex14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2320                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2320                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  12.06.2009/14:09:50
      *****************************************************************
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
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI2320
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2320.
           05  PI-DRKI2320-KEY.


             10 PI-DRKI2320-VERVERTRAGSID   PIC X(13).
             

           05  PI-DRKI2320-DATEN.


             10 PI-DRKI2320-VERABSCHLUSSGRUND.
                15 PI-DRKI2320-VRBSCHLSSGRND-LN   PIC S9(4) COMP.
                15 PI-DRKI2320-VRBSCHLSSGRND-TXT   PIC X(30).
                
             10 PI-DRKI2320-VERABSCHLUSSVBNR   PIC S9(9) COMP.
             10 PI-DRKI2320-VERAENDERUNGSDATUM   PIC X(26).
             10 PI-DRKI2320-VERDATPRODLOESCH   PIC X(10).
             10 PI-DRKI2320-VERDATVERTRAEND   PIC X(10).
             10 PI-DRKI2320-VEREHELEUTEKZ   PIC X(1).
             10 PI-DRKI2320-VEREIGENVERMKZ   PIC X(1).
             10 PI-DRKI2320-VERFRALFDNR   PIC S9(4) COMP.
             10 PI-DRKI2320-VERFRAPERSONENID   PIC X(13).
             10 PI-DRKI2320-VERFREISTAUFTRAG   PIC X(1).
             10 PI-DRKI2320-VERFREMDVERTRAGKZ   PIC X(1).
             10 PI-DRKI2320-VERGEBUENDELTKZ   PIC X(1).
             10 PI-DRKI2320-VERGESAMTBEITRAG   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI2320-VERGESBEITRBKZ   PIC X(1).
             10 PI-DRKI2320-VERGESBEITRWKZ   PIC X(1).
             10 PI-DRKI2320-VERINTERNEVERTRNR   PIC S9(9) COMP.
             10 PI-DRKI2320-VERMANDANTNR   PIC S9(9) COMP.
             10 PI-DRKI2320-VERMONATDERHAUPTF   PIC S9(4) COMP.
             10 PI-DRKI2320-VERNACHLASSKZ   PIC X(1).
             10 PI-DRKI2320-VERNOTIZID   PIC X(13).
             10 PI-DRKI2320-VERPARTGESELLSCHID   PIC S9(4) COMP.
             10 PI-DRKI2320-VERPRUEFZIFFVNR   PIC S9(4) COMP.
             10 PI-DRKI2320-VERREMDVERTRGESNAM.
                15 PI-DRKI2320-VRRMDVRTRGSNM-LN   PIC S9(4) COMP.
                15 PI-DRKI2320-VRRMDVRTRGSNM-TXT   PIC X(25).
                
             10 PI-DRKI2320-VERSTORNGRND-KNR   PIC S9(4) COMP.
             10 PI-DRKI2320-N-VRSTRNGRND-KNR   PIC S9(4) COMP.
             10 PI-DRKI2320-VERSTORNGRND-KTY   PIC S9(4) COMP.
             10 PI-DRKI2320-N-VRSTRNGRND-KTY   PIC S9(4) COMP.
             10 PI-DRKI2320-VERSTORNODATUM   PIC X(10).
             10 PI-DRKI2320-VERVERTRAGSABLAUF   PIC X(10).
             10 PI-DRKI2320-VERVERTRAGSBEGINN   PIC X(10).
             10 PI-DRKI2320-VERVERTRAGSNUMMER   PIC X(15).
             10 PI-DRKI2320-VERVERTRZUSTANDDAT   PIC X(10).
             10 PI-DRKI2320-VERVERTRZUSTANDID   PIC S9(4) COMP.
             10 PI-DRKI2320-VERVSNRZUSATZ   PIC X(10).
             10 PI-DRKI2320-VERZAHLUNGSARTID   PIC S9(4) COMP.
             10 PI-DRKI2320-VERZAHLUNGSWEISEID   PIC S9(4) COMP.
             

      *
       01  PO-DRKI2320.
           05  PO-DRKI2320-RC                 PIC  X(003).
               88 PO-DRKI2320-RC-OK                        VALUE 'OK '.
               88 PO-DRKI2320-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
