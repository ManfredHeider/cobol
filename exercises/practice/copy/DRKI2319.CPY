      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  22.05.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2319.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2319.CPv  $
      *
      *   Rev 2.1   Jul 21 2009 13:33:26   t13ex14
      *AKI00817: Ausweis von Zusatzinformationen zu Advocard; OZP-O
      *bjekte
      *
      *   Rev 2.0   May 22 2009 08:45:12   t13ex14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2319                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2319                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  12.06.2009/14:09:49
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
      *    FÜR DIE FUNKTION DRKI2319
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2319.
           05  PI-DRKI2319-KEY.


             10 PI-DRKI2319-VERVERTRAGSID   PIC X(13).
             

           05  PI-DRKI2319-DATEN.


             10 PI-DRKI2319-VERABSCHLUSSGRUND.
                15 PI-DRKI2319-VRBSCHLSSGRND-LN   PIC S9(4) COMP.
                15 PI-DRKI2319-VRBSCHLSSGRND-TXT   PIC X(30).
                
             10 PI-DRKI2319-VERABSCHLUSSVBNR   PIC S9(9) COMP.
             10 PI-DRKI2319-VERAENDERUNGSDATUM   PIC X(26).
             10 PI-DRKI2319-VERDATPRODLOESCH   PIC X(10).
             10 PI-DRKI2319-VERDATVERTRAEND   PIC X(10).
             10 PI-DRKI2319-VEREHELEUTEKZ   PIC X(1).
             10 PI-DRKI2319-VEREIGENVERMKZ   PIC X(1).
             10 PI-DRKI2319-VERFRALFDNR   PIC S9(4) COMP.
             10 PI-DRKI2319-VERFRAPERSONENID   PIC X(13).
             10 PI-DRKI2319-VERFREISTAUFTRAG   PIC X(1).
             10 PI-DRKI2319-VERFREMDVERTRAGKZ   PIC X(1).
             10 PI-DRKI2319-VERGEBUENDELTKZ   PIC X(1).
             10 PI-DRKI2319-VERGESAMTBEITRAG   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI2319-VERGESBEITRBKZ   PIC X(1).
             10 PI-DRKI2319-VERGESBEITRWKZ   PIC X(1).
             10 PI-DRKI2319-VERINTERNEVERTRNR   PIC S9(9) COMP.
             10 PI-DRKI2319-VERMANDANTNR   PIC S9(9) COMP.
             10 PI-DRKI2319-VERMONATDERHAUPTF   PIC S9(4) COMP.
             10 PI-DRKI2319-VERNACHLASSKZ   PIC X(1).
             10 PI-DRKI2319-VERNOTIZID   PIC X(13).
             10 PI-DRKI2319-VERPARTGESELLSCHID   PIC S9(4) COMP.
             10 PI-DRKI2319-VERPRUEFZIFFVNR   PIC S9(4) COMP.
             10 PI-DRKI2319-VERREMDVERTRGESNAM.
                15 PI-DRKI2319-VRRMDVRTRGSNM-LN   PIC S9(4) COMP.
                15 PI-DRKI2319-VRRMDVRTRGSNM-TXT   PIC X(25).
                
             10 PI-DRKI2319-VERSTORNGRND-KNR   PIC S9(4) COMP.
             10 PI-DRKI2319-N-VRSTRNGRND-KNR   PIC S9(4) COMP.
             10 PI-DRKI2319-VERSTORNGRND-KTY   PIC S9(4) COMP.
             10 PI-DRKI2319-N-VRSTRNGRND-KTY   PIC S9(4) COMP.
             10 PI-DRKI2319-VERSTORNODATUM   PIC X(10).
             10 PI-DRKI2319-VERVERTRAGSABLAUF   PIC X(10).
             10 PI-DRKI2319-VERVERTRAGSBEGINN   PIC X(10).
             10 PI-DRKI2319-VERVERTRAGSNUMMER   PIC X(15).
             10 PI-DRKI2319-VERVERTRZUSTANDDAT   PIC X(10).
             10 PI-DRKI2319-VERVERTRZUSTANDID   PIC S9(4) COMP.
             10 PI-DRKI2319-VERVSNRZUSATZ   PIC X(10).
             10 PI-DRKI2319-VERZAHLUNGSARTID   PIC S9(4) COMP.
             10 PI-DRKI2319-VERZAHLUNGSWEISEID   PIC S9(4) COMP.
             
      *
       01  PO-DRKI2319.
           05  PO-DRKI2319-RC                 PIC  X(003).
               88 PO-DRKI2319-RC-OK                        VALUE 'OK '.
               88 PO-DRKI2319-RC-DUPLICATE-VALUE           VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************
