      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  22.05.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2318.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2318.CPv  $
      *
      *   Rev 2.1   Jul 21 2009 13:33:20   t13ex14
      *AKI00817: Ausweis von Zusatzinformationen zu Advocard; OZP-O
      *bjekte
      *
      *   Rev 2.0   May 22 2009 08:45:02   t13ex14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2318                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2318                    *
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
      *    FÜR DIE FUNKTION DRKI2318
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2318.
           05  PI-DRKI2318-KEY.


             10 PI-DRKI2318-VERVERTRAGSID   PIC X(13).
             
      *
       01  PO-DRKI2318.
           05  PO-DRKI2318-RC                 PIC  X(003).
               88 PO-DRKI2318-RC-OK                        VALUE 'OK '.
               88 PO-DRKI2318-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRKI2318-DATEN.


             10 PO-DRKI2318-VERABSCHLUSSGRUND.
                15 PO-DRKI2318-VRBSCHLSSGRND-LN   PIC S9(4) COMP.
                15 PO-DRKI2318-VRBSCHLSSGRND-TXT   PIC X(30).
                
             10 PO-DRKI2318-VERABSCHLUSSVBNR   PIC S9(9) COMP.
             10 PO-DRKI2318-VERAENDERUNGSDATUM   PIC X(26).
             10 PO-DRKI2318-VERDATPRODLOESCH   PIC X(10).
             10 PO-DRKI2318-VERDATVERTRAEND   PIC X(10).
             10 PO-DRKI2318-VEREHELEUTEKZ   PIC X(1).
             10 PO-DRKI2318-VEREIGENVERMKZ   PIC X(1).
             10 PO-DRKI2318-VERFRALFDNR   PIC S9(4) COMP.
             10 PO-DRKI2318-VERFRAPERSONENID   PIC X(13).
             10 PO-DRKI2318-VERFREISTAUFTRAG   PIC X(1).
             10 PO-DRKI2318-VERFREMDVERTRAGKZ   PIC X(1).
             10 PO-DRKI2318-VERGEBUENDELTKZ   PIC X(1).
             10 PO-DRKI2318-VERGESAMTBEITRAG   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI2318-VERGESBEITRBKZ   PIC X(1).
             10 PO-DRKI2318-VERGESBEITRWKZ   PIC X(1).
             10 PO-DRKI2318-VERINTERNEVERTRNR   PIC S9(9) COMP.
             10 PO-DRKI2318-VERMANDANTNR   PIC S9(9) COMP.
             10 PO-DRKI2318-VERMONATDERHAUPTF   PIC S9(4) COMP.
             10 PO-DRKI2318-VERNACHLASSKZ   PIC X(1).
             10 PO-DRKI2318-VERNOTIZID   PIC X(13).
             10 PO-DRKI2318-VERPARTGESELLSCHID   PIC S9(4) COMP.
             10 PO-DRKI2318-VERPRUEFZIFFVNR   PIC S9(4) COMP.
             10 PO-DRKI2318-VERREMDVERTRGESNAM.
                15 PO-DRKI2318-VRRMDVRTRGSNM-LN   PIC S9(4) COMP.
                15 PO-DRKI2318-VRRMDVRTRGSNM-TXT   PIC X(25).
                
             10 PO-DRKI2318-VERSTORNGRND-KNR   PIC S9(4) COMP.
             10 PO-DRKI2318-N-VRSTRNGRND-KNR   PIC S9(4) COMP.
             10 PO-DRKI2318-VERSTORNGRND-KTY   PIC S9(4) COMP.
             10 PO-DRKI2318-N-VRSTRNGRND-KTY   PIC S9(4) COMP.
             10 PO-DRKI2318-VERSTORNODATUM   PIC X(10).
             10 PO-DRKI2318-VERVERTRAGSABLAUF   PIC X(10).
             10 PO-DRKI2318-VERVERTRAGSBEGINN   PIC X(10).
             10 PO-DRKI2318-VERVERTRAGSNUMMER   PIC X(15).
             10 PO-DRKI2318-VERVERTRZUSTANDDAT   PIC X(10).
             10 PO-DRKI2318-VERVERTRZUSTANDID   PIC S9(4) COMP.
             10 PO-DRKI2318-VERVSNRZUSATZ   PIC X(10).
             10 PO-DRKI2318-VERZAHLUNGSARTID   PIC S9(4) COMP.
             10 PO-DRKI2318-VERZAHLUNGSWEISEID   PIC S9(4) COMP.
             

      *
      ******* ENDE COPY-MEMBER **************************************
