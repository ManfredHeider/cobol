      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  03.09.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0307.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI0307.CPv  $
      *
      *   Rev 2.4   Jan 14 2009 10:33:34   t13ex14
      *AKI00731: OZP ZTKI0357 VERTRAG
      *
      *   Rev 2.0   Sep 03 2007 09:22:28   t13ex14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI0307                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0307                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  14.01.2009/10:26:21
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
      *    FÜR DIE FUNKTION DRKI0307
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI0307.
           05  PI-DRKI0307-KEY.


             10 PI-DRKI0307-VERVERTRAGSID   PIC X(13).
             

           05  PI-DRKI0307-DATEN.


             10 PI-DRKI0307-VERABSCHLUSSGRUND.
                15 PI-DRKI0307-VRBSCHLSSGRND-LN   PIC S9(4) COMP.
                15 PI-DRKI0307-VRBSCHLSSGRND-TXT   PIC X(30).
                
             10 PI-DRKI0307-VERABSCHLUSSVBNR   PIC S9(9) COMP.
             10 PI-DRKI0307-VERAENDERUNGSDATUM   PIC X(26).
             10 PI-DRKI0307-VERDATVERTRAEND   PIC X(10).
             10 PI-DRKI0307-VEREIGENVERMKZ   PIC X(1).
             10 PI-DRKI0307-VERFRALFDNR   PIC S9(4) COMP.
             10 PI-DRKI0307-VERFRAPERSONENID   PIC X(13).
             10 PI-DRKI0307-VERFREISTAUFTRAG   PIC X(1).
             10 PI-DRKI0307-VERFREMDVERTRAGKZ   PIC X(1).
             10 PI-DRKI0307-VERGEBUENDELTKZ   PIC X(1).
             10 PI-DRKI0307-VERGESAMTBEITRAG   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI0307-VERGESBEITRWKZ   PIC X(1).
             10 PI-DRKI0307-VERINTERNEVERTRNR   PIC S9(9) COMP.
             10 PI-DRKI0307-VERMANDANTNR   PIC S9(9) COMP.
             10 PI-DRKI0307-VERMONATDERHAUPTF   PIC S9(4) COMP.
             10 PI-DRKI0307-VERNACHLASSKZ   PIC X(1).
             10 PI-DRKI0307-VERNOTIZID   PIC X(13).
             10 PI-DRKI0307-VERPARTGESELLSCHID   PIC S9(4) COMP.
             10 PI-DRKI0307-VERPRUEFZIFFVNR   PIC S9(4) COMP.
             10 PI-DRKI0307-VERREMDVERTRGESNAM.
                15 PI-DRKI0307-VRRMDVRTRGSNM-LN   PIC S9(4) COMP.
                15 PI-DRKI0307-VRRMDVRTRGSNM-TXT   PIC X(25).
                
             10 PI-DRKI0307-VERSTORNODATUM   PIC X(10).
             10 PI-DRKI0307-VERVERTRAGSABLAUF   PIC X(10).
             10 PI-DRKI0307-VERVERTRAGSBEGINN   PIC X(10).
             10 PI-DRKI0307-VERVERTRAGSNUMMER   PIC X(15).
             10 PI-DRKI0307-VERVERTRZUSTANDDAT   PIC X(10).
             10 PI-DRKI0307-VERVERTRZUSTANDID   PIC S9(4) COMP.
             10 PI-DRKI0307-VERZAHLUNGSARTID   PIC S9(4) COMP.
             10 PI-DRKI0307-VERZAHLUNGSWEISEID   PIC S9(4) COMP.
             
      *
       01  PO-DRKI0307.
           05  PO-DRKI0307-RC                 PIC  X(003).
               88 PO-DRKI0307-RC-OK                        VALUE 'OK '.
               88 PO-DRKI0307-RC-DUPLICATE-VALUE           VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************
