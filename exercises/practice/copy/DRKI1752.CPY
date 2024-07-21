      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  02.03.2005
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2005 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1752.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRKI1752.CPv  $
      *
      *   Rev 2.5   Nov 09 2007 11:41:06   t13ex14
      *AKL00664: Kfz- u. REOB PK-Ermittlung u. Aktualisierung
      *
      *   Rev 2.0   Mar 02 2005 12:09:22   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1752                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI1752                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  30.10.2007/10:42:46
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
      *    FÜR DIE FUNKTION DRKI1752
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1752.
           05  PI-DRKI1752-KEY.


             10 PI-DRKI1752-VERVERTRAGSID   PIC X(13).
             

           05  PI-DRKI1752-DATEN.


             10 PI-DRKI1752-VERABSCHLUSSGRUND.
                15 PI-DRKI1752-VRBSCHLSSGRND-LN   PIC S9(4) COMP.
                15 PI-DRKI1752-VRBSCHLSSGRND-TXT   PIC X(30).
                
             10 PI-DRKI1752-VERABSCHLUSSVBNR   PIC S9(9) COMP.
             10 PI-DRKI1752-VERAENDERUNGSDATUM   PIC X(26).
             10 PI-DRKI1752-VERDATPRODLOESCH   PIC X(10).
             10 PI-DRKI1752-VERDATVERTRAEND   PIC X(10).
             10 PI-DRKI1752-VEREHELEUTEKZ   PIC X(1).
             10 PI-DRKI1752-VEREIGENVERMKZ   PIC X(1).
             10 PI-DRKI1752-VERFRALFDNR   PIC S9(4) COMP.
             10 PI-DRKI1752-VERFRAPERSONENID   PIC X(13).
             10 PI-DRKI1752-VERFREISTAUFTRAG   PIC X(1).
             10 PI-DRKI1752-VERFREMDVERTRAGKZ   PIC X(1).
             10 PI-DRKI1752-VERGEBUENDELTKZ   PIC X(1).
             10 PI-DRKI1752-VERGESAMTBEITRAG   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI1752-VERGESBEITRWKZ   PIC X(1).
             10 PI-DRKI1752-VERINTERNEVERTRNR   PIC S9(9) COMP.
             10 PI-DRKI1752-VERMANDANTNR   PIC S9(9) COMP.
             10 PI-DRKI1752-VERMONATDERHAUPTF   PIC S9(4) COMP.
             10 PI-DRKI1752-VERNACHLASSKZ   PIC X(1).
             10 PI-DRKI1752-VERNOTIZID   PIC X(13).
             10 PI-DRKI1752-VERPARTGESELLSCHID   PIC S9(4) COMP.
             10 PI-DRKI1752-VERPRUEFZIFFVNR   PIC S9(4) COMP.
             10 PI-DRKI1752-VERREMDVERTRGESNAM.
                15 PI-DRKI1752-VRRMDVRTRGSNM-LN   PIC S9(4) COMP.
                15 PI-DRKI1752-VRRMDVRTRGSNM-TXT   PIC X(25).
                
             10 PI-DRKI1752-VERSTORNODATUM   PIC X(10).
             10 PI-DRKI1752-VERVERTRAGSABLAUF   PIC X(10).
             10 PI-DRKI1752-VERVERTRAGSBEGINN   PIC X(10).
             10 PI-DRKI1752-VERVERTRAGSNUMMER   PIC X(15).
             10 PI-DRKI1752-VERVERTRZUSTANDDAT   PIC X(10).
             10 PI-DRKI1752-VERVERTRZUSTANDID   PIC S9(4) COMP.
             10 PI-DRKI1752-VERVSNRZUSATZ   PIC X(10).
             10 PI-DRKI1752-VERZAHLUNGSARTID   PIC S9(4) COMP.
             10 PI-DRKI1752-VERZAHLUNGSWEISEID   PIC S9(4) COMP.
             
      *
       01  PO-DRKI1752.
           05  PO-DRKI1752-RC                 PIC  X(003).
               88 PO-DRKI1752-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1752-RC-DUPLICATE-VALUE           VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************
