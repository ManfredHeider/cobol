      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  02.03.2005
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2005 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1753.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI1753.CPv  $
      *
      *   Rev 2.1   Apr 05 2005 17:44:32   t13ex27
      *AKI00475: Anpassungen für Badenia 
      *
      *   Rev 2.0   Mar 02 2005 12:09:40   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1753                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI1753                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  09.03.2005/13:24:57
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
      *    FÜR DIE FUNKTION DRKI1753
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1753.
           05  PI-DRKI1753-KEY.


             10 PI-DRKI1753-VERVERTRAGSID   PIC X(13).
             

           05  PI-DRKI1753-DATEN.


             10 PI-DRKI1753-VERABSCHLUSSGRUND.
                15 PI-DRKI1753-VRBSCHLSSGRND-LN   PIC S9(4) COMP.
                15 PI-DRKI1753-VRBSCHLSSGRND-TXT   PIC X(30).
                
             10 PI-DRKI1753-VERABSCHLUSSVBNR   PIC S9(9) COMP.
             10 PI-DRKI1753-VERAENDERUNGSDATUM   PIC X(26).
             10 PI-DRKI1753-VERDATPRODLOESCH   PIC X(10).
             10 PI-DRKI1753-VERDATVERTRAEND   PIC X(10).
             10 PI-DRKI1753-VEREHELEUTEKZ   PIC X(1).
             10 PI-DRKI1753-VEREIGENVERMKZ   PIC X(1).
             10 PI-DRKI1753-VERFRALFDNR   PIC S9(4) COMP.
             10 PI-DRKI1753-VERFRAPERSONENID   PIC X(13).
             10 PI-DRKI1753-VERFREISTAUFTRAG   PIC X(1).
             10 PI-DRKI1753-VERFREMDVERTRAGKZ   PIC X(1).
             10 PI-DRKI1753-VERGEBUENDELTKZ   PIC X(1).
             10 PI-DRKI1753-VERGESAMTBEITRAG   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI1753-VERGESBEITRWKZ   PIC X(1).
             10 PI-DRKI1753-VERINTERNEVERTRNR   PIC S9(9) COMP.
             10 PI-DRKI1753-VERMONATDERHAUPTF   PIC S9(4) COMP.
             10 PI-DRKI1753-VERNACHLASSKZ   PIC X(1).
             10 PI-DRKI1753-VERNOTIZID   PIC X(13).
             10 PI-DRKI1753-VERPARTGESELLSCHID   PIC S9(4) COMP.
             10 PI-DRKI1753-VERPRUEFZIFFVNR   PIC S9(4) COMP.
             10 PI-DRKI1753-VERREMDVERTRGESNAM.
                15 PI-DRKI1753-VRRMDVRTRGSNM-LN   PIC S9(4) COMP.
                15 PI-DRKI1753-VRRMDVRTRGSNM-TXT   PIC X(25).
                
             10 PI-DRKI1753-VERSTORNODATUM   PIC X(10).
             10 PI-DRKI1753-VERVERTRAGSABLAUF   PIC X(10).
             10 PI-DRKI1753-VERVERTRAGSBEGINN   PIC X(10).
             10 PI-DRKI1753-VERVERTRAGSNUMMER   PIC X(15).
             10 PI-DRKI1753-VERVERTRZUSTANDDAT   PIC X(10).
             10 PI-DRKI1753-VERVERTRZUSTANDID   PIC S9(4) COMP.
             10 PI-DRKI1753-VERVSNRZUSATZ   PIC X(10).
             10 PI-DRKI1753-VERZAHLUNGSARTID   PIC S9(4) COMP.
             10 PI-DRKI1753-VERZAHLUNGSWEISEID   PIC S9(4) COMP.
             

      *
       01  PO-DRKI1753.
           05  PO-DRKI1753-RC                 PIC  X(003).
               88 PO-DRKI1753-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1753-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
