      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  02.03.2005
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2005 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1754.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI1754.CPv  $
      *
      *   Rev 2.1   Apr 05 2005 17:44:38   t13ex27
      *AKI00475: Anpassungen für Badenia 
      *
      *   Rev 2.0   Mar 02 2005 12:09:54   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1754                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI1754                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  09.03.2005/13:24:58
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
      *    FÜR DIE FUNKTION DRKI1754
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1754.
           05  PI-DRKI1754-KEY.


             10 PI-DRKI1754-VERVERTRAGSID   PIC X(13).
             
      *
       01  PO-DRKI1754.
           05  PO-DRKI1754-RC                 PIC  X(003).
               88 PO-DRKI1754-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1754-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRKI1754-DATEN.


             10 PO-DRKI1754-VERABSCHLUSSGRUND.
                15 PO-DRKI1754-VRBSCHLSSGRND-LN   PIC S9(4) COMP.
                15 PO-DRKI1754-VRBSCHLSSGRND-TXT   PIC X(30).
                
             10 PO-DRKI1754-VERABSCHLUSSVBNR   PIC S9(9) COMP.
             10 PO-DRKI1754-VERAENDERUNGSDATUM   PIC X(26).
             10 PO-DRKI1754-VERDATPRODLOESCH   PIC X(10).
             10 PO-DRKI1754-VERDATVERTRAEND   PIC X(10).
             10 PO-DRKI1754-VEREHELEUTEKZ   PIC X(1).
             10 PO-DRKI1754-VEREIGENVERMKZ   PIC X(1).
             10 PO-DRKI1754-VERFRALFDNR   PIC S9(4) COMP.
             10 PO-DRKI1754-VERFRAPERSONENID   PIC X(13).
             10 PO-DRKI1754-VERFREISTAUFTRAG   PIC X(1).
             10 PO-DRKI1754-VERFREMDVERTRAGKZ   PIC X(1).
             10 PO-DRKI1754-VERGEBUENDELTKZ   PIC X(1).
             10 PO-DRKI1754-VERGESAMTBEITRAG   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI1754-VERGESBEITRWKZ   PIC X(1).
             10 PO-DRKI1754-VERINTERNEVERTRNR   PIC S9(9) COMP.
             10 PO-DRKI1754-VERMONATDERHAUPTF   PIC S9(4) COMP.
             10 PO-DRKI1754-VERNACHLASSKZ   PIC X(1).
             10 PO-DRKI1754-VERNOTIZID   PIC X(13).
             10 PO-DRKI1754-VERPARTGESELLSCHID   PIC S9(4) COMP.
             10 PO-DRKI1754-VERPRUEFZIFFVNR   PIC S9(4) COMP.
             10 PO-DRKI1754-VERREMDVERTRGESNAM.
                15 PO-DRKI1754-VRRMDVRTRGSNM-LN   PIC S9(4) COMP.
                15 PO-DRKI1754-VRRMDVRTRGSNM-TXT   PIC X(25).
                
             10 PO-DRKI1754-VERSTORNODATUM   PIC X(10).
             10 PO-DRKI1754-VERVERTRAGSABLAUF   PIC X(10).
             10 PO-DRKI1754-VERVERTRAGSBEGINN   PIC X(10).
             10 PO-DRKI1754-VERVERTRAGSNUMMER   PIC X(15).
             10 PO-DRKI1754-VERVERTRZUSTANDDAT   PIC X(10).
             10 PO-DRKI1754-VERVERTRZUSTANDID   PIC S9(4) COMP.
             10 PO-DRKI1754-VERVSNRZUSATZ   PIC X(10).
             10 PO-DRKI1754-VERZAHLUNGSARTID   PIC S9(4) COMP.
             10 PO-DRKI1754-VERZAHLUNGSWEISEID   PIC S9(4) COMP.
             

      *
      ******* ENDE COPY-MEMBER **************************************
