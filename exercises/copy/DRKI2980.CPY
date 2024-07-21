      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  11.02.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2980.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2980.CPv  $
      *
      *   Rev 2.1   Feb 18 2015 16:56:30   t13ex29
      *AKI01348: Auslagern EC-Karten in eig. Tabelle- Tabelle
      *
      *   Rev 2.0   Feb 11 2015 15:25:38 T13EX27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2980                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2980                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         11.02.2015 / 15:25:38 / T13EX27
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
      *    FÜR DIE FUNKTION DRKI2980
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  DELETE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2980.
           05  PI-DRKI2980-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI2980-BECKONTOUNR   PIC S9(4) COMP.
             10 PI-DRKI2980-BECLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2980-BECVERTRAGSID   PIC  X(0013).
END   *
 
      *
       01  PO-DRKI2980.
           05  PO-DRKI2980-RC                     PIC  X(003).
               88 PO-DRKI2980-RC-OK               VALUE 'OK '.
               88 PO-DRKI2980-RC-NOT-FOUND        VALUE 'NFD'.
      *
      *
      ******* ENDE COPY-MEMBER **************************************

