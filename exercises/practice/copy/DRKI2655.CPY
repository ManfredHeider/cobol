      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  18.04.2017
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2655.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2655.CPv  $
      *
      *   Rev 2.1   May 23 2017 10:52:00   t13ac17
      *AKI01510: Standard OZP
      *
      *   Rev 2.0   Apr 18 2017 10:27:08 T13AC17
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2655                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2655                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         08.05.2017 / 08:32:08 / T13AC17
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01510!18.04.17!2.1 ! Neuerstellung             ! GWA      *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI2655
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  DELETE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2655.
           05  PI-DRKI2655-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI2655-LRVVERSTARLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2655-LRVVERTRAGSID   PIC  X(0013).
END   *
 
      *
       01  PO-DRKI2655.
           05  PO-DRKI2655-RC                     PIC  X(003).
               88 PO-DRKI2655-RC-OK               VALUE 'OK '.
               88 PO-DRKI2655-RC-NOT-FOUND        VALUE 'NFD'.
      *
      *
      ******* ENDE COPY-MEMBER **************************************

