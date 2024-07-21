      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  14.05.2014
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2014 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2903.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI2903.CPv  $
      *
      *   Rev 2.3   May 27 2014 13:05:40   t13ex15
      *AKI01227: OnlineKI: Darstellung Anteilrückgabe bei OIF - ZTK
      *I0467 SWZ
      *
      *   Rev 2.2   May 19 2014 15:27:22   t13ex78
      *AKI01227: OnlineKI: Darstellung Anteilrückgabe bei OIF - ZTK
      *I0467
      *
      *   Rev 2.1   May 16 2014 11:26:02   t13ex78
      *AKI01227: OnlineKI: Darstellung Anteilrückgabe bei OIF - ZTK
      *I0467
      *
      *   Rev 2.0   May 14 2014 07:25:05 T13EX78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2903                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2903                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         27.05.2014 / 12:09:06 / T13EX78
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
      *    FÜR DIE FUNKTION DRKI2903
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2903.
           05  PI-DRKI2903-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI2903-OIFDATRUECKZAHLUNG   PIC  X(010).
             10 PI-DRKI2903-OIFFONDID   PIC S9(4) COMP.
             10 PI-DRKI2903-OIFGHALFDNR   PIC S9(4) COMP.
             10 PI-DRKI2903-OIFGUTHABPRODLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2903-OIFVERTRAGSID   PIC  X(0013).
END   *


           05  PI-DRKI2903-DATEN.


ANF   *        %M_PI_DATEN%
             10 PI-DRKI2903-OIFANTEILE   PIC S9(9)V9(4) COMP-3.
END   *


      *
       01  PO-DRKI2903.
           05  PO-DRKI2903-RC                     PIC  X(003).
               88 PO-DRKI2903-RC-OK               VALUE 'OK '.
               88 PO-DRKI2903-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
