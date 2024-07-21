      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  23.04.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2756.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI2756.CPv  $
      *
      *   Rev 2.4   May 29 2013 08:56:04   t13ex15
      *AKI01187: Sonderzahlungen zu LV-Verträgen / AS-Club-Wertung 
      *OZP
      *
      *   Rev 2.3   May 21 2013 11:46:00   t13ex15
      *AKI01187: Sonderzahlungen zu LV-Verträgen / AS-Club-Wertung
      *OZP
      *
      *   Rev 2.2   May 14 2013 13:00:04   t13ex15
      *AKI01187: Sonderzahlungen zu LV-Verträgen / AS-Club-Wertung
      *OZP
      *
      *   Rev 2.1   May 06 2013 16:18:20   t13ex15
      *AKI01187: Sonderzahlungen zu LV-Verträgen / AS-Club-Wertung
      *OZP
      *
      *   Rev 2.0   Apr 23 2013 13:12:57 T13EX15
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2756                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2756                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         29.05.2013 / 08:34:30 / T13EX15
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
      *    FÜR DIE FUNKTION DRKI2756
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2756.
           05  PI-DRKI2756-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI2756-VZALFDNR   PIC S9(4) COMP.
             10 PI-DRKI2756-VZAVERTRAGSID   PIC  X(0013).
END   *


           05  PI-DRKI2756-DATEN.


ANF   *        %M_PI_DATEN%
             10 PI-DRKI2756-VZAANZMONVERARB   PIC S9(4) COMP.
             10 PI-DRKI2756-VZALFDMONRECHPGM   PIC S9(4) COMP.
END   *


      *
       01  PO-DRKI2756.
           05  PO-DRKI2756-RC                     PIC  X(003).
               88 PO-DRKI2756-RC-OK               VALUE 'OK '.
               88 PO-DRKI2756-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
