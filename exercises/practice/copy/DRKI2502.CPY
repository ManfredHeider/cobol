      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  14.06.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2502.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2502.CPv  $
      *
      *   Rev 2.2   Jul 12 2011 08:37:42   t13ex18
      *AKI01040: Erw. Deuba- inkl. DWS-Daten / Ausgabe in KI-Vertrag
      *
      *   Rev 2.1   Jul 11 2011 16:17:26   t13ex18
      *AKI01040: Erw. Deuba- inkl. DWS-Daten / Ausgabe in KI-Vertrag
      *
      *   Rev 2.0   Jun 14 2011 14:52:18   t13ex18
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2502                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2502                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         12.07.2011 / 08:28:09 / T13EX18
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
      *    FÜR DIE FUNKTION DRKI2502
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2502.
           05  PI-DRKI2502-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI2502-ZULFONDID   PIC S9(4) COMP.
             10 PI-DRKI2502-ZULFOZLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2502-ZULGUTHABPRODLFD   PIC S9(4) COMP.
             10 PI-DRKI2502-ZULLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2502-ZULVERTRAGSID   PIC  X(0013).
END   *


           05  PI-DRKI2502-DATEN.


ANF   *        %M_PI_DATEN%
             10 PI-DRKI2502-LETZTEAENDERUNG   PIC  X(026).
             10 PI-DRKI2502-ZULBEITRJAHR   PIC S9(4) COMP.
             10 PI-DRKI2502-ZULPORTFOLIONR   PIC  X(0010).
             10 PI-DRKI2502-ZULSUMBEITRJAHR   PIC S9(13)V9(2) COMP-3.
END   *


      *
       01  PO-DRKI2502.
           05  PO-DRKI2502-RC                     PIC  X(003).
               88 PO-DRKI2502-RC-OK               VALUE 'OK '.
               88 PO-DRKI2502-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
